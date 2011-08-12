(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
(* depens *)
module Format = Base.Format
module List = Base.List
module String = Base.String

(* alias *)
module Common = Imp_Common
module MatchGeneration = Imp_MatchGeneration
module PatternAnalysis = Imp_PatternAnalysis
module QmlCons = QmlAstCons.UntypedExpr
module Serializer = Qmljs_Serializer

(* shorthand *)
module E = Imp_Env
module J = JsAst
module Q = QmlAst
module P = Imp_PatternAnalysis

(* directives that are simply removed from the ast when compiling them *)
type ('a, 'b) ignored_directive = [
| QmlAst.type_directive
| `async
| `atomic
| `fun_action of 'a
| `nonexpansive
| `spawn
| `tracker of 'b
| `unsafe_cast
| `may_cps
| `wait
| `backend_ident of string
(* do not add 'lazy' directive here, or any directive that may avoid some computation,
   it will increases the number of directly nested record and letin,
   see may_alias_deep_record and may_flatten_letin, if you need to do it *)
]

let maybe_cons o l =
  match o with
  | None -> l
  | Some v -> v :: l

(*
 * Representation of values in the backend
 * - void is shared
 * - {true=void} and {false=void} are actual booleans
 * - other records are represented as is
 * - records may have a field size` or not, which may
 *   be created during pattern matching
 *)

let is_it_void _env expr =
  let rec aux = function
    | Q.Const _ -> `no
    | Q.Coerce (_, e,_) -> aux e
    | Q.Record (_, []) -> `yes
    | Q.Record (_, _ :: _) -> `no
    | _ -> `maybe in
  aux expr

let compile_bypass env key =
  match Imp_Bsl.JsImpBSL.ByPassMap.find_opt_implementation env.E.private_bymap ~lang:BslLanguage.js key with
  | None ->
      OManager.error
        "bsl-resolution failed for: key %a" BslKey.pp key
  | Some compiled -> (
      match
        Imp_Bsl.JsImpBSL.Implementation.CompiledFunction.compiler_detailed_repr compiled
      with
      | Imp_Bsl.JsImpBSL.Implementation.Ident ident ->
          JsCons.Expr.exprident ident
      | Imp_Bsl.JsImpBSL.Implementation.String s ->
          JsParse.String.expr ~globalize:true s
            (*
              No parse error should happen
              This is verified at the moment we build the bypass plugin
              If an injected code does not reparse, this is notified to
              the developper of the plugin.
            *)
    )

let may_alias_matched_begin cons private_env matched =
  (*
    Introduction of a binding if necessary, because the compilation of pattern
    may duplicate the code of the matched expression.
    The is done by the function [MatchGeneration.compile] but we cannot use this function
    there, because we are generating statement, and no expression
  *)
  match matched with
  | J.Je_ident _ ->
      private_env, None, matched
  | _ ->
      let private_env, ident = E.next private_env "matched" in
      let alias = Some (cons ident matched) in
      let matched = JsCons.Expr.ident ident in
      private_env, alias, matched

let may_alias_matched_end cons alias result =
  match alias with
  | Some alias -> cons alias result
  | None -> result

(* removes coercion and some directives, to simplify remaining code *)
let simplify_noop expr = QmlAstWalk.Expr.map (
  function
  | Q.Coerce (_, e, _)
  | Q.Directive (_, #ignored_directive, [e], _) -> e
  | e -> e
) expr

(* limits the depth of data structures, needed by most browsers *)
let maximum_depth = 42 (* a tuned... parameter *)
(* assumes simplify_noop
   e.g. if maximum_depth is around 2
   r1 = [ 1 , 2 , 3 , 4 ]
   =>
   r1' = [3, 4]
   r1  = [1 , 2| r1'] *)
let may_alias_deep_record expr =
  let rec aux depth bind expr =
    match expr with
    | Q.Record _ when depth > maximum_depth ->
      let id = Ident.next "f" in
      (id,expr)::bind,Q.Ident(QmlAst.Label.expr expr,id)
    | Q.Record (a, fields) as r ->
      let bind',fields = List.fold_left_map (fun bind (f,e)->
        let bind,e = aux (depth+1) bind e in
        bind,(f,e)
      ) bind fields
      in
      if bind==bind' then bind,r
      else bind',Q.Record (a, fields)
    | expr ->  bind,expr
  in
  let bind,e = aux 0 [] expr in
  match bind with
  | [] -> None
  | _  -> (
    let r = Q.LetIn(QmlAst.Label.expr expr,bind,e) in
    Some(r)
  )

(* to avoid depthness inspection at each level in simplify_records since it is not necessary (depth is monotonic),
   it applies a transformation to all datastructure leaf (i.e. non record),
   see simplify_records *)
let rec apply_on_record_leaf tra expr =
  match expr with
  | Q.Record (a, fields) ->
    let fields' = Base.List.map_stable (fun ((f,e) as v) ->
      let e'= apply_on_record_leaf tra e in
      if e==e' then v
      else (f,e')
    ) fields in
    if fields==fields' then expr
    else Q.Record (a, fields')
  | _ -> tra expr

(* apply may_alias_deep_record on the whole code *)
let simplify_records expr = QmlAstWalk.Expr.traverse_map (fun tra expr ->
  match expr with
  |  Q.Record _ ->
    begin match may_alias_deep_record expr with
     | Some(e) -> tra e
     | None -> apply_on_record_leaf tra expr
    end
  | _ -> QmlAstWalk.Expr.map_nonrec tra expr
) expr


(* limits the depth of nested environments (needed by inlining, otherwise it does not scale)
   we consider a record as a letin equivalent and hoist their local bindings
   e.g.
   a =
     b =
       c = 1
       1 + c
     1 + b
   r = { f1 = d=1 d+a }
   =>
   c = 1
   b = 1 + c
   a = 1 + b
   d = 1
   r = { f1 = d+a }
*)
let may_flatten_letin a initial_bind expr =
  let aux_bind_list aux bind el =
    Base.List.fold_left_map_stable (fun bind ((i,e) as v) ->
      let (bind',e') = aux bind e in
      let v = if e==e' then v else (i,e') in
      v::bind',v
    ) bind el
  in
  let aux_assoc_list aux bind el =
    Base.List.fold_left_map_stable (fun bind ((i,e) as v) ->
      let (bind',e') = aux bind e in
      let v = if e==e' then v else (i,e') in
      bind',v
    ) bind el
  in
  let rec aux bind expr =
    match expr with
    | Q.LetIn (_, iel, e) ->
      let bind,_iel = aux_bind_list aux bind iel in
      let bind,e = aux bind e in
      bind,e
    | Q.Record (a, fields) ->
      let bind,fields' = aux_assoc_list aux bind fields in
      if fields==fields' then  bind,expr
      else bind,Q.Record (a, fields')
    | _ -> bind,expr
  in
  let bind,initial_bind' = aux_bind_list aux [] initial_bind in
  let initial_bind_not_changed = initial_bind'==initial_bind in
  let bind,e = aux bind expr in
  if initial_bind_not_changed && e==expr then None
  else Some(Q.LetIn(a,List.rev bind,e))

(* apply may_flatten_letin on the whole code *)
let simplify_letin expr = QmlAstWalk.Expr.traverse_map (fun tra expr ->
  match expr with
  |  Q.LetIn(a, iel, sexpr) ->
    begin match may_flatten_letin a iel sexpr with
     | Some(e) -> tra e
     | None -> QmlAstWalk.Expr.map_nonrec tra expr
    end
  | _ -> QmlAstWalk.Expr.map_nonrec tra expr
) expr


let (|>) a f = f a

(* simplifies a code, to shorten remaining code,
   to limit datastructure and environment nesting *)
let simplify expr =
     expr
 |>  simplify_noop
 |>  simplify_records
 |>  simplify_letin


(* compilation of an expression into a javascript expression *)
let compile_expr_to_expr env private_env expr =
  let rec aux private_env expr =
    let toplevel_expr = expr in
    let unimplemented fmt =
      let context = QmlError.Context.annoted_expr env.E.annotmap expr in
      QmlError.i_error None context ("@[<2>Unimplemented compile_expr_to_expr@\n"^^fmt^^"@]") in
    match expr with
    | Q.Path _ -> assert false (* slicing error *)
    (* impossible cases with simplified code *)
    | Q.Coerce (_,_, _) -> assert false
    | Q.Directive (_, #ignored_directive, [_], _) -> assert false

    | Q.Const (_, c) ->
         private_env, Common.const c

    | Q.Ident (_, i) ->
        (try private_env, JsCons.Expr.ident (IdentMap.find i private_env.E.renaming)
         with Not_found -> private_env, JsCons.Expr.exprident i)

    | Q.Directive (_, `restricted_bypass _, [Q.Bypass (_, key)], _)
    | Q.Bypass (_, key) ->
        private_env, compile_bypass env key

    | Q.Lambda _ ->
        (* a precondition of the backend is that the code is lambda lifted *)
        unimplemented "internal lambda"

    | Q.Apply (_, f, args) ->
        aux_apply ~pure:false private_env f args

    | Q.Directive (_, `partial_apply (Some _, true), e :: ty_args, _) ->
      begin match e with
      | Q.Apply (_, f, args) ->
        aux_partial_apply_with_ty ~pure:true private_env f args ty_args
      | _ -> assert false
      end

    | Q.Directive (_, `partial_apply ((Some _ | None), false), [e], _) ->
      begin match e with
      | Q.Apply (_, f, args) -> aux_apply ~pure:true private_env f args
      | _ -> assert false
      end

    | Q.LetIn (_, iel, e) ->
        let private_env, exprs =
          List.fold_left_map
            (fun private_env (i,e) ->
               let private_env, i = E.next_exprident private_env i in
               let private_env, e = aux private_env e in
               private_env, JsCons.Expr.assign_ident i e
            ) private_env iel in
        let private_env, e = aux private_env e in
        private_env, JsCons.Expr.comma exprs e

    | Q.LetRecIn _ ->
        (* since the code is lambda lifted, and recursion is allowed only on
         * functions, this cannot happen *)
        unimplemented "internal rec lambda"

    | Q.Match (_, expr, patterns) -> (
        let annot = Q.QAnnot.expr expr in
        let private_env, matched = aux private_env expr in
        let private_env, patterns = List.fold_left_map (
          fun private_env (pat, expr) ->
            let private_env, expr = aux private_env expr in
            private_env, (pat, expr)
        ) private_env patterns in
        let gamma = env.E.gamma in
        let annotmap = env.E.annotmap in
        let pos = Q.Pos.expr expr in
        let ty =
          match QmlAnnotMap.find_ty_opt annot annotmap with
          | Some ty -> ty
          | None ->
              let context = QmlError.Context.annoted_expr env.E.annotmap toplevel_expr in
              QmlError.warning ~wclass:Imp_Warnings.missing_type context "no type annotation on matched expression" ;
              Q.TypeVar (Q.TypeVar.next ())
        in
        let private_env, alias, matched = may_alias_matched_begin JsCons.Expr.assign_ident private_env matched in
        let private_env, result =
          (*
            AdHoc optimizations:
          *)
          match
            MatchGeneration.AdHoc.compile
              ~env
              ~penv:private_env
              ~matched
              ~ty
              ~patterns
          with
          | Some cpl -> cpl
          | None -> (
              match PatternAnalysis.analysis ~gamma ~annotmap ~ty patterns with
              | P.Trivial patterns ->
                  MatchGeneration.T.compile
                    ~env
                    ~penv:private_env
                    ~pos
                    ~matched
                    ~ty
                    ~patterns

              | P.Pat patterns ->
                  MatchGeneration.compile
                    ~env
                    ~penv:private_env
                    ~pos
                    ~matched
                    ~ty
                    ~patterns
            )
        in
        let result = may_alias_matched_end (fun alias result -> JsCons.Expr.comma [ alias ] result) alias result in
        private_env, result
      )

    | Q.Record (_, original_l) ->
        let private_env, l =
          List.fold_left_map
            (fun private_env (s,e) ->
               let private_env, e = aux private_env e in
               private_env, (s, e)
            ) private_env original_l in
        let e =
          (* taking care to enforce the invariants described above *)
          match original_l with
          | [] -> Imp_Common.ClientLib.void
          | [("false"|"true" as s), e] ->
              (match is_it_void env e with
               | `no -> JsCons.Expr.obj l
               | `maybe -> JsCons.Expr.call ~pure:true (Imp_Common.ClientLib.build_bool (s = "true")) [snd (List.hd l)]
               | `yes -> JsCons.Expr.bool (s = "true"))
          | _ -> JsCons.Expr.obj l in
        private_env, e

    | Q.Dot (_, original_e, s) ->
        (* beware of the tricky case x.true and x.false,  because x maybe the
         * boolean true or an actual record with the field true *)
        let private_env, e = aux private_env original_e in
        let e =
          match s with
          | "false" | "true" ->
              (match is_it_void env original_e with
               | `no -> JsCons.Expr.field e s
               | `yes -> JsCons.Expr.comma [e] Imp_Common.ClientLib.void
               | `maybe -> JsCons.Expr.call ~pure:true (Imp_Common.ClientLib.dot_bool (s = "true")) [e]
              )
          | _ -> JsCons.Expr.field e s in
        private_env, e

    | Q.ExtendRecord _ ->
        let rec gather acc = function
          | Q.ExtendRecord (_, s, e, rest) -> gather ((s,e) :: acc) rest
          | e -> acc, e in
        let qml_new_fields, qml_original_record = gather [] expr in
        assert (qml_new_fields <> []);
        let private_env, js_new_fields =
          List.fold_left_map
            (fun private_env (s,e) ->
               let private_env, e = aux private_env e in
               private_env, (s, e)
            ) private_env qml_new_fields in
        let private_env, js_original_record = aux private_env qml_original_record in
        let runtime_extend () =
          private_env, JsCons.Expr.call ~pure:true Imp_Common.ClientLib.extend_record [js_original_record; JsCons.Expr.obj js_new_fields] in

        (* whenever we have a tricky case, we don't try to do anything at compile time
         * and instead we call [extendrecord] from the client lib *)
        if List.StringAssoc.mem "true" qml_new_fields
        || List.StringAssoc.mem "false" qml_new_fields then
          runtime_extend ()
        else
          (match QmlAnnotMap.find_ty_opt (Q.QAnnot.expr qml_original_record) env.E.annotmap with
           | None -> runtime_extend ()
           | Some ty ->
               match QmlTypesUtils.Inspect.follow_alias_noopt_private env.E.gamma ty with
               | Q.TypeRecord (Q.TyRow (fields, None)) ->
                   if List.StringAssoc.mem "true" fields || List.StringAssoc.mem "false" fields then
                     runtime_extend ()
                   else (
                     (* we are in the easy case: the record and the extended record
                      * will be regular js records *)
                     let private_env, record, ident_opt =
                       match js_original_record with
                       | J.Je_ident _ -> private_env, js_original_record, None
                       | _ ->
                           let private_env, ident = E.next private_env "extendrecord" in
                           private_env, JsCons.Expr.ident ident, Some ident in
                     let extended_record =
                       let old_fields = List.map (fun (s,_ty) -> (s,JsCons.Expr.dot record s)) fields in
                       let old_fields = List.StringAssoc.sort old_fields in
                       let new_fields = List.StringAssoc.sort js_new_fields in
                       let all_fields = List.StringAssoc.unique_sorted_merge ~merge:(fun _ e -> e) old_fields new_fields in
                       JsCons.Expr.obj all_fields in
                     let final_expr =
                       match ident_opt with
                       | None -> extended_record
                       | Some ident -> JsCons.Expr.comma [JsCons.Expr.assign_ident ident js_original_record] extended_record in
                     private_env, final_expr
                   )
               | Q.TypeRecord (Q.TyRow (_, Some _)) -> runtime_extend ()
               | _ -> assert false
          )

    | Q.Directive (_, `llarray, exprs, _) ->
        (*
          We should produce an javascript array.
          <!> Beware, the list exprs are huge, this should be tail rec.
        *)
        let private_env, exprs = List.fold_left_map aux private_env exprs in
        let array = JsCons.Expr.array exprs in
        private_env, array

    | Q.Directive (_, `fail, args, _) ->
        let position =
          let pos = Q.Pos.expr expr in
          JsCons.Expr.string (FilePos.to_string pos) in
        let private_env, message =
          match args with
          | [] -> private_env, JsCons.Expr.string ""
          | e :: _ -> aux private_env e
        in
        let fail =
          let key = Opacapi.Opabsl.BslPervasives.fail in
          compile_bypass env key
        in
        let fail = JsCons.Expr.call ~pure:false fail [ message ; position ] in
        private_env, fail

    | Q.Directive (_, `thread_context, _, _) ->
        let call =
          let key = Opacapi.Opabsl.BslCps.Notcps_compatibility.thread_context in
          compile_bypass env key
        in
        let call = JsCons.Expr.call ~pure:true call [] in
        private_env, call

    | Q.Directive (_, `with_thread_context, [_thread_context;expr], _) ->
        aux private_env expr

    | Q.Directive (_, `js_ident, [Q.Const (_, Q.String name)], _) ->
        let jsident = Serializer.JsIdent.resolve (env.E.val_ name) in
        let jsident = JsCons.Expr.hole (QmlCons.ident jsident) in
        private_env, jsident

    | Q.Directive (_, `tagged_string (string, kind), _, _) ->
        private_env, JsCons.Expr.runtime (JsAstRuntime.TaggedString (string, kind))

    | Q.Directive (_, `callcc, _, _) ->
        let context = QmlError.Context.annoted_expr env.E.annotmap expr in
        QmlError.error context (
          "The directive @{<bright>@@callcc@} is not available on the client side@\n"^^
          "@[<2>@{<bright>Hint@}:@\n"^^
          "Add a slicer annotation to compile this code on the server side@]"
        )

    | Q.Directive (_, `assert_, [_], _) ->
        (* assert directive should have been resolved sooner *)
        private_env, Imp_Common.ClientLib.void

    | Q.Directive _ ->
        unimplemented "directives"

  and aux_apply private_env ~pure f args =
    let private_env, f = aux private_env f in
    let private_env, args = List.fold_left_map aux private_env args in
    private_env, JsCons.Expr.call ~pure f args

  and aux_partial_apply_with_ty private_env ~pure f args ty_args =
    let private_env, f = aux private_env f in
    let private_env, args = List.fold_left_map aux private_env args in
    let private_env, ty_args = List.fold_left_map aux private_env ty_args in
    private_env, JsCons.Expr.call ~pure Imp_Common.ClientLib.
    env_apply_with_ty [f;(JsCons.Expr.array args);(JsCons.Expr.array ty_args)]
  in

  aux private_env (simplify expr)

let add_bindings_statement bindings statement =
  match bindings with
  | [] -> statement
  | _ ->
      let fold statement (ident, expr) =
        let assign = JsCons.Statement.assign_ident ident expr in
        assign :: statement in
      let stmts = List.fold_left fold [statement] (List.rev bindings) in
      JsCons.Statement.block stmts

(*
Compilation of tail recursion
f(x) = g(x+1)
g(x) = f(x+2)

is compiled as:

function f_g(case_, a) {
  while (true) {
    switch (case_) {
    case 0:
      /* body of f */
      case_ = 1;
      a = a + 1;
      continue;
    case 1:
      /* body of g */
      case_ = 0;
      a = a + 2;
      continue;
    }
  }
}
function f(x) {
  return f_g(0,x);
}
function g(x) {
  return f_g(1,x);
}

The code is almost lambda lifted in the sense that you can have one lambda
at toplevel or two lambdas at toplevel but no other possibility.
When lifted functions are mutually recursive, the two lambdas are squashed together:

outer(y) =
  rec f(x) = g(x+y)
  and g(x) = f(x)
  f

becomes:

function f_g(case_,y,x) {
  while (true) {
    switch(case_) {
    case 0: /* body of f */
    case 1: /* body of g */
    }
  }
}
function f(y) {
  return function (x) {
    return f_g(0,y,x);
  }
}
function g(y) {
  return function (x) {
    return f_g(0,y,x);
  }
}
function outer(y) {
  return f(y);
}
*)
type recursion_info = {
  case_ident : J.ident option;
  (* the variable case_ in the above example, when
   * the recursion is mutual or None otherwise *)
  params : J.ident list IdentMap.t;
  (* these parameters are the renamed ones
   * in the example:
   * f -> [a]
   * g -> [a] *)
  index : int IdentMap.t;
  (* meaningless when not_mutual is true
   * in the example:
   * f -> 0
   * g -> 1 *)
  number_of_funs : int;
}

(*
 * Adds in [acc] the [variables_to_look_for] that appear in [expr]
 *)
let add_occurring_variables acc variables_to_look_for expr =
  JsWalk.Expr.fold
    (fun acc e ->
       match e with
       | J.Je_ident (_,ident) when JsIdentSet.mem ident variables_to_look_for ->
           JsIdentSet.add ident acc
       | _ -> acc
    ) acc expr

(*
 * This function analyses a set of bindings of recursive values
 * and divides them into blocks of {functions making tail calls
 * to each other}
 * This is used to avoid squashing together the body of many
 * recursive functions when they don't make tail call to each other
 * Note that if we have a tail call from one function to another (but not a
 * cycle), here we choose to regroup them, but the other choice would
 * probably be valid as well
 *)
let analyse_tail_recursion bindings =
  (* env contains for each bound identifier in [bindings], the identifiers
   * to which its body makes tail calls to *)
  let env : IdentSet.t IdentTable.t = IdentTable.create (List.length bindings) in
  List.iter (fun (i,_) -> IdentTable.add env i IdentSet.empty) bindings;

  (* first build a kind of call graph with only the recursive tail calls *)
  let rec aux myself = function
    | Q.LetIn (_, _, e) -> aux myself e
    | Q.Match (_, _, pel) -> List.iter (fun (_,e) -> aux myself e) pel
    | Q.Apply (_, Q.Directive (_, `partial_apply _, [Q.Apply (_, Q.Ident (_, f), _)], _), _)
    | Q.Apply (_, Q.Ident (_, f), _) when IdentTable.mem env f ->
        #<If:JS_IMP$contains "tailcall">
          Format.printf " %s\n%!" (Ident.to_string f)
        #<End>;
        let set1 = IdentTable.find env f in
        let set2 = IdentTable.find env myself in
        let full_set = IdentSet.add myself (IdentSet.add f (IdentSet.union set1 set2)) in
        IdentSet.iter (fun i -> IdentTable.replace env i full_set) full_set
    | __e ->
        #<If:JS_IMP$contains "fulltailcall">
          Format.printf "@\nstopped on %a" QmlPrint.pp#expr __e
        #<End>;
        () in
  List.iter
    (fun (i,expr) ->
       #<If:JS_IMP$contains "tailcall">
         Format.printf ">> analysing tail calls of %s: " (Ident.to_string i)
       #<End>;
       (match expr with
        | Q.Lambda (_, _, Q.Lambda (_, _, e))
        | Q.Lambda (_, _, e) -> aux i e
        | _ -> OManager.i_error "Non lambda in a recursion: %a@." QmlPrint.pp#expr expr);
       #<If:JS_IMP$contains "tailcall">
         Format.printf "@."
       #<End>
    ) bindings;

  (* then building the groups of bindings making recursive tail calls to each other *)
  let binding_of_ident i =
    List.find (fun (j,_e) -> Ident.equal i j) bindings in
  let _already_seen, groups =
    IdentTable.fold
      (fun i set (already_seen,acc) ->
         if IdentSet.mem i already_seen then
           (already_seen,acc)
         else
           let already_seen = IdentSet.union set already_seen in
           let acc =
             match IdentSet.cardinal set with
             | 0 -> `no_recursion (binding_of_ident i) :: acc
             | 1 -> assert (IdentSet.mem i set); `self_recursion (binding_of_ident i) :: acc
             | _ ->
                 let bindings = List.map binding_of_ident (IdentSet.elements set) in
                 `mutual_recursion bindings :: acc in
           already_seen, acc
      ) env (IdentSet.empty,[]) in
  groups

(*
 * Compiles one function in a set of recursive bindings
 * with the optimization for tail rec calls
 * To be able to generate the [continue] where there are tail calls, we
 * must compile an opa expression into a javascript statement everywhere
 * where we have a tail call.
 * In practice, we compile to statements everywhere where we _may_ have
 * a tail call
 *)
let compile_function_body_aux env private_env recursion_info name body =
  #<If:JS_MATCH_COMPILATION $contains "code_elt">
    let pos = Q.Pos.expr body in
    OManager.printf (
      "@{<brigth>%a@}: %s@."
    )
      FilePos.pp pos
      (Ident.stident name)
  #<End>;

  (* this function is generating the tail calls
   * it turns out to be quite tricky because we don't want to generate too many
   * auxiliary variables, but sometimes we must generate some anyway:
   * f(x,y) = f(y,x)
   * will be compiled into:
   * function f(x,y) {
   *   while (true) {
   *     var tmp;
   *     tmp = x;
   *     x = y;
   *     y = tmp;
   *   }
   * }
   * because we cannot avoid the temporary variable.
   * Some analysis of dependency between the arguments of the tail calls
   * is necessary to minimize the number of temporary variables.
   *)
  let rec aux_fun private_env ?(fun_env=[]) f args =
    let args = if recursion_info.number_of_funs = 1 then args else fun_env @ args in
    let params = IdentMap.find f recursion_info.params in
    let private_env, args = List.fold_left_map (compile_expr_to_expr env) private_env args in
    let params, args =
      try
        List.filter2
          (fun param arg ->
             match arg with
             | J.Je_ident (_,param') when JsIdent.equal param param' -> false
             | _ -> true
          ) params args
      with Invalid_argument _ ->
        Format.eprintf "%s(%a)(%a) %a@ inside@\n%a."
          (Ident.to_string f)
          (Format.pp_list "," QmlPrint.pp#expr) fun_env
          (Format.pp_list "," (JsPrint.pp#expr ~leading:false)) args
          (Format.pp_list "," (fun f s -> Format.pp_print_string f (JsPrint.string_of_ident s))) params
          QmlPrint.pp#expr body
        ;
        assert false
    in
    let (_previous_params,need_alias) =
      List.fold_left2
        (fun (previous_params,need_alias) param arg ->
           let need_alias = add_occurring_variables need_alias previous_params arg in
           let previous_params = JsIdentSet.add param previous_params in
           (previous_params, need_alias)
        ) (JsIdentSet.empty, JsIdentSet.empty) params args in
    let rev_assignments, rev_aliases1, rev_aliases2, private_env =
      List.fold_left2
        (fun (rev_assignments,rev_aliases1,rev_aliases2,private_env) param arg ->
           if JsIdentSet.mem param need_alias then (
             let private_env, fresh = E.next private_env "tailrec" in
             let rev_aliases1 = JsCons.Statement.assign_ident fresh arg :: rev_aliases1 in
             let rev_aliases2 = JsCons.Statement.assign_ident param (JsCons.Expr.ident fresh) :: rev_aliases2 in
             (rev_assignments,rev_aliases1,rev_aliases2,private_env)
           ) else
             let rev_assignments = JsCons.Statement.assign_ident param arg :: rev_assignments in
             (rev_assignments,rev_aliases1,rev_aliases2,private_env)
        ) ([],[],[],private_env) params args in

    (* adding comments or else the generated code is really difficult to read *)
    let set_callee =
      match recursion_info.case_ident with
      | None -> [JsCons.Statement.comment ~kind:`one_line (Printf.sprintf "simple rec call (to %s)" (Ident.to_string f))]
      | Some case_ ->
          let my_index = IdentMap.find name recursion_info.index in
          let other_index = IdentMap.find f recursion_info.index in
          if my_index + 1 = other_index then
            (* as an additional trick, when calling the next function in the recursive group
             * we can simply fall through into its code:
             * f(x) = g(x)
             * g(x) = f(x)
             * becomes:
             * function f_g(case_,x) {
             *   while (true) {
             *     switch (case_) {
             *     case 0: /* fall through */
             *     case 1: case_ = 0
             *     }
             *   }
             * }
             *)
            [JsCons.Statement.comment ~kind:`one_line (Printf.sprintf "rec call (to %s) / falling through" (Ident.to_string f))]
          else (
            let s1 = JsCons.Statement.assign_ident case_ (JsCons.Expr.int other_index) in
            let s2 = JsCons.Statement.comment ~kind:`one_line (Printf.sprintf "rec call (to %s) / general case" (Ident.to_string f)) in
            if recursion_info.number_of_funs = my_index + 1 then
              (* no need to put a continue in the last case of the switch
               * (see the example above, in the right hand side of the 'case 1') *)
              [s1;s2]
            else
              let s3 = JsCons.Statement.continue () in
              [s1;s2;s3]
          ) in
    private_env, JsCons.Statement.block (rev_aliases1 @ List.rev rev_assignments @ rev_aliases2 @ set_callee)

  (* compilation of an expression as a javascript statement per se *)
  and aux private_env body =
    match body with
    | Q.Apply (_, Q.Directive (_, `partial_apply _, [Q.Apply (_, Q.Ident (_, f), fun_env)], _), args)
      when IdentMap.mem f recursion_info.params ->
        aux_fun private_env ~fun_env f args
    | Q.Apply (_, Q.Ident (_, f), args) when IdentMap.mem f recursion_info.params ->
        aux_fun private_env f args

    | Q.LetIn (_, iel, e) ->
        let private_env, declarations =
          List.fold_left_map
            (fun private_env (i,e) ->
               let private_env, e = compile_expr_to_expr env private_env e in
               private_env, JsCons.Statement.var (J.ExprIdent i) ~expr:e
            ) private_env iel in
        let private_env, stm = aux private_env e in
        private_env, JsCons.Statement.block (declarations @ [stm])

    | Q.Match (_, matched, patterns) -> (
        let annot = Q.QAnnot.expr matched in
        (*
          Compilation of the matched expression
        *)
        let private_env, matched = compile_expr_to_expr env private_env matched in
        (*
          Compilation of right_hand, as statements
        *)
        let private_env, patterns = List.fold_left_map (
          fun private_env (pat, expr) ->
            let private_env, rhs = aux private_env expr in
            private_env, (pat, rhs)
        ) private_env patterns in
        let gamma = env.E.gamma in
        let annotmap = env.E.annotmap in
        let ty =
          match QmlAnnotMap.find_ty_opt annot annotmap with
          | Some ty -> ty
          | None ->
              let context = QmlError.Context.annoted_expr env.E.annotmap body in
              QmlError.warning ~wclass:Imp_Warnings.missing_type context "no type annotation on matched expression" ;
              Q.TypeVar (Q.TypeVar.next ())
        in
        let private_env, alias, matched = may_alias_matched_begin JsCons.Statement.assign_ident private_env matched in
        let private_env, rev_cases =
          match PatternAnalysis.analysis ~gamma ~annotmap ~ty patterns with
          | P.Trivial patterns ->
              MatchGeneration.T.aux_compile
                ~env
                ~penv:private_env
                ~matched
                ~ty
                ~patterns

          | P.Pat patterns ->
              MatchGeneration.aux_compile
                ~env
                ~penv:private_env
                ~matched
                ~ty
                ~patterns
        in
        (*
          Adding a default case only if the match can fail
        *)
        let last_case, rev_cases =
          match rev_cases with
          | (None, bindings, statement) :: rev_cases ->
              let last_case = add_bindings_statement bindings statement in
              last_case, rev_cases
          | _ ->
              let pos = Q.Pos.expr body in
              let default_case = JsCons.Statement.expr (Common.ClientLib.match_failure pos) in
              default_case, rev_cases
        in
        (*
          Construct a sequential if then else
        *)
        let result =
          let fold else_stmt (guard, bindings, right_hand) =
            let then_stmt = add_bindings_statement bindings right_hand in
            match guard with
            | Some guard ->
                JsCons.Statement.if_ guard then_stmt else_stmt
            | None ->
                then_stmt
          in
          List.fold_left fold last_case rev_cases
        in
        let result = may_alias_matched_end (fun alias result -> JsCons.Statement.block [ alias ; result ]) alias result in
        private_env, result
      )

    | _ ->
        let private_env, expr = compile_expr_to_expr env private_env body in
        private_env, JsCons.Statement.return expr

  in
  let private_env, stm = aux private_env body in
  match recursion_info.case_ident with
  | None -> private_env, stm
  | Some _ ->
      let comment = JsCons.Statement.comment ~kind:`one_line ("body of " ^ Ident.to_string name) in
      private_env, JsCons.Statement.block [comment;stm]

let wrap_function_body _env private_env recursion_info fun_env (name:Ident.t) body =
  let private_env, init_body = E.maybe_declare_local_vars private_env in
  let params = IdentMap.find name recursion_info.params in
  let name = J.ExprIdent name in
  let body = JsCons.Statement.while_ (JsCons.Expr.true_ ()) body in
  let fun_ =
    match fun_env with
    | None ->
        JsCons.Statement.function_ name params (maybe_cons init_body [body])
    | Some fun_env ->
        JsCons.Statement.function_ name fun_env [
          JsCons.Statement.return (
            JsCons.Expr.function_ None params
              (maybe_cons init_body [body])
          )
        ] in
  private_env, fun_

let wrap_function_bodies _env private_env recursion_info unified_params bodies =
  let name = E.next_param (String.concat_map "_" Ident.original_name (IdentMap.keys recursion_info.index)) in
  let private_env, init_body = E.maybe_declare_local_vars private_env in
  let case_ident = Option.get (recursion_info.case_ident) in
  let switch =
    JsCons.Statement.switch
      (JsCons.Expr.ident case_ident)
      (List.mapi (fun i body -> (JsCons.Expr.int i, body)) bodies) in
  let while_ =
    JsCons.Statement.while_ (JsCons.Expr.true_ ()) switch in
  let body = JsCons.Statement.function_ name (case_ident :: unified_params) (maybe_cons init_body [while_]) in
  private_env, name, body

(* this generate the 'wrapper' that simply calls the recursive function
 * (ie f or g that call f_g in the example above) *)
let define_functions rec_name funs =
  List.mapi
    (fun i (name,fun_env,params,_body) ->
       let params = List.map (fun p -> J.ExprIdent p) params in
       let fun_env =
         match fun_env with
         | None -> None
         | Some fun_env -> Some (List.map (fun p -> J.ExprIdent p) fun_env) in
       let call =
         JsCons.Expr.call ~pure:false
           (JsCons.Expr.ident rec_name)
           (JsCons.Expr.int i :: List.map JsCons.Expr.ident (Option.default [] fun_env @ params)) in
       match fun_env with
       | None ->
           JsCons.Statement.function_ (J.ExprIdent name) params [
             JsCons.Statement.return call
           ]
       | Some fun_env ->
           JsCons.Statement.function_ (J.ExprIdent name) fun_env [
             JsCons.Statement.return (
               JsCons.Expr.function_ None params [JsCons.Statement.return call]
             )
           ]
    ) funs

(* compilation of l, which is a set of mutually recursive functions *)
let compile_function_bodies env private_env l =
  let extract_function = function
    | (i, Q.Lambda (_, params1, Q.Lambda (_, params2, body))) -> (i, Some params1, params2, body)
    | (i, Q.Lambda (_, params, body)) -> (i,None,params,body)
    | (i, expr) ->
        let context = QmlError.Context.annoted_expr env.E.annotmap expr in
        QmlError.i_error None context "@[<2>Invalid recursion on %s@\n@]" (Ident.to_string i) in
    match List.map extract_function l with
    | [] -> assert false
    | [(name,fun_env,params,body)] ->
        let fun_env = (match fun_env with None -> None | Some l -> Some (List.map (fun p -> J.ExprIdent p) l)) in
        let recursion_info = {
          case_ident = None;
          params = IdentMap.add name (List.map (fun p -> J.ExprIdent p) params) IdentMap.empty;
          index = IdentMap.empty;
          number_of_funs = 1;
        } in
        let private_env, body = compile_function_body_aux env private_env recursion_info name body in
        wrap_function_body env private_env recursion_info fun_env name body
    | funs ->
        let case_ident = E.next_param "case_" in
        let max_arity =
          List.fold_left
            (fun old_max (_,fun_env,params,_) ->
               max old_max (Option.default_map 0 List.length fun_env + List.length params))
            (-1) funs in
        let unified_params = List.init max_arity (fun i -> E.next_param ("p" ^ string_of_int i)) in
        let renaming, params =
          List.fold_left
            (fun (renaming,map) (name,fun_env,params,_) ->
               let renaming, params =
                 List.fold_left_partial_map2
                   (fun renaming orig new_ -> IdentMap.add orig new_ renaming, new_)
                   renaming (Option.default [] fun_env @ params) unified_params in
               renaming, IdentMap.add name params map
            ) (private_env.E.renaming, IdentMap.empty) funs in
        let private_env = {private_env with E.renaming} in
        let recursion_info = {
          case_ident = Some case_ident;
          params = params;
          index = List.fold_left_i (fun map (name,_,_,_) i -> IdentMap.add name i map) IdentMap.empty funs;
          number_of_funs = List.length funs;
        } in
        let private_env, bodies =
          List.fold_left_map
            (fun private_env (name,_,_,body) ->
               compile_function_body_aux env private_env recursion_info name body
            ) private_env funs in
        let private_env, rec_name, rec_fun =
          wrap_function_bodies env private_env recursion_info unified_params bodies in
        let not_rec_funs = define_functions rec_name funs in
        private_env, JsCons.Statement.block (rec_fun :: not_rec_funs)

(* this generates some debug when set: it generates error when
 * on of the formal parameters of a function is undefined (which cannot happen
 * in generated code) very early (so that you don't need to track
 * the undefined value by hand by unwinding the stack) *)
let assert_number_arguments __params =
  #<If:JS_IMP$contains "runtimedebug">
    [
      JsCons.Statement.expr (
        JsCons.Expr.call
          ~pure:false
          Imp_Common.ClientLib.assert_length
          [JsCons.Expr.array (List.map JsCons.Expr.exprident __params)]
      )
    ]
  #<Else>
    []
  #<End>

(* compilation of a non recursive function
 * when there are no tail rec calls, you don't need to compile the body of the
 * function to a statement, so it is compiled as an expression only *)
let compile_fun env private_env i ?fun_env params body =
  let private_env, body = compile_expr_to_expr env private_env body in
  let private_env, declaration = E.maybe_declare_local_vars private_env in
  let body = JsCons.Statement.return body in
  let stm =
    match fun_env with
    | None ->
        JsCons.Statement.function_
          (J.ExprIdent i)
          (List.map (fun s -> J.ExprIdent s) params)
          (assert_number_arguments params @ maybe_cons declaration [body])
    | Some fun_env ->
        JsCons.Statement.function_
          (J.ExprIdent i) (List.map JsCons.Ident.ident fun_env) [
            JsCons.Statement.return (
              JsCons.Expr.function_ None
                (List.map (fun s -> J.ExprIdent s) params)
                (assert_number_arguments (fun_env @ params) @ maybe_cons declaration [body])
            )
          ] in
  private_env, stm

(* this directive compiles the nodes that can appear only at the toplevel of
 * expressions *)
let compile_non_rec_declaration env private_env (i,e) =
  #<If:JS_MATCH_COMPILATION $contains "code_elt">
    let pos = Q.Pos.expr e in
    OManager.printf (
      "@{<brigth>%a@}: %s@."
    )
      FilePos.pp pos
      (Ident.stident i)
  #<End>;
  match e with
  | Q.Lambda (_, fun_env, Q.Lambda (_, params, body)) ->
      compile_fun env private_env i ~fun_env params body
  | Q.Lambda (_, params, body) ->
      compile_fun env private_env i params body

  | Q.Directive(_, `hybrid_value, l, _) -> (
      let exprident = i in
      let ident = JsCons.Ident.ident exprident in
      let unser_opt, qml_expr =
        match l with
        | [server] -> None, server
        | [client; server] -> Some client, server
        | _ -> assert false in
      let newident = Ident.refresh exprident in
      let apply =
        if env.E.options.Qml2jsOptions.qml_closure then
          let annotmap = env.E.annotmap in
          let gamma = env.E.gamma in
          fun f args ->
            snd (Pass_Closure.args_apply ~typed:false (gamma, annotmap) f args)
        else
          QmlAstCons.UntypedExpr.apply
      in
      let hole = JsCons.Expr.hole qml_expr in
      match unser_opt with
      | None ->
          let stm = JsCons.Statement.var ident ~expr:hole in
          private_env, stm
      | Some unser ->
          let expr = apply unser [QmlCons.ident newident] in
          let private_env, expr = compile_expr_to_expr env private_env expr in
          let stm =
            JsCons.Statement.block [
              JsCons.Statement.var ident ~expr ;
              JsCons.Statement.var (JsCons.Ident.ident newident) ~expr:hole ;
            ]
          in
          private_env, stm
    )

  | _ ->
      let private_env, e = compile_expr_to_expr env private_env e in
      let private_env, o = E.maybe_declare_local_vars private_env in
      let e =
        match o with
        | None -> e
        | Some declaration ->
            (* the expression has local vars, we must wrap it inside
             * a function() { ... }() to open a new scope *)
            JsCons.Expr.call ~pure:false
              (JsCons.Expr.function_ None [] [declaration; JsCons.Statement.return e])
              []
      in
      let stm = JsCons.Statement.var (J.ExprIdent i) ~expr:e in
      private_env, stm

let compile_non_rec_declarations env private_env iel =
  let private_env, statements =
    List.fold_left_map (compile_non_rec_declaration env) private_env iel in
  private_env, JsCons.Statement.block statements

let compile_code_elt env private_env code_elt =
  assert (private_env.E.local_vars = []);
  assert (private_env.E.renaming = IdentMap.empty);
  let private_env, res =
  match code_elt with
  | Q.NewVal (_,iel) ->
      compile_non_rec_declarations env private_env iel
  | Q.NewValRec (_,l) ->
      let groups = analyse_tail_recursion l in
      let private_env, statements =
        List.fold_left_map
          (fun private_env kind ->
             let private_env, res =
               match kind with
               | `no_recursion binding ->
                   compile_non_rec_declaration env private_env binding
               | `self_recursion binding ->
                   compile_function_bodies env private_env [binding]
               | `mutual_recursion bindings ->
                   compile_function_bodies env private_env bindings in
             (* it is needed to reset the renaming because the same variable can appear in
              * recursive block where it is renamed (to p0, p1 etc.) and in an other block
              * where it is not renamed. (and since this is the same identifier, the renaming
              * will get confused (happened with ei generated types)) *)
             E.reset_renaming private_env, res
          ) private_env groups in
      private_env, JsCons.Statement.block statements
  | Q.NewType _ -> private_env, JsCons.Statement.block []
  | Q.NewDbValue _
  | Q.Database _ -> assert false (* slicing error if that happens *)
  in
  E.reset_renaming private_env, res

let is_distant env i =
  (* FIXME: factorize with qmlClosure *)
  try
    ignore (QmlRenamingMap.new_from_original env.E.renaming_server (QmlRenamingMap.original_from_new env.E.renaming_client i));
    true
  with Not_found -> false

let distant_identifier env acc = function
  | Q.NewVal (_, iel)
  | Q.NewValRec (_, iel) ->
      List.fold_left
        (fun acc (i,e) ->
           match e with
           | Q.Lambda _ when is_distant env i -> i :: acc
           | _ -> acc
        ) acc iel
  | Q.NewType _ -> acc
  | Q.NewDbValue _
  | Q.Database _ -> assert false

let set_distant_identifiers identifiers =
  JsCons.Statement.expr (JsCons.Expr.runtime (JsAstRuntime.SetDistant (List.map JsCons.Ident.ident identifiers)))

let compile env private_env code =
  let private_env, js_code = List.fold_left_map (compile_code_elt env) private_env code in
  let distant_identifiers = List.fold_left (distant_identifier env) [] code in
  let js_code_elt = set_distant_identifiers distant_identifiers in
  private_env, js_code_elt :: js_code
