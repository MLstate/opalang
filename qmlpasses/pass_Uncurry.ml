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
module Q = QmlAst
module List = Base.List

type ignored_directive = Q.type_directive

(* for each top level function, we record
   1-its defining identifier,
   2-the name of its closure
   3-its arity
   4-its typescheme (as found in \Gamma)
*)
type func_info = {
  arity: int;
  code : Ident.t; (* name of the function *)
  closure : Ident.t; (* name of the closure *)
  used : bool ref; (* shared between a real function and its aliases,
                    * which would not be possible if the field was mutable *)
  tsc : QmlTypes.typescheme option; (* None everywhere in untyped mode
                                     * Some everywhere in typed mode *)
  from_current_package : bool; (* true if the closure is defined in the current package *)
}

let _count_closures = ref 0
let _count_rec_closures = ref 0
let _count_cleaned = ref 0
let _count_rec_cleaned = ref 0

(* the global knowledge *)
type env = { funcs: func_info IdentMap.t }

module S =
struct
  type t = env
  let pass = "qmlUncurry"
  let pp f {funcs} =
    IdentMap.iter
      (fun _ {arity;code;closure} ->
         Format.fprintf f "%s: arity: %d, clos %s@\n" (Ident.to_string code) arity (Ident.to_string closure)
      ) funcs
end

module R =
struct
  include ObjectFiles.MakeClientServer(S)
  let load ~side env =
    let funcs =
      fold_with_name ~side
        (fun package acc env ->
           let refreshed_env =
             IdentMap.map
               (fun r ->
                  assert r.from_current_package;
                  {r with
                     from_current_package = false;
                     tsc = Option.map (QmlRefresh.refresh_typevars_from_tsc package) r.tsc
                  })
               env.funcs in
           IdentMap.safe_merge acc refreshed_env
        ) env.funcs in
    {funcs = funcs}
end

(* utility function *)
let get_ty annotmap ann =
  QmlAnnotMap.find_ty ann annotmap

(* classify between the three kind of objects that we deal with
 * - functions
 * - aliases on function identifier (as an optimization)
 * - everything else (we don't care about it)
 *)
let classify e =
  let o =
    QmlAstWalk.Expr.traverse_findmap
      (fun tra e ->
         match e with
         | Q.Lambda _ -> Some `lambda
         | Q.Ident (_, i) -> Some (`alias i)
         | Q.Coerce _
         | Q.Directive(_, #ignored_directive, _, _) -> tra e
         | _ -> None) e in
  Option.default `nothing o

(* a special of the previous function: returns whether e is a function or not *)
let is_lambda e =
  QmlAstWalk.Expr.traverse_exists
    (fun tra e ->
       match e with
       | Q.Lambda _ -> true
       | Q.Coerce _
       | Q.Directive (_, #ignored_directive, _, _) -> tra e
       | _ -> false) e

(* returns the arity of a function *)
let get_arity e =
  let o =
    QmlAstWalk.Expr.traverse_findmap
      (fun tra e ->
         match e with
         | Q.Lambda (_, l, _) -> Some (List.length l)
         | Q.Coerce _
         | Q.Directive (_, #ignored_directive, _, _) -> tra e
         | _ -> None) e in
  Option.default 0 o

(* rewrite code by inserting closure_create and closure_apply, etc. directives *)
(* build the directive `closure_create using stored information in func_info *)
let closure_create cons label info =
  #<If:CLOSURE_STAT>incr _count_closures#<End>;
  let expr = Q.Directive (label, `closure_create(info.code, info.arity, info.tsc),[],[]) in
  cons#make_from_annot expr (Annot.annot label)

(* build the directive `closure_apply *)
let closure_apply cons label f args =
  let expr = Q.Directive (label, `closure_apply, (f :: args), []) in
  cons#make_from_annot expr (Annot.annot label)

(* build the closure without the implementation, for recursive definitions *)
let closure_create_no_function cons label info =
  #<If:CLOSURE_STAT>incr _count_rec_closures#<End>;
  let expr = Q.Directive (label, `closure_create_no_function (info.code,info.arity,info.tsc), [], []) in
  cons#make_from_annot expr (Annot.annot label)

(* define the implementation of an existing closure, for use on closure defined with the above line *)
let closure_define_function cons label info =
  let expr = Q.Directive (label, `closure_define_function (info.closure, info.code, info.tsc), [],[]) in
  let void_ty = Q.TypeRecord (Q.TyRow ([], None)) in
  cons#make expr void_ty

(* get the identifier of the closure identified by info *)
let get_closure_ident cons annot info =
  info.used := true;
  cons#ident_from_annot info.closure annot


(* register each function in the environment *)
(* register one function val x = body
   we compute the arity from body
   and get the typescheme from \Gamma *)
let make_clos ident =
  Ident.refresh ~map:(fun s -> "clos_"^s) ident
let register_function cons env (x,body) =
  let n = get_arity body in
  let tsc =
    if cons#typed then
      Some (QmlTypes.Env.Ident.find x cons#gamma)
    else
      None in
  let info =
    { arity = n
    ; code = x
    ; closure = make_clos x
    ; used = ref false
    ; tsc = tsc
    ; from_current_package = true } in
  let funcs = IdentMap.add x info env.funcs in
  { funcs = funcs }, info

(* Do not spamm the list manipulated by the compiler with dummy nodes *)
let cons_dec code_elt code =
  match code_elt with
  | Q.NewVal (_, [])
  | Q.NewValRec (_, []) -> code
  | _ -> code_elt :: code

(* go through the code and register each function
 * and add the closure definitions in the code *)
let register_code_elt cons env elt =
  match elt with
  | Q.NewVal (label, bnds) as c ->
      (* [val f(x) = 2
       *  and g(y) = 2]
       *
       * becomes
       *
       * [val f(x) = 2
       *  and g(y) = 2
       *  val clos_f = closure_create f 1 "f"
       *  and clos_g = closure_create g 1 "g"]
       *)
      let env, clos_bnds =
        List.fold_left_filter_map
          (fun env ((i,body) as fun_bnd) ->
             match classify body with
             | `nothing ->
                 env, None
             | `alias ident -> (
                 try
                   (* when we have an alias g = f where f is a lambda
                    * we also create an alias clos_g = clos_f and we won't rewrite g = f
                    * so that the implementation is still reachable *)
                   let info = IdentMap.find ident env.funcs in
                   if info.from_current_package then
                     (* avoiding introducing useless aliases and also avoid confusing
                      * the cleaning below (because we don't clean the aliases introduced
                      * so we should make sure that they never have to been cleaned in
                      * the first place) *)
                     let env = {funcs = IdentMap.add i info env.funcs} in
                     env, None
                   else (
                     let clos_ident = make_clos i in
                     let new_info =
                       (* same arity, tsc, used *)
                       {info with code = i; closure = clos_ident; from_current_package = true} in
                     let code_elt = (clos_ident, cons#ident_from_annot info.closure (Q.QAnnot.expr body)) in
                     let env = {funcs = IdentMap.add i new_info env.funcs} in
                     env, Some code_elt
                   )
                 with Not_found ->
                   (* not an alias to a toplevel function *)
                   env, None
               )
             | `lambda ->
                 let env, info = register_function cons env fun_bnd in
                 let expr = closure_create cons (Q.Label.expr body) info in
                 let closure_decl = (info.closure, expr) in
                 env, Some closure_decl)
          env
          bnds in
      env, (cons_dec c (cons_dec (Q.NewVal (label, clos_bnds)) []))
  | Q.NewValRec (label, bnds) as c ->
      (* [val rec f(x) = g(x)
       *  and     g(y) = f(y)]
       *
       * becomes
       *
       * [val clos_f = closure_create_no_function 1 "f"
       *  and clos_g = closure_create_no_function 1 "g"
       *  val rec f(x) = g(x)
       *  and     g(y) = f(y)
       *  val _ = closure_define_function clos_f f
       *  and _ = closure_define_function clos_g g]
       *)
      (* no need to look for aliases, there can't be any in letrec *)
      let fun_bnds = List.filter (fun (_,e) -> is_lambda e) bnds in
      let env, closure_decl =
        List.fold_left_map
          (fun env ((_,body) as fun_bnd) ->
             let env, info = register_function cons env fun_bnd in
             let expr = closure_create_no_function cons (Q.Label.expr body) info in
             let pre_closure_decl = (info.closure, expr) in
             let expr = closure_define_function cons (Q.Label.expr body) info in
             let post_closure_decl = (Ident.next "_", expr) in
             env, (pre_closure_decl, post_closure_decl))
          env
          fun_bnds in
      let pre_closure_decls, post_closure_decls = List.split closure_decl in
      env, cons_dec
        (Q.NewVal (label, pre_closure_decls))
        (cons_dec c (cons_dec (Q.NewVal (label, post_closure_decls)) []))
    | c -> env, [c]


(* the rewriting process:
   it basically rewrites each Apply node into a `closure_apply directive
   there are two exceptions:
   1- bypass:
   After BypassHoisting pass, it is guaranteed that each bypass application is total.
   Some care must be taken if bypass are allowed to take qml functions as argument.

   2- non partial calls of toplevel functions.

   Hence, each remaining Apply node in the code is guaranteed to be non partial.
*)
let rewrite_expr cons env e =
  QmlAstWalk.Expr.self_traverse_map
    (fun self tra e ->
       match e with
       | Q.Ident (_, x) -> (
           try
             let func_info = IdentMap.find x env.funcs in
             get_closure_ident cons (Q.QAnnot.expr e) func_info
           with Not_found -> e
         )

       | Q.Directive (label2, `partial_apply missing, (Q.Apply (_, Q.Ident (label, x), args) :: more_args), []) ->
           let args = List.map self args in
           let more_args = List.map self more_args in
           let func_info =
             try IdentMap.find x env.funcs
             with Not_found ->
               OManager.i_error "Partial application on %s, which is not in env.funcs" (Ident.to_string x) in
           (*
             we can only do partial application on toplevel lambdas (syntactically)
             because only the lambda lifting introduces such cases
           *)
           let f = get_closure_ident cons (Annot.annot label) func_info in
           Q.Directive (label2, `partial_apply missing, (closure_apply cons label2 f args :: more_args), [])

       | Q.Directive (_, `partial_apply _, _, _) -> assert false

       | Q.Apply (label, (Q.Ident (_, x) as f), args) -> (
           (*
             since we are in n-ary, this case can be either a full application of a toplevel function
             or the application of a closure to its argument
           *)
           let args = List.map self args in
           match IdentMap.find_opt x env.funcs with
           | None ->
               closure_apply cons (Q.Label.expr e) f args

           | Some func_info ->
               (* full application *)
               assert (
                 List.length args = func_info.arity ||
                   (Format.printf "%d arguments given vs arity of %d on %s in %a@."
                      (List.length args) func_info.arity (Ident.to_string x) QmlPrint.pp#expr e; false)
               );
               Q.Apply (label, f, args)
         )

       | Q.Apply (_, (Q.Bypass _ | Q.Directive (_, (`may_cps | `restricted_bypass _), _, _)), _) ->
           (*
             bypass application, it must be full
             FIXME: assert it
           *)
           tra e

       | Q.Apply (label, f, args) ->
           let args = List.map self args in
           let f = self f in
           closure_apply cons label f args

       | _ ->
           tra e

    ) e

(* some 'specialized' cleaning for closures
 * since we know which closure we used and which one was useless, there is no
 * to go through the whole code to find that information
 * it is a good idea to clean useless since we create them for every toplevel function
 * when often, they aren't useful because the function is always fully applied *)
let clean_binding ~can_be_cleaned env (_,e) =
  match e with
  | Q.Directive (_, `closure_create (i, _, _), _, _) ->
      let info = IdentMap.find i env.funcs in
      let kept = !(info.used) || not (can_be_cleaned i) in
      #<If:CLOSURE_STAT> if not kept then incr _count_cleaned#<End>;
      kept
  | Q.Directive (_, `closure_create_no_function (i, _, _), _, _)
  | Q.Directive (_, `closure_define_function (_, i, _), _, _) ->
      let info = IdentMap.find i env.funcs in
      let kept = !(info.used) || not (can_be_cleaned i) in
      #<If:CLOSURE_STAT> if not kept then incr _count_rec_cleaned#<End>;
      kept
  | _ ->
      true

let clean_code ~can_be_cleaned env code =
  QmlAstWalk.Code.filter_binding (clean_binding ~can_be_cleaned env) code

(* initial env
   for separate compilation: it must be populated initially
*)
let empty_env = { funcs = IdentMap.empty }

(* register each toplevel functions *)
let register_code cons env code =
  List.fold_left_collect (register_code_elt cons) env code

let rewrite_bnd cons env ((i,e) as bnd) =
  match classify e with
  | `alias i when IdentMap.mem i env.funcs -> bnd
  | _ -> (i,rewrite_expr cons env e)

(* rewrite the entire code *)
let rewrite_code cons env code =
  QmlAstWalk.CodeExpr.map_name_expr (fun bnd -> rewrite_bnd cons env bnd) code

(* by default, since we don't have separate compilation
 * we allow ourselves to clean every unused closures
 * FIXME we should only clean the closures that cannot be accessed
 * by other modules (ie from local functions, or from functions hidden by the signatures) *)
let process_code ?(can_be_cleaned=fun _ -> ObjectFiles.compilation_mode () = `linking) ~side ~typed gamma annotmap code =
  #<If:CLOSURE_STAT>
    _count_closures := 0;
    _count_cleaned := 0;
  #<End>;
  let cons, get_state = QmlAstCons.make_cons ~typed gamma annotmap in
  let loaded_env = R.load ~side empty_env in
  let computed_env, code = register_code cons loaded_env code in
  let diff_env = {funcs = IdentMap.diff computed_env.funcs loaded_env.funcs} in
  R.save ~side diff_env;
  let exported_map =
    IdentMap.fold
      (fun _ {closure; code} acc -> IdentMap.add closure code acc)
      diff_env.funcs
      IdentMap.empty in
  let code = rewrite_code cons computed_env code in
  let code = clean_code ~can_be_cleaned computed_env code in
  #<If:CLOSURE_STAT>
    let c = !_count_closures in
    let rc = !_count_rec_closures in
    let cleaned = !_count_cleaned in
    let rcleaned = !_count_rec_cleaned / 2 in (* the counter was increment once when removing
                                               * the @closure_define and @closure_create_no_function  *)
    let total = c + rc in
    let total_cleaned = cleaned + rcleaned in
    let percentage c t = if t = 0 then " - " else Printf.sprintf "%2d%%" (c * 100 / t) in
    Printf.printf "Total closures created: %d\n" total;
    Printf.printf "  Simple    closure:    %d\t%s\n" c (percentage c total);
    Printf.printf "  Recursive closure:    %d\t%s\n" rc (percentage rc total);
    Printf.printf "Total closures removed: %d\t%s\n" total_cleaned (percentage total_cleaned total);
    Printf.printf "  Simple closure:       %d\t%s\n" cleaned (percentage cleaned c);
    Printf.printf "  Recursive closure:    %d\t%s\n%!" rcleaned (percentage rcleaned rc)
  #<End>;
  get_state (), exported_map, code
