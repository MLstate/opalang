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
(*
  Authors:
  2010, Rudy Sicard
*)

(* depends *)
module List = Base.List

(* shorthands *)
module P = Passes
module Q = QmlAst


let (|>) = InfixOperator.(|>)

exception Partial_application

type 'a macro = {
  name : Ident.t;
  arity : int;
  def : 'a;
}

let macro =
  WarningClass.create
    ~public:true
    ~name:"macro"
    ~doc:"All the macro expansion related warnings"
    ~err:false
    ~enable:true
    ()

let macro_call =
  WarningClass.create
    ~parent:macro
    ~public:true
    ~name:"call"
    ~doc:"Call with a number of argument inconsistant with its definition, will use function call semantics instead"
    ~err:false
    ~enable:true
    ()

let macro_second_order =
  WarningClass.create
    ~parent:macro
    ~public:true
    ~name:"second-order"
    ~doc:"Second order use of a macro, will use function call semantics instead"
    ~err:false
    ~enable:false
    ()

let warning_set =
  WarningClass.Set.create_from_list [
    macro;
    macro_call;
  ]

type env = Q.expr macro IdentMap.t

let werror ~wclass e fmt =
  QmlError.warning ~wclass (QmlError.Context.expr e) fmt



(*** collecting macro definition ***)
let rec make_macro name def =
  match def with
  (* going through coercions does weaken the typing :/ not sure what to do
   * (same reason as the one explained below) *)
  | Q.Coerce (_, e, _) -> make_macro name e
  | Q.Directive (_, #Q.slicer_directive, [e], _) -> make_macro name e
  | Q.Lambda (_, args, _) ->
      { arity = List.length args ; def = def ; name = name }
  | _ ->
      OManager.error "@[<v>%a@]@\n  Trying to define a non lambda macro."
        FilePos.pp_pos (Q.Pos.expr def)


let check_no_expand e =
  QmlAstWalk.Expr.iter
    (function
     | Q.Directive (label, `expand _, _, _) ->
         let context = QmlError.Context.label label in
         QmlError.serror context
           "Illegal @@expand: it can only appear on toplevel bindings."
     | _ -> ()) e

(* give the definition of a potential macro *)
let rec get_directive_macro name e =
  match e with
  | Q.Directive (_, `expand i,[def],_) ->
      (* discarding the directive *)
      check_no_expand def;
      let macro = make_macro name def in
      def, macro, i
  | Q.Directive (_, `expand _, _, _) ->
      OManager.i_error "Macro Expansion, empty expand directive"

  | Q.Coerce (label, expr, ty) ->
      let expr, macro, i = get_directive_macro name expr in
      Q.Coerce (label, expr, ty), macro, i
  (* going through coercions on purpose but not quite as we should
   * if we say @expand f : t = x -> x
   * then we would need to go and look at the type of t to propagate
   * the type constraint on the body and on the parameters
   * (otoh f(x:truc) : machin = ... works ok) *)
  | Q.Directive (label, d, [expr], t) ->
      let expr, macro, i = get_directive_macro name expr in
      Q.Directive (label, d, [expr], t), macro, i

  | _ ->
      check_no_expand e;
      raise Exit



(* on expression, add the definition of  a potential macro *)
let collect_1_macro macro_map ((name, e) as bnd) =
  try
    let def, macro, _i = get_directive_macro name e in
    let macro_map = IdentMap.add name macro macro_map in
    macro_map, (name, def)
  with Exit ->
    macro_map, bnd



(* on code, same *)
let collect_macro code =
  (* we don't need to go through db default values, because these
   * are only identifiers at this stage *)
  QmlAstWalk.CodeExpr.fold_map_name_expr
    collect_1_macro IdentMap.empty code


exception Bad_application
(*** expanding macro call ***)

let error_2nd_order ident e =
  werror ~wclass:macro_second_order e
    "Second order on macro-function '%s', the result will have non lazy semantic" (Ident.original_name ident)

let error_call ident e =
  werror ~wclass:macro_call e
    "Bad application of a macro-function '%s', the result will have non lazy semantic" (Ident.original_name ident)

(* collect substituation associated to applying args on the lambda expression *)
(* What if the returned type is a lambda ? seems to be buggy *)
let get_subst e args subs =
  let fail () = raise Bad_application in
  match e, args with
  | Q.Lambda (_, pars, e), args ->
      if List.length pars <> List.length args then fail () else
        let subs =
          List.fold_left2
            (fun subs par arg -> IdentMap.add par arg subs) subs pars args in
        (subs, e)
  | _ -> fail ()


type 'a refresh_kind =
  | NoRefresh
  | RefreshWith of FilePos.pos
  | RefreshSamePos



let refresh_annot annoto e =
  match annoto with
  | NoRefresh -> e
  | RefreshSamePos ->
      let label = Annot.next_label (Q.Pos.expr e) in
      Q.Label.New.expr e label
  | RefreshWith pos ->
      let label = Annot.next_label pos in
      Q.Label.New.expr e label

let refresh_pat_annot_deep annoto p =
  match annoto with
  | NoRefresh -> p
  | RefreshSamePos ->
      let pos = Q.Pos.pat p in
      QmlAstWalk.Pattern.map_down (fun p ->
          let label = Annot.next_label pos in
          Q.Label.New.pat p label
      ) p
  | RefreshWith pos ->
      QmlAstWalk.Pattern.map_down (fun p ->
          let label = Annot.next_label pos in
          Q.Label.New.pat p label
      ) p



(* same on code
 * the positions in the expanded body of the macro is the position of the call to the macro
 * except for the arguments of the macro that keep their positions
 *)
let expand_code map_to_expand code =
  QmlAstWalk.CodeExpr.map
    (fun e ->
       QmlAstWalk.Expr.self_traverse_map_context_down
         (fun self tra (subst, stack, annoto) e ->
            match e with
            | Q.Apply (label, (Q.Ident (_, i)), args)
                  when IdentMap.mem i map_to_expand -> (
              try
                let macro = IdentMap.find i map_to_expand in
                let count = Option.default 0 (IdentMap.find_opt i stack) in
                if count == 0 then
                  let stack = IdentMap.add i (count+1) stack in
                  let def = QmlAlphaConv.expr QmlAlphaConv.empty macro.def in
                  let (subst, body) = get_subst def args subst in
                  self (subst, stack, RefreshWith (Annot.pos label)) body
                else
                  let e = refresh_annot annoto e in
                  tra (subst, stack, annoto) e
              with Bad_application ->
                error_call i e;
                let e = refresh_annot annoto e in
                tra (subst, stack, annoto) e
             )

             | Q.Ident (_, i) -> (
                try
                  let e = IdentMap.find i subst in
                  let e = QmlAlphaConv.expr QmlAlphaConv.empty e in
                  let e = QmlAstCons.UntypedExpr.copy e in
                  self (subst, stack, RefreshSamePos (* really need to refresh? *)) e
                with Not_found ->
                  if IdentMap.mem i map_to_expand then error_2nd_order i e;
                  let e = refresh_annot annoto e in
                  tra (subst, stack, annoto) e
              )
            | Q.Match (label, e, pel) ->
                let pel =
                  List.map
                    (fun (p, e) -> (refresh_pat_annot_deep annoto p, e))
                    pel in
                let e = Q.Match (label, e, pel) in
                let e = refresh_annot annoto e in
                tra (subst, stack, annoto) e
            | _ ->
                let e = refresh_annot annoto e in
                tra (subst, stack, annoto) e)
         (IdentMap.empty,IdentMap.empty, NoRefresh) e)
    code



(* Utils for separate compilation *)
module S =
struct
  type t = env (* the annotmap only contains the annotations
                                  * of the expressions contained in the environment
                                  * so it is very small *)
  let pass = "pass_MacroExpansion"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = ObjectFiles.Make(S)

(* you cannot have collisions between the names of different packages *)
let refresh package env : _ macro IdentMap.t =
  IdentMap.map
    (fun (macro: _ macro) ->
       let def =
         QmlRefresh.refresh_expr_no_annotmap package macro.def in
       { macro with def = def })
    env

let merge package env1 env2 =
  let env2 = refresh package env2 in
  IdentMap.merge (fun _ _ -> assert false) env1 env2

let save = R.save

(*
   collect macro expansion definition and apply them on code
*)
let process_code code =
  let map_to_expand,code = collect_macro code in
  save map_to_expand; (* saving the macros defined here *)
  let map_to_expand = R.fold_with_name merge map_to_expand in
  (* gathering all the macros
   * don't need to fold on deep dependencies since macro bodies cannot contain macros
   * (they would have been rewritten) *)
  expand_code map_to_expand code

(* the pass version of the previous function *)
let process ~options:(_:OpaEnv.opa_options) (env:'tmp_env P.env_Gen) : 'tmp_env P.env_Gen =
  let code = env.P.qmlAst in
  let code =  process_code code in
  { env with P.qmlAst = code }
