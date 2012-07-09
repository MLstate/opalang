(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

(*
  Please see .mli for more informations about this module
  @author Esther Baruk
  @author Mathieu Barbin
*)

(* refactoring *)

(* depends *)
module Hashtbl = Base.Hashtbl
module List = Base.List

(* shorthand *)
module Q = QmlAst

(*
  Note: This union-find structure is needed by the compare function, which does not take an
  extra env in argument. We use a global ref which is set with the value shared_toplevel of
  the env before each call to share_constants.
  [imperative_structure_traversing_alias] is a map to a union-find structure for
  traversing alias, and shared some more pattern of code without being lost because of alias.

  example which explain the motivation of the structure :
  {[
  d0 = { a = 4.65 ; b = "titi" }
  e0 = { a = d0 ; g = { g } }

  d1 = { a = 4.65 ; b = "titi" }
  e1 = { a = d1 ; g = { g } }

  * constant-sharing *

  ?? e0 == e1 ??
  ]}
*)
type traversing_alias = (Ident.t, unit) UnionFind.t IdentMap.t

module IHashtbl = Hashtbl.Make (Ident)
let imperative_structure_traversing_alias = IHashtbl.create 10
let reset_traversing_alias () = IHashtbl.clear imperative_structure_traversing_alias

let map_alias id =
  try
    let set = IHashtbl.find imperative_structure_traversing_alias id in
    UnionFind.key set
  with
    (* IHashtbl.find raises Not_found *)
  | Not_found -> id

let resolve_alias expr = QmlAstWalk.Expr.map
  (fun e ->
     match e with
     | Q.Ident (label, id) ->
         let f_id = map_alias id in
         if id == f_id then e else Q.Ident (label, f_id)
     | _ -> e) expr

let find_or_create source =
  try
    IHashtbl.find imperative_structure_traversing_alias source
  with
  | Not_found ->
      let set = UnionFind.make source () in
      IHashtbl.add imperative_structure_traversing_alias source set ;
      set

let define_alias alias source =
  let source_set = find_or_create source in
  let alias_set = find_or_create alias in
  UnionFind.replace ~replaced:alias_set ~keeped:source_set

(* Ordered expressions *)
module Expr =
struct
  type t = QmlAst.expr

  (* Sub ast of QML, handled for constant sharing.
     Coerce < Record < Ident < Const < else *)
  let compare x y =
    let rec aux x y =
      match x, y with
      | Q.Coerce (_, e1, _), Q.Coerce (_, e2, _)  -> aux e1 e2
      | Q.Coerce _, _ -> -1
      | _, Q.Coerce _ -> 1
      | Q.Record (_, fds), Q.Record (_, fds') ->
          let field (f, e) (f', e') =
            let r = String.compare f f' in
            if r <> 0 then r else
              aux e e'
          in
          Base.List.make_compare field fds fds'
      | Q.Record _, _ -> -1
      | _, Q.Record _ -> 1
      | Q.Ident (_, id1), Q.Ident (_, id2) ->
          let id1 = map_alias id1 in
          let id2 = map_alias id2 in
          Ident.compare id1 id2
      | Q.Ident _, _ -> -1
      | _, Q.Ident _ -> 1
      | Q.Const (_, c), Q.Const (_, c') -> Pervasives.compare c c'
      | Q.Const _, _ -> -1
      | _, Q.Const _ -> 1
      | _ -> assert false (* internal error of this module *)
    in aux x y
end

(* A map of expressions *)
module ExprMap : BaseMapSig.S with type key = QmlAst.expr = BaseMap.Make (Expr)

type options = {
  no_string_sharing : bool;
  no_float_sharing : bool;
  no_record_sharing: bool;
  remove_coerce : bool;
}

(*
  Make options from side
*)
let make_options side =
  match side with
  | `server -> {

      no_string_sharing =
        #<If:CONST_SHARING_SERVER_STRING $equals "0">
          true
        #<Else>
          false
        #<End>
      ;

      no_float_sharing =
        #<If:CONST_SHARING_SERVER_FLOAT $equals "0">
          true
        #<Else>
          false
        #<End>
      ;

      no_record_sharing =
        #<If:CONST_SHARING_SERVER_RECORD $equals "0">
          true
        #<Else>
          false
        #<End>
      ;

      remove_coerce =
        #<If:CONST_SHARING_SERVER_REMOVE_COERCE $equals "0">
          false
        #<Else>
          true
        #<End>
      ;
    }
  | `client -> {
      no_string_sharing =
        #<If:CONST_SHARING_CLIENT_STRING $equals "0">
          true
        #<Else>
          false
        #<End>
      ;

      no_float_sharing =
        #<If:CONST_SHARING_CLIENT_FLOAT $equals "0">
          true
        #<Else>
          false
        #<End>
      ;

      no_record_sharing =
        #<If:CONST_SHARING_CLIENT_RECORD $equals "0">
          true
        #<Else>
          false
        #<End>
      ;

      remove_coerce =
        #<If:CONST_SHARING_CLIENT_REMOVE_COERCE $equals "0">
          false
        #<Else>
          true
        #<End>
      ;
    }

(*
  Type of the environment.
  + *constants* is a map of expressions so as to find easily the identifier related to a constant
  + *decls* is the map of identifiers so as to store the toplevel declarations to be added at
  the beginning of the code (in which all constants were replaced by their identifier)
*)
type env = {
  options : options ;
  constants : Ident.t ExprMap.t ;
  decls : QmlAst.expr IdentMap.t ;
}

(* Empty environment
   constant is a string map (ident, value)
   defs is a map which contains the toplevel definition (ident, expr) *)
let empty_env options = {
  options = options ;
  constants = ExprMap.empty ;
  decls = IdentMap.empty ;
}

let is_constant env e =
  let rec aux in_record e =
    match e with
    | Q.Ident (_, id) when in_record -> let id = map_alias id in IdentMap.mem id env.decls
    | Q.Coerce (_, expr, _) when not env.options.remove_coerce -> aux in_record expr
    | Q.Const (_, Q.String _) when not env.options.no_string_sharing -> true
    | Q.Const (_, Q.Float _) when not env.options.no_float_sharing -> true
    | Q.Const (_, Q.Int _) when in_record -> true
    | Q.Record (_, []) when not env.options.no_record_sharing -> true
    | Q.Record (_, [_, expr]) when not env.options.no_record_sharing -> aux true expr
    | Q.Record (_, l) when not env.options.no_record_sharing ->
        List.for_all (fun (_,e) -> aux true e) l
    | _ -> false
  in aux false e

(* Define an new identifier for the constant, register it
   in the environment (into the field [decls] and add it to gamma *)
let make_toplevel_decl ~typed gamma annotmap env const ty =
  let id = Ident.next "const" in
  let annotmap, const =
    let rec aux const =
      match const with
      | Q.Const (_, c) -> if typed then QmlAstCons.TypedExpr.const annotmap c else annotmap,QmlAstCons.UntypedExpr.const c
      | Q.Record (_, l) -> if typed then QmlAstCons.TypedExpr.record annotmap l else annotmap,QmlAstCons.UntypedExpr.record l
      | Q.Coerce (_, e, ty) when not env.options.remove_coerce ->
          let a, c = aux e in
          if typed then QmlAstCons.TypedExpr.coerce a c ty
          else a,QmlAstCons.UntypedExpr.coerce c ty
      | _ -> assert false
    in aux const
  in
  let const = resolve_alias const in
  let consts_ = ExprMap.add const id env.constants in
  let decls_ = IdentMap.add id const env.decls in
  let gamma =
    if typed then
      let tsc = QmlTypes.Scheme.generalize gamma (Option.get ty) in
      QmlTypes.Env.Ident.add id tsc gamma
    else gamma
  in
  let annotmap, id =
    if typed then QmlAstCons.TypedExpr.ident annotmap id (Option.get ty)
    else annotmap,QmlAstCons.UntypedExpr.ident id in
  let env_ = { env with constants = consts_; decls = decls_ } in
    (gamma, annotmap, env_), id

(* Run through the code, create the toplevel declaration and replace
   the constant by its identifier *)
let share_constants ~typed (gamma, annotmap, env) e = QmlAstWalk.Expr.traverse_foldmap
  (fun tra (gamma, annotmap, env) e ->
    let rec aux e =
      match e with
      | Q.Coerce (_, e, _) when env.options.remove_coerce -> aux e
      | _ -> e
    in
    let e = aux e in
    match e with
    | Q.Directive (_, `create_lazy_record, _, _) ->
        (gamma, annotmap, env), e
    | _ ->
        let (gamma, annotmap, env), e = tra (gamma, annotmap, env) e in
        if is_constant env e then
          let ty = if typed then Some (QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap) else None in
          try
            let id = ExprMap.find e env.constants in
            let annotmap, id =
              if typed then
                QmlAstCons.TypedExpr.ident annotmap id (Option.get ty)
              else
                annotmap,QmlAstCons.UntypedExpr.ident id
            in
            (gamma, annotmap, env), id
          with Not_found ->
            make_toplevel_decl ~typed gamma annotmap env e ty
        else
          (gamma, annotmap, env), e
  )
  (gamma, annotmap, env)
  e

(* Add the toplevel declarations to the code *)
let add_decls env code =
  IdentMap.fold_rev
    (fun x const code ->
       let label = Annot.nolabel "QmlConstantSharing.add_decls" in
       Q.NewVal (label, [x, const]) :: code)
    env.decls
    code

let process_code ~side ~typed gamma annotmap code =
  let options = make_options side in
  reset_traversing_alias () ;
  let env = empty_env options in
  let (gamma, annotmap, env), code =
    List.fold_left_map_stable
      (fun acc code_elt ->
         match code_elt with
         | Q.NewVal (label, bindings) ->
             let acc, shared_bindings = List.fold_left_map_stable
               (fun acc ((id, expr) as bind) ->
                  let acc, shared_expr = share_constants ~typed acc expr in
                  let _ =
                    match shared_expr with
                    | Q.Ident (_, const) -> (
                        (* store this alias *)
                        define_alias id const;
                      )
                    | _ -> ()
                  in
                  acc,
                  if expr == shared_expr then bind else (id, shared_expr)
               )
               acc bindings in
             acc,
             if bindings == shared_bindings then code_elt
             else Q.NewVal (label, shared_bindings)
         (* Alias or constant in ValRec are not allowed any way *)
         | code_elt ->
             QmlAstWalk.Top.fold_map_expr (share_constants ~typed) acc code_elt
      ) (gamma, annotmap, env) code
  in (gamma, annotmap), (add_decls env code)
