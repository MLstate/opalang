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

(* @author Esther Baruk
   Please see the .mli for more informations about this module

   Beware that it is necessary to do rewriting and analysis at the same time
   or else you will miss cases where you should have rewritten:
   m1 = {{ m2 = {{ x = ... }} }}
   m3 = m1.m2 // if haven't rewritten m1.m2 to an identifier, then you don't know that is an
              // alias on a module
   _ = m3.x
 *)

module Q = QmlAst
module List = Base.List

type env = {
  modules : Q.expr StringMap.t IdentMap.t; (* maps module identifiers to maps of their field *)
  aliases : Ident.t IdentMap.t; (* maps identifiers to an identifier of module, shorcuting any aliases:
                                 * m = {{ ... }}
                                 * m1 = m
                                 * m2 = m1
                                 * generates the map:
                                 * m1 -> m
                                 * m2 -> m
                                 *)
}

type ignored_directives = [ Q.type_directive | Q.slicer_directive | `workable ]

let fold_expr f acc env =
  IdentMap.fold (fun _ fields acc -> StringMap.fold (fun _ e acc -> f acc e) fields acc) env.modules acc

let empty_env = {
  modules = IdentMap.empty;
  aliases = IdentMap.empty;
}

let add_to_aliases id m env = { env with aliases = IdentMap.add id m env.aliases }

let find_module ident env =
  if IdentMap.mem ident env.modules then
    ident
  else
    IdentMap.find ident env.aliases
let find_module_field ident field env =
  let module_ = find_module ident env in
  try StringMap.find field (IdentMap.find module_ env.modules)
  with Not_found -> assert false

(* If an identifier is present in the given expression, it returns the expr containing it
   -- It first returned an ident but we had to preserve the annotations for a later use
      so it returns the corresponding expr --
   else it returns None *)
let get_id_in_expr e =
  let rec aux acc e = QmlAstWalk.Expr.traverse_fold
    (fun tra id e ->
      match e with
      | Q.Ident _ ->
          Some e
      | Q.Apply (_, expr, _)
      | Q.Lambda (_, _, expr) ->
          (match aux acc expr with
            | Some _ as acc_ -> acc_
            | None -> None)
      | _ -> tra id e)
    acc
    e
  in aux None e

(* Stores in the environment the module fields and the corresponding toplevel aliases *)
let collect_declarations module_name env expr =
  match expr with
    | Q.Record (_, field_list) ->
        let field_list =
          List.map
            (fun (field,expr) ->
               let id =
                 let id_ = get_id_in_expr expr in (* get the toplevel alias of the field *)
                 (match id_ with Some e -> e | _ -> assert false) in
               (field, id)
            ) field_list in
        let stringmap = StringMap.from_list field_list in
        {env with modules = IdentMap.add module_name stringmap env.modules}
    | _ -> assert false

(* This function is called on an expression from a NewVal
   If the given expression contains a module, then it collects its identifiers and add
   them to the environment (see the collect_declarations function)
   Else it doesn't have any effects on the environment *)
let rec inspect_module env (name,e) =
  match e with
  | Q.Directive (_, `module_, [expr], _) ->
      collect_declarations name env expr
  | Q.Coerce (_, e, _)
  | Q.Directive (_, `deprecated, [ _ ; e ], _)
  | Q.Directive (_, #ignored_directives, [e], _) ->
      inspect_module env (name, e)
  | Q.Directive (_, `deprecated, _, _)
  | Q.Directive (_, #ignored_directives, _, _) ->
      assert false
  | Q.Ident (_, id) -> (
      try
        let module_ = find_module id env in
        {env with aliases = IdentMap.add name module_ env.aliases}
      with Not_found ->
        env
    )
  | _ -> env

let analyze_code_elt env = function
  | QmlAst.NewVal (_, decl_list) ->
      List.fold_left inspect_module env decl_list
  | _ ->
      (* cannot have module, nor aliases in recursive bindings *)
      env


(* Useful to take the ident from an expr *)
let extract_ident e = match e with
  | Q.Ident (_, id) -> id
  | _ -> assert false

(* The main function that does the substitutions
   If we have a dot (i.e. module.field) it search its toplevel alias in the ident_map.
   If it exists then it's replaced by it, else the expression remains the same *)
let rewrite_code_elt env annotmap code_elt =
  let annotmap, code =
    QmlAstWalk.CodeExpr.fold_map
      (QmlAstWalk.Expr.foldmap_up
         (* need to go up because to rewrite A.B.x, you first rewrite A.B into B1 and B1.x in x1 *)
         (fun annotmap e ->
            match e with
            | Q.Dot (label, expr, s) -> (
                match get_id_in_expr expr with
                | Some id ->
                    let module_name = extract_ident id in (
                      try
                        (* get the alias corresponding to this call (it's an expr) *)
                        let toplevel_alias = find_module_field module_name s env in
                        let id_ = extract_ident toplevel_alias in
                        (* the tsc_inst needed at the call site need to be the same as the one in the
                         * field and not the one that was originally there because the toplevel definition
                         * can expect typevars while the field definition doesn't (even if this is just
                         * an alias, because of phantom types) *)
                        let tsc_inst_opt =
                          QmlAnnotMap.find_tsc_inst_opt (Q.QAnnot.expr toplevel_alias) annotmap in
                        let annotmap =
                          QmlAnnotMap.add_tsc_inst_opt (Q.QAnnot.expr e) tsc_inst_opt annotmap in
                        let id = QmlAst.Ident (label, id_) in
                        annotmap, id
                      with Not_found ->
                        (* that was not a module name (maybe it's just a record or a functor) *)
                        annotmap, e
                    )
                | None ->
                    annotmap, e
              )
            | _ ->
                annotmap, e
         )
      ) annotmap [code_elt] in
  annotmap, List.get_only_element code

let analyse_and_rewrite_code_elt (env, annotmap) code_elt =
  let annotmap, code_elt = rewrite_code_elt env annotmap code_elt in
  let env = analyze_code_elt env code_elt in
  (env, annotmap), code_elt

let analyse_and_rewrite_code env annotmap code =
  List.fold_left_map analyse_and_rewrite_code_elt (env, annotmap) code

module S =
struct
  type t = env *  QmlAst.annotmap
  let pass = "undot"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = ObjectFiles.Make(S)

let load_env (env, annotmap) =
  R.fold_with_name
    (fun package (env, annotmap) (old_env, old_annotmap) ->
       let old_annotmap = QmlRefresh.refresh_annotmap package old_annotmap in
       let annotmap, modules =
         IdentMap.fold_map
           (fun _k v annotmap ->
              StringMap.fold_map
                (fun _i e annotmap ->
                   QmlRefresh.refresh_expr package ~annotmap_old:old_annotmap annotmap e
                ) v annotmap
           ) old_env.modules annotmap in
       let modules = IdentMap.safe_merge env.modules modules in
       let aliases = IdentMap.fold
         (fun alias module_ aliases ->
            if IdentMap.mem module_ modules then
              IdentMap.add alias module_ aliases
            else
              (* filtering out aliases that refer to packages that are not in the direct deps *)
              aliases
         ) old_env.aliases env.aliases in
       ({modules; aliases}, annotmap)
    ) (env, annotmap)

let save_env ~new_env ~env ~annotmap =
  let modules = IdentMap.diff new_env.modules env.modules in
  let aliases = IdentMap.diff new_env.aliases env.aliases in
  let small_env = {modules; aliases} in
  let small_annotmap = QmlRefresh.restrict_annotmap_fold_expr fold_expr annotmap small_env in
  R.save (small_env, small_annotmap)

let process_code gamma annotmap code =
  let env, annotmap = load_env (empty_env, annotmap) in
  let (new_env, annotmap), code = analyse_and_rewrite_code env annotmap code in
  save_env ~new_env ~env ~annotmap;
  (gamma, annotmap), code
