(*
    Copyright © 2011, 2012 MLstate

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

module Format = BaseFormat
module String = BaseString
module List = BaseList

module J = JsAst
module C = JsCons

module S =
struct
  type t = string list (* required opx *)
  let pass = "ServerJavascriptOptimization"
  let pp fmt opx_requires =
    Format.fprintf fmt "opx: %a"
      (Format.pp_list ",@ " Format.pp_print_string) opx_requires
end

module R = ObjectFiles.Make(S)

let export_to_global ident e =
  JsCons.Statement.assign
    (JsCons.Expr.dot ~own_property:false
       (JsCons.Expr.native_global "global")
       (Format.to_string JsPrint.pp#ident ident))
    e

let process_code_elt is_exported = function
  | J.Js_var (_, i, Some e) when is_exported i -> export_to_global i e
  | J.Js_function (l, i, p, b) when is_exported i ->
      export_to_global i (J.Je_function (l, Some i, p, b))
  | x -> x

let cons_require opx =
  JsCons.Statement.expr (
    JsCons.Expr.call ~pure:false
      (JsCons.Expr.native "require")
      [(JsCons.Expr.string opx)]
  )

let process_code extrajs env_bsl is_exported code =
  (* Exports idents to global node scope *)
  let code = List.map (process_code_elt is_exported) code in
  (* Adding require *)

  let is_a_real_deps =
    if ObjectFiles.stdlib_package_names (ObjectFiles.get_current_package_name ()) then
      (fun _ -> true)
    else
      let real_depends =
        List.fold_left
          (JsWalk.TStatement.fold
             (fun real_depends _ -> real_depends)
             (fun real_depends -> function
              | J.Je_ident (_, JsIdent.ExprIdent i) ->
                  begin match Ident.safe_get_package_name i with
                  | None -> real_depends
                  | Some p -> StringSet.add p real_depends
                  end
              | _ -> real_depends)
          ) StringSet.empty code
      in
      (fun opx -> not (ObjectFiles.stdlib_package_names opx) || StringSet.mem opx real_depends)
  in
  let opx_requires =
    ObjectFiles.fold_dir_name ~packages:true
      (fun requires opx name ->
         let opx = Filename.basename opx in
         if is_a_real_deps (fst name) then (
           opx :: requires ) else requires)
      []
  in
  let already_required =
    (R.fold_with_name ~deep:true ~packages:true
       (fun pack k saved_requires ->
          (fun acc ->
             k (
               let pname = fst pack in
               if
                 is_a_real_deps (Filename.basename pname)
                 || StringSet.mem (pname ^ ".opx") acc
               then (
                 StringSet.add_list (List.map Filename.basename saved_requires) acc
               ) else acc)
          )
       ) (fun s -> s)
    ) StringSet.empty
  in
  let opx_requires =
    List.filter
      (fun opx -> not (StringSet.mem opx already_required))
      opx_requires
  in
  R.save opx_requires;
  let opp_requires =
    List.filter_map
      (fun plugin ->
         if List.is_empty plugin.BslPluginInterface.nodejs_code then None
         else plugin.BslPluginInterface.basename
      ) env_bsl.BslLib.direct_external_plugins
  in
  let opp_requires = List.map (Printf.sprintf "%s.opp") opp_requires in
  let extra_requires =
    List.filter_map
      (fun extra_lib ->
         match extra_lib with
         | `server (name, _) -> Some name
         | _ -> None
      ) extrajs
  in
  let code =
    List.rev_map_append
      (fun opx -> cons_require opx)
      opx_requires code
  in
  let code =
    List.rev_map_append cons_require opp_requires code
  in
  let code =
    List.rev_map_append cons_require extra_requires code
  in

  extra_requires @ opp_requires @ opx_requires, code

