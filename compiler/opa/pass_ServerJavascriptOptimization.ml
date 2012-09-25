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

let is_exported exported i = JsIdentSet.mem i exported

let process_code_elt exported = function
  | J.Js_var (_, i, Some e) when is_exported exported i -> export_to_global i e
  | J.Js_function (l, i, p, b) when is_exported exported i ->
      export_to_global i (J.Je_function (l, Some i, p, b))
  | x -> x

let cons_require opx =
  JsCons.Statement.expr (
    JsCons.Expr.call ~pure:false
      (JsCons.Expr.native "require")
      [(JsCons.Expr.string opx)]
  )

let process_code exported code =
  (* Exports idents to global node scope *)
  let code = List.map (process_code_elt exported) code in
  (* Adding require *)

  let is_a_real_deps =
    if ObjectFiles.stdlib_package_names (ObjectFiles.get_current_package_name ()) then
      (fun _ -> true)
    else
      (* Compute real depends in the JavaScript code *)
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
      Format.eprintf "real_depends: %a\n%!" (StringSet.pp ", " Format.pp_print_string) real_depends;
      (fun opx -> not (ObjectFiles.stdlib_package_names opx) || StringSet.mem opx real_depends)
  in
  let opx_requires =
    ObjectFiles.fold_dir_name ~packages:true
      (fun requires opx name ->
         let opx = Filename.basename opx in
         if is_a_real_deps (fst name) then (
           Format.eprintf "Add : %s\n%!" opx;
           opx :: requires ) else requires)
      []
  in
  Format.eprintf "opx_requires: %a\n%!" (BaseFormat.pp_list ", " Format.pp_print_string) opx_requires;
  let already_required =
    (R.fold_with_name ~deep:true ~packages:true
       (fun pack k saved_requires ->
          (fun acc ->
             k (
               Format.eprintf "acc: %a\n%!" (StringSet.pp ", " Format.pp_print_string) acc;
               let pname = fst pack in
               if
                 is_a_real_deps (Filename.basename pname)
                 || StringSet.mem (pname ^ ".opx") acc
               then (
                 Format.eprintf "Add %s and %a\n%!" (fst pack) (Format.pp_list ", " Format.pp_print_string) saved_requires;
                 StringSet.add_list (List.map Filename.basename saved_requires) acc
               ) else acc)
          )
       ) (fun s -> s)
    ) StringSet.empty
  in
  Format.eprintf "opx_requires: %a\n%!" (BaseFormat.pp_list ", " Format.pp_print_string) opx_requires;
  let opx_requires =
    List.filter
      (fun opx -> not (StringSet.mem opx already_required))
      opx_requires in
  Format.eprintf "opx_requires: %a\n%!" (BaseFormat.pp_list ", " Format.pp_print_string) opx_requires;
  R.save opx_requires;
  let opx_requires =
    List.rev_map
      (fun opx -> cons_require opx)
      opx_requires in
  opx_requires @ code

