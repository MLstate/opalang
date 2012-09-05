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
(* CF mli *)

module BPI = BslPluginInterface
module Hashtbl = BaseHashtbl

(* Need to do a topologic sort of plugin *)
module ItemPlugin =
struct
  type t = BPI.plugin
  let index t = t.BPI.uniq_id
  let depends t = t.BPI.depends
end

module PluginSort = TopologicSort.Make (ItemPlugin)

let debug fmt =
  OManager.printf ("@{<cyan>[Bsl]@}@ @[<2>"^^fmt^^"@]@.")

let pp_list = Base.Format.pp_list

let table : (string option, BPI.plugin) Hashtbl.t = Hashtbl.create 10

let private_last_finalize = ref None

let pp_item fmt t = Format.pp_print_string fmt (ItemPlugin.index t)

let clear () =
  Hashtbl.clear table;
  private_last_finalize := None

let finalize () =
  let plugins = Hashtbl.fold (fun _ plugin acc -> plugin::acc) table [] in
  let plugins, not_referenced = PluginSort.sort plugins in
  match not_referenced with
  | [] ->
      private_last_finalize := Some plugins ;
      plugins
  | _ ->
      let report fmt (name, deps) =
        Format.fprintf fmt (
          "@[<2>+ missing %s@\n@[<2>referenced in@\n%a@]@]@\n"^^
          "@[<2>@{<bright>Hint@}:@\nAdd '@{<bright>import-plugin %s@}'@]"
        )
          name
          (pp_list "@\n" pp_item) deps
          name
      in
      OManager.error
        "External primitives plugins dependancies are not satisfied:@\n%a"
        (pp_list "@\n" report) not_referenced

let last_finalize () = !private_last_finalize

let store plugin =
  let mname = plugin.BPI.basename in
  let uniq_id = plugin.BPI.uniq_id in
  match Hashtbl.find_opt table mname with
  | Some plugin' ->
      let uniq_id' = plugin'.BPI.uniq_id in
      if String.compare uniq_id uniq_id' <> 0
      then
        let ml_runtime = plugin.BPI.ml_runtime in
        OManager.error (
          "@[<2>External plugin: conflicting versions for %S@\n"^^
          "mismatch: <%s> / <%s>@]@\n"
        )
          ml_runtime (Option.default "bundled plugin" mname) uniq_id uniq_id'
      else ()

  | None ->
      let _ =
        #<If:BSL_LOADING $minlevel 1>
          debug "loading plugin <%S>" uniq_id
        #<End>
      in
      Hashtbl.add table mname plugin

let get plugin_name =
  try Some (Hashtbl.find table plugin_name) with Not_found -> None
