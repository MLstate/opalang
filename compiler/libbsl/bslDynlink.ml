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
type bypass_plugin_file = SharedObject of string  | MarshalPlugin of string
let file = function
  | SharedObject s | MarshalPlugin s -> s

let load_bypass_plugin = function
  | SharedObject file -> (
      try
        OManager.verbose "loading file \"%s\" (plugin)" file;
        Dynlink.loadfile_private file
      with
      | Dynlink.Error e ->
          OManager.printf "Primitives library plugin:@ Cannot dynlink file @{<bright>%S@}@\n" file;
          OManager.error "@[<2>@{<bright>Hint@}:@\n%s@]@\n" (Dynlink.error_message e)
    )
  | MarshalPlugin file ->
      OManager.verbose "loading file \"%s\" (plugin)" file;
      BslMarshalPlugin.loadfile_private file

let loadfile_private = load_bypass_plugin

let cache : (bypass_plugin_file, unit) Hashtbl.t = Hashtbl.create 16
let load_bypass_plugin_cache bypass_plugin =
  if not (Hashtbl.mem cache bypass_plugin)
  then (
    Hashtbl.add cache bypass_plugin () ;
    load_bypass_plugin bypass_plugin
  )
let reset_cache () = Hashtbl.clear cache
