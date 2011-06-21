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
    @author Louis Gesbert
**)
(** This file contains the lib for an interactive upgrade of a database. It
    reads a database and the dump of the new expected schema *)

let db_dir name =
  if Sys.file_exists (name^"_node_file") then name
  else
    let name = Sys.getenv "HOME" ^ "/.mlstate/" ^ name ^ "/" in
    if Sys.file_exists (name ^ "_node_file") then name
    else failwith "Database file not found"

let get_schemas ?new_file name =
  let filename = db_dir name in
  let old_schema = (DbImport.get_schema Official_database.do_simpleread_on_db filename).s in
  let new_schema =
    let fnam = match new_file with Some n -> n | None -> filename^"_schema.gml" in
    Schema_io.from_gml_string (File.content fnam)
  in
  old_schema, new_schema

type mapping = 

