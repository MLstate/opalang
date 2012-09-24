/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

package stdlib.database.common

DbCommon = {{

    @package path_to_id(path:list(string)) =
        Uri.to_string(~{path fragment=none query=[] is_directory=false is_from_root=true})

    Compose = {{

        @package read(elements) =
            (value, nothing) =
                List.fold(
                (field, path), (acc, nothing) ->
                  #<Ifstatic:DBGEN_DEBUG>
                        do Log.notice("Db.build_vpath_compose", " field {field} start reading")
                  #<End>
                  backfield = OpaValue.Record.field_of_name_unsafe(field)
                  match path.read() with
                  | {none} ->
                    data = OpaValue.Record.unsafe_dot(default, backfield)
                    (OpaValue.Record.add_field(acc, backfield, data), nothing)
                  | {some=data} ->
                    (OpaValue.Record.add_field(acc, backfield, data), false)
                , elements, (OpaValue.Record.empty_constructor(), true))
            if nothing then {none} else {some = OpaValue.Record.make_record(value)}

        @package remove(elements) =
            List.iter((_field, path) -> path.more.remove(), elements)

        @package write(elements, data) =
            List.fold(
             (field, path), b ->
               backfield = OpaValue.Record.field_of_name_unsafe(field)
               data = OpaValue.Record.unsafe_dot(data, backfield)
               b && path.more.write(data)
             , elements, true
            )

    }}
}}
