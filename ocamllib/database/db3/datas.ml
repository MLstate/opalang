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
    @author1 Henri Binsztok,
    @author2 Gregoire Makridis
**)

type ns =
  | Data of DataImpl.t
  | Link of Path.t
  | Copy of Revision.t option * Path.t
  | UnsetData

type t = ns
type io = ns

let empty = UnsetData

let to_string = function
  | UnsetData -> "# UNSET #"
  | Link p -> Printf.sprintf "# LINK --> %s #" (Path.to_string p)
  | Copy (r, p) -> Printf.sprintf "# COPY --> %s, %s #"
      (Option.to_string Revision.to_string r) (Path.to_string p)
  | Data e -> Printf.sprintf "# DATA %s #" (DataImpl.to_string e)

let get_string = to_string

let version = 1

let index_fun = function
  | Data d -> DataImpl.index_fun d
  | _ -> StringMap.empty
