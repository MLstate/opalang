(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
