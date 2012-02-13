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


(* Type of imported mimetype database *)
type mimetype_database

exception MimeType_NotFound
exception Malformed
exception Open of string

(** Import mlstate the database.
  It is stored in a xml file : $MLSTATELIBS/share/opa/mimetype_database.xml.
  Takes in argumetns, the path of mlstatelibs directory.
  The result of [mimetype_database] is memoized.

  Raises [Malformed] if the xml file is malformed
 *)
val mimetype_database : string Lazy.t -> mimetype_database

(** Like mimetype_database but with the specified file as database
    (not defualt file in $MLSTATELIBS). *)
val get_mimetype_database : string -> mimetype_database

(** [get_mimetype] filename database return the mimetype of [filename]
    * according [database].
    *
    * Raises [MimeType_NotFound] if no match is found
    * Raises [Open <error>] if an error occurs during file's openning
*)
val get_mimetype : string -> mimetype_database -> string
