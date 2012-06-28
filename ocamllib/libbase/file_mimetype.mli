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
