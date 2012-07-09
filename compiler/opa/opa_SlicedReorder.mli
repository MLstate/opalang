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
(** Reordering of sliced code *)

(** Reorder client code, and server code. For server code, the value
    wich starts the server is placed at the end of code (or uniquely
    before its dependencies). Returns (client_code, server_code)
    reordened. *)
val perform : client:QmlAst.code -> server:QmlAst.code -> (QmlAst.code * QmlAst.code)

val reorder_in_new_qml : ?val_:(string -> Ident.t) -> QmlAst.code -> QmlAst.code
