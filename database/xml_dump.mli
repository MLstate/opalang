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

(** This module provides a facility for generating an XML tree that follows the
    structure of an OPA database. It first extracts the schema from that
    database (as saved by OPA programs thanks to dbGen), then uses it to follow
    the data and to structure the XML. XML dump format is documented in the opa
    stdlib.

    See also Xml_import, that loads back the same XML format into a database.
*)

(** The version number of the XML format *)
val xml_dump_version : string

(** We want to be CPS but continuation-implementation agnostic, hence the
    following functor on continuation type, creation and application. *)
module type SigCpsBackend = sig
  type 'a continuation
    (** the type of continuations that take ['a] (typically ['a -> unit]) *)
  val mkcont : 'b continuation -> ('a -> unit) -> 'a continuation
    (** construction of a new continuation (first arg. may be used for context information) *)
  val return : 'a -> 'a continuation -> unit
    (** application of a continuation (usually bound to the [|>] operator) *)
end

module F : functor (C: SigCpsBackend) -> sig

  (** [to_file db_read filename @> k] dumps the badop database accessible
      through function [db_read] to the file named [filename]. [db_read] has the
      same type as the partial application of [Badop.read] to a transaction. *)
  val to_file :
    (Badop.path -> (Badop.Dialog.query, 'rev) Badop.generic_read_op -> (Badop.Dialog.response, 'rev) Badop.generic_read_op Badop.answer Cps.t) ->
    string ->
    unit C.continuation -> unit
end
