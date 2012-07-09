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
    @author Louis Gesbert
**)
(** This module contains common types, functions and helpers used throughout the
    badop interface and implementations ; it's a generic lib for queries, cps
    and stuff, all db-specific stuff is in badop.ml *)

(** {4 The dialog type and handlers} *)

(** we provide a private [dialog] type that is a either-or of two types with
    safe access functions (using a phantom type).
    - [(query,'q,'r) dialog] holds (only) a data of type 'q. Build it with function [query]
    - [(response,'q,'r) dialog] holds (only) a data of type 'r. Extract it with function [response]
    @inline doc
*)

type ('which, 'q, 'r) dialog = private
  | Query of 'q
  | Response of 'r

module Dialog : sig
  type query (** phantom type *)
  type response (** phantom type *)
  type ('which, 'q, 'r) t = ('which, 'q, 'r) dialog = private
    | Query of 'q
    | Response of 'r

  (** Builds a query *)
  val query: 'q -> (query, 'q, 'r) t

  (** Extracts a response *)
  val response: (response, 'q, 'r) t -> 'r
end

(** This one is used by backends, not exported outside of the lib *)
module Dialog_aux : sig
  (** Turns a query into response (only for use by database engines) *)
  val respond: (Dialog.query, 'q, 'r) dialog -> 'r -> (Dialog.response, 'q, 'r) dialog

  (** Used to transform queries (eg when forwarding) (only for use by database engines) *)
  val map_dialog: query:('q1 -> 'q2 Cps.t) -> response:('r1 -> 'r2 Cps.t) ->
    ('which, 'q1, 'r1) dialog -> ('which, 'q2, 'r2) dialog Cps.t

  (** WARNING: unsafe, ONLY for use in de-serialisation functions *)
  val make_unsafe_query : 'q -> ('w, 'q, 'r) dialog
  val make_unsafe_response : 'r -> ('w, 'q, 'r) dialog
end
