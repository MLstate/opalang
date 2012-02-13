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
