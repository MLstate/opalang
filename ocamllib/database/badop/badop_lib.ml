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
    badop interface and implementations ; it's a generic lib, all specific stuff
    is in badop.ml *)

type ('which, 'q, 'r) dialog =
  | Query of 'q
  | Response of 'r

module Dialog = struct
  type query
  type response

  type ('which, 'q, 'r) t = ('which, 'q, 'r) dialog =
    | Query of 'q
    | Response of 'r

  let query q = Query q

  let response = function
    | Response r -> r
    | Query _ -> assert false (* guaranteed not to happen by the phantom type *)
end

module Dialog_aux = struct
  let (@>) = Cps.Ops.(@>)
  let (|>) = Cps.Ops.(|>)

  let respond _ r = Response r

  let map_dialog ~query ~response dialog k = match dialog with
    | Query q -> query q @> fun q -> Query q |> k
    | Response r -> response r @> fun r -> Response r |> k

  (* WARNING: unsafe, ONLY for use in de-serialisation functions *)
  let make_unsafe_query q = Query q
  let make_unsafe_response r = Response r
end
