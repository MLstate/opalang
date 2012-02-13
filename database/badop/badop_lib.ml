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
