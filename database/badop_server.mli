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
(** This functor puts a network layer on a given database backend that follows
    the Badop interface. To be used with Badop_client. *)

module F : functor (Backend: Badop.S) ->
sig
  type t
  val start : Scheduler.t -> Hlnet.endpoint -> Badop.options -> (t -> unit) -> unit
  val stop : t -> (unit -> unit) -> unit
end
