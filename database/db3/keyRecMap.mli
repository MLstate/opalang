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

(* This module makes sense in a separate file, because it lets us
   specifiy which subset of SigMap operations we use in the db for keys *)

type 'a map

  val add : Keys.t -> ('b * 'b map) -> 'b map -> 'b map
  val remove : Keys.t -> 'b map -> 'b map
  val mem : Keys.t -> 'b map -> bool
  val find : Keys.t -> 'b map -> 'b * 'b map
  val find_opt : Keys.t -> 'b map -> ('b * 'b map) option
  val empty : 'b map
  val iter : (Keys.t -> ('b * 'b map) -> unit) -> 'b map -> unit
  val fold : (Keys.t -> ('b * 'b map) -> 'c -> 'c) -> 'b map ->
    'c -> 'c
  val size : 'b map -> int
  val max : Keys.t -> 'b map -> Keys.t
  val min : Keys.t -> 'b map -> Keys.t
  val is_empty : 'b map -> bool
  val keys : 'b map -> Keys.t list
  val fold_rev : (Keys.t -> 'a * 'a map -> 'b -> 'b) -> 'a map -> 'b -> 'b
