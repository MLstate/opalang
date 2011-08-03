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
