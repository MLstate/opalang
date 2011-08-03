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
(**
   This module implements a fake 'return' function

   The main reason for using this is when you want to traverse
   a data structure and stop at some point with a value.
   You can use it to defined a find function from an iter function for instance:
   {[ let find predicate =
       set_checkpoint (fun label ->
         iter (fun elt -> if predicate elt then return label (Some elt));
         None
       ) ]}
*)
type 'a label
val set_checkpoint : ('a label -> 'a) -> 'a
val return : 'a label -> 'a -> _
val set_checkpoint_opt : ('a label -> unit) -> 'a option
val set_checkpoint_either : ('returned label -> 'normal) -> ('normal,'returned) Base.either

(**
   If no break are done, return [Some a],
   or else return [None]
*)
val set_checkpoint_none : (unit label -> 'a) -> 'a option
