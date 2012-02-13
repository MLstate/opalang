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
