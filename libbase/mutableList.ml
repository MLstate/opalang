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
(* CF mli *)
module List = BaseList

type 'a t = { mutable d : 'a list }
let create () = { d = [] }
let clear d = d.d <- []
let add d a = d.d <- a::d.d
let append d l = d.d <- List.rev_append l d.d
let rev_append d l = d.d <- List.append l d.d
let mem a d = List.mem a d.d
let fold_right fct d acc = List.fold_left (fun acc d -> fct d acc) acc d.d
let fold_left fct acc d = List.fold_left fct acc (List.rev d.d)
let to_list d = List.rev d.d
let to_rev_list d = d.d
let length d = List.length d.d
let iter f d = List.iter f (List.rev d.d)
let rev_iter f d = List.iter f d.d
let is_empty d = List.is_empty d.d
let from_list l = { d = List.rev l }
let from_rev_list l = { d = l }
let reset_from_list d l = d.d <- List.rev l
let reset_from_rev_list d l = d.d <- l
let update f d = reset_from_list d (f (to_list d))
