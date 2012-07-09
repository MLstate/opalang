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
