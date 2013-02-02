(*
    Copyright © 2011, 2012 MLstate

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
type printer = { f : 'a. 'a -> string option }
let printers = ref ([] : printer list)
let register p = printers := p :: !printers

let get_registered_print v =
  let rec aux v l =
    match l with
    | [] -> None
    | h::rl ->
      match h.f v with
      | None -> aux v rl
      | Some _ as r-> r
  in aux v !printers

let custom v = Option.map (fun s-> (fun buff _ -> Buffer.add_string buff s)) (get_registered_print (Obj.obj v))

let rec print ?(depth=1000) x =
  let rec aux = function
    | [] -> Printf.sprintf "$%s$" (BaseObj.dump ~custom ~depth x)
    | h :: t ->
        match h.f x with
        | None -> aux t
        | Some s -> s in
  aux !printers

let pp ?(depth=1000) fmt a = Format.pp_print_string fmt (print ~depth a)

let rec simple_print ?(depth=1000) x =
  let rec aux = function
    | [] -> Printf.sprintf "%s" (BaseObj.dump ~custom ~depth x)
    | h :: t ->
        match h.f x with
        | None -> aux t
        | Some s -> s in
  aux !printers


let true_ _ = true
let false_ _ = false

let bool x = Obj.obj x == true || Obj.obj x == false
let string x = Obj.tag x = Obj.string_tag
let option ?(a=true_) x =
  let t = Obj.tag x in
  t = Obj.int_tag && (Obj.obj x = 0) || (* none *)
    t = 0 && (* some *)
    Obj.size x = 1 &&
    a (Obj.field x 0)
let array ?(tag=0) ?a x =
  Obj.tag x = tag &&
  match a with
  | None -> true
  | Some check_a ->
      let i = ref 0 in
      let s = Obj.size x in
      let ok = ref true in
      while !i < s && !ok do
        ok := check_a (Obj.field x !i);
        incr i;
      done;
      !ok
let unit x = Obj.obj x = 0
let int ?(plus=true_) x = Obj.is_int x && plus (Obj.obj x : int)
let tuple0 = unit
let tuple1 ?(f1=true_) x =
  Obj.tag x = 0 && Obj.size x = 1 && f1 (Obj.field x 0)
let tuple2 ?(f1=true_) ?(f2=true_) x =
  Obj.tag x = 0 && Obj.size x = 2 && f1 (Obj.field x 0) && f2 (Obj.field x 1)
let tuple3 ?(f1=true_) ?(f2=true_) ?(f3=true_) x =
  Obj.tag x = 0 && Obj.size x = 3 && f1 (Obj.field x 0) && f2 (Obj.field x 1) && f3 (Obj.field x 2)
let tuple4 ?(f1=true_) ?(f2=true_) ?(f3=true_) ?(f4=true_) x =
  Obj.tag x = 0 && Obj.size x = 4 && f1 (Obj.field x 0) && f2 (Obj.field x 1) && f3 (Obj.field x 2) && f4 (Obj.field x 3)
let tuple_n checkers x =
  let n = List.length checkers in
  Obj.tag x = 0 && Obj.size x = n &&
    let rec aux i = function
      | [] -> true
      | h :: t -> h (Obj.field x i) && aux (i+1) t in
    aux 0 checkers
