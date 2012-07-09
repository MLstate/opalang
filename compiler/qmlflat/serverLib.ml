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
(* depends *)
module Array = Base.Array
module Lazy = Base.Lazy
module List = Base.List

(* alias *)
module Complex = Flat_Runtime.Complex
module Field = Flat_Runtime.Field
module Simple = Flat_Runtime.Simple
module VTable = Flat_Runtime.VTable

exception RuntimeError of string
(* -- *)

let rec compare_cpx c1 c2 =
  let c1 = ( Obj.magic c1 : Complex.t ) in
  let c2 = ( Obj.magic c2 : Complex.t ) in
  let v1 = VTable.export (Complex.get_vtable c1) in
  let v2 = VTable.export (Complex.get_vtable c2) in
  let r = Array.compare Field.compare v1 v2 in
  if r <> 0 then r else
    let len = Array.length v1 in
    let rec aux i =
      if i >= len then 0 else
        let r = compare
          (Complex.get_value i c1)
          (Complex.get_value i c2)
        in
        if r <> 0 then r else aux (succ i)
    in aux 0

and compare r1 r2 =
  let is_r1_cpx = Flat_Runtime.is_record (Obj.repr r1) in
  if is_r1_cpx
  then
    let is_r2_cpx = Flat_Runtime.is_record (Obj.repr r2) in
    if is_r2_cpx then compare_cpx r1 r2
    else
      Pervasives.compare r1 r2
  else
    Pervasives.compare r1 r2

type ty_record = Flat_Runtime.record

(* we can't write [type 'a ty_info = 'a info] otherwise we get
   "inconsistent assumption blah blah blah i'm stupid" from ocaml. So let's
   define the /exact same/ type as it is waiting for and use some magic *)
type 'a ty_info = 'a constraint 'a = [> ]

let empty_record = Flat_Runtime.empty

let has_lazy_data r =
  Obj.obj (Complex.get_info r) <> None

let get_lazy_data_opt r = Obj.magic (Flat_Runtime.get_record_info r)

let embed_lazy_data r o =
  Complex.update_info r (Obj.repr o)

let inject_lazy_data r o =
  (* check with Louis: we may corrupt shared representation *)
  if Flat_Runtime.is_simple r then () else
    Complex.inject_info r (Obj.repr o)

let at_exit, do_exit, get_exit =
  let at_exit_fun = ref (fun () -> ()) in
  let once f = (* Because we don't want to execute twice with our do_exit and ocaml's at_exit *)
    let don = ref false in fun x -> if !don then () else (don := true; f x) in
  let at_exit_first = (* Because our at_exit keeps only one function (overriding previous ones) *)
    let don = ref false in fun f ->
      Pervasives.at_exit (fun () -> if !don then () else (don := true; f ()))
  in
  (fun f -> let f = once f in at_exit_fun := f; at_exit_first f),
  (fun i -> !at_exit_fun (); exit i),
  (fun () -> !at_exit_fun)

type field = Field.t

let compare_field = Field.compare

type field_index = int
type fields_indexes = VTable.t
type patterns_indexes = fields_indexes array
type 'a rt_record = 'a

external rt_to_ty_record : 'a rt_record -> ty_record = "%identity"
external ty_to_rt_record : ty_record -> 'a rt_record = "%identity"


let fields_indexes (x:field array) =
  let x = Array.map Field.name x in
  Array.fast_sort (Obj.magic Field.compare) x;
  (VTable.register x: fields_indexes)

let field_index (x:fields_indexes) (f:field) =
  let x =  VTable.export x in
  let rec aux i =
    if x.(i) == f then (i:field_index)
    else  aux (i+1)
  in aux 0

let dot_with_field_index rt_record (field_index:field_index) = Complex.get_value field_index (rt_to_ty_record rt_record)

(* rely on shared vtable hypothesis *)
let patterns_indexes (x:fields_indexes array) = x

let gt = -1
let lt = -2

let compare_structure pattern_index r1 r2 =
  let r1 = rt_to_ty_record r1 in
  let r2 = rt_to_ty_record r2 in
  (* keep synchronised with opatop version until the comment in opatop indicate otherwise *)
  (* rely on shared vtable hypothesis *)
  let rec common_search v1 i=
    if pattern_index.(i) == v1 then i
    else if i!=0 then common_search v1 (i-1)
    else
      raise (RuntimeError "serverLib.compare_structure.common : pattern_index is not compatible with record")
  in
  let rec diff_search v1 v2 i=
    let fields = pattern_index.(i) in
    if fields == v1 then gt
    else if fields == v2 then lt
    else if i != 0 then
      diff_search v1 v2 (i-1)
    else
      raise (RuntimeError "serverLib.compare_structure.diff : pattern_index is not compatible with record")
  in
  let v1 = Flat_Runtime.Complex.get_vtable r1 in
  let v2 = Flat_Runtime.Complex.get_vtable r2 in
  let n = Array.length pattern_index in
  if v1 == v2 then common_search v1 (n-1)
  else diff_search v1 v2 (n-1)

let fold_record folder record acc =
  let record = (( Obj.magic record ) : ty_record) in
  if Flat_Runtime.is_empty record then
    acc
  else
    let vtable = VTable.export (Complex.get_vtable record) in
    Array.fold_left_i
      (fun acc field index ->
         folder field (Complex.get_value index record) acc
      ) acc vtable

let fold_2_record folder record1 record2 acc =
  let record1 = (( Obj.magic record1 ) : ty_record) in
  let record2 = (( Obj.magic record2 ) : ty_record) in
  if Flat_Runtime.is_empty record1 then
    acc
  else
    let vtable = VTable.export (Complex.get_vtable record1) in
    Array.fold_left_i
      (fun acc field index ->
         folder field
           (Complex.get_value index record1)
           (Complex.get_value index record2)
           acc
      ) acc vtable

type record_constructor = (Field.t * Obj.t) list

let empty_record_constructor = []

let add_field rc field value = (field, Obj.repr(value))::rc

let make_record rc =
  let cmp (f1,_) (f2,_) = Field.compare f1 f2 in
  let sorted = List.sort cmp rc in
  let sorted = List.uniq ~cmp sorted in
  Obj.magic (Flat_Runtime.init_from_list sorted)

(* TODO: check who does use it *)
let make_simple_record s = Obj.magic (Simple.register (Obj.magic s))

let name_of_field = Field.name_of_field
let field_of_name = Field.field_of_name

let static_name_of_field = Field.name
let static_field_of_name = Field.register

(* Extension for explicit projection *)

let unsafe_dot record field = Flat_Runtime.dot field record
let dot record field = Flat_Runtime.dot_opt field record
let is_present record field = Option.is_some (Flat_Runtime.dot_opt field record)

(* Projections on constants *)

type ty_char = char
type ty_float = float (* youpi *)
type ty_int = int
type ty_null = unit
type ty_string = string

external wrap_float : float -> ty_float = "%identity"
external unwrap_float : ty_float -> float = "%identity"

external wrap_int : int -> ty_int = "%identity"
external unwrap_int : ty_int -> int = "%identity"

external wrap_string : string -> ty_string = "%identity"
external unwrap_string : ty_string -> string = "%identity"

let void = empty_record

let null = ()

type ty_void = ty_record
type ty_bool = ty_record
type 'a ty_option = ty_record

let wrap_bool = Flat_Runtime.wrap_bool
let unwrap_bool = Flat_Runtime.unwrap_bool

let true_ = Flat_Runtime.true_
let false_ = Flat_Runtime.false_

let wrap_option = Flat_Runtime.wrap_option
let unwrap_option = Flat_Runtime.unwrap_option

let none = Flat_Runtime.none
let some = Flat_Runtime.some

(* support for marshaling *)

let deep_force_eval a = Obj.obj (Lazy.deep_force (Obj.repr a))

let rec sharing_refresh alpha =
  if Flat_Runtime.is_record (Obj.repr alpha)
  then (
    let ma = (Obj.magic alpha : Flat_Runtime.flat_record) in
    let contents_len = (Array.length ma) - Flat_Runtime.val_shift in
    if contents_len = 0 then Obj.magic empty_record
    else (
      for i = Flat_Runtime.val_shift to pred (Array.length ma) do
        Array.set ma i (Obj.magic (sharing_refresh (Obj.magic (Array.get ma i)))) ;
      done ;
      if contents_len = 1 && Array.get ma Flat_Runtime.val_shift == Flat_Runtime.shared_void
      then Obj.magic (Simple.register (Array.get (Obj.magic (Array.get ma 0)) 0))
      else
        let () = Array.set ma 0 (Obj.repr (VTable.register (Obj.magic (Array.get ma 0)))) in
        Obj.magic (Complex.init_from_evaluated ma)
    )
  )
  else
    alpha
