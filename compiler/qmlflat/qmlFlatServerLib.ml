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
(* alias *)
module FField = Flat_Runtime.Field
module VTable = Flat_Runtime.VTable

(*
  Magic traduction, because ocaml does not accept friend modules
*)
external field_t_of_field : ServerLib.field -> FField.t = "%identity"
external field_of_field_t : FField.t -> ServerLib.field = "%identity"
external ty_record_of_record : Flat_Runtime.record -> ServerLib.ty_record = "%identity"
external record_of_ty_record : ServerLib.ty_record -> Flat_Runtime.record = "%identity"

(* this module isn't meant to be used directly, call DebugPrint.print instead *)
module Debug : sig end =
struct
  let check_vtable = VTable.check
  let check_record_repr (x:Obj.t) : bool =
    (Obj.tag x = 0) && (* array *)
    Obj.size x >= 2 && (* vtable + option + flattened fields *)
    check_vtable (Obj.field x 0) &&
    DebugPrint.option (Obj.field x 1) &&
    Obj.size x - 2 = Obj.size (Obj.field x 0) (* content and vtable have same size *)
    (* cant't check anything on the content *)
  let check_record (x:'a) : bool = check_record_repr (Obj.repr x)

  (* printing by inspection of the values *)
  let unsafe_print x =
    Printf.sprintf "{%s}"
      (ServerLib.fold_record
         (fun field obj acc ->
            let acc = if acc = "" then "" else acc^"; " in
            let name_field = FField.name (field_t_of_field field) in
            if Flat_Runtime.is_empty (Obj.magic obj)
            then Printf.sprintf "%s%s" acc name_field
            else Printf.sprintf "%s%s: %s" acc name_field (DebugPrint.print obj)
         )
         x
         "")
  let print_opt x =
    if check_record x then (
      if Flat_Runtime.is_empty (Obj.magic x) then
        Some "{}"
      else
        Some (unsafe_print x)
    ) else
      None
  let () = DebugPrint.register {DebugPrint.f = print_opt}
end

type flat_record = Flat_Runtime.flat_record

module Field =
struct
  type t = ServerLib.field
  let register f = field_of_field_t (FField.register f)
end

module FieldAccess = Flat_Runtime.FieldAccess

type record = ServerLib.ty_record

module Simple =
struct
  let register s = ty_record_of_record (Flat_Runtime.Simple.register s)
end

let runtime_error = Flat_Runtime.runtime_error
external unwrap_record : ServerLib.ty_record -> _ array = "%identity"

external get_vtable : ServerLib.ty_record -> VTable.t = "%field0"

let empty = ServerLib.empty_record

let true_ = ty_record_of_record Flat_Runtime.true_
let false_ = ty_record_of_record Flat_Runtime.false_

let wrap_bool b = ty_record_of_record (Flat_Runtime.wrap_bool b)
let unwrap_bool r = Flat_Runtime.unwrap_bool (record_of_ty_record r)

let none = ty_record_of_record Flat_Runtime.none
let some a = ty_record_of_record (Flat_Runtime.some a)
let unwrap_option r = Flat_Runtime.unwrap_option (record_of_ty_record r)

let dot f r = Flat_Runtime.dot (field_t_of_field f) (record_of_ty_record r)
let dot_opt f r = Flat_Runtime.dot_opt (field_t_of_field f) (record_of_ty_record r)
let unsafe_get i r = Flat_Runtime.unsafe_get i (record_of_ty_record r)

let dot_with_cache cache f r = Flat_Runtime.dot_with_cache cache (field_t_of_field f) (record_of_ty_record r)

let extend_with_array r a =
  ty_record_of_record (Flat_Runtime.extend_with_array (record_of_ty_record r) (Obj.magic a))
external unsafe_init_static : flat_record -> record = "%identity"
let may_be_simple f = ty_record_of_record (Flat_Runtime.may_be_simple f)

let do_exit = ServerLib.do_exit
