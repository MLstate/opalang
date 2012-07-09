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

(*
   Important notes:
   - never perform [Array.copy] on a [flat_record], always a [Array.copy_memory]
  (Ocaml optimisation about float array)

  - be sure you use the shared representation for a field, and not directly the string,
  or you would break the physic compare used in field access
*)

(* depends *)
module Array = Base.Array
module Hashtbl = Base.Hashtbl
module Lazy = Base.Lazy
module List = Base.List
module String = Base.String
module StringMap = StringMap

(* -- *)

exception Field_not_found
exception RuntimeError

let pp_error fmt =
  let k _ = raise RuntimeError in
  Format.kfprintf k Format.err_formatter ("Runtime error: "^^fmt^^"@.")

let runtime_error s = pp_error "%s" s

(* We reproduce the sig there for abstracting the implementation also in this file *)
module Field :
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val register : string -> t
  val field_of_name : string -> t option
  val name_of_field : t -> string option
  external name : t -> string = "%identity"

  (**
     A fake field which is lt any other value of type t
  *)
  val bottom : t

  (**
     Safe function which does not segfault on any object.
     Tell if the object is a field
  *)
  val check : Obj.t -> bool

end =
struct
  type t = string

  let equal a b = a == b
  let compare a b =
    if a == b then 0 else if a < b then -1 else 1

  let lt a b = if a == b then false else a < b
  let gt a b = if a == b then false else a > b

  let bottom = ""

  (*
    This table is used by every separated compilation unit.
  *)
  let table = Hashtbl.create 1024
  let register s =
    try
      Hashtbl.find table s
    with
    | Not_found ->
        (* if we get scared, we can do : *)
        (* let field = String.copy s in *)
        let field = s in
        Hashtbl.add table s field ;
        field

  (* let register_fields fs = List.iter (fun s -> let _ = register_field s in ()) fs *)

  let field_of_name s =
    try
      Some (Hashtbl.find table s)
    with
    | Not_found -> None

  let name_of_field = field_of_name

  external name : t -> string = "%identity"

  let check x =
    DebugPrint.string x &&
    (Option.is_some (field_of_name (Obj.obj x : string)))

end

(* The sig is not reproduced, because we use the implementation in this file *)
module FieldAccess =
struct
  type t = int
  type cache = int ref
  let default = 0
  let start_from = -1
  let make_cache () = ref default
end

module VTable :
sig
  type t
  external export : t -> Field.t array = "%identity"
  val register : string array -> t
  val shared : string array -> t option
  val empty : t
  val is_vtable : 'a -> bool
  val check : Obj.t -> bool
end =
struct
  type t = Field.t array
  external export : t -> Field.t array = "%identity"

  (*
    <!> Should be a free value.
    A test in reftester checks that this value is not used by Ocaml.Obj
  *)
  let vtable_tag = 245
  let set_vtable_tag v =
    let () = Obj.set_tag (Obj.repr v) vtable_tag in
    ()

  let is_vtable a =
    Obj.tag (Obj.repr a) = vtable_tag

  let check a =
    (Obj.is_block a) &&
    DebugPrint.array ~tag:vtable_tag ~a:Field.check a

  module HVT = Hashtbl.Make (
    struct
      type t = string array
      let equal a b = Array.compare String.compare a b = 0
      let hash a =
        let fold acc s = Hashtbl.combine acc (Hashtbl.hash s) in
        Array.fold_left fold 0 a
    end
  )

  (*
    This table is used by every separated compilation unit.
    This is a custom hashing because vtable are tagged with a tag
    different than string array (0 != vtable_tag)
  *)
  let table = HVT.create 1024

  let register fields =
    try
      HVT.find table fields
    with
    | Not_found ->
        let vtable = Array.map Field.register fields in
        set_vtable_tag vtable ;
        HVT.add table fields vtable ;
        vtable

  let shared fields =
    try
      Some (HVT.find table fields)
    with
    | Not_found -> None

  let empty = register [||]
end

(*
  Non-trivial flat records.
  All other records, including the empty record.
  <!> Invariant: a simple record is always on its simple form.
*)
type flat_record = Obj.t array

external field0 : 'a -> 'b  = "%field0"
let is_record o =
     (Obj.is_block o)
  && (Obj.size o >= 2)
  && (VTable.is_vtable (field0 o))

(* Start index of the values inside the record representation *)
let val_shift = 2

module Complex :
sig
  type t

  (**
     Shared empty record
  *)
  val empty : t

  (** {6 Getters} *)

  val get_value : int -> t -> 'a
  val get_info : t -> Obj.t
  external get_vtable : t -> VTable.t = "%field0"
  val number_of_values : t -> int

  (**
     Get the content, without forcing evaluation
  *)
  val get_no_force : int -> t -> Obj.t

  (** {6 Check} *)
  (**
     Check that the length of the vtable correspond to the number of fields,
     and that fields are well-ordered.
  *)
  val sanity_check : t -> bool

  (** {6 Constructors} *)

  (**
     Initialize a complex record with a set of already evaluated field values
  *)
  external init_from_evaluated : flat_record -> t = "%identity"

  (**
     Initialize a complex record with a set of lazy field values
  *)
  external init_from_lazy : flat_record -> t = "%identity"

  (** return a fresh record, sharing data *)
  val update_info : t -> Obj.t -> t

  (** side effect, modification in place *)
  val inject_info : t -> Obj.t -> unit

  (** {6 Access} *)

  (**
     The suffix exn is for tagging the fact that function may raise [Field_not_found].
     Positions lowpos and highpos are included.
  *)

  (**
     Dichotomic search. O(log(n))
     Interpolation does not work with string.

     Equality on fields can use physicality.
     Comparison, String.compare (GC can move strings)

     @raise Field_not_found if the value could not be found,
     for instance if the record was empty.
     @return index if the field could be found at index [index]
  *)
  val search_in_vtable : Field.t -> VTable.t -> lowpos:int -> highpos:int -> int

  (**
     @raise Field_not_found if the field does not belong to the record
  *)
  val search_from_exn : Field.t -> t -> lowpos:int -> 'a

  (**
     The function perform a side effect on the cache.
     @raise Field_not_found if the field does not belong to the record
  *)
  val search_from_exn_cache : FieldAccess.cache -> Field.t -> t -> lowpos:int -> 'a

end
=
struct

  type t = flat_record

  let empty = [| Obj.repr VTable.empty ; Obj.repr None |]

  (* getters *)
  let get_no_force i r = r.(i + val_shift)
  let get_value i r = Lazy.force (Obj.obj r.(i + val_shift))
  let get_info r = r.(1)
  external get_vtable : t -> VTable.t = "%field0"
  let number_of_values r = Array.length r - val_shift

  (* check *)

  (**
    Check that the vtable of a complex record is consistent,
    i.e. that fields are in increasing order.
  *)
  let sanity_check cpx =
    (is_record (Obj.repr cpx)) &&
    let len_val = number_of_values cpx in
    let vtable = VTable.export (get_vtable cpx) in
    let len_vt = Array.length vtable in
    len_val = len_vt
        && len_val >= 0
        && fst (
          Array.fold_left
            (fun (res, last) x -> res && (Field.lt last x), x)
            (true, Field.bottom)
            vtable
        )

  (* constructors *)

  external init_from_evaluated : flat_record -> t = "%identity"
  external init_from_lazy : flat_record -> t = "%identity"

  let update_info r i =
    let t = Array.copy_memory r in
    t.(1) <- i;
    t

  let inject_info r i = r.(1) <- i

  let search_in_vtable field vtable ~lowpos ~highpos =
    assert (0 <= lowpos );
    assert (0 <= highpos);
    assert (lowpos <= highpos + 1);
    if lowpos = highpos + 1 then
      (* being in this case means we were doing a pattern matching on a sum types with
       * where two record types share a field name:
       * [ match x with
       *   | {a; b} -> ...
       *   | {a; c} -> ... ]
      *)
      raise Field_not_found
    else begin
      let vtable = VTable.export vtable in
      assert (lowpos < Array.length vtable);
      assert (highpos < Array.length vtable);
      let lowfield = vtable.(lowpos)
      and highfield = vtable.(highpos) in
      let rec aux ~lowpos ~lowfield ~highpos ~highfield =
        (*Search between [lowpos] and [highpos] included*)
        if Field.gt lowfield field || Field.lt highfield field then raise Field_not_found
        else
          let guesspos =
            if Field.equal highfield lowfield then lowpos
            else
              (lowpos + highpos) / 2
          in
          let guessfield = vtable.(guesspos) in
          if Field.equal guessfield field then (*Found*)
            guesspos
          else
            if Field.lt guessfield field then (*Retry with higher positions*)
              let lowpos = succ guesspos in
              aux ~lowpos ~lowfield:(vtable.(lowpos)) ~highpos ~highfield
            else (*Retry with lower positions*)
              let highpos = pred guesspos in
              aux ~lowpos ~lowfield ~highpos ~highfield:(vtable.(highpos))
      in
      aux ~lowpos ~lowfield ~highpos ~highfield
    end

  let search_from_exn field (record:t) ~lowpos =
    let vtable = get_vtable record in
    let last = Array.length (VTable.export vtable) - 1 in
    let pos = search_in_vtable field vtable ~lowpos ~highpos:last in
    get_value pos record

  let search_from_exn_cache cache field cpx ~lowpos =
    let hint = !cache in
    let vtable = get_vtable cpx in
    let last = Array.length (VTable.export vtable) - 1 in
    let pos =
      if hint <= last then
        let guessfield = (VTable.export vtable).(hint) in
        if Field.equal guessfield field then hint (*Yeah, found it!*)
        else if Field.lt guessfield field then (*[hint] was too low*)
          begin
            if hint < last then
              search_in_vtable field vtable ~lowpos:(hint+1) ~highpos:last
            else
              (* the comparison is strict because if hint = last, *)
              (* since we can't go higher, the field is simply absent *)
              raise Field_not_found
          end
        else(*if guessfield > field then*) (*[hint] was too high*)
          begin
            if hint > lowpos then
              search_in_vtable field vtable ~lowpos ~highpos:(hint-1)
            else
              raise Field_not_found
          end
      else (*[hint] not acceptable*)
        (* we have all needed ident bined to what is needed to call search_in_vtable
           rather than calling search_from_exn *)
        search_in_vtable field vtable ~lowpos ~highpos:last
    in
    cache := pos;
    get_value pos cpx
end


(*
  A record.

  A record is either a [complex_record] or a [simple_record].
  We count on [Obj.tag] to tell us if an object is a record.
*)
type record = Complex.t

external unwrap_record : record -> _ array = "%identity"

(*
   {6 Shared stuff}
*)

(*
  The empty record
*)
let empty = Complex.empty
let void_of_unit _ = empty
let is_empty r = (Obj.magic r) == empty
let shared_void = Obj.repr empty
let shared_void_lazy = shared_void

(*
  Simple records.

  All records consisting exactly in one field without content have a special optimization.
  They are represented at runtime by the shared field corresponding to the label
  that they contain.

  <!> Since the composionality of qmlflat, the empty record is no longer a simple record.
*)
module Simple :
sig
  val register : string -> record
end =
struct
  (*
    This table is used by every separated compilation unit.
  *)
  let table = Hashtbl.create 1024
  let register s =
    try
      Hashtbl.find table s
    with
    | Not_found ->
        let vtable = VTable.register [|s|] in
        let simple = Complex.init_from_evaluated
          [| Obj.repr vtable ; Obj.repr None ; Obj.repr empty |] in
        Hashtbl.add table s simple ;
        simple
end

let is_simple (r:record) =
  let r = ( Obj.magic r : _ array ) in
  Array.length r = (succ val_shift) &&
    is_empty (Array.unsafe_get r val_shift)

let true_ = Simple.register "true"
let false_ = Simple.register "false"
let wrap_bool b =
  if b
  then true_
  else false_
let unwrap_bool tb = tb == true_

let none = Simple.register "none"
let some_field = Field.register "some"
let some_structure = VTable.register [|Field.name some_field|]
let simple_some = Simple.register "some"
let some a =
  if is_empty a then simple_some else
    (Complex.init_from_evaluated [| Obj.repr some_structure ; Obj.repr None ; Obj.repr a |])

let number_of_fields r =
  Complex.number_of_values r

type 'a info = 'a constraint 'a = [> ]

let get_record_info r =
  Obj.obj (Complex.get_info r)

let safe_init ( t : flat_record ) =
  let contents_len = (Array.length t) - val_shift in
  assert (not (Array.length (Obj.obj t.(0)) <> contents_len));
  if contents_len = 0 then empty
  else
    if contents_len = 1 && (let obj = t.(val_shift) in (Lazy.force (Obj.obj obj)) == shared_void) then Simple.register ((Obj.obj t.(0)).(0))
      (* /!\ info ignored ! *)
    else
      let () = t.(0) <- Obj.repr (VTable.register (Obj.obj t.(0))) in
      Complex.init_from_evaluated t

let may_be_simple ( t : flat_record ) =
  if (let obj = t.(val_shift) in (Lazy.force (Obj.obj obj))) == shared_void
  then Simple.register ((Obj.obj t.(0)).(0))
  else
    Complex.init_from_evaluated t

external unsafe_init_static : flat_record -> record = "%identity"

let check_record = Complex.sanity_check

(**
   Split the list and invoke [safe_init].
*)
let init_from_list l =
  let vtable, contents = List.split l in
  let vtable = Array.of_list vtable in
  let t = Array.unsafe_create (Array.length vtable + val_shift) in
  let () =
    t.(0) <- Obj.repr vtable ;
    t.(1) <- Obj.repr None ;
    List.iteri (fun x i -> t.(i+val_shift) <- Obj.repr x) contents
  in
  safe_init t

(**
   Split the array and invoke [safe_init]
*)
let init_from_array a =
  let vtable, contents = Array.split a in
  let c = Array.length contents in
  let t = Array.unsafe_create (c+val_shift) in
  let _ =
    t.(0) <- Obj.repr vtable ;
    t.(1) <- Obj.repr None ;
    Array.unsafe_blit contents 0 t val_shift c
  in
  safe_init t

(**
   Extend a record with the fields appearing in an array.
*)
let extend_with_array record extend =
  if Array.is_empty extend then record
  else if is_empty record then init_from_array extend
  else (* We are now faced with a non-empty list and a complex record *)
    let vtable_t = Complex.get_vtable record in
    let vtable = VTable.export vtable_t in
    let length_v = Array.length vtable in
    let length_e = Array.length extend in

    (*
      No tmp allocation allowed, no tmp dynamic structures (e.g. list)
      Solution: using 2 sorted merge
      -one for computing the length of the allocation
      -one for blitting the fresh record.
      At end: normalization if the record became simple
    *)

    let rec aux acc iv ie =
      if iv >= length_v then
        if ie >= length_e then acc
        else acc + length_e - ie
      else if ie >= length_e then
        acc + length_v - iv
      else
        let fv = Array.get vtable iv in
        let fe = fst (Array.get extend ie) in
        match Field.compare fv fe with
        | -1 -> (* fv < fe *)
            aux (succ acc) (succ iv) ie
        | 0 -> (* fv = fe *)
            aux (succ acc) (succ iv) (succ ie)
        | 1 -> (* fv > fe *)
            aux (succ acc) iv (succ ie)
        | _ -> assert false
    in

    let allocation_length = aux 0 0 0 in

    let allocation_vtable =
      if allocation_length = length_v
      then
        vtable_t
      else
        let tmp = Array.unsafe_create allocation_length in
        (* extra blitting. Even efficient than tmp allocation *)
        let rec aux index iv ie =
          if iv >= length_v then
            if ie >= length_e then ()
            else
              for i = 0 to length_e - ie - 1 do
                Array.set tmp (index + i) (Field.name (fst (Array.get extend (ie + i))))
              done
          else if ie >= length_e then
            for i = 0 to length_v - iv - 1 do
              Array.set tmp (index + i) (Field.name (Array.get vtable (iv + i)))
            done
          else
            let fv = Array.get vtable iv in
            let fe = fst (Array.get extend ie) in
            match Field.compare fv fe with
            | -1 -> (* fv < fe *)
                Array.set tmp index (Field.name fv) ;
                aux (succ index) (succ iv) ie
            | 0 -> (* fv = fe *)
                Array.set tmp index (Field.name fv) ;
                aux (succ index) (succ iv) (succ ie)
            | 1 -> (* fv > fe *)
                Array.set tmp index (Field.name fe) ;
                aux (succ index) iv (succ ie)
            | _ -> assert false
        in
        aux 0 0 0 ;
        VTable.register tmp

    in
    let allocation_record = Array.unsafe_create (allocation_length + val_shift) in

    (* blitting *)

    Array.set allocation_record 0 (Obj.repr allocation_vtable) ;
    Array.set allocation_record 1 (Obj.repr None) ;

    let rec aux index iv ie =
      if iv >= length_v then
        if ie >= length_e then ()
        else
          for i = 0 to length_e - ie - 1 do
            Array.set allocation_record (index + i) (snd (Array.get extend (ie + i)))
          done
      else if ie >= length_e then
        for i = 0 to length_v - iv - 1 do
          Array.set allocation_record (index + i) (Complex.get_no_force (iv + i) record)
        done
      else
        let fv = Array.get vtable iv in
        let fe = fst (Array.get extend ie) in
        match Field.compare fv fe with
        | -1 -> (* fv < fe *)
            Array.set allocation_record index (Complex.get_no_force iv record) ;
            aux (succ index) (succ iv) ie
        | 0 -> (* fv = fe *)
            Array.set allocation_record index (snd (Array.get extend ie)) ;
            aux (succ index) (succ iv) (succ ie)
        | 1 -> (* fv > fe *)
            Array.set allocation_record index (snd (Array.get extend ie)) ;
            aux (succ index) iv (succ ie)
        | _ -> assert false
    in
    aux val_shift 0 0 ;

    (* empty ? *)
    (* the record cannot be empty at this point *)

    (* simple ? *)
    (*
      If the record is simple at this point, that means that the value void was inserted
      from the extend, and so is not a lazy, e.g. :

      { { x = 5 } with x }
    *)
    if allocation_length = 1
      && Array.get allocation_record val_shift == shared_void
    then
      Simple.register (Field.name (Array.get (VTable.export allocation_vtable) 0))
    else
      Complex.init_from_lazy allocation_record

let dot_with_cache cache field record =
  Complex.search_from_exn_cache cache field record ~lowpos:0

let dot_opt field record =
  try Some (Complex.search_from_exn field record ~lowpos:0)
  with
  | Field_not_found -> None

let wrap_option = function
  | None -> none
  | Some a -> some a

let unwrap_option opt =
  dot_opt some_field opt

let dot field record =
  try Complex.search_from_exn field record ~lowpos:0
  with
  | Field_not_found ->
      pp_error (
        "non-existing field %S@\n"^^
        "the record is: %s"
      )
        (Field.name field)
        (DebugPrint.print record)

let unsafe_get = Complex.get_value


(*TODO: Report field name when accessing non-existing field?*)

(*TODO: [unsafe_set] ?*)

(*TODO: Optimize case of [extend_with_list] when the final [vtable] is already known, based on type information.*)

(*TODO: optimize laziness to make sure that [search_from] doesn't need to re-evaluate*)
