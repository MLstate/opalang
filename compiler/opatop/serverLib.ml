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
(* depends *)
module Lazy = Base.Lazy

(* alias *)
module Proj = OpaTopValue.Proj

(* shorthands *)
module Q    = QmlAst
module B    = BslTypes
module V    = OpaTopValue

(* -- *)
let debug fmt =
  OManager.printf ("@[<2>@{<cyan>[SL]@} "^^fmt^^"@]@.")

let fail value fmt =
  OManager.printf "RuntimeError:@\n";
  OManager.printf "%a" FilePos.pp_citation (V.pos value);
  OManager.printf "Value: %a" V.pp value;
  OManager.error fmt

(* template
  let _ =
    #<If:BSL_SL $minlevel 1>
      debug "do some %s of level %d@\n" "debug" 1
    #<End>
  in
*)

let compare x y = V.compare ~strong:true (Obj.magic x : V.t) (Obj.magic y : V.t)

type ty_record = V.t
type 'a ty_info = 'a constraint 'a = [> ]

let empty_record = Proj.shared_void

let has_lazy_data r = match r with
  | V.V_record (_ , _, o) ->
      !o <> None

  | _ ->
      fail r "SL.has_lazy_data, expecting a record@\n"

let get_lazy_data_opt r = match r with
  | V.V_record (_, _, o) -> (
      match !o with
      | Some (V.V_extern (_, _, _, o)) -> Some (Obj.obj o)
      | Some x ->
          let _ =
            #<If:BSL_SL $minlevel 1>
              debug "get_lazy_data_opa, Found unexpected format for embedded data in a lazy DB record";
              debug "I'll probably @{<bright>segfault@} now. Pray for my soul.";
              debug "Value: %a@\n" V.pp r
            #<End>
          in
          Some (Obj.magic x)
      | None -> None
    )
  | _ ->
      fail r "SL.get_lazy_data_opt, expecting a record@\n"

(*
  Mathieu: Thu Aug 19 15:15:53 CEST 2010
  I found the following question there about the 2 following functions (embed & inject) lazy data

  What is the status of the "o" we get here ?

  Element of answer:
  The 'o' value is not projected, so it depends on its utilisation in the ml bsl.
  If it is manipulated via the server lib, it can be an opa value,
  if not, this is an ocaml value.
*)

let build_internal_path_t pos o = V.V_extern (pos, "internal_path_t", [], Obj.repr o)

let embed_lazy_data r o = match r with
  | V.V_record (pos, m,_) ->
      V.V_record (pos, m, ref (Option.map (build_internal_path_t pos) o))
  | _ ->
      fail r "SL.embed_lazy_data, expecting a record@\n"

let inject_lazy_data r o = match r with
  | V.V_record (pos, _, oref) ->
      oref := Option.map (build_internal_path_t pos) o

  | _ ->
      fail r "SL.embed_lazy_data, expecting a record@\n"

(*
  FIXME: add a documentation about the exception Exit,
   why it is used there, and who does catch it,
  and what does the program do by catching it.
*)
let at_exit, do_exit, get_exit =
  let at_exit_fun = ref (fun () -> ()) in
  (fun f -> at_exit_fun := f),
  (fun _ -> !at_exit_fun (); raise Exit),
  (fun _ -> !at_exit_fun)

type field = string

let compare_field = String.compare

let get_map = function
  | V.V_record (_, map, _) -> map
  | r ->
      fail r "SL.record-manipulation, expecting a record@\n"

type field_index = field
type fields_indexes = field array
type patterns_indexes = fields_indexes array
type 'a rt_record = ty_record

let fields_indexes x = x
let field_index _ f = f
let patterns_indexes x = x
let dot_with_field_index (rt_record: 'a rt_record) (field_index:field_index) =
  (Obj.magic ( Lazy.force (StringMap.find field_index (get_map rt_record))) : 'a)

let compare_structure pattern_index (r1:'a rt_record) (r2:'a rt_record) =
  (* common code between qmlflat/serverLib
     keep synchronised (bug fix, improvement) until dictionnary is used *)
  let (===) fields map = (* TODO slow should be a dictionnary *)
     fst (StringMap.fold (fun k _v (bool,pos) ->
      (bool && (k==fields.(pos)) ), pos+1
    ) map (true,0))
  in
  let gt = -1 in
  let lt = -2 in
  let rec search v1 v2 i=
    let i = i-1 in
    let fields = pattern_index.(i) in
    if fields === v1 then
    (
        if fields === v2 then i
        else gt (* v1 is has bigger index *)
    )
    else if fields === v2 then
      lt (* v2 is has bigger index *)
    else if i != 0 then
      search v1 v2 (i-1)
    else
      fail r1 "serverLib.compare_structure : pattern_index is not compatible with record"
  in
  let v1 = get_map r1 in
  let v2 = get_map r2 in
  let n = Array.length pattern_index in
  search v1 v2 n


let fold_record folder record =
  StringMap.fold
    (fun field value ->
       folder field (Obj.magic value))
    (get_map (Obj.magic record))

let fold_2_record folder r1 r2 acc =
  let map2 = get_map (Obj.magic r2) in
  StringMap.fold
    (fun field value ->
       folder field (Obj.magic value) (Obj.magic (StringMap.find field map2)))
    (get_map (Obj.magic r1)) acc

let name_of_field field = Some field
let field_of_name = name_of_field

let static_name_of_field field = field
let static_field_of_name = static_name_of_field

type record_constructor = V.t Lazy.t StringMap.t

let empty_record_constructor = StringMap.empty

let add_field cons field value =
  StringMap.add field (Lazy.lazy_from_val (Obj.magic value)) cons

let make_record cons = Obj.magic (V.V_record (V.nopos, cons, ref None))
let make_simple_record field = make_record (add_field empty_record_constructor field empty_record)

let dot r field = match r with
  | V.V_record (_ , map, _) -> (
      match StringMap.find_opt field map with
      | None -> None
      | Some lazy_val -> (
          let value = Obj.magic (Lazy.force lazy_val) in
          Some value
        )
    )
  | _ ->
      fail r "SL.dot, expecting a record@\n"

let unsafe_dot r field =
  match dot r field with
  | Some value -> value
  | None ->
      fail r "SL.unsafe_dot, runtime error, no field %S@\n" field

let is_present r field =
  match r with
  | V.V_record (_ , map, _) -> (
      StringMap.mem field map
    )
  | _ ->
      fail r "SL.is_present, expecting a record@\n"

(* Explicit projection API *)

(* standard bsl types *)
(* in qmltop, everything is boxed, so that catchable RuntimeErrors can replace seg faults *)
type ty_char = V.t
type ty_float = V.t
type ty_int = V.t
type ty_null = V.t
type ty_string = V.t

let wrap_float c = V.Proj.t_float c
let unwrap_float = function
  | V.V_const (_, Q.Float f) -> f
  | t ->
      fail t "SL.unwrap_float, expecting a float@\n"

let wrap_int c = V.Proj.t_int c
let unwrap_int = function
  | V.V_const (_, Q.Int i) -> Big_int.int_of_big_int i
  | t ->
      fail t "SL.unwrap_int, expecting an int@\n"

let null = V.t_null ~pos:(FilePos.nopos "SL.null") ()

let wrap_string c = V.Proj.t_string c
let unwrap_string = function
  | V.V_const (_, Q.String s) -> s
  | t ->
      fail t "SL.unwrap_string, expecting an string@\n"

type ty_void = V.t
let void = empty_record

type ty_bool = V.t

let shared_true = V.Proj.t_bool true
let shared_false = V.Proj.t_bool false
let wrap_bool b =
  if b then shared_true else shared_false

let true_ = shared_true
let false_ = shared_false

let unwrap_bool = function
  | V.V_record (_, fields, _) ->
      let semantic_bool = (StringMap.mem "true" fields) && not (StringMap.mem "false" fields) in
      Obj.magic semantic_bool
  | t ->
      fail t "SL.unwrap_bool, expecting a bool@\n"

type 'a ty_option = V.t

let none = V.Proj.t_none ()
let some a = V.Proj.t_some (Obj.magic a)

let wrap_option = function
  | None -> none
  | Some a -> some a

let unwrap_option t =
  let clash () =
    fail t "SL.unwrap_option, expecting an option@\n"
  in
  match t with
  | V.V_record (_, fields, _) ->
      let semantic_option =
        match StringMap.find_opt "some" fields with
        | None ->
            if not (StringMap.mem "none" fields)
            then clash ()
            else None
        | Some v -> Some (Lazy.force v)
      in
      Obj.magic semantic_option
  | _ -> clash ()


(* support for marshaling *)
let deep_force_eval a = Obj.obj (Lazy.deep_force (Obj.repr a))

(*
  No sharing in opatop
*)
external sharing_refresh : 'a -> 'a = "%identity"
