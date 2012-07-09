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

let (!!) = Annot.annot

type trace = Annot.t AnnotMap.t

exception AnnotNotFound of string * Annot.t
exception ConflictingAnnotations of Annot.t

(* For the semantics, have a look at the .mli. *)


type 'a typed_annot =
    {
      a_ty : 'a option ;
      (* TODO: rename a_tsc to a_tsc_gen and perhaps change it's type
         to [TypeVar.t list] and rename even more *)
      a_tsc : ('a, unit) QmlGenericScheme.tsc option ;
      a_tsc_inst : ('a, unit) QmlGenericScheme.tsc option ;
    }

type 'a gen_annotmap = ('a typed_annot) AnnotMap.t

(* registering exception printers *)
let () = Printexc.register_printer
  (function
     | AnnotNotFound (s,annot) -> Some (Printf.sprintf "QmlAnnotMap.AnnotNotFound (%S,%s)" s (Annot.to_string annot))
     | ConflictingAnnotations annot -> Some (Printf.sprintf "QmlAnnotMap.ConflictingAnnotations %s" (Annot.to_string annot))
     | _ -> None
  )

let default_annot = {
  a_ty = None ;
  a_tsc = None ;
  a_tsc_inst = None ;
}

let empty = AnnotMap.empty

let is_empty am = AnnotMap.is_empty am
let size am = AnnotMap.size am

let rec lift f f_tsc annot = {
  a_ty = Option.map f annot.a_ty ;
  a_tsc = Option.map f_tsc annot.a_tsc ;
  a_tsc_inst = Option.map f_tsc annot.a_tsc_inst ;
}
and map f f_tsc annotmap =
  AnnotMap.map (lift f f_tsc) annotmap

let map_ty_tsc ~ty:f ~tsc:f_tsc annotmap =
  map f f_tsc annotmap
let map f annotmap =
  map_ty_tsc ~ty:f ~tsc:(QmlGenericScheme.map_body_unsafe f) annotmap

let annot_merge conflict_t conflict_s i annot1 annot2 =
  let { a_ty = t1 ; a_tsc = tsc1 ; a_tsc_inst = tsc_inst1 } = annot1
  and { a_ty = t2 ; a_tsc = tsc2 ; a_tsc_inst = tsc_inst2 } = annot2 in
  {
    a_ty = Option.merge (conflict_t i) t1 t2 ;
    a_tsc = Option.merge (conflict_s i) tsc1 tsc2 ;
    a_tsc_inst = Option.merge (conflict_s i) tsc_inst1 tsc_inst2 ;
  }

let merge_i f annotmap1 annotmap2 =
 AnnotMap.merge_i f annotmap1 annotmap2

let merge ?(no_conflict_if_equal=false) annotmap1 annotmap2 =
  let f i x y =
    if no_conflict_if_equal && (x = y) then y
    else raise (ConflictingAnnotations i)
  in
  merge_i (annot_merge f f) annotmap1 annotmap2

let overwrite annotmap1 annotmap2 =
  let f _ _ y = y in
  merge_i (annot_merge f f) annotmap1 annotmap2

let unsafe_overwrite annotmap1 annotmap2 =
  let f _ _ y = y in
  merge_i f annotmap1 annotmap2

let find_opt i annotmap = AnnotMap.find_opt i annotmap
let find_opt_label label = find_opt (!! label)
let find i annotmap = Option.get_exn (AnnotNotFound ("annot", i)) (find_opt i annotmap)
let find_label label = find (!! label)
let add i annot annotmap = AnnotMap.add i annot annotmap
let add_label label = add (!! label)
let remove i annotmap = AnnotMap.remove i annotmap

let find_opt_factory _name accessor i annotmap =
  Option.join (Option.map accessor (find_opt i annotmap))
let find_factory name accessor i annotmap =
  Option.get_exn (AnnotNotFound (name, i)) (accessor (find i annotmap))
let add_factory _name builder i t annotmap =
  match find_opt i annotmap with
  | None -> add i (builder default_annot t) annotmap
  | Some annot -> add i (builder annot t) annotmap

let find_ty_opt i annotmap =
  find_opt_factory "ty"
    (fun x -> x.a_ty)
    i annotmap

let find_ty_opt_label label = find_ty_opt (!! label)

let find_ty i annotmap =
  find_factory "ty"
    (fun x -> x.a_ty)
    i annotmap

let find_ty_label label = find_ty (!! label)

let add_ty i t annotmap =
  add_factory "ty"
    (fun annot t ->  { annot with a_ty = Some t })
    i t annotmap

let add_ty_label label = add_ty (!! label)

let find_tsc_opt i annotmap =
  find_opt_factory "tsc"
    (fun x -> x.a_tsc)
    i annotmap

let find_tsc_opt_label label = find_tsc_opt (!! label)

let find_tsc i annotmap =
  find_factory "tsc"
    (fun x -> x.a_tsc)
    i annotmap

let find_tsc_label label = find_tsc (!! label)

let add_tsc i t annotmap =
  add_factory "tsc"
    (fun annot t -> { annot with a_tsc = Some t } )
    i t annotmap
let add_tsc_label label = add_tsc (!! label)
let add_tsc_opt i t annotmap =
  Option.default annotmap (Option.map (fun t -> add_tsc i t annotmap) t)
let add_tsc_opt_label label = add_tsc_opt (!! label)

let find_tsc_inst_opt i annotmap =
  find_opt_factory "tsc_inst"
    (fun x -> x.a_tsc_inst)
    i annotmap

let find_tsc_inst_opt_label label = find_tsc_inst_opt (!! label)

let find_tsc_inst i annotmap =
  find_factory "tsc_inst"
    (fun x -> x.a_tsc_inst)
    i annotmap

let find_tsc_inst_label label = find_tsc_inst (!! label)

let add_tsc_inst i t annotmap =
  add_factory "tsc_inst"
    (fun annot t -> { annot with a_tsc_inst = Some t })
    i t annotmap

let add_tsc_inst_label label = add_tsc_inst (!! label)

let add_tsc_inst_opt i t annotmap =
  Option.default annotmap (Option.map (fun t -> add_tsc_inst i t annotmap) t)

let add_tsc_inst_opt_label label = add_tsc_inst_opt (!! label)

let remove_tsc i annotmap =
  match find_opt i annotmap with
  | None -> annotmap
  | Some annot -> add i { annot with a_tsc = None } annotmap

let remove_tsc_label label = remove_tsc (!! label)

let remove_tsc_inst i annotmap =
  match find_opt i annotmap with
  | None -> annotmap
  | Some annot -> add i { annot with a_tsc_inst = None } annotmap

let remove_tsc_inst_label label = remove_tsc_inst (!! label)

let iteri ~f_for_key ~f_for_ty ~f_for_tsc ~f_for_tsc_inst
    annotmap =
  AnnotMap.iter
    (fun key bound_value ->
       f_for_key key ;
       f_for_ty bound_value.a_ty ;
       f_for_tsc bound_value.a_tsc ;
       f_for_tsc_inst bound_value.a_tsc_inst)
    annotmap



module Ref = struct

module type REF = sig type ty val _global : (ty gen_annotmap) ref end

module type ANNOTMAPREF =
sig
  type ty

  val clear : unit -> unit
  val import : ty gen_annotmap -> unit
  val merge : ty gen_annotmap -> unit
  val overwrite : ty gen_annotmap -> unit
  val export : unit -> ty gen_annotmap
  val get_opt : Annot.t -> (ty typed_annot) option
  val get : Annot.t -> ty typed_annot
  val set : Annot.t -> ty typed_annot -> unit

(* specific functions: please add more if useful *)
  val set_ty : Annot.t -> ty -> unit
  val get_ty : Annot.t -> ty
  val get_ty_opt : Annot.t -> ty option
  val get_tsc_opt : Annot.t -> (ty, unit) QmlGenericScheme.tsc option

(* we don't provide Ref interface for everything -- please add if you need *)
end

module Make (Ref: REF) : (ANNOTMAPREF with type ty = Ref.ty) =
struct
  type ty = Ref.ty

  let clear () = Ref._global := empty
  let import annotmap = Ref._global := annotmap
  let merge annotmap = Ref._global := merge !Ref._global annotmap
  let overwrite annotmap = Ref._global := overwrite !Ref._global annotmap
  let export () = !Ref._global
  let get_opt i = find_opt i !Ref._global
  let get i = find i !Ref._global
  let set i annot = Ref._global := add i annot !Ref._global

  let set_ty i t = Ref._global := add_ty i t !Ref._global
  let get_ty i = find_ty i !Ref._global
  let get_ty_opt i = find_ty_opt i !Ref._global
  let get_tsc_opt i = find_tsc_opt i !Ref._global

end

end (* module Ref *)
