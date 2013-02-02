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

type data = string list
type t =
    {
      pos : int ; (* the len currently used in (hd data) *)
      data : data ; (* rev *)
      len_data : int (* caching sum of length of data *)
    }

let s_alloc min (i:int) =
  Pervasives.min Sys.max_string_length (Pervasives.max min i)

let create ?name:_ hint =
  {
    pos = 0 ;
    data = [ String.create hint ] ;
    len_data = 0
  }

let diverge t =
  match t.data with
  | h::q -> { t with data = (String.copy h)::q }
  | _ -> t

(* Mathieu : Sat Jul  3 01:26:23 CEST 2010
   + Keep ident like in SRope for homogenity
   + merging add and add_substring
*)

let add_substring t s start len_s =
  ( if start < 0 || len_s < 0 || start + len_s > String.length s then invalid_arg "FBuffer.add_substring" ) ;
  let e = List.hd t.data in
  let full_e = String.length e in
  let len_e = t.pos in
  let rest_e = full_e - len_e in
  if len_s <= rest_e
  then (* s can be inserted in place *)
    let _ = String.unsafe_blit s start e len_e len_s in
    { t with pos = len_e + len_s }
  else
    (* blit fully e, and allocated a new head *)
    let _ = String.unsafe_blit s start e len_e rest_e in
    let rest_s = len_s - rest_e in
    let snew = String.create (s_alloc rest_s (2 * full_e)) in
    let _ = String.unsafe_blit s (start + rest_e) snew 0 rest_s in
    { pos = rest_s ; data = snew :: t.data ; len_data = full_e + t.len_data }

let add t s = add_substring t s 0 (String.length s)

let addln t s = add (add t s) "\n"

(* must be keeped tail rec *)
let iter f t =
  match t.data with
  | [] -> ()
  | s::q ->
      let iter s = f s 0 (String.length s) in
      List.iter iter (List.rev q) ;
      f s 0 t.pos

let fold f acc t =
  match t.data with
  | [] -> acc
  | s::q ->
      let fold acc s = f acc s 0 (String.length s) in
      let acc = List.fold_left fold acc (List.rev q) in
      f acc s 0 t.pos

let rev_iter f t =
  match t.data with
  | [] -> ()
  | s::q ->
      f s 0 t.pos ;
      let iter s = f s 0 (String.length s) in
      List.iter iter q

let rev_fold f acc t =
  match t.data with
  | [] -> acc
  | s::q ->
      let acc = f acc s 0 t.pos in
      let fold acc s = f acc s 0 (String.length s) in
      List.fold_left fold acc q

let iter_sub f t =
  match t.data with
  | [] -> ()
  | s::q ->
      List.iter f (List.rev q) ;
      f (String.sub s 0 t.pos)

let fold_sub f acc t =
  match t.data with
  | [] -> acc
  | s::q ->
      let acc = List.fold_left f acc (List.rev q) in
      f acc (String.sub s 0 t.pos)

let concat t1 t2 =
  {
    pos = t2.pos ;
    data = (
      match t1.data with
      | hd :: tl ->
          (* please, keep tail rec *)
          List.rev_append (List.rev t2.data)
            (String.sub hd 0 t1.pos :: tl)
      | [] -> t2.data
    ) ;
    len_data = t1.len_data + t1.pos + t2.len_data
  }

let length t = t.pos + t.len_data
  (* without cache is : *)
  (* t.pos +
     (List.fold_left (fun acc x -> acc + String.length x) 0 (List.tl t.data)) *)

(* rev_iter has better perf, we could use it, or inline its code there *)
let contents t =
  let len = length t in
  if len > Sys.max_string_length then invalid_arg "FBuffer.contents"
  else
    let r = String.create len in
    let len_h = t.pos in
    let p = ref (len - len_h) in
    let _ =
      match t.data with
      | [] -> ()
      | h::q ->
          String.unsafe_blit h 0 r !p len_h ;
          List.iter
            ( fun s ->
                let len_s = String.length s in
                p := !p - len_s ;
                String.unsafe_blit s 0 r !p len_s ) q
    in
    r

let sub t start len =
  ( if start < 0 || len < 0 then invalid_arg "FBuffer.sub" ) ;
  let s = String.create len in
  let len_data = t.len_data in
  let right_s = start + len in
  let length = len_data + t.pos in
  ( if right_s > length then invalid_arg "FBuffer.sub" ) ;
  let _ =
    if start >= len_data
    then String.unsafe_blit (List.hd t.data) (start - len_data) s 0 len
    else (* start < len_data *)
      (* bliting s from right to left. *)
      let blit =
        let p = ref len in
        (fun src start len -> p := !p - len ; String.unsafe_blit src start s !p len)
      in
      (* please, keep tail rec *)
      let rec aux right_elt right_s = function
        | [] -> ()
        | elt::q ->
            let left_elt = right_elt - (String.length elt) in
            if right_s <= left_elt then aux left_elt right_s q
            else (* some piece of elt goes into s *)
              if start >= left_elt (* elt is the last elt going into s *)
              then blit elt (start - left_elt) (right_s - start)
              else
                begin
                  (* blit elt into s and continue with q *)
                  blit elt 0 (right_s - left_elt) ;
                  aux left_elt left_elt q
                end
      in
      (* <!> special case for hd because (String.length hd <> t.pos) *)
      let right_elt = length in
      match t.data with
      | elt::q ->
          let left_elt = right_elt - t.pos (* <> String.length elt *) in
          if right_s <= left_elt then aux left_elt right_s q
          else (* some piece of elt goes into s *)
            if start >= left_elt (* elt is the last elt going into s *)
            then blit elt (start - left_elt) (right_s - start)
            else
              begin
                (* blit elt into s and continue with q *)
                blit elt 0 (right_s - left_elt) ;
                aux left_elt left_elt q
              end
      | _ -> assert false (* internal error *)
  in
  s

let output oc = iter (Pervasives.output oc)

(* format *)
let make_formatter t_ref =
  Format.make_formatter
    (fun s start len -> t_ref := add_substring !t_ref s start len)
    (fun _ -> ())

let fmt fmt =
  let pp = fst (Format.pp_get_formatter_output_functions fmt ()) in
  iter pp

let printf t fmt =
  let t = ref t in
  let formatter = make_formatter t in
  Format.kfprintf (fun _ -> Format.pp_print_flush formatter (); !t) formatter fmt

let sprintf fmt =
  let t = ref (create 1024) in
  let formatter = make_formatter t in
  Format.kfprintf (fun _ -> Format.pp_print_flush formatter (); contents !t) formatter fmt

(* dynamic choice of impl *)
type 'fbuffer implementation =
    {
      create : int -> 'fbuffer ;
      add : 'fbuffer -> string -> 'fbuffer ;
      addln : 'fbuffer -> string -> 'fbuffer ;
      concat : 'fbuffer -> 'fbuffer -> 'fbuffer ;
      add_substring : 'fbuffer -> string -> int -> int -> 'fbuffer ;
      diverge : 'fbuffer -> 'fbuffer ;
      contents : 'fbuffer -> string ;
      output : out_channel -> 'fbuffer -> unit ;
      length : 'fbuffer -> int ;
      sub : 'fbuffer -> int -> int -> string ;
      iter : (string -> int -> int -> unit) -> 'fbuffer -> unit ;
      fold : 'a. ('a -> string -> int -> int -> 'a) -> 'a -> 'fbuffer -> 'a ;
      rev_iter : (string -> int -> int -> unit) -> 'fbuffer -> unit ;
      rev_fold : 'a. ('a -> string -> int -> int -> 'a) -> 'a -> 'fbuffer -> 'a ;
      iter_sub : (string -> unit) -> 'fbuffer -> unit ;
      fold_sub : 'a. ('a -> string -> 'a) -> 'a -> 'fbuffer -> 'a ;
      fmt : Format.formatter -> 'fbuffer -> unit ;
      printf : 'params. 'fbuffer -> ('params, Format.formatter, unit, 'fbuffer) format4 -> 'params ;
      sprintf : 'params. ('params, Format.formatter, unit, string) format4 -> 'params ;
    }

let implementation =
  {
    create = create ;
    add = add ;
    addln = addln ;
    concat = concat ;
    add_substring = add_substring ;
    diverge = diverge ;
    contents = contents ;
    output = output ;
    length = length ;
    sub = sub ;
    iter = iter ;
    fold = fold ;
    rev_iter = rev_iter ;
    rev_fold = rev_fold ;
    iter_sub = iter_sub ;
    fold_sub = fold_sub ;
    fmt = fmt ;
    printf = printf ;
    sprintf = sprintf ;
  }

(* deprecated, backward compat *)
let make ?name:_ i = create i
let union = concat
let write t oc = output oc t
