(*
    Copyright Â© 2011 MLstate

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
(** String Edit Distance / Levenshtein 
    @author Henri.Binsztok\@gmail.com
*)

let sed distance s1 s2 =
  let l1 = String.length s1
  and l2 = String.length s2 in  
  let mat = Array.make_matrix (succ l1) (succ l2) 0. in
  mat.(0).(0) <- 0. ;
  for i = 1 to l1 do
    mat.(i).(0) <- mat.(pred i).(0) +. distance None (Some (String.unsafe_get s1 (pred i)))
  done ;
  for j = 1 to l2 do
    mat.(0).(j) <- mat.(0).(pred j) +. distance (Some (String.unsafe_get s2 (pred j))) None
  done ;
  for i = 0 to pred l1 do
    for j = 0 to pred l2 do
      let x1 = mat.(i).(succ j) +. distance (Some (String.unsafe_get s1 i)) None
      and x2 = mat.(succ i).(j) +. distance (Some (String.unsafe_get s2 j)) None
      and x3 = mat.(i).(j)      +. distance (Some (String.unsafe_get s1 i)) (Some (String.unsafe_get s2 j)) in
      mat.(succ i).(succ j) <- min x1 (min x2 x3)
    done
  done ;
  mat.(l1).(l2)

(* FIXME: vraie map / sauvé en binary... ! *)
(* FIXME: optimiser les valeurs ? *)
(* FIXME: prendre une layout clavier en entrée... *)
let qwerty_map =
  let row y0 x0 w u1 u2 =
    let m = String.length u1 in
    let r = ref [] in
    for i = 0 to m - 1 do
      r := (y0, x0 +. (float_of_int i) *. w, 0.0, u1.[i])::!r;
      r := (y0, x0 +. (float_of_int i) *. w, 1.0, u2.[i])::!r
    done;
    !r in
  let qwerty_dummy = (20.0, 4.0, 0.5)
  and qwerty_description = List.concat [
    row 0.0 0.0 1.7461 "`1234567890-=" "~!@#$%^&*()_+" ;
    [1.5, 1.0, 0.0, '\t'] ;
    row 1.5 2.8 1.7461 "qwertyuiop[]" "QWERTYUIOP{}" ;
    row 3.0 3.4 1.7461 "asdfghjkl;'\\" "ASDFGHJKL:\"|";
    row 4.5 2.8 1.7461 "<zxcvbnm,./" ">ZXCVBNM<>?" ;
    [6.0, 12.5, 0.0, ' ']
  ] in
  let a = Array.make 256 qwerty_dummy in
  List.iter (fun (x,y,z,c) -> a.(Char.code c) <- (x,y,z)) qwerty_description ;
  a

let euclidian_distance (x1, y1, z1) (x2, y2, z2) =
  let f a1 a2 = (a1 -. a2) *. (a1 -. a2) in
  sqrt ((f x1 x2) +. (f y1 y2) +. (f z1 z2))

let qwerty_distance =
  let insertion_cost = 7.0
  and deletion_cost = 5.0 in
  fun c1 c2 ->  
    let p c = qwerty_map.(Char.code c) in
    match c1, c2 with
    | None, None -> 0.
    | Some _, None -> deletion_cost
    | None, Some _ -> insertion_cost
    | Some c1, Some c2 -> euclidian_distance (p c1) (p c2)

let simple_distance c1 c2 = match c1,c2 with
  | None, None -> 0.
  | Some c1, Some c2 -> if c1 = c2 then 0. else 1.
  | _ -> 1. (* insertion, deletion *)

let qwerty_sed = sed qwerty_distance

let simple_sed = sed simple_distance
