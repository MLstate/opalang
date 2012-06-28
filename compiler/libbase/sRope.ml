(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

type t =
  | Leaf of int * string (* string, real string length *)
  | Node of int * t * t  (* left, right, len *)

(* joke: who does want a Walker ? *)

(* start size for a Leaf, after that perform each realloc * 2 *)
let create hint = Leaf (0, String.create hint)

external length : t -> int = "%field0"

let concat t t' = Node ((length t) + (length t'), t, t')

let diverge = function
  | Leaf (i, s) -> Leaf (i, String.copy s)
  | Node (i, t, Leaf (j, s)) -> Node (i, t, Leaf (j, String.copy s))
  | t -> t

let s_alloc min (i:int) =
  Pervasives.min Sys.max_string_length (Pervasives.max min i)

let add_leaf e len_e s =
  let full_e = String.length e in
  let len_s = String.length s in
  let len_es = len_e + len_s in
  let rest_e = full_e - len_e in
  if len_s <= rest_e
  then (* s can be inserted in place *)
    let _ = String.unsafe_blit s 0 e len_e len_s in
    Leaf (len_es, e)
  else
    (* blit fully e, and allocate a new Leaf *)
    let _ = String.unsafe_blit s 0 e len_e rest_e in
    let rest_s = len_s - rest_e in
    let snew = String.create (s_alloc rest_s (2 * full_e)) in
    let _ = String.unsafe_blit s rest_e snew 0 rest_s in
    Node (len_es, Leaf (full_e, e), Leaf (rest_s, snew))

let add t s =
  match t with
  | Node (len_t, t, Leaf (len_e, e)) ->
      let t' = add_leaf e len_e s in
      Node (len_t + (String.length s), t, t')
  | Leaf (len_e, e) -> add_leaf e len_e s
  | _ ->
      let len_t = length t in
      let len_s = String.length s in
      Node (len_t + len_s, t, Leaf (len_s, s))

let addln t s = add (add t s) "\n"

let iter f t =
  let stack = Stack.create () in
  let push t = Stack.push t stack in
  let pop () =
    match Stack.pop stack with
    | Node (_, l, r) ->
        push r ;
        push l
    | Leaf (len_s, s) ->
        f s 0 len_s
  in
  try
    push t ;
    while true do
      pop ()
    done
  with
  | Stack.Empty -> ()

let fold f acc t =
  let stack = Stack.create () in
  let push t = Stack.push t stack in
  let acc = ref acc in
  let pop () =
    match Stack.pop stack with
    | Node (_, l, r) ->
        push r ;
        push l
    | Leaf (len_s, s) ->
        acc := f !acc s 0 len_s
  in
  try
    push t ;
    while true do
      pop ()
    done ;
    !acc
  with
  | Stack.Empty -> !acc

let rev_iter f t =
  let stack = Stack.create () in
  let push t = Stack.push t stack in
  let pop () =
    match Stack.pop stack with
    | Node (_, l, r) ->
        push l ;
        push r
    | Leaf (len_s, s) ->
        f s 0 len_s
  in
  try
    push t ;
    while true do
      pop ()
    done
  with
  | Stack.Empty -> ()

let rev_fold f acc t =
  let stack = Stack.create () in
  let push t = Stack.push t stack in
  let acc = ref acc in
  let pop () =
    match Stack.pop stack with
    | Node (_, l, r) ->
        push l ;
        push r
    | Leaf (len_s, s) ->
        acc := f !acc s 0 len_s
  in
  try
    push t ;
    while true do
      pop ()
    done ;
    !acc
  with
  | Stack.Empty -> !acc


let iter_sub f = iter (fun s start len -> f (String.sub s start len))
let fold_sub f = fold (fun acc s start len -> f acc (String.sub s start len))

let add_substring t s start len = add t (String.sub s start len)

let contents t =
  let len = length t in
  if len > Sys.max_string_length then invalid_arg "SRope.contents" else
  let s = String.create len in
  let push =
    let p = ref 0 in
    ( fun s' start len ->
        Base.String.unsafe_blit s' start s !p len ;
        p := !p + len ) in
  iter push t ;
  s

(* should be tail rec => Stack *)
let sub t start len =
  let s = String.create len in
  (* bliting from left to right *)
  let blit =
    let p = ref 0 in
    (fun src start len -> String.unsafe_blit src start s !p len ; p := !p + len)
  in
  let stack = Stack.create () in
  let push t = Stack.push t stack in
  let pop () =
    match Stack.pop stack with
    | t, start, len ->
        begin
          match t with
          | Leaf (len_s, s) ->
              if start + len > len_s then invalid_arg "SRope.sub"
              else blit s start len
          | Node (len_n, l, r) ->
              let stop = start + len in
              if start + len > len_n then invalid_arg "SRope.sub"
              else
                let a = length l in
                if stop < a
                then push (l, start, len)
                else
                  (* stop >= a *)
                  if start >= a
                  then push (r, (start - a), len)
                  else
                    (* should take from l and from r, Stack => push r first *)
                    let from_a = a - start in
                    ( push (r, 0, (len - from_a)) ;
                      push (l, start, from_a) )
        end
  in
  try
    push (t, start, len) ;
    while true do
      pop ()
    done ;
    s
  with
  | Stack.Empty -> s

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
let implementation =
  { FBuffer.
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
