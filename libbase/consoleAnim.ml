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

type anim = string array array

type t = {
  och : out_channel ;
  step : int ref ;
  auto_walking : bool ;
  walk_anim : anim ;
  turn_anim : anim ;
  walk_anim_rev : anim ;
  turn_anim_rev : anim ;
}

(* please, extend it if needed *)
let reverse_char = function
  | '\\' -> '/'
  | '/' -> '\\'
  | '(' -> ')'
  | ')' -> '('
  | '[' -> ']'
  | ']' -> '['
  | '{' -> '}'
  | '}' -> '{'
  | '<' -> '>'
  | '>' -> '<'
  | c -> c

(* sorry, we can't use String.init because Base depends on Journal *)
let reverse_string s =
  let len = String.length s in
  let s' = String.make len ' ' in
  for i = 0 to len - 1 do
    s'.[i] <- reverse_char s.[len - 1 - i]
  done;
  s'

let print_frame och pos frame =
  let output s = Pervasives.output_string och s in
  let flush () = Pervasives.flush och in
  let reset =
    let n = Array.length frame in
    if n > 1 then Printf.sprintf "\027[%dA\r" (n-1) else "\r" in
  output "\r";
  (* Print the frame *)
  Array.iteri
    (fun i l ->
       if i <> 0 then output "\027E";
       if pos <> 0 then output (Printf.sprintf "\027[%dC" pos);
       output l)
    frame;
  output reset;
  (* Then flush and clear, so that it's absent in further console printings *)
  flush ();
  (* if pos <> 0 then output (Printf.sprintf "\027[%dC" pos); *)
  (* if Array.length frame <> 0 *)
  (* then output (String.make (String.length frame.(0)) ' '); *)
  Array.iteri
    (fun i l ->
       if i <> 0 then output "\027E";
       if pos <> 0 then output (Printf.sprintf "\027[%dC" pos);
       output (String.make (String.length l) ' '))
    frame;
  (* Replace the cursor where it was found *)
  output reset

let anim t x =
  let print_frame = print_frame t.och in
  let walk_length = Array.length t.walk_anim in
  let turn_length = Array.length t.turn_anim in
  let i = x mod ((60 * walk_length + turn_length) * 2) in
  if i < 60 * walk_length then
    print_frame (if t.auto_walking then i / walk_length else 0) t.walk_anim.(i mod walk_length)
  else if i < 60 * walk_length + turn_length then
    print_frame (if t.auto_walking then 59 else 0) t.turn_anim.(i - 60 * walk_length)
  else if i < 120 * walk_length + turn_length then
    let j = i - 60 * walk_length - turn_length in
    print_frame (if t.auto_walking then 60 - j / walk_length else 0) t.walk_anim_rev.(j mod walk_length)
  else
    let j = i - 120 * walk_length - turn_length in
    print_frame 0 t.turn_anim_rev.(j)

let check ~auto_walking walk_anim turn_anim =
  if auto_walking then begin
    let check_frame frame =
      if Array.length frame > 0 then
        let l = String.length frame.(0) in
        Array.iter (fun s -> if String.length s <> l then failwith "Different frame lengths") frame
    in
    Array.iter check_frame walk_anim;
    Array.iter check_frame turn_anim
  end

let init ?(och=stdout) ~auto_walking walk_anim turn_anim =
  check ~auto_walking walk_anim turn_anim;
  {
    och = och ;
    step = ref 0 ;
    auto_walking = auto_walking ;
    walk_anim = walk_anim ;
    turn_anim = turn_anim ;
    walk_anim_rev = Array.map (Array.map reverse_string) walk_anim ;
    turn_anim_rev = Array.map (Array.map reverse_string) turn_anim ;
  }

let reset t = t.step := 0

let update t = anim t !(t.step); incr t.step
