(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(* CF mli *)

include Random

let ensure_init =
  let random_self_init = ref false in
  fun () -> if not !random_self_init then begin
    random_self_init := true;
    self_init ();
  end

let max_int () = Nativeint.to_int (Random.nativeint (Nativeint.of_int max_int))
let max_int64 () = int64 Int64.max_int

let string len =
  let s = String.create len in
  for i = 0 to len - 1 do
    s.[i] <- Char.chr (int 128);
  done;
  s

let abstract_string alphabet len =
  let apha_len = String.length alphabet in
  let s = String.create len in
  for i = 0 to len - 1 do
      let x = int apha_len in
      s.[i] <- alphabet.[x];
  done;
  s

let alpha_string len =
  abstract_string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" len
