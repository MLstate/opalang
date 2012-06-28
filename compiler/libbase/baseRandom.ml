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
