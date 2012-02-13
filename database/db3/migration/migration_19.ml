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

(* depends*)
module List = BaseList

(* shorthands *)
module F = Init.F
module Migration = Init.Migration


(* Migrate to version 19
 * Changes :
 *   Time format float -> int
 *)
let migrate_to_version_19 v_19 fm =
  let tms = F.Timestamp in

  let init = Migration.initialize v_19 fm [ tms ] in

  (* migrate timestamps file *)
  let length = F.length fm tms in
  let to_read =
    List.tail_concat
      (List.init (length / 9)
         (fun _ -> [F.RChar; F.RFloat]))
  in
  let timestamps = F.read fm tms to_read in

  (* FIXME empty file if append? *)
  F.seek_out fm tms 0;

  let to_write =
    List.map
      (fun x ->
         match x with
         | F.WChar '\000' -> x
         | F.WFloat f -> F.WInt64 (Int64.of_int (Time.in_milliseconds (Time.of_unix_time f)))
         | _ -> Init.error "Timestamp file corrupted; cannot migrate"; raise F.Corruption) timestamps in
  F.add fm tms to_write;

  Migration.finalize fm init
