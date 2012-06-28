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
