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

(* Migrate to version 23
 * Changes:
 *   Flag file is now incremental
 * *)
let migrate_to_version_23 v_23 fm =
  let init = Migration.initialize v_23 fm [ F.Config; F.Flags; F.Uid_rev ] in

  (* retrieve old flags *)
  F.seek_in fm F.Flags 0;
  let flag_file = F.read fm F.Flags (List.init 5 (fun _ -> F.RInt)) in

  (* get last revision *)
  let upos = match List.hd flag_file with F.WInt u -> u | _ -> assert false in
  F.seek_in fm F.Uid_rev (upos - 4);
  let lastrev = F.read_int fm F.Uid_rev in
  let new_beginnning_of_flags_file = lastrev * 20 in


  (* add flags at the good place & padding from the beginning *)
  let padding = (List.init 5 (fun _ -> F.WInt 0)) in
  F.seek_out fm F.Flags 0;
  F.add fm F.Flags padding;
  F.seek_out fm F.Flags new_beginnning_of_flags_file;
  F.add fm F.Flags flag_file;
  (* "truncate" the file *)
  let new_length = new_beginnning_of_flags_file + 20 in
  F.set_size fm F.Flags new_length;


  Migration.finalize fm init
