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


(* shorthands *)
module F = Init.F
module Migration = Init.Migration

(* Migrate to version 20
 * Changes :
 *   add the revision of the last snapshot
 *)

let migrate_to_version_20 v_20 fm =

  let init = Migration.initialize v_20 fm [ F.Uid_rev ; F.Config ] in

  let ur = F.Uid_rev in
  let length_ur = F.length fm ur in
  (* get last revision *)
  F.seek_in fm ur (length_ur - 4);
  let lastrev = F.read_int fm ur in
  (* snapshot at version 18 are taken each 1000 revision *)
  let lastsn = (lastrev / 1000) * 1000 in

  F.empty_file fm F.Config;
  F.add fm F.Config [ F.WInt 20; F.WInt lastsn ];

  F.seek_in fm ur 0;
  F.seek_out fm F.Config 0 ;

  Migration.finalize fm init
