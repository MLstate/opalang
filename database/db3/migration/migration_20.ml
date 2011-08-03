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
