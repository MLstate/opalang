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
module I = Init
module F = Init.F
module Migration = Init.Migration

(*
  Migration to version 22
  Changes:
  EidMap is now expected to be in increasing order of eids, for recovery.
*)

let migrate_to_version_22 v_22 fm =
  let init = Migration.initialize v_22 fm [ F.Db_state ] in

  let ch = F.Channel.get fm F.Db_state in
  let out lst = List.iter (F.Channel.add ch) lst in
  let char_int_char_int = [F.RChar; F.RInt; F.RChar; F.RInt ] in

  let map =
    if (F.read_char fm F.Db_state) <> I.cc0 then raise F.Corruption;
    let size = F.read_int fm F.Db_state in
    Loop.For.range 0 size IntMap.empty (
      fun _ uidmap -> (
        let eid, rev_size =
          match F.read fm F.Db_state char_int_char_int with
          | [ c00; F.WInt eid; c11; F.WInt rev_size ] when c00 = I.c0 && c11 = I.c1 ->
              eid, rev_size
          | _ -> raise F.Corruption
        in
        let revmap =
          Loop.For.range 0 rev_size IntMap.empty (
            fun _ revmap -> (
              match F.read fm F.Db_state char_int_char_int with
              | [ c00 ; F.WInt r ; c11 ; F.WInt u ] when c00 = I.c0 && c11 = I.c1 ->
                  IntMap.add r u revmap
              | _ -> raise F.Corruption
            )) in
        IntMap.add eid revmap uidmap
      )) in

  F.seek_out fm F.Db_state 0;


  let size = IntMap.size map in
  let lst = [ I.c0; F.WInt size ] in
  out lst;

  IntMap.iter
    (fun eid rum ->
       let lst = [ I.c0; F.WInt eid; I.c1; F.WInt (IntMap.size rum) ] in
       out lst;
       IntMap.iter
         (fun r u ->
            let lst = [I.c0; F.WInt r; I.c1; F.WInt u] in
            out lst) rum) map;

  Migration.finalize fm init
