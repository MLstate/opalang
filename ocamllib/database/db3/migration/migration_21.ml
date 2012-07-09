(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

(* depends*)
module List = BaseList

(* shorthands *)
module I = Init
module F = Init.F
module Migration = I.Migration

(*
  Migration to version 21
  Changes:
  Index is now expected to be in increasing order of words.
*)
module MigrationUtils_21 =
struct

  module K = Keys

  let rec key2fm = function
    | K.IntKey i -> [I.c0; F.WInt i]
    | K.StringKey s -> [I.c1; F.WString s]
    | K.ListKey (l) ->
        let ll =
          let fold acc ik =
            List.rev_append (key2fm ik) acc
          in
          Array.fold_left fold [] l
        in
        I.c2 :: F.WInt (Array.length l) :: (List.rev ll)
    | K.VariableKey _ -> [I.c3]

  let rec fm2key fm f =
    let shb = F.read_char fm f in
    match shb with
    | c when c = I.cc0 -> K.IntKey (F.read_int fm f)
    | c when c = I.cc1 -> K.StringKey (F.read_string fm f)
    | c when c = I.cc2 ->
        let size = F.read_int fm f in
        let arr = Array.make size (K.IntKey 0) in
        for i = 0 to pred size do
          Array.unsafe_set arr i (fm2key fm f)
        done ;
        K.ListKey arr
    | c when c = I.cc3 -> K.VariableKey 0
    | _ -> raise F.Corruption

  let path2fm path =
    let rev_keys = Path.write path in
    F.WInt (List.length rev_keys) :: List.concat_map key2fm rev_keys

  let fm2path fm f =
    let plength = F.read_int fm f in
    let rev_keys = List.side_effect_init plength (fun _ -> fm2key fm f) in
    Path.read rev_keys

  let char_int_char_int = [F.RChar; F.RInt; F.RChar; F.RInt ]
end

let migrate_to_version_21 v_21 fm =

  let init = Migration.initialize v_21 fm [ F.Db_state ] in

  (* uidmap *)
  let db = F.Db_state in
  let _ = F.read_char fm db in
  let size = F.read_int fm db in
  for _i = 0 to pred size do
    let rev_size =
      match F.read fm db MigrationUtils_21.char_int_char_int with
      | [ c00; F.WInt _; c11; F.WInt rev_size ] when c00 = I.c0 && c11 = I.c1 ->
          rev_size
      | _ ->
          raise F.Corruption
    in
    for _i = 0 to pred rev_size do
      match F.read fm db MigrationUtils_21.char_int_char_int with
      | [ c00 ; F.WInt _ ; c11 ; F.WInt _ ] when c00 = I.c0 && c11 = I.c1 ->
          ()
      | _ ->
          raise F.Corruption
    done ;
  done ;

  (* index *)
  let _ = F.read_char fm db in
  let size = F.read_int fm db in
  let end_uidmap_pos = F.position_in fm F.Db_state in

  let words = Array.make size "" in
  let pfls = Array.make size [] in
  let db_chan = F.Channel.get fm F.Db_state in
  for index = 0 to pred size do
    let c00 = F.Channel.read_char db_chan in
    let word = F.Channel.read_string db_chan in

    let c11 = F.Channel.read_char db_chan in
    let pfl_size = F.Channel.read_int db_chan in

    let c22 = F.Channel.read_char db_chan in
    if not (c00 = I.cc0 && c11 = I.cc1 && c22 = I.cc2) then raise F.Corruption;

    let pfl = List.init pfl_size (
      fun _ ->
        let _ = F.read_char fm db in
        let path = MigrationUtils_21.fm2path fm db in
        let _ = F.read_char fm db in
        let fl = F.read_float fm db in
        path, fl
    ) in
    Array.unsafe_set words index word ;
    Array.unsafe_set pfls index pfl ;
  done ;

  let write_index index =
    let word = Array.unsafe_get words index in
    let pfl = Array.unsafe_get pfls index in
    let pfl_size = List.length pfl in

    F.Channel.add_char db_chan I.cc0 ;
    F.Channel.add_string db_chan word ;

    F.Channel.add_char db_chan I.cc1 ;
    F.Channel.add_int db_chan pfl_size ;

    F.Channel.add_char db_chan I.cc2 ;
    List.iter (
      fun (path, fl) ->
        let lst = I.c0 :: (MigrationUtils_21.path2fm path) @ [I.c1; F.WFloat fl] in
        List.iter (F.Channel.add db_chan) lst
    ) pfl
  in
  (*
    Before the version 21, the order used for storing the index was not
    specified. Now, it is needed that the index is stored in increasing
    order of words.
  *)
  let is_decreasing =
    (size >= 2) && (words.(0) > words.(1))
  in
  F.seek_out fm F.Db_state end_uidmap_pos ;
  if is_decreasing
  then
    for index = pred size downto 0 do
      write_index index
    done
  else
    for index = 0 to pred size do
      write_index index
    done
  ;

  Migration.finalize fm init
