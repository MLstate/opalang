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
module I = Init
module F = Init.F
module Migration = Init.Migration

module MigrationUtils_25 =
struct

  let concat = List.tail_concat
  let init_concat l f = concat (List.init l f)
  let rev_concat l = concat (List.rev l)
  let init_rev_concat l f = rev_concat (List.init l f)


  let rec k fm f =
    match F.read_char fm f with
    | '\000' ->
      let i = F.read_int fm f in
        (* XXX: CHANGE HERE *)
      let ii = Int64.of_int i in
      [F.WChar '\000'; F.WInt64 ii]
    | '\001' ->
      let s = F.read_string fm f in
      [F.WChar '\001'; F.WString s]
    | '\002' ->
      let l = F.read_int fm f in
      let keys = init_concat l (fun _ -> k fm f) in
      F.WChar '\003' :: F.WInt l :: keys
    | '\003' ->
      [F.WChar '\003']
    | _ -> raise F.Corruption

  let p fm f =
    let l = F.read_int fm f in
    let lst = init_rev_concat l (fun _ -> k fm f) in
    F.WInt l :: lst

  let di fm f =
    match F.read_char fm f with
    | 'J' -> F.WChar 'J' :: F.read fm f [F.RInt64]
    | 'S' -> F.WChar 'S' :: F.read fm f [F.RString]
    | 'B' -> F.WChar 'B' :: F.read fm f [F.RString]
    | 'F' -> F.WChar 'F' :: F.read fm f [F.RFloat]
    | 'U' -> [F.WChar 'U']
    | _ -> raise F.Corruption

  let d fm f =
    match F.read_char fm f with
    | '\000' ->
      let di = di fm f in
      F.WChar '\000' :: di
    | '\001' ->
      let p = p fm f in
      F.WChar '\001' :: p
    | '\003' -> [F.WChar '\003']
    | '\004' ->
      let r = F.read_int fm f in
      let p = p fm f in
        (* HERE : add char 0 *)
      F.WChar '\004' :: F.WChar '\000' :: F.WInt r ::  p
    | _ -> raise F.Corruption


  let q fm f =
    match F.read_char fm f with
    | '\000' ->
      let data = d fm f in
      F.WChar '\000' :: data
    | '\001' ->
      let key = k fm f in
      F.WChar '\001' :: key
    | _ -> raise F.Corruption

  let qm fm f =
    let rec aux () =
      let l = F.read_int fm f in
      let qm_map =
        List.init l
          (fun _ ->
            let key = (k fm f) in
            let l1 = F.read_int fm f in
            let q  = init_concat l1 (fun _ -> q fm f) in
            let query = F.WInt l1 :: q in
            let querymap = (aux ()) in
            [key; query; querymap])
      in
      F.WInt l :: concat (rev_concat qm_map) in
    aux ()

  let single_node fm f =
    let node =
      match F.read_char fm f  with
      | '\000' ->
        let _ = F.read_char fm f in (* I.c0 *)
        let nodemax = k fm f in
        let _ = F.read_char fm f in (* I.c1 *)
        let nodemin = k fm f in
        let _ = F.read_char fm f in (* I.c2 *)
        let rev = [F.WInt (F.read_int fm f)] in
        let _ = F.read_char fm f in (* I.c3 *)
        let prev =
          match F.read_char fm f with
          | '\000' -> F.WChar '\000' :: F.WInt (F.read_int fm f) :: []
          | '\001' -> [F.WChar '\001']
          | _ -> raise F.Corruption in
        let _ = F.read_char fm f in (* I.c4 *)
        let data = d fm f in
        let _ = F.read_char fm f in (* I.c5 *)
        let l5 = F.read_int fm f in
        let lst = init_rev_concat l5 (fun _ -> let k = k fm f in let e = F.read_int fm f in [k @ [F.WInt e]]) in
        let nodemap = F.WInt l5 :: (concat lst) in
        let _ = F.read_char fm f in (* I.c6 *)
        let l6 = F.read_int fm f in
        let lst = init_rev_concat l6 (fun _ -> let r = F.read_int fm f in let u = F.read_int fm f in [F.WInt r; F.WInt u]) in
        let oldrev = F.WInt l6 :: lst in

        let final =
          [ [F.WChar '\000'];
            F.WChar '\000' :: nodemax;
            F.WChar '\001' :: nodemin;
            F.WChar '\002' :: rev;
            F.WChar '\003' :: prev;
            F.WChar '\004' :: data;
            F.WChar '\005' :: nodemap;
            F.WChar '\006' :: oldrev;
          ] in
        final

      | '\001' ->
        let _ = F.read_char fm f in (* I.c0 *)
        let uid = F.read_int fm f in
        let _ = F.read_char fm f in (* I.c1 *)
        let rev = F.read_int fm f in
        let _ = F.read_char fm f in (* I.c2 *)
        let prev =
          match F.read_char fm f with
          | '\000' -> F.WChar '\000' :: (d fm f)
          | '\001' -> [F.WChar '\001']
          | _ -> raise F.Corruption in
        let _ = F.read_char fm f in (* I.c3 *)
        let l3 = F.read_int fm f in
        let lst = init_concat l3 (fun _ -> let k = k fm f in let e = F.read_int fm f in k @ [F.WInt e]) in
        let nodemap = F.WInt l3 :: lst in
        let _ = F.read_char fm f in (* I.c4 *)
        let prof = F.read_int fm f in

        let final =
          [ [F.WChar '\001';
             F.WChar '\000'; F.WInt uid;
             F.WChar '\001'; F.WInt rev];
            F.WChar '\002' :: prev;
            F.WChar '\003' :: nodemap;
            [F.WChar '\004'; F.WInt prof];
          ] in
        final
      | _ -> raise F.Corruption
    in
    concat node

  (* TRANSACTIONS *)

  let single_trans fm f =
    let _ = F.read_char fm f in (* I.c0 *)
    let querymap = qm fm f in
    let _ = F.read_char fm f in (* I.c1 *)
    let read_only = F.read_char fm f in
    let _ = F.read_char fm f in (* I.c2 *)
    let l2 = F.read_int fm f in
    let paths = init_concat l2 (fun _ -> p fm f) in
    let removelist = F.WInt l2 :: paths in

    let final =
      [ F.WChar '\000' :: querymap;
        [F.WChar '\001'; F.WChar read_only];
        F.WChar '\002' :: removelist;
      ] in

    concat final

  (* dbstate *)
  let char_int_char_int = [F.RChar; F.RInt; F.RChar; F.RInt ]
end


let migrate_to_version_25 v_25 fm =
  let init = Migration.initialize v_25 fm [ F.Node; F.Uid; F.Trans; F.Flags; F.Db_state ] in

  let nodes_name = F.get_name fm F.Node ^ ".migrtmp" in
  let transitional_node = F.create_unik nodes_name in

  (* NODES *)
  let read_flags = List.init 5 (fun _ -> F.RInt) in
  let length_flags = F.length fm F.Flags in
  (* double protection *)
  let length_uid = F.length fm F.Uid in

  let rec boucle_noeuds flags uid =
    if length_uid <= (F.position_in fm F.Uid) then failwith "finished";

    let uidflpos =
      match flags with
      | None ->
        (if F.position_in fm F.Flags >= length_flags then failwith "finished";
         match F.read fm F.Flags read_flags with
         | [F.WInt 0; F.WInt 0; F.WInt 0; F.WInt 0 ; F.WInt 0] -> None
         | [F.WInt _; F.WInt u; F.WInt _; F.WInt _ ; F.WInt _] -> Some u
         | _ -> assert false)
      | Some u -> Some u
    in

    let rec readnodepos () =
      match F.read_int fm F.Uid with
      | -1 -> boucle_noeuds flags (succ uid)
      | x -> x in

    let nodepos = readnodepos () in
    let uidpos = uid * 4 in

    F.seek_in fm F.Node nodepos;
    F.seek_out fm F.Uid uidpos;
    F.add_int fm F.Uid (F.position_out_unik transitional_node);

    let node = MigrationUtils_25.single_node fm F.Node in
    F.add_unik transitional_node node;

    let nodefinalpos = F.position_out_unik transitional_node in
    let uidfinalpos = F.position_in fm F.Uid in

    match uidflpos with
    | Some uidflpos ->
      if uidfinalpos = uidflpos then
        (let pos = F.position_in fm F.Flags in
         let pos = pos - (3*4) in
         F.seek_out fm F.Flags pos;
         F.add_int fm F.Flags nodefinalpos;
         boucle_noeuds None (succ uid))
      else
        (boucle_noeuds (Some(uidflpos)) (succ uid))
    | None -> boucle_noeuds None (succ uid)
  in

  (try ignore (boucle_noeuds None 0)
   with Failure "finished" ->
     (* replace with the good one *)
     (try F.mv fm transitional_node F.Node;
      with Failure "No mv" ->
        I.error "Can't save the new node file. It may corrupt the db"));

  (* END NODES *)


  let last_snapshot =
    F.seek_in fm F.Config 4;
    F.read_int fm F.Config in
  let seek = succ last_snapshot * 20 in
  F.seek_in fm F.Flags seek;
  F.seek_out fm F.Flags seek;

  let trans_name = F.get_name fm F.Trans ^ ".migrtmp" in
  let transitional_trans = F.create_unik trans_name in

  (* double check *)
  let length_trans = F.length fm F.Trans in

  let rec boucle_trans last_flag_pos =
    if F.position_in fm F.Trans >= length_trans then failwith "finished";

    let trsflpos =
      let posflags = F.position_in fm F.Flags in
      if posflags >= length_flags then failwith "finished";
      assert (if posflags <> last_flag_pos then (I.error "Flags: position_in %d & last_pos %d" posflags last_flag_pos; false) else true);
      match F.read fm F.Flags read_flags with
      | [F.WInt 0; F.WInt 0; F.WInt 0; F.WInt 0 ; F.WInt 0] -> None
      | [F.WInt _; F.WInt _; F.WInt _; F.WInt _ ; F.WInt trs] -> Some trs
      | _ -> assert false
    in

    let last_flag_pos = last_flag_pos + 20 in

    let transaction = MigrationUtils_25.single_trans fm F.Trans in
    F.add_unik transitional_trans transaction;

    let trspos = F.position_out_unik transitional_trans  in
    let overwrite = Option.default_map true (fun pos -> pos <> trspos) trsflpos in
    if overwrite then
      (F.seek_out fm F.Flags (last_flag_pos - 4);
       F.add_int fm F.Flags trspos);

    boucle_trans last_flag_pos
  in

  (if length_trans <> 0 then
      try ignore (boucle_trans seek)
      with Failure "finished" ->
      (* replace with the good one *)
        (try F.mv fm transitional_trans F.Trans;
         with Failure "No mv" ->
           I.error "Can't save the new trans file. It may corrupt the db"));


  (* DB STATE *)

  let dbstate_name = F.get_name fm F.Db_state ^ ".migrtmp" in
  F.copy_file fm F.Db_state ".migrtmp";

  (* read uidmap *)
  let _ = F.read_char fm F.Db_state in
  let size = F.read_int fm F.Db_state in
  for _i = 0 to pred size do
    let rev_size =
      match F.read fm F.Db_state MigrationUtils_25.char_int_char_int with
      | [ c00; F.WInt _; c11; F.WInt rev_size ] when c00 = I.c0 && c11 = I.c1 ->
        rev_size
      | _ -> raise F.Corruption
    in
    for _i = 0 to pred rev_size do
      match F.read fm F.Db_state MigrationUtils_25.char_int_char_int with
      | [ c00 ; F.WInt _ ; c11 ; F.WInt _ ] when c00 = I.c0 && c11 = I.c1 -> ()
      | _ -> raise F.Corruption
    done ;
  done ;

  let transitional_dbstate = F.create_unik ~mode:F.Append dbstate_name in

  let index_beginning = F.position_in fm F.Db_state in
  F.seek_out_unik transitional_dbstate index_beginning;

  let _ = F.read_char fm F.Db_state in
  let size = F.read_int fm F.Db_state in
  F.add_unik transitional_dbstate [ I.c0; F.WInt size ];
  let db_chan = F.Channel.get fm F.Db_state in
  for index = 0 to pred size do
    let c00 = F.Channel.read_char db_chan in
    let word = F.Channel.read_string db_chan in
    let c11 = F.Channel.read_char db_chan in
    let pfl_size = F.Channel.read_int db_chan in
    let c22 = F.Channel.read_char db_chan in
    if not (c00 = I.cc0 && c11 = I.cc1 && c22 = I.cc2) then raise F.Corruption;
    let pfl = List.side_effect_init pfl_size (
      fun _ ->
        let _ = F.read_char fm F.Db_state in
        let path = MigrationUtils_25.p fm F.Db_state in
        let _ = F.read_char fm F.Db_state in
        let fl = F.read_float fm F.Db_state in
        I.c0 :: path @ [ I.c1; F.WFloat fl ]
    ) in
    let full = [ I.c0; F.WString word; I.c1; F.WInt pfl_size; I.c2;] @ (List.tail_concat pfl) in
    F.add_unik transitional_dbstate full;
  done;

  (* replace with the good one *)
  (try F.mv fm transitional_dbstate F.Db_state;
   with Failure "No mv" -> I.error "Can't save the new dbstate file. It may corrupt the db");


  Migration.finalize fm init
