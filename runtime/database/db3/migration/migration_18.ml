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

(* Migration to version 18
 * Changes :
 *   replace Copy revision argument with option
 *)
let migrate_to_version_18 v_18 fm =

  let init = Migration.initialize v_18 fm [ F.Node; F.Uid; F.Trans; F.Config; F.Flags ] in

  let concat = List.tail_concat in
  let init_concat l f = concat (List.init l f) in
  let rev_concat l = concat (List.rev l) in
  let init_rev_concat l f = rev_concat (List.init l f) in


  (* Utils *)
  let rec k f =
    match F.read_char fm f with
    | '\000' ->
        let i = F.read_int fm f in
        [F.WChar '\000'; F.WInt i]
    | '\001' ->
        let s = F.read_string fm f in
        [F.WChar '\001'; F.WString s]
    | '\002' ->
        let l = F.read_int fm f in
        let keys = init_concat l (fun _ -> k f) in
        F.WChar '\003' :: F.WInt l :: keys
    | '\003' ->
        [F.WChar '\003']
    | _ -> raise F.Corruption
  in


  let p f =
    let l = F.read_int fm f in
    let lst = init_rev_concat l (fun _ -> k f) in
    F.WInt l :: lst in

  let di f =
    match F.read_char fm f with
    | 'J' -> F.WChar 'J' :: F.read fm f [F.RInt64]
    | 'S' -> F.WChar 'S' :: F.read fm f [F.RString]
    | 'B' -> F.WChar 'B' :: F.read fm f [F.RString]
    | 'F' -> F.WChar 'F' :: F.read fm f [F.RFloat]
    | 'U' -> [F.WChar 'U']
    | _ -> raise F.Corruption
  in

  let d f =
    match F.read_char fm f with
    | '\000' ->
        let di = di f in
        F.WChar '\000' :: di
    | '\001' ->
        let p = p f in
        F.WChar '\001' :: p
    | '\003' -> [F.WChar '\003']
    | '\004' ->
        let r = F.read_int fm f in
        let p = p f in
        (* HERE : add char 0 *)
        F.WChar '\004' :: F.WChar '\000' :: F.WInt r ::  p
    | _ -> raise F.Corruption
  in


  let q f =
    match F.read_char fm f with
    | '\000' ->
        let data = d f in
        F.WChar '\000' :: data
    | '\001' ->
        let key = k f in
        F.WChar '\001' :: key
    | _ -> raise F.Corruption
  in

  let qm f =
    let rec aux () =
      let l = F.read_int fm f in
      let qm_map =
        List.init l
          (fun _ ->
             let key = (k f) in
             let l1 = F.read_int fm f in
             let q  = init_concat l1 (fun _ -> q f) in
             let query = F.WInt l1 :: q in
             let querymap = (aux ()) in
             [key; query; querymap])
      in
      F.WInt l :: concat (rev_concat qm_map) in
    aux () in

  let single_node f =
    let node =
      match F.read_char fm f  with
      | '\000' ->
          let _ = F.read_char fm f in (* I.c0 *)
          let nodemax = k f in
          let _ = F.read_char fm f in (* I.c1 *)
          let nodemin = k f in
          let _ = F.read_char fm f in (* I.c2 *)
          let rev = [F.WInt (F.read_int fm f)] in
          let _ = F.read_char fm f in (* I.c3 *)
          let prev =
            match F.read_char fm f with
            | '\000' -> F.WChar '\000' :: F.WInt (F.read_int fm f) :: []
            | '\001' -> [F.WChar '\001']
            | _ -> raise F.Corruption in
          let _ = F.read_char fm f in (* I.c4 *)
          let data = d f in
          let _ = F.read_char fm f in (* I.c5 *)
          let l5 = F.read_int fm f in
          let lst = init_rev_concat l5 (fun _ -> let k = k f in let e = F.read_int fm f in [k @ [F.WInt e]]) in
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
            | '\000' -> F.WChar '\000' :: (d f)
            | '\001' -> [F.WChar '\001']
            | _ -> raise F.Corruption in
          let _ = F.read_char fm f in (* I.c3 *)
          let l3 = F.read_int fm f in
          let lst = init_concat l3 (fun _ -> let k = k f in let e = F.read_int fm f in k @ [F.WInt e]) in
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
    concat node in

  (* NODE & UID *)
  let nd = F.Node and ui = F.Uid in
  let length_uid = F.length fm ui in
  let get_uids =
    List.init (length_uid / 4)
      (fun _ -> F.read_int fm ui ) in


  let rec aux_node nodes uids old olduids =
    match olduids with
    | [] -> nodes, uids
    | -1 :: y -> aux_node nodes uids old y
    | _ :: y ->
        let n = single_node nd in
        let taille = old + Init.get_length_write n in
        aux_node (n :: nodes) (old :: uids) taille y
  in
  let new_nodes, new_uids = aux_node [] [] 0 get_uids in
  let new_nodes = rev_concat new_nodes in
  let new_uids = List.rev_map (fun x -> F.WInt x) new_uids in


  F.seek_out fm ui 0;
  F.add fm ui new_uids;
  F.seek_out fm nd 0;
  F.add fm nd new_nodes;
  F.seek_in fm ui 0;
  F.seek_in fm nd 0;

  let new_node_pos = F.position_out fm nd in



  (* TRANSACTIONS *)

  let tr = F.Trans in

  let single_trans f =
    let _ = F.read_char fm f in (* I.c0 *)
    let querymap = qm f in
    let _ = F.read_char fm f in (* I.c1 *)
    let read_only = F.read_char fm f in
    let _ = F.read_char fm f in (* I.c2 *)
    let l2 = F.read_int fm f in
    let paths = init_concat l2 (fun _ -> p f) in
    let removelist = F.WInt l2 :: paths in

    let final =
      [ F.WChar '\000' :: querymap;
        [F.WChar '\001'; F.WChar read_only];
        F.WChar '\002' :: removelist;
      ] in

    concat final in

  let length_trans = F.length fm tr in

  let rec aux_trans transs =
    if F.position_in fm tr < length_trans - 1 then
      let tr = single_trans tr in
      aux_trans (tr :: transs)
    else
      transs in

  let new_transs = aux_trans [] in
  let new_transs = rev_concat new_transs in

  F.seek_out fm tr 0;
  F.add fm tr new_transs;
  F.seek_in fm tr 0;

  let new_trans_pos = F.position_out fm tr in


  (* FLAGS *)


  let fl = F.Flags in

  (* FIXME check uid? *)
  F.seek_out fm fl 8;
  F.add_int fm fl new_node_pos;
  F.seek_out fm fl 16;
  F.add_int fm fl new_trans_pos;

  F.seek_out fm fl 0;

  (* Finalization *)
  Migration.finalize fm init
