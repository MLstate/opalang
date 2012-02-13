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

(* Migrate to version 24
 * Changes:
 *   Nodes don't store all their old revisions, just last 2
 * *)

module MigrationUtils_24 =
struct
  (* we just need to char by char, not construct the data *)
  let concat = List.tail_concat
  let init_concat l f = concat (List.side_effect_init l f)
  let rev_concat l = concat (List.rev l)
  let init_rev_concat l f = rev_concat (List.init l f)


  let rec k fm f =
    match F.read_char fm f with
    | '\000' ->
        let i = F.read_int fm f in
        [F.WChar '\000'; F.WInt i]
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
        let l =
          (match F.read_char fm f with
           | '\000' ->
               let r = F.read_int fm f in
               F.WChar '\000' :: F.WInt r :: []
           | '\001' ->
               F.WChar '\001' :: []
           | _ -> raise F.Corruption)
        in
        let p = p fm f in
        l @ p
    | _ -> raise F.Corruption

  type node =
    | Delta of F.write list
    | Full of F.write list * (int * int) list

  let single_node fm f =
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
        let lst = List.side_effect_init l6 (fun _ -> let r = F.read_int fm f in let u = F.read_int fm f in (r,u)) in
        let oldrev = lst in

        let final =
          [ [F.WChar '\000'];
            F.WChar '\000' :: nodemax;
            F.WChar '\001' :: nodemin;
            F.WChar '\002' :: rev;
            F.WChar '\003' :: prev;
            F.WChar '\004' :: data;
            F.WChar '\005' :: nodemap;
          ] in
        Full (concat final,oldrev)

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
        Delta (concat final)
    | _ -> raise F.Corruption
end

let migrate_to_version_24 v_24 fm =
  let init = Migration.initialize v_24 fm [ F.Flags; F.Node; F.Uid ] in

  let read_flags = List.init 5 (fun _ -> F.RInt) in
  let length_flags = F.length fm F.Flags in
  (* double protection *)
  let length_uid = F.length fm F.Uid in

  let rec boucle flags uid =
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
      | -1 -> boucle flags (succ uid)
      | x -> x in

    let nodepos = readnodepos () in
    let uidpos = uid * 4 in

    F.seek_in fm F.Node nodepos;
    F.seek_out fm F.Uid uidpos;
    F.add_int fm F.Uid (F.position_out fm F.Node);

    (match MigrationUtils_24.single_node fm F.Node with
     | MigrationUtils_24.Delta lst ->
         F.add fm F.Node lst
     | MigrationUtils_24.Full (lst,oldrev) ->
         let oldrev =
           match oldrev with
           | [] -> []
           | (r,u) :: [] ->
               [ F.WInt r; F.WInt u ]
           | (r1,u1) :: (r2,u2) :: _ ->
               List.map (fun x -> F.WInt x) [r1; u1; r2; u2]
         in
         let oldrev = F.WChar '\006' :: F.WInt (List.length oldrev / 2) :: oldrev in

         F.add fm F.Node lst;
         F.add fm F.Node oldrev);

    let nodefinalpos = F.position_out fm  F.Node in
    let uidfinalpos = F.position_in fm F.Uid in

    match uidflpos with
    | Some uidflpos ->
        if uidfinalpos = uidflpos then
          (let pos = F.position_in fm F.Flags in
           let pos = pos - (3*4) in
           F.seek_out fm F.Flags pos;
           F.add_int fm F.Flags nodefinalpos;
           boucle None (succ uid))
        else
          (boucle (Some(uidflpos)) (succ uid))
    | None -> boucle None (succ uid)
  in

  (try ignore (boucle None 0)
   with Failure "finished" -> ());

  (* truncate the file *)
  let new_length = F.position_out fm F.Node in
  F.set_size fm F.Node new_length;

  Migration.finalize fm init
