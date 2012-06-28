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

(* Simple image of the DB tree in memory without data *)

#<Debugvar:DEBUG_DB>

module String = Base.String
let eprintf fmt = Printf.eprintf fmt
let sprintf fmt = Printf.sprintf fmt
let fprintf fmt = Printf.fprintf fmt

type mem_tree = {
  msts : (Keys.t, mem_tree) Hashtbl.t;
  mutable mkey : Keys.t;
  mutable mdata : bool;
}

let make ?(hint=10) key = { msts = Hashtbl.create hint; mkey = key; mdata = false; }

let rec fold ff def mtree path =
  Hashtbl.fold (fun k mt def ->
                  let spath = Path.add path k in
                  fold ff def mt spath) mtree.msts (ff path mtree.mkey mtree.mdata def)

let find_mtree mtree path =
  let rec aux here mtree kl =
    match kl with
    | [] -> Some mtree
    | k::rest ->
        try
          let st = Hashtbl.find mtree.msts k in
          aux (Path.add here k) st rest
        with Not_found -> None
  in
  aux Path.root mtree (Path.to_list path)

let find_mtree_data mtree path =
  Option.map (fun mtree -> mtree.mdata) (find_mtree mtree path)

let find_mtree_sts mtree path =
  Option.map (fun mtree -> Hashtbl.fold (fun _ mt a -> mt::a) mtree.msts []) (find_mtree mtree path)

let find_mtree_sks mtree path =
  Option.map (fun mtree -> Hashtbl.fold (fun _ mt a -> mt.mkey::a) mtree.msts []) (find_mtree mtree path)

(*
let refresh_mtree mtree path mdata =
  let rec aux here mtree = function
    | [] ->
        #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : refresh_mtree: path=%s old mdata=%b new mdata=%b"
                                                   (Path.to_string path) mtree.mdata mdata#<End>;
        mtree.mdata <- mdata
    | k::rest ->
        (try
           let st = Hashtbl.find mtree.msts k in
           aux (Path.add here k) st rest
         with Not_found -> assert false)
  in
  aux Path.root mtree (Path.to_list path)
*)

let add_mtree mtree path data =
  #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : add_mtree: path=%s" (Path.to_string path)#<End>;
  let rec aux here mtree = function
    | [] -> mtree.mdata <- (data <> Datas.UnsetData)
    | k::rest ->
        (try
           let st = Hashtbl.find mtree.msts k in
           aux (Path.add here k) st rest
         with Not_found ->
           let st = make k in
           Hashtbl.add mtree.msts k st;
           aux (Path.add here k) st rest)
  in
  aux Path.root mtree (Path.to_list path)

(* Not needed, it had to be wrapped with Db_light.remove_tree...
let remove_mtree mtree path =
  #<If$minlevel 10>Logger.log ~color:`yellow "DB-LIGHT : remove_mtree: path=%s" (Path.to_string path)#<End>;
  let rec aux here mtree kl =
    match kl with
    | [] -> false
    | [k] ->
        (Logger.log ~color:`yellow "DB-LIGHT : remove_mtree: here=%s removing key %s"
                                   (Path.to_string (Path.add here k)) (Keys.to_string k);
         Hashtbl.remove mtree.msts k;
         true)
    | k::rest ->
        (try
           let here = Path.add here k in
           let st = Hashtbl.find mtree.msts k in
           let stsks = Hashtbl.fold (fun k _ ks -> k::ks) st.msts [] in
           Logger.log ~color:`yellow "DB-LIGHT : remove_mtree: here=%s stsks=[%s]"
                                     (Path.to_string here) (String.concat_map "; " Keys.to_string stsks);
           let removed = aux here st rest in
           let stsks = Hashtbl.fold (fun k _ ks -> k::ks) st.msts [] in
           Logger.log ~color:`yellow "DB-LIGHT : remove_mtree: here=%s k=%s removed=%b stsks=[%s] st.mdata=%b"
                                     (Path.to_string here) (Keys.to_string k) removed
                                     (String.concat_map "; " Keys.to_string stsks) st.mdata;
           if removed && Hashtbl.length st.msts = 0 && not st.mdata
           then (Logger.log ~color:`yellow "DB-LIGHT : remove_mtree: here=%s removing key %s"
                                           (Path.to_string here) (Keys.to_string k);
                 Hashtbl.remove mtree.msts k);
           removed
         with Not_found -> false)
  in
  aux Path.root mtree (Path.to_list path)
*)

let rec copymt mt =
  let ht = Hashtbl.create (Hashtbl.length mt.msts) in
  Hashtbl.iter (fun k mt -> Hashtbl.add ht k (copymt mt)) mt.msts;
  { msts = ht; mkey = mt.mkey; mdata = mt.mdata }

let rec comparemt mt1 mt2 =
  let cmpht ht1 ht2 =
    Hashtbl.fold
      (fun k mt1 eq ->
         eq &&
           (try
              let mt2 = Hashtbl.find ht2 k in
              comparemt mt1 mt2
            with Not_found -> false)) ht1 true
  in
  mt1.mkey = mt2.mkey &&
  mt1.mdata = mt2.mdata &&
  cmpht mt1.msts mt2.msts &&
  cmpht mt2.msts mt1.msts

let output_mt oc tree =
  let rec aux tree =
    let sts = Hashtbl.fold (fun _ st acc -> st::acc) tree.msts [] in
    fprintf oc "%s%s%s"
      (if tree.mdata then "N" else "O")
      (Encode_light.encode_key tree.mkey)
      (Encode_light.put_len 'r' 'z' 'R' 'Z' (List.length sts));
    List.iter aux sts
  in
  aux tree

let input_mt ic =
  let rec aux () =
    let has_data = match input_char ic with | 'N' -> true | 'O' -> false | _ -> assert false in
    let n = Encode_light.decode_key_ic ic in
    let len =
      match input_char ic with
      | ('r' | 'z' | 'R' | 'Z') as c -> Encode_light.get_len_ic 'r' 'z' 'R' 'Z' c ic
      | _ -> assert false
    in
    let mtree = make ~hint:len (Keys.StringKey "") in
    mtree.mdata <- has_data;
    mtree.mkey <- n;
    let rec aux2 i =
      if i >= len
      then n, mtree
      else
        let k, st = aux () in
        Hashtbl.add mtree.msts k st;
        aux2 (i+1)
    in
    aux2 0
  in
  let _, mtree = aux () in
  (*eprintf "mtree=%s\n%!" (string_of_mtree mtree);*)
  mtree

let rec string_of_mtree0 indent mtree =
  Hashtbl.fold
    (fun _k v acc -> sprintf "%s\n%s%s" acc indent (string_of_mtree0 (indent^" ") v))
    mtree.msts (sprintf "%s%s -> %s" indent (Keys.to_string mtree.mkey) (if mtree.mdata then "*" else ""))
let string_of_mtree = string_of_mtree0 ""

(*
let _K_a = Keys.StringKey "a";;
let _K_b = Keys.StringKey "b";;
let _K_c = Keys.StringKey "c";;
let _K_d = Keys.StringKey "d";;
let _K_e = Keys.StringKey "e";;
let _K_f = Keys.StringKey "f";;
let a = Path.of_list [_K_a];;
let abc = Path.of_list [_K_a; _K_b; _K_c];;
let abd = Path.of_list [_K_a; _K_b; _K_d];;
let def = Path.of_list [_K_d; _K_e; _K_f];;
let mtree = make (Keys.StringKey "");;
let mt1 = copymt mtree;;
add_mtree mtree a (Datas.Data (DataImpl.Int 1));;
let mt2 = copymt mtree;;
add_mtree mtree abc (Datas.Data (DataImpl.Int 123));;
let mt3 = copymt mtree;;
add_mtree mtree abd (Datas.Data (DataImpl.Int 124));;
let mt4 = copymt mtree;;
add_mtree mtree def (Datas.Data (DataImpl.Int 456));;
let mt5 = copymt mtree;;
let file = "mem_tree_light_test";;
let test_ot file mtree =
  let oc = open_out file in
  output_mt oc mtree;
  close_out oc;;
let test_it file =
  let ic = open_in file in
  let mtree = input_mt ic in
  close_in ic;
  mtree;;
let tstmt mt =
  test_ot file mt;
  let mt2 = test_it file in
  comparemt mt mt2;;
let allmt = [mt1;mt2;mt3;mt4;mt5];;
let good = List.for_all tstmt allmt;;
eprintf "good=%b\n%!" good;;
open Mem_tree_light;;
let file = "/home/norman/.mlstate/import_cities.exe/db_light_mtree";;
let mtree = test_it file;;
print_endline (string_of_mtree mtree);;
HttpTools.timefn 1 test_it file;;
*)
