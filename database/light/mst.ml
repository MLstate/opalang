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

(* Imports *)

module String = BaseString
module KS = KeySet

(* Support functions *)

let list_of_keyset ks = KS.fold (fun k l -> k::l) ks []
let keyset_of_list l = List.fold_right KS.add l KS.empty
let string_of_keyset ks = String.concat_map ~left:"[" ~right:"]" ~nil:"[]" ";" Keys.to_string (list_of_keyset ks)
let eprintf fmt = Printf.eprintf fmt

(*
(* Debug code *)
let k n = Keys.IntKey n;;
let s v = Keys.StringKey v;;
let p l = Path.of_list (List.map k l);;
let kts = Keys.to_string;;
let lkts ks = String.concat_map ~left:"[" ~right:"]" "; " kts ks;;
let pts = Path.to_string;;
let lpts = List.map pts;;
let spts = String.concat_map ~left:"[" ~right:"]" "; " pts;;
let ks l = keyset_of_list (List.map k l);;
*)

(* Types *)

type status = Cached | Updated | Removed

type t = {
  dbm : Dbm.t;
  ht : (string, (status * KS.t)) Hashtbl.t;
}

(* Basic operations *)

let replace t (p:Path.t) ks =
  Hashtbl.replace t.ht (Marshal.to_string p []) (Updated, ks)

let find t (p:Path.t) =
  let k = Marshal.to_string p [] in
  let removed = ref false in
  try
    (match Hashtbl.find t.ht k with
     | (Removed, _) -> removed := true; raise Not_found
     | (_, ks) -> ks)
  with Not_found ->
    if !removed then raise Not_found;
    let ks = Marshal.from_string (Dbm.find t.dbm k) 0 in
    Hashtbl.replace t.ht k (Cached, ks);
    ks

let create ?(create=false) ?(hint=10000) file =
  let t = { dbm = Dbm.opendbm file ((if create then [Dbm.Dbm_create] else [])@[Dbm.Dbm_rdwr]) 0O664;
            ht = Hashtbl.create hint; }
  in
  try ignore (find t Path.root); t
  with Not_found -> replace t Path.root KS.empty; t

let set_path t (p:Path.t) =
  let rec aux pks here = function
    | [] -> ()
    | k::rest ->
        let sp = Path.add here k in
        if not (KS.mem k pks) then replace t here (KS.add k pks);
        (try
           aux (find t sp) sp rest
         with Not_found ->
           replace t sp KS.empty;
           aux KS.empty sp rest)
  in
  aux (try find t Path.root with Not_found -> KS.empty) Path.root (Path.to_list p)

let find_all t (p:Path.t) =
  let rec aux l = function
  | [] -> l
  | p::ps ->
      try
        let sps = List.map (Path.add p) (list_of_keyset (find t p)) in
        (*eprintf "p: %s  sps: %s\n%!" (pts p) (spts sps);*)
        aux (p::l) (ps@sps)
      with Not_found ->
        aux (p::l) ps
  in
  aux [] [p]

let remove ?(cb=((fun _ _ -> ()):Path.t -> string -> unit)) t (p:Path.t)  =
  let rec aux here = function
    | [] ->
        (*eprintf "p=%s here=%s\n%!" (pts p) (pts here);*)
        assert(p = here);
        let allsp = find_all t p in
        List.iter (fun p ->
                     (*eprintf "remove %s\n%!" (pts p);*)
                     let k = Marshal.to_string p [] in
                     Hashtbl.replace t.ht k (Removed, KS.empty);
                     cb p k)
          allsp;
        allsp <> []
    | k::ks ->
        (try
           if aux (Path.add here k) ks
           then (let sks = KS.remove k (find t here) in
                 let emp = KS.is_empty sks && here <> Path.root in
                 (*eprintf "update %s\n%!" (pts here);*)
                 let k = Marshal.to_string here [] in
                 Hashtbl.replace t.ht k ((if emp then Removed else Updated), sks);
                 if emp then cb here k;
                 emp)
           else false
         with Not_found -> false)
  in
  ignore (aux Path.root (Path.to_list p))

let write t =
  (*eprintf "write\n%!";*)
  List.iter (function
             | (Cached,_,_) -> ()
             | (Updated,k,ks) -> Hashtbl.replace t.ht k (Cached,ks)
             | (Removed,k,_) -> Hashtbl.remove t.ht k)
    (Hashtbl.fold
       (fun k v acc ->
          match v with
          | (Cached, _) -> acc
          | (Updated, ks) -> Dbm.replace t.dbm k (Marshal.to_string ks []); ((Updated,k,ks)::acc)
          | (Removed, _) ->
              (*eprintf "dbm remove: %s\n%!" (pts ((Marshal.from_string k 0):Path.t));*)
              (try Dbm.remove t.dbm k with Dbm.Dbm_error "dbm_delete" -> ()); ((Removed,k,KS.empty)::acc))
       t.ht [])

let close t =
  write t;
  Dbm.close t.dbm;
  Hashtbl.clear t.ht

(* Test code *)

(*
(*let file = "/home/norman/.mlstate/fr1.exe/db_light";;
let file = "/home/norman/.mlstate/bigtrans.native/db_light";;*)
let file = "/tmp/mst";;
let pfr = Path.of_list [k 1; k 0; k 0; s "0"];;
let domst f =
  let mst = create ~create:true file in
  try let r = f mst in close mst; r
  with Not_found -> close mst; raise Not_found;;
let getp p = list_of_keyset (domst (fun mst -> find mst p));;
let get_ f l = domst (fun mst -> f mst (p l));;
let get l = list_of_keyset (get_ find l);;
let rmv = get_ remove;;
let get_all l = lpts (get_ find_all l);;
let cb = fun p _ -> eprintf "cb: p=%s\n%!" (pts p);;
let mst = create ~create:true file;;
let () = replace mst (p [1]) (ks [0; 1]);;
let () = write mst;;
let kl1 = (try find mst (p [1]) with Not_found -> KS.empty) = (ks [0; 1]);;
let () = replace mst (p [1]) (ks [0; 1; 2]);;
let () = write mst;;
let kl2 = (try find mst (p [1]) with Not_found -> KS.empty) = ks [0; 1; 2];;
let () = replace mst (p [1]) (ks [0; 1; 2; 3]);;
let () = replace mst (p [1; 2]) (ks [0]);;
let () = replace mst (p [1; 3]) (ks [0]);;
let kl3 = lpts (try find_all mst (p [1]) with Not_found -> []) =
  ["[1; 2; 0]"; "[1; 3; 0]"; "[1; 0]"; "[1; 1]"; "[1; 2]"; "[1; 3]"; "[1]"];;
let () = remove mst (p [1]);;
let kl4 = (try find mst (p [1]) with Not_found -> KS.empty) = KS.empty;;
let kl5 = (try find mst (p [1; 0]) with Not_found -> KS.empty) = KS.empty;;
let kl6 = (try find mst (p [1; 1]) with Not_found -> KS.empty) = KS.empty;;
let kl7 = (try find mst (p [1; 2]) with Not_found -> KS.empty) = KS.empty;;
let kl8 = (try find mst (p [1; 3]) with Not_found -> KS.empty) = KS.empty;;
let kl9 = (try find mst (p [1; 2; 0]) with Not_found -> KS.empty) = KS.empty;;
let kl10 = (try find mst (p [1; 3; 0]) with Not_found -> KS.empty) = KS.empty;;
let () = write mst;;
let kl11 = (try find mst (p [1]) with Not_found -> KS.empty) = KS.empty;;
let () = set_path mst (p [1;2;3]);;
let kl12 = lpts (try find_all mst (p [1]) with Not_found -> []) = ["[1; 2; 3]"; "[1; 2]"; "[1]"];;
(* *)
let tst mst n =
  for i = 0 to n - 1 do
    (*replace mst (p [1; i]) (ks [0; 1; 2])*)
    set_path mst (p [1; 2; 3; i])
  done;
  eprintf "size=%d\n%!" (Hashtbl.length mst.ht)
;;
let tst2 mst n =
  for i = 0 to n - 1 do
    ignore (find mst (p [1; 2; 3; i]))
  done;;
let tst3 mst n =
  for i = 0 to n - 1 do
    remove mst (p [1; 2; 3; i])
  done;;
let n = 400000;;
let () = HttpTools.timefn 1 (tst mst) n;;
let () = HttpTools.timefn 1 (tst2 mst) n;;
let () = HttpTools.timefn 1 write mst;;
let () = close mst;;
let mst = create file;;
let () = HttpTools.timefn 1 (tst2 mst) n;;
let () = HttpTools.timefn 1 (tst2 mst) n;;
let () = HttpTools.timefn 1 (tst3 mst) n;;
(*let () = HttpTools.timefn 1 (remove ~cb mst (p [1]));;*)
let () = HttpTools.timefn 1 write mst;;
(* *)
let () = close mst;;
*)
