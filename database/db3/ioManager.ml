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
(* depends *)
module List = BaseList

(* shorthands *)
module D = Datas
module DI = DataImpl
module DT = DbTypes
module K = Keys
module N = Node
module Q = QueryMap

(* debug *)
#<Debugvar:DEBUG_DB$flag "io">

(* -- *)

module F = DbIo

type t = F.t

(* Utils *)

let cc0 = '\000'
let cc1 = '\001'
let cc2 = '\002'
let cc3 = '\003'
let cc4 = '\004'
let cc5 = '\005'
let cc6 = '\006'

let c0 = F.WChar cc0
let c1 = F.WChar cc1
let c2 = F.WChar cc2
let c3 = F.WChar cc3
let c4 = F.WChar cc4
let c5 = F.WChar cc5
let c6 = F.WChar cc6


(* -- Logger File -- *)
let logger =
  #<If:LOW_LEVEL_DB_LOG>
  let iomanager_file = Logger.make_file_destination "low_level_db.log" in
  Some (Logger.make_logger [iomanager_file] 6)
  #<Else> None #<End>

let logf fmt =
  #<If:LOW_LEVEL_DB_LOG>
    Logger.log ~logger:(Option.get logger) ~color:`blue fmt
  #<Else> Format.ifprintf Format.std_formatter fmt #<End>

let logfm fm fmt =
  let location = F.get_location fm in
  logf ("[%s] "^^fmt) location

(* -- Normal log -- *)
let debug fmt =
  #<If> Printf.fprintf stdout ("[35m[IoManager][0m"^^fmt^^"\n%!")
  #<Else> Printf.ifprintf stderr ("[35m[IoManager][0m"^^fmt^^"\n%!")
  #<End>

let print fmt = Logger.info ("[IoManager] "^^fmt)

let error fmt = Logger.error ("[IoManager] "^^fmt)


let concat_map f l = List.concat (List.map f l)

let get_length_write w =
  let rec aux acc = function
    | x::y ->
        let size =
          match x with
          | F.WChar _ -> 1
          | F.WInt _ -> 4
          | F.WString s -> 4 + String.length s
          | F.WFloat _ -> 8
          | F.WInt32 _ -> 4
          | F.WInt64 _ -> 8
        in
        aux (size+acc) y
    | [] -> acc
  in aux 0 w

let get_pos_from_flags fm file =
  let position = F.length fm F.Flags in
  if position = 0 then
    (logfm fm "Read Flags, position 0, don't read"; 0)
  else
    (assert (position mod 20 = 0);
     let pos =
       match file with
       | F.Uid_rev -> 0
       | F.Uid -> 1
       | F.Node -> 2
       | F.Timestamp -> 3
       | F.Trans -> 4
       | _ -> assert false
     in
     let real_pos = position - ((5-pos) * 4) in
     let rem = F.position_in fm F.Flags in
     F.seek_in fm F.Flags real_pos;
     let respos = F.read_int fm F.Flags in
     logfm fm "Read fags at %d, position %d (pos %d)" real_pos position pos;
     F.seek_in fm F.Flags rem;
     respos)

let seek_file fm file =
  let pos = get_pos_from_flags fm file in
  F.seek_out fm file pos

let iscorrupted un deux =
  if un != deux then
    (#<If> error "read %d(%x) instead of %d"
       (Char.code un) (Char.code un) (Char.code deux) #<End>;
     raise F.Corruption)


let int64_toint i =
  if i > Int64.of_int max_int then (
    prerr_endline "ERROR: overflow in int from the database, it is too large for your architecture";
    raise F.Corruption
  );
  Int64.to_int i

(* Utils (end) *)

(* FileIo Things *)
let create mode file =
  let strmode,mode =
    match mode with
    | `create -> "create",F.Create
    | `append -> "append",F.Append
    | `readonly -> "readonly",F.ReadOnly
  in
  let fm = F.make mode file in
  logf "-------------";
  logf "Create new instance : %s (mode: %s)" file strmode;
  logf "-------------";
  (* check integrity of flag file, must be well truncated *)
  (let length = F.length fm F.Flags in
  let fst_check length = length mod 20 = 0  in
  let snd_check length =
    let rec aux pos =
      let pos_tms = pos - 20 in
      F.seek_in fm F.Flags pos_tms;
      if F.read_int fm F.Flags = 0 then
        aux (pos -20)
      else pos in
    aux length in

  let set_size new_length =
    if new_length = 0 then
      (print "Flags file is corrupted from the beginnig";
      raise F.Corruption);
    F.set_size fm F.Flags new_length
  in

  if length <> 0 then
    (if fst_check length then
      let new_length = snd_check length in
      if new_length = length then
        (* all is ok, nothing to do *)
        ()
      else
        set_size new_length
    else
      let new_length = length / 20 * 20 in
      let new_length = snd_check new_length in
      set_size new_length;

    F.seek_in fm F.Flags 0;
    F.seek_out fm F.Flags 0));
  (* finished to torture flags' file *)

  fm


let close fm =
  logfm fm "-------------";
  logfm fm "Close";
  logfm fm "-------------";
  F.close fm

let get_location fm = F.get_location fm

let is_uidfile_existing file =
  let test_file = Printf.sprintf "%s%s" file "_uid_file" in
  Sys.file_exists test_file

let is_open = F.is_open

(* +++ DOT +++ *)
let output_dot fm rev content =
  let file = F.get_location fm in
  let _dir = Filename.dirname file in
  let name = Filename.basename file in
  let rep = Filename.concat file "dot" in
  let _ = if Sys.file_exists rep then ()
  else if not (File.check_create_path ~rights:0o750 rep) then
    failwith "output_dot" in
  let file_name =
    Printf.sprintf "%s/%s_%04d.dot" rep name rev in
  logfm fm "Dot: Output dot for rev %d at %s" rev file_name;
  debug "Output dot for rev %d at %s" rev file_name;
  let oc = open_out file_name in
  (* Unix.chmod file_name 0o640; *)
  output_string oc content;
  flush oc;
  close_out oc

let rebirth fm =
  let lst = [ F.Db_state; F.Node; F.Uid; F.Trans; F.Uid_rev ] in
  List.iter (fun f -> F.seek_in fm f 0; F.seek_out fm f 0) lst


(* +++ LOCK +++ *)
let lock_file fm =
  let name = F.get_name fm F.Lock in
  let lockfile = Unix.openfile name [Unix.O_CREAT; Unix.O_WRONLY] File.default_rights in
  try Unix.lockf lockfile Unix.F_TLOCK 0; true
  with Unix.Unix_error (Unix.EAGAIN, "lockf", _) -> false


(* Values *)
let eid2fm eid = F.WInt (Eid.value eid)
let uid2fm uid = F.WInt (Uid.value uid)
let rev2fm rev = F.WInt (Revision.value rev)

let fm2eid fm f = Eid.make (F.read_int fm f)
let fm2uid fm f = Uid.make (F.read_int fm f)
let fm2rev fm f = Revision.make (F.read_int fm f)


let rec key2fm = function
  | K.IntKey i -> [c0; F.WInt64 (Int64.of_int i)]
  | K.StringKey s -> [c1; F.WString s]
  | K.ListKey (l) ->
      let ll =
        let fold acc ik =
          List.rev_append (key2fm ik) acc
        in
        Array.fold_left fold [] l
      in
      c2 :: F.WInt (Array.length l) :: (List.rev ll)
  | K.VariableKey _ -> [c3]


let rec fm2key fm f =
  let shb = F.read_char fm f in
  match shb with
  | c when c = cc0 -> K.IntKey (int64_toint (F.read_int64 fm f))
  | c when c = cc1 -> K.StringKey (F.read_string fm f)
  | c when c = cc2 ->
      let size = F.read_int fm f in
      let arr = Array.make size (K.IntKey 0) in
      for i = 0 to pred size do
        Array.unsafe_set arr i (fm2key fm f)
      done ;
      K.ListKey arr
  | c when c = cc3 -> K.VariableKey 0
  | _ -> raise F.Corruption

(*
  Paths take too much space in memory, esp. in the search index.
  This hash/cons should help minimise the footprint
  Used also old revision field of Nodes
*)

module MakeHashCons
 (E: sig
   type elem
   val elem0 : elem

   module HashCons :
   sig
     type ht
     val create : unit -> ht
     val clear : ht -> unit
     val find : ht -> elem -> ht * elem list
     val add : ht -> elem -> ht * elem list -> unit
   end
 end) =
struct

  module Buf :
  sig
    (**
       [get min_size]
       Return an array of keus, with a minimum size of [min_size].
       <!> The size of the returned array may be bigger than [min_size ].
       <!> The content of the array is totally unspecified.
    *)
    val get : int -> E.elem array
  end =
  struct
    let growing_size = ref 10
    let keys_buffer = ref (Array.make !growing_size E.elem0)
    let get len =
      if len > !growing_size
      then (
        growing_size := max len (!growing_size * 2) ;
        let fresh = Array.make !growing_size E.elem0 in
        keys_buffer := fresh ;
      )
      ;
      !keys_buffer
  end

  module HCons = E.HashCons
  let get_buffer = Buf.get

  let dummy_hkt = HCons.create ()

  (*
    keys are stored in reverse order on the disk.
    hash/cons works from beginning of path.
    The array correspond to the bufferisation of the path read from the disk,
    so the array is in reverse order. We should iterate from its end, to
    its begin. <!> beware, the size of the array may be biger, and should
    not be taken into consideration.
  *)
  let read_aux (hkt : HCons.ht) keys_buffer size =
    let rec aux hkt path index =
      if index < 0
      then path
      else
        let key = Array.unsafe_get keys_buffer index in
        let (hkt, path) =
          try let r = HCons.find hkt key in r with
          | Not_found ->
              (let acc = HCons.create (), key :: path in
              HCons.add hkt key acc ;
              acc)
        in
        aux hkt path (pred index)
    in
    aux hkt [] (pred size)

  let hash_cons_table = HCons.create ()
  let read keys_buffer size =
    read_aux hash_cons_table keys_buffer size

  let cleanup () = HCons.clear hash_cons_table
end

module PATH = struct
  type elem = Keys.t
  let elem0 = K.IntKey 0
  module HashCons = Path.HashCons
end
module PathHashCons = MakeHashCons(PATH)

let path2fm path =
  let rev_keys = Path.write path in
  F.WInt (List.length rev_keys) :: concat_map key2fm rev_keys

let fm2path fm f =
  let plength = F.read_int fm f in
  let key_buffer = PathHashCons.get_buffer plength in
  for index = 0 to pred plength do
    Array.unsafe_set key_buffer index (fm2key fm f) ;
  done ;
  let rev_keys = PathHashCons.read key_buffer plength in
  Path.read rev_keys

let dataimpl2fm = function
  | DI.Int i ->
      let i = Int64.of_int i in
      [F.WChar 'J'; F.WInt64 i]
  | DI.Text s -> [F.WChar 'S'; F.WString s]
  | DI.Binary s -> [F.WChar 'B'; F.WString s]
  | DI.Float f -> [F.WChar 'F'; F.WFloat f]
  | DI.Unit -> [F.WChar 'U']


let fm2dataimpl fm f =
  match F.read_char fm f with
  | 'J' -> DI.Int (int64_toint (F.read_int64 fm f))
  | 'S' -> DI.Text (F.read_string fm f)
  | 'B' -> DI.Binary (F.read_string fm f)
  | 'F' -> DI.Float (F.read_float fm f)
  | 'U' -> DI.Unit
  | _ -> raise F.Corruption

let data2fm = function
  | D.Data d -> c0 :: dataimpl2fm d
  | D.Link p -> c1 :: path2fm p
  | D.UnsetData -> [c3]
  | D.Copy (r, p) ->
      c4
      :: Option.default_map [c1] (
        fun x ->
          c0 :: rev2fm x :: []
      ) r
      @ path2fm p

let fm2data fm f =
  match F.read_char fm f with
  | c when c = cc0 -> D.Data (fm2dataimpl fm f)
  | c when c = cc1 -> D.Link (fm2path fm f)
  | c when c = cc3 -> D.UnsetData
  | c when c = cc4 ->
      let rev =
        let shb = F.read_char fm f in
        if shb = cc0 then Some (fm2rev fm f)
        else (iscorrupted shb cc1;  None)
      in
      let path = fm2path fm f in
      D.Copy (rev, path)
  | _ -> raise F.Corruption

let query2fm = function
  | Q.Set d -> c0 :: data2fm d
  | Q.Remove k -> c1 :: key2fm k

let fm2query fm f =
  match F.read_char fm f with
  | c when c = cc0 -> Q.Set (fm2data fm f)
  | c when c = cc1 -> Q.Remove (fm2key fm f)
  | _ -> raise F.Corruption


let querymap2fm querymap =
  let rec aux querymap =
    F.WInt (KeyRecMap.size querymap) ::
      (KeyRecMap.fold_rev
         (fun k (ql, qm) acc ->
            let lst = key2fm k in
            let lst1 = F.WInt (List.length ql) :: (List.concat_map query2fm ql) in
            let lst2 = aux qm in
            let acc = List.tail_append lst2 acc in
            let acc = List.tail_append lst1 acc in
            let acc = List.tail_append lst acc in
            acc)
         querymap
         [])
  in
  aux querymap

let fm2querymap fm f =
  let rec aux () =
    let qsize = F.read_int fm f in
    Loop.For.range 0 qsize KeyRecMap.empty (
      fun _ querymap -> (
        let key = fm2key fm f in
        let size = F.read_int fm f in
        let ql = List.init size (fun _ -> fm2query fm f) in
        let qm = aux () in
        KeyRecMap.add key (ql, qm) querymap
      )
    )
  in
  aux ()

let write_key_map keymap =
  let length = ref 0 in
  let fold key eid rev_acc =
    incr(length);
    let key = key2fm key in
    let eid = eid2fm eid in
    eid :: List.rev_append key rev_acc
  in
  let rev_acc = KeyMap.fold fold keymap [] in
  let nodemap = List.rev rev_acc in
  !length, nodemap

let read_key_map size fm f =
  let rec aux acc i =
    if i >= size then acc
    else
      let acc =
        let key = fm2key fm f in
        let eid = fm2eid fm f in
        KeyMap.add key eid acc
      in
      aux acc (succ i)
  in
  aux KeyMap.empty 0

(*
  Mathieu Wed Mar 16 17:19:18 CET 2011
  FIXME:
  we can avoid the allocation of lots of temporary lists there,
  trying to keep the clarty of the code.
*)
let write_node node =
  match Node.write node with
  | N.Full full ->
      let lst =
        [ [c0];
          c0 :: key2fm (K.IntKey (-1)) (*full.N.max*) ;
          c1 :: key2fm (K.IntKey (-1)) (*full.N.min*) ;
          c2 :: [rev2fm full.N.cur_rev] ;
          c3 :: [c1] (* : None *)
(*            Option.default_map [c1]
            (fun x -> c0 :: rev2fm x :: [])
            full.N.pred_rev*) ;
          c4 :: data2fm full.N.content ;
        ] in
      let oldrev = [] in
      let lst1 =
        let length, nodemap = write_key_map full.N.map in
        [ c5 :: F.WInt length :: nodemap ]
      in
      let lst2 =
        match oldrev with
        | (r1,u1)::(r2,u2)::_ ->
            [c6; F.WInt 2]::[[rev2fm r1; uid2fm u1; rev2fm r2; uid2fm u2]]
        | _ -> (* catch only [] & [x], keep it general to factorize *)
            ([c6; F.WInt (List.length oldrev)] ::
                  [concat_map (fun (r,u) -> rev2fm r :: uid2fm u :: []) oldrev])
      in
      List.tail_concat (List.tail_concat [lst; lst1; lst2])

  | N.Delta (d_uid, d_rev, delta) ->
      let lst =
        c1
        :: c0 :: uid2fm d_uid
        :: c1 :: rev2fm d_rev
        :: c2 ::
          Option.default [c1]
          (Option.map (fun x -> c0 :: data2fm x) delta.N.new_content)
      in
      let lst1 =
        let length, nodemap = write_key_map delta.N.new_childs in
        c3 :: F.WInt length :: nodemap
      in
      let lst2 = [ c4; F.WInt delta.N.prof ] in
      List.tail_concat [ lst ; lst1 ; lst2 ]

  | N.RevDelta _ -> assert false


let hash_oldrevs = Hashtbl.create 11

let read_node fm =
  let f = F.Node in
  let io_node =
  match F.read_char fm f with
  | c when c = cc0 ->
      iscorrupted (F.read_char fm f) cc0;
      let _nodemax = fm2key fm f in

      iscorrupted (F.read_char fm f) cc1;
      let _nodemin = fm2key fm f in

      iscorrupted (F.read_char fm f) cc2;
      let current_revision = fm2rev fm f in

      iscorrupted (F.read_char fm f) cc3;
      let _previous_revision =
        let shb = F.read_char fm f in
        if shb = cc0 then Some (fm2rev fm f)
        else (iscorrupted shb cc1;  None) in

      iscorrupted (F.read_char fm f) cc4;
      let content = fm2data fm f in

      iscorrupted (F.read_char fm f) cc5;
      let size = F.read_int fm f in
      let nodemap = read_key_map size fm f in

      iscorrupted (F.read_char fm f) cc6;
      let size = F.read_int fm f in

      let _old_revisions =
          match size with
          | 0 -> []
          | 1 ->
              let r1 = fm2rev fm f in
              let u1 = fm2uid fm f in
              let ru = r1, u1 in
              Hashtbl.add hash_oldrevs ru [ru];
              [ru]
          | 2 ->
              let r1 = fm2rev fm f in
              let u1 = fm2uid fm f in
              let r2 = fm2rev fm f in
              let u2 = fm2uid fm f in
              let ru1 = r1, u1 and ru2 = r2, u2 in
              let rest =
                try Hashtbl.find hash_oldrevs ru2
                with Not_found -> [] in
              let res = ru1 :: rest in
              Hashtbl.add hash_oldrevs ru1 res;
              res
          | _ -> raise F.Corruption in

      N.Full { N.
        cur_rev = current_revision ;
        content ;
        map = nodemap ;
      }

  | c when c = cc1 ->
      iscorrupted (F.read_char fm f) cc0;
      let d_uid = fm2uid fm f in

      iscorrupted (F.read_char fm f) cc1;
      let d_rev = fm2rev fm f in

      iscorrupted (F.read_char fm f) cc2;
      let d_new_content =
        let shb = F.read_char fm f in
        if shb = cc0 then Some (fm2data fm f)
        else (iscorrupted shb cc1; None) in

      iscorrupted (F.read_char fm f) cc3;
      let nsize = F.read_int fm f in
      let d_nodemap = read_key_map nsize fm f in

      iscorrupted (F.read_char fm f) cc4;
      let d_profondeur = F.read_int fm f in
      N.Delta (d_uid, d_rev, { N.
        new_content = d_new_content ;
        new_childs = d_nodemap ;
        prof = d_profondeur ;
      })

  | _ -> raise F.Corruption

  in
  Node.read io_node


(* +++ CONFIG +++ *)
(* Configuration file
 * Contain db version number
 * - int : version number
 * - rev : revision of the last db state (snapshot)
 *)
let write_config iom config =
  logfm iom "Write config";
  F.seek_out iom F.Config 0;
  F.add iom F.Config [F.WInt config.DT.version; rev2fm config.DT.snapshot_rev]

let write_config_last_snapshot iom revision =
  logfm iom "Write config (only last snapshot)";
  F.seek_out iom F.Config 4;
  F.add iom F.Config [rev2fm revision]

let read_config iom =
  logfm iom "Read config";
  F.seek_in iom F.Config 0;
  let version = F.read_int iom F.Config in
  let snapshot_rev = fm2rev iom F.Config in
  { DT.
      version ;
      snapshot_rev ;
  }

let read_version iom =
  logfm iom "Read config (only version)";
  F.seek_in iom F.Config 0;
  F.read_int iom F.Config

let write_version iom vers =
  logfm iom "Write config (only version)";
  F.seek_out iom F.Config 0;
  F.add_int iom F.Config vers;
  if F.length iom F.Config = 4 then
    (logfm iom "Write config : add 0 for last snapshot";
    F.add_int iom F.Config 0)



(* +++ FLAGS +++ *)
(* Flag file
 * Contain last out position of each file
 * TODO : keep flags for each revision
 * Rewrite from the begin every time : no more since version 23
 * Structure only ints:
 * - uid_rev position
 * - uid position
 * - node position
 * - timestamps position
 * - trans position
 *)

let write_flags iom =
  (* version alternative
     let list = [ F.Uid_rev; F.Uid; F.Node; F.Timestamp; F.Trans ] in
     let list = List.tail_map (fun x -> WInt (F.position_out iom x)) list
     F.add ~output:true iom F.Flags list
  *)
  let uidr_pos = F.position_out iom F.Uid_rev in
  let uid_pos = F.position_out iom F.Uid in
  let node_pos = F.position_out iom F.Node in
  let tms_pos = F.position_out iom F.Timestamp in
  let tr_pos = F.position_out iom F.Trans in
  let length = F.length iom F.Flags in
  logfm iom "Write flags at %d" length;
  F.seek_out iom F.Flags length;
  F.add ~output:true iom F.Flags
    [F.WInt uidr_pos; F.WInt uid_pos; F.WInt node_pos; F.WInt tms_pos; F.WInt tr_pos]


let overwrite_flags ?uidr ?node ?trans ?tms ~uid ~rev iom =
  let rev = Revision.value rev in
  let uid = Uid.value uid in
  let uidr = Option.default (15 * (rev +1)) uidr in
  let node = match node with Some n -> n
  | None ->
      F.seek_in iom F.Uid (uid * 4);
      F.read_int iom F.Uid in
  let tms = Option.default (9*(rev +1)) tms in
  let tr = Option.default 0 trans in
  let uid = uid * 4 in
  let position = rev * 20 in
  logfm iom "Overwrite flags at %d" position;
  F.seek_out iom F.Flags position;
  F.add ~output:true iom F.Flags
    [F.WInt uidr; F.WInt uid; F.WInt node; F.WInt tms; F.WInt tr];
 (* as we want to overwrite, so we truncate all other data that are after the last position *)
  F.set_size iom F.Flags (position + 20)

(* +++ TIMESTAMP +++ *)
(* Timestamp file :
 * Contain each revision date
 * Structure :
 * - [[ '0' + date (float) ]]
 *)
let write_timestamp fm ts =
  seek_file fm F.Timestamp;
  logfm fm "Write timestamp at %d" (F.position_out fm F.Timestamp);
  F.add ~output:true fm F.Timestamp [ c0; F.WInt64 (Int64.of_int (Time.in_milliseconds ts))]

let read_timestamp fm rev =
  (* 9 bytes are written for each revision *)
  let pos = 9 * rev in
  logfm fm "Read timestamp at %d (for rev %d)" pos rev;
  F.seek_in fm F.Timestamp pos;
  iscorrupted (F.read_char fm F.Timestamp) cc0;
  let t = F.read_int64 fm F.Timestamp in
  Time.milliseconds (Int64.to_int t)

(* +++ UID REV +++ *)
(* Uid Rev file :
 * Contain the eid, uid and revision for each revision
 * schema :
 * - [[  '0' + Eid
 * - + '1' + Uid
 * - + '2' + Revision
 * - ]]
 *)

let write_uid_rev fm uidrev =
  seek_file fm F.Uid_rev;
  (* check *)
  let position = 15 * Revision.value uidrev.DT.rev in
  let seeked = F.position_out fm F.Uid_rev in
  if position <> seeked then
    (error "Database disk writing problem: Write Uid rev : seeked at %d, should be at %d (%s)" seeked position (Revision.to_string uidrev.DT.rev); assert false);
  logfm fm "Write uidrev at %d" position;
  let w = [
    c0 ; eid2fm uidrev.DT.eid ;
    c1 ; uid2fm uidrev.DT.uid ;
    c2 ; rev2fm uidrev.DT.rev ;
  ] in
  F.add ~output:true fm F.Uid_rev w

(** we want to read the last rev, uid and eid written on disk
    we read the last position valid on the flag file
    we seek at last position minus 15 (3 ints and 3 chars)
    we read *)
let read_uid_rev ?rev fm =
  let vrev = Option.map Revision.value rev in
  let real_pos =
    match vrev with
    | None -> (get_pos_from_flags fm F.Uid_rev) - 15
    | Some vrev -> 15 * vrev
  in
  logfm fm "Read uidrev at %d%s"
    real_pos (Option.default_map "" (fun x -> "(for rev " ^ string_of_int x ^ ")") vrev);
  try
    F.seek_in fm F.Uid_rev real_pos;
    iscorrupted (F.read_char fm F.Uid_rev) cc0;
    let eid = fm2eid fm F.Uid_rev in
    iscorrupted (F.read_char fm F.Uid_rev) cc1;
    let uid = fm2uid fm F.Uid_rev in
    iscorrupted (F.read_char fm F.Uid_rev) cc2;
    let rev = fm2rev fm F.Uid_rev in
    { DT.
        eid ;
        uid ;
        rev ;
    }
  with F.EOF -> raise (DT.CrashUidRev (F.position_in fm F.Uid_rev < 15))


(* +++ DB STATE +++ *)
(* Database state file
 * Contain a snapshot of the db, taken each 1000 revision (see session.ml, write_limit)
 * Rewritten from the beginning every time, and empty transaction file
 * Update transaction file flag on flags file
 * Structure :
 * - [[  '0' + length uid map + Uid Map
 * -   + '1' + length index + Index
 * - Uid Map :
 * -  [[ '0' + eid + '1' + length next + [[ '0' + revision + '1' + uid ]] ]]
 * - Index :
 * -  [[ '0' + name + '1' + length next + '2' + [[ '0' + path + '1' + score ]] ]]
 * - ]]
 *)

let write_dbstate ?(reset=true) fm ~uidmap ~index =
  logfm fm "Write dbstate";
  if reset then
    (logfm fm "Reset files"; F.reset_files fm);

  let transitional = F.get_name fm F.Db_state ^ ".tmp" in

  let db_aux = F.create_unik transitional in

  let out lst = F.add_unik db_aux lst in

  let lst = [ c0; F.WInt (EidMap.size uidmap) ] in
  out lst;
  (*
    Mathieu Thu Mar 17 11:21:23 CET 2011
    For historical reasons, the order in the binary format was in reverse order
    of eid order. But, this does not make any importance, because we rebuild an
    ordered map when we read it back.
    We could switch to a regular iter in an other commit, checking compatibility
    of import/export between binary formats.
  *)
  EidMap.iter
    (fun e rul ->
       let e = Eid.value e in
       let lst = [ c0; F.WInt e; c1; F.WInt (RevisionMap.size rul) ] in
       out lst;
       RevisionMap.iter
         (fun r u ->
            let r = Revision.value r in
            let u = Uid.value u in
            let lst = [c0; F.WInt r; c1; F.WInt u] in
            out lst)
         rul)
    uidmap;
  let lst = [ c0; F.WInt (StringMap.size index) ] in
  out lst;
  (*
    Exactly same remark, the list was build in reverse order.
    <!> be extremly aware of the symetric function read, which
    needs to know the order used because of the opimization
    about the map reconstruction.
    Since the revision 21, the order is increasing.
  *)
  StringMap.iter
    (fun n pfl ->
       let lst =  [ c0; F.WString n; c1; F.WInt (List.length pfl); c2 ] in
       out lst;
       (List.iter
          (fun (p,sc) ->
             let lst = c0 :: (path2fm p) @ [c1; F.WFloat sc] in
             out lst)
          pfl))
    index;
  (* XXX Flags length en dur *)
  let rem = (F.length fm F.Flags) - 4 in
  logfm fm "Write flags: overwrite trans flag";
  F.seek_out fm F.Flags rem;
  F.add_int fm F.Flags 0;

  (try F.mv fm db_aux F.Db_state; logfm fm "Moved dbstate.tmp to dbstate"
  with Failure "No mv" -> error "Can't save the new db_state file. It may corrupt the db");
  logfm fm "Empty transaction files";
  F.empty_file fm F.Trans


let char_int_char_int = [F.RChar; F.RInt; F.RChar; F.RInt ]

let read_dbstate fm =
  let length = F.length fm F.Db_state in
  let is_empty = length = 0 in
  if is_empty then
    error "Read error : Try to read an empty db state";

  F.seek_in fm F.Db_state 0;

  logfm fm "Read dbstate at %d (length %d)" (F.position_in fm F.Db_state)  length;

  let uidmap =
    let mappumulator = ref (EidMap.empty, 0) in
    if is_empty then EidMap.empty
    else (
      try
      iscorrupted (F.read_char fm F.Db_state) cc0;
      let size = F.read_int fm F.Db_state in
      Loop.For.range 0 size EidMap.empty (
        fun _ uidmap -> (
          let _lastpos = F.position_in fm F.Db_state in
            let eid, rev_size =
              match F.read fm F.Db_state char_int_char_int with
              | [ c00; F.WInt eid; c11; F.WInt rev_size ] when c00 = c0 && c11 = c1 ->
                  Eid.make eid, rev_size
              | _ ->
                  raise F.Corruption
            in
            let revmap =
              Loop.For.range 0 rev_size RevisionMap.empty (
                fun _ revmap -> (
                  match F.read fm F.Db_state char_int_char_int with
                  | [ c00 ; F.WInt r ; c11 ; F.WInt u ] when c00 = c0 && c11 = c1 ->
                      let r = Revision.make r in
                      let u = Uid.make u in
                      let revmap = RevisionMap.add r u revmap in
                      revmap
                  | _ ->
                      raise F.Corruption
                )
              )
            in
            let uidmap = EidMap.add eid revmap uidmap in
            mappumulator := (uidmap, _lastpos);
            uidmap
        )
      )
    with F.EOF -> (let m,p = !mappumulator in raise (DT.CrashStateMap (m,p)))
    )
  in

  let index =
    if is_empty then StringMap.empty
    else (
      iscorrupted (F.read_char fm F.Db_state) cc0;
      let size = F.read_int fm F.Db_state in
      (*
        Optimized reconstruction, without tmp balancing.
      *)
      let words = Array.make size "" in
      let pfls = Array.make size [] in
      let db_chan = F.Channel.get fm F.Db_state in
      for index = 0 to pred size do
        try
        let c00 = F.Channel.read_char db_chan in
        let word = F.Channel.read_string db_chan in
        let c11 = F.Channel.read_char db_chan in
        let pfl_size = F.Channel.read_int db_chan in
        let c22 = F.Channel.read_char db_chan in
        if not (c00 = cc0 && c11 = cc1 && c22 = cc2) then raise F.Corruption;
        let pfl = List.init pfl_size (
          fun _ ->
            iscorrupted (F.read_char fm F.Db_state) cc0;
            let path = fm2path fm F.Db_state in
            iscorrupted (F.read_char fm F.Db_state) cc1;
            let fl = F.read_float fm F.Db_state in
            path, fl
        ) in
        Array.unsafe_set words index word ;
        Array.unsafe_set pfls index pfl ;

        with F.EOF ->
          (let pos = F.position_in fm F.Db_state in
           if index = 0 then raise (DT.CrashStateIndex ({DT. uidmap; index = StringMap.empty}, pos))
           else
            let words = Array.sub words 0 index in
            let pfls = Array.sub pfls 0 index in
            let index = StringMap.from_sorted_array words pfls in
            raise (DT.CrashStateIndex ({DT. uidmap; index}, pos)));

      done ;
      let indexmap = StringMap.from_sorted_array words pfls in
      indexmap
    )
  in

  { DT.
      uidmap ;
      index ;
  }



(* +++ NODE && UID +++ *)
(* Node and Uid file
 * Node : contain all db nodes
 * Uid : given an uid, contain its node position
 * Node Structure :
 * - [[ nodes ]]
 * Uid Structure :
 * - if the Uid exist, an int for Node position, else Int 0
 *)
let write_nodes_aux ~seek fm ?last_uid nodes =
  if seek then
    (seek_file fm F.Node;
     seek_file fm F.Uid);
  let last_uid =
    match last_uid with
    | None ->
        (-1)
    | Some last_uid ->
        Uid.value last_uid
  in

  let accumulator = ref last_uid in

  let _ = UidMap.fold
   (fun uid node memory ->
     let uid = Uid.value uid in
     let lastu = !accumulator in

     (* Normally, the elements will follow,
      * but we keep checking to be sure to have the good positioning *)
     let uidpos = F.position_out fm F.Node in
     if lastu < 0 || (IntSet.mem lastu memory && uid = succ lastu) then
        F.add_int fm F.Uid uidpos
     else
        (for i = 0 to pred (uid - lastu) do
          F.add_int fm F.Uid (-1)
        done;
        F.add_int fm F.Uid uidpos);

     let wnode = write_node node in
     F.add ~output:true fm F.Node wnode;

     accumulator := uid;

    IntSet.add uid memory)
   nodes IntSet.empty in

  let wr = if seek then "Write" else "Overwrite" in
  logfm fm "%s nodes at %d" wr (F.position_out fm F.Node);
  logfm fm "%s uid at %d" wr (F.position_out fm F.Uid)


let write_nodes = write_nodes_aux ~seek:true
let overwrite_nodes fm nodes = write_nodes_aux ~seek:false fm nodes

(** we want to read all the nodes
    we start at the begining of the file
    we read until last valid position which is read in
    the flag file *)
let read_nodes fm =
  let mappumulator = ref (UidMap.empty,0) in
  let limit = get_pos_from_flags fm F.Node in
  F.seek_in fm F.Node 0;
  F.seek_in fm F.Uid 0;

  let rec aux uid acc =
    let pos = F.position_in fm F.Node in
    if pos >= limit then acc
    else
      (let acc = UidMap.add (Uid.make uid) (read_node fm) acc in
      mappumulator := (acc,pos);
      aux (succ uid) acc)
  in
  logfm fm "Read nodes at %d (to %d) & uid at %d"
    (F.position_in fm F.Node) limit (F.position_in fm F.Uid);
  try aux 0 UidMap.empty
  with F.EOF -> (let m,p = !mappumulator in raise (DT.CrashNode (m,p)))



let read_specific_node fm uid =
  let uid = Uid.value uid in
  let upos = 4 * uid in
  F.seek_in fm F.Uid upos;
  let npos = F.read_int fm F.Uid in
  F.seek_in fm F.Node npos;
  logfm fm "Read a node at %d, from uidfile at %d (inital value : %d)" npos upos  uid;
  read_node fm


(* +++ TRANS +++ *)
(* Transaction file
 * Contain transaction for each revision
 * Structure :
 * - [[  '0' + querymap
 * -   + '1' + ('0' if read_only, '1' else)
 * -   + '2' + length next + list of paths to remove ]]
 *)
let write_trans fm trans =
  Option.iter
    (fun trans ->
       seek_file fm F.Trans;
       logfm fm "Write transaction at %d" (F.position_in fm F.Trans);
       let lst = c0 :: querymap2fm trans.DT.querymap in
       let lst1 = [ c1; if trans.DT.read_only then c0 else c1 ] in
       let rml = trans.DT.remove_list in
       let lst2 = c2 :: F.WInt (List.length rml) :: List.concat_map path2fm rml in
       let lstf = List.tail_concat [lst; lst1; lst2] in
       F.add ~output:true fm F.Trans lstf) trans

let read_trans fm =
  logfm fm "Read Trans at %d" (F.position_in fm F.Trans);
  iscorrupted (F.read_char fm F.Trans) cc0;
  let querymap = fm2querymap fm F.Trans in
  iscorrupted (F.read_char fm F.Trans) cc1;
  let read_only = F.read_char fm F.Trans = cc0 in
  iscorrupted (F.read_char fm F.Trans) cc2;
  let size = F.read_int fm F.Trans in
  let rml = List.init size (fun _ -> fm2path fm F.Trans) in
  let remove_list = rml in
  { DT.
      querymap ;
      read_only ;
      remove_list ;
  }

let read_transs fm =
  let final_pos = get_pos_from_flags fm F.Trans in
  assert (F.position_in fm F.Trans = 0);
  let rec aux acc last_pos =
    if last_pos >= final_pos then
      List.rev acc
    else
      try
        let trans = read_trans fm in
        let last_pos = F.position_in fm F.Trans in
        aux (trans :: acc) last_pos
      with F.EOF -> raise (DT.CrashTrans (acc, last_pos))
  in
  logfm fm "Read trans file at %d (to %d)" (F.position_in fm F.Trans) final_pos;
  if final_pos = 0 then []
  else aux [] 0

let get_filemanager fm = fm

let do_backup fm =
    let lst = [ F.Node; F.Uid; F.Uid_rev; F.Timestamp; F.Flags; F.Trans; F.Db_state; F.Config ] in
    let ext =
      let date = Time.now () in
      let y = Time.gmt_year date
      and m = succ (Time.gmt_mon date) (* this succ is here because Unix count month from 0 to 11... *)
      and d = Time.gmt_mday date
      and h = Time.gmt_hour date
      and mi = Time.gmt_min date
      in Printf.sprintf ".backup_%d_%d_%d-%d_%d" y m d h mi
    in
    let copy f =
     try F.copy_file fm f ext
     with Failure "No cp" ->
       error "Can not do a backup of the file : %s" (F.get_name fm f)
    in
    List.iter copy lst


let restore_uidrev ~restore fm =
  let length = F.length fm F.Uid_rev in
  let fst_check length = length mod 15 = 0  in
  let snd_check length =
    let rec aux pos =
      let pos_tms = pos - 10 in
      F.seek_in fm F.Uid_rev pos_tms;
      if F.read_int fm F.Uid_rev = 0 then
        aux (pos -15)
  else pos in
    aux length in

  let set_size new_length =
    if restore then
      (if new_length = 0 then
        (print "Uid_rev file is corrupted from the beginnig";
        raise F.Corruption);
        F.set_size fm F.Uid_rev new_length)
  in

  let read pos =
    let pos = pos - 15 in
    F.seek_in fm F.Uid_rev pos;
    iscorrupted (F.read_char fm F.Uid_rev) cc0;
    let eid = fm2eid fm F.Uid_rev in
    iscorrupted (F.read_char fm F.Uid_rev) cc1;
    let uid = fm2uid fm F.Uid_rev in
    iscorrupted (F.read_char fm F.Uid_rev) cc2;
    let rev = fm2rev fm F.Uid_rev in
    let res =
      { DT.
          eid ;
          uid ;
          rev ;
      } in
    Some res
  in

  let last =
    if length <> 0 then
      if fst_check length then
        let new_length = snd_check length in
        if new_length = length then
          read length
        else
          (set_size new_length;
          read new_length)

      else
        (let new_length = length / 15 * 15 in
        let new_length = snd_check new_length in
        set_size new_length;
        read new_length)

    else None
  in
  F.seek_in fm F.Uid_rev 0;
  F.seek_out fm F.Uid_rev 0;
  (* finished to torture flags' file *)
  last

let cleanup_hashtbls () =
  PathHashCons.cleanup();
  Hashtbl.clear hash_oldrevs
