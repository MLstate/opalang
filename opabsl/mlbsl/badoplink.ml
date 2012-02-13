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
(* ================================================================= *)
(** DB : link to the new BADOP interface                             *)
(* ================================================================= *)

module C = QmlCpsServerLib
module D = Badop.Dialog
module E = Badop_engine
open C.Ops (* This file follows the duck-style cps guidelines © *)

let error s = Logger.error "Database error: %s" s

(* Manual conversion for the cases where the BSL can't convert (value passed to a cont...) *)
let qml_some x = ServerLib.wrap_option (Some x)
let qml_none () = ServerLib.wrap_option None

let transaction_fail_exception =
  (*
     <!> keep consistent with the type Exception.common from the Opa stdlib
  *)
  let fld = ServerLib.static_field_of_name "Transaction_failure" in
  let fields = ServerLib.empty_record_constructor in
  let fields = ServerLib.add_field fields fld ServerLib.void in
  let record = ServerLib.make_record fields in
  BslNativeLib.wrap_opa_exception_common record

let abort_transaction k =
  QmlCpsServerLib.return (QmlCpsServerLib.handler_cont k) transaction_fail_exception

(** abstract types used by dbgenlink *)
##extern-type [normalize] db_path_key = Badop.key
##extern-type [normalize] db_partial_key = Badop.key option list
##extern-type [normalize] path = Badop.path
##extern-type [normalize] database = { db_engine: Badop_engine.t; db: Badop_engine.db }
##extern-type [normalize] transaction = { tr_engine: Badop_engine.t; tr: Badop_engine.tr }
##extern-type [normalize] revision = Badop_engine.rv
##extern-type dbset('a) = {     \
  transaction : transaction ;   \
  path : path ;                 \
  reader : transaction -> path -> 'a QmlCpsServerLib.continuation -> unit ;  \
  keys : Badop.key option array ; \
}

##extern-type [normalize] node_config = Badop.Node_property.config

##module [restricted:dbgen] data
  ##extern-type [normalize] d = Badop.Data.t

##register [opacapi] int : int -> d
  let int i = Badop.Data.Int i

##register [opacapi] text : string -> d
  let text s = Badop.Data.Text s

##register [opacapi] binary : string -> d
  let binary s = Badop.Data.Binary s

##register [opacapi] float : float -> d
  let float f = Badop.Data.Float f

##register [opacapi] unit : -> d
  let unit () = Badop.Data.Unit

(* Fixme: with low-level typing in the db, this could be made safe
   at this level (handle checking at lower level) *)
##register [opacapi] obj_int : d -> int
  let obj_int = function Badop.Data.Int i -> i | _ -> assert false

##register [opacapi] obj_text : d -> string
  let obj_text = function Badop.Data.Text s -> s | _ -> assert false

##register [opacapi] obj_binary : d -> string
  let obj_binary = function Badop.Data.Binary s -> s | _ -> assert false

##register [opacapi] obj_float : d -> float
  let obj_float = function Badop.Data.Float f -> f | _ -> assert false

##register obj_unit : d -> void
  let obj_unit = function Badop.Data.Unit -> () | _ -> assert false
(*
% ##register i : d -> int
%   let i x = Badop.Data.obj_int x
%  register f \ fcast : extern d -> float
%  let fcast = DataImpl.obj_float
%
%  register s \ scast : extern d -> string
%  let scast = DataImpl.obj_string
*)
##endmodule


##register [opacapi;restricted:dbgen] jlog: string -> void
let jlog s = Logger.notice "%s" s

let debug __s = #<If:DEBUG_DB> Logger.log ~color:`magenta __s #<End>

##register [opacapi;restricted:dbgen,cps-bypass] error: string, continuation('a) -> void
let error s k =
  error s; abort_transaction @> k

##register [opacapi;restricted:dbgen] fatal_error \ fatal_error_for_dbgen: string, string, string -> 'a
let fatal_error_for_dbgen = fun s1 s2 s3 -> Logger.critical "%s%s%s" s1 s2 s3; BslSys.do_exit 1

(* let wrap_option : 'a_option BslCps.continuation -> 'a Dblib.answer -> unit = fun k a -> match a with *)
(*   | `Answer x -> qmlreturn k (qml_some x) *)
(*   | `Absent -> debug s; qmlreturn k (qml_none ()) *)

##register[cps-bypass] status_string: database, continuation(string) -> void
let status_string db k =
  let rec print_status st k = match st with
    | Badop.Local file -> Printf.sprintf "Local(\"%s\")" file |> k
    | Badop.Light file -> Printf.sprintf "Light(\"%s\")" file |> k
    | Badop.Client (_local, (remote,port), remote_st) ->
        let remote =
          try (Unix.gethostbyaddr remote).Unix.h_name
          with Not_found -> Unix.string_of_inet_addr remote in
        print_status remote_st @> C.ccont_ml k
        @> fun remote_s -> Printf.sprintf "Client(%s:%d,%s)" remote port remote_s |> k
    | Badop.Layer (name,st) -> print_status st @> C.ccont_ml k @> fun st -> Printf.sprintf "%s(%s)" name st |> k
    | Badop.Layer_multi (name,sts) ->
        C.fold_list
          (fun acc st k ->
             print_status st @> C.ccont_ml k
             @> fun s -> (if acc = "" then "" else acc^", ") ^ s |> k)
          "" sts
        @> C.ccont_ml k
        @> fun s -> Printf.sprintf "%s(%s)" name s |> k
    | Badop.Other s -> s |> k
  in
  db.db_engine.E.status db.db @> fun st -> print_status st @> k

##register [opacapi;cps-bypass] db_prefix \ get_db_prefix: database, continuation(string) -> void
let get_db_prefix db k =
  let rec get_file st k = match st with
    | Badop.Local file -> file |> k
    | Badop.Light file -> file |> k
    | Badop.Client (_local, (remote,_port), remote_st) ->
        let remote =
          try (Unix.gethostbyaddr remote).Unix.h_name
          with Not_found -> Unix.string_of_inet_addr remote in
        get_file remote_st @> C.ccont_ml k
        @> fun remote_s -> Printf.sprintf "%s:%s" remote remote_s |> k
    | Badop.Layer (_name,st) -> get_file st @> k
    | Badop.Layer_multi (_name,sts) ->
        C.fold_list
          (fun acc st k ->
             get_file st @> C.ccont_ml k
             @> fun s -> (if acc = "" then "" else acc^", ") ^ s |> k)
          "" sts
        @> k
    | Badop.Other s -> s |> k
  in
  db.db_engine.E.status db.db @> fun st -> get_file st @> k

##register [opacapi;restricted:dbgen,cps-bypass] open_db: badop_engine.t, continuation(database) -> void
let open_db engine k =
  engine.E.open_database ()
  @> fun db ->
    Scheduler.at_exit BslScheduler.opa (fun () ->
      flush stdout; flush stderr;
      engine.E.close_database db @> (fun () -> ())
    );
    let r = { db_engine = engine; db = db } in
    #<If:DEBUG_DB>
      (status_string r @> C.ccont_ml k
       @> fun st -> Printf.eprintf "Database opened successfully as %s\n" st; r |> k)
    #<Else>
      (r |> k)
    #<End>

##register[opacapi;cps-bypass] node_properties: database, node_config, continuation(opa[void]) -> void
let node_properties db config k =
  db.db_engine.E.node_properties db.db config @> fun () -> ServerLib.void |> k

##register[opacapi] node_config_construct \ `Badop.Structure.Node_property.construct` : string -> node_config

##register[cps-bypass] client_address: database, continuation(opa[option(string)]) -> void
let client_address db k =
  db.db_engine.E.status db.db
  @> function
  | Badop.Client (addr, _remote, _) -> qml_some (ServerLib.wrap_string (Unix.string_of_inet_addr addr)) |> k
  | _ -> qml_none () |> k

let in_trans db k f =
  db.db_engine.E.tr_start db.db
    (fun _exc -> abort_transaction k)
  @> fun tr ->
    f tr @> C.ccont_ml k
    @> fun x ->
      db.db_engine.E.tr_prepare tr
      @> (fun k (tr, success) -> assert success; db.db_engine.E.tr_commit tr k)
      @> (fun k success -> assert success; x |> k)
      @> k

##register [opacapi;restricted:dbgen,cps-bypass] is_db_new : database, continuation(int) -> void
let is_db_new db k =
  in_trans db k
    (fun tr k ->
       (* /2 is the config-path (defined in dbGen_private) *)
       db.db_engine.E.read tr (Badop.Path.of_list [Badop.Key.IntKey 2]) (Badop.Stat (D.query ()))
       @> function
       | `Answer (Badop.Stat _) -> 0 |> k
       | `Answer _ | `Linkto _ -> assert false
       | `Absent -> 1 |> k)

##register [opacapi;restricted:dbgen] key_int: int -> db_path_key
let key_int =
  fun i -> Badop.Key.IntKey i

##register [opacapi;restricted:dbgen] key_string: string -> db_path_key
let key_string =
  fun s -> Badop.Key.StringKey s

##register [opacapi;restricted:dbgen] key_list: caml_list(db_path_key) -> db_path_key
let key_list =
  fun l -> Badop.Key.ListKey (Array.of_list l)

##register [opacapi;restricted:dbgen;cps-bypass] key_value_int: db_path_key, continuation(int) -> void
let key_value_int key k = match key with
  | Badop.Key.IntKey i -> i |> k
  | _ -> error "Inconsistent key in the database (not int)" @> k

##register [opacapi;restricted:dbgen;cps-bypass] key_value_string: db_path_key, continuation(string) -> void
let key_value_string key k = match key with
  | Badop.Key.StringKey s -> s |> k
  | _ -> error "Inconsistent key in the database (not string)" @> k

(* #register [restricted : dbgen] key_to_string : db_path_key -> string *)
(* let key_to_string = Dblib.Key.to_string *)

##register [opacapi;restricted:dbgen] dbpath_root: path
let dbpath_root = Badop.Path.root

##register [opacapi;restricted:dbgen] dbpath_add: path, db_path_key -> path
let dbpath_add p key = Badop.Path.add p key

##register [opacapi;restricted:dbgen,cps-bypass] trans_start: database, continuation(transaction) -> void
let trans_start db k =
  db.db_engine.E.tr_start db.db
    (fun _exc -> abort_transaction k)
  @> fun tr -> { tr_engine = db.db_engine; tr = tr } |> k

let trans_start_at_revision db rev k =
  db.db_engine.E.tr_start_at_revision db.db rev
    (fun _exc -> abort_transaction k)
  @> fun tr -> { tr_engine = db.db_engine; tr = tr } |> k

##register[restricted:dbgen,cps-bypass] trans_start_read_only: database, continuation(transaction) -> void
let trans_start_read_only = trans_start (* deprecated *)

##register [opacapi;restricted:dbgen,cps-bypass] trans_abort: transaction, continuation(opa[void]) -> void
let trans_abort tr k =
  tr.tr_engine.E.tr_abort tr.tr @> fun () -> ServerLib.void |> k

##register [opacapi;restricted:dbgen,cps-bypass] trans_commit: transaction, continuation(opa[void]) -> void
let trans_commit tr k_final =
  tr.tr_engine.E.tr_prepare tr.tr
  @> (fun k (low_tr, success) ->
        if not success then begin
          Logger.info "Transaction prepare failed";
          ServerLib.void |> k_final
        end else begin
          tr.tr_engine.E.tr_commit low_tr k
        end)
  @> (fun k success ->
        if not success then Logger.info "Transaction commit failed";
        ServerLib.void |> k)
  @> k_final

##register[opacapi;restricted:dbgen,cps-bypass] get_opt: transaction, path, continuation(opa[option(data_d)]) -> void
let get_opt tr path k =
    tr.tr_engine.E.read tr.tr path (Badop.Contents (D.query ()))
    @> function
    | `Answer (Badop.Contents (D.Response resp)) -> qml_some resp |> k
    | `Answer _ | `Linkto _ -> assert false
    | `Absent -> qml_none () |> k

##register[opacapi;restricted:dbgen, cps-bypass] get_new_key: transaction, path, continuation(int) -> void
let newkey_cache = (Hashtbl.create 11 : (path,int) Hashtbl.t)
(* FIXME: to get proper fresh keys, we need help from the backend. The hashtbl
   here, assuming it's local, should just ensure that successive calls in the
   same transaction will return different keys. Avoiding conflicts for real is
   another story. (also, this is a memory hole) *)
(* FIXME CRITICAL: BROKEN for multi-db *)
let get_new_key tr path k =
  tr.tr_engine.E.read tr.tr path (Badop.Children (D.query (None,-1)))
  @> fun answer ->
    (fun k -> match answer with
     | `Answer (Badop.Children (D.Response [path])) ->
         key_value_int (Badop.Path.last path) @> k
     | `Answer (Badop.Children (D.Response (_path0::paths))) ->
         Printf.eprintf "Error => got a list with %d elements with range (None,-1) ! <workaround>\n"
           (List.length paths + 1);
         key_value_int (Badop.Path.last (Base.List.last paths)) @> k
     | `Answer (Badop.Children (D.Response [])) | `Absent ->
         (-1) |> k
     | `Answer _ | `Linkto _ -> assert false)
    @> C.ccont_ml k
    @> fun dbmax ->
      let newkey =
        try 1 + max (Hashtbl.find newkey_cache path) dbmax
        with Not_found -> 1 + dbmax
      in
      Hashtbl.replace newkey_cache path newkey;
      newkey |> k

##register [opacapi;restricted:dbgen,cps-bypass] exists: transaction, path, continuation(int) -> void
let exists tr path k =
  tr.tr_engine.E.read tr.tr path (Badop.Stat (D.query ()))
  @> function
  | `Answer (Badop.Stat (D.Response _)) -> 1 |> k
  | `Answer _ | `Linkto _ -> assert false
  | `Absent -> 0 |> k

##register [opacapi;restricted:dbgen,cps-bypass] uppath: transaction, path, continuation(path) -> void
let uppath tr path k =
  tr.tr_engine.E.read tr.tr path (Badop.Stat (D.query ())) (* Follows links *)
  @> function
  | `Answer (Badop.Stat (D.Response (path, _, _))) ->
      snd (Option.get (Badop.Path.pop_last path)) |> k
  | `Answer _ | `Linkto _ -> assert false
  | `Absent ->
      snd (Option.get (Badop.Path.pop_last path)) |> k

(* from database/Badop_locator *)
let parallel_map sched (iter: ('a Cps.t -> unit) -> unit) (kf: 'a array -> unit) =
  let n = ref 0 in
  let results = ref [||] in
  let ki =
    fun i x ->
      !results.(i) <- Some x;
      decr n;
      if !n > 0 then () else kf (Array.map Option.get !results)
  in
  iter
    (fun f ->
       let i = !n in incr n;
       Scheduler.push sched (fun () -> f @> ki i));
  if !n = 0 then kf [||]
  else results := Array.make !n None

##register [opacapi;restricted:dbgen,cps-bypass] fold_children: \
    transaction, \
    path, \
    (transaction, path, continuation('a) -> void), \
    ('acc, db_path_key, 'a, continuation('acc) -> void), \
    'acc, \
    continuation('acc) -> \
    void
let fold_children tr path read_child f acc k =
  tr.tr_engine.E.read tr.tr path (Badop.Children (D.query (None,0)))
  @> function
  | `Answer (Badop.Children (D.Response children)) ->
      parallel_map Scheduler.default
        (fun g ->
           List.iter
             (fun path ->
                g (fun kf -> read_child tr path @> C.ccont_ml k @> fun child -> kf (Badop.Path.last path, child))
             )
             children)
      @> fun arr ->
        C.fold_array
          (fun acc (key,value) k -> f acc key value @> k)
          acc arr
      @> k
  | `Answer _ | `Linkto _ -> assert false
  | `Absent -> acc |> k

##register [opacapi;restricted:dbgen,cps-bypass] fold_int_keys: \
    transaction, \
    path, \
    (int, 'a, continuation('a) -> void), \
    'a, \
    continuation(opa[option('a)]) -> \
    void
let fold_int_keys tr path f acc k =
  tr.tr_engine.E.read tr.tr path (Badop.Children (D.query (None,0)))
  @> function
  | `Answer (Badop.Children (D.Response children)) ->
      C.fold_list
        (fun acc path k -> key_value_int (Badop.Path.last path) @> C.ccont_ml k @> fun i -> f i acc @> k)
        acc children @> C.ccont_ml k
      @> (fun acc -> qml_some acc |> k)
  | `Answer _ | `Linkto _ -> assert false
  | `Absent -> qml_none () |> k

##register [opacapi;restricted:dbgen,cps-bypass] fold_string_keys: \
    transaction, \
    path, \
    (string, 'a, continuation('a) -> void), \
    'a, \
    continuation(opa[option('a)]) -> \
    void
let fold_string_keys tr path f acc k =
  tr.tr_engine.E.read tr.tr path (Badop.Children (D.query (None,0)))
  @> function
  | `Answer (Badop.Children (D.Response children)) ->
      C.fold_list
        (fun acc path k -> key_value_string (Badop.Path.last path) @> C.ccont_ml k @> fun s -> f s acc @> k)
        acc children @> C.ccont_ml k
      @> (fun acc -> qml_some acc |> k)
  | `Answer _ | `Linkto _ -> assert false
  | `Absent -> qml_none () |> k

##register [opacapi;restricted:dbgen,cps-bypass] create_dbset : \
    transaction, \
    path, \
    (transaction, path, continuation('a) -> void), \
    continuation(dbset('a)) -> \
    void
let create_dbset tr path reader k = {
  transaction = tr;
  path = path;
  reader = reader;
  keys = [||];
} |> k

##register [opacapi;restricted:dbgen] empty_partial_key : db_partial_key
let empty_partial_key = []

##register [opacapi;restricted:dbgen,cps-bypass] add_key : db_path_key, db_partial_key, continuation(db_partial_key) -> void
let add_key key p k = Some key::p |> k

##register [opacapi;restricted:dbgen,cps-bypass] add_hole : db_partial_key, continuation(db_partial_key) -> void
let add_hole p k = None::p |> k

let rec partial_match partial keylist =
  assert (Array.length partial = Array.length keylist);
  let rec aux i =
    if i < 0 then true
    else match Array.get partial i with
    | None -> aux (i-1)
    | Some k when k = Array.get keylist i -> aux (i-1)
    | _ -> false
  in aux (Array.length partial - 1)


##register [opacapi;restricted:dbgen,cps-bypass] set_dbset_keys : dbset('a), db_partial_key, continuation(dbset('a)) -> void
let set_dbset_keys dbset keys k = { dbset with keys = Array.of_list keys } |> k

##register[cps-bypass] fold_dbset : 'acc, dbset('a), ('acc, 'a, continuation('acc) -> void), continuation('acc) -> void
let fold_dbset acc dbset folder k =
  let tr = dbset.transaction in
  tr.tr_engine.E.read tr.tr dbset.path (Badop.Children (D.query (None, 0))) @>
    function
      | `Answer (Badop.Children (D.Response children)) ->
          let partial = dbset.keys in
          let reader = dbset.reader in
          C.fold_list
            (fun acc path k ->
               match Badop.Path.last path with
               | Badop.Key.ListKey kl when partial_match partial kl ->
                   (reader tr path @> C.ccont_ml k (function x -> folder acc x k))
               | Badop.Key.ListKey _ ->
                   acc |> k
               | _ -> assert false
            ) acc children k
      | `Absent ->
          Logger.error "[fold_dbset] Path to the dbset is absent";
          acc |> k
      | _ -> assert false

type 'a cps = 'a C.continuation -> unit
let rec traverse_fold tr (f:('acc -> path -> 'acc cps) -> 'acc -> path -> 'acc cps) acc path k =
  f (fun acc path k ->
       tr.tr_engine.E.read tr.tr path (Badop.Children (D.query (None,0)))
       @> function
       | `Answer (Badop.Children (D.Response children)) ->
           C.fold_list (traverse_fold tr f) acc children @> k
       | `Answer _ | `Linkto _ -> assert false
       | `Absent -> acc |> k)
    acc path
  @> k

let fold_down tr path f acc k =
  traverse_fold tr
    (fun tra acc path k -> f acc path @> C.ccont_ml k @> fun acc -> tra acc path @> k)
    acc path
  @> k

let fold_up tr path f acc k =
  traverse_fold tr
    (fun tra acc path k -> tra acc path @> C.ccont_ml k @> fun acc -> f acc path @> k)
    acc path
  @> k

(* Was used in recursive removal. Unused right now. *)
let fold_up_no_follow_links tr path f acc k =
  traverse_fold tr
    (fun tra acc path k ->
       tr.tr_engine.E.read tr.tr path (Badop.Stat (D.query ()))
       @> function
       | `Answer (Badop.Stat (D.Response (_path,_revopt,(`Data|`Unset)))) ->
           tra acc path @> C.ccont_ml k @> fun acc -> f acc path @> k
       | `Answer _ | `Absent -> (* on a link or non-existing node, don't get deeper but still apply f *)
           f acc path @> k
       | `Linkto _ -> assert false)
    acc path
  @> k

let merge_lists_range (first, nb_max, last) l1 l2 =
  let (<<),(>>) = if nb_max >= 0 then (<),(>) else (>),(<) in
  let rec aux acc n l1 l2 = (* Returns the merge of l1,l2 cut at n, and optional upper bound *)
    if n = 0 then acc else
      match l1,l2 with
      | x1::r1, x2::r2 ->
          if x1 << x2 then aux (x1::acc) (n-1) r1 l2
          else if x1 >> x2 then aux (x2::acc) (n-1) l1 r2
          else aux (x1::acc) (n-1) r1 r2
            (* about this last case, if not comparable but not equal (for
               lattice-revisions in the distributed database) maybe both should be
               kept (in any order) so that we don't flatten distinct changes *)
      | [], x::r | x::r, [] ->
          aux (x::acc) (n-1) [] r
      | [], [] -> acc
  in
  let count = if nb_max > 0 then nb_max else if nb_max < 0 then -nb_max else -1 in
  let rev_merge = aux [] count l1 l2 in
  match rev_merge with
  | x::_ -> (first, nb_max, Some x), List.rev rev_merge
  | [] -> (first, nb_max, last), []

let revisions tr path range k =
  traverse_fold tr
    (fun tra (range,acc) path k ->
       tr.tr_engine.E.read tr.tr path (Badop.Revisions (D.query (let (fst,nb,_last) = range in (fst,nb))))
       @> function
       | `Answer (Badop.Revisions (D.Response revisions)) ->
           tra (merge_lists_range range acc revisions) path @> k
       | `Answer _ | `Linkto _ -> assert false
       | `Absent -> (range, acc) |> k)
    (range, [])
    path
  @> C.ccont_ml k
  @> fun (_range, revision_list) -> revision_list |> k
      (* Todo: code to consistently extract revision from within links too:
         tr.B.tr_engine.E.read tr.B.tr path (Badop.Exists (D.query ())) (* check for links *)
       @> function
       | `Answer (Badop.Exists (R.response otherpath)) ->
           if otherpath = path then (* Not a link *)
             (* Normal case, as above *)
           else (* [path] is a link to [otherpath] *)
             tr.B.tr_engine.E.read tr.B.tr path (Badop.Revisions (D.query range))
             @> function
             | `Answer (Badop.Revisions (D.Response revisions)) ->
                 (* Each revision of the link opens a "window" in which we should search *)
                 ....
             | `Answer _ | `Linkto _ -> assert false
             | `Absent -> (acc, range) |> k
    *)

##register [opacapi;restricted:dbgen,cps-bypass] set: transaction, path, data.d, continuation(transaction) -> void
let set tr path x k =
  tr.tr_engine.E.write tr.tr path (Badop.Set (D.query x))
  @> function
  | Badop.Set (D.Response resp) -> { tr with tr = resp } |> k
  | _ -> assert false

##register [opacapi;restricted:dbgen,cps-bypass] clear: transaction, path, continuation(transaction) -> void
let clear tr path k =
  tr.tr_engine.E.write tr.tr path (Badop.Clear (D.query ()))
  @> function
  | Badop.Clear (D.Response resp) -> { tr with tr = resp } |> k
  | _ -> assert false

##register [opacapi;restricted:dbgen,cps-bypass] remove_children: transaction, path, continuation(transaction) -> void
  (* Useful when we want to clean up a node before rewriting it (eg for sums, or
     maps), but don't want to remove the node itself because that would clear a link
     there *)
let remove_children tr path k =
  tr.tr_engine.E.read tr.tr path (Badop.Children (D.query (None,0)))
  @> function
  | `Answer (Badop.Children (D.Response children)) ->
      C.fold_list clear tr children @> k
  | `Answer _ | `Linkto _ -> assert false
  | `Absent -> tr |> k


##register [opacapi;restricted:dbgen,cps-bypass] set_link: transaction, path, path, continuation(transaction) -> void
let set_link tr writeto_path pointto_path k =
  tr.tr_engine.E.write tr.tr writeto_path (Badop.Link (D.query pointto_path))
  @> function
  | Badop.Link (D.Response resp) -> { tr with tr = resp } |> k
  | _ -> assert false

##register [opacapi;restricted:dbgen,cps-bypass] set_current_copy: transaction, path, path, continuation(transaction) -> void
let set_current_copy tr writeto_path pointto_path k =
  tr.tr_engine.E.write tr.tr writeto_path (Badop.Copy (D.query (pointto_path, None)))
  @> function
  | Badop.Copy (D.Response resp) -> { tr with tr = resp } |> k
  | _ -> assert false

(* ------------------------------------------------------------ *)
(* These functions are for access to hidden internal data in    *)
(* the database, outside of DbGen.                              *)
(* ------------------------------------------------------------ *)

(* -- DISABLED FOR NOW --
%
%let restricted_data_path =
%  let k = match Dblib.Key.int 3 with Dblib.Success k -> k | _ -> assert false in
%    (* specification of the key in DbGen_private.ml *)
%  Dblib.Path.add Dblib.Path.root k
%
%register [restricted] set_restricted : string -> string -> unit
%let set_restricted key value =
%  let db = get_global_database () in
%  let tr = match Dblib.Trans.start db with
%  | Dblib.Success tr -> tr
%  | Dblib.Failure s -> failwith s in
%  let tr = Dblib.Trans.set tr restricted_data_path (DataImpl.data_binary value) in
%  match Dblib.Trans.execute db tr
%  with Dblib.Success () -> () | Dblib.Failure _ -> failwith "Unable to set internal data"
%
%register [restricted] get_restricted_first : string -> string option
%let get_restricted_first key =
%  let db = get_global_database () in
%  let tr = match Dblib.Trans.start db with
%  | Dblib.Success tr -> tr
%  | Dblib.Failure s -> failwith s in
%  let v =
%    match Dblib.Trans.get_all_rev_of_path tr restricted_data_path with
%      | Dblib.Success (rev::_) ->
%          (match Dblib.Trans.get db tr ~rev restricted_data_path with
%             | Dblib.Success v -> Some (DataImpl.obj_binary v)
%             | Dblib.Failure _ -> None)
%      | _ -> None in
%  let _ = Dblib.Trans.execute db tr in
%  v
%
%register [restricted] get_restricted_last : string -> string option
%let get_restricted_last key =
%  let db = get_global_database () in
%  let tr = match Dblib.Trans.start db with
%  | Dblib.Success tr -> tr
%  | Dblib.Failure s -> failwith s in
%  let v = match Dblib.Trans.get db tr restricted_data_path with
%    | Dblib.Success v -> Some (DataImpl.obj_binary v)
%    | Dblib.Failure _ -> None in
%  let _ = Dblib.Trans.execute db tr in
%  v
%
*)

(* just a get at the right (hidden) location in the bd ; this one is allowed for debug outside of dbGen *)
##register [cps-bypass] get_raw_schema : database, continuation(string) -> void
let get_raw_schema db k =
  db.db_engine.E.tr_start db.db
    (fun exc ->
       Logger.error "Database connection error before OPA runtime initialisation: %s" (Printexc.to_string exc);
       exit 3)
  @> fun tr ->
    (* Path /2/0 to schema defined in dbGen_private.ml (/2 is config_keys, config key 0 is schema) *)
    db.db_engine.E.read tr (Badop.Path.of_list [ key_int 2; key_int 0 ]) (Badop.Contents (D.query ()))
    @> function
    | `Answer (Badop.Contents (D.Response sch)) ->
        db.db_engine.E.tr_prepare tr
        @> (fun k (tr, success) ->
              assert success; db.db_engine.E.tr_commit tr k)
        @> (fun k success ->
              assert success; Data.obj_binary sch |> k)
        @> k
    | `Answer _ | `Linkto _ -> assert false
    | `Absent -> "get_raw_schema: data not found" |> k

(** Used  to determine if it should give up or save the upgrade information
    in the database and continue when there has been changes in the schema. *)
##register [opacapi;restricted:dbgen] shall_i_upgrade : database -> int
let shall_i_upgrade db = (* returns C-style boolean *)
  #<If:DBGEN_ALWAYS_UPGRADE> 1 #<Else>
  if db.db_engine.E.options.E.force_upgrade then 1 else 0
  #<End>


(* -- Registration in hashtables -- *)

(* This is needed for separate compilation, for stuff that will only be
   generated in the `init stage but is accessed by all packages (that is
   to say, we generate their usage before we can actually generate them) ;
   for that, `init stores in a hashtable, and packages use get_registered_xxx
   to get the identifier back.
*)

let registered_db_idents = ((Hashtbl.create 12) : (string, Obj.t) Hashtbl.t)

##register [opacapi;restricted:dbgen] get_registered_db_ident: string -> 'a
let get_registered_db_ident dbname =
  try Obj.obj (Hashtbl.find registered_db_idents dbname)
  with Not_found as e ->
    Printf.printf "Cannot find db %s\n%!" dbname;
    raise e

##register [opacapi;restricted:dbgen] register_db_ident: string, 'a -> void
let register_db_ident dbname dbident =
  Hashtbl.add registered_db_idents dbname (Obj.repr dbident)

(* register for each root edge, the corresponding edgeid in the global original schema
   root edges are denoted by a package name and a local edgeid in its package *)
let registered_root_edge_map = ((Hashtbl.create 48) : ((string * string * int), int) Hashtbl.t)

##register [opacapi;restricted:dbgen] register_root_edge: string, string, int, int -> void
let register_root_edge db_name package_name local_edgeid global_matching_edgeid =
  Hashtbl.add registered_root_edge_map (db_name, package_name, local_edgeid) global_matching_edgeid

(* get the matching edge of a root edge in the original database schema *)
##register [opacapi] get_registered_root_edge : string, string, int -> int
let get_registered_root_edge db_name package_name local_edgeid2 =
 try Hashtbl.find registered_root_edge_map (db_name, package_name, local_edgeid2)
 with Not_found as e ->
   Printf.printf "Cannot find edge %d of package %s\n%!" local_edgeid2 package_name;
   raise e
