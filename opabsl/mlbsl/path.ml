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
(** Database paths handling (high-level, low level ones are in Badoplink) *)

(* TODO: move all operations using the engine directly to badoplink *)

module B = Badoplink
module C = QmlCpsServerLib
module D = Badop.Dialog
module E = Badop_engine

open C.Ops (* This file follows the duck-style cps guidelines © *)

(* Phantom types for the 'kind below, helping discrimination among val paths and ref paths *)
type val_p_
##extern-type [normalize] val_p = val_p_
type ref_p_
##extern-type [normalize] ref_p = ref_p_

(* These types are that of the hidden data, only visible from here and used through Serverlib.*lazy_data* *)
##extern-type [normalize] path_contents('a) =  \
    Path_val of  Badoplink.transaction         \
  | Path_ref of (Badoplink.database) *         \
      ( Badoplink.transaction -> 'a ->  Badoplink.transaction QmlCpsServerLib.continuation -> unit) (* writer *)

(* todo: add to this record:
 children : Badoplink.key -> ('kind,'b) path; -- or stg like that, for path composition
 check : trans -> bool -- would be useful to have the same as ?/path with 1c paths *)
##extern-type [normalize] t('kind, 'a) = { \
  path: Badoplink.path ; \
  reader:  Badoplink.transaction -> 'a QmlCpsServerLib.continuation -> unit ; \
  kind: ('a) path_contents ; \
}

(* Used for hidden embedded data in records: at first we put the current transaction, then
   when accessed it's checked and may be turned into a revision if there is one that holds
   the expected data. *)
##extern-type [normalize] trans_or_rev = Transaction of Badoplink.transaction | Revision of Badop_engine.t * Badoplink.revision

##extern-type [normalize] embed_info = { embedded_path: Badoplink.path; embedded_transaction: trans_or_rev }

##extern-type [normalize] embedded_obj = [ `path of Obj.t ] ServerLib.ty_info


let ( @* ) f g x = f(g(x))

(* -- Some conversion helpers -- *)

let nil_record = ServerLib.make_simple_record (BslNativeLib.field_nil)
let qml_nil () =
  BslNativeLib.wrap_opa_list nil_record

let qml_cons x l =
  let record =
    ServerLib.make_record
      (ServerLib.add_field
         (ServerLib.add_field ServerLib.empty_record_constructor BslNativeLib.field_hd x)
         BslNativeLib.field_tl l)
  in BslNativeLib.wrap_opa_list record

let qml_pair x y =
  let record =
    ServerLib.make_record
      (ServerLib.add_field
         (ServerLib.add_field ServerLib.empty_record_constructor
            BslNativeLib.f1 x)
         BslNativeLib.f2 y)
  in BslNativeLib.wrap_opa_tuple_2 record

(* We need to handle map types (with values passed to dbgen) *)
##opa-type map('keys,'values)


(* -- Internal functions and bypasses -- *)

(* Invariant: never returns an info containing a running Transaction (in the type trans_or_rev):
   transactions are resolved to revisions *)
##register[opacapi;restricted:dbgen,cps-bypass] get_lazy_info_opt: 'record, continuation(opa[option(embed_info)]) -> void
let get_lazy_info_opt r k =
  let record = Obj.magic r in
  match ServerLib.get_lazy_data_opt record with
    | Some (`path p) ->
        let embed_info = (Obj.obj p:  embed_info) in
        (match embed_info.embedded_transaction with
         | Revision _ -> ServerLib.wrap_option(Some embed_info) |> k
         | Transaction tr ->
             (* validate the lazy info wrt the current state of the db *)
             tr.B.tr_engine.E.read tr.B.tr embed_info.embedded_path (Badop.Stat (D.query ()))
             @> function
             | `Answer (Badop.Stat (D.Response (_path, Some rev, _kind))) ->
                 (* The info is valid and bound to revision rev ; update the lazy data *)
                 let embed_info = { embed_info with embedded_transaction = Revision (tr.B.tr_engine,rev) } in
                 ignore (ServerLib.inject_lazy_data record (Some (`path (Obj.repr embed_info))));
                 ServerLib.wrap_option (Some embed_info) |> k
             | _ -> (* The info is not valid (doesn't belong to a committed rev) *)
                 ignore (ServerLib.inject_lazy_data record None); (* invalidate it *)
                 (* note: if we manage to implement post-precognition, the data may be invalid but
                    become valid later. Then, we should not invalidate if the transaction is still
                    alive and not yet committed. *)
                ServerLib.wrap_option(None) |> k)
    | _ ->
        ServerLib.wrap_option(None) |> k

(** Adds lazy-data to an existing record. Do not mistake for the directive that
    builds a lazy record, this is a run-time add. *)
##register [opacapi;restricted:dbgen] embed_record_data: 'a, option(embedded_obj) -> 'a
let embed_record_data record data =
  Obj.magic (ServerLib.embed_lazy_data (Obj.magic record) (Obj.magic data))

(** Injects lazy-data into an existing record, as a side-effect.
    Used for maximal sharing *)
##register[opacapi;restricted:dbgen] inject_record_data: 'a, option(embedded_obj) -> void
let inject_record_data record data =
  ignore (ServerLib.inject_lazy_data (Obj.magic record) (Option.map Obj.magic data))

(* Used from the QML side to embed hidden data *)
##register [opacapi;restricted:dbgen] embedded_path: Badoplink.transaction, Badoplink.path -> embedded_obj
let embedded_path' tr llpath =
  { embedded_path = llpath; embedded_transaction = Transaction tr }
let embedded_path tr llpath =
  `path (Obj.repr (embedded_path' tr llpath))

##register [opacapi;restricted:dbgen,cps-bypass] get_ref_path: \
    Badoplink.database, \
    Badoplink.path, \
    (Badoplink.transaction, continuation('a) -> void), \
    (Badoplink.transaction, 'a, continuation(Badoplink.transaction) -> void), \
    continuation(t( ref_p, 'a)) -> \
    void
let get_ref_path db path reader writer k =
  {
    path = path;
    reader = reader;
    kind = Path_ref (db, writer);
  } |> k

##register [opacapi;restricted:dbgen,cps-bypass] get_val_path: \
   Badoplink.transaction, \
   Badoplink.path, \
   (Badoplink.transaction, continuation('a) -> void), \
   continuation(t( val_p, 'a)) -> \
   void
let get_val_path tr path reader k =
  {
    path = path;
    reader = reader;
    kind = Path_val tr;
  } |> k

let get_trans t k =
  match t with
    | { kind = Path_val tr } -> tr |> k
    | { kind = Path_ref (db,_) } ->
        Opa_transaction.get_db_transaction db
        @> C.ccont_ml k
        @> function
        | Some tr -> tr |> k
        | None -> Badoplink.trans_start db @> k

(* -- User bypasses -- *)

##register[cps-bypass] read: t( 'kind, 'a), continuation('a) -> void
let read t k = match t with
  | { reader = reader; kind = Path_val tr } -> reader tr @> k
  | { reader = reader; kind = Path_ref _ } ->
      get_trans t @> C.ccont_ml k
      @> fun tr -> reader tr @> k

##register[cps-bypass] ref_to_val: t( ref_p, 'a), continuation(t( val_p, 'a)) -> void
let ref_to_val t k =
  get_trans t @> C.ccont_ml k
  @> fun tr -> {
    path = t.path;
    reader = t.reader;
    kind = Path_val tr
  } |> k

let fold_range key2val val2key f acc start end_opt filter t k =
  let range = Some (val2key start), match end_opt with None -> 0 | Some x -> x - 1 in
  get_trans t @> C.ccont_ml k
  @> fun tr ->
    tr.B.tr_engine.E.read tr.B.tr t.path (Badop.Children (D.query range))
    @> function
    | `Answer (Badop.Children (D.Response children)) ->
        C.fold_list
          (fun acc path k ->
             key2val (Badop.Path.last path) @> C.ccont_ml k
             @> fun key ->
               filter key @> C.ccont_ml k
             @> fun opa_bool ->
               ServerLib.unwrap_bool opa_bool |> C.ccont_ml k
                 @> function
                 | true -> f acc key @> k
                 | false -> acc |> k)
          acc children
        @> k
    | `Answer _ | `Linkto _ -> assert false
    | `Absent -> acc |> k

##register[cps-bypass] intmap_fold_range: \
    t('kind,opa[map(int,'a)]), \
    ('acc, int, continuation('acc) -> void), \
    'acc, \
    int, \
    option(int), \
    (int, continuation(opa[bool]) -> void), \
    continuation('acc) -> \
    void
(* FIXME: optimise with help from the low-level (too many reads) *)
let intmap_fold_range t f acc start end_opt filter k =
  fold_range Badoplink.key_value_int Badoplink.key_int f acc start end_opt filter t @> k

##register[cps-bypass] stringmap_fold_range: \
    t('kind,opa[map(string,'a)]), \
    ('acc, string, continuation('acc) -> void), \
    'acc, \
    string, \
    option(int), \
    (string, continuation(opa[bool]) -> void), \
    continuation('acc) -> \
    void
let stringmap_fold_range t f acc start end_opt filter k =
  fold_range Badoplink.key_value_string Badoplink.key_string f acc start end_opt filter t @> k

let search key2val words t k =
  let words = Cactutf.lowercase words in
  let words = Base.String.slice_chars " \t,." words in
  let words = List.map Base.String.trim words in
  let words = List.filter ((<>) "") words in
  get_trans t @> C.ccont_ml k
  @> fun tr ->
    tr.B.tr_engine.E.read tr.B.tr t.path (Badop.Search (D.query (words, (None,0))))
    @> function
    | `Answer (Badop.Search (D.Response keylist)) ->
        C.fold_list
          (fun acc key k -> key2val key @> C.ccont_ml k @> fun key -> qml_cons key acc |> k)
          (qml_nil()) (List.rev keylist)
        @> k
    | `Answer _ | `Linkto _ -> assert false
    | `Absent -> qml_nil() |> k

##register[cps-bypass] intmap_search: t(ref_p,opa[map(int,'a)]), string, continuation(opa[list(int)]) -> void
let intmap_search t words k =
  search Badoplink.key_value_int words t @> k

##register[cps-bypass] stringmap_search: t(ref_p,opa[map(string,'a)]), string, continuation(opa[list(string)]) -> void
let stringmap_search t words k =
  search Badoplink.key_value_string words t @> k

##register[cps-bypass] fresh_key: t('kind,opa[map(int,'a)]), continuation(int) -> void
let fresh_key t k =
  get_trans t @> C.ccont_ml k
  @> fun tr -> Badoplink.get_new_key tr t.path @> k

(* type trans = Badoplink.Dblib.trans *)

let do_in_trans
    (db:  Badoplink.database)
    (f:  Badoplink.transaction -> ( Badoplink.transaction) QmlCpsServerLib.continuation -> unit)
    (k: 'opavoid QmlCpsServerLib.continuation) =
  Opa_transaction.get_db_transaction db
  @> C.ccont_ml k
  @> function
  | Some tr ->
      f tr
      @> C.ccont_ml k
      @> fun tr -> Opa_transaction.set_global_transaction db tr @> k
  | None ->
      B.trans_start db @> C.ccont_ml k
      @> fun tr ->
        f tr
        @> C.ccont_ml k
        @> fun tr ->
          B.trans_commit tr @> k

##register [opacapi;restricted:dbgen,cps-bypass] copy: Badoplink.transaction, embed_info, Badoplink.path, continuation(Badoplink.transaction) -> void
let copy tr embed dbpath k =
  match embed.embedded_transaction with
  | Transaction _ ->
      Badoplink.error "Bad internal copy query from a transaction" @> k
        (* this should have been resolved to a Revision or discarded by get_lazy_data_opt *)
  | Revision (engine,revision) when engine == tr.B.tr_engine ->
      (tr.B.tr_engine.E.write tr.B.tr dbpath (Badop.Copy (D.query (embed.embedded_path, Some revision)))
       @> function
       | Badop.Copy (D.Response resp) -> { tr with Badoplink.tr = resp } |> k
       | _ -> assert false)
  | _ ->
      Badoplink.error "Error: attempt to share between databases; fixme" @> k

##register[cps-bypass] write: t( ref_p, 'a), 'a, continuation(opa[void]) -> void
let write t x k = match t with
  | { kind = Path_val _; _ } -> assert false (* should be guaranteed by typing *)
  | { kind = Path_ref (db, writer); path = _path; _ } ->
      #<If:DBGEN_DEBUG>
        Printf.eprintf "[35m[dbGen][0m Write to path %s (%s)\n" (Badop.Path.to_string _path) (DebugPrint.print x)
      #<End>;
      do_in_trans db (fun tr k -> writer tr x @> k) @> k

##register[cps-bypass] remove: t( ref_p, 'a), continuation(opa[void]) -> void
let remove t k = match t with
  | { kind = Path_val _ } -> assert false (* should be guaranteed by typing *)
  | { path = path; kind = Path_ref (db, _) } ->
      do_in_trans db (fun tr -> Badoplink.clear tr path) @> k

##register[cps-bypass] exists: t( 'kind, 'a), continuation(opa[bool]) -> void
let exists t k =
  get_trans t @> C.ccont_ml k
  @> fun tr ->
    tr.B.tr_engine.E.read tr.B.tr t.path (Badop.Stat (D.query ()))
    @> function
    | `Answer (Badop.Stat (D.Response _)) -> ServerLib.wrap_bool true |> k
    | `Answer _ | `Linkto _ -> assert false
    | `Absent -> ServerLib.wrap_bool false |> k

let trans_start_rev db rev k =
  db.B.db_engine.E.tr_start_at_revision db.B.db rev
    (fun _exc -> B.abort_transaction k)
  @> fun tr ->
    { B.tr_engine = db.B.db_engine; B.tr = tr } |> k

##register[cps-bypass] history: t( ref_p, 'a), int, int, continuation(opa[list('a)]) -> void
(** pos len describe a range in history: pos gives the first rev (strictly
    positive is from the origin of time with 1 the first rev, negative from
    now, 0 is the last rev), len gives the max number of revisions wanted
    (strictly positive is forward from pos, negative backwards). If len=0,
    returns all revs from pos to the end, so (0 0) is the full reverse history
    from now, (1 0) is the history from the origin of time
*)
let history t pos len k = match t with
  | { kind = Path_val _ } -> assert false (* should be guaranteed by typing *)
  | { path = path; reader = reader; kind = Path_ref (db, _) } ->
      (fun k ->
         Opa_transaction.get_db_transaction db
         @> C.ccont_ml k
         @> function
         | Some tr -> tr |> k
         | None -> Badoplink.trans_start db @> k)
      @> C.ccont_ml k
      @> fun tr ->
        let rec skip n = function _::r when n > 0 -> skip (n-1) r | l -> l in
        let range, post_filter =
          if pos >= 1 then
            if len >= 0 then (None, pos - 1 + len), List.rev @* skip (pos - 1)
            else (None, pos), skip (pos + len)
          else
            if len = 0 then (None, 0), List.rev @* skip (- pos) @* List.rev
            else if len > 0 then (None, pos - 1), skip (- pos - len + 1)
            else (None, pos + len), List.rev @* skip (- pos)
        in
        Badoplink.revisions tr path (let first,count = range in first,count,None)
        @> C.ccont_ml k
        @> fun revisions ->
          C.fold_list
            (fun acc (rev, _time) k ->
               trans_start_rev db rev @> C.ccont_ml k
               @> fun tr ->
                 reader tr @> C.ccont_ml k
                 @> fun x ->
                   qml_cons x acc |> k)
            (qml_nil())
            (post_filter revisions)
          @> k

##register[cps-bypass] history_time: t(ref_p, 'a), time_t, time_t, continuation(opa[list(tuple_2('a,time_t))]) -> void
(* todo optimise with a low-level filter: now it gets all revs before filtering *)
let history_time t from until k = match t with
  | { kind = Path_val _ } -> assert false (* should be guaranteed by typing *)
  | { path = path; reader = reader; kind = Path_ref (db, _) } ->
      (fun k ->
         Opa_transaction.get_db_transaction db
         @> C.ccont_ml k
         @> function
         | Some tr -> tr |> k
         | None -> Badoplink.trans_start db @> k)
      @> C.ccont_ml k
      @> fun tr ->
            let rec skip = function
              | (_rev,ts)::r as l -> if ts < Time.milliseconds from then skip r else l
              | [] -> [] in
            let rec cut = function
              | (rev,ts)::r -> if ts <= Time.milliseconds until then (rev,ts) :: cut r else []
              | [] -> [] in
            let post_filter revisions = List.rev (cut (skip revisions)) in
            Badoplink.revisions tr path (None, 0, None)
            @> C.ccont_ml k
            @> fun revisions ->
              C.fold_list
              (fun acc (rev,time) k ->
                 trans_start_rev db rev @> C.ccont_ml k
                 @> fun tr ->
                   reader tr @> C.ccont_ml k
                   @> fun x ->
                     qml_cons (qml_pair x time) acc |> k)
              (qml_nil())
              (post_filter revisions)
            @> k

##register[cps-bypass] modification_time: t('a, 'kind), continuation(time_t) -> void
let modification_time t k =
  get_trans t @> C.ccont_ml k
  @> fun tr ->
    tr.B.tr_engine.E.read tr.B.tr t.path (Badop.Revisions (D.query (None, -1)))
    @> function
    | `Answer (Badop.Revisions (D.Response [_rev, timestamp])) ->
        Time.in_milliseconds timestamp |> k
    | `Answer (Badop.Revisions (D.Response [])) | `Absent ->
        0 |> k
    | `Answer _ | `Linkto _ -> assert false
