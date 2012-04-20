(*
    Copyright © 2011, 2012 MLstate

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
(* ================================================================ *)
(** Low-level transaction handling for the database              -- *)
(* ================================================================ *)
module BslNativeLib = Badop_engine.BslNativeLib
module BslUtils = Badop_engine.BslUtils

module B = Badoplink
module C = QmlCpsServerLib
module D = Badoplink.D
module E = Badop_engine

open C.Ops (* This file follows the duck-style cps guidelines © *)

##extern-type [normalize] status = \
  | Active of (Badop_engine.t * Badoplink.transaction) list \
  | Aborted
(*| Busy of (status C.continuation) Queue.t;
   *todo*: to prevent
   race-condition if ever two threads use the same transaction
   simultaneously (at the time being, they would override each
   other's transaction and write operations may be lost) *)

(* A sub-transaction is a transaction started within another one ;
   it shares the status reference of its parent *)
##extern-type [normalize] t = { \
  status: status ref; \
  sub: bool; \
}

let get_opt: 'a C.continuation ->  t option =
  fun k ->
    match C.transaction_context k with
    | None -> None
    | Some obj -> Some (Obj.obj obj : t)

let set: 'a C.continuation -> t -> 'a C.continuation =
  fun k t ->
    C.with_transaction_context (Obj.repr t) k

let rec update_tr db tr = function
  | (db',_tr')::r when db' == db -> (db,tr)::r
  | x::r -> x::update_tr db tr r
  | [] -> [db,tr]

##register abort: t -> void

let abort t = match !(t.status) with
  | Active trs ->
      List.iter (fun (_db,tr) -> tr.B.tr_engine.E.tr_abort tr.B.tr @> fun () -> ()) trs;
      t.status := Aborted;
  | Aborted ->
      #<If:DBGEN_DEBUG> Logger.notice "Transaction aborted twice" #<End>

##register [opacapi;restricted:dbgen,cps-bypass] set_global_transaction: \
      Badoplink.database, Badoplink.transaction, continuation(opa[void]) -> void

let set_global_transaction db tr k =
  match get_opt k with
  | None ->
      #<If:DBGEN_DEBUG> Logger.error "Set global transaction without context..." #<End>;
      ServerLib.void |> set k {
        status = ref (Active [ db.B.db_engine,tr ]);
        sub = false;
      };
  | Some { status = { contents = Active trs } as status; _ } ->
      status := Active (update_tr db.B.db_engine tr trs);
      ServerLib.void |> k
  | Some { status = { contents = Aborted }; _ } ->
      Logger.error "'set transaction' within a broken transaction context, this shouldn't happen";
      ServerLib.void |> k

##register [opacapi;restricted:dbgen,cps-bypass] get_global_transaction_opt: \
    Badoplink.database, continuation(opa[option(Badoplink.transaction)]) -> void

let get_db_transaction db k =
  match get_opt k with
  | None -> None |> k
  | Some ({ status = { contents = Active trs }; _ } as t) -> (
      match Base.List.assq_opt db.B.db_engine trs with
      | Some tr ->
          Some tr |> k
      | None ->
          db.B.db_engine.E.tr_start db.B.db
            (fun _exc ->
               #<If:DBGEN_DEBUG> Logger.error "get_gl_trans/fail: %s" (Printexc.to_string _exc) #<End>;
               abort t)
          @> fun tr ->
            let tr = { B. tr_engine = db.B.db_engine; tr = tr } in
            t.status := Active (update_tr db.B.db_engine tr trs);
            Some tr |> k
    )
  | Some { status = { contents = Aborted }; _ } ->
      Logger.error "'get transaction' within a broken transaction context, this shouldn't happen";
      None |> k

let get_global_transaction_opt db k =
  get_db_transaction db @> C.ccont_ml k @> fun opt -> ServerLib.wrap_option opt |> k

##register [cps-bypass] init: t,opa[list(Badoplink.database)],continuation(opa[void]) -> void
let init t dbs k = match !(t.status) with
  | Aborted -> ServerLib.void |> k
  | Active trs ->
      C.iter_list
        (fun db k ->
           match Base.List.assq_opt db.B.db_engine trs with
           | Some _ -> () |> k
           | None ->
               db.B.db_engine.E.tr_start db.B.db
                 (fun _exc ->
                    #<If:DBGEN_DEBUG> Logger.error "init_hl_trans/fail: %s" (Printexc.to_string _exc) #<End>;
                    abort t)
               @> fun tr ->
                 match !(t.status) with
                 | Active trs ->
                     (* The call case in opa stdlib should guarantee this race condition doesn't happen *)
                     assert (not (List.exists (fun (db',_) -> db' == db.B.db_engine) trs));
                     let tr = { B. tr_engine = db.B.db_engine; tr = tr } in
                     t.status := Active ((db.B.db_engine,tr) :: trs);
                     () |> k
                 | Aborted -> () |> k)
        (Badop_engine.opa_list_to_ocaml_list (fun db -> db) dbs)
      @> C.ccont_ml k
      @> fun () -> ServerLib.void |> k

##register [opacapi;cps-bypass] fail: continuation('a) -> void

let fail k = Badoplink.transaction_fail_exception |> C.handler_cont k

##register [opacapi;cps-bypass] start: continuation(t) -> void

let start k =
  match get_opt k with
  | None ->
      { status = ref (Active []); sub = false } |> k
  | Some t ->
      { status = t.status; sub = true } |> k

##register [opacapi;cps-bypass] continue: \
    t, (continuation('a) -> void), (continuation('a) -> void), continuation('a) -> void

let continue t f errh k =
  match get_opt k with
  | Some { status; _ } when status == t.status ->
      (* we already are in that transaction (or a parent) *)
      f @> set k t (* in that case, do not catch exceptions *)
  | _ ->
      match !(t.status) with
      | Aborted -> errh @> k
      | Active _ ->
          let k =
            C.catch_ml
              (fun exc k ->
                 abort t;
                 if ServerLib.compare exc B.transaction_fail_exception <> 0 then
                   exc |> C.handler_cont k
                 else
                   errh @> k)
              k
          in
          f @> set k t

##register [opacapi;cps-bypass] commit: t, continuation(opa[outcome(void,void)]) -> void

let opa_success : (ServerLib.ty_void, ServerLib.ty_void) Badop_engine.opa_outcome =
  let fld = ServerLib.static_field_of_name "success" in
  let fields = ServerLib.empty_record_constructor in
  let fields = ServerLib.add_field fields fld ServerLib.void in
  let record = ServerLib.make_record fields in
  Badop_engine.wrap_opa_outcome record
let opa_failure : (ServerLib.ty_void, ServerLib.ty_void) Badop_engine.opa_outcome =
  let fld = ServerLib.static_field_of_name "failure" in
  let fields = ServerLib.empty_record_constructor in
  let fields = ServerLib.add_field fields fld ServerLib.void in
  let record = ServerLib.make_record fields in
  Badop_engine.wrap_opa_outcome record

let opa_outcome b = if b then opa_success else opa_failure

let commit t k =
  let fin_success = fun success ->
    if success then
      (t.status := Active []; opa_success |> k)
    else
      (t.status := Aborted; opa_failure |> k)
  in
  match t with
  | { sub = true; _ } ->
      opa_outcome (!(t.status) <> Aborted) |> k
  | { status = { contents = Aborted }; _ } ->
      opa_failure |> k
  | { status = { contents = Active [] }; _ } ->
      #<If:DBGEN_DEBUG> Logger.notice "Committing empty transaction" #<End>;
      opa_success |> k
  | { status = { contents = Active [_db,tr] }; _ } -> (* Simple version for single transaction *)
      tr.B.tr_engine.E.tr_prepare tr.B.tr
      @> fun (ptr,success) ->
        if success then
          tr.B.tr_engine.E.tr_commit ptr @> fin_success
        else
          (t.status := Aborted;
           opa_failure |> k)
  | { status = { contents = Active trs }; _ } -> (* Two-phase commit for multiple databases *)
      QmlCpsServerLib.map_list
        (fun (db,tr) k ->
           tr.B.tr_engine.E.tr_prepare tr.B.tr
           @> fun (tr,success) -> ({ B. tr_engine = db; tr = tr }, success) |> k)
        trs
      @> C.ccont_ml k
      @> fun prepare_result ->
        let trs,successes = List.split prepare_result in
        if Base.List.for_all (fun x -> x) successes then
          QmlCpsServerLib.map_list
            (fun tr k -> tr.B.tr_engine.E.tr_commit tr.B.tr @> fun success -> success |> k)
            trs
          @> C.ccont_ml k
          @> fun successes ->
            fin_success (Base.List.for_all (fun x -> x) successes)
        else
          (List.iter
             (fun (tr,success) ->
                if not success then tr.B.tr_engine.E.tr_abort tr.B.tr @> fun () -> ())
             prepare_result;
           t.status := Aborted;
           opa_failure |> k)
