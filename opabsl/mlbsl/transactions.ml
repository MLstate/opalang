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
(* ================================================================ *)
(** Low-level transaction handling for the database              -- *)
(* ================================================================ *)

module B = Badoplink
module C = QmlCpsServerLib
module D = Badoplink.D
module E = Badop_engine

open C.Ops (* This file follows the duck-style cps guidelines © *)

##property [mli]
##extern-type [normalize] transaction_status = Active | Aborted (* | Finished (?) *)

##extern-type [normalize] monadic_trans('a) = {                 \
  database: Badoplink.database ;                                \
  badoplink_tr: Badoplink.transaction ;                         \
  status: transaction_status ;                                  \
  fallback: unit -> 'a QmlCpsServerLib.continuation -> unit ;   \
  (* == (unit, 'a) C.func *)                                    \
  embedded: bool;                                               \
  value: 'a;                                                    \
}

##extern-type [normalize] t('a) = ('a) monadic_trans
##property [endmli]

type trans_in_context = {
  mutable db_tr_map: (B.database * B.transaction) list
}

let get_transaction: 'a C.continuation ->  B.database ->  B.transaction option =
  fun k db ->
    match C.transaction_context k with
    | None -> None
    | Some obj ->
        let context = (Obj.obj obj : trans_in_context) in
        try
          let db = (Obj.magic (db:  B.database): B.database) in
          let tr = List.assq db context.db_tr_map in
          Some (Obj.magic (tr:  B.transaction):  B.transaction)
        with Not_found -> None

let set_transaction: 'a C.continuation ->  B.database ->  B.transaction -> 'a C.continuation =
  fun k db tr ->
    let db = (Obj.magic (db:  B.database):  B.database) in
    let tr = (Obj.magic (tr:  B.transaction):  B.transaction) in
    match C.transaction_context k with
    | None ->
        let context = { db_tr_map = [db,tr] } in
        C.with_transaction_context (Obj.repr context) k
    | Some obj ->
        let context = (Obj.obj obj: trans_in_context) in
        context.db_tr_map <- (db, tr) :: List.remove_assq db context.db_tr_map;
        k

##register [opacapi;restricted:dbgen,cps-bypass] get_global_transaction_opt: Badoplink.database, continuation(opa[option(Badoplink.transaction)]) -> void
let get_global_transaction_opt db k =
  ServerLib.wrap_option (get_transaction k db) |> k

##register [opacapi;restricted:dbgen,cps-bypass] set_global_transaction: Badoplink.database, Badoplink.transaction, continuation(opa[void]) -> void
let set_global_transaction db tr k =
  ServerLib.void |> set_transaction k db tr

##register [opacapi;cps-bypass] fail: Badoplink.database, string, 'a, continuation('a) -> void
let fail db s ret k =
  match get_transaction k db with
  | Some _ -> Badoplink.transaction_fail_exception () |> C.handler_cont k
  | None ->
      Logger.warning "Database operation silently aborted (outside of a transaction). Reason: %s" s;
      ret |> k

##register [opacapi;cps-bypass] start: Badoplink.database, 'a, continuation(t('a)) -> void
let start db x k =
  (fun k -> match get_transaction k db with
   | Some tr -> (true, tr) |> k
   | None -> B.trans_start db @> C.ccont_ml k @> fun tr -> (false, tr) |> k)
  @> C.ccont_ml k
  @> fun (embedded, tr) ->
    {
      database = db;
      badoplink_tr = tr;
      status = Active;
      fallback = (fun () k -> x |> k);
      embedded = embedded;
      value = x;
    } |> k

##register[cps-bypass] abort: t('a), continuation('a) -> void
let abort htr k = match htr.status with
  | Aborted -> htr.value |> k
  | Active ->
      htr.badoplink_tr.Badoplink.tr_engine.Badop_engine.tr_abort htr.badoplink_tr.Badoplink.tr
      @> fun () -> htr.fallback () @> k

##register [opacapi;cps-bypass] commit: t('a), continuation('a) -> void
let commit htr k =
  match htr.status with
    | Aborted -> htr.value |> k
    | Active ->
        if htr.embedded then htr.value |> k
        else
          htr.badoplink_tr.Badoplink.tr_engine.Badop_engine.tr_prepare htr.badoplink_tr.Badoplink.tr
          @> (fun k (tr, success) ->
                match success with
                | true ->
                    htr.badoplink_tr.Badoplink.tr_engine.Badop_engine.tr_commit
                      tr k
                | false -> k false)
          @> (fun k success ->
                match success with
                | true -> htr.value |> k
                | false -> htr.fallback () @> k)
          @> k

##register [opacapi;cps-bypass] continue: t('a), ('a, continuation('b) -> void), ('a, continuation('b) -> void), continuation(t('b)) -> void
let continue htr f fallback k =
  let abort_trans htr k =
    htr.fallback () @> C.ccont_ml k
    @> fun a ->
      fallback a @> C.ccont_ml k
      @> fun value -> {
        htr with
          status = Aborted;
          value = value;
          fallback = fun () k -> value |> k
      } |> k
  in
  match htr.status with
    | Aborted ->
        abort_trans htr @> k
    | Active ->
        match get_transaction k htr.database with
        | None ->
            let k = (* Set the handler inside the continuation *)
              C.catch_ml
                (fun exc k -> (* Handler *)
                   Logger.debug "Transaction aborted";
                   htr.badoplink_tr.B.tr_engine.E.tr_abort htr.badoplink_tr.B.tr
                   @> fun () ->
                     abort_trans htr @> C.ccont_ml k
                     @> fun htr ->
                       if ServerLib.compare exc (B.transaction_fail_exception()) <> 0 then
                         htr.fallback () @> C.ccont_ml k
                         @> fun _ -> exc |> C.handler_cont k (* re-raise *)
                       else
                         htr |> k)
                k
            in
            let k = (* Then set the context inside the continuation *)
              set_transaction k htr.database htr.badoplink_tr
            in
            f htr.value @> C.ccont_ml k
            @> (fun value ->
                  match get_transaction k htr.database with
                  | None ->
                      Logger.error "Internal inconsistency: transaction disappeared from context";
                      abort_trans htr @> k
                  | Some badoplink_tr -> {
                      htr with
                        value = value;
                        badoplink_tr = badoplink_tr;
                        fallback = fun () k -> htr.fallback () @> C.ccont_ml k @> fun a -> fallback a @> k;
                    } |> k)
        | Some badoplink_tr ->
            if not htr.embedded || badoplink_tr != htr.badoplink_tr
            then
              (Logger.error "Transaction mismatch (trying to continue a transaction within an incompatible one)";
               abort_trans htr @> k)
            else
              f htr.value @> C.ccont_ml k
              @> fun value -> {
                htr with
                  value = value;
                  fallback = fun () k -> htr.fallback () @> C.ccont_ml k @> fun a -> fallback a @> k;
              } |> k

(* This somehow breaks the above design... which is deprecated where it's
   used, ie transaction controllers using sessions in OPA *)
##register get_value: t('a) -> 'a
let get_value htr = htr.value
