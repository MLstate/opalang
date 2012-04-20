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
(* DB-serialisation: dump-recovery of the database *)
module BslNativeLib = Badop_engine.BslNativeLib
module C = QmlCpsServerLib
module D = Badop.Dialog
module E = Badop_engine

open C.Ops (* This file follows the duck-style cps guidelines © *)

module DbSerializer = Xml_dump.F(
  struct
    type 'a continuation = 'a C.continuation
    let mkcont = C.ccont_ml
    let return = (|>)
  end
)

##register [cps-bypass] dump_db : Badoplink.database, string, continuation(opa[void]) -> void
let dump_db db file k =
  Badoplink.trans_start db @> C.ccont_ml k
  @> fun tr ->
    let db_read = tr.Badoplink.tr_engine.E.read tr.Badoplink.tr in
    DbSerializer.to_file db_read file @> C.ccont_ml k
    @> fun () -> ServerLib.void |> k
