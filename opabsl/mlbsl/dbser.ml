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
(* DB-serialisation: dump-recovery of the database *)

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
