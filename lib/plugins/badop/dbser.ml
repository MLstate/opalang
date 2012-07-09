(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
