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
##extern-type mongo_buf = Mongo.mongo_buf
##extern-type cursorID = int64
##extern-type reply = Buf.buf

##module Mongo

##register create: int -> mongo_buf
let create = Mongo.create

(* AAAAAAARGHHHHHHH!!!!!! OPA just can't equate opa_rpc_bson_bson with ServerLib.ty_record *)

##register insert: mongo_buf, int, string, 'a -> void
let insert m f ns (bson:'a) =
  let (bson:BslBson.opa_rpc_bson_bson) = Obj.magic bson in
  Mongo.start_insert m 0l f ns;
  Mongo.bson_init m;
  BslBson.Bson.serializeb bson m;
  Mongo.bson_finish m;
  Mongo.finish m

##register update: mongo_buf, int, string, 'a, 'a -> void
let update m flags ns selector update =
  let (selector:BslBson.opa_rpc_bson_bson) = Obj.magic selector in
  let (update:BslBson.opa_rpc_bson_bson) = Obj.magic update in
  Mongo.start_update m 0l flags ns;
  Mongo.bson_init m;
  BslBson.Bson.serializeb selector m;
  Mongo.bson_finish m;
  Mongo.bson_init m;
  BslBson.Bson.serializeb update m;
  Mongo.bson_finish m;
  Mongo.finish m

##register query: mongo_buf, int, string, int, int, 'a, option('a) -> void
let query m flags ns numberToSkip numberToReturn query returnFieldSelector_opt =
  let (query:BslBson.opa_rpc_bson_bson) = Obj.magic query in
  let (returnFieldSelector_opt:BslBson.opa_rpc_bson_bson option) = Obj.magic returnFieldSelector_opt in
  Mongo.start_query m 0l flags ns numberToSkip numberToReturn;
  Mongo.bson_init m;
  BslBson.Bson.serializeb query m;
  Mongo.bson_finish m;
  (match returnFieldSelector_opt with
   | Some returnFieldSelector ->
       Mongo.bson_init m;
       BslBson.Bson.serializeb returnFieldSelector m;
       Mongo.bson_finish m
   | None -> ());
  Mongo.finish m

##register get_more: mongo_buf, string, int, cursorID -> void
let get_more m ns numberToReturn cursorID =
  Mongo.start_getmore m 0l ns numberToReturn cursorID;
  Mongo.finish m

##register delete: mongo_buf, int, string, 'a -> void
let delete m flags ns selector =
  let (selector:BslBson.opa_rpc_bson_bson) = Obj.magic selector in
  Mongo.start_delete m 0l flags ns;
  Mongo.bson_init m;
  BslBson.Bson.serializeb selector m;
  Mongo.bson_finish m;
  Mongo.finish m

##register kill_cursors: mongo_buf, list(cursorID) -> void
let kill_cursors m clist =
  Mongo.start_kill_cursors m 0l (BslNativeLib.opa_list_to_ocaml_list (fun x -> x) clist);
  Mongo.finish m

##register msg: mongo_buf, string -> void
let msg m msg =
  Mongo.start_msg m 0l msg;
  Mongo.finish m

##register get: mongo_buf -> string
let get = Mongo.get

##register export: mongo_buf -> opa[tuple_2(string, int)]
let export m =
  let (str, i) = Mongo.export m in
  BslNativeLib.opa_tuple_2 (ServerLib.wrap_string str, ServerLib.wrap_int i)

##register clear: mongo_buf -> void
let clear = Mongo.clear

##register reset: mongo_buf -> void
let reset = Mongo.reset

module C = QmlCpsServerLib
open C.Ops

##register [cps-bypass] read_mongo : Socket.connection, int, continuation(reply) -> void
let read_mongo conn size k =
  let buf = Buf.create size in
  let rec aux (len, str) =
    Buf.append buf str len;
    let len = Buf.length buf in
    if len < 4
    then Scheduler.read Scheduler.default conn aux
    else
      if len < Stuff.StuffString.ldi32 buf.Buf.str 0
      then Scheduler.read Scheduler.default conn aux
      else buf |> k
  in
  Scheduler.read Scheduler.default conn aux

##endmodule
