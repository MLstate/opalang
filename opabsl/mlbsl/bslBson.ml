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
##opa-type RPC.Bson.bson
##extern-type bson_buf = Bson.buf

##module Bson

##register dump: int, string -> string
let dump base s =
  let bb = Buffer.create 1024 in
  let bh = Buffer.create 1024 in
  let ba = Buffer.create 1024 in
  let len = String.length s in
  let m, n = len / base, len mod base in
  for i = 0 to m do
    let row = i * base in
    for j = 0 to (if i = m then n-1 else base-1) do
      let idx = i * base + j in
      let code = Char.code s.[idx] in
      Printf.bprintf bh "%02x " code;
      Printf.bprintf ba "%c" (if code >= 32 && code < 127 then s.[idx] else '.');
      if j = base-1 || (i = m && j = n-1)
      then
        (if base = 10
         then Printf.bprintf bb "%04d %-30s %-10s\n" row (Buffer.contents bh) (Buffer.contents ba)
         else Printf.bprintf bb "%04x %-48s %-16s\n" row (Buffer.contents bh) (Buffer.contents ba);
         Buffer.clear bh; Buffer.clear ba)
    done
  done;
  Buffer.contents bb

##register new_oid: void -> string
let new_oid () = Bson.Oid.gen ()

##register oid_of_string: string -> string
let oid_of_string = Bson.Oid.from_string

##register oid_to_string: string -> string
let oid_to_string = Bson.Oid.to_string

##register serializeb: RPC.Bson.bson, bson_buf -> void
let serializeb bsons b =
  let rec aux bsons =
    let rec aux3 r =
      match ServerLib.dot r BslNativeLib.field_hd with
      | None -> ()
      | Some bson ->
          ServerLib.fold_record
            (fun f v () ->
               match ServerLib.name_of_field f with
               | Some "Double" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.double b key (ServerLib.unwrap_float value))
               | Some "String" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.string b key (ServerLib.unwrap_string value))
               | Some "Document" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) ->
                        Bson.Append.start_object b key;
                        aux (unwrap_opa_rpc_bson_bson value);
                        Bson.Append.finish_object b)
               | Some "Array" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) ->
                        Bson.Append.start_array b key;
                        aux (unwrap_opa_rpc_bson_bson value);
                        Bson.Append.finish_array b)
               | Some "Binary" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) ->
                        let bin = ServerLib.unwrap_string value in
                        Bson.Append.binary b key Bson.st_bin_binary bin (String.length bin))
               | Some "ObjectID" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.oid b key (ServerLib.unwrap_string value))
               | Some "Boolean" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.bool b key (ServerLib.unwrap_bool value))
               | Some "Date" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.date b key (ServerLib.unwrap_int value))
               | Some "Null" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, _) -> Bson.Append.null b key)
               | Some "Regexp" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) ->
                        (match BslNativeLib.ocaml_tuple_2 value with
                         | (regexp, regexp_opts) ->
                             Bson.Append.regex b key (ServerLib.unwrap_string regexp) (ServerLib.unwrap_string regexp_opts)))
               | Some "Code" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.code b key (ServerLib.unwrap_string value))
               | Some "Symbol" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.symbol b key (ServerLib.unwrap_string value))
               | Some "CodeScope" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) ->
                        (match BslNativeLib.ocaml_tuple_2 value with
                         | (code, scope) ->
                             Bson.Append.start_codewscope b key code;
                             aux scope;
                             Bson.Append.finish_codewscope b code))
               | Some "Int32" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.int b key (ServerLib.unwrap_int value))
               | Some "Timestamp" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) ->
                        (match BslNativeLib.ocaml_tuple_2 value with
                         | (i, t) ->
                             Bson.Append.timestamp b key ((ServerLib.unwrap_int i), (ServerLib.unwrap_int t))))
               | Some "Int64" ->
                   (match BslNativeLib.ocaml_tuple_2 v with
                    | (key, value) -> Bson.Append.long b key (ServerLib.unwrap_int value))
               | Some str ->
                   Printf.eprintf "Unknown code: %s\n%!" str;
                   assert false
               | None ->
                   assert false)
            bson ();
          aux3 (ServerLib.unsafe_dot r BslNativeLib.field_tl)
    in
    aux3 bsons
  in
  aux (unwrap_opa_rpc_bson_bson bsons)

##register serialize: int, RPC.Bson.bson -> bson_buf
let serialize hint bsons =
  let b = Bson.Append.init ~hint () in
  serializeb bsons b;
  Bson.Append.finish b;
  b

##register get: bson_buf -> string
let get b = Bson.Append.get b

let field_fst       = ServerLib.static_field_of_name "f1"
let field_snd       = ServerLib.static_field_of_name "f2"
let field_hd        = ServerLib.static_field_of_name "hd"
let field_tl        = ServerLib.static_field_of_name "tl"
let field_nil       = ServerLib.static_field_of_name "nil"
let field_int32     = ServerLib.static_field_of_name "Int32"
let field_int64     = ServerLib.static_field_of_name "Int64"
let field_double    = ServerLib.static_field_of_name "Double"
let field_bool      = ServerLib.static_field_of_name "Boolean"
let field_string    = ServerLib.static_field_of_name "String"
let field_document  = ServerLib.static_field_of_name "Document"
let field_null      = ServerLib.static_field_of_name "Null"
let field_array     = ServerLib.static_field_of_name "Array"
let field_binary    = ServerLib.static_field_of_name "Binary"
let field_objectid  = ServerLib.static_field_of_name "ObjectID"
let field_date      = ServerLib.static_field_of_name "Date"
let field_regexp    = ServerLib.static_field_of_name "Regexp"
let field_code      = ServerLib.static_field_of_name "Code"
let field_symbol    = ServerLib.static_field_of_name "Symbol"
let field_codescope = ServerLib.static_field_of_name "CodeScope"
let field_timestamp = ServerLib.static_field_of_name "Timestamp"

let shared_nil     = ServerLib.make_simple_record field_nil
let make_pair x y =
  ServerLib.make_record (ServerLib.add_field (ServerLib.add_field ServerLib.empty_record_constructor field_fst x) field_snd y)
let make_cons hd tl =
  ServerLib.make_record (ServerLib.add_field (ServerLib.add_field ServerLib.empty_record_constructor field_hd hd) field_tl tl)

let make_val fld n x =
  ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor fld (make_pair n x))

let make_null = make_val field_null
let make_int32 = make_val field_int32
let make_int64 = make_val field_int64
let make_double = make_val field_double
let make_bool n x = make_val field_bool n (ServerLib.wrap_bool x)
let make_string = make_val field_string
let make_document = make_val field_document
let make_array = make_val field_array
let make_binary = make_val field_binary
let make_objectid = make_val field_objectid
let make_date = make_val field_date
let make_regexp n r ro = make_val field_regexp n (make_pair r ro)
let make_code = make_val field_code
let make_symbol = make_val field_symbol
let make_codescope n c s = make_val field_codescope n (make_pair c s)
let make_timestamp n (i,t) = make_val field_timestamp n (make_pair i t)

let null_value = make_null "<missing value>" ServerLib.void

##register deserialize: string -> RPC.Bson.bson
let deserialize str =
  let i = Bson.Iterator.from_buffer str in
  let rec aux i =
    (function
     | c when c = Bson.el_eoo -> shared_nil
     | c when c = Bson.el_int -> let e = make_int32 (Bson.Iterator.key i) (Bson.Iterator.int i) in auxn e i
     | c when c = Bson.el_long -> let e = make_int64 (Bson.Iterator.key i) (Bson.Iterator.long i) in auxn e i
     | c when c = Bson.el_double -> let e = make_double (Bson.Iterator.key i) (Bson.Iterator.double i) in auxn e i
     | c when c = Bson.el_bool -> let e = make_bool (Bson.Iterator.key i) (Bson.Iterator.bool i) in auxn e i
     | c when c = Bson.el_string -> let e = make_string (Bson.Iterator.key i) (Bson.Iterator.string i) in auxn e i
     | c when c = Bson.el_object ->
         let key =  Bson.Iterator.key i in
         let ii = Bson.Iterator.subiterator i in
         let bsons = aux ii (Bson.Iterator.next ii) in
         let e = make_document key bsons in auxn e i
     | c when c = Bson.el_array ->
         let key = Bson.Iterator.key i in
         let ii = Bson.Iterator.subiterator i in
         let bsons = aux ii (Bson.Iterator.next ii) in
         let e = make_array key bsons in auxn e i
     | c when c = Bson.el_bindata -> let e = make_binary (Bson.Iterator.key i) (Bson.Iterator.bin_data i) in auxn e i
     | c when c = Bson.el_oid -> let e = make_objectid (Bson.Iterator.key i) (Bson.Iterator.oid i) in auxn e i
     | c when c = Bson.el_date -> let e = make_date (Bson.Iterator.key i) (Bson.Iterator.date i) in auxn e i
     | c when c = Bson.el_null -> let e = make_null (Bson.Iterator.key i) ServerLib.void in auxn e i
     | c when c = Bson.el_regex ->
         let e = make_regexp (Bson.Iterator.key i) (Bson.Iterator.regex i) (Bson.Iterator.regex_opts i) in auxn e i
     | c when c = Bson.el_code -> let e = make_code (Bson.Iterator.key i) (Bson.Iterator.code i) in auxn e i
     | c when c = Bson.el_symbol -> let e = make_symbol (Bson.Iterator.key i) (Bson.Iterator.symbol i) in auxn e i
     | c when c = Bson.el_codewscope ->
         let key = Bson.Iterator.key i in
         let code = Bson.Iterator.code i in
         let b = Bson.Iterator.code_scope i in
         let ii = Bson.Iterator.init b in
         let scope = aux ii (Bson.Iterator.next ii) in
         let e = make_codescope key code scope in auxn e i
     | c when c = Bson.el_timestamp -> let e = make_timestamp (Bson.Iterator.key i) (Bson.Iterator.timestamp i) in auxn e i
     | c ->
         Printf.eprintf "Unknown Bson code: %c\n%!" c;
         assert false)
  and auxn e i = make_cons (wrap_opa_rpc_bson_bson e) (aux i (Bson.Iterator.next i))
  in
  wrap_opa_rpc_bson_bson (aux i (Bson.Iterator.next i))

##endmodule
