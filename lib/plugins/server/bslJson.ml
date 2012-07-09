(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
##opa-type RPC.Json.json
##extern-type RPC.Json.private.native = JsonTypes.json
##extern-type ll_json_list_repr = JsonTypes.json (*RPC.Json.private.native*) list
##extern-type ll_json_record_repr = (string * JsonTypes.json (*RPC.Json.private.native*)) list

##module Json

let field_int   = ServerLib.static_field_of_name "Int"
let field_float = ServerLib.static_field_of_name "Float"
let field_bool  = ServerLib.static_field_of_name "Bool"
let field_string= ServerLib.static_field_of_name "String"
let field_list  = ServerLib.static_field_of_name "List"
let field_record= ServerLib.static_field_of_name "Record"
let field_fst   = ServerLib.static_field_of_name "f1"
let field_snd   = ServerLib.static_field_of_name "f2"
let field_hd    = ServerLib.static_field_of_name "hd"
let field_tl    = ServerLib.static_field_of_name "tl"
let field_nil   = ServerLib.static_field_of_name "nil"
let make_int    x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor field_int x)
let make_float  x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor field_float x)
let make_bool   x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor field_bool (ServerLib.wrap_bool x))
let make_string x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor field_string x)
let make_list   x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor field_list x)
let make_record x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor field_record x)
let shared_nil    = ServerLib.make_simple_record field_nil
let shared_void   = make_record shared_nil
let make_nil ()   = shared_nil
let make_pair x y = ServerLib.make_record (ServerLib.add_field (ServerLib.add_field ServerLib.empty_record_constructor field_fst x) field_snd y)

(**
 * This part concerns translation of a OPA Json object to/from Json
 * implementation in OCaml. It's defined on libqml/libqml/jsonTypes.ml.
 *)


##register of_json_repr: RPC.Json.private.native -> option(RPC.Json.json)
  let  of_json_repr js =
      let rec aux  = function
      | JsonTypes.Int i   -> make_int   i
      | JsonTypes.Float f -> make_float f
      | JsonTypes.Bool b  -> make_bool  b
      | JsonTypes.Void    -> shared_void
      | JsonTypes.String s-> make_string s
      | JsonTypes.Array l ->
          make_list (
            List.fold_right (fun x acc ->
                               let cons = ServerLib.empty_record_constructor        in
                               let cons = ServerLib.add_field cons field_hd (aux x) in
                               let cons = ServerLib.add_field cons field_tl acc     in
                               ServerLib.make_record cons
                            )
              l shared_nil
          )
      | JsonTypes.Record r ->
          make_record (List.fold_right (fun (x,y) acc ->
                                          let cons = ServerLib.empty_record_constructor in
                                          let cons = ServerLib.add_field cons field_hd (make_pair x (aux y)) in
                                          let cons = ServerLib.add_field cons field_tl acc in
                                          ServerLib.make_record cons
                                       )
                         r shared_nil
                      )
      in
      Some (wrap_opa_rpc_json_json (aux js))

##register of_string: string -> option(RPC.Json.json)
  let of_string s = match JsonLex.transform true s with
    | None   -> None
    | Some x -> of_json_repr x

##register of_latin1_string: string -> option(RPC.Json.json)
  let of_latin1_string s = match JsonLex.transform false s with
    | None   -> None
    | Some x -> of_json_repr x


##register json_list_empty : -> ll_json_list_repr
  let json_list_empty () = []

##register json_list_cons : RPC.Json.private.native, ll_json_list_repr -> ll_json_list_repr
  let json_list_cons a b = a::b

##register json_record_empty : -> ll_json_record_repr
  let json_record_empty () = []

##register json_record_cons : string, RPC.Json.private.native, ll_json_record_repr -> ll_json_record_repr
  let json_record_cons s b r = (s,b)::r


##register json_repr_int : int -> RPC.Json.private.native
  let json_repr_int i  = JsonTypes.Int i

##register json_repr_float : float -> RPC.Json.private.native
  let json_repr_float f = JsonTypes.Float f

##register json_repr_string : string -> RPC.Json.private.native
  let json_repr_string s = JsonTypes.String s

##register json_repr_bool : bool -> RPC.Json.private.native
  let json_repr_bool b = JsonTypes.Bool b

##register json_repr_array : ll_json_list_repr -> RPC.Json.private.native
  let json_repr_array lst = JsonTypes.Array lst

##register json_repr_record : ll_json_record_repr -> RPC.Json.private.native
  let json_repr_record lst = JsonTypes.Record (List.rev lst)



##endmodule
