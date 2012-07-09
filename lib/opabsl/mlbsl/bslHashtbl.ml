(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

##extern-type Hashtbl.t('key, 'value) = ('key, 'value) Hashtbl.t

##opa-type Hashtbl.binding('key, 'value)

let fkey = ServerLib.static_field_of_name "key"
let fval = ServerLib.static_field_of_name "value"
let build_binding key val_ =
  let r = ServerLib.empty_record_constructor in
  let r = ServerLib.add_field r fkey key in
  let r = ServerLib.add_field r fval val_ in
  wrap_opa_hashtbl_binding (ServerLib.make_record r)

##register make : ('key -> string), ('key, 'key -> bool), int -> Hashtbl.t('key, 'value)
let make _hash _equals size = (* TODO *)
  Hashtbl.create size

##register create \ `Hashtbl.create` : int -> Hashtbl.t('key, 'value)

##register clear \ `Hashtbl.clear` : Hashtbl.t('key, 'value) -> void

##register add \ `Hashtbl.add` : Hashtbl.t('key, 'value), opa['key], opa['value] -> void

##register replace \ `Hashtbl.replace` : Hashtbl.t('key, 'value), opa['key], opa['value] -> void

##register try_find : Hashtbl.t('key, 'value), opa['key] -> option(opa['value])
let try_find h k = try Some (Hashtbl.find h k) with Not_found -> None

##register remove \ `Hashtbl.remove` : Hashtbl.t('key, 'value), opa['key] -> void

##register size \ `Hashtbl.length`: Hashtbl.t('key, 'value) -> int

##register mem \ `Hashtbl.mem` : Hashtbl.t('key, 'value), opa['key] -> bool

##register bindings : Hashtbl.t('key, 'value) -> llarray(opa[Hashtbl.binding('key, 'value)])
let bindings (table : ('key, 'value) Hashtbl.t) =
  let fake = build_binding 0 0 in
  let size = Hashtbl.length table in
  if size = 0 then LowLevelArray.create 0 fake
  else
    let res = LowLevelArray.create size fake in
    let _ = Hashtbl.fold
      (fun key val_ i ->
         LowLevelArray.set res i ((build_binding key val_) : ('key, 'value) opa_hashtbl_binding);
         i+1)
      table 0
    in res

