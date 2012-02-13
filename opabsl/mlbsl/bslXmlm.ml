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
(**
   Binding for Xmlm
*)

(**
   Current status:
   * only parsing
   * no option supported
*)

##opa-type Xmlm.signal

let field_data = ServerLib.static_field_of_name "Data"
let field_dtd = ServerLib.static_field_of_name "Dtd"
let field_el_end = ServerLib.static_field_of_name "El_end"
let field_el_start = ServerLib.static_field_of_name "El_start"
let field_namespace = ServerLib.static_field_of_name "namespace"
let field_name = ServerLib.static_field_of_name "name"
let field_value = ServerLib.static_field_of_name "value"
let field_tag = ServerLib.static_field_of_name "tag"
let field_args = ServerLib.static_field_of_name "args"
let field_content = ServerLib.static_field_of_name "content"
let field_specific_attributes = ServerLib.static_field_of_name "specific_attributes"
let field_text = ServerLib.static_field_of_name "text"

let field_fragment = ServerLib.static_field_of_name "fragment"

let handle_attribute ((namespace, name), value) =
  let acc = ServerLib.empty_record_constructor in
  let acc = ServerLib.add_field acc field_namespace namespace in
  let acc = ServerLib.add_field acc field_name name in
  let acc = ServerLib.add_field acc field_value value in
  ServerLib.make_record acc

##register make_scanner: string -> (-> option(opa[Xmlm.signal]))
let make_scanner input =
  let input = Xmlm.make_input ?enc:None ~strip:true ?ns:None ?entity:None (`String (0, input)) in
  fun () ->
    try
      let r =
        match Xmlm.input input with
        | `Data s -> 
            let r = ServerLib.empty_record_constructor in
            let r = ServerLib.add_field r field_data s in
            ServerLib.make_record r
        | `Dtd s -> 
            let r = ServerLib.empty_record_constructor in
            let r = ServerLib.add_field r field_dtd s in
            ServerLib.make_record r
        | `El_end -> 
            ServerLib.make_simple_record field_el_end
        | `El_start ((namespace, tag), args) ->
            let args = BslNativeLib.caml_list_to_opa_list handle_attribute args in
            let r = ServerLib.empty_record_constructor in
            let r = ServerLib.add_field r field_el_start ServerLib.void in
            let r = ServerLib.add_field r field_namespace namespace in
            let r = ServerLib.add_field r field_tag tag in
            let r = ServerLib.add_field r field_args args in
            ServerLib.make_record r
      in
      Some (wrap_opa_xmlm_signal r)
    with 
    | Xmlm.Error (_, _) -> None

##opa-type xmlns

let handle_tag ((uri, local), attributes) children = 
  let acc = ServerLib.empty_record_constructor in
  let acc = ServerLib.add_field acc field_tag local in
  let acc = ServerLib.add_field acc field_namespace uri in
  let args = BslNativeLib.caml_list_to_opa_list handle_attribute attributes in
  let acc = ServerLib.add_field acc field_args args in
  let acc = ServerLib.add_field acc field_content (BslNativeLib.caml_list_to_opa_list Base.identity children) in
  let acc = ServerLib.add_field acc field_specific_attributes ServerLib.none in
  ServerLib.make_record acc

let handle_text_node str = 
  let acc = ServerLib.empty_record_constructor in
  let acc = ServerLib.add_field acc field_text str in
  ServerLib.make_record acc

let handle_fragment caml_list_xmlns = 
  let acc = ServerLib.empty_record_constructor in
  let list_opa_xmlns = (BslNativeLib.caml_list_to_opa_list Base.identity caml_list_xmlns) in
  let acc = ServerLib.add_field acc field_fragment list_opa_xmlns in
  ServerLib.make_record acc
   
##register parse_tree : string -> option(opa[xmlns])
let parse_tree input = 
  let entity_fun(el) = match el with 
  | "lt" -> Some("&lt;")
  | "gt" -> Some("&gt;")
  | "nbsp" -> Some("&nbsp;")
  | "amp" -> Some("&amp;")
  | "apos" -> Some("&apos;")
  | "quot" -> Some("&quot;")
  | _ -> None
  in
  let input = Xmlm.make_input ?enc:None ~strip:false ?ns:None ~entity:entity_fun (`String (0, input)) in
  try
    let rec parse(current_list) = 
      let (_, r) = Xmlm.input_doc_tree ~el:handle_tag ~data:handle_text_node input in
      let new_list = BaseList.cons r current_list in
      if Xmlm.eoi input
        then new_list
        else parse(new_list) 
    in

    match parse([]) with
      | [] -> None
      | [xmlns] -> Some (wrap_opa_xmlns xmlns)
      | list_xmlns -> 
          let list_xmlns = BaseList.rev list_xmlns in
          let fragment = handle_fragment list_xmlns in
          Some (wrap_opa_xmlns fragment)
  with
  | Xmlm.Error (_, _) -> None
  | Invalid_argument _ -> None

