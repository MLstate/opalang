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
module CL = CryptoLib
module C = Cryptokit

type license_version = int
type license_id = string
type customer_info_data = string StringMap.t
type product_bool_info_data = bool StringMap.t
type product_int_info_data = int StringMap.t
type license = { (* 'a license ? *)
  license_version : license_version;
  license_id : license_id;
  customer_info_data : customer_info_data;
  product_bool_info_data : product_bool_info_data;
  product_int_info_data : product_int_info_data;
  license_signature : string;
}

exception InvalidLicense

let get_customer_info license_opt key default = try
  let license = Option.get license_opt in
  StringMap.find key license.customer_info_data
with Failure "Option.get" | Not_found -> default

let get_product_bool_info license_opt key default = try
  let license = Option.get license_opt in
  StringMap.find key license.product_bool_info_data
with Failure "Option.get" | Not_found  -> default

let get_product_int_info license_opt key default = try
  let license = Option.get license_opt in
  StringMap.find key license.product_int_info_data
with Failure "Option.get" | Not_found  -> default

let make_license license_version license_id customer_info_data product_bool_info_data product_int_info_data signature = {
  license_version = license_version;
  license_id = license_id;
  customer_info_data = customer_info_data;
  product_bool_info_data = product_bool_info_data;
  product_int_info_data = product_int_info_data;
  license_signature = signature;
}

let serialize_license license =
  let ci_data = StringMap.fold (
    fun k v acc ->
      Printf.sprintf "%s=%s,%s" k (CryptoLib.to_base64 v) acc
  ) license.customer_info_data "" in
  let pbi_data = StringMap.fold (
    fun k v acc ->
      Printf.sprintf "%s=%s,%s" k (string_of_bool v) acc
  ) license.product_bool_info_data "" in
  let pii_data = StringMap.fold (
    fun k v acc ->
      Printf.sprintf "%s=%s,%s" k (string_of_int v) acc
  ) license.product_int_info_data "" in
  let data = Printf.sprintf "%d;%s;%s;%s;%s" license.license_version license.license_id ci_data pbi_data pii_data in
  data

let sign_serialized_license data sym_key rsa_sig_privkey=
  let to_sign = CL.hash data in
  let signature = CL.to_hex (CL.sign rsa_sig_privkey to_sign) in
  let final = Printf.sprintf "%s\n%s" data signature in
  CL.to_base64_m (CL.symetric_encrypt (CL.from_hex sym_key) final)

let check_license_data license_data sym_key rsa_sig_pubkey = try
  let license_content = CL.symetric_decrypt (CL.from_hex sym_key) (CL.from_base64 license_data) in
  let p1, p2 =
    let l = Base.String.slice '\n' license_content in
    if List.length l = 2 then
      let p1 = List.nth l 0
      and p2 = List.nth l 1 in
      p1, p2
    else raise InvalidLicense in
  if CL.hash p1 = Base.String.ltrim ~is_space:((=) '\000') (CL.unsign rsa_sig_pubkey (CL.from_hex p2)) then Some (p1, p2)
  else None
with C.Error _ | InvalidLicense -> None

let recover_license serialized_license signature =
  let l = Base.String.slice ';' serialized_license in
  if List.length l = 5 then
    let lv = List.nth l 0
    and lid = List.nth l 1
    and ci = List.nth l 2
    and pbi = List.nth l 3
    and pii = List.nth l 4 in
    let cl = Base.String.slice ',' ci
    and pbl = Base.String.slice ',' pbi
    and pil = Base.String.slice ',' pii in
    let customer_info_data =
      List.fold_left (
        fun acc elt ->
          let kv = Base.String.slice '=' elt in
          let k = List.nth kv 0
          and v = (CryptoLib.from_base64 (List.nth kv 1)) in
          StringMap.add k v acc
      ) StringMap.empty cl
    and product_bool_info_data =
      List.fold_left (
        fun acc elt ->
          let kv = Base.String.slice '=' elt in
          let k = List.nth kv 0
          and v = List.nth kv 1 in
          StringMap.add k (bool_of_string v) acc
      ) StringMap.empty pbl
    and product_int_info_data =
      List.fold_left (
        fun acc elt ->
          let kv = Base.String.slice '=' elt in
          let k = List.nth kv 0
          and v = List.nth kv 1 in
          StringMap.add k (int_of_string v) acc
      ) StringMap.empty pil in
    make_license (int_of_string lv) lid customer_info_data product_bool_info_data product_int_info_data signature
  else raise InvalidLicense

let system_uuid () = try
  let sysprof = "/usr/sbin/system_profiler" in
  if Sys.file_exists sysprof then
    let ic = Unix.open_process_in (Printf.sprintf "%s -detailLevel full SPHardwareDataType | grep UUID" sysprof) in
    let huuid = input_line ic in
    let l = Base.String.slice ':' huuid in
    let uuid = Base.String.trim (List.nth l 1) in
    uuid
  else ""
with End_of_file | Invalid_argument "List.nth" | Failure "nth" -> ""
