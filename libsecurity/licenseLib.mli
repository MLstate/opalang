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
(*
  Mathieu
  Fri Jan 28 11:34:30 CET 2011
  FIXME
  What kind of documentation is that ??

  1) Lot's of these function raise some undocumented exception
  2) not any comment but a hazardous one
*)

(* val rsa_sig_privkey : Cryptokit.RSA.key *)
(* val rsa_sig_pubkey : Cryptokit.RSA.key *)
(* val sym_key : string *)

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

val get_customer_info : license option -> string -> string -> string

val get_product_bool_info : license option -> string -> bool -> bool

val get_product_int_info : license option -> string -> int -> int

val make_license : license_version -> license_id -> customer_info_data -> product_bool_info_data -> product_int_info_data -> string -> license

val serialize_license : license -> string

val sign_serialized_license : string -> string -> Cryptokit.RSA.key -> string

val check_license_data : string -> string -> Cryptokit.RSA.key -> (string * string) option

val recover_license : string -> string -> license

val system_uuid : unit -> string
