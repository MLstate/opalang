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
(** {6 From Ocaml Standard Library} *)

external code : char -> int = "%identity"
val chr : int -> char
val escaped : char -> string
val lowercase : char -> char
val uppercase : char -> char
type t = char
val compare : t -> t -> int
external unsafe_chr : int -> char = "%identity"

(** {6 Extra API} *)

val equal_insensitive : char -> char -> bool
val compare_insensitive : char -> char -> int
val width : char -> int
val is_digit : char -> bool
val is_lower : char -> bool
val is_upper : char -> bool
val is_alpha : char -> bool
(* val pred : char -> char *)
(* val succ : char -> char *)

(**
   A space char : {[ ' ' ; '\t' ; '\r' ; '\n' ]}
*)
val is_space : char -> bool
val hexa_value : char -> int
