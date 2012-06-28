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

(**
   Keys for manipulating bypass in the bsl.

   Since the apparition of the bsl, we use keys instead of row code for bypassing qml compilers by adding some primitives
   form target languages (ocaml, js, llvm). Primitives need to be registred using the application {b bslregister},
   which produces some plugins containing the definition of bypass, indexed by their {b keys}.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(** {6 Guidelines} *)

(**
   Whenever you use a value of type [BslKey.t] in your code,
   you should use [key] or [bslkey] as variable identifier.

   When you are manipulating a key after a [to_string] manipulation,
   you should use [skey] as variable identifier.

   {[
   match expr with
   | Bypass key ->
       let skey = BslKey.to_string key in
        .....
   ]}

*)

(** {6 Standard utilisation} *)

(** The abstract type of bsl keys.*)
type t

(** comparing 2 keys. returned value as usual compare functions *)
val compare : t -> t -> int

(** Convert a key to a printable representation *)
external to_string : t -> string = "%identity"

val pp : t LangPrint.pprinter

(** Normalization of key in the Bsl : lowercase, and replace . with '_' *)
val normalize : string -> t
val normalize_string : string -> string

(** {6 Internal utilisation} *)
(**
   Not for casual users.

   Since every bypass is normalized, if you transform directly
   a string into a key, you may not found a bypass because
   the unnormalized string does not correspond to the normalized one.

   Essentially, this function is used whenever you know that the
   string has already be normalized.
*)
external of_string : string -> t = "%identity"

(** {6 Hash} *)

val equal : t -> t -> bool

val hash : t -> int
