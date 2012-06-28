(*
    Copyright © 2011, 2012 MLstate

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
   Abstraction on backend languages : "ml" / "c" / "js"

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(** The extension corresponding to the language *)
type ext = string

(** The abstract type of a back-end language *)
type t
type format = [ `compiled | `interpreted ]
val compare : t -> t -> int

(** building a value of type [Language.t]. If the string is not recognized, the function returns [None] *)
val of_string : ?format:format -> string -> t option
val to_string : t -> string
val pp : t LangPrint.pprinter
val pp_list : t list LangPrint.pprinter

(** if you don't want to do a map + verification of unicity *)
val of_list : string list -> t list option

(** Get a list of language from a string. use the syntax : ["{ml, js, c}"] *)
val parse : string -> t list option

(** Get a printable representation of a list of languages. e.g. "[{ml, js, c}]" *)
val print : t list -> string

(** Force the format of a language.*)
val formate : format -> t -> t

(** {6 Compiled}*)

val ml : t (** Traditional ocaml code *)
val js : t (** JavaScript code, for use on the client (technically interpreted, but not by us) *)
val c  : t (** C code, for use with e.g. LLVM *)
val nodejs : t (** JavaScript code, for use on the server with NodeJS *)

(** {6 Sugar} *)

val is_ml : t -> bool
val is_js : t -> bool
val is_c  : t -> bool
val is_nodejs  : t -> bool

(**{6 Interpreted}*)

val mli : t (** Traditional ocaml code, to be used in an interpreter (opatop) *)

(** {6 Code generation} *)
(**
   In the generated plugins and loaders, there are reference to languages.
   {[
   L.ml, L.mli, L.js, etc...
   ]}
   The generated code should contains
   {[
   module L = BslLanguag
   ]}
*)
val pp_meta : t LangPrint.pprinter
