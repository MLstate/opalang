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
   MLstate common utils.
   @author everybody\@mlstate
*)

(** {6 Guidelines} *)

(**
   Top level :

   Base contains at top-level all functions that we feel missing in Ocaml Stdlib Pervasives.

   Keep in mide that people does 'open Base', so be kind with your collegues
   Do not put in base thinks like :

   {[let print_endline _ = Sys.command "rm -rf $HOME"]}

   Modules :

   We use Base to redefine some module when they have an implementation
   in Ocaml stdlib, to avoid module name conflicts.

   Be carrefull, note that the ocaml manual does not say what happen in case of :
   {[
   open Base
(* use module List e.g. *)
   ]}
   So, use [include] in your module, but
   DO NOT CHANGE THE IMPLEMENTATION OF AN EXISTING OCAML STDLIB FUNCTION

   You should DOCUMENT and ORGANIZE your code : base.mli

   If your module is not implemented in Ocaml stdlib, then
   DO NOT PUT IN BASE A MODULE IN THIS CASE.
   You should put it in a new file, with its own mli.

   This last point is not yet respected in base, this is until we clean this up.
   Any time soon, Base will no longuer be a Bad Big Blob.

   Updating, adding code :

   If the module has a unit test in reftester, please update it, and add complete
   tests for your new modules and functions.

   Thanks !
*)

(** *)

(** {6 Pervasives type definitions} *)
type ('a,'b) either = Left of 'a | Right of 'b

(** {6 Meta values} *)
(** *)

val copyright : string

val is_windows : bool

(** {6 Miscellaneous} *)

external identity : 'a -> 'a = "%identity"
val intmax : int -> int -> int
val intmin : int -> int -> int
val compare_int : int -> int -> int
external format_float : string -> float -> string = "caml_format_float"
val int_of_string_opt : string -> int option
val max3 : 'a -> 'a -> 'a -> 'a

(** round a float with precision [10^(-i)] *)
val round : int -> float -> float

(** windows stuff : \\r\\n *)
val crlf : string

(** same as [failwith (Printf.sprintf ...)] *)
val failwithf : ('params,unit,string,'exn) format4 -> 'params
(** same as [invalid_arg (Printf.sprintf ...)] *)
val invalid_argf : ('params,unit,string,'exn) format4 -> 'params


(** {6 Debug Get Env} *)

(**
   Deprecated, and not working function (dummy implementation).
   Please use pp-debug.
   @see "DebugVariables" for a documentation of pp-debug.
*)
(** *)
val debug_getenv: string -> string -> string
val debug_getenv_toggle: string -> bool

exception NotImplemented of string

(** /!\  Unused *)
exception ParseError

(* alphabetic order *)
module Arg : module type of BaseArg
module Array : module type of BaseArray
module Char : module type of BaseChar
module Filename : module type of BaseFilename
module Format : module type of BaseFormat
module Hashtbl : module type of BaseHashtbl
module Int64 : module type of BaseInt64
module Lazy : module type of BaseLazy
module List : module type of BaseList
module Map : module type of BaseMap
module Marshal : module type of BaseMarshal
module Obj : module type of BaseObj
module Random : module type of BaseRandom
module Set : module type of BaseSet
module String : module type of BaseString
module Utf8 : module type of BaseUtf8
