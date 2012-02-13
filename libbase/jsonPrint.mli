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
   Printer of Json structure

   @author Rudy Sicard
   @author Raja Boujbel
   @author Mathieu Barbin
*)

(**
   Escape some special character.
*)
val escape_non_utf8_special : string -> string

module type Printer =
sig
  type t
  val json : t -> JsonTypes.json -> unit
end

(** {6 Efficient printing} *)

(**
   Factory
*)
val print : ('output -> string -> unit) -> 'output -> JsonTypes.json -> unit

(**
   This printer is used whenever you do not care about the look of the printed code.
   It's main quality reside in the fact that it prints directly in an [output_channel],
   without neither with parsing some format.
*)

module Output : Printer with type t = out_channel

(** {6 Unformated printing, with Buffer interface} *)

module Buffer : Printer with type t = Buffer.t


(** {6 Unefficient printing} *)

(**
   Do not use for big structure
*)
val to_string : JsonTypes.json -> string
