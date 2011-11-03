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
type simple_completion =
  | Nothing (** no completion possible *)
  | File of string (** completion matching the bash pattern *)
  | Dir (** any directory *)
  | Oneof of string list (** a finite set of possibilities *)

type completion = {params : simple_completion list; stop : bool}
    (**
       The type of bash completions
       The list represents the completion of consecutive arguments
       The boolean is true when completion should stop here (after '--' for instance)
    *)

type spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref
  | String of (string -> unit)
  | Set_string of string ref
  | Int of (int -> unit)
  | Set_int of int ref
  | Float of (float -> unit)
  | Set_float of float ref
  | Tuple of spec list
  | Symbol of string list * (string -> unit)
  | Rest of (string -> unit)
  | Complete of spec * completion
      (** to be used when the spec does not allow automatic completion, but you can specify one
          for example, with [("--impl", Arg.String _, "")], it can't be guessed that
          it should be completed with a file, so you should say
          [("--impl", Arg.Complete (Arg.String _, {params=[File "*"];stop=false}), "")] instead
      *)

type key = string
type doc = string
type usage_msg = string
type anon_fun = string -> unit
exception Help of string
exception Bad of string
val parse : (key * spec * doc) list -> anon_fun -> usage_msg -> unit
val parse_argv : ?current:int ref -> string array ->
  (key * spec * doc) list -> anon_fun -> usage_msg -> unit
val usage : (key * spec * doc) list -> usage_msg -> unit

val align : (key * spec * doc) list -> (key * spec * doc) list
  (** beware, if you wish to call [add_bash_completion], you should do it before calling [align] *)
val current : int ref

val sort : (key * spec * doc) list -> (key * spec * doc) list
  (** sort the options by alphabetical order on the key.
      if a key appear more than once in the spec list,
      only the first occurrence is keeped, while any other
      occurrence is simply removed (no error) *)

val spec_fun_of_assoc : ('a -> unit) -> (string * 'a) list -> spec
val spec_of_assoc : 'a ref -> (string * 'a) list -> spec
val spec_opt_of_assoc : 'a option ref -> (string * 'a) list -> spec
val spec_of_opt_assoc : 'a ref -> 'a -> (string * 'a) list -> spec

val add_bash_completion :
  ?name:string ->
  ?names:string list ->
  ?default:simple_completion ->
  (string * spec * string) list ->
  (string * spec * string) list
    (**
       adds a --bash-completion option to the command line that generates a
       "bash_completion" for the given command line parser
       Should be the last option added
       @param name The name of the executable that completion will work on (eg qmlflat)
       @param names Same as [name], but several names can be given (eg [\[qmlflat;qmlflat.native\]])
       @param default The completion for anonymous argument
       @param arg The arguments for which completion will happen
       @return The same arguments with a new option --bash-completion
    *)

(**
   Often, options in command line can be given grouped.
   Like : [-I "foo,bar,foobar"]
   This function split a given argument into words. Char separators
   are : [';' ; ',' ; ' ']
*)
val split : string -> string list
