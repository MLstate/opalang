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
(** {1 General parser for the arguments of servers}

    This must be very generic because it may need to parse arguments related to
    any "appserver component", eg. http server, database server, opa server,
    etc.

    Option parsers are applied with the [filter] function that removes parsed
    args from the current command-line. Hence it is possible to parse several
    times, for different kinds of options.

    {5 Current use}
    Each server-module defines its own options and applies the parser, filtering
    them out and building its [options] record. The opa server, at the end, may
    take whatever is left. When [--help] is present, parsers only print out the
    help {b and continue} for the others to do as well.

    {5 Intended future use}
    Appserver components have no side-effects at loading, but follow an
    interface that requires a speclist (option parser), some appserver options
    and an initialiser ([options -> unit]) to be defined. We define a
    component-combiner functor, and the Appserver is a functor that takes a
    component as argument.

    This way, all speclists are merged (and checked for conflicts), and we parse
    arguments in one pass, fill each individual options structure, and call the
    init function of each component. Then a main-server-loop may be run.

    {5 More remarks and todos}
    For opa servers, they would need a specific construction (pass?) to register
    options, if we don't want database opening to be necessary to print the (end
    of the) help message.

    Last option parsing should also be a special case: it needs to check for any
    remaining options and warn about the errors, as well as quit in the case
    '--help' was present.

    In the "intended future use", the [Appserver.start] could to both these
    tasks. (It would probably take place before any opa code is executed).
*)


(** The type holding the command-line arguments *)
type args

(** The current command-line (may be filtered several times) *)
val get_argv: unit -> args
val set_argv: args -> unit

val is_empty: args -> bool

val to_list: args -> string list
val from_list: string list -> args

(** Gives the remaining command-line arguments out as string. Useful to print a
    message on remaining arguments after parsing *)
val argv_to_string: unit -> string

(** The general type of command-line parsers: filters some options and returns a
    value of type ['a] *)
type 'a param_parser

(** Low-level parameter parsers *)

val int: int param_parser
val string: string param_parser (** Does not accept strings starting with '-' (safer) *)
val anystring: string param_parser (** Accepts even strings starting with '-' *)
val float: float param_parser
val bool: bool param_parser
val unit: unit param_parser
val stringset: (string * 'a) list -> 'a param_parser
  (** Parse a string within the given association list or fails *)
val list: char -> 'a param_parser -> 'a list param_parser
  (** splits a string using the given separator character, then parses individual elements *)
val option: 'a param_parser -> 'a option param_parser
val pair: 'a param_parser -> 'b param_parser -> ('a * 'b) param_parser
val check: 'a param_parser -> ('a -> bool) -> 'a param_parser
  (** Check a property on the value returned by a parser: fail parsing if false *)
val keep: 'a param_parser -> 'a param_parser
  (** Returns the same parser, but that doesn't consume any arguments *)
val wrap: 'a param_parser -> ('a -> 'b) -> 'b param_parser
val wrap_opt: 'a param_parser -> ('a -> 'b option) -> 'b param_parser
  (** Same as [wrap], but your function returns an option to possibly indicate a
      failure *)
val func: 'a param_parser -> ('acc -> 'a -> 'acc) -> 'acc -> 'acc param_parser
  (** Useful to build arg_parsers used to fold on the arguments *)
val func_opt: 'a param_parser -> ('acc -> 'a -> 'acc option) -> 'acc -> 'acc param_parser
  (** Same as [func], but your function returns an option to possibly indicate a
      failure *)

val skip: unit param_parser
  (** Skips the next argument, but keeps it for the next parsers *)
val skip_all: unit param_parser
  (** Skips all following arguments, but keeps them for the next parsers *)
val fold: ('a -> 'a param_parser) -> 'a -> 'a param_parser
  (** Repeat the application of a parser, accumulating the results. Stops if
      there is an error *)
val fold_until: string -> ('a -> 'a param_parser) -> 'a -> 'a param_parser
  (** Apply the parser repeatedly until [string] is found or all args are
      parsed, or there is an error (typically [string] is "--") *)

val fold_all : ('a -> 'a param_parser) -> 'a -> 'a param_parser
  (**
     Repeat the application of a parser until the end of args, accumulating the results.
     When the parser does not apply, arguments are skipped, and previous result of
     accumulation is passed to the next arguments.
     This is used for parsing e.g. anonymous arguments
  *)

val skip_str: string -> 'a param_parser -> 'a option param_parser
  (** If the args contain [string], skip it and apply parser if it is first,
      return None otherwise. Apply parser normally if they don't. [str] is
      typically "--". *)
val push: string -> unit param_parser
  (** Pushes back an argument to the command-line, for the next modules to
      see. For example, you can use it if you want to treat "--help" manually
      but leave it for the others, of if some option in your module A implies
      another option in a module B that loads later. *)


(**
    {1 Higher-level command-line parsers}
*)

type 'a arg_parser = string list * ('a -> 'a param_parser) * string * string
    (** [(names,effect,params_doc,doc)] tuple:
        - [names] are the option name and variants (guideline: long name first
        starting with "--", optionally followed by shortcut (one char)
        starting with "-")
        - the effect will be called on the following arguments when the name matched
        - [params_doc] should be the empty string if the option has no
        additional parameters, [<type>] for mandatory parameters, [[type]] for
        optional parameters.
        - [doc] is the description of the option. Guideline: first letter
        capitalised, no dot at the end.

        For example:
        {[
        (["--opa"], func string (fun opts file -> { opts with opa = file }),
        "<string>", "Specify an OPA source file")
        ]}
        is the specification of the [--opa] option that takes one mandatory
        argument of type string.

        Note: you should not do side-effects inside the parser (they may be
        executed even if a "--help" is read later). *)

val doc_string: string -> 'a arg_parser list -> string
  (** Doc string as printed by --help. The first argument is the title. *)

val write_simple_manpage :
  ?nohelp:bool ->
  cmdname:string ->
  section:int ->
  ?centerfooter:string ->
  ?leftfooter:string ->
  ?centerheader:string ->
  ?summary:string ->
  ?synopsis:string ->
  ?description:string ->
  ?options:'a arg_parser list ->
  ?other:(string * string) list -> out_channel -> unit
  (** Create a simple manpage. *)

val make_parser: ?final:bool -> ?nohelp:bool -> string -> 'a arg_parser list -> 'a -> 'a param_parser
  (** Makes a param_parser from a list of arg_parsers. The '-help' and '--help'
      options have a default handler that turns off handling of other options,
      prints the help string with Logger.log_error and return the structure
      unchanged.
      The first argument is the title of the parser, printed in the help (no
      capital letters, no punctuation).

      You are allowed to have arguments toggled by ppdebug vars in your
      arg_parser list, but don't forget to add [debug] at the start of the
      description string in that case, to avoid confusion.

      @raise Exit after printing a help message when applied in case of
      error, or if '--help' and [final] is true *)

(**
   {1 Alternative construction API}
*)
type 'a state = No_more_params of 'a
              | More_params    of 'a
              | Maybe_params   of 'a

type 'a instruction = 'a state option

val make_arg_parser: names:(string list) -> param_doc:string -> doc:string -> initialize:('a -> 'a state) -> step:('a -> string -> 'a instruction) -> 'a arg_parser

(**
   {1 Filters}
*)

val filter_functional: args -> 'a -> ('a -> 'a param_parser) -> 'a * args
  (** Same as [filter] but without the side-effect *)

val filter: 'a -> ('a -> 'a param_parser) -> 'a
  (** Returns the result of the application of a param_parser on the current
      command-line. What was matched is removed from [argv]. Returns its
      argument unchanged in case the parser did not apply. *)

val extract_prefix: string -> args
  (** Use to cleanup the usage with multi-parsers: at the loading of your
      module, use [extract_prefix "--mypfx-"]. Any options starting with
      "--mypfx-" will be removed from the argv and returned to you, for
      later parsing. Extraction stops if "--" is met ; the argument after
      each --mypfx-option is extracted too if it doesn't start with "-".
  *)

(** {6 A few useful pre-defined parsers} *)
val parse_addr_raw: string -> (Unix.inet_addr * int option) option
val parse_addr: (Unix.inet_addr * int option) param_parser

  (** Parses the syntax [host[:port]]. Resolves the host using the Unix module
      (fails if it is not found). *)

(** {6 Binding with Arg} *)
(**
   For certain runtime applications which contain some compiler pass,
   or want to use existing spec. Typically, for OManager, WarningClass.

   <!> The imported options does not respect the contrat of ServerArg,
   because they are implemented using side-effects.

   <!> Currently, some case are not implemented (assert false) :
   - Tuple
   - Symbol
   - Rest
   - Complete

   <!> Not for casual users.
*)

val import_arg_spec : Base.Arg.spec -> ('a -> 'a param_parser)
val import_arg_opt : Base.Arg.key * Base.Arg.spec * Base.Arg.doc -> 'a arg_parser
val import_arg_options : ( Base.Arg.key * Base.Arg.spec * Base.Arg.doc ) list -> 'a arg_parser list
