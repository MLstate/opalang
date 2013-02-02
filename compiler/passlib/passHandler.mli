(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(**
   Generic Library for compilers working in passes.

   @author Mathieu Barbin
   @author Quentin Bourgerie
   @author Valentin Gatien-Baron
*)

(**
    {6 A compiler working in passes}

    - First {b parse command line} with module [Arg] and adding to options
    specifications [Arg.options].

    - Second {b write your passes} Your passes can have
    preconditions and/or postconditions (See {!passCond}). And it
    should have a function which compute on a environment and
    eventually redefine [printer] and [iter_tracker], mostly if the
    environment type has been transformed.

    - Third {b create initial environment}, you can use either
    [init] for make a empty environment or [make_env] for make your
    initial environment.

    - Finally {b handle your passes}. You can use two programming
    style for make this.

    {9 Simple programming style}

    Use the pass handlers functions (See Section {!passHandlers})
    Thereafter a typical example :
    {[
    let _ = Arg.parse (PassHandler.Arg.options @ ... ) ...

    (* An if function *)
    let if_toto ~options e = options.toto

    (* The main handling *)
    let e = PassHandler.init
    let e = PassHandler.handler "MyFirstPass" pass_First
    let e = PassHandler.if_handler ~if_:if_toto "MyTotoPass" pass_Toto
    ...
    let r = PassHandler.return e
    ]}

    {9 Binary operators programming style}

    For handle your passes you can use also the binary operators (See
    {!binop}), it's a most pretty programming style. You can see that
    on this example :
    {[
    (* Parsing command line *)
    let _ = Arg.parse (PassHandler.Arg.options @ ... ) ...

    (* An if function *)
    let if_toto ~options e = options.toto

    (* The main handling *)
    open PassHandler

    let r =
      init
      |+> ("MyFirstPass", pass_First)
      |?> (if_toto,
          "MyTotoPass",  pass_Toto)
      ...
      |> return
    ]}

*)

(** {6 Options} *)
(** Provides a specification list for parse line command. *)
module Arg : sig
  (* TODO: document this *)
  (** The specification list of options for [PassHandler]. Define :
      - --check-all
      - --check-pass
      - --check
      - --track-all
      - --track-pass
      - --track
      - --track-dir
      - --print-all
      - --print-pass
      - --print
  *)
  val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list

end

(** Set title of the pass system.*)
val set_title : string -> unit

(**{6 Passes environment} *)

(** Just a alias for readability of the interface *)
type passname = string

(** Type of a env printer *)
type 'env printer = 'env PassTracker.printer

(** Type of a env tracker *)
type 'env tracker = 'env PassTracker.tracker

(**
   Type for identifying printers and trackers

   use [--print-help | --track-help] for listing all available printers
   and trackers.
   Constructor of id are [define_printer] and [define_tracker]
*)
(** *)
type printer_id
type tracker_id

(**
   Type of environment

   The environment contains a local ['env] which is the real compilation
   environment, the [options] of the compilation, and [printers] and [trackers]
   are functions to follow the transformations of the 'env along of passes.

   Todo: see if there is generalization problems if we remove the ['opt ->]
   from [printers and trackers]. Maybe the interface would be simpler.
*)
type ('opt, 'env) one_env = {
  env : 'env;
  options : 'opt;
  printers : 'opt -> (printer_id * 'env printer) list;
  trackers : 'opt -> (tracker_id * 'env tracker) list;
}

(** Create a environment. By default have no printers and no
    trackers. *)
val make_env :
  ?printers:('opt -> (printer_id * 'env printer) list) ->
  ?trackers:('opt -> (tracker_id * 'env tracker) list) ->
  'opt -> 'env -> ('opt, 'env) one_env

(**{6:passCond Passes condition} *)
(** Type of identifier of condition *)
type cond_id

(** Type of pass condition *)
type 'env cond

(**
   Define a condition on the pass handler system.

   This function does a side effect on a local table.
   Conditions are related to a specific warning class,
   which must be a child of the [cond] warning class.
   @see "WarningClass" for pre/post conditions warnings
   @raise Invalid_argument if the condition is already
   defined.
*)
val define_cond : WarningClass.wclass -> cond_id

(**
   Define a printer on the pass handler system.

   Like [define_cond], this function does a side effect on
   a local table, for checking the presence of a printer
   when the compiler is called with the option [--print]
   @raise Invalid_argument if the condition is already
   defined.
*)
val define_printer : string -> printer_id

(**
   Same function than [define_printer] but for tracker [--track]
   instead of printer.
*)
val define_tracker : string -> tracker_id

(**
   Return the string version of a cond_id.
   It is the same string than the string version
   of the warning class, which is the same string
   than what parse WarningClass.Arg.options.

   Example: ["cond.annot.unicity"]
   Warning class are hierarchic, and children are separated
   by a '.'

   @see "WarningClass" for more details.
*)
val cond_id : cond_id -> string

(**
   Return the string version of a printer_id
*)
val printer_id : printer_id -> string

(**
   Return the string version of a tracker_id
*)
val tracker_id : tracker_id -> string

(** Make a condition.
    + The first argument is the [cond_id] of the
    condition which must have been created with [define_cond].
    + Second it's a function to check the environment.
    The function which checks the environment should necessarily use
    a function dedicated to condition checkers if it want to fail.
    + ErrorManagment in this module
    + [LangCheck.check_fail] and related functions

    Internally, the good way for a checker to fail is to perform
    a warning of its class definition. That is what is done
    in any higher level function which takes all the [cond_id]
    as argument which has been built with the warning class,
    so that no confusion is possible.

    Read carefully the documentation before writing a checker.*)
val make_condition : cond_id -> ('env -> unit) -> 'env cond

(**
   Compose condition.
   It asserts that all conditions have been built with the same [cond_id].
   Uses [List.hd] internally.
*)
val compose_condition : 'env cond list -> 'env cond

(**
  Apply a function before the condition evaluation
*)
val compose_fun_condition : ('env_a -> 'env_b) -> 'env_b cond -> 'env_a cond

(** Check a condition. Usefull for check yourself a condition. *)
val check_condition : 'env -> 'env cond -> unit

(**{6 Invariants} *)
(** Type of an invariant *)
type ('env, 'env2) invariant

(** Make an invariant with two conditions. These conditions must be
    have the same id else raise [Invalid_argument]. *)
val make_invariant : 'env cond -> 'env2 cond -> ('env, 'env2) invariant

(**
  Make a new invariant with another environement : we apply a function before the invariant checking
*)
val compose_fun_invariant : ('env_1b -> 'env_1a) -> ('env_2b -> 'env_2a) -> ('env_1a, 'env_2a) invariant -> ('env_1b, 'env_2b) invariant

(** Make an invariant for a constant pass. *)
val make_cons_invariant : 'env cond -> ('env, 'env) invariant

(** {6 Passes}*)
(** Type of pass. *)
type ('opt, 'opt2, 'env, 'env2) pass = {
  invariant : ('env, 'env2) invariant list;
  precond  : 'env cond list;
  postcond : 'env2 cond list;
  f : ('opt, 'env) one_env -> ('opt2, 'env2) one_env;
}

(** Make a pass from a function which takes a environment and returns an
    another. And from optional pre and post conditions. *)
val make_pass :
  ?invariant:('env, 'env2) invariant list ->
  ?precond:'env cond list ->
  ?postcond:'env2 cond list ->
  (('opt, 'env) one_env -> ('opt2, 'env2) one_env) ->
  ('opt, 'opt2, 'env, 'env2) pass

(**{6:passHandlers Passes handlers} *)
(** Initial (or unit) environment. *)
val init : (unit, unit) one_env

(** Return handler, extract environment of generic environment. *)
val return : ('opt, 'env) one_env -> 'env

(** [handler name pass] Handle [pass] named by the given [name].
    [count_time] indicates whether the time of this pass should be stored
*)
val handler :
  ?count_time:bool ->
  passname -> ('opt, 'opt2, 'env, 'env2) pass ->
  ('opt, 'env) one_env -> ('opt2, 'env2) one_env

(** Handle pass if [_if] returns true. *)
val if_handler :
  ?if_:(options:'opt -> 'env -> bool) ->
  passname -> ('opt, 'opt, 'env, 'env) pass ->
  ('opt, 'env) one_env -> ('opt, 'env) one_env

(** [alt_handler if_ (name1, pass1) (name2, pass2)] handle pass1 if
    [if_] return [true] else handle pass2 *)
val alt_handler :
  (options:'opt -> 'env -> bool) ->
  (string * (('opt, 'opt2, 'env, 'env2) pass)) ->
  (string * (('opt, 'opt2, 'env, 'env2) pass)) ->
  ('opt, 'env) one_env -> ('opt2, 'env2) one_env

(** [switch_handler select switch_pass] [select] should produce from
    pass environment a value that be used by [switch_pass] to switch
    to the wanted pass. *)
val switch_handler :
  (options:'opt -> 'env -> 'switch) ->
  ('switch -> (string * (('opt, 'opt2, 'env, 'env2) pass))) ->
  ('opt, 'env) one_env -> ('opt2, 'env2) one_env

(** Compose if functions. The resulting function return true if all
    composed functions returns true.*)
val and_if :
  (options:'opt -> 'env -> bool) list -> (options:'opt -> 'env -> bool)

(** Compose if functions. The resulting function return true if at
    least one composed functions returns true.*)
val or_if :
  (options:'opt -> 'env -> bool) list -> (options:'opt -> 'env -> bool)

(**{6:binop Binary operators} *)
(** A binary operator for [handler] *)
val (|+>) :
  ('opt, 'env) one_env ->
  (passname * ('opt, 'opt2, 'env, 'env2) pass) ->
  ('opt2, 'env2) one_env

(** A binary operator for [if_handler] *)
val (|?>) :
  ('opt, 'env) one_env ->
  ((options:'opt -> 'env -> bool) *
     passname * ('opt, 'opt, 'env, 'env) pass) ->
  ('opt, 'env) one_env

(** A binary operator for [alt_handler] *)
val (<?>) :
  ('opt, 'env) one_env ->
  ((options:'opt -> 'env -> bool)
   * (passname * (('opt, 'opt2, 'env, 'env2) pass))
   * (passname * (('opt, 'opt2, 'env, 'env2) pass))) ->
  ('opt2, 'env2) one_env

(** A binary operator for [switch_handler] *)
val (|?|) :
  ('opt, 'env) one_env ->
  (options:'opt -> 'env -> 'switch) *
  ('switch -> (string * (('opt, 'opt2, 'env, 'env2) pass))) ->
  ('opt2, 'env2) one_env

(** A binary operator for make a pipe [a |> f] equals to [f a] *)
val (|>) : 'a -> ('a -> 'b) -> 'b

(**
   A few combinators for if_handlers
*)
val (or) : (options:'opt -> 'env -> bool) -> (options:'opt -> 'env -> bool) -> (options:'opt -> 'env -> bool)
val (&) : (options:'opt -> 'env -> bool) -> (options:'opt -> 'env -> bool) -> (options:'opt -> 'env -> bool)
val neg : (options:'opt -> 'env -> bool) -> (options:'opt -> 'env -> bool)

(**{6 Error management} *)

(**
   Internally, the [handler] function performs checks, printing, tracking, etc...
   For the good behavior of the system, error flows should be controlled,
   for an optimal reporting.

   That means that a discipline is asked to what passes and checkers do
   in case of errors. They should follow the rules.
*)

(**{9 Functions used by passes}*)

(**
   This functions are called indirectly by a pass when a condition has been
   violated but was not checked before the pass.
   It is not called directly, but overlayed with a [LangError] module, which
   precise once for all the ['context printer] corresponding to the context
   of the language it checks.

   During the passsystem, if this function is called, it will lead to try to
   start the corresponding check on the previous compilation environment
   (the one returned by the previous pass), for having a better error report.

   If the precondition is found in the list of precondition of the pass,
   the check is done. If the test was already activated, somebody is laying,
   this error is reported too. If the precondition is not part of the pass,
   the pass is declared incoherent, and the error is reported.

   It stores everything possible in the track system, and exit.

   Do not use this function Outside of the passsystem, this function
   would raise an internal exception not exported in the api.
 *)
val cond_violation : 'context printer -> cond_id -> 'context -> ('c, 'error) OManager.oformat -> 'c

(**
   The same function as [cond_violation] but returns unit.
   It allows you to report several errors before failing.
*)
val scond_violation : 'context printer -> cond_id -> 'context -> ('c, unit) OManager.oformat -> 'c

(**
   Anonymous Internal error.

   Sense of the [cond_id option]
   + With [Some cond_id] : it is an alias for [cond_violation]
   + With [None] :
   Basically the same function than [cond_violation], but this error cannot be related to a precondition.

   Why we let the [cond_id option] in the interface ? So that if you uses this function, and
   ask yourself about what you should do with it, it will normally lead you until this part
   of the documentation. So please, read the following :

   <!> The case [None] is pretty rare, so consider that if you are using this function without precising
   a related condition to your internal error, you could maybe extract for your current problem
   a generic enough invariant to be checked officially at some point during the passes, and let
   potentially other passes take benefits of this check.

   If you need a new condition, consider adding this condition in the [LangCheck] module
   corresponding to the language your are compiling, and then add the precondition to your pass,
   and finally use [Some cond] or directly [cond_violation] instead of [i_error] in your pass.
*)
val i_error : 'context printer -> cond_id option -> 'context -> ('c, 'error) OManager.oformat -> 'c

(** The same function [i_error] but returns unit for several errors reporting *)
val i_serror : 'context printer -> cond_id option -> 'context -> ('c, unit) OManager.oformat -> 'c

(** {9 Functions used by conditions checkers} *)

(**
   Condition failure: [check_fail] and [scheck_fail].

   + These functions are usually not called directly by checkers because it would duplicate
   the work of context formatting for a same language. (how to print an error message depending
   on the expressions, code kind your are manipulating, etc...)
   + Normally, you should find a [LangError] module dedicated to your language which uses
   this functions but applying specifics [printers] and [context] once for all, so that
   you can call directly functions from [LangError] without specifying the printers used
   for error reporting in your language.
   + There is a simple functor in [PassError] for making the life of [LangError] easier.
   + If you are outside of a language, or if your context is very different that the
   common error context defined in [LangError], you are invited to call this function directly,
   specifying [printers] which apply to your local ad-hoc context.
   + Typically, these functions are indirectly used in [LangCheck] modules, using the [LangError]
   redefinition. [LangError] is invited to keep the same names by redefining functions, for clarity.
   + These functions should not be used by a pass, only by checkers !
   if a pass failed because of a broken invariant related to a cond,
   the pass should use a function from [LangError] dedicated to this case.

   Order of args are for partial application :
   The [LangError] module knows how to print its own context
   because it defines the type context :

   In langError.ml :
   {[
   type context = ...
   let context_printer = ....
   let check_fail c = PassHandler.check_fail context_printer c
   ]}

   In a checker, we will use :
   {[
   let rewrite ... =
     let context = ... in
     Lang.Error.check_fail context Lang.Check.Cond.check "%s is broken" "this invariant"
   ]}

   Finally for readability of the code, you can define as soon as you have the context in your scope
   an alias for [Lang.Error.check_fail] with e.g. ['!!'] which looks like a danger sign.

   {[
   let my_check code =
     let rec aux expr =
     ....
     (* from there you can build your context, and then : *)
     let (!!) = Lang.error.check_fail mycond_id context in
     ...
     match ... with
      | ... -> !! "this is an %s" "error"
      | e -> !! "an other one related to %s" (... e)
   ]}

*)

(**
   What is happening with a [check_fail] ?

   + Will store the full context in the track system, using the [full] printer
   + Will print a reduced version on stderr using the [console] printer
   + Will call a warning with the given format, using the warning class of the cond_id

   @see "PassError" for a common implementation of context overlay.
*)
val check_fail : full:'context printer -> console:'context printer -> cond_id -> 'context -> ('c, 'error) OManager.oformat -> 'c

(**
   The same function, but for outputting many messages without killing the check with the first error.
*)
val scheck_fail : full:'context printer -> console:'context printer -> cond_id -> 'context -> ('c, unit) OManager.oformat -> 'c

(** {6 Marshal Pass} *)

val register_printer : (PassTracker.passname -> ('opt -> (printer_id * 'env PassTracker.printer) list) option) -> unit

(** {6 Deprecated API} *)

(** Old passes handlers
    This handlers shouldn't used except for handle old passes which are not yet ported, or which will never be. *)

(** Type of an old pass.
    @deprecated Use [pass]
*)
type ('opt, 'env, 'env2) old_pass = options:'opt -> 'env -> 'env2

(** Handle old passes which transform environment. Generated
    environment contains default printer and tracker.
    @deprecated Use [handler] *)
val old_handler :
  ?precond:('env cond list) ->
  ?postcond:('env2 cond list) ->
  passname -> ('opt, 'env, 'env2) old_pass ->
  ('opt, 'env) one_env -> ('opt, 'env2) one_env

(** Handle old passes.
    @deprecated Use [if_handler] *)
val old_if_handler :
  ?if_:(options:'opt -> 'env -> bool) ->
  ?precond:('env cond list) ->
  ?postcond:('env cond list) ->
  passname -> ('opt, 'env, 'env) old_pass ->
  ('opt, 'env) one_env -> ('opt, 'env) one_env
