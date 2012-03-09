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

(** Manage all warning of compiler. *)

(** {6 Defined types}*)
(** Type of a warning.*)
type wclass

(**
   Warning classes (wclass values) forms a hierarchy.
   You can manipule both node and leaves of this hierarchy.
   Each class describe a sub-set of warning.
   You can then choose the default properties of all warnings in the class when creating it:
   -enable/disable
   -error/warning
   and change this behaviour later
   and you can use the class when emitting so that it follows the class common properties
*)

(** {6 Creating of warning classes} *)
(**
   Create a new class of warning

   parent : specify the ancestor class of the new class.
     If you do not provide a parent value, the warning should be loaded for beeing part of the
     warning table.
   public : specify is the warning class is public or not
   (i.e. if not public the user has no control on it,
         and the message is an internal error (in error mode) or skipped )

   doc : is the message associate to the command line option
   err : behaviour of a warning of the class (true => error, false => warning)
   enable : true => warning class enable, false => disable

*)

val create : ?parent:wclass -> ?public:bool -> name:string -> doc:string -> err:bool -> enable:bool -> unit -> wclass

(** {6 Regrouping Warnings} *)

(** Imperatives Set of wclasses *)
module Set :
sig
  type t

  (** Create a new empty set *)
  val create : unit -> t

  (**
     Add a warning class and alls its children into the set.
     Beware, you should add the parent warning given an hieararchy.
     You cannot add a child, or a sub-branch alone, without adding the parent.
  *)
  val add : t -> wclass -> unit

  val add_all : t -> wclass list -> unit

  (**
     [Set.add_set t t'] add in [t] all wclass of [t'].
     Usefull for regrouping several warnings for a binary.
  *)
  val add_set : t -> t -> unit

  (**
     Create a new warning set and initialize it with the given list
     of warning classes
  *)
  val create_from_list : wclass list -> t
end

(** {6 Loading Warnings} *)
(**
   Load all wclass of a set into the global table. Any not loaded wclass is ignored,
   the functions [is_warn] and [is_warn_error] will return always [false].

   When you load a wclass, the load also all the children of the wclass
*)

val load : wclass -> unit
val load_set : Set.t -> unit

(** {6 Set properties of warning classes}*)

(** Set the error on warning. If is set to true, warning has same
    behavior that an error.*)
val set_warn_error : wclass -> bool -> unit

(** Set print on warning. If is set to true warning is printed
    else is not printed. *)
val set_warn : wclass -> bool -> unit

(** {6 Get properties of warning classes}*)

(**
   If the wclass is not loaded, these functions returns always [false].
*)

val is_warn_error : wclass -> bool
val is_warn : wclass -> bool

(** returns the full name of the warning,
    as it is used by the Arg.options.
    In the hierarchy, warning are separated
    with dots. except for the root node which
    does not appear in any name *)
val get_name : wclass -> string

val get_doc : wclass -> string

(** {6 Iterators}*)

val fold : ('acc -> string list -> wclass -> 'acc) -> 'acc -> 'acc
(**
   Fold on all the warnings loaded in the global table.
   @param f takes:
            - the accumulator
            - the path to the current warning class
            - the current warning class
            The warning class can be used to access other information
            of the warning class like the documentation string
*)

(** {6 Options} *)
(** Provides a specification list for parse line command. *)
module Arg : sig
  (** The specification list of options for [PassHandler]. Define :
      - --warn
      - --no-warn
      - --warn-error
      - --no-warn-error
      - --warn-help

      TODO:--warn-bash-completion

      The unit is there so that the warning class of this module have time
      to be loaded and registered before returning the options list
  *)
  val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list

end

(** {6 Warning classes} *)

val get_from_path : string list -> wclass option
(**
   Get a class from a name, searching in loaded wclass.
   @param path The path in the warning hierarchy
               Note that [\[\]] is a valid path that designates all the warnings
   @raise Invalid_argument if the path doesn't exist
*)

val root_warning : wclass

(* GUIDELINES : ALPHABETIC ORDER OF PARENTS !!! *)
(* Define a parent in each part, use prefix for names hierarchy *)

(** {6 BSL Warnings} *)

(** The root of all bsl related warnings *)
val bsl : wclass

(** Warnings related to backend restriction *)
val bsl_backend_restriction : wclass

(** Warnings related to loading (essentially, plugins) *)
val bsl_loading : wclass

(** Warnings related to registering bypass *)
val bsl_projection : wclass

(** Warnings related to registering bypass *)
val bsl_register : wclass

(** Warnings related to type checking *)
val bsl_type_checking : wclass

(** Unknown bypass during compilation (not supported in every mode) *)
val bsl_unknown_bypass : wclass

(** {6 Conditions Warnings} *)

(**
   GUIDELINES

   Hierarchical warnings, alphabetical ordering,
   and keep coherent with names from $(Lang)Check modules.

   [QmlCheck], [OpaCheck], [OcamlCheck], [JsCheck], etc..

   In modules [LangCheck], checks are in module, there,
   everything is at top level with underscore.

   Example :
   [cond_annot_unicity], [cond_bypass_expanded]
   represents the wclass used in :
   {[
   OpaCheck.Annot.unicity
   QmlCheck.Bypass.expanded
   ]}
*)

(** Used for pre/post condition on passes. *)
val cond : wclass

(** {6 Dbgen Warnings} *)

val dbgen : wclass

val dbgen_schema : wclass

val dbgen_mongo : wclass

(** {6 Explicit instantiation} *)

(** The root of all warnings related to explicit instantiation. *)
val ei : wclass

(** Explicit instantiation generalize a non-functionnal value. (A
    non-functionnal value is transformed on functional value.) *)
val ei_generalize : wclass

(** {6 Patterns} *)

(** Warn suspicious patterns *)
val pattern : wclass


(** {6 PassHandler Warnings} *)

(** Root of pass system warnings. *)
val phandler : wclass

(** Warn if the pass system is not consistent. *)
val phandler_consistency : wclass

(** {6 Common warnings}
    Warnings that can be raise by any compiler passes
*)
(** Root of common warnings *)
val warn : wclass

(** Use it when an optimization level is wrong*)
val warn_olevel : wclass
