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
   The types recognized by any bypass standard library.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)


(**
   The standard types (represented by type [t])
   are the types which can be mapped to all destination
   targets.
   They are the types allowed in the interface between opa code and back-end languages.

   What if I need more type ?

   BslTypes are involved in an auto-projection process which can have a cost for
   complex data-structures.

   That's why we do not want to allow auto-projection for recursive types for example.
   There is now a support via the [ServerLib] for a low level management of complex
   projection between arbitrary external types and opa structures.
   This is the sense of the type constuctor {b OpaValue},
   which is available with the syntax [opa[...]] handled by {b bslregister}.
*)

(** {6 Type alias} *)
type pos = FilePos.pos


(** {6 Type Variables} *)

(**
   A type for polymorphic type variables.

   [BslTypes] uses TypeVar for e.g. keeping trace of name of parameter names and design
   For meta generation of loaders, since TypeVar would be abstract, use a let
   intro with call to TypeVar.next, or maybe let the type t be parametrized
   by typevariables, so that we can use int locally for code generation.
*)

type typevar = QmlTypeVars.TypeVar.t

module TypeVar       : QmlTypeVars.GEN_VAR with type t = typevar
module TypeVarPrint  : QmlTypeVars.VAR_PRINT with type t = typevar

module TypeVarMap    : BaseMapSig.S with type key = typevar
module TypeVarSet    : BaseSetSig.S with type elt = typevar

(**
   Used internally and for code generation. Returns a canonical [typevar] given an [int].
   The typevar returned is always the same for a fixed positive [int].
   The implementation is [assert false] for negative indexes.
*)
val (~@) : int -> typevar

(**
   Used only for code generation, for keeping trace of type variables name
   from the implementation. Given the name of the variable in a type definition
   or coercion, returns always the same [typevar].
   This is dangerous because for a given string, the typevar returned is always
   the same.
   Not for casual user.
*)
val (~$) : string -> typevar

(** {6 Types algebra} *)

(**
   The algebra of standard types. Some details :
   + [External of string * t list] : the name of the type * the instance of the parameters if the type is parametric.
   + [Fun of t list * t] is the representation for functions.
   + [Void] For a non Nary language (Ocaml), Void is replace by an inserted [unit].

   Positions: it is much more efficient to flat all the data after the constructor.
   We do not want to use QmlLoc.label for this reason.
   TODO: do the same in QmlAst and OpaAst.

*)
type t =
  | Const    of pos * QmlAst.const_ty
  | TypeVar  of pos * typevar
  | Void     of pos
  | Bool     of pos
  | Option   of pos * t
  | OpaValue of pos * t
  | Fun      of pos * t list * t
  | External of pos * string * t list

(** {6 GUIDELINES for matching a [t] } *)
(**
   Variables, and module shorthand :
   + [bslty] is the variable name for a value of type [t]
   + [B] is the letter for short access (local allias)

  Verbose:
   {[
   | Const (pos, const)
   | TypeVar (pos, typevar)
   | Void pos
   | Bool pos
   | Option (pos, t)
   | OpaValue (pos, t)
   | Fun (pos, args, ret)
   | External (pos, name, params)
   ]}

  Shorter:
   {[
   | Const (p, c)
   | TypeVar (p, v)
   | Void p
   | Bool p
   | Option (p, t)
   | OpaValue (p, t)
   | Fun (p, u, v)
   | External (p, n, vs)
   ]}
*)

(** {6 Positions} *)
(**
   This module follows the guidelines for error reporting :
   + It uses [OManager] for printing localized error messages.
*)
(** *)
val pos : t -> pos
val reset_pos : t -> pos -> t
val merge_pos : t -> pos -> t

(** {6 Printing} *)
(**
   This module follows the guidelines for printing.
*)
(** *)
type 't pprinter = 't LangPrint.pprinter

(**
   Printer for [t] with the bsl syntax.

   The scope of type vars is fresh for every call to pp.

   Currently the syntax is not really specified, but it should
   in a near future correspond exactly to the syntax parsed in
   bslregister directives.
*)
val pp : t pprinter

(**
   Same than [pp] but with a manual initialisation of the scope
   for printing type variables.
*)
val pp_scope : scope:TypeVarPrint.scope -> t pprinter

(**
   Search for the position of t, if it is not empty print the citation,
   if the type is a builtin, does nothing.
*)
val pp_citation : t pprinter

(**
   Search for the position of t, if it is not empty print the citation,
   if the type is a builtin, print the type.
*)
val pp_context : t pprinter
val pp_multi_context : t list pprinter

(** {6 Types Manipulations and Transformations} *)

(** Ocaml does not like [with type t = t], [this_t] is just an alias to type [t] *)
type this_t = t

(** The instance of TRAVERSE module for the type [t] *)
module Walk : TraverseInterface.TRAVERSE
  with type 'a t = this_t constraint 'a = _ * _ * _
  and type 'a container = this_t constraint 'a = _ * _ * _

(**
   The type for representing free type variables.
*)
type freevars = TypeVarSet.t

(** Compute the set of free type variables of a given type [t]. *)
val freevars : t -> freevars

(** Like [freevars] but with a non empty initial set. *)
val fold_freevars : freevars -> t -> freevars

(**
   Canonical representation of types after an type variables alpha conversion.

   This is used for being able to compare two types non sensible to alpha conversion,
   or for caching, memoizing, etc...

   After a normalization, 2 types equal under alphaconf will be structurally equal.

   Note about the canonical representation of typevars:

   Since we use [TypeVar.t] instead of [int], you cannot reset manually the
   stamps of [TypeVar.t] but there is an API for such things, used e.g. for typeschemes
   sharing during the Explicit Instantiation pass, this is the [get_canonical_typevar],
   defined in [QmlTypeVars].
*)
val normalize : t -> t

(**
   After a projection, a type is an opavalue.
*)
val opavalue : t -> t

(**
   Once all projection have been performed,
   the opavalue constructor does not mean
   anything. You can use this function for
   purging it.
*)
val purge_opavalue : t -> t

(** {6 Substitution} *)

(**
   A common type for subtitutions.
*)
type 'a substitution = 'a TypeVarMap.t

val empty_substitution : t substitution

(**
   Given a [substitution] between [typevar] and [t], replace every occurrence
   of [TypeVar i] by [substitute(i)].

   The mapping is the identity if [i] is [Not_found]
*)
val substitute : t substitution -> t -> t

(** {6 Comparaison, Set and Map} *)

(**
   Compare values of type [t] :
   if [normalize] is set to [true], the comparaison returns [0] if [a] and [b] are
   equal under alphaconversion of type variable, which means that we perform an operation
   of normalization before the comparaison. (can be done in line)

   Otherwise ([normalize] set to [false]) the comparaison is structural, ['a <> 'b]
   The default value is [alphaconv:false].
*)
val compare : ?normalize:bool -> t -> t -> int

(** {6 Checking} *)

(**
   Checks if the given type is a function of second order,
   which means that one of the arguement (or the returned type)
   is or contains an arrow type.
   This is used for adding parent continuation to the uncps
   projection, in order not to lose the context.
*)
val is_second_order : t -> bool

(**
   This function is used e.g. to check the definition of a bsl primitive
   in opa, regarding to its external definition.

   It is not a unification for checking its utilisation, it is more
   restrictive than an unification.

   It returns the [substitution] built during the inclusion check
   (and it takes a substitution to start with).

   There is mostly 2 utilisations of this :

   + compile time :
   Example, a primitive defined as:
   {[##register cmp : 'a, 'a -> int]}
   and then declared in opa as:
   {[val cmp_int = %%cmp%% : int, int -> int]}
   This is allowed, because [int -> int -> int] is included in ['a -> 'a -> int].
   The [expected] type should be the type of the definition,
   and the [found] type the type of the coercion.
   + runtime :
   e.g. in the interpreter.
   We perform some runtime checks before applying functions.
   This feature is used in the interpreter to avoid seg-faults.
   It is used essentially for debug and pedagogic support.
   In the real life (compiled opa code), there is no such checks,
   it is just seg-faults.
   We are definitly not targetting performances with the interpreter.
   We'd like rather to get documented error messages for patching part
   and part of the framework.

   <!> Warning : this function is not as complete as a correct type unification.
   It is not meant to replace the typer, which implement already (let's hope)
   a correct unification. This function is just use as a pedagogic issue for
   the interpreter opatop to notify runtime errors instead of segfaulting,
   and avoiding mispeling or mistakes in the bsl.

   The {b static_strict_check:true} option is used for failing in case of magic
   parameters. Exemple :
   {[
   expected:[int->int] found:['a->'a]
   ]}
   will fail in case of [static_strict_check:true].
   By default, the behavior is strict. The not strict option is used by opatop.

   @error if the inclusion is not possible (e.g. this would segfault at runtime)
*)
val check_inclusion :
  ?static_strict_check:bool ->
  t substitution -> expected:t -> found:t -> t substitution

(**
   The same function, reseting from no substitution, and ignoring the returned subst
   @error same than [check_inclusion]
*)
val check :
  ?static_strict_check:bool ->
  expected:t -> found:t -> unit

(**
   Given a list of instance for type parameters and a value of type [t],
   return the specialized version of the polymorphic type [t].

   Types that can be specialized are :
   + [External]

   Error if the type is not a parametric type, or if the list of instance
   is not of the same length as parameters of the type [t]
*)
val specialize : t list -> t -> t

(** {6 Binding with QmlAst.ty} *)

(** *)
val of_const : QmlAst.const_expr -> t

(**
   The gamma is needed to distinguate extern-abstract types and other types.
   This function is used as a runtime type verification (top-level) by
   building a new bypass function
   That checks that the coercion given is coherent with the type in the bsl

   <!> Beware, the scope is local to one call to [of_ty]

   TODO: QmlAst should provide some pos for ty as well
   as OpaAst does.

   @error if some incoherency in gamma, or if the type is not allowed to be
   a bsl type.
*)
val of_ty :
  gamma:QmlTypes.Env.t ->
  QmlAst.ty -> t

(** *)
val to_ty : ?typeident:(?check:bool -> string -> QmlAst.TypeIdent.t) -> t -> QmlAst.ty

(** {6 Bypass Types Runtime Restriction} *)

(**
   Test about specialization of parameters of parametric extern types.

   Exemple:
   {[
   ##register foo : external[foo(void)] -> void
   ]}

   This is not allowed because there is no way to project an extern type without an explicit
   function provided with the implementation by the user.

   Type Variables of parametric extern types should be instanciated at runtime with :

   + polymorphic values [('a, 'b, etc...)].
   In this case, we have the garanty that the ocaml implementation
   will also work with opa values (if it works for any ['a], wy not with an opa value...)
   + opa values (not ocaml).
   In this case, they are manipulated in the implementation with the [ServerLib] API,
   and we know that there is no need for projection.
   + external types
   This means that at runtime, it will be Ocaml values manipulated from opa code,
   and the opa code does not need to enter the implementation of the type.
   In this case, there is no projection.

   Since when we compile the bypass lib, we have no idea about the final representation
   for opa values (even, what backend), we just trust the tag ["backend"].
   But don't worry, others checks are performed by the back-ends during
   the bypass projection.

   This function is just a check for detecting this kind of errors sooner. (at compile
   time of the extern library)

   @warning [bsl_runtime_restriction] if the type is illicit, but the tag ["runtime"]
   was provided

   @error if the type is illicit
*)
val check_runtime_restriction :
  BslTags.t -> (* reading tag runtime *)
  t -> unit

(** {6 Bslregister Code Generation} *)

(**
   Plugins and Loaders code generation, ocaml syntax.

   The application [bslregister] generates some files in which types are present, as
   meta ocaml code. It is one of the 2 supported ways of dealing with
   plugins (cf BslMarsahlLoader, too).

   To avoid conflict and too long strings, we generate [B.] as prefix of every
   constructor. Bslregister should insert :
   {[
   module B = BslTypes
   module Q = QmlAst
   ]}
   in the beginning of the generated files.
   In will also help the refactoring.

   This could actually be replaced by an [OcamlAst] production.
   The probleme is that [OcamlAst] is not yet stable, and we need bsl to be operational.

   This can be a possible TODO, for factorizing syntax formatting, and printing.
   It will also help to perform some optimizations (princiappaly, string sharing)
   and more safety : static checks, etc... but is actually not really needed.

   Position:

   The meta_pos will be the pos for every generated pos.
   The scope of typevars is fresh for each call to pp_meta.
*)
(** *)
val meta_pos : pos
val pp_meta : t pprinter
val pp_meta_fields : (string * t) list pprinter
