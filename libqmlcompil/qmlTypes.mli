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


(**)

type error = TyperError of QmlAst.code_elt * (exn * exn list) (** guard for a non empty list *)
exception Exception of error

(**
    Mathieu mercredi 24 juin 2009, 23:59:05 (UTC+0100)

    ABOUT GENERALIZATION and the dependancies between TypeScheme and TypeEnv
    ------------------------------------------------------------------------

    The generalization should normaly depends on gamma since the processus of
    generalization need to access all the free-vars of gamma.

    The academic type for function generalize is :

    [val generalize : gamma -> ty -> typescheme]

    But we don't want to have a mutuall dependancy between modules rec TypeScheme and
    TypeEnv !!
    (it was to difficult to get away of module rec, so let's try not to do it again)

    We need just a common representation define here for the "QmlTypeVars.quantif", and then
    provide a function in gamma :

    [val freevars : gamma -> QmlTypeVars.quantif]

    then TypeEnv can depend on TypeScheme.

    We do not want that this type to be public, but it must be shared between
    TypeScheme and TypeEnv

    That's why we define these 2 modules in the same file : QmlTypes.ml

    in the ml, the module are not coerced, implementation is shared.
    with this mli, we abstract the implementation of it for the rest of the world

    The implementation can only raise Exceptions defined in QmlTyperException
*)

(* public AST types and no constraints in public env: *)
type typescheme = (QmlAst.ty, unit) QmlGenericScheme.tsc
type gamma

(* ************************************************************************** *)
(** {b Descr}: Represents the number of chained type abbreviations this
    constructor leads to. For instance:
    - [int] : being a basic type, its height is 0.
    - [type t1 = int] : t1's height is 1 + int's height = 1.
    - [type t2 = t1] : 1 + t2's height = 2.

    Can also be negative. In this case, it represents the position (numbered
    from 1) of the definition's variable that will give (once the constructor
    of the definition will be used) the height of the resulting type expression.
    For instance:
     - [type u('a, 'b) = 'b] : The height of a type expression using [u] is
       the height of the effective argument provided to instantiate ['b]. Hence,
       u's height is -2 (minus of the second argument).
       Then, using [t2] above in the espression [u(t2, int)] will give a height
       of 0 ([int] has height 0) and [u(int, t2)] will give a height of 2
       ([t2] has height 2).
    {b Visibility}: Transparently visible outside this module. We do not hid
    its implementation since manipulations of heights are very frequent and
    we prefer to avoid some overhead induced by wrapping functions.
    In effect, such information is used by [QmlMakeTyper] on the QML side and
    its better to have it seeing the implementation. This information is also
    used by the low-level typer W.                                            *)
(* ************************************************************************** *)
type abbrev_height = int

type bypass_typer = BslKey.t -> QmlAst.ty option

(** the options are orthogonal; first three off give max speed *)
type options =
    {
      (*** general options about the behaviour of the typer *)
      (** see the OPA option --explicit-instantiation *)
      explicit_instantiation : bool;

      (** see the OPA option --value-restriction *)
      value_restriction : [`disabled|`normal|`strict];
    }

(** the safest, most complete (and slowest) set of options *)
val default_options : options

(** definition of annot moved to qmlAst *)

module type QML_LOW_LEVEL_TYPER =
sig
  val type_of_expr :
    ?options : options ->
    ?annotmap : QmlAst.annotmap ->
    bypass_typer : bypass_typer ->
    gamma : gamma ->
    QmlAst.expr ->
    gamma * QmlAst.annotmap * QmlAst.ty
end



(** Todo : Thing about a possible type for exportation (not necessary) *)
module FreeVars :
sig
  type t = QmlTypeVars.quantif

  val union : t -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val subset : t -> t -> bool
  val equal : t -> t -> bool

  val map : (QmlAst.typevar -> QmlAst.typevar) -> (QmlAst.rowvar -> QmlAst.rowvar) -> (QmlAst.colvar -> QmlAst.colvar) -> t -> t

  val empty : t
  val is_empty : t -> bool
  val is_type_empty : t -> bool
  val is_row_empty : t -> bool
  val is_col_empty : t -> bool

  val compare : t -> t -> int

  val mem_typevar : QmlAst.typevar -> t -> bool
  val mem_rowvar : QmlAst.rowvar -> t -> bool
  val mem_colvar : QmlAst.colvar -> t -> bool

  val add_ty : QmlAst.typevar -> t -> t
  val add_row : QmlAst.rowvar -> t -> t
  val add_col : QmlAst.colvar -> t -> t

  val refresh : t -> t
  val export_as_lists : t -> (QmlAst.typevar list * QmlAst.rowvar list * QmlAst.colvar list) (* TODO: remove when we use Scheme in HMX *)
  val import_from_sets : QmlTypeVars.TypeVarSet.t -> QmlTypeVars.RowVarSet.t -> QmlTypeVars.ColVarSet.t -> t (* TODO: remove when we use Scheme in HMX *)

  val to_string : t -> string
end

val freevars_of_ty :
  ?with_forall:bool -> ?free:FreeVars.t -> QmlAst.ty -> FreeVars.t
val freevars_of_row :
  ?with_forall:bool -> ?free:FreeVars.t -> QmlAst.ty_row -> FreeVars.t
val freevars_of_col :
  ?with_forall:bool -> ?free:FreeVars.t -> QmlAst.ty_col -> FreeVars.t

module Scheme :
sig
  type t = typescheme
  type renaming
  (* val to_string : t -> string (\** e.g : [Forall { 'a } : 'a -> int] *\) *)

  (* alpha-renaming to new, fresh type vars *)
  val refresh : t -> t
  val refresh_and_renaming : t -> t * renaming
  val apply_renaming : renaming -> QmlAst.ty -> QmlAst.ty
  val empty_renaming : renaming

  (** <<<<<<<<<<<<<<<<<<<<<<<< *)
  (** EXPORT : can be usefull for Named-type as well (keep it in API) *)
  val export : t -> (QmlAst.typevar list * QmlAst.rowvar list * QmlAst.colvar list) * QmlAst.ty
  (** IMPORT : use definition *)
  (** >>>>>>>>>>>>>>>>>>>>>>>> *)

  (** introduction of a new schema, without quantification (e.g. Lambda, or LetRecIn) *)
  (** the schema returned is : [Forall {} : 'a  where 'a is a fresh typevar] *)
  val next : QmlAst.typevar -> t

  val instantiate : t -> QmlAst.ty
    (** contain a refresh so that typescheme cannot be corrupted *)
    (** the refresh is done only on quantified vars, to be compatible with the w algorithm *)

  (**
      ABOUT GENERALIZATION :
      -----------------------

      With the value of first level, it should not be any non-closed schema,
      so gamma is not needed to generalize the type of such values --

      so it is possible to define :

      val quantify : ty -> t   ( returning a closed schema )

      a [generalization_with_gamma] is called only internly by the typer in a
      w algorithm at least so it should theoreticly not be put in the API,
      but then, since Scheme.t is private, the typer wont be able to provide
      the implementation for it

      +    However, if somebody writte a typer, he should know that the generalization
      needs the freevars of gamma and then he will use the function generalize, not the quantify
      function

      +    If a human user (not a typer guy) mistakes, and call the function generalize instead of
      quantify on a first level type of qml, it is equivalent (can just be less efficient)

      Both contain a refresh on quantified variables, to be sure that any vars of
      ty cannot appear in typescheme if ty will be corrupted later,
      typescheme keep clean *)

  val generalize : gamma -> QmlAst.ty -> t
  val quantify : QmlAst.ty -> t

 (**
     EXTRA API FOR TYPE DEFINITION AND NAME TYPES :
     ---------------------------------------------

     given a type, with some parameters (type definition -- [type ('a, 'b) = { a : 'a ; b : 'b } ]
     will build a schema, by verifying the well-formed property freevars(t) = params
     It will also check that the parameters are uniq :
     don't allow type ('a, 'a) toto = ....
     it possibly raises an TyperException (like arity problems, unbound typevars, etc ...)

     12/08/09, Adam: I add a restriction for recursive types, in the body of type t:
         [type ('a1, ... 'an) t]
       if there is a recursive reference to [t] it must be with _exactly the same_ parameters
       ('a1, ... 'an). If this is not the case QmlTyperException.WrongUseOfParameters is
       thrown
 *)
  val definition : ?typevar:QmlAst.typevar list -> ?ty_row:QmlAst.rowvar list -> QmlAst.typeident -> QmlAst.ty -> t
  (** if you find a TypeName ( [ int ; float], "toto") and you want to unify it with a other type,
      you will need to specialize the schema of your type scheme
      it possibly raises an TyperException (like arity problems, unbound typevars, etc ...)
      to help the error message, you must provide a TypeIdent.t (without it, the message is totally useless)

      if you don't provide any vars (all default arg are []), then the function is equivalent
      to the function [instantiate] if the schema has an empty quantification
      but will raise an exception otherwise
  *)
  val specialize : typeident:QmlAst.typeident -> ?ty:(QmlAst.ty list) -> ?ty_row:(QmlAst.ty_row list) -> t -> QmlAst.ty

  val id : QmlAst.ty -> t
  val explicit_forall : t -> QmlAst.ty
end

module Env :
sig
  type t = gamma

  val empty : t

  module Ident :
  sig
    val find_opt : QmlAst.ident -> gamma -> typescheme option
    val find : QmlAst.ident -> gamma -> typescheme
    val add : QmlAst.ident -> typescheme -> gamma -> gamma
    val remove : QmlAst.ident -> gamma -> gamma
    val mem : QmlAst.ident -> gamma -> bool
    val iter : (QmlAst.ident -> typescheme -> unit) -> gamma -> unit
    val fold : (QmlAst.ident -> typescheme -> 'a -> 'a) -> gamma -> 'a -> 'a
    val map : (typescheme -> typescheme) -> gamma -> gamma
    val fold_map : (QmlAst.ident -> typescheme -> 'acc -> 'acc * typescheme) -> gamma -> 'acc -> 'acc * gamma
    val from_map : typescheme IdentMap.t -> gamma -> gamma
    val to_map : gamma -> typescheme IdentMap.t
    val pp : Format.formatter -> gamma -> unit
  end

  module TypeIdent :
  sig
    val find_opt :
      visibility_applies: bool -> QmlAst.typeident -> gamma ->
        (typescheme * abbrev_height) option
    val findi_opt :
      visibility_applies: bool -> QmlAst.typeident -> gamma ->
        (QmlAst.typeident * (typescheme * abbrev_height)) option
    val find :
      visibility_applies: bool -> QmlAst.typeident -> gamma ->
        (typescheme * abbrev_height)
    val findi :
      visibility_applies: bool -> QmlAst.typeident -> gamma ->
        (QmlAst.typeident * (typescheme * abbrev_height))
   (* *********************************************************************** *)
   (** {b Descr}: Lookup in the environment for the type definition bound to
        a type name, ignoring the visibility (i.e. scope) of this name, and
        returning this visibility in addition to the bound definition.
        This function is dedicated to be used by the check that no private
        type espace by appearing in the signature of a toplevel value not marked
        as @private. For this reason, this processing needs to know the
        visibility of the type name.                                          *)
   (* *********************************************************************** *)
    val raw_find :
      QmlAst.typeident -> gamma ->
        (typescheme * abbrev_height * QmlAst.type_def_visibility)
    val add :
      QmlAst.typeident ->
        (typescheme * abbrev_height * QmlAst.type_def_visibility) -> gamma ->
          gamma
    val mem : QmlAst.typeident -> gamma -> bool
    val iter :
      (QmlAst.typeident ->
        (typescheme * abbrev_height * QmlAst.type_def_visibility) -> unit) ->
          gamma -> unit
    val fold :
      (QmlAst.typeident ->
        (typescheme * abbrev_height * QmlAst.type_def_visibility) ->
          'a -> 'a) ->
            gamma -> 'a -> 'a
    val to_list :
      gamma ->
        (QmlAst.typeident *
           (typescheme * abbrev_height * QmlAst.type_def_visibility))
        list
    val fold_map :
      (QmlAst.typeident ->
        (typescheme * abbrev_height * QmlAst.type_def_visibility) -> 'acc ->
         'acc * (typescheme * abbrev_height * QmlAst.type_def_visibility)) ->
      gamma -> 'acc -> 'acc * gamma
    val map :
      ((typescheme * abbrev_height * QmlAst.type_def_visibility) ->
         (typescheme * abbrev_height * QmlAst.type_def_visibility)) ->
      gamma -> gamma
    val pp : Format.formatter -> gamma -> unit
  end

  (** a map of field which update with every TypeIdent.add in gamma
      Given a field, return the TypeIdentSet of every type containing such a field *)
  module FieldMap :
  sig
    val find : string -> gamma -> QmlAst.typeident list
  end

  val pp : Format.formatter -> gamma -> unit

  (** Appends the definition in g2 to those of g1 *)
  val append : gamma -> gamma -> gamma

  (** with let type in, gamma can be updated with abstract type *)
  (** in fact, this module should not be here because it is possibly used
      by the typers only, typing such expr :

      [let  ... =
        let type toto = ... in <- from here the abstract type toto IS in gamma
         ....
      in] <-- from here the abstract type toto IS NO MORE in gamma

      It brings also confusion to be able to add abstract type in gamma, for example in
      a type definition

      [type ('a, 'b) toto = abstract]

      what should we do ? people would probably say, if we let an API in gamma to
      add abstracttype, that they should probably add toto in the abstract types
      map of gamma ! but it is not implemented in this way in our code !

      we must :

      1) create a new abstract type (extern)
      2) add in gamma in the type ident map the binding "toto" -> typescheme :
              [Forall {'a, 'b} : TypeAbstract ('a, 'b) ,  toto]

      with :
      [type ('a, 'b) toto = abstract]
      you don't know the implementation of the type toto

      with :
      [let type ('a, 'b) toto = { a : 'a ; b : 'b } in ..]
      you know the implementation, so you can both bind and a new type ident with it

  *)

(**

   HACKS IN GAMMA :
   ---------------

   we do not want that the typer use gamma like a set of hacks

   If something typer specific is missing in gamma, as long as it is not needed to
   be with dealed with a continuation, it is possible to have something like :

   (the typer inference should use a tuned gamma)

   [let w gamma expr =
     let env = { gamma = gamma ; private_extra_env = private_extra_env_empty } in
     let rec aux env e =
      ...  ... use env.gamma and env.private_extra_env
     in
     let typ = aux env e in
     typ]

   if something need continuation passing, let's talk about it, and maybe if it is
   really typer-generic and needed then it can be added in gamma
   (for example,  let's thing about add_intypingident & is_intypingident which are not necessarly in gamma !)
*)

(*   module Hacks : *)
(*   sig *)
(*     type add_your_hacks_here = unit *)
(*     val hack_api : add_your_hacks_here -> add_your_hacks_here *)
(*   end *)

end

(** More Common Types, needed in order that differents HighTyper could share the type env *)
type typed_code_elt = (QmlAst.ty, Scheme.t) QmlAst.maped_code_elt
(** Now the type env is public, so that we can also share it between HighTyper *)
(** avoid cyclic dependencies between QmlTypes and DbGen *)
type 'schema public_env =
    {
      (** The set of toplevel identifiers that are visible outside the package.
        It will be used to raise an error if a value has a type containing a
        @private type and this value is not marked also by a @private. This
        is to avoid private types escaping from their scope. *)
      exported_values_idents : IdentSet.t ;
      gamma        : gamma ;
      schema       : 'schema ;
      annotmap     : QmlAst.annotmap ;
      bypass_typer : bypass_typer ;
      had_error    : bool ;
      exception_handler : 'schema public_env -> exn -> unit ;
      display      : bool ;     (** false by default *)
      options : options ;
    }

(** typedef=true -> be strict about arguments of named types *)
val type_of_type :
  ?typedef:bool -> ?tirec:((QmlAst.typeident * QmlAst.typevar list) list) ->
    gamma -> QmlAst.ty -> (QmlAst.ty * int)
(*This function may raises an exception if you give it garbage (e.g. incorrect gamma) *)
(* : ... -> TypeIdent.raw ty -> TypeIdent.processed ty *)

val process_gamma :
  gamma:gamma (* the one that is processed and contains all types for the other one *) ->
  gamma (* the one to process *) -> gamma
val process_scheme : gamma -> typescheme -> (typescheme * abbrev_height)
val process_annotmap : gamma:gamma -> QmlAst.annotmap -> QmlAst.annotmap
val process_typenames_annotmap : gamma:gamma -> QmlAst.annotmap -> QmlAst.annotmap

(** fails if there are duplicate type definitions *)
val check_no_duplicate_type_defs : QmlAst.code -> unit
