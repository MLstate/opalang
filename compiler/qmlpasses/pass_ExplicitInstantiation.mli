(*
    Copyright © 2011, 2012 MLstate

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
   Explicit instantiation adds type abstractions and type applications
   throughout the code to resolve the \@typeof directive

   Only the necessary part of the code is instrumented in this way
   (when you don't use (transitively) \@typeof, your code is not
   rewritten)

   Preconditions: the code is typed

   Directives removed: \@typeof
*)




(** A map of top-level expressions published between client and server. This map
    binds the name of the published function and informations of an
    'ei_skeleton' function. This skeleton throws useless type arguments. These
    informations are the ident of the skeleton and how the skeleton has been
    generated.

    - [`one_lambda e] means the skeleton is composed by a uniq lambda, i.e. the
    type and casual arguments are in the same block. [e] is the size of the
    previous lifted lambda.

    - [`two_lambdas] means the type and casual arguments are in two separated
    blocks

    Where f and g are published:
    {[
      _v0_f = a, b -> \@typeof('b)
      _v0_g = a -> void
    ]}

    Rewrited as:
    {[

      _v0_f = \@lifted_lambda(1, (vvva, a, b -> vvva))
      // _v0_f -> (`one_lambda 0 , _v0 -> f)

      _v0_g = a -> void
      _v1_g = _ -> (a -> void)
      // _v0_g -> (`two_lambdas, _v1_g)
    ]}
*)
type published_map = (Annot.label * Ident.t * [`one_lambda of int | `two_lambdas]) option IdentMap.t
val published_ref : published_map ref

(** The link between current identifiers and the ones before slicing *)
val renaming_map : QmlRenamingMap.t ref


(** Building representation of types in Opa *)

(**
   The type of type representations
   See stdlib/package/stdlib/core/opatype.opa
*)
val opatype_type : QmlAst.ty
val oparow_type : QmlAst.ty
val opacol_type : QmlAst.ty

(** [ty_to_opaty ~side annotmap gamma ty] represents a type [ty] in Opa,
    as an Opa expression. The type of these OPA expressions is defined
    in the Opa standard library.

    @param side  on which side we are, meaningless when memoization is off
    @param annotmap  the annotmap for, e.g., creating new Opa expressions
    @param gamma  the environment with type definitions
    @param ty  type to be represented in Opa.

    @return Updated annotmap, and an Opa representation of [ty].
*)
val ty_to_opaty :
  side:[ `server | `client ] -> ?memoize:bool -> ?normalize:bool
  -> QmlAst.annotmap -> QmlTypes.gamma -> QmlAst.ty -> val_:(?side:[`client|`server] -> string -> Ident.t)
  -> QmlAst.annotmap * QmlAst.expr

(** [get_memoized_definitions gamma side] adds to gamma type schemes
    of the new top level definitions visible at [side], generated
    as a side effect by [ty_to_opaty]. *)
val get_memoized_definitions :
  QmlTypes.gamma -> [ `server | `client ]
  -> QmlTypes.gamma * QmlAst.code

(** A dummy representation of type that can be inserted when [process_code]
    will not be called on a given expression, e.g., because
    Explicit Instantiation is disabled by a compilation option. *)
val dummy_opaty :
  QmlAst.annotmap -> QmlAstCons.TypedExpr.gamma
  -> QmlAst.annotmap * QmlAst.expr

(**
   A specialized version of [ty_to_opaty] for opadoc.
   Scopes of variables are passed for normalization
   keeping names, and the function is returned for
   limiting object creation.
*)
val ty_to_opaty_for_opadoc :
  val_:(?side:[`client|`server] -> string -> Ident.t) ->
  gamma:QmlTypes.gamma ->
  annotmap:QmlAst.annotmap ->
  (
    QmlTypeVars.TypeVarPrint.scope ->
    QmlTypeVars.RowVarPrint.scope ->
    QmlTypeVars.ColVarPrint.scope ->
    QmlAst.ty -> QmlAst.expr
  )

(** Building representation of type schemes in Opa *)

(** The type of representation of type schemes. *)
val opatsc_type : QmlAst.ty

(** [tsc_to_opatsc ~side (annotmap, gamma) tsc] represents a type scheme [tsc]
    in Opa, as OPA expression (of type [QmlAst.expr]). The OPA type of this
    Opa expression is defined in file "specialisation.opa" as [OpaTsc.t].

    @param side  on which side we are
    @param annotmap  the annotmap for, e.g., creating new Opa expressions
    @param gamma  the environment with type definitions
    @param tsc  type scheme to be represented in Opa

    @return Updated annotmap, and an Opa expression that represents
    [tsc] in Opa.
*)
val tsc_to_opatsc :
  side:[ `server | `client ] -> val_:(?side:[`client|`server] -> string -> Ident.t) -> ?memoize:bool ->
  QmlAst.annotmap * QmlTypes.gamma -> QmlTypes.typescheme
  -> QmlAst.annotmap * QmlAst.expr


(** Transforming code to enable run-time explicit instantiation *)

(** [have_typeof ?set gamma annotmap qmlAst] extends [set] with with type
    variables that are used to compute the results of sll [\@typeof] directives
    within [qmlAst].

    @param set  initial set of variables, empty if not given
    @param gamma  the environment with type definitions
    @param annotmap  the annotmap for, e.g., looking up types of expressions
    @param qmlAst  the syntax tree to scan

    @return Extended set, with all variables meaningful for [\@typeof].
*)
val have_typeof :
  QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code
  -> QmlTypeVars.FreeVars.t

(** [process_code have_typeof gamma annotmap _published qmlAst] inserts
    directives for Explicit Instantiation into the code of [qmlAst].

    @param have_typeof  set of type variables to be considered meaningful for [\@typeof]
    @param gamma  the environment with type definitions
    @param annotmap  the annotmap for, e.g., looking up types of expressions
    @param _published  unused; currently [published_ref] is used instead
    @param qmlAst  the syntax tree to transform

    @return Transformed syntax tree, with extra type arguments represented by directives. Also, updated annotmap and environment with changed types for top level definitions.
 *)
val process_code :
  QmlTypeVars.FreeVars.t
  -> QmlTypes.gamma -> QmlAst.annotmap -> IdentSet.t -> QmlAst.code
  -> QmlAst.annotmap * QmlTypes.gamma * QmlAst.code


(**
   Takes the gamma of the current compilation unit after being modified by process_code
   and returns the gamma of the standard library after modification by process_code
*)
val get_stdlib_gamma : QmlTypes.gamma -> QmlTypes.gamma


(** [unprocess_code gamma annotmap qmlAst] eliminates Explicit Instantiation
    directives in code [qmlAst], replacing them with standard Opa code.

    @param gamma  the environment with type and value definitions
    @param annotmap  the annotmap for, e.g., looking up types of expressions
    @param qmlAst  the syntax tree to transform

    @return Updated annotmap and updated code with extra type arguments
    abstracted and applied using standard Opa code and with [\@typeof]
    directives implemented using the extra type arguments.
*)
val unprocess_code :
  val_:(?side:[`client|`server] -> string -> Ident.t) -> side:[ `server | `client ] ->
  QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code
  -> QmlAst.annotmap * QmlAst.code


(** Inserts code generating type definition environment accessible
    from within Opa code (for runtime explicit instantiation).

    @param gamma  the environment with type definitions
    @param annotmap  the annotmap for creating new Opa expressions

    @return Updated annotmap and code building the Opa representation
    of the type environment.
 *)
val generate_tsc_map_updates :
  val_:(?side:[`client|`server] -> string -> Ident.t) -> side:[ `server | `client ] ->
  ?memoize:bool -> local_typedefs:QmlAst.TypeIdentSet.t -> QmlTypes.gamma -> QmlAst.annotmap
  -> QmlAst.annotmap * QmlAst.code_elt

(**
   Initialize internal data structure for memoization of runtime gamma.
   The boolean parameter indicates if compositionnality should be use.
*)
val init_memoized_definitions : bool -> unit

(**
   Finalize internal data structure for memoization of runtime gamma
   The boolean parameter indicates if compositionnality should be use.
*)
val finalize_memoized_defintions : bool -> unit
