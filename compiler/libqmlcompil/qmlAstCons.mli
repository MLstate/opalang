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
   Qml Ast Constructors.

   Facilities for meta generation of ast (used in the trx parser of qml e.g.)

   @author Rudy Sicard
   @author Louis Gesbert
   @author Mikolaj Konarsky
   @author Valentin Gatien-Baron
   @author Esther Baruk
   @author Mathieu Barbin
   @author David Rajchenbach-Teller
*)


module ExprIdent :
sig
  val source : string -> QmlAst.ident
  val next : string -> QmlAst.ident
  val _type : ?check:bool -> string -> QmlAst.TypeIdent.t
end

(** Tuples *)
module Tuple :
sig
  val string : int -> string
  val typeident : int -> QmlAst.TypeIdent.t
  val field : int -> string
  val first_field : int

  (** {6 Qml couples} *)
  (**
     @deprecated use opa tuple instead
  *)
  (** *)
  val qml_fst : string
  val qml_snd : string
end



(** TYPES *)

module Type :
sig

  val typevar : QmlAst.typevar -> QmlAst.ty
  val next_var : unit -> QmlAst.ty

  module Row :
  sig
    exception Missing_field of string * string list

    val make : ?extend:bool -> (string * QmlAst.ty) list -> QmlAst.ty_row
    val make_qml_tuple : QmlAst.ty list -> QmlAst.ty
    val make_opa_tuple : QmlAst.ty list -> QmlAst.ty

    val to_list : QmlAst.ty_row -> (string * QmlAst.ty) list
    val to_list_and_extend : QmlAst.ty_row ->  (string * QmlAst.ty) list * QmlAst.rowvar option

    val map : (QmlAst.ty -> QmlAst.ty) -> QmlAst.ty_row -> QmlAst.ty_row
    val dot : QmlAst.ty_row -> string -> QmlAst.ty

    (** The ordered make apply a List.sort on the input list before building the record *)
    val ordered_make : ?extend:bool -> (string * QmlAst.ty) list -> QmlAst.ty_row

    val sort : QmlAst.ty_row -> QmlAst.ty_row
  end

  module Col :
  sig
    val make : ?extend:bool -> (string * QmlAst.ty) list list -> QmlAst.ty_col
    val sort : QmlAst.ty_col -> QmlAst.ty_col
  end


  module Arrow : sig
    (** transform arrow type into a list of type, or a list of args and a result type
        eventually result type can be expanded as a list using expandlast function:
        -expandlast is called using the current position of result type in list and result type
        (typically it used gamma and wanted type arity to do the expansion)
        note :
    *)
    val to_rev_list : ?expandlast:(int -> QmlAst.ty -> QmlAst.ty list) -> QmlAst.ty -> QmlAst.ty list
    val to_list     : ?expandlast:(int -> QmlAst.ty -> QmlAst.ty list) -> QmlAst.ty -> QmlAst.ty list
    val to_args_res : ?expandlast:(int -> QmlAst.ty -> QmlAst.ty list) -> QmlAst.ty -> (QmlAst.ty list * QmlAst.ty)

    (** [drop nb ty] Drop the first [nb] arguments of [ty]. Argument
        [ty] must be an arrow and must have at least [nb]
        arguments else it raise [Invalid_argument].
        @raise Invalid_argument when [ty] doesn't have at least [nb] arrows
    *)
    val drop : int -> QmlAst.ty -> QmlAst.ty
  end

  val sort : QmlAst.ty -> QmlAst.ty (* sorts all that needs to be sorted, recursively *)

end

(** UNTYPED EXPR *)
(** WARNING: no annotmap, no position ! *)
module UntypedExpr :
sig

  (** Copy an expression with annot refresh *)
  val copy : QmlAst.expr -> QmlAst.expr

  (**
     {6 Identifiers}
  *)
  val source : string -> QmlAst.expr
  val ident : QmlAst.ident -> QmlAst.expr
  val fresh_internal : string -> QmlAst.expr


  (**
     {6 Expressions}
  *)

  val const :  QmlAst.const_expr -> QmlAst.expr
  val int :    int -> QmlAst.expr
  val float :  float -> QmlAst.expr
  val string : string -> QmlAst.expr
  val unit :   unit -> QmlAst.expr
  val _false:  unit -> QmlAst.expr
  val _true:   unit -> QmlAst.expr
  val bool : bool -> QmlAst.expr

  val letin : (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr
  val letrecin : (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr
  val stupid_letin : (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr (* construct LetIn even when list is empty *)
  val lambda : QmlAst.ident list -> QmlAst.expr -> QmlAst.expr
  val apply : QmlAst.expr -> QmlAst.expr list -> QmlAst.expr

  val may_lambda : QmlAst.ident list -> QmlAst.expr -> QmlAst.expr
  val may_apply : QmlAst.expr -> QmlAst.expr list -> QmlAst.expr
  (** same as [lambda] or [apply], except when the list is empty. In this case, they
      just return the expression *)

  val match_ : QmlAst.expr -> (QmlAst.pat * QmlAst.expr) list -> QmlAst.expr
  val record : (string * QmlAst.expr) list -> QmlAst.expr
  val dot : QmlAst.expr -> string -> QmlAst.expr
  val extendrecord : string -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr
  val bypass : BslKey.t -> QmlAst.expr
  val restricted_bypass : pass:string -> BslKey.t -> QmlAst.expr
  val coerce : QmlAst.expr -> QmlAst.ty -> QmlAst.expr
  val directive : QmlAst.qml_directive -> QmlAst.expr list -> QmlAst.ty list -> QmlAst.expr

  (* parser : wait for type unification *)

  (** {6 Patterns}*)

  val patconst : QmlAst.const_expr -> QmlAst.pat
  val patvar : QmlAst.ident -> QmlAst.pat
  val patany : unit -> QmlAst.pat (** unit -> refresh annotation ! *)
  val patemptyrecord : unit -> QmlAst.pat
  val pat_opa_tuple : QmlAst.pat list -> QmlAst.pat
  val patcoerce : QmlAst.pat -> QmlAst.ty -> QmlAst.pat
  val pattrue: unit -> QmlAst.pat
  val patfalse:unit -> QmlAst.pat
  val patsome: QmlAst.ident -> QmlAst.pat
  val patnone:unit -> QmlAst.pat


  (** Sugar : produce patern on record (the terminaison can be '...' or closed) *)
  (** use fold_right : keep the order *)
  val patrecord : ?rowvar:QmlAst.pat_rowvar -> (string * QmlAst.pat) list -> QmlAst.pat
  val patextendrecord : (string * QmlAst.pat) list -> QmlAst.pat

  (** construct the pattern matching a list with the given subpatterns *)
  val patlist : QmlAst.pat list -> QmlAst.pat

  (** Helper to generate | x ->  where x is fresh *)
  val patfreshvar : string -> QmlAst.ident * QmlAst.pat

  (** Other sugar *)
  val list : QmlAst.expr list -> QmlAst.expr
  val qml_tuple : QmlAst.expr list -> QmlAst.expr
  val opa_tuple : QmlAst.expr list -> QmlAst.expr

  (** if sugar *)
  val ifthenelse : QmlAst.expr -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr
end

(** UnValRec : (ex Triple Val Rec) : see the documentation in qmlAst.ml *)
module UnValRec :
sig
  (** creates an unvalrec (cf. QmlAst ) *)
  val make : ( QmlAst.ident * QmlAst.expr ) list -> QmlAst.unvalrec

  (** sometime, there is no need to create the full unvalrec, but just some part of it *)
  val letrec : ( QmlAst.ident * QmlAst.expr ) list -> QmlAst.expr

  (** from a NewVal(Rec), gives a Let(Rec)In, the fiels list, and a rebuilder function ``à la Louis'' *)
  val make_let : QmlAst.code_elt -> ( string * QmlAst.ident * QmlAst.expr ) list * QmlAst.expr * (QmlAst.expr -> QmlAst.code_elt) * (Annot.t list * Annot.t list) list

  (** Extra Higher Level API : transformation of code in a unvalrec passe *)
  val unvalrec_code : QmlAst.code -> QmlAst.code

  (** from an expr, makes a NewVal [("_", expr)] and a rebuilder function *)
  val make_code_elt : QmlAst.expr -> QmlAst.code_elt * (QmlAst.code_elt -> QmlAst.expr)

  (** same, but the rebuilder function is from maped_code_elt (expecting a M_NewVal) *)
  val make_code_elt_maped : QmlAst.expr -> QmlAst.code_elt * (('a, 'b) QmlAst.maped_code_elt -> 'a)
end

module TypedExpr :
sig
  type annotmap = QmlAst.annotmap
  type gamma = QmlTypes.Env.t

  (**
     <!> Do not use the annotation of the given expression.
     Prefer using a specilized constructor
  *)
  val make : ?pos:FilePos.pos -> annotmap -> QmlAst.expr -> QmlAst.ty -> annotmap * QmlAst.expr

  (**
     Refresh the annotations in the given expression
     Copies only the restricted annot, i.e. ty, original, position (drop precise_let, precise_index)
  *)
  val copy : annotmap -> QmlAst.expr -> annotmap * QmlAst.expr
  val shallow_copy_new : annotmap_old:annotmap -> annotmap -> QmlAst.expr -> annotmap * QmlAst.expr
  val shallow_copy : annotmap -> QmlAst.expr -> annotmap * QmlAst.expr
  val shallow_copys : annotmap -> QmlAst.expr list -> annotmap * QmlAst.expr list
  (** Copy annotations of an expression from an old to a new annotmap *)
  val copy_new : annotmap_old:annotmap -> annotmap -> QmlAst.expr -> annotmap * QmlAst.expr
  val copy_new_when_possible : annotmap_old:annotmap -> annotmap -> QmlAst.expr -> annotmap * QmlAst.expr
  (** A special version with a trace (source -> dest intmap) *)
  val copy_with_trace : annotmap -> QmlAst.expr -> (annotmap * QmlAnnotMap.trace) * QmlAst.expr

  (**
     natives types
  *)
  val ty_string : QmlAst.ty
  val ty_int : QmlAst.ty
  val ty_float : QmlAst.ty

  val bypass : ?pos:FilePos.pos -> annotmap -> BslKey.t -> QmlAst.ty -> annotmap * QmlAst.expr
  val ident :  ?pos:FilePos.pos -> annotmap -> QmlAst.ident -> QmlAst.ty -> annotmap * QmlAst.expr
  val const :  ?pos:FilePos.pos -> annotmap -> QmlAst.const_expr -> annotmap * QmlAst.expr
  val unit :   ?pos:FilePos.pos -> annotmap -> annotmap * QmlAst.expr
  val cheap_void : ?pos:FilePos.pos -> annotmap -> gamma -> annotmap * QmlAst.expr
  val int :    ?pos:FilePos.pos -> annotmap -> int -> annotmap * QmlAst.expr
  val float :  ?pos:FilePos.pos -> annotmap -> float -> annotmap * QmlAst.expr
  val string : ?pos:FilePos.pos -> annotmap -> string -> annotmap * QmlAst.expr

  (* try to guess positions and types from given expressions, when possible *)
  val coerce :   annotmap -> QmlAst.expr -> QmlAst.ty -> annotmap * QmlAst.expr
  val letin :    annotmap -> (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> annotmap * QmlAst.expr
  val letrecin : annotmap -> (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> annotmap * QmlAst.expr

  (** lambda [(x : tx); (y : ty)] (m : t) : tx, ty -> t *)
  val lambda :  ?pos:FilePos.pos -> annotmap -> (QmlAst.ident * QmlAst.ty) list -> QmlAst.expr -> annotmap * QmlAst.expr
  (** same as lambda, but when the list is empty, returns [e] and not [-> e] *)
  val may_lambda :  ?pos:FilePos.pos -> annotmap -> (QmlAst.ident * QmlAst.ty) list -> QmlAst.expr -> annotmap * QmlAst.expr

  (** apply (m : tx, ty -> t) [(x : tx); (y : ty)] : t *)
  val apply : ?ty:QmlAst.ty -> gamma -> annotmap -> QmlAst.expr -> QmlAst.expr list -> annotmap * QmlAst.expr
  (** [apply_partial] allows to apply not enough arguments or to apply one group of arguments
      when the type has two arrows *)
  val apply_partial : gamma -> annotmap -> QmlAst.expr -> QmlAst.expr list -> annotmap * QmlAst.expr
  (** same as [may_lambda] *)
  val may_apply : gamma -> annotmap -> QmlAst.expr -> QmlAst.expr list -> annotmap * QmlAst.expr

  val apply_ty : ?pos:FilePos.pos -> annotmap -> QmlAst.expr -> QmlAst.expr list -> QmlAst.ty -> annotmap * QmlAst.expr

  val record :   ?pos:FilePos.pos -> ?extend:bool -> annotmap -> (string * QmlAst.expr) list -> annotmap * QmlAst.expr
  val sum_element : ?pos:FilePos.pos -> ?ty:QmlAst.ty -> annotmap -> (string * QmlAst.expr) list -> annotmap * QmlAst.expr
  val dot :      gamma -> annotmap -> QmlAst.expr -> string -> annotmap * QmlAst.expr

  (**{6 Match constructors}*)
  (** [match_ annotmap matched l] make a match. [matched] it's matched
      expression. And [l] is a list of patterns and expressions. All
      pattern, expression is an entry of constructed match. The type
      of match is the type of the first expression on [l]. [l] can't
      be empty.

      Warning : Types of expressions in [l] must be able to unify, but
      is not checked *)
  val match_ : ?pos:FilePos.pos -> annotmap -> QmlAst.expr -> (QmlAst.pat * QmlAst.expr) list
    -> annotmap * QmlAst.expr

  (**
     Same than [match_] but use the given [ty] instead of taking the
     type of the first production.
  *)
  val match_ty : ?pos:FilePos.pos -> annotmap -> QmlAst.expr -> (QmlAst.pat * QmlAst.expr) list
    -> QmlAst.ty -> annotmap * QmlAst.expr

(* TODO: *)
(*   val extendrecord : annotmap -> string -> QmlAst.expr -> QmlAst.expr -> annotmap * QmlAst.expr *)

  (**
     Use the module [QmlDirective] to guess the type of the node
  *)
  val directive: ?pos:FilePos.pos -> annotmap -> QmlAst.qml_directive -> QmlAst.expr list ->  QmlAst.ty list -> annotmap * QmlAst.expr

  (**
     Do not use [QmlDirective] fot guessing the type of the node,
     but use the given ty instead.
  *)
  val directive_ty: ?pos:FilePos.pos -> annotmap -> QmlAst.qml_directive -> QmlAst.expr list ->  QmlAst.ty list -> QmlAst.ty -> annotmap * QmlAst.expr
  val directive_id: ?pos:FilePos.pos -> annotmap -> QmlAst.qml_directive -> QmlAst.expr -> annotmap * QmlAst.expr

  val _false:?pos:FilePos.pos -> annotmap * gamma -> annotmap * QmlAst.expr
  val _true: ?pos:FilePos.pos -> annotmap * gamma -> annotmap * QmlAst.expr
  val bool: ?pos:FilePos.pos -> annotmap * gamma -> bool -> annotmap * QmlAst.expr

  val _false_no_named_type:?pos:FilePos.pos -> annotmap * gamma -> annotmap * QmlAst.expr
  val _true_no_named_type: ?pos:FilePos.pos -> annotmap * gamma -> annotmap * QmlAst.expr
  val bool_no_named_type: ?pos:FilePos.pos -> annotmap * gamma -> bool -> annotmap * QmlAst.expr

  val opa_tuple_2 :
    ?pos:FilePos.pos -> annotmap * gamma -> QmlAst.expr * QmlAst.expr -> annotmap * QmlAst.expr

  val lambda_coerce : annotmap -> QmlAst.ident -> QmlAst.ty (* of ident *) -> QmlAst.expr -> annotmap * QmlAst.expr

  (** {6 List constructors} *)
  (** Generate a qml list reversed compare to the given list.  *)
  val rev_list : ?pos:FilePos.pos -> ?ty:QmlAst.ty -> annotmap * gamma -> QmlAst.expr list
    -> annotmap * QmlAst.expr

  (** Like that [make_rev_list], but generated list isn't reversed. *)
  val list : ?pos:FilePos.pos -> ?ty:QmlAst.ty -> annotmap * gamma -> QmlAst.expr list -> annotmap * QmlAst.expr

  (** Construct a list from a function [f] that able to create a typed
      expression with an ['a], and from an ['a list]. Constructed qml
      list is reversed compared to the given list. Optional [ty]
      argument is type of generated qml list, by default it's a
      variant.

      WARNING : For consistency [f] must be generated typed expression
      with comparable types, but [make_rev_list] doesn't check
      that. *)
  val rev_list_map :
    ?pos:FilePos.pos -> ?ty:QmlAst.ty ->
    (annotmap -> 'a -> annotmap * QmlAst.expr) ->
    (annotmap * gamma) -> 'a list -> annotmap * QmlAst.expr

  (** Like that [make_rev_list], but generated list isn't reversed. *)
  val list_map :
    ?pos:FilePos.pos -> ?ty:QmlAst.ty ->
    (annotmap -> 'a -> annotmap * QmlAst.expr) ->
    (annotmap * gamma) -> 'a list -> annotmap * QmlAst.expr

  (** {Option constructors}*)
  (** [some expr] Generate an [option] expression, like that in Opa
      Syntax : [{some = expr}] *)
  val some : ?pos:FilePos.pos -> annotmap -> gamma -> QmlAst.expr -> annotmap * QmlAst.expr

  (** Generate an [option] expression, like that in Opa Syntax :
      [{none}]. [ty] optional argument it's the type of option, if it
      not given it's a type variable. *)
  val none : ?pos:FilePos.pos -> ?ty:QmlAst.ty -> annotmap -> gamma -> annotmap * QmlAst.expr

  val tagged_string : ?pos:FilePos.pos -> annotmap -> string -> QmlAst.tagged_string_kind -> annotmap * QmlAst.expr

end

(** Typed pattern and match constructor module

    @author Quentin Bourgerie

    Warning : I'm not sure of typing on field (needs review)
*)
module TypedPat :
sig
  (**{6 Shortcut types} *)

  type annotmap = QmlAst.annotmap

  type gamma = QmlTypes.Env.t

  (** same behaviour as in TypedExpr *)
  val copy : annotmap -> QmlAst.pat -> annotmap * QmlAst.pat
  val copy_new_when_possible : annotmap_old:annotmap -> annotmap -> QmlAst.pat -> annotmap * QmlAst.pat

  (**{6 Basic constructors}*)
  (**
     [patmake annotmap pat0 ty] Make a typed pattern from a untyped
     pattern ([pat]) and a type ([ty]). Returns updated annotmap and
     typed pattern.
     <!> The annotation of pat is not used, it is refreshed
  *)
  val make : ?pos:FilePos.pos -> annotmap -> QmlAst.pat -> QmlAst.ty -> annotmap * QmlAst.pat

  (** Make a any typed pattern. If [ty] is not given, type is a type
      variable *)
  val any : ?pos:FilePos.pos -> ?ty:QmlAst.ty -> annotmap -> annotmap * QmlAst.pat

  (** [patvar annotmap ident ty] Make a typed variable pattern from an
      [ident] and a type ([ty]). Returns updated annotmap and typed
      pattern.

      Warning : [patvar] don't check if the given [ident] is
      typed by [ty]. *)
  val var : ?pos:FilePos.pos -> annotmap -> QmlAst.ident -> QmlAst.ty -> annotmap * QmlAst.pat

  (** Make a typed pattern on empty record *)
  val emptyrecord : ?pos:FilePos.pos -> annotmap -> annotmap * QmlAst.pat

  (** {6 Record pattern constructors}*)
  (** [record ~extend annotmap fields] make a pattern on record with a
      list of fields. The first element of generated pattern it's the
      first element of the list. If [extend] is setted to [true] then
      the pattern is extensible, by default extend is [false]. *)
  val record : ?pos:FilePos.pos -> ?extend:bool ->
    annotmap -> (string * QmlAst.pat) list -> annotmap * QmlAst.pat

  val tuple : ?pos:FilePos.pos -> annotmap -> QmlAst.pat list -> annotmap * QmlAst.pat

  (** [some annotmap gamma pat] make a pattern on [{ some = pat
      }]. This pattern it's typed by type [option]. *)
  val some : ?pos:FilePos.pos -> annotmap -> gamma -> QmlAst.pat -> annotmap * QmlAst.pat

  (** Make a pattern on [{none}]. This pattern it's typed by type
      [option('a)] else if [ty] is given the it's typed by
      [option(ty)].*)
  val none : ?pos:FilePos.pos -> ?ty:QmlAst.ty -> annotmap -> gamma -> annotmap * QmlAst.pat

  val bool : ?pos:FilePos.pos -> bool -> annotmap -> gamma -> annotmap * QmlAst.pat

  (** [match_option annotmap gamma matched patsome ok_expr ko_expr]
      generate a match on option, like this in OPA syntax :

      [{
      match matched with
       | { some = patsome } -> ok_expr
       | {none} -> ko_expr
      }]

      @param matched Expression to match. This expression must be type of
      option.
      @param patsome Pattern in some.
      @param ok_expr Expression when match success.
      @param ko_expr Expression when match failed.
      @return (annotmap, match expression).
  *)
  val match_option : ?pos:FilePos.pos -> annotmap -> gamma -> QmlAst.expr -> QmlAst.pat
    -> QmlAst.expr -> QmlAst.expr -> annotmap * QmlAst.expr

  val list : ?pos:FilePos.pos -> annotmap -> QmlAst.pat list -> annotmap * QmlAst.pat

  val ifthenelse : ?pos:FilePos.pos -> annotmap -> gamma -> QmlAst.expr
    -> QmlAst.expr -> QmlAst.expr -> annotmap * QmlAst.expr
end

module TypedCode :
sig
  val copy_new : annotmap_old:QmlAst.annotmap -> QmlAst.annotmap -> QmlAst.code -> QmlAst.annotmap * QmlAst.code
  val copy_new_when_possible : annotmap_old:QmlAst.annotmap -> QmlAst.annotmap -> QmlAst.code -> QmlAst.annotmap * QmlAst.code
end

module UntypedExprWithLabel :
sig
  (**
     Usage of this module is discouraged: by giving explicitly an
     annotation, you are able to break the strong invariant that
     annotations are unique. It's generally better to use functions
     from UntypedExpr (maybe followed by a call to the typer) or from
     TypedExpr.
  *)

  val ident :  ?label:Annot.label -> QmlAst.ident -> QmlAst.expr
  val const :  ?label:Annot.label -> QmlAst.const_expr -> QmlAst.expr
  val int :    ?label:Annot.label -> int -> QmlAst.expr
  val float :  ?label:Annot.label -> float -> QmlAst.expr
  val string : ?label:Annot.label -> string -> QmlAst.expr

  val directive : ?label:Annot.label -> QmlAst.qml_directive -> QmlAst.expr list -> QmlAst.ty list -> QmlAst.expr

  val letin : ?label:Annot.label -> (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr
  val letrecin : ?label:Annot.label -> (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr
  val lambda1 : ?label:Annot.label -> QmlAst.ident -> QmlAst.expr -> QmlAst.expr
  val lambda : ?label:Annot.label -> QmlAst.ident list -> QmlAst.expr -> QmlAst.expr
  val apply1 : ?label:Annot.label -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr
  val apply : ?label:Annot.label -> QmlAst.expr -> QmlAst.expr list -> QmlAst.expr
  val may_apply : ?label:Annot.label -> QmlAst.expr -> QmlAst.expr list -> QmlAst.expr
  val match_ : ?label:Annot.label -> QmlAst.expr -> (QmlAst.pat * QmlAst.expr) list -> QmlAst.expr
  val record : ?label:Annot.label -> (string * QmlAst.expr) list -> QmlAst.expr
  val dot : ?label:Annot.label -> QmlAst.expr -> string -> QmlAst.expr
  val extendrecord : ?label:Annot.label -> string -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr
  val bypass : ?label:Annot.label -> BslKey.t -> QmlAst.expr
  val coerce : ?label:Annot.label -> QmlAst.expr -> QmlAst.ty -> QmlAst.expr
end

type stateful_constructor =
  < make : QmlAst.expr -> QmlAst.ty -> QmlAst.expr;
    make_from_annot : QmlAst.expr -> Annot.t -> QmlAst.expr;
    (* make_from_annotated : 'a. expr0 -> 'a QmlAst.annot -> QmlAst.expr; *)
    copy : QmlAst.expr -> QmlAst.expr;
    shallow_copy_new : annotmap_old:QmlAst.annotmap -> QmlAst.expr -> QmlAst.expr;
    shallow_copy : QmlAst.expr -> QmlAst.expr;
    copy_new : annotmap_old:QmlAst.annotmap -> QmlAst.expr -> QmlAst.expr;

    directive : QmlAst.qml_directive -> QmlAst.expr list -> QmlAst.ty list -> QmlAst.expr ;

    ident :  QmlAst.ident -> QmlAst.ty -> QmlAst.expr;
    ident_from_annot : QmlAst.ident -> Annot.t -> QmlAst.expr;
    (* ident_from_annotated : 'a. QmlAst.ident -> 'a QmlAst.annot -> QmlAst.expr; *)
    const :  QmlAst.const_expr -> QmlAst.expr;
    unit :   QmlAst.expr;
    cheap_void : QmlAst.expr;
    int :    int -> QmlAst.expr;
    float :  float -> QmlAst.expr;
    string : string -> QmlAst.expr;

    coerce : QmlAst.expr -> QmlAst.ty -> QmlAst.expr;
    letin : QmlAst.ident -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr;
    letins : (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr;
    letrec : QmlAst.ident -> QmlAst.expr -> QmlAst.expr -> QmlAst.expr;
    letrecs : (QmlAst.ident * QmlAst.expr) list -> QmlAst.expr -> QmlAst.expr;
    lambda : (QmlAst.ident * QmlAst.ty) list -> QmlAst.expr -> QmlAst.expr;
    lambda_from_annot : (QmlAst.ident * Annot.t) list -> QmlAst.expr -> QmlAst.expr;
    (* lambda_from_annotated : 'a. (QmlAst.ident * 'a QmlAst.annot) list -> QmlAst.expr -> QmlAst.expr; *)
    apply : QmlAst.expr -> QmlAst.expr list -> QmlAst.expr;

    record : (string * QmlAst.expr) list -> QmlAst.expr;
    dot : QmlAst.expr -> string -> QmlAst.expr;

    list : QmlAst.expr list -> QmlAst.expr;
    false_: QmlAst.expr;
    true_: QmlAst.expr;
    bool : bool -> QmlAst.expr ;

    opa_tuple_2 : QmlAst.expr * QmlAst.expr -> QmlAst.expr;

    some : QmlAst.expr -> QmlAst.expr;
    none : ?ty:QmlAst.ty -> unit -> QmlAst.expr;

    bypass : BslKey.t -> QmlAst.ty -> QmlAst.expr;
    bypass_from_annot : BslKey.t -> Annot.t -> QmlAst.expr;
    (* bypass_from_annotated : 'a. BslKey.t -> 'a QmlAst.annot -> QmlAst.expr; *)
    bypass_from_typer : BslKey.t -> (BslKey.t -> QmlAst.ty option) -> QmlAst.expr;

    patvar : QmlAst.ident -> QmlAst.ty -> QmlAst.pat;
    patany : QmlAst.pat;
    patlist : QmlAst.pat list -> QmlAst.pat;
    match_ : QmlAst.expr -> (QmlAst.pat * QmlAst.expr) list -> QmlAst.expr;

    typed : bool;
    gamma : TypedExpr.gamma;

    tyname : string -> QmlAst.ty list -> QmlAst.ty;
    tyoption : QmlAst.ty -> QmlAst.ty;
    tylist : QmlAst.ty -> QmlAst.ty;

    add_to_gamma : QmlAst.ident -> QmlAst.expr -> unit
  >

val make_typed_cons :
  TypedExpr.gamma -> TypedExpr.annotmap ->
    stateful_constructor * (unit -> (TypedExpr.gamma * TypedExpr.annotmap))
(**
   [make_cons gamma annotmap] allow you to write the same thing you would
   with calls to TypedExpr in a much more concise way since the object
   you create contains and updates the annotmap itself

   If you want to build the term [error("blabla")], you would do:
   {[[let cons, get_state = TypedExpr.make_cons gamma annotmap in
    let e = cons#apply (cons#ident ident <<string -> 'a>>) (cons#string "blabla") in
    get_state (), e ]}

   Instead of
   {[ let annotmap, ident = TypedExpr.ident annotmap ident <<string -> 'a>> in
     let annotmap, string_expr = TypedExpr.string annotmap "blabla" in
     let annotmap, app = TypedExpr.apply gamma annotmap ident string_expr in
     (gamma, annotmap), app
   ]}
   @return the constructor object and a function that gives you the new state
*)

val untyped_cons : stateful_constructor
(**
   same interface as [make_typed_cons _ _] except that the built terms
   are untyped
*)

val make_cons :
  typed:bool -> TypedExpr.gamma -> TypedExpr.annotmap ->
    stateful_constructor * (unit -> (TypedExpr.gamma * TypedExpr.annotmap))
(**
   Choose between the typed constructor and the untyped constructor
   depending on the [typed] argument
*)
