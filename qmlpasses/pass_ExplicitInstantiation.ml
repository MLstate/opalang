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

(*
  Explicit instantiation works mainly in 3 passes:
  1. compute the set of type variables that need to be propagated
  2. add @abtract_ty_arg and @apply_ty_arg where there are respectively
     generalization and instantiation of propagated type variables
  3. replace these directives by actual lambdas or apply nodes

  In addition to this, explicit instantiation also creates the runtime
  representation of the gamma.

  The preconditions of explicit instantiations are the following:
  - the ast should contain a tsc in the annotmap where there are generalization
    (which is generally under a newval or a let, but also on modules fields).
    When it is sure that no type propagation will occur, it can be omitted
    (if rewriting bypasses for instances).
  - the ast should contain a tsc_inst in the annotmap where there are instantiation
    (which is generally on identifiers, but also on modules accesses).
    Under the same conditions as above, it can be omitted.
  - the tsc at a generalization point and at a corresponding instantiation point
    should be the same not modulo alpha renaming
  - the types in the annotmap should be correct where there are tsc or tsc_inst, typeof,
    and remote calls. The other types don't matter.

  1. Computing the set of type variables
    To determine where there needs to be some type propagation, ei relies of the
    identity of type variables.
    It means that if you generate some typed code in the compiler between the
    typer and ei, and you duplicate types, then either both definitions will
    receive type variables, or none will.

    Ei propagates type variables:
    - to @typeof: all the variables that are free in the type of the inner expression
    - to remote calls: all the variables that are quantified in the scheme of the identifier

    Ei keeps a set of type variables that need to be propagated.
    Whenever ei sees a typeof or a remote calls, its free variables are added to the set.
    Whenever ei sees a generalization, it does nothing because it relies of the fact
    that the variable in the schema in identical to the one in the types.
    Whenever ei sees an instantiation, it finds the substitution that was applied on
    the instantiated scheme. All the variable appearing in type expressions that are images of
    propagated variables are also propagated:
      on the code: g = x -> identity([x]), there is a tsc_inst: 'a. 'a -> 'a on identity
      and a type list('b) on the node. The substitution is 'a -> list('b). If 'a was propagated
      (because someone coded a wierd identity with some debug printing inside for instance), then
      'b would be propagated too.

    Please note that this allows to propagate type variables only when necessary but
    doesn't work with explicit forall. When a function takes a polymorphic value,
    then it has no way of knowing whether it expects type variables or not. In doubt,
    it should give it type variables but:
    1. this is costly, it would mean propagate type variable to all functors for instance
    2. this is not done anyway, so for now, explicit polymorphism is evil.

  2. Instrumenting the code
   Once you have determined the set of propagated type variables, whenever you have an
   instantiation that instantiates at least one propagated type variable, you generate
   an @apply_ty_arg node containing all the (compile time) types that need to be applied.
   In the same way, when you see a generalization of a least one propagated type variable,
   it generates an @abstract_ty_arg node containing all the compile time types that need
   to be applied.
   Because value restriction disables the generalization of expressions containing side
   effects, the fact of adding abstractions never adds, removes of duplicates (observable)
   side effects.

   At this point, ei also updates the types where it adds the directives, by
   adding an arrow type on the expression that are @apply'ed, and on the expression
   that are @abstract'ed.
   Note that these types usually don't have the right arity (see section 3.), because
   ei should instead sometimes squash the new arrow with the type arrow of the existing
   expression.
   Ei also updates the gamma with these possibly crappy types.

   For all identifiers that can be called remotely, ei also generates an 'eta expansion'
   to which all the type variables are propagated (and not only the needed one).
   This identifier is not called by anyone at this point in the code, but it will be used
   in the code generated by opa_InsertRemote (this is what the published_map is for).

  3. Code generation per se
    This pass looks simple as it replaces @apply_ty_arg with applications, and
    @abstract_ty_arg with abstractions.
    But it is made more complicated by the early lambda lifting, since ei must be
    careful to preserve the 'lambda lifted'ness of the code, that is the fact
    that you can only have partial application on toplevel identifiers:
    f(x) = @typeof(x)
    _ = @partial_apply(f)
    must become:
    f(ty,x) = ty
    _(ty) = @partial_apply(f(ty))
    and not:
    f(ty)(x) = ty
    _(ty) = @partial_apply(f(ty))

    There is also a complication due to the fact that:
    f(x) = @typeof([x])
    and
    f() = @typeof([])
    must be compiled respectively to:
    f(ty,x) = list(ty)
    and
    f() = list('v-1)

    Ei traverses the whole ast to rewrite it. When doing that, it keeps the sets
    of type variables that are in the scope in an environment.
    When it sees an @abstract_ty_arg, it generates the runtime representation of
    the compile time types contained in it (using the environment described just
    above to know whether a compile time variable must be translated to
    an ident {Ident = "vvv234"}, or to the node {TyVar = "v-1"}).
    If the underlying node is a lambda that was lifted, ei adds the new arguments
    to the lambda. If not, then ei generates a different lambda (that will be
    re lifted later).
    When ei sees an @apply_ty_arg, it generates an apply node in very much the same way.
    If it is an application of a lifted lambda, then it adds arguments to the existing
    apply, or else it generates a new node.

*)


(* refactoring in progress *)

(* depends *)
module List = BaseList
module Map = Base.Map
module Format = Base.Format
module String = BaseString

(* alias *)
module Inspect = QmlTypesUtils.Inspect
module TypedExpr = QmlAstCons.TypedExpr
module TypeIdent = QmlAst.TypeIdent
module TypeIdentTable = QmlAst.TypeIdentTable
module TypeIdentSet = QmlAst.TypeIdentSet
module TypeVarMap = QmlTypeVars.TypeVarMap

(* shorthands *)
module Q = QmlAst

(* -- *)

(* Here a reference to published map *)
type published_map = (Annot.label * Ident.t * [`one_lambda | `two_lambdas]) option IdentMap.t
let published_ref = ref (IdentMap.empty : published_map)
(* Same hack as above, but for ei to update the link between current identifiers and the one before slicing *)
let renaming_map = ref QmlRenamingMap.empty

(*----------------------------------------*)
(*------ a few memoization utils ---------*)
(*----------------------------------------*)
module TySyntacticOrder =
struct
  type t = QmlAst.ty
  let compare = QmlAst.EqualsTy.compare
    (* FIXME: with this, sharing of explicit forall will not be good *)
end
module TylOrder =
struct
  type t = QmlAst.ty list
  let compare tyl1 tyl2 = List.make_compare TySyntacticOrder.compare tyl1 tyl2
end
module StylOrder =
struct
  type t = (string * QmlAst.ty) list
  let compare_field (field1,ty1) (field2,ty2) =
    let c = String.compare field1 field2 in
    if c <> 0 then c else
    TySyntacticOrder.compare ty1 ty2
  let compare styl1 styl2 = List.make_compare compare_field styl1 styl2
end
module TscOrder =
struct
  type t = QmlTypes.typescheme
  let compare = Pervasives.compare (* FIXME: as above *)
end
module TyvlOrder =
struct
  type t = QmlAst.typevar list
  let compare tyv1 tyv2 = List.make_compare QmlAst.TypeVar.compare tyv1 tyv2
end
module RowvlOrder =
struct
  type t = QmlAst.rowvar list
  let compare tyv1 tyv2 = List.make_compare QmlAst.RowVar.compare tyv1 tyv2
end
module ColvlOrder =
struct
  type t = QmlAst.colvar list
  let compare tyv1 tyv2 = List.make_compare QmlAst.ColVar.compare tyv1 tyv2
end
module QuantOrder =
struct
  type t = (QmlAst.typevar list * QmlAst.rowvar list * QmlAst.colvar list)
  let compare (v1,r1,c1) (v2,r2,c2) =
     match TyvlOrder.compare v1 v2 with
     | 0 -> (
         match RowvlOrder.compare r1 r2 with
         | 0 -> ColvlOrder.compare c1 c2
         | c -> c
       )
     | c -> c
end

module TyMap = Map.Make(TySyntacticOrder)
module TylMap = Map.Make(TylOrder)
module StylMap = Map.Make(StylOrder)
module TscMap = Map.Make(TscOrder)
module TyvlMap = Map.Make(TyvlOrder)
module RowvlMap = Map.Make(RowvlOrder)
module ColvlMap = Map.Make(ColvlOrder)
module QuantMap = Map.Make(QuantOrder)

type one_side_memo = {
  mutable memoty : (Ident.t * Q.ty) TyMap.t;
  mutable memotyl : (Ident.t * Q.ty) TylMap.t;
  mutable memostyl : (Ident.t * Q.ty) StylMap.t;
  mutable memotsc : (Ident.t * Q.ty) TscMap.t;
  mutable memotyvl : (Ident.t * Q.ty) TyvlMap.t;
  mutable memorowvl : (Ident.t * Q.ty) RowvlMap.t;
  mutable memocolvl : (Ident.t * Q.ty) ColvlMap.t;
  mutable memoquant : (Ident.t * Q.ty) QuantMap.t;
  mutable definitions : (Ident.t * Q.expr * Q.ty) list;
}

(* DO NOT USE a printer that normalizes variables here
   or that hides column variables, or anything wierd
   we need that the following properties hold (for all the printers):
   - pp_ty ty1 = pp_ty ty2 <=> ty1 = ty2 (at runtime)
   - the printers are functions (do not depend on a global state) and injective (no collision)
*)
let pp_ty = QmlPrint.sexp_ty
let pp_tyl f l = Format.fprintf f "(Tyl %a)" (Format.pp_list ",@ " (fun f ty -> Format.fprintf f "(%a)" pp_ty ty)) l
let pp_styl f l = Format.fprintf f "(Styl %a)" (Format.pp_list ",@ " (fun f (s,ty) -> Format.fprintf f "(%s: %a)" s pp_ty ty)) l
let pp_tsc = QmlPrint.sexp_tsc
let pp_tyvl f l = Format.fprintf f "(Tyvl %a)" (Format.pp_list ",@ " QmlPrint.sexp_tyv) l
let pp_rowvl f l = Format.fprintf f "(Rowvl %a)" (Format.pp_list ",@ " QmlPrint.sexp_rowv) l
let pp_colvl f l = Format.fprintf f "(Colvl %a)" (Format.pp_list ",@ " QmlPrint.sexp_colv) l
let pp_quant f (v,r,c) = Format.fprintf f "(Quant %a %a %a)" pp_tyvl v pp_rowvl r pp_colvl c

let make_memo () = {
  memoty = TyMap.empty;
  memotyl = TylMap.empty;
  memostyl = StylMap.empty;
  memotsc = TscMap.empty;
  memotyvl = TyvlMap.empty;
  memorowvl = RowvlMap.empty;
  memocolvl = ColvlMap.empty;
  memoquant = QuantMap.empty;
  definitions = [];
}

let server_memo = make_memo ()
let client_memo = make_memo ()

let reset_memo memo =
  memo.memoty <- TyMap.empty;
  memo.memotyl <- TylMap.empty;
  memo.memostyl <- StylMap.empty;
  memo.memotsc <- TscMap.empty;
  memo.memotyvl <- TyvlMap.empty;
  memo.memorowvl <- RowvlMap.empty;
  memo.memocolvl <- ColvlMap.empty;
  memo.memoquant <- QuantMap.empty;
  memo.definitions <- []

let same_ident (ident1,_) (ident2,_) = Ident.equal ident1 ident2

module type MEMO = sig
  type t
  val look_up : side:[< `server | `client ] -> t -> (Ident.t * QmlAst.ty)
  val def  : side:[< `server | `client ] -> t -> (Ident.t * QmlAst.ty) -> unit
  val ident : side:[< `server | `client ] -> t -> Ident.t
end

module MakeMemoUtils(M:BaseMapSig.S)
  (S:sig
     val extract : one_side_memo -> (Ident.t * Q.ty) M.t
     val assign : one_side_memo -> (Ident.t * Q.ty) M.t -> unit
     val pp : Format.formatter -> M.key -> unit
   end) : MEMO with type t = M.key =
struct
  type t = M.key
  let look_up ~side key =
    match side with
    | `server -> M.find key (S.extract server_memo)
    | `client -> M.find key (S.extract client_memo)

  let def ~side key value =
    match side with
    | `server -> S.assign server_memo (M.add key value (S.extract server_memo))
    | `client -> S.assign client_memo (M.add key value (S.extract client_memo))
  let ident ~side t =
    match side with
    | `client ->
        let s = Format.to_string S.pp t in
        Ident.fake_source ("ei_" ^ s) (* fake source because we want collisions between different packages
                                       * (in the js only, because in caml, identifiers are in different
                                       * scopes anyway) *)
    | `server ->
        Ident.next "memo_ty"
end


let compare_field (s1,_) (s2,_) = String.compare s1 s2
let compare_record record1 record2 = List.make_compare compare_field record1 record2
let sort_record fields = List.sort compare_field fields
let sort_sum sums = List.sort compare_record (List.map sort_record sums)

(* we need the property that typevars are generated in decreasing order
 * because [forall 'a,'b. ...] becomes [forall $lookup 1$,$lookup 0$. ...]
 * and since the quantifiers are sorted, we must have [$lookup 1$ < $lookup 0$] *)
module MakeNormalizer(VarMap : BaseMapSig.S)(Gen : sig val gen : int -> VarMap.key end) =
struct
  let map = ref VarMap.empty
  let lookup v = Gen.gen (VarMap.find v !map)
  let def =
    let count = ref (-1) in
    fun v ->
      incr count;
      map := VarMap.add v !count !map;
      lookup v
  (* this variable is used to normalize unbound variables *)
  let dummy = Gen.gen (-1)
end

(*
 * normalizing consists in replacing [forall 'a,'b,'c. ('a*'b) -> forall 'd,'e. ...] by
 * [forall v3,v2,v1. (v3*v2) -> forall v5,v4. ...]
 * this way, the sharing of the list of quantifiers of typeschemes is maximal
 *
 * (this is better than the solution that consists in giving the lower number to the
 * variable that occurs most often (which breaks some sharing of quantifiers lists)
 * to gain some sharing in the body of typeschemes and foralls
 * (plus that solution would make it harder to guarantee that variables in quantifiers
 * are in increasing order)
 *)
let normalize_ty_for_sharing ~propagated_vars ty =
  let module Var = MakeNormalizer(QmlTypeVars.TypeVarMap)(QmlTypeVars.CanonicalVar) in
  let module Row = MakeNormalizer(QmlTypeVars.RowVarMap)(QmlTypeVars.CanonicalRow) in
  let module Col = MakeNormalizer(QmlTypeVars.ColVarMap)(QmlTypeVars.CanonicalCol) in
  #<If:EXPL_INST_NORMALIZE$maxlevel 0>
    ty
  #<Else>
  QmlAstWalk.Type.self_traverse_map
    (fun self tra -> function
       | Q.TypeVar v as ty ->
           (try Q.TypeVar (Var.lookup v) with Not_found ->
              #<If:EXPL_INST_NORMALIZE$maxlevel 1>
                ty
              #<Else>
                if QmlTypeVars.FreeVars.mem_typevar v propagated_vars then ty
                else Q.TypeVar Var.dummy
                    (* this variable is used to normalize unbound variables *)
              #<End>
           )
       | Q.TypeRecord (Q.TyRow (l, o)) ->
           let o =
             match o with
             | None -> None
             | Some v -> (
                 try Some (Row.lookup v)
                 with Not_found ->
                   #<If:EXPL_INST_NORMALIZE$maxlevel 1>
                     o
                   #<Else>
                     if QmlTypeVars.FreeVars.mem_rowvar v propagated_vars then o
                     else Some Row.dummy
                   #<End>
               ) in
           let l = sort_record l in
           tra (Q.TypeRecord (Q.TyRow (l, o)))
       | Q.TypeSum (Q.TyCol (l,o)) ->
           let o =
             match o with
             | None -> None
             | Some v -> (
                 try Some (Col.lookup v)
                 with Not_found ->
                   #<If:EXPL_INST_NORMALIZE$maxlevel 1>
                     o
                   #<Else>
                     if QmlTypeVars.FreeVars.mem_colvar v propagated_vars then o
                     else Some Col.dummy
                   #<End>
               ) in
           let l = sort_sum l in
           tra (Q.TypeSum (Q.TyCol (l,o)))
       | Q.TypeForall (tyvl,rowvl,colvl,ty) ->
           let tyvl = List.map_right Var.def tyvl in
           let rowvl = List.map_right Row.def rowvl in
           let colvl = List.map_right Col.def colvl in
           let ty = self ty in
           Q.TypeForall (tyvl,rowvl,colvl,ty)
       | ty -> tra ty) ty
  #<End>

let normalize_tyrow_for_sharing ~propagated_vars tyrow =
  match normalize_ty_for_sharing ~propagated_vars (Q.TypeRecord tyrow) with
  | Q.TypeRecord r -> r
  | _ -> assert false

let normalize_tycol_for_sharing ~propagated_vars tycol =
  match normalize_ty_for_sharing ~propagated_vars (Q.TypeSum tycol) with
  | Q.TypeSum r -> r
  | _ -> assert false

let normalize_tsc_for_sharing ~propagated_vars tsc =
  #<If:EXPL_INST_NORMALIZE$maxlevel 0>
    tsc
  #<Else>
  (* FIXME: CLEAN IT *)
  let tv,rv,cv,ty =
    match normalize_ty_for_sharing ~propagated_vars (QmlTypes.Scheme.explicit_forall tsc) with
    | Q.TypeForall (tv,rv,cv,ty) -> tv,rv,cv,ty
    | ty -> [],[],[],ty in
  QmlGenericScheme.import (QmlTypeVars.FreeVars.import_from_sets
                             (QmlTypeVars.TypeVarSet.from_list tv)
                             (QmlTypeVars.RowVarSet.from_list rv)
                             (QmlTypeVars.ColVarSet.from_list cv)) ty ()
  #<End>

module MemoTy = MakeMemoUtils(TyMap)(
  struct
    let extract one_side_memo = one_side_memo.memoty
    let assign one_side_memo v = one_side_memo.memoty <- v
    let pp = pp_ty
  end)
module MemoTyl = MakeMemoUtils(TylMap)(
  struct
    let extract one_side_memo = one_side_memo.memotyl
    let assign one_side_memo v = one_side_memo.memotyl <- v
    let pp = pp_tyl
  end)
module MemoStyl = MakeMemoUtils(StylMap)(
  struct
    let extract one_side_memo = one_side_memo.memostyl
    let assign one_side_memo v = one_side_memo.memostyl <- v
    let pp = pp_styl
  end)
module MemoTsc = MakeMemoUtils(TscMap)(
  struct
    let extract one_side_memo = one_side_memo.memotsc
    let assign one_side_memo v = one_side_memo.memotsc <- v
    let pp = pp_tsc
  end)
module MemoTyvl = MakeMemoUtils(TyvlMap)(
  struct
    let extract one_side_memo = one_side_memo.memotyvl
    let assign one_side_memo v = one_side_memo.memotyvl <- v
    let pp = pp_tyvl
  end)
module MemoRowvl = MakeMemoUtils(RowvlMap)(
  struct
    let extract one_side_memo = one_side_memo.memorowvl
    let assign one_side_memo v = one_side_memo.memorowvl <- v
    let pp = pp_rowvl
  end)
module MemoColvl = MakeMemoUtils(ColvlMap)(
  struct
    let extract one_side_memo = one_side_memo.memocolvl
    let assign one_side_memo v = one_side_memo.memocolvl <- v
    let pp = pp_colvl
  end)
module MemoQuant = MakeMemoUtils(QuantMap)(
  struct
    let extract one_side_memo = one_side_memo.memoquant
    let assign one_side_memo v = one_side_memo.memoquant <- v
    let pp = pp_quant
  end)

let add_definition ~side v =
  match side with
  | `server -> server_memo.definitions <- v :: server_memo.definitions
  | `client -> client_memo.definitions <- v :: client_memo.definitions

(*------------------------------*)
(*------ end memo utils --------*)
(*------------------------------*)

(** Building representation of types in OPA *)

let opatype_type =
  Q.TypeName ([], QmlAst.TypeIdent.of_string Opacapi.Types.OpaType.ty)
let oparow_type =
  Q.TypeName ([], QmlAst.TypeIdent.of_string Opacapi.Types.OpaType.row)
let opacol_type =
  Q.TypeName ([], QmlAst.TypeIdent.of_string Opacapi.Types.OpaType.col)

let opatype_typevar =
  Q.TypeName ([], QmlAst.TypeIdent.of_string Opacapi.Types.OpaType.typevar)

(** Construct expression with one unit field named by the given name.
    Example : rep_of_unit_field annotmap "Toto" represents the OPA
    value { Toto } *)
let rep_of_unit_field annotmap gamma name =
  let annotmap, unit_expr = TypedExpr.cheap_void annotmap gamma in
  TypedExpr.record annotmap [name, unit_expr]

(** Construct an expression that represents type of simple value. This
    expression is typed by OpaType.ty_const. *)
let rep_of_const_ty annotmap gamma = function
  | Q.TyFloat -> rep_of_unit_field annotmap gamma "TyFloat"
  | Q.TyInt -> rep_of_unit_field annotmap gamma "TyInt"
  | Q.TyString -> rep_of_unit_field annotmap gamma "TyString"
  | Q.TyNull -> failwith("TyNull is never used in Opa")

type env = {
  side : [ `client | `server ];
  gamma : QmlTypes.gamma;
  propagated_vars : QmlTypeVars.FreeVars.t;
  val_ : ?side:[`client|`server] -> string -> Ident.t;
}

let make_env ~side ~gamma ~propagated_vars ~val_ = {
  side = (side :> [ `server | `client ]);
  gamma;
  propagated_vars;
  val_;
}

(*
  creates of function that build a list expression and memoizes
  every intermediate list, given a function that build each element
*)
module MemoList(S:sig type t end)(Memo:MEMO with type t = S.t list) :
sig
  val rep_of_list :
    memoize:bool -> (Q.annotmap -> S.t -> Q.annotmap * bool * Q.expr) ->
    env -> Q.annotmap -> Memo.t -> Q.annotmap * bool * Q.expr
end =
struct
  let rec rep_of_list ~memoize rep_of_elt env annotmap elt_list =
    try
      let ident, ty = Memo.look_up ~side:env.side elt_list in
      let annotmap, e = TypedExpr.ident annotmap ident ty in
      annotmap, true, e
    with Not_found ->
      match elt_list with
      | [] ->
          (* always sharing nil, whether we ask for it or not *)
          let annotmap, nil = rep_of_unit_field annotmap env.gamma "nil" in
          let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr nil) annotmap in
          let ident = Memo.ident ~side:env.side elt_list in
          (* not shared between all the MemoList instances, you can have several
           * definitions of nil (but at most one by application of this functor) *)
          add_definition ~side:env.side (ident,nil,ty);
          Memo.def ~side:env.side elt_list (ident,ty);
          let annotmap, e = TypedExpr.ident annotmap ident ty in
          annotmap, memoize, e
      | elt :: elts ->
          let annotmap, bool1, type_exprl = rep_of_list ~memoize rep_of_elt env annotmap elts in
          let annotmap, bool2, type_expr = rep_of_elt annotmap elt in
          let annotmap, cons = TypedExpr.record annotmap ["hd",type_expr;"tl", type_exprl] in
          if bool1 && bool2 then (
            let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr cons) annotmap in
            let ident = Memo.ident ~side:env.side elt_list in
            add_definition ~side:env.side (ident,cons,ty);
            Memo.def ~side:env.side elt_list (ident,ty);
            let annotmap, e = TypedExpr.ident annotmap ident ty in
            annotmap, true, e
          ) else
            annotmap, false, cons
end

module RepOfList = MemoList(struct type t = Q.ty end)(MemoTyl)
module RepOfFields = MemoList(struct type t = string * Q.ty end)(MemoStyl)
module RepOfVars = MemoList(struct type t = Q.typevar end)(MemoTyvl)
module RepOfRowVars = MemoList(struct type t = Q.rowvar end)(MemoRowvl)
module RepOfColVars = MemoList(struct type t = Q.colvar end)(MemoColvl)

(* TODO: normaliser les types en enlevant les privates pour commencer *)
let fold_left_forall_map f acc l =
  let rec aux f acc acc_bool acc_list = function
    | [] -> acc, acc_bool, List.rev acc_list
    | h :: t ->
        let acc, bool, h = f acc h in
        let acc_bool = acc_bool && bool in
        let acc_list = h :: acc_list in
        aux f acc acc_bool acc_list t in
  aux f acc true [] l

let opaty gamma = fst (QmlTypes.type_of_type gamma opatype_type)
let oparow gamma = fst (QmlTypes.type_of_type gamma oparow_type)
let opacol gamma = fst (QmlTypes.type_of_type gamma opacol_type)

let id_of_var_gen kind s_v =
  let s_v = String.sub s_v 1 (String.length s_v - 1) in
  let digest s = String.sub (Digest.to_hex (Digest.string s)) 0 8 in
  let s_v = kind ^ (if Base.String.is_word s_v then s_v else digest s_v) in
  Ident.source s_v (* TODO: use Ident.next and the maps from strings to ids *)
let id_of_var v =
  id_of_var_gen "vvv" (QmlTypeVars.TypeVar.to_string v)
let id_of_rowvar v =
  id_of_var_gen "rrr" (QmlTypeVars.RowVar.to_string v)
let id_of_colvar v =
  id_of_var_gen "ccc" (QmlTypeVars.ColVar.to_string v)

class dynamic_repr env =
object (self)

  method const_ty annotmap ty =
    let annotmap, e =
      match ty with
      | Q.TyFloat -> rep_of_unit_field annotmap env.gamma "TyFloat"
      | Q.TyInt -> rep_of_unit_field annotmap env.gamma "TyInt"
      | Q.TyString -> rep_of_unit_field annotmap env.gamma "TyString"
      | Q.TyNull -> failwith "TyNull is never used in Opa" in
    annotmap, true, e

  method typevar annotmap var =
    if QmlTypeVars.FreeVars.mem_typevar var env.propagated_vars then
      let opaty = opaty env.gamma in
      let annotmap, e = QmlAstCons.TypedExpr.ident annotmap (id_of_var var) opaty in
      annotmap, false, e
    else (
      #<If:EXPL_INST_DEBUG>
        prerr_endline
        (Printf.sprintf
           "Warning: raw type variable: %s"
           (QmlTypeVars.TypeVar.to_string var))
      #<End>;
      let annotmap, string_expr = self#quantified_var annotmap var in
      let annotmap, e = TypedExpr.record annotmap ["TyVar", string_expr] in
      annotmap, true, e
    )

  method rowvar annotmap rowvar =
    if QmlTypeVars.FreeVars.mem_rowvar rowvar env.propagated_vars then
      let oparow = oparow env.gamma in
      let annotmap, e = QmlAstCons.TypedExpr.ident annotmap (id_of_rowvar rowvar) oparow in
      annotmap, false, e
    else
      let annotmap, string_expr = self#quantified_rowvar annotmap rowvar in
      annotmap, true, string_expr

  method colvar annotmap colvar =
    if QmlTypeVars.FreeVars.mem_colvar colvar env.propagated_vars then
      let opacol = opacol env.gamma in
      let annotmap, e = QmlAstCons.TypedExpr.ident annotmap (id_of_colvar colvar) opacol in
      annotmap, false, e
    else
      let annotmap, string_expr = self#quantified_colvar annotmap colvar in
      annotmap, true, string_expr

  method type_list annotmap tyl =
    let annotmap, forall, tyl = fold_left_forall_map self#type_ annotmap tyl in
    let annotmap, e = TypedExpr.list (annotmap, env.gamma) tyl in
    annotmap, forall, e

  method field annotmap (s,ty) =
    let annotmap, string_expr = TypedExpr.string annotmap s in
    let annotmap, forall, type_expr = self#type_ annotmap ty in
    let annotmap, e = TypedExpr.record annotmap [("label", string_expr); ("ty", type_expr)] in
    annotmap, forall, e

  method fields annotmap styl =
    let annotmap, forall, styl = fold_left_forall_map self#field annotmap styl in
    let annotmap, e = TypedExpr.list (annotmap, env.gamma) styl in
    annotmap, forall, e

  method fieldss annotmap styll =
    let annotmap, forall, styll = fold_left_forall_map self#fields annotmap styll in
    let annotmap, e = TypedExpr.list (annotmap, env.gamma) styll in
    annotmap, forall, e

  method typevar_to_string = QmlTypeVars.TypeVar.to_string
  method rowvar_to_string = QmlTypeVars.RowVar.to_string
  method colvar_to_string = QmlTypeVars.ColVar.to_string

  method quantified_var annotmap var = TypedExpr.string annotmap (self#typevar_to_string var)
  method quantified_rowvar annotmap var = TypedExpr.string annotmap (self#rowvar_to_string var)
  method quantified_colvar annotmap var = TypedExpr.string annotmap (self#colvar_to_string var)

  method quantified_vars annotmap vars rows cols =
    let annotmap, vars = List.fold_left_map self#quantified_var annotmap vars in
    let annotmap, rows = List.fold_left_map self#quantified_rowvar annotmap rows in
    let annotmap, cols = List.fold_left_map self#quantified_colvar annotmap cols in
    let annotmap, vars = TypedExpr.list (annotmap, env.gamma) vars in
    let annotmap, rows = TypedExpr.list (annotmap, env.gamma) rows in
    let annotmap, cols = TypedExpr.list (annotmap, env.gamma) cols in
    annotmap, vars, rows, cols

  method quantifier annotmap (vars, rows, cols) =
    let annotmap, vars, rows, cols = self#quantified_vars annotmap vars rows cols in
    TypedExpr.record annotmap [
      "types", vars;
      "rows", rows;
      "cols", cols;
    ]

  method type_row annotmap (Q.TyRow (fields, rowvar)) =
    let annotmap, bool1, fields_expr = self#fields annotmap fields in
    begin match rowvar with
    | None ->
        let annotmap, e = TypedExpr.record annotmap ["TyRecord_row", fields_expr] in
        annotmap, bool1, e
    | Some rowvar ->
        let annotmap, bool2, string_expr = self#rowvar annotmap rowvar in
        if bool2 then
          (* dummy row var *)
          let annotmap, e =
            TypedExpr.record annotmap [("TyRecord_row", fields_expr);
                                       ("TyRecord_rowvar", string_expr)] in
          annotmap, bool1 && bool2, e
        else
          if fields = [] then
            (* slight optimization, not need to merge with the empty list of fields *)
            annotmap, false, string_expr
          else
            (* propagated row var *)
            let ident = env.val_ ~side:env.side Opacapi.OpaType.instantiate_row in
            (* instantiate_row is used to merge the two lists of fields,
               the one from fields_expr and the one from the value of string_expr *)
            (* FIXME: give it a real type *)
            let annotmap, ident = TypedExpr.ident annotmap ident (QmlAstCons.Type.next_var ()) in
            let annotmap, app = TypedExpr.apply env.gamma annotmap ident [fields_expr; string_expr] in
            annotmap, false, app
    end

  method type_col annotmap (Q.TyCol (fields_list, colvar)) =
    let annotmap, bool1, fields_list_expr = self#fieldss annotmap fields_list in
    begin match colvar with
    | None ->
        let annotmap, e = TypedExpr.record annotmap ["TySum_col", fields_list_expr] in
        annotmap, bool1, e
    | Some colvar ->
        let annotmap, bool2, string_expr = self#colvar annotmap colvar in
        if bool2 then
          (* dummy col var *)
          let annotmap, e =
            TypedExpr.record annotmap [("TySum_col", fields_list_expr);
                                       ("TySum_colvar", string_expr)] in
          annotmap, bool1 && bool2, e
        else
          if fields_list = [] then
            (* slight optimization, not need to merge with the empty list of fields *)
            annotmap, false, string_expr
          else
            (* propagated col var *)
            let ident = env.val_ ~side:env.side Opacapi.OpaType.instantiate_col in
            (* instantiate_col is used to merge the two lists of lists of fields,
               the one from fields_list_expr and the one from the value of string_expr *)
            (* FIXME: give it a real type *)
            let annotmap, ident = TypedExpr.ident annotmap ident (QmlAstCons.Type.next_var ()) in
            let annotmap, app = TypedExpr.apply env.gamma annotmap ident [fields_list_expr; string_expr] in
            annotmap, false, app
    end


  method type_ annotmap ty =
    match ty with
    | Q.TypeConst ty ->
        let annotmap, forall, ty_const_expr = self#const_ty annotmap ty in
        let annotmap, e = TypedExpr.record annotmap ["TyConst", ty_const_expr] in
        annotmap, forall, e

    | Q.TypeVar ty ->
        self#typevar annotmap ty

    | Q.TypeArrow (lt, t) ->
        let annotmap, bool1, lt = self#type_list annotmap lt in
        let annotmap, bool2, t = self#type_ annotmap t in
        let annotmap, e = TypedExpr.record annotmap [("TyArrow_params", lt); ("TyArrow_res", t)] in
        annotmap, bool1 && bool2, e

    | Q.TypeRecord type_row ->
        self#type_row annotmap type_row

    | Q.TypeSum type_col ->
        self#type_col annotmap type_col

    | Q.TypeSumSugar ty_list ->
        (* This case was restored for opadoc *)
        let annotmap, forall, list_expr = self#type_list annotmap ty_list in
        let annotmap, e = TypedExpr.record annotmap ["TySumSugar", list_expr] in
        annotmap, forall, e

    | Q.TypeName (ty_list, ident) ->
        let annotmap, forall, list_expr = self#type_list annotmap ty_list in
        let register_key = Q.TypeIdent.to_string ident in
        let annotmap, ident_expr = TypedExpr.tagged_string annotmap register_key Q.Type_use in
        let annotmap, e = TypedExpr.record annotmap [("TyName_args", list_expr);
                                                     ("TyName_ident", ident_expr)] in
        annotmap, forall, e

    | Q.TypeAbstract ->
        let annotmap, e = rep_of_unit_field annotmap env.gamma "TyAbstract" in
        annotmap, true, e

    | Q.TypeForall (vars, rows, cols, ty) ->
        self#forall annotmap vars rows cols ty

  method forall annotmap vars rows cols ty =
    let annotmap, quant = self#quantifier annotmap (vars, rows, cols) in
    let annotmap, forall, ty = self#type_ annotmap ty in
    let annotmap, e = TypedExpr.record annotmap [("TyForall_quant", quant);("TyForall_body", ty)] in
    annotmap, forall, e

  method tsc annotmap tsc =
    let (quantif, body, ()) = QmlGenericScheme.export_unsafe tsc in
    let quant = QmlTypeVars.FreeVars.export_as_lists quantif in
    let annotmap, quant = self#quantifier annotmap quant in
    let annotmap, _, body = self#type_ annotmap body in
    QmlAstCons.TypedExpr.record annotmap [("quantifier", quant); ("body", body)]

end

class dynamic_repr_memoization env =
object (self)

  inherit dynamic_repr env as super

  method! type_ annotmap ty =
    try
      let ident, id_ty = MemoTy.look_up ~side:env.side ty in
      let annotmap, e = TypedExpr.ident annotmap ident id_ty in
      annotmap, true, e (* FIXME Should we say true here ?*)
    with Not_found ->
      let annotmap, can_memo, content = super#type_ annotmap ty in
      let should_memo = true (*
        match ty with
        | Q.TypeRecord (Q.TyRow (l, _)) -> List.length l <= 4
        | Q.TypeSum (Q.TyCol (l, _)) -> List.length l <= 4 && List.for_all (fun l -> List.length l <= 4) l
        | _ -> true *) in (* FIXME? condition on the depth also *)
      if can_memo && should_memo then (
        let ty_res = QmlAnnotMap.find_ty (Q.QAnnot.expr content) annotmap in
        let ident =
          match content with
          | Q.Ident (_, ident) -> ident (* avoid creating aliases *)
          | _ ->
              let ident = MemoTy.ident ~side:env.side ty in
              add_definition ~side:env.side (ident,content,ty_res);
              ident in
        MemoTy.def ~side:env.side ty (ident,ty_res);
        let annotmap, e = TypedExpr.ident annotmap ident ty_res in
        annotmap, can_memo, e
      ) else
        (annotmap, false, content)

  method! fields annotmap styl =
    (*match styl with
    | [] ->
        let should_memo = true in
        RepOfFields.rep_of_list ~memoize:should_memo self#field env annotmap styl
    | _ -> super#fields annotmap styl*)
    let should_memo = true (*List.length styl <= 4*) in
    RepOfFields.rep_of_list ~memoize:should_memo
      self#field
      env annotmap styl

  method! type_list annotmap tyl =
    let should_memo = true in (* ?? *)
    RepOfList.rep_of_list ~memoize:should_memo
      self#type_
      env annotmap tyl

  (*method! fieldss annotmap styll =
    let annotmap, forall, e = super#fieldss annotmap styll in
    let forall = forall (*&& List.length styll <= 4 && List.for_all (fun l -> List.length l <= 4) styll*) in
    annotmap, forall, e*)

  method! quantified_vars annotmap vars rowvars colvars =
    let should_memo = true in
    let annotmap, memo1, vars =
      RepOfVars.rep_of_list ~memoize:should_memo
        (fun annotmap var ->
           let annotmap, var = self#quantified_var annotmap var in
           annotmap, true, var)
        env annotmap vars in
    let annotmap, memo2, rowvars =
      RepOfRowVars.rep_of_list ~memoize:should_memo
        (fun annotmap var ->
           let annotmap, var = self#quantified_rowvar annotmap var in
           annotmap, true, var)
        env annotmap rowvars in
    let annotmap, memo3, colvars =
      RepOfColVars.rep_of_list ~memoize:should_memo
        (fun annotmap var ->
           let annotmap, var = self#quantified_colvar annotmap var in
           annotmap, true, var)
        env annotmap colvars in
    assert (memo1 && memo2 && memo3);
    annotmap, vars, rowvars, colvars

  method! quantifier annotmap quant =
    try
      let ident, ty = MemoQuant.look_up ~side:env.side quant in
      TypedExpr.ident annotmap ident ty
    with Not_found ->
      let annotmap, content = super#quantifier annotmap quant in
      let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr content) annotmap in
      let ident = MemoQuant.ident ~side:env.side quant in
      add_definition ~side:env.side (ident,content,ty);
      MemoQuant.def ~side:env.side quant (ident,ty);
      TypedExpr.ident annotmap ident ty

  method! tsc annotmap tsc =
    try
      let ident, ty = MemoTsc.look_up ~side:env.side tsc in
      TypedExpr.ident annotmap ident ty
    with
    | Not_found ->
        let annotmap, content = super#tsc annotmap tsc in
        let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr content) annotmap in
        let ident = MemoTsc.ident ~side:env.side tsc in
        add_definition ~side:env.side (ident,content,ty);
        MemoTsc.def ~side:env.side tsc (ident,ty);
        TypedExpr.ident annotmap ident ty
end

(*
  Used by opadoc api generation for normalizing type variables
*)
class dynamic_repr_for_opadoc env =
object
  inherit dynamic_repr env as super

  val mutable typevar_scope = QmlTypeVars.TypeVarPrint.new_scope ()
  val mutable rowvar_scope = QmlTypeVars.RowVarPrint.new_scope ()
  val mutable colvar_scope = QmlTypeVars.ColVarPrint.new_scope ()

  method set_scopes typevar rowvar colvar =
    typevar_scope <- typevar ;
    rowvar_scope <- rowvar ;
    colvar_scope <- colvar

  method! forall annotmap vars rows cols ty =
    QmlTypeVars.TypeVarPrint.push typevar_scope ;
    QmlTypeVars.RowVarPrint.push rowvar_scope ;
    QmlTypeVars.ColVarPrint.push colvar_scope ;
    let result = super#forall annotmap vars rows cols ty in
    QmlTypeVars.TypeVarPrint.pop typevar_scope ;
    QmlTypeVars.RowVarPrint.pop rowvar_scope ;
    QmlTypeVars.ColVarPrint.pop colvar_scope ;
    result

  method! typevar_to_string var = QmlTypeVars.TypeVarPrint.get typevar_scope var
  method! rowvar_to_string var = QmlTypeVars.RowVarPrint.get rowvar_scope var
  method! colvar_to_string var = QmlTypeVars.ColVarPrint.get colvar_scope var
end

let ty_to_opaty_for_opadoc ~val_ ~gamma ~annotmap =
  let propagated_vars = QmlTypeVars.FreeVars.empty in
  let side = `server in
  let env = make_env ~side ~propagated_vars ~gamma ~val_ in
  let dynamic_repr = new dynamic_repr_for_opadoc env in
  let ty tyvar rowvar colvar ty =
    dynamic_repr#set_scopes tyvar rowvar colvar ;
    let _, _, expr = dynamic_repr#type_ annotmap ty in
    expr
  in
  ty

let rep_of_vars ~memoize ~env annotmap quant =
  if memoize then
    (new dynamic_repr_memoization env)#quantified_vars annotmap quant
  else
    (new dynamic_repr env)#quantified_vars annotmap quant

let rep_of_type_gen call_method ~val_ ~side ~memoize ~propagated_vars ~annotmap ~gamma ty =
  let env = make_env ~side ~propagated_vars ~gamma ~val_ in
  let annotmap, _, e =
    if memoize then
      call_method (new dynamic_repr_memoization env) annotmap ty
    else
      call_method (new dynamic_repr env) annotmap ty in
  annotmap, e

let rep_of_type =
  rep_of_type_gen (fun obj -> obj#type_)
(*let rep_of_type_row =
  rep_of_type_gen (fun obj -> obj#type_row)
let rep_of_type_col =
  rep_of_type_gen (fun obj -> obj#type_col)*)

let default_memoize = #<If:EXPL_INST_NO_MEMO>false#<Else>true#<End>
let internal_ty_to_opaty_gen normalizer call_method ~side ?(normalize=true) ?(memoize=default_memoize) ~propagated_vars ~annotmap ~gamma ty =
  let ty =
    if normalize then normalizer ~propagated_vars ty
    else ty in
  rep_of_type_gen call_method ~side ~memoize ~propagated_vars ~annotmap ~gamma ty
let internal_ty_to_opaty =
  internal_ty_to_opaty_gen normalize_ty_for_sharing (fun obj -> obj#type_)
let internal_tyrow_to_opaty =
  internal_ty_to_opaty_gen normalize_tyrow_for_sharing (fun obj -> obj#type_row)
let internal_tycol_to_opaty =
  internal_ty_to_opaty_gen normalize_tycol_for_sharing (fun obj -> obj#type_col)

(* this function is not useless, it 'discards' optional parameters *)
let ty_to_opaty ~side ?(memoize=default_memoize) ?(normalize=true) annotmap gamma ty =
  internal_ty_to_opaty ~side ~memoize ~normalize ~propagated_vars:QmlTypeVars.FreeVars.empty ~annotmap ~gamma ty

(* to use when the pass explicit_instantiation is disabled *)
let dummy_opaty annotmap gamma =
  let annotmap, e = rep_of_unit_field annotmap gamma "TyAbstract" in
  let opaty = opaty gamma in
  let annotmap = QmlAnnotMap.add_ty (Q.QAnnot.expr e) opaty annotmap in
  annotmap, e


(** Building representation of type schems in OPA *)

let opatsc_type =
  Q.TypeName ([], QmlAst.TypeIdent.of_string Opacapi.Types.OpaTsc.t)

let tsc_to_opatsc ~side ~val_ ?(memoize=default_memoize) (annotmap, gamma) (tsc : QmlTypes.typescheme) =
  let propagated_vars = QmlTypeVars.FreeVars.empty in
  let env = make_env ~side ~propagated_vars ~gamma ~val_ in
  let tsc = normalize_tsc_for_sharing ~propagated_vars tsc in
  if memoize then
    (new dynamic_repr_memoization env)#tsc annotmap tsc
  else
    (new dynamic_repr env)#tsc annotmap tsc

let arg_of_type ~val_ side set gamma annotmap t =
  internal_ty_to_opaty ~val_ ~side ~propagated_vars:set ~annotmap ~gamma t
let arg_of_type_row ~val_ side set gamma annotmap t =
  internal_tyrow_to_opaty ~val_ ~side ~propagated_vars:set ~annotmap ~gamma t
let arg_of_type_col ~val_ side set gamma annotmap t =
  internal_tycol_to_opaty ~val_ ~side ~propagated_vars:set ~annotmap ~gamma t

(** Inserting directives for Explicit Instantiation *)
(* Directives just rewrite type*)
type 'a directive_just_type =
    [`ajax_publish of [`sync | `async] | `comet_publish
    | `insert_server_value of QmlAst.ident ]

(* Directives not to traverse *)
type call = [ `ajax_call of [`sync | `async] | `comet_call ]
type directive_no_traverse =
    [`ajax_publish of [`sync | `async] | `comet_publish | call ]

let console_debug hdr fmt =
  OManager.printf (
    "@{<bright>%s@}: "^^fmt^^"@."
  ) hdr

(*
  The printer used for pprinting debug logs.
  For changing the printer, choose a different instance of the same class
*)
let pp = QmlPrint.pp_base
let filter_left f l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | h1 :: t1, h2 :: t2 ->
        if f h2 then aux (h1 :: acc) t1 t2 else aux acc t1 t2
    | _ -> invalid_arg "filter_left" in
  aux [] l1 l2

let type_of_args_from_quant gamma lt lrow lcol =
  let opaty = opaty gamma in
  let oparow = oparow gamma in
  let opacol = opacol gamma in
  List.map (fun _ -> opaty) lt @ List.map (fun _ -> oparow) lrow @ List.map (fun _ -> opacol) lcol

let debug fmt = console_debug "walk_instantiation" fmt
let walk_instantiation (have_typeof:QmlTypeVars.FreeVars.t) gamma annotmap e =
  match QmlAnnotMap.find_tsc_inst_opt (Q.QAnnot.expr e) annotmap with
  | None -> (annotmap, e)
  | Some tsc ->
      let annotmap = QmlAnnotMap.remove_tsc_inst (Q.QAnnot.expr e) annotmap in
      let t = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
      let (quantif, _body, ()) = QmlGenericScheme.export_unsafe tsc in
      let (quant_var, quant_row, quant_col) = QmlTypeVars.FreeVars.export_as_lists quantif in
      #<If:EXPL_INST_DEBUG>
        if not (Inspect.is_type_arrow gamma t) then
          debug "Warning: instantiating nonfunctional value:"
        else ();
        debug "Instantiated value %a" pp#expr e ;
        debug "from %a" pp#ty _body ;
        debug "to %a" pp#ty t ;
        ()
      #<End>;
      (* TODO: optimize: use have_typeof already here *)
      let lt, lrow, lcol =
        QmlMoreTypes.unify_and_show_instantiation ~gamma ~allow_partial_application:true t tsc
      in
      #<If:EXPL_INST_DEBUG>
        let lvt = List.combine quant_var lt in
        let pp_lvt fmt (v, t) = Format.fprintf fmt "%a/%s" pp#ty t (QmlTypeVars.TypeVar.to_string v) in
        debug (
          "the instantiation is [ %a ]; the instantiation used for @@typeof is "
        )
          (Format.pp_list " ; " pp_lvt) lvt
      #<End>;
      let choose_vars v = QmlTypeVars.FreeVars.mem_typevar v have_typeof in
      let choose_rowvars v = QmlTypeVars.FreeVars.mem_rowvar v have_typeof in
      let choose_colvars v = QmlTypeVars.FreeVars.mem_colvar v have_typeof in
      let lt = filter_left choose_vars lt quant_var in
      let lrow = filter_left choose_rowvars lrow quant_row in
      let lcol = filter_left choose_colvars lcol quant_col in

      if lt = [] && lrow = [] && lcol = [] then
        (annotmap, e)
      else (
        #<If:EXPL_INST_DEBUG>
          let lvt = (*List.combine quant*) lt in
          let pp_lvt fmt ((*v, *)t) = Format.fprintf fmt "%a/%s" pp#ty t "x" (* (QmlTypeVars.TypeVar.to_string v) *) in
          debug "[ %a ]" (Format.pp_list " ; " pp_lvt) lvt
        #<End>;
        let args = type_of_args_from_quant gamma lt lrow lcol in
        let arrow = QmlAst.TypeArrow (args, t) in
        let pos = Q.Pos.expr e in
        let annotmap = QmlAnnotMap.add_ty (Q.QAnnot.expr e) arrow annotmap in
        QmlAstCons.TypedExpr.directive_ty ~pos annotmap (`apply_ty_arg (lt,lrow,lcol)) [e] [] t
      )


let debug fmt = console_debug "walk_generalization" fmt
let walk_generalization (have_typeof:QmlTypeVars.FreeVars.t) id gamma annotmap e =
  match QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap with
  | None -> (annotmap, e)
  | Some tsc ->
      let annotmap = QmlAnnotMap.remove_tsc (Q.QAnnot.expr e) annotmap in
      let (quantif, body, ()) = QmlGenericScheme.export_unsafe tsc in
      if not (Inspect.is_type_arrow gamma body) then (
        QmlError.warning
          ~wclass:WarningClass.ei_generalize
          (QmlError.Context.merge2 (QmlError.Context.annotmap annotmap) (QmlError.Context.expr e))
          "Generalizing non-functional value : %a"
          QmlPrint.pp#ident id;
        ()
      );
      let (quant_var, quant_row, quant_col) = QmlTypeVars.FreeVars.export_as_lists quantif in
      #<If:EXPL_INST_DEBUG>
        if not (Inspect.is_type_arrow gamma body) then
          debug "Warning: generalizing nonfunctional value:"
        else ();
        debug "Generalized value %a:    %a" pp#ident id pp#expr e;
        debug "with type %a" pp#ty body;
        let lvt = List.map QmlTypeVars.TypeVar.to_string quant_var in
        let s_lt = String.concat " ; " lvt in
        debug (
          "the variables are [%s]; the variables used in @@typeof are "
        )
          s_lt
      #<End>;
      let choose_var v = QmlTypeVars.FreeVars.mem_typevar v have_typeof in
      let choose_row v = QmlTypeVars.FreeVars.mem_rowvar v have_typeof in
      let choose_col v = QmlTypeVars.FreeVars.mem_colvar v have_typeof in
      let quant_var = List.filter choose_var quant_var in
      let quant_row = List.filter choose_row quant_row in
      let quant_col = List.filter choose_col quant_col in
      if quant_var = [] && quant_row = [] && quant_col = [] then (annotmap, e) else
        begin
          #<If:EXPL_INST_DEBUG>
            let lvt = List.map QmlTypeVars.TypeVar.to_string quant_var in
            let s_lt = String.concat " ; " lvt in
            debug "[%s]" s_lt
          #<End>;
          let args = type_of_args_from_quant gamma quant_var quant_row quant_col in
          let arrow = Q.TypeArrow (args, body) in
          match e with
          | Q.Directive (label, (#directive_just_type as dir), es, tys) -> (
              (* Doesn't generalize insert_server_value. Because its already
                 generalized on server side. But still update type. *)
              match es with
              | [] ->
                  QmlAstCons.TypedExpr.make annotmap e arrow
              | [e] ->
                  let annotmap, e = QmlAstCons.TypedExpr.make annotmap e arrow in
                  let pos = Annot.pos label in
                  QmlAstCons.TypedExpr.directive_ty ~pos annotmap dir [e] tys arrow
              | _ :: _ :: _ -> assert false
            )
          | _ ->
              let pos = QmlAst.Pos.expr e in
              QmlAstCons.TypedExpr.directive_ty ~pos annotmap (`abstract_ty_arg (quant_var,quant_row,quant_col)) [e] [] arrow
        end

(* Calculates the set of type variables that are used in any @typeof;
   can be computed incrementally, but according to the order
   of dependecies between top-level values; until we implement polymorphic
   recursion, we don't need a fixpoint computation for mutual recursion,
   even a local fixpoint (for a single toplevel ValRec set) is not needed *)
let debug fmt = console_debug "have_typeof" fmt
let have_typeof ~set gamma annotmap qmlAst =
  let walk (tainted:QmlTypeVars.FreeVars.t) e =
    let tainted =
      match e with
      | Q.Directive (_, #call, [_], _) -> (
          match QmlAnnotMap.find_tsc_inst_opt (Q.QAnnot.expr e) annotmap with
          | None -> tainted
          | Some tsc ->
              let quantif, _body, () = QmlGenericScheme.export_unsafe tsc in
              QmlTypeVars.FreeVars.union tainted quantif
        )
      | Q.Directive (_, `typeof, [de], _) ->
          let t = QmlAnnotMap.find_ty (Q.QAnnot.expr de) annotmap in
          let vars = QmlTypes.freevars_of_ty t in
          #<If:EXPL_INST_DEBUG>
            debug "Have typeof for typeof:  %a" pp#expr de ;
            debug "the instantiation used for @@typeof is %s" (QmlTypeVars.FreeVars.to_string vars)
          #<End>;
          QmlTypeVars.FreeVars.union tainted vars
      | _ -> tainted in
    match QmlAnnotMap.find_tsc_inst_opt (Q.QAnnot.expr e) annotmap with
    | None -> tainted
    | Some tsc ->
        let t = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
        let (quantif, _body, ()) = QmlGenericScheme.export_unsafe tsc in
        #<If:EXPL_INST_DEBUG>
          debug "Have typeof for value  %a" pp#expr e ;
          debug "from %a" pp#ty _body ;
          debug "  to %a" pp#ty t ;
          ()
        #<End>;
        let lt,lrow,lcol =
          try QmlMoreTypes.unify_and_show_instantiation ~allow_partial_application:true ~gamma t tsc
          with QmlTyperException.Exception _ as exn ->
            OManager.i_error "%a@\non %a@."
              (QmlTyperErrHandling.pp_report_from_typer_exception annotmap)
              exn QmlPrint.pp#expr e
          | exn ->
              let context = QmlError.Context.expr e in
              QmlError.i_error None context "@[<2>Typing error: %s@\n(tsc:%a vs@ ty:%a)]@." (Printexc.to_string exn) QmlPrint.pp#tsc tsc QmlPrint.pp#ty t
        in
        let (quant_var, quant_row, quant_col) = QmlTypeVars.FreeVars.export_as_lists quantif in
        #<If:EXPL_INST_DEBUG>
          let lvt = List.combine quant_var lt in
          let lvrow = List.combine quant_row lrow in
          let lvcol = List.combine quant_col lcol in
          let pp_lvt fmt (v, t) = Format.fprintf fmt "%a/%s" pp#ty t (QmlTypeVars.TypeVar.to_string v) in
          let pp_lvrow fmt (v, t) = Format.fprintf fmt "%a/%s" pp#tyrow t (QmlTypeVars.RowVar.to_string v) in
          let pp_lvcol fmt (v, t) = Format.fprintf fmt "%a/%s" pp#tycol t (QmlTypeVars.ColVar.to_string v) in
          debug (
            "the have typeof instantiation is [%a|%a|%a]; the instantiation used for @@typeof is "
          )
            (Format.pp_list " ; " pp_lvt) lvt
            (Format.pp_list " ; " pp_lvrow) lvrow
            (Format.pp_list " ; " pp_lvcol) lvcol
        #<End>;
        let check mem freevars =
          fun acc v t ->
            if mem v tainted then
              let vars = freevars t in
              QmlTypeVars.FreeVars.union acc vars
            else
              acc
        in
        let _original_tainted = tainted in
        let tainted = List.fold_left2 (check QmlTypeVars.FreeVars.mem_typevar QmlTypes.freevars_of_ty) tainted quant_var lt in
        let tainted = List.fold_left2 (check QmlTypeVars.FreeVars.mem_rowvar QmlTypes.freevars_of_row) tainted quant_row lrow in
        let tainted = List.fold_left2 (check QmlTypeVars.FreeVars.mem_colvar QmlTypes.freevars_of_col) tainted quant_col lcol in
        #<If:EXPL_INST_DEBUG>
          let new_tainted = QmlTypeVars.FreeVars.diff tainted _original_tainted in
          debug "%s" (QmlTypeVars.FreeVars.to_string new_tainted)
        #<End>;
        tainted
  in
  let walk_top tainted top = QmlAstWalk.Expr.fold_up walk tainted top in
  let lt =
    QmlAstWalk.CodeExpr.fold walk_top set qmlAst
  in
  #<If:EXPL_INST_DEBUG>
    let s_lt = QmlTypeVars.FreeVars.to_string lt in
    debug "Total have typeof list is %s" s_lt
  #<End>;
  lt

(*------------------------------*)
(*--- wrapper for separation ---*)
(*------------------------------*)
module S =
struct
  type t = QmlTypeVars.FreeVars.t
  let pass = "pass_ExplicitInstantiation"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R =
struct
  include ObjectFiles.Make(S)
  let load () =
    fold_with_name
      (fun package acc vars ->
         let refreshed_vars =
           QmlTypeVars.FreeVars.map
             (QmlRefresh.find_t package)
             (QmlRefresh.find_r package)
             (QmlRefresh.find_c package)
             vars in
         QmlTypeVars.FreeVars.union acc refreshed_vars
      ) QmlTypeVars.FreeVars.empty
  let save loaded_set full_set =
    let new_set = QmlTypeVars.FreeVars.diff full_set loaded_set in
    save new_set;
end

module S_gamma =
struct
  type t = (QmlAst.ty, unit) QmlGenericScheme.tsc IdentMap.t
  let pass = "pass_ExplicitInstantiation_gamma"
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module R_gamma =
struct
  include ObjectFiles.Make(S_gamma)
  let save_and_load gamma =
    let map =
      if ObjectFiles.stdlib_packages (ObjectFiles.get_current_package ()) then
        QmlTypes.Env.Ident.to_map gamma
      else
        IdentMap.empty in
    save map;
    let map =
      fold_with_name
        (fun package acc map ->
           if ObjectFiles.stdlib_packages package then (
             let map = IdentMap.map (QmlRefresh.refresh_typevars_from_tsc package) map in
             IdentMap.safe_merge acc map
           ) else
             acc) map in
    QmlTypes.Env.Ident.from_map map QmlTypes.Env.empty
end

let have_typeof gamma annotmap qmlAst =
  let loaded_set = R.load () in
  let full_set = have_typeof ~set:loaded_set gamma annotmap qmlAst in
  R.save loaded_set full_set;
  full_set

(* adding directives for implementing @typeof *)
(* TODO: take care of annot_tsc_inst_deep by gathering tsc from submodules,
   removing them? and creating a big tsc from them, etc. *)
let debug fmt = console_debug "process_code" fmt
let process_code (have_typeof:QmlTypeVars.FreeVars.t) gamma annotmap _published qmlAst =
  let walk_top  (annotmap, ajax_ast) (id, e) =
    (* generalize top level *)
    let ((annotmap, e), ajax_ast) =
      if IdentMap.mem id !published_ref then
        let tsc =
          QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap in
        match tsc with
        | None ->
            (* no type variables in the schema, no type arguments needed *)
            ((annotmap, e), ajax_ast)
        | Some tsc ->
            #<If:EXPL_INST_DEBUG>
              debug "Ajax generalization of %a" pp#ident id
            #<End>;
            let ty_orig = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
            let (annotmap, e) =
              walk_generalization have_typeof id gamma annotmap e
            in
            let (quantif, _, ()) = QmlGenericScheme.export_unsafe tsc in
            let used_vars = QmlTypeVars.FreeVars.inter quantif have_typeof in
            let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
            if QmlTypeVars.FreeVars.equal used_vars quantif then (
              (* all variables used *)
              let annotmap, map_e = TypedExpr.ident annotmap id ty in
              (* put the ident in the map in any case
               * so that InsertRemote knows that it was rewritten *)
              published_ref := IdentMap.add id (Some (Q.Label.expr map_e, id, `one_lambda)) !published_ref;
              ((annotmap, e), ajax_ast)
            ) else
              let ajax_id = Ident.refresh id in
              let (annotmap, id_e) =
                QmlAstCons.TypedExpr.ident annotmap id ty
              in
              let (annotmap, appl) =
                if QmlTypeVars.FreeVars.is_empty used_vars then
                  (* no variables used *)
                  (annotmap, id_e)
                else
                  let lt,lrow,lcol = QmlTypeVars.FreeVars.export_as_lists used_vars in
                  let lt = List.map (fun v -> QmlAst.TypeVar v) lt in
                  let lrow = List.map (fun v -> QmlAst.TyRow ([], Some v)) lrow in
                  let lcol = List.map (fun v -> QmlAst.TyCol ([], Some v)) lcol in
                  let pos = QmlAst.Pos.expr id_e in
                  QmlAstCons.TypedExpr.directive_ty ~pos annotmap (`apply_ty_arg (lt,lrow,lcol)) [id_e] [] ty_orig
              in
              let annotmap =
                QmlAnnotMap.add_tsc (Q.QAnnot.expr appl) tsc annotmap
              in
              let (annotmap, ajax_e) =
                walk_generalization quantif ajax_id gamma annotmap appl
              in
              let ajax_ast = (ajax_id, ajax_e) :: ajax_ast in
              let annotmap =
                let arrow =
                  QmlAnnotMap.find_ty (Q.QAnnot.expr ajax_e) annotmap
                in
                let (annotmap, map_e) =
                  QmlAstCons.TypedExpr.ident annotmap ajax_id arrow
                in
                #<If:EXPL_INST_DEBUG>
                  debug "Ajax value %a set to type %a" pp#ident ajax_id pp#ty arrow ;
                  debug "Published map updated: %a maps to %a" pp#ident id pp#expr map_e ;
                  ()
                #<End>;
                published_ref := IdentMap.add id (Some (Q.Label.expr map_e, ajax_id, `two_lambdas)) !published_ref;
                renaming_map := QmlRenamingMap.add !renaming_map (QmlRenamingMap.original_from_new !renaming_map id) ajax_id;
                annotmap
              in
              ((annotmap, e), ajax_ast)
      else
        (walk_generalization have_typeof id gamma annotmap e, ajax_ast)
    in
    (* generalize and instantiate inside the expression *)
    let (annotmap, e) =
      let trav_expr trav annotmap e =
        let (annotmap, e) =
          let id = Ident.next "ExprInst_local" in
          walk_generalization have_typeof id gamma annotmap e
        in
        let (annotmap, e) =
          walk_instantiation have_typeof gamma annotmap e
        in
        match e with
        | Q.Directive (_, #directive_no_traverse, _, _) ->
            annotmap, e
        | _ ->
            trav annotmap e
      in
      QmlAstWalk.Expr.traverse_foldmap trav_expr annotmap e
    in
    ((annotmap, ajax_ast), (id, e))
  in
  let ((annotmap, ajax_ast), qmlAst) =
    List.fold_left_map
      (QmlAstWalk.Top.fold_map_name_expr walk_top) (annotmap, []) qmlAst
  in
  let label = Annot.nolabel "Pass_ExplicitInstantiation.process_code" in
  let qmlAst =
    if ajax_ast = [] then
      qmlAst
    else
      qmlAst @ [QmlAst.NewVal (label, ajax_ast)] in
  let gamma_updt gamma (id, e) =
    let tsc = QmlTypes.Scheme.quantify (QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap) in
    QmlTypes.Env.Ident.add id tsc gamma
  in
  (* Update gamma for ALL the expressions *)
  let gamma = List.fold_left
      (QmlAstWalk.Top.fold_name_expr gamma_updt) gamma qmlAst
  in
  (annotmap, gamma, qmlAst)

let get_stdlib_gamma gamma =
  if ObjectFiles.Arg.is_fully_separated () then
    R_gamma.save_and_load gamma
  else
    gamma

(** Eliminating directives for Explicit Instantiation *)

let eta_expansion lt lrow lcol lt' lrow' lcol' =
  try
    List.iter2 (fun v ty ->
                  match ty with
                  | Q.TypeVar v' when QmlTypeVars.TypeVar.equal v v' -> ()
                  | _ -> raise Exit) lt lt';
    List.iter2 (fun v ty_row ->
                  match ty_row with
                  | Q.TyRow ([], Some v') when QmlTypeVars.RowVar.equal v v' -> ()
                  | _ -> raise Exit) lrow lrow';
    List.iter2 (fun v ty_col ->
                  match ty_col with
                  | Q.TyCol ([], Some v') when QmlTypeVars.ColVar.equal v v' -> ()
                  | _ -> raise Exit) lcol lcol';
    true
  with Exit | Invalid_argument _ ->
    false

(* TODO: clarify what directives we go through exactly
 * i think it should be the same as the one ignored by lambda lifting
 * plus @abstract_ty and @apply_ty, but i am not sure
 * (@server, @client, etc. can be ignored since they have been removed by the slicer) *)
let rec get_lambda = function
  | Q.Coerce (_,e,_)
  | Q.Directive (_, (#Q.type_directive | `abstract_ty_arg _ | `apply_ty_arg _ | `async), [e], _) -> get_lambda e
  | Q.Directive (_, `lifted_lambda env, [e], _) -> (
      match e with
      | Q.Lambda (_,params,e) -> `lambda (env,params,e)
      | _ -> assert false
    )
  | Q.Ident (_,x) -> `ident x
  | Q.Lambda (_,params,e) -> `lambda ((0,[]),params,e)
  | _ -> `none

(* not sure if this test should be in sync with something else *)
let rec try_get_ident = function
  | Q.Coerce (_,e,_) (* keep in sync with the same pattern below *)
  | Q.Directive (_, (#Q.type_directive), [e], _) -> try_get_ident e
  | Q.Directive (_, (`comet_call | `ajax_call _), _, _) -> `call
  | Q.Ident (_,x) -> `ident x
  | _ -> `none

let debug fmt = console_debug "walk_undirective" fmt
let walk_undirective ~val_ side gamma toplevel_lambdas annotmap e =
  let opaty = lazy (opaty gamma) in
  let oparow = lazy (oparow gamma) in
  let opacol = lazy (opacol gamma) in
  let aux_lambda abstracted_set annotmap __de lt lrow lcol =
    #<If:EXPL_INST_OPT_DEBUG>
      debug "Applying function  %a" pp#expr __de ;
    let t = QmlAnnotMap.find_ty (Q.QAnnot.expr __de) annotmap in
    debug "with type %a" pp#ty t
      #<End>;
    let (annotmap, args1) = List.fold_left_map (arg_of_type ~val_ side abstracted_set gamma) annotmap lt in
    let (annotmap, args2) = List.fold_left_map (arg_of_type_row ~val_ side abstracted_set gamma) annotmap lrow in
    let (annotmap, args3) = List.fold_left_map (arg_of_type_col ~val_ side abstracted_set gamma) annotmap lcol in
    annotmap, args1 @ args2 @ args3 in
  QmlAstWalk.Expr.traverse_foldmap_context_down
    (fun tra ((abstracted_set,at_toplevel) as context) annotmap e ->
       match e with
       (* keep in sync with try_get_ident *)
       | Q.Coerce (_,_,_)
       | Q.Directive (_,#Q.type_directive,_,_) -> tra context annotmap e

       | Q.Directive (label, (`partial_apply _ | `full_apply _ as v), [Q.Apply (_, Q.Directive (_, `apply_ty_arg (lt,lrow,lcol), [de], _),args)], _) -> (
           assert (match try_get_ident de with `ident x -> IdentSet.mem x toplevel_lambdas | `call -> true | `none -> false);
           let annotmap, args' = aux_lambda abstracted_set annotmap de lt lrow lcol in
           let annotmap, e = QmlAstCons.TypedExpr.apply_partial gamma annotmap de (args' @ args) in
           let v =
             match v with
             | `partial_apply missing -> `partial_apply missing
             | `full_apply env -> `full_apply (env + List.length lt + List.length lrow + List.length lcol) in
           let e = Q.Directive (label, v, [e], []) in
           tra (abstracted_set,false) annotmap e
         )

       | Q.Apply (_, Q.Directive (_, `apply_ty_arg (lt,lrow,lcol), [de], _),args)
           when (match try_get_ident de with `ident x -> IdentSet.mem x toplevel_lambdas | `call -> true | `none -> false) ->
           let annotmap, args' = aux_lambda abstracted_set annotmap de lt lrow lcol in
           let annotmap, e = QmlAstCons.TypedExpr.apply_partial gamma annotmap de (args' @ args) in
           let annotmap, e = QmlAstCons.TypedExpr.directive annotmap (`full_apply (List.length args')) [e] [] in
           tra (abstracted_set,false) annotmap e

       | Q.Directive (_, `apply_ty_arg (lt,lrow,lcol), [de], _) -> (
           let annotmap, args = aux_lambda abstracted_set annotmap de lt lrow lcol in
           let annotmap, e = QmlAstCons.TypedExpr.apply_partial gamma annotmap de args in
           match try_get_ident de with
           | `ident x when IdentSet.mem x toplevel_lambdas ->
               let annotmap, e = QmlAstCons.TypedExpr.directive annotmap (`partial_apply None) [e] [] in
               tra (abstracted_set,false) annotmap e
           | `call ->
               let annotmap, e = QmlAstCons.TypedExpr.directive annotmap (`partial_apply None) [e] [] in
               tra (abstracted_set,false) annotmap e
           | _ ->
               tra (abstracted_set,false) annotmap e
         )

       | Q.Directive (_, `apply_ty_arg _, _, _) ->
           assert false

       | Q.Directive (_, `abstract_ty_arg ((lv,lrow,lcol) as lt), [de], _) ->
           let abstracted_set = QmlTypeVars.FreeVars.add_list lt abstracted_set in
           #<If:EXPL_INST_OPT_DEBUG>
             debug "Abstracting expression  %a" pp#expr de ;
             let t = QmlAnnotMap.find_ty (Q.QAnnot.expr de) annotmap in
             debug "with type %a" pp#ty t
           #<End>;
           let (annotmap, e) =
             (* optimization: eta-reduction; TODO: do some of it after have_typeof *)
             match de with
             | Q.Directive (_, `apply_ty_arg (lt',lrow',lcol'), [de'], _)
                 when at_toplevel
                   && eta_expansion lv lrow lcol lt' lrow' lcol' ->
                 assert (match try_get_ident de' with `ident _ -> true | _ -> false);
                 (* TODO: check if changing it to [lt' = List.rev lt]
                    boosts up the speedup from this optimization *)
                 (annotmap, de')
             | _ ->
                 let params, de, lifted_lambda =
                   match get_lambda de with
                   | `lambda (env,params,de) -> params, de, Some env
                   | `ident _ | `none -> [], de, None in
                 let lit = List.map (fun v -> (id_of_var v, Lazy.force opaty)) lv in
                 let rows = List.map (fun v -> (id_of_rowvar v, Lazy.force oparow)) lrow in
                 let cols = List.map (fun v -> (id_of_colvar v, Lazy.force opacol)) lcol in
                 let params = List.map (fun p -> (p,QmlAstCons.Type.next_var ())) params in (* FIXME *)
                 let annotmap, e = QmlAstCons.TypedExpr.lambda annotmap (lit @ rows @ cols @ params) de in
                 match lifted_lambda with
                 | Some ((env_size,idento)) ->
                   let env_size = env_size + List.length lit + List.length rows + List.length cols in
                   QmlAstCons.TypedExpr.directive annotmap (`lifted_lambda (env_size,idento)) [e] []
                 | None ->
                   (annotmap, e) in
           tra (abstracted_set,false) annotmap e

       | Q.Directive (_, `abstract_ty_arg _, _, _) ->
           assert false

       | Q.Directive (_, `typeof, [de], _) ->
           let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr de) annotmap in
           let annotmap, e = arg_of_type ~val_ side abstracted_set gamma annotmap ty in
           tra (abstracted_set,false) annotmap e

       | _ ->
           tra (abstracted_set,false) annotmap e

    ) (QmlTypeVars.FreeVars.empty,true) annotmap e

(* since ei needs to keep the code 'roughly' lambda lifted
 * it needs to keep some information between packages
 * 'roughly' lambda lifted means that that whenever ei is adding a lambda
 * on top of an other lambda, then there are merged (and so are the corresponding applys)
 * BUT
 * when it adds a lambda on top of something that is not a lambda, then the code is not lambda lifted anymore
 * (but these lambdas can be lifted again later, and the code will have the same properties
 *  as we had lambda lifted just once:
 *  - partial application only on toplevel lambdas (modulo aliases)
 *    in which case you have a @partial_apply
 *    and that happends only when you are applygin the environment of a local lambda
 *  - all other applications are full applications
 * )
 *)
module S_ei_lifting =
struct
  type t = IdentSet.t
  let pass = "pass_ExplicitInstantiation_lifting"
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module R_ei_lifting = ObjectFiles.MakeClientServer(S_ei_lifting)

(* eliminating directives and replacing them with real code *)
let unprocess_code ~val_ ~side gamma annotmap qmlAst =
  let initial_toplevel_lambdas = R_ei_lifting.fold ~side IdentSet.union IdentSet.empty in
  let toplevel_lambdas =
    QmlAstWalk.CodeExpr.fold_name_expr
      (fun acc (i,e) ->
         match get_lambda e with
         | `lambda _ -> IdentSet.add i acc
         | _ -> acc
      ) initial_toplevel_lambdas qmlAst in
  let (annotmap, final_toplevel_lambdas), code =
    QmlAstWalk.CodeExpr.fold_map_name_expr
      (fun (annotmap,toplevel_lambdas) (i,e) ->
         let annotmap, e = walk_undirective ~val_ side gamma toplevel_lambdas annotmap e in
         let toplevel_lambdas =
           match try_get_ident e with (* can't say get_lambda after rewriting to
                                       * detect lambdas because we want to know
                                       * if there were lambdas originally, not
                                       * because ei added them. On the other hand
                                       * it must be done this way for aliases *)
           | `ident x when IdentSet.mem x toplevel_lambdas ->
               IdentSet.add i toplevel_lambdas
           | _ -> toplevel_lambdas in
         (annotmap, toplevel_lambdas), (i,e)
      ) (annotmap, toplevel_lambdas) qmlAst in
  R_ei_lifting.save ~side (IdentSet.diff final_toplevel_lambdas initial_toplevel_lambdas);
  annotmap, code

let tsc_add_op gamma annotmap =
  let ty_void = Q.TypeRecord (Q.TyRow ([], None)) in
  let (opa_tsc, _) = QmlTypes.type_of_type gamma opatsc_type in
  let ty_add = QmlAst.TypeArrow ([QmlAst.TypeConst (Q.TyString); opa_tsc], ty_void) in
  let pos = FilePos.nopos "tsc_add_op" in
  QmlAstCons.TypedExpr.bypass ~pos annotmap Opacapi.Opabsl.BslValue.Tsc.add ty_add

let generate_tsc_map_updates ~val_ ~side ?(memoize=true) ~local_typedefs gamma annotmap =
  let label = Annot.nolabel "Pass_ExplicitInstantiation.generate_tsc_map_updates" in
  match ObjectFiles.compilation_mode () with
  | `init -> annotmap, QmlAst.NewVal (label, [])
  | `compilation | `linking | `prelude ->
  let update annotmap (id, (tsc, _, _)) =
    (* [fun_add] has to be inside [update] to generate fresh annots *)
    let (annotmap, fun_add) = tsc_add_op gamma annotmap in
    let (annotmap, tsc) =
      tsc_to_opatsc ~val_ ~side ~memoize (annotmap, gamma) tsc in
    let id = QmlAst.TypeIdent.to_string id in
    (* fakesource allows to squash tuple types *)
    let dummy_id = Ident.fake_source ("typedef_" ^ id) in
    let (annotmap, id) = QmlAstCons.TypedExpr.tagged_string annotmap id Q.Type_def in
    let (annotmap, appl) =
      QmlAstCons.TypedExpr.apply gamma annotmap fun_add [id; tsc]
    in
    (annotmap, (dummy_id, appl))
  in
  let gamma_list = QmlTypes.Env.TypeIdent.to_list gamma in
  let gamma_list =
    if ObjectFiles.Arg.is_fully_separated () then
      List.filter (fun (i,_tsc) -> QmlAst.TypeIdentSet.mem i local_typedefs) gamma_list
    else
      gamma_list in
  try
    let (annotmap, l) = List.fold_left_map update annotmap gamma_list in
    (annotmap, QmlAst.NewVal (label, l))
  with
    QmlTyperException.Exception _ (* happens in --no-stdlib when not compiling stdlib.core
                                   * because OpaTsc.t is not defined for instance *) ->
      #<If:TESTING> () #<Else> OManager.printf "Beware: not generating the runtime representation of the gamma.@." #<End>;
      annotmap, QmlAst.NewVal (label, [])

(* Code that generated the newvals that all the identifiers generated by opaty_to_ty
 * and tsc_to_opatsc point to *)
let get_memoized_definitions gamma side =
  let add_ty i ty gamma = QmlTypes.Env.Ident.add i (QmlTypes.Scheme.quantify ((*QmlTypes.type_of_type gamma*) ty)) gamma in
  match side with
  | `server ->
      let l = server_memo.definitions in
      reset_memo server_memo;
      List.fold_left (* the code is reversed on purpose *)
        (fun (gamma,code) (i,e,ty) ->
           let label = Annot.nolabel "Pass_ExplicitInstantiation.get_memoized_definitions" in
           add_ty i ty gamma, QmlAst.NewVal (label, [(i,e)]) :: code) (gamma, []) l
  | `client ->
      let l = client_memo.definitions in
      reset_memo client_memo;
      List.fold_left (* the code is reversed on purpose *)
        (fun (gamma,code) (i,e,ty) ->
           let label = Annot.nolabel "Pass_ExplicitInstantiation.get_memoized_definitions" in
           add_ty i ty gamma, QmlAst.NewVal (label, [(i,e)]) :: code) (gamma, []) l
