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
(* cf mli *)

(* refactoring in progress *)

(* depends *)
module Format = BaseFormat
module List = BaseList

(* aliases *)
module TypeIdent = QmlAst.TypeIdent
module TypeVar = QmlAst.TypeVar
module RowVar = QmlAst.RowVar
module ColVar = QmlAst.ColVar

(* shorthands *)
module Q = QmlAst

type annot = int
type annotmap = Q.annotmap

(*
  To evoid to overwrite Ident module
*)
module ExprIdent =
struct
  let source = Ident.source
  let next s = Ident.next s
  let _type = TypeIdent.of_string
end

module Tuple =
struct
  let string = Printf.sprintf "tuple_%d"
  let typeident n = TypeIdent.of_string (string n)
  let field = Printf.sprintf "f%d"
  let first_field = 1

  let qml_fst = "fst"
  let qml_snd = "snd"
end

(** TYPES *)
module Type =
struct
  let name ?(p=[]) t = Q.TypeName (p, t)
  let typevar v = Q.TypeVar v
  let next_var () = typevar (TypeVar.next ())

  module Var = struct
    let make _ = typevar (TypeVar.next())
  end

  module Row =
  struct
    exception Missing_field of string * string list

    let make ?(extend=false) l =
      let rv = if extend then Some (RowVar.next ()) else None in
      Q.TyRow (l, rv)

    let make_qml_tuple l =
      List.foldr1 (fun fst snd -> Q.TypeRecord (make [("fst",fst);("snd",snd)])) l

    let make_opa_tuple l =
      Q.TypeRecord (make (List.mapi (fun i t -> (Printf.sprintf "f%d" (i+1),t)) l))

    let to_list (Q.TyRow(l,_)) = l
    let to_list_and_extend  (Q.TyRow(l,tvar)) = l,tvar

    let sort_fields =
      let compare (f, _) (f', _) = String.compare f f' in
      List.sort compare

    let ordered_make ?(extend=false) l =
      make ~extend (sort_fields l)

    let sort (Q.TyRow (l, rv)) =
      Q.TyRow (sort_fields l, rv)

    let is_empty (Q.TyRow (l, _)) = l = []

    let cons (Q.TyRow (l, rv)) field value =
      Q.TyRow ((field,value)::l, rv)

    let dot (Q.TyRow (l, _)) field =
      try
        List.assoc field l
      with Not_found ->
        raise (Missing_field(field, List.map fst l))

    let has_field (Q.TyRow (l, _)) s = List.mem_assoc s l

    let map f (Q.TyRow (l, rv)) =
      Q.TyRow (List.map (fun (s, x) -> (s, f x)) l, rv)

    let fold f (Q.TyRow (l, _)) acc =
      List.fold_left (fun acc (s, x) -> f s x acc) acc l

    let compare (Q.TyRow (r1, _)) (Q.TyRow (r2, _)) =
      List.make_compare Pervasives.compare r1 r2

  end

  module Col =
  struct
    let make ?(extend=false) l =
      let cv = if extend then Some (ColVar.next ()) else None in
      Q.TyCol (l, cv)

    let sort (Q.TyCol (l, cv)) =
      let compare_field (f, _) (f', _) = String.compare f f' in
      let sort_fields = List.sort compare_field in
      let compare_ordered_col col col' =
        Pervasives.compare (List.map fst col) (List.map fst col') in
      let sort_ordered_cols = List.sort compare_ordered_col in
      let l = List.map sort_fields l in
      let l = sort_ordered_cols l in
      Q.TyCol (l, cv)
  end

  module Arrow = struct
    let to_rev_list ?(expandlast=fun _ ty->[ty]) ty=
      let rec aux ty size l =
        match ty with
          | Q.TypeArrow (lt0, t1) ->
              aux t1 (size + List.length lt0) (List.rev lt0 @ l)
          | ty -> (size+1,ty::l)
      in
      let size,revtys = aux ty 0 [] in
        match revtys with
          | tylast::tyargs -> (expandlast size tylast)@tyargs
          | _ -> assert false

    let to_list ?expandlast ty = List.rev (to_rev_list ?expandlast ty)

    let to_args_res ?expandlast ty =
      match (to_rev_list ?expandlast ty) with
        | tylast::tyargs -> (List.rev tyargs),tylast
        | _ -> assert false

    let rec drop nb ty =
      assert (nb >= 0);
      if nb = 0 then ty
      else
        match ty with
        | Q.TypeArrow (l,ty) ->
            let len = List.length l in
            if nb >= len then
              drop (nb - len) ty
            else
              Q.TypeArrow (List.drop nb l,ty)
        | _ ->
            invalid_arg
              ("QmlAstCons.Type.drop: You try to drop too much \
                arguments on an arrow, or you try drop a non-arrow value")


  end

  let sort =
    QmlAstWalk.Type.map_down (function
      | Q.TypeRecord row -> Q.TypeRecord (Row.sort row)
      | Q.TypeSum col -> Q.TypeSum (Col.sort col)
      (* | TypeSumSugar l -> assert false *) (* not really clear what to do here *)
      | ty -> ty)
end


(** UNTYPED EXPR *)
module UntypedExpr =
struct

  let nopos = FilePos.nopos "cons.untyped"
  let a () = Annot.next_label nopos

  let copy_1_expr e = Q.QAnnot.New.expr e (Annot.next ())
  let copy_1_pat e = Q.QAnnot.New.pat e (Annot.next ())

  (* copy an expression, i.e. refresh all annots *)
  let copy = QmlAstWalk.ExprPatt.map_down copy_1_expr copy_1_pat

  let ident i = Q.Ident (a(), i)
  let source s = ident (Ident.source s)
  let fresh_internal n = ident (Ident.next n)

  let const e = Q.Const (a(), e)
  let int i = const (Q.Int i)
  let float f = const (Q.Float f)
  let string s = const (Q.String s)

  let coerce e t = Q.Coerce (Annot.refresh (Q.Label.expr e), e, t)

  (* for letin and letrecin, maybe assert (l <> []) instead, and have
     a different name for the more clever functions *)
  let stupid_letin l e = Q.LetIn (a(), l, e)
  let letin l e = if l = [] then e else Q.LetIn (a(), l, e)
  let letrecin l e = if l = [] then e else Q.LetRecIn (a(), l, e)
  let lambda il e = Q.Lambda (a(), il, e)
  let may_lambda il e =
    match il with
    | [] -> e
    | _ -> lambda il e
  let directive variant exprs tys = Q.Directive (a(), variant, exprs, tys)
  let apply fct args = Q.Apply (a(), fct, args)
  let may_apply e el =
    match el with
    | [] -> e
    | _ -> apply e el
  let match_ e p = Q.Match (a(), e, p)
  let record r = Q.Record (a(), r)
  let dot e f = Q.Dot (a(), e, f)
  let extendrecord f e n = Q.ExtendRecord (a(), f, e, n)
  let bypass s = Q.Bypass (a(), s)
  let restricted_bypass ~pass s = directive (`restricted_bypass pass) [bypass s] []

  let unit () = coerce (record []) (Q.TypeRecord (Q.TyRow ([], None)))

  let _false () = record ["false", unit ()]
  let _true  () = record ["true", unit ()]
  let bool b = if b then _true () else _false ()

  let patrecord ?(rowvar=`closed) fields = Q.PatRecord (a(), fields, rowvar)
  let patconst k = Q.PatConst (a(), k)
  let patvar i = Q.PatVar (a(), i)
  let patany () = Q.PatAny (a())
  let patemptyrecord () = Q.PatRecord (a(), [], `closed)
  let patcoerce p ty = Q.PatCoerce (a(), p, ty)

  let pattrue  () = patrecord ["true", patemptyrecord()]
  let patfalse () = patrecord ["false", patemptyrecord()]

  let patsome  v  = patrecord ["some", patvar v]
  let patnone  () = patrecord ["none", patemptyrecord ()]

  let patextendrecord r = patrecord ~rowvar:`open_ r

  let patlist pats =
    let tail = patrecord ["nil",patemptyrecord()] in
    List.fold_right
      (fun pat acc -> patrecord ["hd",pat ; "tl",acc]) pats tail

  let pat_opa_tuple l =
    let fields = List.mapi (fun i p-> Printf.sprintf "f%d" (i+1),p) l in
      patrecord fields

  let patfreshvar n =
    let fe = Ident.next n in
    fe, patvar fe

  let qml_tuple l =
    let tuple = List.foldr1 (fun fst snd -> record [ "fst", fst ; "snd", snd ]) l in
    tuple
  let opa_tuple l =
    let fields = List.mapi (fun i t-> Printf.sprintf "f%d" (i+1),t) l in
    record fields

  let list =
    let list = Type.name (ExprIdent._type Opacapi.Types.list) in
    let rec aux = function
      | [] -> coerce (record ["nil", unit ()]) list
      | hd :: tl ->  coerce (record [("hd", hd) ; ("tl", aux tl)]) list in
    aux

  let ifthenelse cond e1 e2 =
    let tbool = Q.TypeName ([], Q.TypeIdent.of_string Opacapi.Types.bool) in
    match_ (coerce cond tbool)
      [ pattrue() , e1
      ; patfalse(), e2 ]
end


(** Beware -- see the note in qmlAst.ml about the unvalrec *)
module UnValRec =
struct

  (** can make a letrecin or a letin *)
  let make_let_gen letmaker valrec =
    let old_annot_list = List.map (fun (_, e) -> Q.QAnnot.expr e) valrec in
    let fresh a = Ident.refresh ~map:(Printf.sprintf "unvalrec_fun_%s") a in
    let rev = List.rev_map (fun (i, e) ->
                              let field = fresh i in
                              let field = Ident.stident field in
                              (field, i, e)) valrec in
    let fields = List.rev_map (fun (f, i, _) -> (f, UntypedExpr.ident i)) rev in
    let record = UntypedExpr.record fields in
    let expr = letmaker valrec record in
    let annottrack = [
      old_annot_list, [
        Q.QAnnot.expr record;
        Q.QAnnot.expr expr
      ] @ (List.map (fun (_, i) -> Q.QAnnot.expr i) fields)
    ] in
    rev, expr, annottrack

  let letrec valrec =
    let _, expr, _ = make_let_gen UntypedExpr.letrecin valrec
    in expr

  let make valrec =
    let rev, letrec, _ = make_let_gen UntypedExpr.letrecin valrec in
    let top = Ident.next "unvalrec" in
    let freshval = top, letrec in
    let newval = List.rev_map (fun (f, i, _) -> i, UntypedExpr.dot (UntypedExpr.ident top) f) rev in
    {
      Q.valrec = valrec ;
      Q.letrec = letrec ;
      Q.freshval = freshval ;
      Q.newval = newval
    }

  (** tail optimization : produce everything reverted, and do a rev at end *)
  let unvalrec_code =
    let rec aux acc = function
      | [] -> List.rev acc
      | (Q.NewValRec (label, valrec))::q ->
          let unrec = make valrec in
          aux ((Q.NewVal (label, unrec.Q.newval))::(Q.NewVal (label, [unrec.Q.freshval]))::acc) q
      (* could be done for NewVal as well, maybe with an option *)
      | elt::q -> aux (elt::acc) q in
    aux []

  let make_let = function
    | Q.NewVal (label, vals) ->
        let rev, expr, annottrack = make_let_gen UntypedExpr.letin vals in
        List.rev rev, expr,
        (fun e -> match e with
          | Q.LetIn (_, bndlist, Q.Record _) -> Q.NewVal (label, bndlist)
          | _ -> failwith "QmlAstCons.UnValRec.make_let.rebuilder"),
        annottrack
    | Q.NewValRec (label, vals) ->
        let rev, expr, annottrack = make_let_gen UntypedExpr.letrecin vals in
        List.rev rev, expr,
        (fun e -> match e with
          | Q.LetRecIn (_, bndlist, Q.Record _) -> Q.NewValRec (label, bndlist)
          | _ -> failwith "QmlAstCons.UnValRec.make_let.rebuilder"),
        annottrack
    | _ ->  failwith "QmlAstCons.UnValRec.make_let"

  let make_code_elt expr =
    let label = Annot.nolabel "QmlAstCons.make_code_elt" in
    Q.NewVal (label, [(Ident.next "_", expr)]),
    (function
      | Q.NewVal (_, [(_, e)]) -> e
      | _ -> failwith "QmlAstCons.UnValRec.make_code_elt.rebuilder")

  let make_code_elt_maped expr =
    let label = Annot.nolabel "QmlAstCons.make_code_elt_maped" in
    Q.NewVal (label, [(Ident.next "_", expr)]),
    (function
      | Q.M_NewVal [(_, t)] -> t
      | _ -> failwith "QmlAstCons.UnValRec.make_code_elt_maped.rebuilder")

end

let nopos = FilePos.nopos "cons.typed"
let typed_label annotmap ?(pos=nopos) ty =
  let annot = Annot.next () in
  let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
  let label = Annot.make_label annot pos in
  annotmap, label

module TypedExpr =
struct
  type annotmap = Q.annotmap
  type gamma = QmlTypes.Env.t

  let make ?pos annotmap e t =
    let pos =
      match pos with
      | Some pos -> pos
      | None ->
          Q.Pos.expr e
    in
    let annot = Annot.next () in
    let annotmap = QmlAnnotMap.add_ty annot t annotmap in
    let label = Annot.make_label annot pos in
    let e = Q.Label.New.expr e label in
    annotmap, e

  let shallow_copy_new ~annotmap_old annotmap_new e =
    let annot = QmlAnnotMap.find (Q.QAnnot.expr e) annotmap_old in
    let i = Annot.next () in
    let annotmap = QmlAnnotMap.add i annot annotmap_new in
    annotmap, Q.QAnnot.New.expr e i

  let shallow_copy_new_pat ~annotmap_old annotmap_new e =
    let annot = QmlAnnotMap.find (Q.QAnnot.pat e) annotmap_old in
    let i = Annot.next () in
    let annotmap = QmlAnnotMap.add i annot annotmap_new in
    annotmap, Q.QAnnot.New.pat e i

  let shallow_copy_new_when_possible ~annotmap_old annotmap_new e =
    let orig_i = Q.QAnnot.expr e in
    let i = Annot.next () in
    let e = Q.QAnnot.New.expr e i in
    let annotmap =
      try
        let annot = QmlAnnotMap.find orig_i annotmap_old in
        let annotmap = QmlAnnotMap.add i annot annotmap_new in
        annotmap
      with
        QmlAnnotMap.AnnotNotFound _ ->
          annotmap_new in
    annotmap, e

  let shallow_copy_new_when_possible_pat ~annotmap_old annotmap_new e =
    let orig_i = Q.QAnnot.pat e in
    let i = Annot.next () in
    let e = Q.QAnnot.New.pat e i in
    let annotmap =
      try
        let annot = QmlAnnotMap.find orig_i annotmap_old in
        let annotmap = QmlAnnotMap.add i annot annotmap_new in
        annotmap
      with
        QmlAnnotMap.AnnotNotFound _ ->
          annotmap_new in
    annotmap, e

  let shallow_copy annotmap e =
    shallow_copy_new ~annotmap_old:annotmap annotmap e

  let shallow_copy_pat annotmap e =
    shallow_copy_new_pat ~annotmap_old:annotmap annotmap e

  let shallow_copys annotmap l =
    List.fold_right
      (fun e (annotmap, acc) ->
         let annotmap, e = shallow_copy annotmap e in
         annotmap, e::acc
      ) l (annotmap, [])

  let copy = QmlAstWalk.ExprPatt.foldmap_down shallow_copy shallow_copy_pat

  let copy_new ~annotmap_old annotmap_new e =
    let shallow_copy an x = shallow_copy_new ~annotmap_old an x in
    let shallow_copy_pat an x = shallow_copy_new_pat ~annotmap_old an x in
    QmlAstWalk.ExprPatt.foldmap_down shallow_copy shallow_copy_pat annotmap_new e

  let copy_new_when_possible ~annotmap_old annotmap_new e =
    let shallow_copy an x = shallow_copy_new_when_possible ~annotmap_old an x in
    let shallow_copy_pat an x = shallow_copy_new_when_possible_pat ~annotmap_old an x in
    QmlAstWalk.ExprPatt.foldmap_down shallow_copy shallow_copy_pat annotmap_new e

  (* similar version, with an extra source -> dest IntMap *)
  let copy_with_trace annotmap e =
    let shallow_copy_with_trace_expr (annotmap, convmap) e =
      let annotmap, new_e = shallow_copy annotmap e in
      let convmap = AnnotMap.add (Q.QAnnot.expr e) (Q.QAnnot.expr new_e) convmap in
      (annotmap, convmap), new_e
    in
    let shallow_copy_with_trace_pat (annotmap, convmap) e =
      let annotmap, new_e = shallow_copy_pat annotmap e in
      let convmap = AnnotMap.add (Q.QAnnot.pat e) (Q.QAnnot.pat new_e) convmap in
      (annotmap, convmap), new_e
    in
    QmlAstWalk.ExprPatt.foldmap_down shallow_copy_with_trace_expr shallow_copy_with_trace_pat
      (annotmap, AnnotMap.empty) e

  let ti_from_string gamma str =
    fst
      (QmlTypes.Env.TypeIdent.findi
         ~visibility_applies: true (TypeIdent.of_string str) gamma)

  let ty_int = Q.TypeConst Q.TyInt

  let ty_string = Q.TypeConst Q.TyString

  let ty_float = Q.TypeConst Q.TyFloat

  let bypass ?pos annotmap key ty =
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Bypass (label, key)

  let ident ?pos annotmap id ty =
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Ident (label, id)

  let const ?pos annotmap c =
    let annotmap, label = typed_label ?pos annotmap (Q.TypeConst (Q.Const.type_of c)) in
    annotmap, Q.Const (label, c)

  let unit ?pos annotmap =
    let ty_void =  Q.TypeRecord (Q.TyRow ([], None)) in
    let annotmap, empty_record =
      let annotmap, label = typed_label ?pos annotmap ty_void in
      annotmap, Q.Record (label, [])
    in
    let annotmap, label = typed_label ?pos annotmap ty_void in
    annotmap, Q.Coerce (label, empty_record, ty_void)

  (* the cheapest unit, typed with a typename "void"; works only in OPA *)
  let cheap_void ?pos annotmap gamma =
    let ti_void = ti_from_string gamma Opacapi.Types.void in
    let annotmap, label = typed_label ?pos annotmap (Q.TypeName ([], ti_void)) in
    annotmap, Q.Record (label, [])

  let string ?pos annotmap s =
    let annotmap, label = typed_label ?pos annotmap ty_string in
    annotmap, Q.Const (label, Q.String s)

  let int ?pos annotmap x =
    let annotmap, label = typed_label ?pos annotmap ty_int in
    annotmap, Q.Const (label, Q.Int x)

  let float ?pos annotmap x =
    let annotmap, label = typed_label ?pos annotmap ty_float in
    annotmap, Q.Const (label, Q.Float x)

  (*
    FIXME: why the type put in the annotmap is not relaxed ?
  *)
  let coerce annotmap e ty =
    let pos = Q.Pos.expr e in
    let annotmap, label = typed_label ~pos annotmap ty in
    annotmap, Q.Coerce (label, e, ty)

  let letin annotmap bindings expr =
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap in
    let pos = Q.Pos.expr expr in
    let annotmap, label = typed_label ~pos annotmap ty in
    annotmap, Q.LetIn (label, bindings, expr)

  let letrecin annotmap bindings expr =
    let label = Q.Label.expr expr in
    let ty = QmlAnnotMap.find_ty (Annot.annot label) annotmap in
    let pos = Annot.pos label in
    let annotmap, label = typed_label ~pos annotmap ty in
    annotmap, Q.LetRecIn (label, bindings, expr)

  let record ?pos ?extend annotmap bindings =
    let ty = Q.TypeRecord (Type.Row.make ?extend
      (List.map (fun (f, e) ->
        (f, QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap)
      ) bindings)) in
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Record (label, bindings)

  let sum_element ?pos ?ty annotmap bindings =
    let ty =
      match ty with
      | None ->
          Q.TypeSum
            (Type.Col.make ~extend:true
               [List.map (fun (f, e) ->
                            (f, QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap)
                         ) bindings])
      | Some ty -> ty in
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Record (label, bindings)

(** specialize a typename
 it can failwith when we have a loop in the type definition.
*)
  let ty_of_typename_no_loop gamma ty =
    let rec aux acc = function
      | Q.TypeName (targs, tn) ->
          if List.mem tn acc then failwith "loop in a type definition"
          else
            let (tsc, _) =
              QmlTypes.Env.TypeIdent.find ~visibility_applies:false tn gamma in
            let ty = QmlTypes.Scheme.specialize ~typeident: tn ~ty: targs tsc in
            aux (tn :: acc) ty
      | ty -> ty in
    aux [] ty

  let dot gamma annotmap record field =
    let label = Q.Label.expr record in
    let pos = Annot.pos label in
    let ty_record = QmlAnnotMap.find_ty (Annot.annot label) annotmap in
    let ty =
      let ty = ty_of_typename_no_loop gamma ty_record in
      match ty with
      | Q.TypeRecord row -> (
          match List.assoc_opt field (Type.Row.to_list row) with
          | Some ty -> ty
          | None -> failwith (Printf.sprintf "QmlAstCons.TypedExpr.dot: unable to find type for field %s" field)
        )
      | _ -> failwith (Printf.sprintf "QmlAstCons.TypedExpr.dot: unable to find type for field %s (not a record)" field)
    in
    let annotmap, label = typed_label ~pos annotmap ty in
    annotmap, Q.Dot (label, record, field)

  (* Below there are no type checks on arguments, because invoking HMX compare
     is costly and needs refactoring to work around cyclic dependencies
     and QML compare is an approximation, so it may cause false alarms.
     Eventually, the type correctness of the AST should be checked
     by pre and postconditions of passes (however, right now after some passes
     the code no longer type-checks, thought it's morally type-correct).

     TODO: What kind of errors should we raise in arity checks here?
     Mikolaj says: Internal errors are OK for me, this really shoudn't happen.
  *)

  let apply_gen gamma annotmap ~partial func params =
    let params_len = List.length params in
    let label = Q.Label.expr func in
    let pos = Annot.pos label in
    let annot = Annot.annot label in
    let ty_func = QmlAnnotMap.find_ty annot annotmap in
    let rec aux ty_func =
      let ty_func = ty_of_typename_no_loop gamma ty_func in
      match ty_func with
      | Q.TypeArrow (tparams, ty) ->
          let arity = List.length tparams in
          if partial then (
            let tparams, ty =
              if params_len <= arity then
                tparams, ty
              else (
                let ty_func = ty_of_typename_no_loop gamma ty in
                match ty_func with
                | Q.TypeArrow (more_params,more_ty) ->
                    let tparams = tparams @ more_params in
                    if params_len = List.length tparams then
                      tparams, more_ty
                    else (
                      OManager.i_error "QmlAstCons.TypedExpr.apply_gen: try to apply %d args to a function [%a] with %d-%d parameters (type = %a) (at %s)" params_len QmlPrint.pp#expr func (List.length tparams) (List.length more_params) QmlPrint.pp#ty (QmlAnnotMap.find_ty annot annotmap) (FilePos.to_string pos)
                    )
                | _ -> assert false
              ) in
            let tparams = List.drop params_len tparams in
            let ty = if tparams = [] then ty else Q.TypeArrow (tparams,ty) in
            let annotmap, label = typed_label ~pos annotmap ty in
            annotmap, Q.Apply (label, func, params)
          ) else (
            if arity <> params_len then (
              invalid_arg (Format.sprintf "QmlAstCons.TypedExpr.apply_gen: try to apply %d args to a function [%a] with %d parameters (type = %a) (at %s)" params_len (QmlPrint.pp#expr) func arity QmlPrint.pp#ty ty_func (FilePos.to_string pos))
            );
            let annotmap, label = typed_label ~pos annotmap ty in
            annotmap, Q.Apply (label, func, params)
          )
      | Q.TypeVar _ ->
          (* After typing this case should not happen, but if we insert some
             code into AST in a lazy way using typevars, this is required.
             The coerce is (was?) probably needed by some OPA pass
             and documents in the AST our assumption about types.
             As soon as it does not break any OPA pass, we may change
             [tparams] below to the possibly more precise types found
             in annotmap for [params], as follows:
               let tparams = List.map (fun param -> QmlAnnotMap.find_ty param.annot annotmap) params in
          *)
          let tparams = List.map (fun _ -> Q.TypeVar (TypeVar.next ())) params in
          let t_body = Q.TypeVar (TypeVar.next ()) in
          let t_func = Q.TypeArrow (tparams, t_body) in
          let annotmap, func = coerce annotmap func t_func in
          let annotmap, label = typed_label ~pos annotmap t_body in
          annotmap, Q.Apply (label, func, params)

      | _ -> invalid_arg (Format.sprintf "QmlAstCons.TypedExpr.apply_gen: %a" QmlPrint.pp#ty ty_func) in
          (* FIXME: this may happen in overloads; check if in any other case. *)
    aux ty_func

  let apply gamma annotmap func params =  apply_gen gamma annotmap ~partial:false func params
  let apply_partial gamma annotmap func params = apply_gen gamma annotmap ~partial:true func params

  (**
     Apply arguments to a function - or, if the list of arguments is empty, return just the function

     @param params A list of arguments for the application
  *)
  let may_apply gamma annotmap func params =
    match params with
    | [] -> annotmap, func
    | _ -> apply gamma annotmap func params

  let apply_ty ?pos annotmap func params ty =
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Apply (label, func, params)

  let directive_ty ?pos annotmap dir exprs tys ty =
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Directive (label, dir, exprs, tys)

  let lambda ?pos annotmap lit body =
    let label = Q.Label.expr body in
    let pos = Option.default (Annot.pos label) pos in
    let annot = Annot.annot label in
    let ty_body = QmlAnnotMap.find_ty annot annotmap in
    let params, typarams = Base.List.split lit in
    let ty = Q.TypeArrow (typarams, ty_body) in
    let annotmap, label = typed_label ~pos annotmap ty in
    annotmap, Q.Lambda (label, params, body)

  let may_lambda ?pos annotmap lit body =
    match lit with
    | [] -> annotmap, body
    | _ -> lambda ?pos annotmap lit body

  let directive ?pos annotmap variant exprs tys  =
    let dty =
      match QmlDirectives.ty variant exprs tys with
      | Q.TypeArrow (args, return) ->
          assert (List.length args = List.length exprs);
          return
      | _ -> assert false in
    let annotmap, label = typed_label ?pos annotmap dty in
    annotmap, Q.Directive (label, variant, exprs, tys)

  let directive_id ?pos annotmap variant expr =
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap in
    directive_ty ?pos annotmap variant [expr] [] ty

  let tagged_string ?pos annotmap string kind =
    directive ?pos annotmap (`tagged_string (string, kind)) [] []

  let rev_list ?pos ?(ty=Type.typevar (TypeVar.next ())) (annotmap, gamma) l =
    let ti_list = ti_from_string gamma Opacapi.Types.list in
    let ty_list = Q.TypeName ([ty], ti_list) in
    let annotmap, unit = unit ?pos annotmap in
    List.fold_left (
      fun (annotmap, r) e ->
        let annotmap, label = typed_label ?pos annotmap ty_list in
        annotmap, Q.Record (label, [("hd", e);("tl", r)])
    )
      (
        let annotmap, label = typed_label ?pos annotmap ty_list in
        annotmap, Q.Record (label, [("nil", unit)])
      )
      l

  let list ?pos ?ty p l = rev_list ?pos ?ty p (List.rev l)

  let rev_list_map ?pos ?ty f (annotmap, gamma) rev_lst =
    let annotmap, lst = List.fold_left_rev_map f annotmap rev_lst in
    list ?pos ?ty (annotmap,gamma) lst

  let list_map ?pos ?ty f (annotmap,gamma) lst =
    let annotmap, rev_lst = List.fold_left_rev_map f annotmap lst in
    rev_list ?pos ?ty (annotmap,gamma) rev_lst

  let simple_record ~ty field ?pos (annotmap,_gamma) =
    let annotmap, u = unit ?pos annotmap in
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Record (label, [(field,u)])

  let _false_no_named_type, _true_no_named_type =
    let ty_void = Q.TypeRecord (Q.TyRow ([],None)) in
    let ty = Q.TypeSum (Q.TyCol ([["false", ty_void];["true", ty_void]],None)) in
    simple_record ~ty "false", simple_record ~ty "true"

  let bool ?pos (annotmap, gamma) bool =
    let ti_bool = ti_from_string gamma Opacapi.Types.bool in
    simple_record ~ty:(Q.TypeName ([], ti_bool)) (if bool then "true" else "false") ?pos (annotmap, gamma)

  let _true ?pos annotmap_gamma = bool ?pos annotmap_gamma true
  let _false ?pos annotmap_gamma = bool ?pos annotmap_gamma false

  let bool_no_named_type ?pos annotmap_gamma b =
    (if b then _true_no_named_type else _false_no_named_type) ?pos annotmap_gamma

  let opa_tuple_2 ?pos (annotmap, gamma) (e1, e2) =
    let ti_tuple_2 =
      fst
        (QmlTypes.Env.TypeIdent.findi
           ~visibility_applies: true (TypeIdent.of_string Opacapi.Types.tuple_2) gamma)
    in
    let t1 = QmlAnnotMap.find_ty (Q.QAnnot.expr e1) annotmap in
    let t2 = QmlAnnotMap.find_ty (Q.QAnnot.expr e2) annotmap in
    let ty = Q.TypeName ([ t1 ; t2 ], ti_tuple_2) in
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Record (label, [ ("f1", e1) ; ("f2", e2) ])

  let lambda_coerce annotmap id ty_id body =
    let pos = Q.Pos.expr body in
    let annotmap, label = typed_label ~pos annotmap ty_id in
    let patvar = Q.PatVar (label, id) in
    let annotmap, label = typed_label ~pos annotmap ty_id in
    let patvarcoerce = Q.PatCoerce (label, patvar, ty_id) in
    let fresh_id = Ident.next "match" in
    let annotmap, matchexp = ident ~pos annotmap fresh_id ty_id in
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr body) annotmap in
    let annotmap, label = typed_label ~pos annotmap ty in
    let matchbody = Q.Match (label, matchexp, [patvarcoerce, body]) in
    lambda ~pos annotmap [fresh_id, ty_id] matchbody

  let some ?pos annotmap gamma expr =
    let t_opt =
      fst
        (QmlTypes.Env.TypeIdent.findi
           ~visibility_applies: true (TypeIdent.of_string Opacapi.Types.option) gamma)
    in
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap in
    let ty = Q.TypeName ([ty], t_opt) in
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Record (label, ["some", expr])

  let none ?pos ?(ty=Type.typevar (TypeVar.next ()))
      annotmap gamma =
    let t_opt =
      fst
        (QmlTypes.Env.TypeIdent.findi
           ~visibility_applies: true (TypeIdent.of_string Opacapi.Types.option) gamma)
    in
    let annotmap, void = unit ?pos annotmap in
    let ty = Q.TypeName ([ty], t_opt) in
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Record (label, ["none", void])

  let match_ty ?pos annotmap matched l ty =
    assert(l != []);
    let annotmap, label = typed_label ?pos annotmap ty in
    annotmap, Q.Match (label, matched, l)

  let match_ ?pos annotmap matched l =
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr (snd (List.hd l))) annotmap in
    match_ty ?pos annotmap matched l ty
end


module TypedPat =
struct

  type annotmap = Q.annotmap

  type gamma = QmlTypes.Env.t

  let nopos = FilePos.nopos "QmlAstCons.TypedPat"

  let make ?pos annotmap pat ty =
    let pos =
      match pos with
      | Some pos -> pos
      | None ->
          Q.Pos.pat pat
    in
    let annot = Annot.next () in
    let annotmap = QmlAnnotMap.add_ty annot ty annotmap in
    let label = Annot.make_label annot pos in
    let pat = Q.Label.New.pat pat label in
    annotmap, pat

  let copy_new_when_possible ~annotmap_old annotmap_new p =
    let shallow_copy_pat an x = TypedExpr.shallow_copy_new_when_possible_pat ~annotmap_old an x in
    QmlAstWalk.Pattern.foldmap_down shallow_copy_pat annotmap_new p

  let copy annotmap p = copy_new_when_possible ~annotmap_old:annotmap annotmap p

  let var ?pos annotmap ident ty =
    let annotmap, label = typed_label annotmap ?pos ty in
    annotmap, Q.PatVar (label, ident)

  let any ?pos ?(ty=Type.typevar (TypeVar.next ())) annotmap =
    let annotmap, label = typed_label annotmap ?pos ty in
    annotmap, Q.PatAny (label)

  let emptyrecord ?pos annotmap =
    let annotmap, label = typed_label annotmap ?pos (Q.TypeRecord (Type.Row.make [])) in
    annotmap, Q.PatRecord (label, [], `closed)

  let record ?pos ?(extend=false) annotmap fields =
    let ty_fields =
      List.fold_right
        (fun (f, pat) ty_fields ->
           let ty_fields =
             (f, QmlAnnotMap.find_ty (Q.QAnnot.pat pat) annotmap) :: ty_fields
           in
           ty_fields
        ) fields []
    in
    let annotmap, label = typed_label ?pos annotmap (Q.TypeRecord (Type.Row.make ~extend ty_fields)) in
    let pat0 = Q.PatRecord (label, fields, if extend then `open_ else `closed) in
    annotmap, pat0

  let tuple ?pos annotmap l =
    record ?pos annotmap (List.mapi (fun i v -> ("f"^string_of_int (i+1), v)) l)

  let some ?pos annotmap gamma pat =
    let t_opt =
      fst
        (QmlTypes.Env.TypeIdent.findi
           ~visibility_applies: true (TypeIdent.of_string Opacapi.Types.option) gamma)
    in
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.pat pat) annotmap in
    let annotmap, label = typed_label ?pos annotmap (Q.TypeName ([ty], t_opt)) in
    annotmap, Q.PatRecord (label, ["some", pat], `closed)

  let none ?pos ?(ty=Type.typevar (TypeVar.next ()))
      annotmap gamma =
    let t_opt =
      fst
        (QmlTypes.Env.TypeIdent.findi
           ~visibility_applies: true (TypeIdent.of_string Opacapi.Types.option) gamma)
    in
    let annotmap, pat1 = emptyrecord ?pos annotmap in
    let annotmap, label = typed_label ?pos annotmap (Q.TypeName ([ty], t_opt)) in
    annotmap, Q.PatRecord (label, ["none", pat1], `closed)

  let bool ?pos bool annotmap gamma =
    let tbool = fst
      (QmlTypes.Env.TypeIdent.findi
         ~visibility_applies: true (TypeIdent.of_string Opacapi.Types.bool) gamma) in
    let annotmap, pat1 = emptyrecord ?pos annotmap in
    let annotmap, label = typed_label ?pos annotmap (Q.TypeName ([], tbool)) in
    annotmap, Q.PatRecord (label, [(if bool then "true" else "false"), pat1], `closed)

  let match_option ?pos annotmap gamma
      matched patsome ok_expr ko_expr =
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.pat patsome) annotmap in
    let annotmap, patsome =
      some annotmap gamma patsome in
    let annotmap, patnone =
      none ~ty annotmap gamma in
    TypedExpr.match_ ?pos annotmap matched [patsome, ok_expr; patnone, ko_expr]

  let list ?pos annotmap pats =
    let annotmap, pat_list =
      let annotmap, e1 = emptyrecord ?pos annotmap in
      record ?pos annotmap ["nil", e1] in
    List.fold_right
      (fun pat (annotmap, acc) ->
         record ?pos annotmap ["hd", pat ; "tl", acc]
      ) pats (annotmap, pat_list)

  let ifthenelse ?pos annotmap gamma cond e1 e2 =
    let annotmap, pattrue = bool ?pos true annotmap gamma in
    let annotmap, patfalse = bool ?pos false annotmap gamma in
    TypedExpr.match_ ?pos annotmap cond [pattrue, e1; patfalse, e2]

end

module TypedCode =
struct
  let copy_new ~annotmap_old annotmap code =
    QmlAstWalk.CodeExpr.fold_map
      (fun annotmap e -> TypedExpr.copy_new ~annotmap_old annotmap e)
      annotmap
      code
  let copy_new_when_possible ~annotmap_old annotmap code =
    QmlAstWalk.CodeExpr.fold_map
      (fun annotmap e -> TypedExpr.copy_new_when_possible ~annotmap_old annotmap e)
      annotmap
      code
end

module UntypedExprWithLabel =
struct
  let nopos = FilePos.nopos "untyped-with-label"

  let ident ?(label=Annot.next_label nopos) i = Q.Ident (label, i)
  let source ?(label=Annot.next_label nopos) s = ident ~label (Ident.source s)
  let fresh_internal ?(label=Annot.next_label nopos) n = ident ~label (Ident.next n)

  let const ?(label=Annot.next_label nopos) e = Q.Const (label, e)
  let int ?(label=Annot.next_label nopos) i = const ~label (Q.Int i)
  let float ?(label=Annot.next_label nopos) f = const ~label (Q.Float f)
  let string ?(label=Annot.next_label nopos) s = const ~label (Q.String s)

  let letin ?(label=Annot.next_label nopos) l e = Q.LetIn (label, l, e)
  let letrecin ?(label=Annot.next_label nopos) l e = Q.LetRecIn (label, l, e)
  let lambda1 ?(label=Annot.next_label nopos) i e = Q.Lambda (label, [i], e)
  let lambda ?(label=Annot.next_label nopos) il e = Q.Lambda (label, il, e)
  let directive ?(label=Annot.next_label nopos) d exprs tys = Q.Directive (label, d, exprs, tys)
  let apply ?(label=Annot.next_label nopos) f bl = Q.Apply (label, f, bl)
  let may_apply ?label f bl =
    match bl with
    | [] -> f
    | _ -> apply ?label f bl
  let apply1 ?(label=Annot.next_label nopos) f bl = apply ~label f [bl]
  let match_ ?(label=Annot.next_label nopos) e p = Q.Match (label, e, p)
  let record ?(label=Annot.next_label nopos) r = Q.Record (label, r)
  let dot ?(label=Annot.next_label nopos) e f = Q.Dot (label, e, f)
  let extendrecord ?(label=Annot.next_label nopos) f e n = Q.ExtendRecord (label, f, e, n)
  let bypass ?(label=Annot.next_label nopos) s = Q.Bypass (label, s)
  let coerce ?(label=Annot.next_label nopos) e t = Q.Coerce (label, e, t)
end

type stateful_constructor =
  < make : Q.expr -> Q.ty -> Q.expr;
    make_from_annot : Q.expr -> Annot.t -> Q.expr;
    copy : Q.expr -> Q.expr;
    shallow_copy_new : annotmap_old:annotmap -> Q.expr -> Q.expr;
    shallow_copy : Q.expr -> Q.expr;
    copy_new : annotmap_old:annotmap -> Q.expr -> Q.expr;

    directive : Q.qml_directive -> Q.expr list -> Q.ty list -> Q.expr ;

    ident : Q.ident -> Q.ty -> Q.expr;
    ident_from_annot : Q.ident -> Annot.t -> Q.expr;
    const : Q.const_expr -> Q.expr;
    unit : Q.expr;
    cheap_void : Q.expr;
    int : int -> Q.expr;
    float : float -> Q.expr;
    string : string -> Q.expr;

    coerce : Q.expr -> Q.ty -> Q.expr;
    letin : Q.ident -> Q.expr -> Q.expr -> Q.expr;
    letins : (Q.ident * Q.expr) list -> Q.expr -> Q.expr;
    letrec : Q.ident -> Q.expr -> Q.expr -> Q.expr;
    letrecs : (Q.ident * Q.expr) list -> Q.expr -> Q.expr;
    lambda : (Q.ident * Q.ty) list -> Q.expr -> Q.expr;
    lambda_from_annot : (Q.ident * Annot.t) list -> Q.expr -> Q.expr;
    apply : Q.expr -> Q.expr list -> Q.expr;

    record : (string * Q.expr) list -> Q.expr;
    dot : Q.expr -> string -> Q.expr;

    list : Q.expr list -> Q.expr;
    false_: Q.expr;
    true_: Q.expr;
    bool : bool -> Q.expr ;

    opa_tuple_2 : Q.expr * Q.expr -> Q.expr;

    some : Q.expr -> Q.expr;
    none : ?ty:Q.ty -> unit -> Q.expr;

    bypass : BslKey.t -> Q.ty -> Q.expr;
    bypass_from_annot : BslKey.t -> Annot.t -> Q.expr;
    bypass_from_typer : BslKey.t -> (BslKey.t -> Q.ty option) -> Q.expr;

    patvar : Q.ident -> Q.ty -> Q.pat;
    patany : Q.pat;
    patlist : Q.pat list -> Q.pat;
    match_ : Q.expr -> (Q.pat * Q.expr) list -> Q.expr;

    typed : bool;
    gamma : TypedExpr.gamma;

    tyname : string -> Q.ty list -> Q.ty;
    tyoption : Q.ty -> Q.ty;
    tylist : Q.ty -> Q.ty;

    add_to_gamma : Q.ident -> Q.expr -> unit;
  >

let make_typed_cons gamma annotmap =
  (* if using an object slow noticeably the construction of ast, it could be replaced by a simple record *)
  let obj =
object (self)
    val mutable annotmap = annotmap
    val mutable gamma = gamma
    method wrap : 'a. annotmap * 'a -> 'a = fun (a,e) -> annotmap <- a; e

    method make expr ty = self#wrap (TypedExpr.make annotmap expr ty)
    method make_from_annot expr annot = self#make expr (QmlAnnotMap.find_ty annot annotmap)
    (* method make_from_annotated : 'a. expr0 -> 'a Q.annot -> expr = *)
    (*   fun expr0 node -> self#make_from_annot expr0 node.Q.annot *)
    method copy e = self#wrap (TypedExpr.copy annotmap e)
    method shallow_copy_new ~annotmap_old e = self#wrap (TypedExpr.shallow_copy_new ~annotmap_old annotmap e)
    method shallow_copy e = self#wrap (TypedExpr.shallow_copy annotmap e)
    method copy_new ~annotmap_old e = self#wrap (TypedExpr.copy_new ~annotmap_old annotmap e)

    method directive variant exprs tys = self#wrap (TypedExpr.directive annotmap variant exprs tys)

    (* do copy with trace ? *)
    method ident ident ty = self#wrap (TypedExpr.ident annotmap ident ty)
    method ident_from_annot ident annot = self#ident ident (QmlAnnotMap.find_ty annot annotmap)
    method const const_expr = self#wrap (TypedExpr.const annotmap const_expr)
    method unit = self#wrap (TypedExpr.unit annotmap)
    method cheap_void = self#wrap (TypedExpr.cheap_void annotmap gamma)
    method int int = self#wrap (TypedExpr.int annotmap int)
    method float float = self#wrap (TypedExpr.float annotmap float)
    method string string = self#wrap (TypedExpr.string annotmap string)
    method coerce expr ty = self#wrap (TypedExpr.coerce annotmap expr ty)
    method letin i e1 e2 = self#wrap (TypedExpr.letin annotmap [i,e1] e2)
    method letins iel e2 = self#wrap (TypedExpr.letin annotmap iel e2)
    method letrec i e1 e2 = self#wrap (TypedExpr.letrecin annotmap [i,e1] e2)
    method letrecs iel e2 = self#wrap (TypedExpr.letrecin annotmap iel e2)
    method lambda ityl e = self#wrap (TypedExpr.lambda annotmap ityl e)
    method lambda_from_annot iannotl e =
      let annotmap = annotmap in
      self#lambda (List.map (fun (i,annot) -> (i,QmlAnnotMap.find_ty annot annotmap)) iannotl) e
    method apply e el = self#wrap (TypedExpr.apply gamma annotmap e el)
    method record sel = self#wrap (TypedExpr.record annotmap sel)
    method dot e s = self#wrap (TypedExpr.dot gamma annotmap e s)
    method list el = self#wrap (TypedExpr.list (annotmap, gamma) el)
    method false_ = self#wrap (TypedExpr._false (annotmap, gamma))
    method true_ = self#wrap (TypedExpr._true (annotmap, gamma))
    method bool b = if b then self#true_ else self#false_
    method opa_tuple_2 p = self#wrap (TypedExpr.opa_tuple_2 (annotmap, gamma) p)
    method some e = self#wrap (TypedExpr.some annotmap gamma e)
    method none ?ty () = self#wrap (TypedExpr.none ?ty annotmap gamma)
    method bypass name ty = self#wrap (TypedExpr.bypass annotmap name ty)
    method bypass_from_annot name annot = self#make_from_annot (UntypedExpr.bypass name) annot
    method bypass_from_typer name typer = self#wrap (TypedExpr.bypass annotmap name (Option.get (typer name)))

    method patvar ident ty = self#wrap (TypedPat.var annotmap ident ty)
    method patany = self#wrap (TypedPat.any annotmap)
    method patlist pl = self#wrap (TypedPat.list annotmap pl)
    method match_ e pel = self#wrap (TypedExpr.match_ annotmap e pel)

    method tyname name tyl =
      let (ty, _) =
        QmlTypes.type_of_type
          gamma (Q.TypeName (tyl,TypeIdent.of_string name)) in
      ty
    method tylist ty = self#tyname "list" [ty]
    method tyoption ty = self#tyname Opacapi.Types.option [ty]

    method typed = true
    method gamma = gamma
    method annotmap = annotmap

    method add_to_gamma ident (expr:Q.expr) =
      let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap in
      let tsc = QmlTypes.Scheme.quantify ty in
      gamma <- QmlTypes.Env.Ident.add ident tsc gamma

  end in
  (obj :> stateful_constructor), (fun () -> (obj#gamma, obj#annotmap))

let untyped_cons : stateful_constructor =
  let module C = UntypedExpr in
object(self)
    method make expr _ty = Q.QAnnot.Refresh.expr expr
    method make_from_annot expr _annot = Q.QAnnot.Refresh.expr expr
    (* method make_from_annotated : 'a. expr0 -> 'a Q.annot -> expr = *)
    (*   fun expr0 _node -> C.fresh_annot expr0 *)
    method copy = C.copy
    method shallow_copy_new ~annotmap_old:_ expr = Q.QAnnot.Refresh.expr expr
    method shallow_copy expr = Q.QAnnot.Refresh.expr expr
    method copy_new ~annotmap_old:_ e = C.copy e
    method directive variant exprs tys = UntypedExpr.directive variant exprs tys
    method ident ident _ty = C.ident ident
    method ident_from_annot ident _annot = C.ident ident
    method const const = C.const const
    method unit = C.unit ()
    method cheap_void = C.unit ()
    method int int = C.int int
    method float float = C.float float
    method string string = C.string string
    method coerce = C.coerce
    method letin i e1 e2 = C.letin [i,e1] e2
    method letins = C.letin
    method letrec i e1 e2 = C.letrecin [i,e1] e2
    method letrecs = C.letrecin
    method lambda ityl e = C.lambda (List.map fst ityl) e
    method lambda_from_annot iannotl e = C.lambda (List.map fst iannotl) e
    method apply e1 params = C.apply e1 params
    method record iel = C.record iel
    method dot = C.dot
    method list el = C.list el
    method false_ = C._false ()
    method true_ = C._true ()
    method bool b = if b then self#true_ else self#false_
    method opa_tuple_2 (e1,e2) = C.opa_tuple [e1;e2]
    method some e = C.record ["some",e]
    method none ?ty:_ () = C.record ["none",C.unit ()]
    method bypass name _ty = UntypedExpr.bypass name
    method bypass_from_annot name _annot = UntypedExpr.bypass name
    method bypass_from_typer name _typer = UntypedExpr.bypass name
    method typed = false
    method gamma = assert false

    method patvar ident _ty = UntypedExpr.patvar ident
    method patany = UntypedExpr.patany ()
    method patlist pl = UntypedExpr.patlist pl
    method match_ e pel =  UntypedExpr.match_ e pel


    (* FIXME *)
    method tyname = assert false
    method tylist = assert false
    method tyoption = assert false

    method add_to_gamma = assert false
  end

let make_cons ~typed gamma annotmap =
  if typed then
    make_typed_cons gamma annotmap
  else
    untyped_cons, (fun () -> gamma, annotmap)
