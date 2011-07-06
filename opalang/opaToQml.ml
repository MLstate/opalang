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

(* depends *)
module List = BaseList

let (|>) = InfixOperator.(|>)

(* shorthands *)
module QA = QmlAst
module QC = QmlAstCons
module SA = SurfaceAst
module SH = SurfaceAstHelper
module L = QmlLoc

(* open QmlTypeVarsScope *)

(* errors *)

type error = string
exception Exception of error
external string_of_error : error -> string = "%identity"

let (!!!) fmt =
  Format.ksprintf (fun s -> raise (Exception s)) fmt

(*
  Options of the conversion, set before a code transformation
*)

(* HACK : please, clean-up in opa lang *)
module Parser_utils = OpaParserUtils

type options = unit

let set_options () = ()

let options = ()


let propagate_slicer_annotation e =
  let module D = SurfaceAstDecons in
  let throw = [D.Remove.Basic.letin;
               D.Remove.Basic.opacapi;
               D.Remove.Basic.coerce;
               D.Remove.Basic.doctype] in
  let _, visibility_annotater, side_annotater =
    D.Context.filter2
      ~keep1:[D.Context.Basic.visibility_annotation]
      ~keep2:[D.Context.Basic.side_annotation]
      ~throw
      e in
  let reannotate e =
    let e =
      if D.Look.at ~at:[D.Remove.Basic.visibility_annotation]
                   ~through:(D.Remove.Basic.side_annotation :: throw)
                   e
      then e
      else visibility_annotater e in
    let e =
      if D.Look.at ~at:[D.Remove.Basic.side_annotation]
                   ~through:(D.Remove.Basic.visibility_annotation :: throw)
                   e
      then e
      else side_annotater e in
    e in
  reannotate

(* a few utility functions that simplifies opa patterns
 * because they are more expressive that old qml ones (no coerce, no 'as')
 * and qml ones (no 'as)
 *)
module PatternUtils (C : SurfaceAstConsSig.CONS) =
struct
  let next = C.I.ns_fresh

  let copy_label = Parser_utils.copy_label
  let same_pos v (_, label) = (v, copy_label label)

  let simplify_lambda (r,e) =
    List.fold_right_map
      (fun (s,p) e ->
         SurfaceAstCons.with_same_pos p (fun () -> let label = snd p in
         match fst p with
           | SA.PatAny
           | SA.PatVar _ ->
               (s,p), e
           | SA.PatCoerce ((SA.PatRecord ([], `closed), _), _) ->
               (* special case for void so that the code is more readable *)
               let i = next ~label "remove_void" in
               let void = next ~label "void" in
               (s, C.P.var void),
               same_pos (SA.LetIn (false,[(i, C.T.coerce (same_pos (SA.Ident void) p) (C.T.void ()))], e)) p
           | SA.PatCoerce ((SA.PatVar v, l) as p, ty) ->
               let i = next ~label "remove_coerce" in
               (s, p),
               (same_pos (SA.LetIn (false,[(i,C.T.coerce (SA.Ident v,l) ty)], e)) e)
           | _ ->
               let i = next ~label "simplify_lambda" in
               (s, same_pos (SA.PatVar i) p),
               (same_pos (SA.Match (same_pos (SA.Ident i) p, [(p, e)])) e)
         )
      ) r e

  (* FIXME: move me *)
  let map2_2 f (x,y) = (x, f y)
  let (@>) f g x = x |> f |> g

  let rec rebuild ~and_coerces map ((p,label) as pp) =
    SurfaceAstCons.with_label label (fun () ->
    match IntMap.find_opt label.QmlLoc.notes map with
      | Some i -> pp, (SA.Ident i, copy_label label)
      | None ->
    match p with
      | SA.PatVar a ->
          pp, (SA.Ident a, copy_label label)
      | SA.PatRecord (spl, rowvar) ->
          if rowvar = `open_ then (
            let context = OpaError.Context.annot label in
            OpaError.error context (
              "You cannot put a 'as'%s around a '...' pattern"
            )
              (if and_coerces then " or a coercion (which includes tuples and lists patterns)" else "")
          ) ;
          let sl, pel = List.split (List.map (map2_2 (rebuild ~and_coerces map)) spl) in
          let pl, el = List.split pel in
          let spl = List.combine sl pl in
          let sel = List.combine sl el in
          (SA.PatRecord (spl, `closed), label), (SA.Record sel, copy_label label)
      | SA.PatConst c ->
          pp, (SA.Const c, label)
      | SA.PatCoerce (p,ty) ->
          let p, e = rebuild ~and_coerces map p in
          p, C.T.coerce e ty
      | SA.PatAs (p,i) ->
          p, (SA.Ident i, label)
      | SA.PatAny ->
          let i = next ~label "rebuild" in
          (SA.PatVar i, label), (SA.Ident i, copy_label label)
    )

  let remove_as ~and_coerces p e =
    let (_map,acc),p =
      SurfaceAstTraversal.PatTraverse.foldmap_up
        (fun (map,acc) ((p,label) as p') ->
           match p with
             | SA.PatAs (p, s) ->
                 let p,e = rebuild ~and_coerces map p in
                 let map = IntMap.add label.QmlLoc.notes s map in
                 let map = IntMap.add (snd p).QmlLoc.notes s map in
                   (map, (s,e,p)::acc), p
             | SA.PatCoerce (pc, _) when and_coerces ->
                 let i = next ~label "remove_coerce" in
                 let p,e = rebuild ~and_coerces map p' in
                 let map = IntMap.add label.QmlLoc.notes i map in
                 let map = IntMap.add (snd pc).QmlLoc.notes i map in
                   (map, (i,e,p)::acc), p
             | _ ->
                 (map, acc), p'
        ) (IntMap.empty,[]) p in
    let acc = List.fold_left (fun acc (i,e,p) -> same_pos (SA.LetIn (false,[(i, e)],acc)) p) e acc in
    acc,p

      (* p = expr *)
  let rec pattern_to_bindings expr p =
    let label = snd p in
    match fst p with
      | SA.PatVar v ->
          [(v, SurfaceAstCons.Refresh.expr expr)]
      | SA.PatRecord (spl, _) ->
          List.concat_map
            (fun (s, p) ->
               pattern_to_bindings (same_pos (SA.Dot (expr, s)) p) p
            ) spl
      | SA.PatAny _ -> []
      | SA.PatConst _ ->
          (* how to rewrite 2 = x in Qml ?
           * one could do assert (2 = x)
           * or _ = match x with 2 -> {} | _ -> error("")
           *)
          assert false
      | SA.PatAs (p, s) ->
          (s, expr) :: pattern_to_bindings expr p
      | SA.PatCoerce (p, ty) ->
          let i = next ~label "pattern_to_bindings" in
          SurfaceAstCons.with_same_pos p (fun () ->
            (i, C.T.coerce expr ty) :: pattern_to_bindings expr p
          )
end

(* functorisation for the 2 traductions *)
module type ARG =
sig
  (** The type of the parameter 'ident of OpaAst *)
  type ident
  val to_string : ident -> string
  val of_string : string -> ident
  val typevar : ident -> QA.typevar
  val rowvar : ident -> QA.rowvar
  val colvar : ident -> QA.colvar
  val add_local_scope : unit -> unit
  val remove_local_scope : unit -> unit
  val get_local_vars : unit -> QA.typevar list * QA.rowvar list * QA.colvar list
  val reset_var_scopes : unit -> unit
    (** should be invoked before each top-level phrase *)
  val typeident : ?check:bool -> ident -> QA.TypeIdent.t
  val exprident : ident -> Ident.t
  val pp_print_directive : (ident, [< SurfaceAst.all_directives ]) SurfaceAst.directive LangPrint.pprinter
end

module MakeOpaToQml (C : SurfaceAstConsSig.CONS) (Arg : ARG with type ident = C.ident) =
struct
  module PatternUtils = PatternUtils (C)

  let qlabel sa_label = Annot.next_label sa_label.QmlLoc.pos

  let keep_magic_directive = false

  let fail p s = raise (Exception (Printf.sprintf "%s : %s" (SH.Annot.to_string' p) s))
    (* indicate mostly a node that can't be converted to the new ast *)

  let rec const_ty_node = function
    | SA.TyInt -> QA.TyInt
    | SA.TyFloat -> QA.TyFloat
    | SA.TyString -> QA.TyString
    | SA.TyChar -> QA.TyChar

  and ty x = ty_node (fst x)
  and ty_node = function
    | SA.TypeConst c -> QA.TypeConst (const_ty_node c)
    | SA.TypeVar (SA.Flatvar tv) -> QA.TypeVar (Arg.typevar tv)
    | SA.TypeArrow arrow -> typearrow_aux arrow
    | SA.TypeRecord row -> typerecord row
    | SA.TypeSumSugar ts -> ty_sum ts
        (* if accept_anonymous_sum_type.contents then ty_sum ts else fail (List.hd ts) "Anonymous sum types are not yet supported in this version of the compiler. Please define the sum type and use the name from the definition instead of the anonymous sum type. (Note that, e.g, type expression \"private({a} / {b})\" contains an anonymous sum type!)" *)
    | SA.TypeNamed (SA.Typeident s,tyl) ->
        QA.TypeName (List.map ty tyl, Arg.typeident ~check:false s)
    | SA.TypeExternal -> QA.TypeAbstract
    | SA.TypeForall (vars, t) ->
        QA.TypeForall
          (List.map (function (SA.Flatvar v) -> Arg.typevar v) vars,
           [], [], ty t)
    | SA.TypeModule fields ->
        let aux_module_field (s, t) =
          Arg.add_local_scope ();
          let t = ty t in
          let (ty_vars, row_vars, col_vars) as vars = Arg.get_local_vars () in
          let t_quantified =
            if vars = ([], [], []) then t
            else QA.TypeForall (ty_vars, row_vars, col_vars, t) in
          Arg.remove_local_scope () ;
          (s, t_quantified) in
        let fields = List.map aux_module_field fields in
        QA.TypeRecord(QA.TyRow(fields, None))

  and ty_sum ts =
    let ts', last = List.extract_last ts in
    let ts, colvar =
      match last with
      | SA.SumVar (SA.Colvar v),_ -> ts', Some (Arg.colvar v)
      | _ -> ts, None in
    let is_TypeRecord = function
      | SA.SumRecord (SA.TyRow (_, None)),_ -> true
      | _ -> false in
    if List.for_all is_TypeRecord ts then (
      QA.TypeSum
        (QA.TyCol
           (List.map
              (function
               | (SA.SumRecord row,_) ->
                   let fields, rowvar = typerow row in
                   assert (rowvar = None) ;
                   fields
               | _  -> assert false
              ) ts, colvar))
    )
    else (
      assert (colvar = None) ;
      QA.TypeSumSugar
        (List.map
           (function
            | (SA.SumRecord row,_) -> ty_node (SA.TypeRecord row)
            | (SA.SumName n,_) -> ty_node (SA.TypeNamed n)
            | (SA.SumVar (SA.Colvar _),_) -> assert false) ts)
    )

  and typearrow x = typearrow_aux (fst x)
  and typearrow_aux (row, t) =
    let SA.TyRow (fields, rowvaro) = fst row in
    assert (rowvaro = None);
    QA.TypeArrow ((List.map (fun (_,x) -> ty x) fields), ty t)

  and typerecord row =
    let l,r=typerow row in
    QA.TypeRecord (QA.TyRow (l, r))
  and typerow (SA.TyRow (fields, rowvaro)) =
    let l = List.map (fun (s, t) -> (s, (ty t))) fields in
    let r = Option.map (function SA.Rowvar v -> Arg.rowvar v) rowvaro in
    (l, r)

  let typeident_aux = Arg.typeident
  let typeident ?(check=true)(SA.Typeident i) = typeident_aux ~check i

  let typedef ty_def =
    let vars =
      List.map
        (function SA.Flatvar var -> Arg.typevar var)
        ty_def.SurfaceAst.ty_def_params in
    let visibility' =
      (match ty_def.SurfaceAst.ty_def_visibility with
       | SA.TDV_public -> QmlAst.TDV_public
       | SA.TDV_abstract ->
           QmlAst.TDV_abstract (ObjectFiles.get_current_package_name ())
       | SA.TDV_private ->
           QmlAst.TDV_private (ObjectFiles.get_current_package_name ())) in
    let SA.Typeident ti = ty_def.SurfaceAst.ty_def_name in
    {
      QmlAst.ty_def_options = ty_def.SA.ty_def_options ;
      QmlAst.ty_def_visibility = visibility' ;
      QmlAst.ty_def_name = Arg.typeident ~check:false ti ;
      QmlAst.ty_def_params = vars ;
      QmlAst.ty_def_body = ty ty_def.SurfaceAst.ty_def_body ;
    }

  (* Note that the OPA annot [opa_annot] is only used for error messages
     purpose. *)
  let const_expr (const_node, opa_annot) =
    match const_node with
    | SA.CInt i ->
        (try QA.Int (Big_int.int_of_big_int i)
         with Failure "int_of_big_int" ->
           let context = OpaError.Context.annot opa_annot in
           OpaError.error context
             "Too big integer literal : %s@\nThe biggest int handled : %d"
             (Big_int.string_of_big_int i)
             Pervasives.max_int)
    | SA.CFloat f -> QA.Float f
    | SA.CString s -> QA.String s
    | SA.CChar i ->
        try QA.Char (Char.chr i)
        with Invalid_argument "Char.chr" ->
          let context = OpaError.Context.annot opa_annot in
          OpaError.error context "Character %d is not representable@." i

  let ident = Arg.exprident


  (* ************************************************************************ *)
  (** {b Descr}: Creates a new label, i.e. key in an annotation map + position
      in source code. The key in annotation map is a fresh one. The position
      in source code is copied from the OPA annotation received in input.
      {b Visibility}: Not exported outside this module.                       *)
  (* ************************************************************************ *)
  let make_label_from_opa_annot opa_annot =
    let annot = Annot.next () in
    let pos = opa_annot.QmlLoc.pos in
    Annot.make_label annot pos



  let lookup ~with_label ?(special = false) e x =
    if special then QA.Ident (with_label, (Ident.source x))
    else
      match OpaMapToIdent.val_opt x with
      | None ->
          OManager.error
            "Please define %s (used at %s)@\n"
            x
            (SurfaceAstHelper.Annot.to_string' e)
      | Some ident -> QA.Ident (with_label, ident)



  let rec pat (pat, e) =

    (* ********************************************************************** *)
    (** {b Descr}: Local function to process general patterns.
        {b Visibility}: Local to the surrounding function.                    *)
    (* ********************************************************************** *)
    let rec aux (x, opa_annot) =
      match x with
      | SA.PatRecord (fields, rowvar) ->
          let fields = List.map (fun (field, opa_pat) -> field, aux opa_pat) fields in
          QA.PatRecord (make_label_from_opa_annot opa_annot, fields, rowvar)
      | SA.PatAny -> QA.PatAny (make_label_from_opa_annot opa_annot)
      | SA.PatConst c ->
          QA.PatConst
            (make_label_from_opa_annot opa_annot, (const_expr (c, opa_annot)))
      | SA.PatVar i ->
          QA.PatVar (make_label_from_opa_annot opa_annot, ident i)
      | SA.PatCoerce (p, ty_) ->
          let ty_ = ty ty_ in
          let p = aux p in
          QA.PatCoerce ((make_label_from_opa_annot opa_annot), p, ty_)
      | SA.PatAs (p, i) ->
          #<If:PATTERNS_REAL_PATAS $equals "0">
            fail p (Printf.sprintf "PatAs %s" (Arg.to_string i))
          #<Else>
            let p = aux p in
            QA.PatAs (make_label_from_opa_annot opa_annot, p, ident i)
          #<End>
    in
    (* Effective body of the function [pat] dealing with a whole
       pattern-matching case, i.e. a left-side pattern and a right-side
       expression. *)
    let (e, pat) =
      #<If:PATTERNS_REAL_PATAS $equals "0">
        PatternUtils.remove_as ~and_coerces: false pat e
      #<Else>
        e, pat
      #<End>
    in
    let pat = aux pat in
    let e = expr e in
    (pat, e)


  and expr original_expr =
    (* ********************************************************************** *)
    (** {b Descr}: Local function to process a record expression. It simply
        recursively apply on each sub-expression of the record expression and
        rebuild a QML isomorphic record expression.
        {b Visibility}: Local to the surrounding function.                    *)
    (* ********************************************************************** *)
    let rec aux_record (r, opa_annot) =
      (* CHECK : the order of the pattern has no importance for qml *)
      let fields =
        List.map
          (fun (field_name, field_expr) -> (field_name, (aux field_expr)))
          r in
      QA.Record ((make_label_from_opa_annot opa_annot), fields)

    (* ********************************************************************** *)
    (** {b Descr}: Local function to process general expressions.
        {b Visibility}: Local to the surrounding function.                    *)
    (* ********************************************************************** *)
    and aux (x, opa_annot) =
      match x with
      | SA.DBPath (path, access_kind) ->
          let path = List.map (fun (elt, _) -> db_path elt) (fst path) in
          QA.Path ((make_label_from_opa_annot opa_annot), path, access_kind)
      | SA.Apply (e, r) ->
          let e = aux e in
          let args = List.map (fun (_, e') -> aux e') (fst r) in
          QA.Apply ((make_label_from_opa_annot opa_annot), e, args)
      | SA.Lambda (params, body) ->
          let (params, body) = PatternUtils.simplify_lambda (params, body) in
          let params =
            let extract_ident (s_, p) =
              match fst p with
              | SA.PatVar i -> ident i
              | SA.PatAny -> Ident.nextf "anonymous_lambda_arg_%s" s_
                  (* not equivalent but once typing is done, it doesn't matter *)
                  (*| SA.PatRecord [] -> fresh_ident ()*)
              | _ -> fail p "LambdaPattern" in
            List.map extract_ident params in
          let body = aux body in
          QA.Lambda ((make_label_from_opa_annot opa_annot), params, body)
      | SA.Const c ->
          QA.Const
            ((make_label_from_opa_annot opa_annot), const_expr (c, opa_annot))
      | SA.Ident i ->
          QA.Ident ((make_label_from_opa_annot opa_annot), (ident i))
      | SA.LetIn (rec_, iel, e) ->
          let iel = List.map (fun (i, e') -> ((ident i), (aux e'))) iel in
          let e = aux e in
          let new_label = make_label_from_opa_annot opa_annot in
          if rec_ then QA.LetRecIn (new_label, iel, e)
          else QA.LetIn (new_label, iel, e)
      | SA.Match (e, pel) ->
          let e = aux e in
          let pel = List.map pat pel in
          QA.Match ((make_label_from_opa_annot opa_annot), e, pel)
      | SA.Record r -> aux_record (r, opa_annot)
      | SA.ExtendRecord (r, e) ->
          let inner =
            let i = PatternUtils.next ~label:opa_annot "surfaceAstConverte" in
            aux
              (SA.LetIn (false, [(i,e)], (SA.Ident i, Parser_utils.nlabel e)),
               Parser_utils.nlabel e) in
          let fold acc (s, e) =
            let e = aux e in
            QA.ExtendRecord
              ((make_label_from_opa_annot opa_annot), s, e, acc) in
          List.fold_left fold inner (List.rev r)
      | SA.Dot (e, f) ->
          QA.Dot ((make_label_from_opa_annot opa_annot), (aux e), f)
      (* TODO: opalang does not depends on libbsl SA.Bypass of string *)
      | SA.Bypass bslkey ->
          QA.Bypass ((make_label_from_opa_annot opa_annot), bslkey)
      | SA.Directive d -> directive opa_annot d in

    (* Effective body of the function [expr] dealing with expressions. *)
    aux original_expr



  and expr_of_record e =
    expr (SA.Record ((Parser_utils.encode_tuple [e])), Parser_utils.nlabel e)



  and may_make_tuple1 (e: (_,_) SA.expr) = expr e



  and apply_directive ?(special = false) opa_annot name e =
    let args = may_make_tuple1 e in
    let ident =
      lookup
        ~with_label: (make_label_from_opa_annot opa_annot) ~special e name in
    QA.Apply ((make_label_from_opa_annot opa_annot), ident, [args])



  (* used for magic_* directives only *)
  and directive_variant_to_string = function
    | `magic_to_string -> Opacapi.magicToString
    | `magic_to_xml -> Opacapi.magicToXml



  and directive opa_annot ((c, e, t) as d) =
    match c, e, t with
    | (
        `typeof | `opensums | `openrecord | `unsafe_cast
      | `nonexpansive | `doctype _ | `module_ | `module_field_lifting
      | `spawn | `wait | `atomic | `callcc | `js_ident | `expand _
      | `create_lazy_record | `assert_  | `fail
      | `thread_context
      | `async
      | `throw | `catch | `tracker _
      | `with_thread_context
      | `sliced_expr
      | `may_cps
      | `specialize _
      | `deprecated
      | `todo
      | `recval
      | #SA.opavalue_directive
      | #SA.distribution_directive
      | `llarray
      ) as variant, el, tl ->
        let el =  List.map expr el in
        let tl = List.map ty tl in
        QA.Directive ((make_label_from_opa_annot opa_annot), variant, el, tl)
    (* TODO: remove Coerce from QmlAst, and use the directive instead *)
    | `coerce, [e], [t] ->
        let t = ty t in
        let e = expr e in
        QA.Coerce ((make_label_from_opa_annot opa_annot), e, t)
    | `coerce, _, _ -> assert false

    | `warncoerce, _, _ ->
        (*
          Currently, this directive is not in the syntax,
          and not any pass insert it before the pass_OpaToQml.
        *)
        assert false

    | `opacapi, args, _ -> (
        match args with
        | [e] -> expr e
        | _ ->
            (*
              The parser ensure that the directive has exactly 1 argument.
            *)
            assert false
      )

    | `magic_do, args, [] -> (
        match args with
        | [e] ->
            (*
              magic_do is traduced to warncoerce in qml.
              we do not use magic_do for funaction anymore.
            *)
            let e = expr e in
            let void = QA.TypeName ([], QA.TypeIdent.of_string Opacapi.Types.void) in
            QA.Directive
              ((make_label_from_opa_annot opa_annot), `warncoerce, [e], [void])

        | _ ->
            (*
              this directive is generated by the parser, with exactly 1 argument.
            *)
            assert false
      )

    | `fun_action, [e], [] ->
        let e = expr e in
        QA.Directive
          ((make_label_from_opa_annot opa_annot), `fun_action None, [e], [])
    (* magic directive can be converted to directive or fun call *)
    | (`magic_to_string | `magic_to_xml) as variant, [e], []
          when not keep_magic_directive ->
        apply_directive opa_annot (directive_variant_to_string variant) e

    | #SA.all_directives, e :: _, _ ->
        Format.eprintf "%a%!" Arg.pp_print_directive d;
        fail e "directive: Not implemented" (* TODO *)
    | #SA.all_directives, [], _ ->
        Format.eprintf "%a%!" Arg.pp_print_directive d;
        !!! "directive: Not implemented" (* TODO *)


  and db_path path =
    match path with
    | SA.FldKey s -> QA.FldKey s
    | SA.ExprKey e -> QA.ExprKey (expr e)
    | SA.NewKey -> QA.NewKey



  module DbConv =
  struct
    open QA
    let db_constraint const =
      match const with
      | Db.C_Ordering expr_ -> Db.C_Ordering (expr expr_)
      | Db.C_Inclusion p -> Db.C_Inclusion p
      | Db.C_Validation expr_ -> Db.C_Validation (expr expr_)
      | Db.C_Inverse p -> Db.C_Inverse p
      | Db.C_Private -> Db.C_Private

    let db_def = function
      | Db.Db_TypeDecl (path_decl, ty_) ->
          Db.Db_TypeDecl (path_decl, (ty ty_))
      | Db.Db_Alias (path_decl, path_decl2) ->
          Db.Db_Alias (path_decl, path_decl2)
      | Db.Db_Default (path_decl, expr_) ->
          Db.Db_Default (path_decl, (expr expr_))
      | Db.Db_Constraint (path_decl, db_const) ->
          let db_const = db_constraint db_const in
          Db.Db_Constraint (path_decl, db_const)
      | Db.Db_Virtual (path_decl, handlers) ->
          Db.Db_Virtual (path_decl, (expr handlers))
  end



  let code_elt (elt, sa_label) =
    let qa_label = qlabel sa_label in
    (* the scope of type variables is limited to the top-level phrase *)
    Arg.reset_var_scopes () ;
    match elt with
    | SA.Database (i, flds , db_options) ->
        ([ QA.Database
             (qa_label, ident i,
              List.map (fun s -> QA.Db.Decl_fld s) flds,
              db_options) ],
         [])
    | SA.NewDbDef dbdef ->
        let db_def = DbConv.db_def dbdef in
        ([QA.NewDbValue (qa_label, db_def)], [])
    | SA.NewType td ->
        let typedefs = List.map (fun (ty_def, _) -> typedef ty_def) td in
        ([QA.NewType (qa_label, typedefs)], [])
    | SA.NewVal (pel, is_rec) ->
        let ibel =
          List.concat_map
            (let rec aux (pat,e) =
               let label = snd pat in
               match fst pat with
               | SA.PatCoerce (p,ty) ->
                   (* this simplification on coercions is necessary because if you a : int = 42
                    * and a slicer annotation, you don't want to introduce an indirection *)
                   aux (p, C.E.coerce ~label e ty)
                     (* the boolean is true when we will keep this name in the future roots *)
               | SA.PatVar ident -> [(ident, false, e)]
               | SA.PatAny -> [(PatternUtils.next ~label "_do_", true, e)]
               | _ ->
                   let annotate = propagate_slicer_annotation e in
                   let ident = PatternUtils.next ~label "_toplevlpattern_" in
                   let ident_expr = (SA.Ident ident, Parser_utils.nlabel pat) in
                   let others =
                     List.map
                       (fun (i, e) -> (i, false, annotate e))
                       (PatternUtils.pattern_to_bindings ident_expr pat) in
                   (* we put the new ident in the roots only if there is no other names
                    * _ = (a,b) -> fresh = (a,b) a = fresh.f1 b = fresh.f2 and no new names
                    * (_,_) = (a,b) -> fresh = (a,b) and fresh is added in the roots *)
                   ((ident, (others = []), e) :: others) in
             aux)
            pel in
        let ibel = List.map (fun (i, b, e) -> ((ident i), b, (expr e))) ibel in
        let iel = List.map (fun (i, _, e) -> (i, e)) ibel in
        let il =
          List.filter_map (fun (i, b, _) -> if b then Some i else None) ibel in
        if is_rec then [QA.NewValRec (qa_label, iel)], il
        else List.map (fun bnd -> QA.NewVal (qa_label, [bnd])) iel, il
    | SA.Package _ -> assert false



  let code sa_list =
    let fold_map ils elt =
      let (c, il) = code_elt elt in
      ((il :: ils), c) in
    let (ils, code) = List.fold_left_collect fold_map [] sa_list in
    ((List.flatten ils), code)



  (* The exported function, should set option before calling code *)
  let code ?options sa_list =
    Option.iter set_options options ;
    code sa_list
end




module Nonuid : ARG with type ident = SurfaceAst.nonuid =
struct
  (** The type of the parameter 'ident of OpaAst *)
  type ident = SurfaceAst.nonuid

  let to_string s = s
  let of_string s = s
  let pp_print_directive a = OpaPrint.string#directive a

  module IdentScope = QmlTypeVarsScope.TypeVarsScope(struct type id = ident end)
  let vars = IdentScope.create 1024

  let typevar ident =
    match IdentScope.find_typevar_opt vars ident with
    | Some v -> v
    | _ ->
        let tyv = QA.TypeVar.next ~name:ident () in
        IdentScope.bind_typevar vars ident tyv;
        tyv

  let rowvar ident =
    match IdentScope.find_rowvar_opt vars ident with
    | Some v -> v
    | _ ->
        let tyv = QA.RowVar.next ~name:ident () in
        IdentScope.bind_rowvar vars ident tyv;
        tyv

  let colvar ident =
    match IdentScope.find_colvar_opt vars ident with
    | Some v -> v
    | _ ->
        let tyv = QA.ColVar.next ~name:ident () in
        IdentScope.bind_colvar vars ident tyv;
        tyv

  let add_local_scope () = IdentScope.add_local_scope vars
  let remove_local_scope () = IdentScope.remove_local_scope vars
  let get_local_vars () = IdentScope.get_local_vars vars
  let reset_var_scopes () = IdentScope.reset vars

  let typeident = QA.TypeIdent.of_string
  let exprident = Ident.source
end

module Uids : ARG with type ident = SurfaceAst.uids =
struct
  (** The type of the parameter 'ident of OpaAst *)
  type ident = SurfaceAst.uids

  let to_string = Ident.to_string
  let of_string = Ident.source
  let pp_print_directive a = OpaPrint.ident#directive a

  module IdentScope = QmlTypeVarsScope.TypeVarsScope(struct type id = ident end)
  let vars = IdentScope.create 1024

  let typevar ident =
    match IdentScope.find_typevar_opt vars ident with
    | Some v -> v
    | _ ->
        let tyv = QA.TypeVar.next ~name:(Ident.original_name ident) () in
        IdentScope.bind_typevar vars ident tyv;
        tyv

  let rowvar ident =
    match IdentScope.find_rowvar_opt vars ident with
    | Some v -> v
    | _ ->
        let tyv = QA.RowVar.next ~name:(Ident.original_name ident) () in
        IdentScope.bind_rowvar vars ident tyv;
        tyv

  let colvar ident =
    match IdentScope.find_colvar_opt vars ident with
    | Some v -> v
    | _ ->
        let tyv = QA.ColVar.next ~name:(Ident.original_name ident) () in
        IdentScope.bind_colvar vars ident tyv;
        tyv

  let add_local_scope () = IdentScope.add_local_scope vars
  let remove_local_scope () = IdentScope.remove_local_scope vars
  let get_local_vars () = IdentScope.get_local_vars vars
  let reset_var_scopes () = IdentScope.reset vars

  let typeident ?(check=true) ident =
    (* FIXME: if you have duplicate type definitions in the stdlib,
     * this is going to break *)
    match ident with
    | Ident.Source _ -> QA.TypeIdent.of_ident ident
    | Ident.FakeSource _ -> assert false (* fakesources are never used for types *)
    | Ident.Internal _ ->
        let package_name = Ident.get_package_name ident in
        if ObjectFiles.stdlib_package_names package_name then
          (* for types from the standard library we generate source type identifier
           * because the compiler inserts call to these identifiers brutally by
           * just saying "list" without mentioning the package *)
          QA.TypeIdent.of_string ~check (Ident.original_name ident)
        else
          QA.TypeIdent.of_ident ident

  let exprident ident = ident
end

module NonuidOpaToQml = MakeOpaToQml (SurfaceAstCons.StringCons) (Nonuid)
module UidsOpaToQml = MakeOpaToQml (SurfaceAstCons.ExprIdentCons) (Uids)

module Parser =
struct
  exception Exception of string

  let of_string ?filename source =
    let opa_code =
      try OpaParser.code ?filename source
      with
        (* An error message has been printed by the parseOpa function. *)
      | exn -> raise (Exception (Printexc.to_string exn)) in
    let _, qml_code =
      try NonuidOpaToQml.code ~options opa_code
      with exn -> raise (Exception (Printexc.to_string exn)) in
    qml_code
end
