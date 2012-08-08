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
(*
    @author Louis Gesbert
**)
(* This file uses bindings from BSL: to make sense, it uses the files:
      libqml.git/libqmlcompil/dbGen/dbGenByPass.ml (interface & impl for BSL)
      libqml.git/libbsl/mlbsl/dbgenlink.ml
   Don't forget to also check out the backwards-compatibility hack in
      qml2llvm.git/qml/qmlCommonCompiler.ml
*)

(* shorthands *)
module Q = QmlAst

(* Bypasses from Opacapi *)
module Badoplink = Opacapi.Opabsl.Badoplink
module BadopEngine = Opacapi.Opabsl.BadopEngine
module BslNativeLib = Opacapi.Opabsl.BslNativeLib
module Transactions = Opacapi.Opabsl.Transactions
module Path = Opacapi.Opabsl.Path
module Dbgraph = Opacapi.Opabsl.Dbgraph


#<Debugvar:DBGEN_DEBUG>

let warn fmt = OManager.warning ~wclass:WarningClass.dbgen fmt
let verb fmt = OManager.verbose ("DbGenHelpers: "^^fmt)

(* DbGen uses two imperative structures defined here. One annot-table for
   typing, with only add and get operations, and a read-only reference to
   the external env used to obtain the ValInitial *)

let current_built_pos = ref (None : FilePos.pos option)

let start_built_pos p = current_built_pos := Some p

let end_built_pos () = current_built_pos := None

(* Local imperative annotmap. *)
module AnnotTable =
struct

  let dummy = ref false
  let is_open = ref false

  let annots_base = (ref QmlAnnotMap.empty: Q.annotmap ref)
  module AnTbl = QmlAnnotMap.Ref.Make (struct
                                        type ty = Q.ty
                                        type bonus = unit
                                        let _global = ref QmlAnnotMap.empty
                                      end)

  let open_table ?(annotmap=None) ?silent:(_silent=false) () =
    if !is_open then failwith "DbGen internal: re-opening annot table"
    else
      (is_open := true;
       assert (QmlAnnotMap.is_empty !annots_base);
       assert (QmlAnnotMap.is_empty (AnTbl.export ()));
       match annotmap with
         | None ->
             #<If> if not _silent then verb "Entering DbGen in typing off mode" #<End>;
             dummy := true
         | Some annotmap ->
             #<If> if not _silent then verb "Entering DbGen, annotmap size @{<bright>%d@}" (QmlAnnotMap.size annotmap) #<End>;
             annots_base := annotmap;
             dummy := false)

  let getTypeOfAnnot a =
    assert !is_open;
    if !dummy then Q.typeNull else
    match AnTbl.get_ty_opt a with
      | Some ty -> ty
      | None -> match QmlAnnotMap.find_ty_opt a !annots_base with
          | Some ty -> ty
          | None -> warn "Error: annot not found in DbGen, and typing wasn't disabled"; assert false
(*           Q.typeNull *)

  let newAnnot ty =
    assert !is_open;
    let a = Annot.next () in
    if !dummy then a else (AnTbl.set_ty a ty; a)

  let close_table ?silent:(_silent=false) () =
    assert !is_open;
    let annotmap = if !dummy then None else Some (AnTbl.export ()) in
      #<If>
        if not _silent then (match annotmap with
          | None -> verb "DbGen in typer-off mode, not returning any annotmap"
          | Some a -> verb "DbGen: closing annot table with size @{<bright>%d@}" (QmlAnnotMap.size a))
      #<End>;
      AnTbl.clear ();
      annots_base := QmlAnnotMap.empty;
      is_open := false;
      annotmap

  let emergency_close () = (* use in case of error *)
    AnTbl.clear ();
    annots_base := QmlAnnotMap.empty;
    is_open := false

end

(* ------- AST building helper functions -------- *)
let equalsty ty1 ty2 =
  ty1 = Q.typeNull || ty2 = Q.typeNull || Q.EqualsTy.equal ty1 ty2

let new_ident =
  #<If:DBGEN_FLAGS$flag "source">
    let x = ref 0 in
    fun n -> incr x; Ident.source ("__db_internal__" ^ string_of_int !x ^ "__" ^ n)
  #<Else>
    fun n -> Ident.next n (* because OCaml is stupid *)
  #<End>

let newexpr_annot_nocoerce e t =
  let annot = AnnotTable.newAnnot t in
  QmlAst.QAnnot.New.expr e annot

(* activate for full-coerce of generated code, useful to pinpoint typing errors *)
let newexpr_annot e ty =
  #<If:DBGEN_FLAGS$flag "fullcoerce">
    let label = QmlAst.Label.expr e in
    newexpr_annot_nocoerce (Q.Coerce (label, newexpr_annot_nocoerce e ty, ty)) ty
  #<Else>
    newexpr_annot_nocoerce e ty
  #<End>

let newpatt_annot p t =
  let annot = AnnotTable.newAnnot t in
  QmlAst.QAnnot.New.pat p annot

let id_expr id ty =
  let label = Annot.nolabel "dbgen" in
  newexpr_annot (Q.Ident (label, id)) ty

let (@:) = id_expr

let copy_expr e =
  if !AnnotTable.dummy then e else
    let fresh_expr_annots e =
      let annot = AnnotTable.newAnnot (AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.expr e)) in
      QmlAst.QAnnot.New.expr e annot
    in
    let fresh_pat_annots e =
      let annot = AnnotTable.newAnnot (AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.pat e)) in
      QmlAst.QAnnot.New.pat e annot
    in QmlAstWalk.ExprPatt.map_down fresh_expr_annots fresh_pat_annots e

(* Assumes the expr is alpha-converted *)
let is_id_free_in_expr id e =
  (*
    FIXME: this looks like an exists
  *)
  try QmlAstWalk.Expr.iter_up (
    function
    | Q.Ident (_, id') -> if id = id' then raise Exit
    | _ -> ()
  ) e; true
  with Exit -> false
let are_ids_free_in_expr ids e =
  (*
    FIXME: this looks like an exists
  *)
  try
    QmlAstWalk.Expr.iter_up
      (function
         | Q.Ident (_, id) -> if List.mem id ids then raise Exit
         | _ -> ())
      e ;
    true
  with Exit -> false

let tyunit = Q.TypeRecord (Q.TyRow ([], None))
let tyint = Q.TypeConst Q.TyInt
let tystring = Q.TypeConst Q.TyString
let tyfloat = Q.TypeConst Q.TyFloat
let tybool = tyint (* C-style: <> 0 is true *)

let args_ret l =
  let rec aux acc = function
    | [] -> assert false (* wrong construction of static args passed to tyfun *)
    | [ret] -> List.rev acc, ret
    | arg::tl -> aux (arg::acc) tl
  in aux [] l

let tyfun l =
  let args, ret = args_ret l in
  Q.TypeArrow (args, ret)
let tylist ty = Q.TypeName ([ty], Q.TypeIdent.of_string Opacapi.Types.list)
let tycaml_list ty = Q.TypeName ([ty], Q.TypeIdent.of_string Opacapi.Types.caml_list)
let tycont ty = Q.TypeName ([ty], Q.TypeIdent.of_string Opacapi.Types.continuation)
let tyfun_cps_firstclass l =
  match List.rev l with
    | retty::args -> Q.TypeArrow (( List.rev args @ [tycont retty] ), tyunit)
    | [] -> assert false

let typemap ty ty' =
  if ty = Q.typeNull || ty' = Q.typeNull then Q.typeNull
  else Q.TypeName ([ty;ty'], Q.TypeIdent.of_string Opacapi.Types.map)
let typeoption ty =
  if ty = Q.typeNull then Q.typeNull
  else Q.TypeName ([ty], Q.TypeIdent.of_string Opacapi.Types.option)

let typevirtualhandler realty rty wty =
  let rty = Q.TypeArrow ([realty], rty) in
  let wty = Q.TypeArrow ([wty], realty) in
  Q.TypeRecord (Q.TyRow ([("read", rty);("write", wty)], None))

let type_from_annot e = AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.expr e)
let type_from_pat_annot e = AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.pat e)

let type_inside_option e = match type_from_annot e with
  | Q.TypeName ([ty], id_option) when Q.TypeIdent.to_string id_option = "option" -> ty
  | ty when ty = Q.typeNull -> Q.typeNull
  | _ -> assert false

(* FIXME: is there a way to share with QmlAstCons ? *)

let make_lambda' par_ty_list body =
  let params, typarams = Base.List.split par_ty_list in
  let ty = Q.TypeArrow (typarams, AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.expr body)) in
  let label = QmlAst.Label.expr body in
  newexpr_annot (Q.Lambda (label, params, body)) ty

let make_lambda par_ty = make_lambda' [par_ty]

let apply_lambda' lambda params =
  let params_len = List.length params in
  match type_from_annot lambda with
  | (Q.TypeArrow (typarams, tyret)) as ty ->
      let arity = List.length typarams in
      (if arity <> params_len then
         let context = QmlError.Context.expr (QmlAstCons.UntypedExpr.apply lambda params) in
         QmlError.i_error None context (
           "DbGen: trying to apply an arrow type of arity %d on %d parameter(s)"^^
           "Type of the fct is : %a@\n"
         )
           arity params_len
           QmlPrint.pp#ty ty
      );
      (* assert (List.fold_left2 -- we need a gamma in equalsty for this *)
      (*           (fun ok param ty -> ok && *)
      (*              (equalsty (AnnotTable.getTypeOfAnnot param.Q.annot) ty || (Format.eprintf "Unification failed: [1m%a[24m vs [1m%a[24m\n" QmlPrint.pp#ty (AnnotTable.getTypeOfAnnot param.Q.annot) QmlPrint.pp#ty ty; false))) *)
      (*           true params typarams); *)
      let label = QmlAst.Label.expr lambda in
      newexpr_annot (Q.Apply (label, lambda, params)) tyret
  | ty when ty = Q.typeNull ->
      let label = QmlAst.Label.expr lambda in
      newexpr_annot (Q.Apply (label, lambda, params)) Q.typeNull
  | ty ->
      let context = QmlError.Context.expr (QmlAstCons.UntypedExpr.apply lambda params) in
      QmlError.i_error None context (
        "DbGen: trying to apply non-arrow type %a"
      )
        QmlPrint.pp#ty ty

let apply_lambda lambda e = apply_lambda' lambda [e]

let make_letand defs in_e =
  let label = QmlAst.Label.expr in_e in
  newexpr_annot (Q.LetIn (label, defs, in_e)) (AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.expr in_e))

let make_letin (id, e1) e2 =
  match e1 with
    | Q.Ident (_, id1) when id1 = id -> e2
    | Q.Ident (_, id1) when is_id_free_in_expr id1 e2 ->
        QmlAstWalk.Expr.map_up (
          function
          | Q.Ident (label, i) when i = id -> Q.Ident (label, id1)
          | e -> e
        )
          e2
          (* isn't that a bit dangerous ? *)
    | _ -> make_letand [id,e1] e2

let make_letin' id_e_lst e =
  List.fold_right (fun id_e e -> make_letin id_e e) id_e_lst e
  (* newexpr_annot (Q.LetIn (id_e_lst,e)) (AnnotTable.getTypeOfAnnot e.Q.annot) -- we want letin, not letand *)

let make_coerce ?ty e =
  let ty = match ty with
  | Some ty -> ty
  | None -> AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.expr e) in
(*     assert (ty = None || Some ty' = ty || (jlog (PrettyPrint.string_of_ty (let Some tyx = ty in tyx) ^ "~=~" ^ PrettyPrint.string_of_ty ty'); false)); -- sometimes we want to coerce to a slightly different type *)
    if ty = Q.typeNull then e
    else match e with
    | Q.Coerce (_, _e0, ty0) when ty0 = ty -> e
    | _ ->
        let label = QmlAst.Label.expr e in
        newexpr_annot_nocoerce (Q.Coerce (label, e, ty)) ty

let make_match e matchcaselist =
  let tye = type_from_annot e in
  let tylst = List.map (fun mc -> type_from_pat_annot (fst mc), type_from_annot (snd mc)) matchcaselist
  in
  let ty = snd (List.hd tylst) in
    (* assert (snd (List.fold_left (fun (ty0,s) ty ->  *)
    (*                                Base.jlog (QmlPrint.ty (snd ty0) ^ "~=~" ^ QmlPrint.ty (snd ty)); *)
    (*                                ty0,s && (equalsty (snd ty0) (snd ty))) (List.hd tylst, true) (List.tl tylst))); *)
    (* **doesn't work properly on named types** *)
  let label = QmlAst.Label.expr e in
    newexpr_annot
      (Q.Match (label, make_coerce ~ty:tye e, matchcaselist))
      ty

let tyrecord ?(extend=false) lst = Q.TypeRecord (QmlAstCons.Type.Row.make ~extend lst)
let tytuple t1 t2 = tyrecord [ "f1", t1; "f2", t2]

let make_record lst =
  let tyl = List.map (fun (fld,e) -> fld, type_from_annot e) lst in
  let label = Annot.nolabel "dbgen.make_record" in
  newexpr_annot (Q.Record (label, lst)) (tyrecord tyl)

let make_tuple e1 e2 =
  make_record ["f1",e1; "f2",e2]

let make_dot ?ty record fld =
  let ty = match ty with
    | Some ty -> ty
    | None -> match type_from_annot record with
        | Q.TypeRecord row ->
            (match QmlAstWalk.Row.get_field fld row with
               | Some ty -> ty
               | None -> assert false)
        | Q.TypeConst Q.TyNull -> Q.typeNull
        | _ -> assert false
  in
  let label = Annot.nolabel "dbgen.make_dot" in
  newexpr_annot (Q.Dot (label, record, fld)) ty

let make_fst tuple = make_dot tuple "f1"
let make_snd tuple = make_dot tuple "f2"


let over_valpath_ty ty =
  ty

let callcc e ty =
  let label = QmlAst.Label.expr e in
  newexpr_annot (Q.Directive (label, `callcc, [e], [])) ty

let make_lazyrecord param fldmap ty =
  let pos = FilePos.nopos "make_lazyrecord" in
  #<If:DBGEN_FLAGS$flag "nolazy">
    let label = Annot.next_label pos in
    make_coerce (newexpr_annot (Q.Record (label, fldmap)) ty)
  #<Else>
    let ret_ty = over_valpath_ty ty in
    newexpr_annot
      (Q.Directive
         (
           Annot.next_label pos,
           `create_lazy_record,
           [(newexpr_annot_nocoerce (Q.Record (Annot.next_label pos, fldmap)) ty);param],
           []
         )
      )
      ret_ty
  #<End>

(* useful for converting one case of a sum type to the more general sum type (eg {hd;tl} to list) *)
let expand_record ?(lazy_param=None) id tyrec tysum =
  let pos = FilePos.nopos "dbgen.expand_record" in
  match tyrec with
  | Q.TypeRecord row ->
      let fldmap =
        QmlAstWalk.Row.fold_right
          (fun f tyfld acc -> (f, newexpr_annot (Q.Dot (Annot.next_label pos, id @: tyrec, f)) tyfld) :: acc)
          row [] in
      (match lazy_param with
         | Some param -> make_lazyrecord param fldmap tysum
         | None -> newexpr_annot (Q.Record (Annot.next_label pos, fldmap)) tysum)
  | _ -> id @: tysum

(* Only needed because of gramp's backend *)
let convert_case_to_sum ?(lazy_param=None) tysum e =
  let var = new_ident "res" in
  make_letin (var, e)
    (make_coerce
       (expand_record ~lazy_param var (AnnotTable.getTypeOfAnnot (QmlAst.QAnnot.expr e)) tysum))

let freshid = let z = ref 0 in fun () -> let a = !z in incr z; "dbpathid_" ^ string_of_int a

(*
  This is the default (position + dummy annot)
  for generated ast. In case we use newexpr_annot,
  the annot is refreshed, and we can share the label
  used to initialize the construction.
*)
let nolabel = Annot.nolabel "dbgen"

let expr_unit () = newexpr_annot (Q.Record (nolabel, [])) tyunit
let patt_unit () = newpatt_annot (Q.PatRecord (nolabel, [], `closed)) tyunit

let const_int i = newexpr_annot (Q.Const (nolabel, (Q.Int (Big_int.big_int_of_int i)))) tyint
let patt_const_int i = newpatt_annot (Q.PatConst (nolabel, (Q.Int  (Big_int.big_int_of_int i)))) tyint
let const_string s = newexpr_annot (Q.Const (nolabel, (Q.String s))) tystring
let patt_const_string s = newpatt_annot (Q.PatConst (nolabel, (Q.String s))) tystring
let expr_true () = const_int 1
let expr_false () = const_int 0

let make_ifthenelse x ethen eelse = match x with
  | Q.Const (_, Q.Int i) -> if i <> Big_int.big_int_of_int 0 then ethen else eelse
  | _ -> make_match x [patt_const_int 0, eelse; newpatt_annot (Q.PatAny nolabel) tybool, ethen]

let make_list l ty =
  let make_nil () = newexpr_annot (Q.Record (nolabel, [("nil",expr_unit())])) (tylist ty) in
  let make_hdtl hd tl = newexpr_annot (Q.Record (nolabel, [("hd",hd);("tl",tl)])) (tylist ty) in
  let rec make_list l =
  match l with
  | [] -> make_nil()
  | hd::tl -> make_hdtl hd (make_list tl)
  in
  make_list l

let patt_emptyrec () =
  newpatt_annot
    (Q.PatRecord (nolabel, [], `closed))
    (Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:false []))

let patt_some p =
  newpatt_annot
    (Q.PatRecord (nolabel, ["some", p], `closed))
    (typeoption (type_from_pat_annot p))

let patt_some_var id ty =
  patt_some (newpatt_annot (Q.PatVar (nolabel, id)) ty)

let patt_none ty =
  newpatt_annot
    (Q.PatRecord (nolabel, ["none", patt_unit()], `closed))
    (typeoption ty)

let patt_none_p p =
  patt_none (type_from_annot p)

module Helpers_gen = functor ( Arg: DbGenByPass.S ) ->
struct
  let valinitial_env_ref = ref Arg.ValInitial.empty

  (* Should correspond to stg defined in initial *)
  (* These are the only code called from qml within ocaml *)
  (* let expr_none ty = make_coerce (id_expr (Arg.ValInitial.none !valinitial_env_ref) (typeoption ty)) *)
  (* let expr_none _ty = make_record ["none", expr_unit()] *)
  let expr_none ty = newexpr_annot (QmlAst.Record (nolabel, ["none", expr_unit()])) (typeoption ty)
  let id_some () = Arg.ValInitial.some !valinitial_env_ref
  let expr_some ty = id_expr (Arg.ValInitial.some !valinitial_env_ref) (tyfun [ty; typeoption ty])

  let expr_dbset_empty ty =
    id_expr (Arg.ValInitial.dbset_empty !valinitial_env_ref) ty
  let expr_intmap_empty ty =
    id_expr (Arg.ValInitial.intmap_empty !valinitial_env_ref) ty
  let expr_stringmap_empty ty =
    id_expr (Arg.ValInitial.stringmap_empty !valinitial_env_ref) ty
  let expr_intmap_add ty ty' =
    id_expr (Arg.ValInitial.intmap_add !valinitial_env_ref)
      (tyfun [ty; ty'; typemap ty ty'; typemap ty ty'])
  let expr_stringmap_add ty ty' =
    id_expr (Arg.ValInitial.stringmap_add !valinitial_env_ref)
      (tyfun [ty; ty'; typemap ty ty'; typemap ty ty'])
  let expr_intmap_fold ty ty' tyacc =
    id_expr (Arg.ValInitial.intmap_fold !valinitial_env_ref)
      (tyfun [tyfun [ty;ty';tyacc;tyacc]; typemap ty ty'; tyacc; tyacc])
  let expr_stringmap_fold ty ty' tyacc =
    id_expr (Arg.ValInitial.stringmap_fold !valinitial_env_ref)
      (tyfun [tyfun [ty;ty';tyacc;tyacc]; typemap ty ty'; tyacc; tyacc])

  let expr_make_virtual_val real_ty vread_ty =
    id_expr (Arg.ValInitial.make_virtual_val !valinitial_env_ref)
      (tyfun [DbGen_common.val_path_ty real_ty; tyfun [real_ty; vread_ty]; DbGen_common.virtual_val_path_ty vread_ty])

  let expr_make_virtual_ref real_ty vread_ty vwrite_ty =
    id_expr (Arg.ValInitial.make_virtual_ref !valinitial_env_ref)
      (tyfun [DbGen_common.ref_path_ty real_ty;
              tyfun [real_ty; vread_ty];
              tyfun [vwrite_ty; real_ty];
              DbGen_common.virtual_ref_path_ty vread_ty vwrite_ty])

  let expr_val_to_val ty =
    let tydb3path = DbGen_common.ref_path_ty ty in
    id_expr (Arg.ValInitial.val_to_val !valinitial_env_ref)
      (tyfun [tydb3path; DbGen_common.Db.val_path_ty ty])

  let expr_ref_to_ref ty =
    let tydb3path = DbGen_common.ref_path_ty ty in
    id_expr (Arg.ValInitial.ref_to_ref !valinitial_env_ref)
      (tyfun [tydb3path; DbGen_common.Db.ref_path_ty ty])

  let expr_dbset_genbuild ty =
    let iter = DbGen_common.iter ty in
    let tydb3 = DbGen_common.db3set_engine_ty ty in
    id_expr (Arg.ValInitial.dbset_genbuild !valinitial_env_ref)
      (tyfun [iter; tydb3; DbGen_common.Db.set ty])

  let expr_db3set_iterator ty =
    let tydb3 = DbGen_common.db3set_engine_ty ty in
    id_expr (Arg.ValInitial.db3set_iterator !valinitial_env_ref)
      (tyfun [tydb3; DbGen_common.iter ty])


  let expr_write ty =
    id_expr (Arg.ValInitial.write !valinitial_env_ref)
      (tyfun [DbGen_common.ref_path_ty ty; ty; tyunit])

  (* Also used above: type-identifier option and field labels "some", "none" *)

  (*let ti ?(modul="badoplink") ?(param=[]) s =
    Q.TypeName (param, Q.TypeIdent.of_string*)
  let ti ?(param=[]) s () =
    Q.TypeName (param, DbGen_common.typ s)

  (* These type idents are defined in the mlBSL. Do they need type_of_type ? *)
  (* beware, there is also the types from mlbsl/path.ml in DbGen_common (since
     they may be used by Schema) *)
  let typath = ti Opacapi.Types.badoplink_path
  let tytrans = ti Opacapi.Types.badoplink_transaction
  let tydbset ty =
    ti ~param:[ty] Opacapi.Types.db3set ()
  let tykey = ti Opacapi.Types.badoplink_db_path_key
  let typartialkey = ti Opacapi.Types.badoplink_db_partial_key
  let tydata = ti Opacapi.Types.badoplink_data_d
  let tyhltrans ty =
    ti ~param:[ty] Opacapi.Types.transactions_t ()
  let tydiff = ti Opacapi.Types.dbgraph_diff
  let ty_lazy_data = ti Opacapi.Types.path_embed_info
  let tyobj = ti Opacapi.Types.path_embedded_obj
  let tyengine = ti Opacapi.Types.badop_engine_t
  let tyoptions = ti Opacapi.Types.badop_engine_database_options
  let tynodeconfig = ti Opacapi.Types.badoplink_node_config

  module Bypass = struct
    (** All bypasses used by dbGen are linked here and defined in one of
        dbgenlink.ml, transactions.ml, path.ml. The types given here are not
        checked  directly (but indirectly with option retype_dbgen) *)

    (** returns an expr from a bypass key and its type *)
    let expr key ty =
      let dbgen_id = "dbgen" in
      let bypass = newexpr_annot_nocoerce (Q.Bypass (nolabel, key)) ty in
      newexpr_annot
        (Q.Directive(nolabel, `restricted_bypass dbgen_id, [bypass], [])) ty

    (** A non-cps bypass function with its arrow-type as list *)
    let func s tyl =
      expr s (tyfun tyl)

    (** -- Bypasses from mlbsl/dbgenlink --*)

    let jlog() = func (Badoplink.jlog) [tystring;tyunit]

    let fatal_error ty =
      func (Badoplink.fatal_error) [tystring;tystring;tystring;ty]

    let error ty =
      func (Badoplink.error) [tystring;ty]

    let db_prefix tydb = func (Badoplink.db_prefix) [tydb; tystring]
    let make_engine() = func (BadopEngine.get) [(tyoptions ()); (tyengine ())]

    let local_options() =
      func (BadopEngine.local_options) [typeoption tystring; typeoption tystring; (tyoptions ())]
    let light_options =
      #<Ifstatic:HAS_DBM 1>
        fun () -> func (BadopEngine.light_options) [typeoption tystring; typeoption tystring; (tyoptions ())]
      #<Else>
        let haswarn = ref false in
        fun () ->
          if not !haswarn then
            (warn (
               "The compiler was compiled without support for dblight, which is\n" ^^
               "set as default in your source (using \"@{<bright>database @@light@}\").\n" ^^
               "Defaulting to the normal database."
             );
             haswarn := true);
          local_options()
      #<End>
    let client_options() =
      func (BadopEngine.client_options) [typeoption tystring; typeoption tystring; typeoption tyint; (tyoptions ())]
    let check_remaining_arguments() =
      func (BadopEngine.check_remaining_arguments) [ tyunit ]

    let open_db() =
      func (Badoplink.open_db) [(tyengine ()); DbGen_common.Db.t ()]

    let node_properties() =
      func (Badoplink.node_properties) [DbGen_common.Db.t (); (tynodeconfig ()); tyunit]

    let node_config_construct() =
      func (Badoplink.node_config_construct) [tystring; (tynodeconfig ())]

    let is_db_new() =
      func (Badoplink.is_db_new) [DbGen_common.Db.t (); tyint]

    let key_int() = func (Badoplink.key_int) [tyint;(tykey ())]

    let key_string() = func (Badoplink.key_string) [tystring;(tykey ())]

    let key_value_int() = func (Badoplink.key_value_int) [(tykey ());tyint]

    let key_value_string() = func (Badoplink.key_value_string) [(tykey ());tystring]

    let make_ocaml_list l ty =
      let make_nil () = expr BslNativeLib.empty_list (tycaml_list ty) in
      let make_hdtl hd tl = apply_lambda' (expr BslNativeLib.cons (tyfun [ty;tycaml_list ty;tycaml_list ty])) [hd;tl] in
      let rec make_list l =
        match l with
        | [] -> make_nil()
        | hd::tl -> make_hdtl hd (make_list tl)
      in
      make_list l

    let key_list() = func (Badoplink.key_list) [tycaml_list (tykey ());(tykey ())]

    let empty_partial_key () =
      expr (Badoplink.empty_partial_key) (typartialkey ())

    let add_hole () = func (Badoplink.add_hole) [(typartialkey ()); (typartialkey ())]

    let add_key () = func (Badoplink.add_key) [(typartialkey ()); (tykey ()); (typartialkey ())]



    let dbpath_root() = expr (Badoplink.dbpath_root) (typath ())

    let dbpath_add() = func (Badoplink.dbpath_add) [(typath ());(tykey ());(typath ())]

    let trans_start() =
      func (Badoplink.trans_start) [DbGen_common.Db.t (); (tytrans ())]

    let trans_commit() =
      func (Badoplink.trans_commit) [(tytrans ());tyunit]

    let trans_abort() =
      func (Badoplink.trans_abort) [(tytrans ());tyunit]

    let data_int() = func (Badoplink.data_int) [tyint;(tydata ())]

    let data_text() =
      func (Badoplink.data_text) [tystring;(tydata ())]

    let data_binary() =
      func (Badoplink.data_binary) [tystring;(tydata ())]

    let data_float() =
      func (Badoplink.data_float) [tyfloat;(tydata ())]

    let data_unit() =
      func (Badoplink.data_unit) [(tydata ())]


    let proj_dbtype leaf_t =
      let proj_fun, ty = match leaf_t with
        | DbGen_common.Leaf_int -> (Badoplink.data_obj_int), tyint
        | DbGen_common.Leaf_float -> (Badoplink.data_obj_float), tyfloat
        | DbGen_common.Leaf_text -> (Badoplink.data_obj_text), tystring
        | DbGen_common.Leaf_binary -> (Badoplink.data_obj_binary), tystring
      in func proj_fun [(tydata ());ty]

    let get_opt() =
      func (Badoplink.get_opt) [(tytrans ()); (typath ()); typeoption (tydata ())]

    let get_new_key() =
      func(Badoplink.get_new_key) [(tytrans ()); (typath ()); tyint]

    let exists() =
      func (Badoplink.exists) [(tytrans ()); (typath ()); tyint]

    let uppath() =
      func (Badoplink.uppath) [(tytrans ()); (typath ()); (typath ())]

    let set() =
      func (Badoplink.set) [(tytrans ()); (typath ()); (tydata ()); (tytrans ())]

    let clear() =
      func (Badoplink.clear) [(tytrans ()); (typath ()); (tytrans ())]

    let remove_children () =
      func (Badoplink.remove_children) [(tytrans ()); (typath ()); (tytrans ())]

    let set_link() =
      func (Badoplink.set_link) [(tytrans ()); (typath ()); (typath ()); (tytrans ())]

    let set_current_copy() =
      func (Badoplink.set_current_copy) [(tytrans ()); (typath ()); (typath ()); (tytrans ())]

    let fold_children ty tyacc =
      func Badoplink.fold_children [
        (tytrans ()); (typath ());
        tyfun [(tytrans ());(typath ());ty];
        tyfun [tyacc;(tykey ());ty;tyacc];
        tyacc;
        tyacc;
      ]
(*
    let fold_int_keys ty =
      func (Badoplink.fold_int_keys)
        [(tytrans ());(typath ());tyfun [tyint;ty;ty];ty;typeoption ty]

    let fold_string_keys ty =
      func (Badoplink.fold_string_keys)
        [(tytrans ());(typath ());tyfun [tystring;ty;ty];ty;typeoption ty]
*)
    let compare ty = func Opacapi.Opabsl.BslPervasives.compare_raw [ty;ty;tyint]

    let get_registered_db_ident tydb =
      func (Badoplink.get_registered_db_ident) [tystring; tydb]

    let register_db_ident tydb =
      func (Badoplink.register_db_ident) [tystring; tydb; tyunit]

    let get_registered_root_edge() =
      func (Badoplink.get_registered_root_edge) [tystring;tystring;tyint;tyint]

    let register_root_edge() =
      func (Badoplink.register_root_edge) [tystring;tystring;tyint;tyint;tyunit]

    (** -- Bypasses from mlbsl/transactions --*)

    let highlevel_trans_start ty =
      func (Transactions.start) [ty;tyhltrans ty]

    let highlevel_trans_continue ty1 ty2 =
      func (Transactions.continue)
        [tyhltrans ty1; tyfun [ty1;ty2]; tyfun [ty1;ty2]; tyhltrans ty2]

    let highlevel_trans_commit ty =
      func (Transactions.commit) [tyhltrans ty; ty]

    let get_global_transaction_opt() =
      func (Transactions.get_global_transaction_opt) [DbGen_common.Db.t (); typeoption (tytrans ())]

    let set_global_transaction() =
      func (Transactions.set_global_transaction)
        [DbGen_common.Db.t (); (tytrans ()); tyunit]

    let fail ty = func (Transactions.fail) [tystring; ty; ty]

    (** -- Bypasses from mlbsl/path --*)

    let embedded_path () =
      func (Path.embedded_path) [(tytrans ()); (typath ()); (tyobj ())]

    let get_ref_path ty =
      func (Path.get_ref_path)
        [DbGen_common.Db.t ();
         (typath ());
         tyfun [(tytrans ()); ty];
         tyfun [(tytrans ()); ty; (tytrans ())];
         DbGen_common.ref_path_ty ty]

    let get_val_path ty =
      func (Path.get_val_path)
        [(tytrans ());
         (typath ());
         tyfun [(tytrans ()); ty];
         DbGen_common.val_path_ty ty]

    let create_dbset ty =
      func (Badoplink.create_dbset)
        [(tytrans ());
         (typath ());
         tyfun [(tytrans ()); (typath ()); ty];
         tydbset ty]

    let set_dbset_keys dbsetty =
      func (Badoplink.set_dbset_keys)
        [dbsetty;
         (typartialkey ());
         dbsetty]


    let copy ty =
      func (Path.copy)
        [(tytrans ()); DbGen_common.val_path_ty ty; (typath ()); (tytrans ())]

    let get_lazy_info_opt ty = func (Path.get_lazy_info_opt) [ty; typeoption (ty_lazy_data ())]

    let embed_record_data ty = func (Path.embed_record_data) [ty; typeoption (tyobj ()); ty]

    let inject_record_data ty = func (Path.inject_record_data) [ty; typeoption (tyobj ()); tyunit]

    (** -- Bypasses from mlbsl/dbgraph *)

    let matching_edge() = func (Dbgraph.matching_edge) [(tydiff ());tystring;tyint;tyint]

    let diff() = func (Dbgraph.diff) [tystring;tystring;(tydiff ())]

    let empty_diff() = expr (Dbgraph.empty_diff) (tydiff ())

    let diff_status() = func (Dbgraph.diff_status) [(tydiff ());tyint]

    let diff_message() = func (Dbgraph.diff_message) [tystring;(tydiff ());tystring]

    let get_diffed_schema() = func (Dbgraph.get_diffed_schema) [(tydiff ());tystring]

    let print_tree() = func (Dbgraph.print_tree) [tystring;tyunit]

    let shall_i_upgrade() = func (Badoplink.shall_i_upgrade) [DbGen_common.Db.t ();tybool]
  end
end
