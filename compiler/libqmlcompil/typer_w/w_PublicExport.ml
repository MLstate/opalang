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
   @author François Pessaux
*)



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_PublicExport.mli].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
type ill_formed_column_type_reason =
  | IFCTR_row_and_column_vars
  | IFCTR_sum_case_with_row_variable



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_PublicExport.mli].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Ill_formed_column_type of
  (W_Algebra.simple_type * ill_formed_column_type_reason)



exception Cyclic_type of W_Algebra.simple_type

let cyclic_type = ref QmlTypes.Env.empty

(* ************************************************************************** *)
(** {b Descr}: Function prepare multiples exporting
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let prepare_export () = cyclic_type := QmlTypes.Env.empty

(* ************************************************************************** *)
(** {b Descr}: Function finalize a sequence of exporting and returns a gamma
    which contains definition for the cyclic types.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let finalize_export() =
  let gamma = !cyclic_type in
  cyclic_type := QmlTypes.Env.empty;
  gamma

(* ************************************************************************** *)
 (** {b Descr}: Function exporting a [simple_type] to a QML type. This function
     is the only one exported outside this module. It especially starts
     exportation with empty variables mappings, which is important to prevent
     having in memory variables from another type.
     @raise Ill_formed_column_type
     @raise Cyclic_type
     {b Visibility}: Exported outside this module.                            *)
(* ************************************************************************** *)
let simple_type_to_qml_type initial_ty =

  (* ************************************************************************ *)
  (** {B Descr}: Local function exporting a [simple_type] to a QML type. This
      function is used to recurse in the type term structure.                 *)
  (* ************************************************************************ *)
  let rec rec_export_simple_type ty =
    let ty = W_CoreTypes.simple_type_repr ty in
    match ty.W_Algebra.sty_mark with
    | W_Algebra.TM_export_seen _ ->
        (* Since this marker means that the type was previously marked with
           [TM_export_seen_not_rec], then this means we saw it again in a
           sub-term of itself. This will mean that the type is in fact
           recursive. Since QML type algebra can't express recursive types,
           this raises an error.
           We must cleanup the type in order to be able to print it in an error
           message. If we don't clean it, we will get assertion failures because
           the exportation set its own markers. *)
        let ident = Ident.next (Printf.sprintf "c%d" (Fresh.Int.get ())) in
        ty.W_Algebra.sty_mark <- W_Algebra.TM_export_cyclic ident;
        QmlAst.TypeName ([], ident)
    | W_Algebra.TM_export_cyclic ident ->
        if QmlTypes.Env.TypeIdent.mem ident !cyclic_type then(
          let sch = QmlTypes.Env.TypeIdent.find ~visibility_applies:false
            ident !cyclic_type in
          let (_, ty, _) = QmlGenericScheme.export_unsafe (fst sch) in
          ty
        )else (
          QmlAst.TypeName ([], ident)
        )
    | W_Algebra.TM_not_seen
    | W_Algebra.TM_export_seen_not_rec -> (
        (* The type was never seen of just seen but as non-recursive. We will
           tell that temporarily, if it seen without telling if it is
           recursive. *)
        ty.W_Algebra.sty_mark <- W_Algebra.TM_export_seen (ref 1) ;
        (* We now descend on it structure. If at the return of the sub-term it
           appears that the type is marked [TM_export_seen] more than 1,
           then this means that this type appears in its sub-terms, and hence
           is recursive. *)
        let exported_ty =
          (match ty.W_Algebra.sty_desc with
           | W_Algebra.SType_var var -> QmlAst.TypeVar var.W_Algebra.tv_qml
           | W_Algebra.SType_arrow (args_tys, res_ty) ->
               (* Nothing very sexy to say... Simple structural descent. *)
               let qml_args_tys = List.map rec_export_simple_type args_tys in
               let qml_res_ty = rec_export_simple_type res_ty in
               QmlAst.TypeArrow (qml_args_tys, qml_res_ty)
           | W_Algebra.SType_named { W_Algebra.nst_name = name ;
                                     W_Algebra.nst_args = args } -> (
               (* Since QML treats int, float, string and char as special
                  cases, we must look for these type constructors names as
                  special cases.
                  Silently ignore the arguments since there should not be
                  some for these basic types. *)
               match QmlAst.TypeIdent.to_string name with
               | "int" -> QmlAst.TypeConst QmlAst.TyInt
               | "float" -> QmlAst.TypeConst QmlAst.TyFloat
               | "string" -> QmlAst.TypeConst QmlAst.TyString
               | _ ->
                   (* The general and default case. Attention, ni QML we
                      never export the unwounded representation of the type,
                      rather its "visible" identity. This contributes to
                      preserve type constraints annotations ! *)
                   let qml_args = List.map rec_export_simple_type args in
                   QmlAst.TypeName (qml_args, name)
             )
           | W_Algebra.SType_sum_of_records sumcases_column ->
               (* QML make a distinction between record types and sum types. A
                  record type is a sum type with only 1 row. The typechecker
                  algebra doesn't make this distinction internally. So we will
                  make the distinction during the export of a column type. *)
               rec_export_column_type sumcases_column
           | W_Algebra.SType_forall scheme ->
               (* QML type forall expects the 3 kinds of variables as 3 lists.
                  So build these 3 lists from the list of generalized
                  variables of the scheme. *)
               let qml_ty_vars =
                 List.map
                   (fun ty_being_var ->
                      let ty_being_var =
                        W_CoreTypes.simple_type_repr ty_being_var in
                      match ty_being_var.W_Algebra.sty_desc with
                      | W_Algebra.SType_var var -> var.W_Algebra.tv_qml
                      | _ -> assert false)
                   scheme.W_Algebra.ty_parameters in
               let qml_row_vars =
                 List.map
                   (fun var -> var.W_Algebra.rv_public_identity)
                   scheme.W_Algebra.row_parameters in
               let qml_column_vars =
                 List.map
                   (fun var -> var.W_Algebra.cv_public_identity)
                   scheme.W_Algebra.column_parameters in
               (* Now, export the body of the scheme. *)
               let qml_body = rec_export_simple_type scheme.W_Algebra.body in
               QmlAst.TypeForall
                 (qml_ty_vars, qml_row_vars, qml_column_vars, qml_body))
        in
        (* Now we check if the current type is recursive, i.e if it
           appeared in the subtree below it. If yes, then since QML type
           algebra can't express recursive types, we raise an error.
           If no, then we turn back its marker as "seen but non
           recursive" (i.e. [TM_export_seen_not_rec]). *)
        match ty.W_Algebra.sty_mark with
         | W_Algebra.TM_export_cyclic ident ->
             if not (QmlTypes.Env.TypeIdent.mem ident !cyclic_type) then (
               let free = QmlTypes.freevars_of_ty exported_ty in
               let typevar = QmlTypeVars.TypeVarSet.elements free.QmlTypeVars.typevar in
               let exported_ty =
                 QmlAstWalk.Type.map
                   (function
                    | QmlAst.TypeName ([], i) when QmlAst.TypeIdent.equal ident i ->
                        QmlAst.TypeName (List.map (fun v -> QmlAst.TypeVar v) typevar, i)
                    | x -> x) exported_ty
               in
               let sch = QmlTypes.Scheme.definition ~typevar ident exported_ty in
               cyclic_type := QmlTypes.Env.TypeIdent.add ident (sch, 0, QmlAst.TDV_public) !cyclic_type;
               exported_ty
             ) else (
               exported_ty
             )
         | W_Algebra.TM_export_seen xtimes' ->
             if !xtimes' = 1 then
               ty.W_Algebra.sty_mark <- W_Algebra.TM_export_seen_not_rec
             else
               (* Should have broken above when encountering
                  [TM_export_seen]. *)
               assert false;
             exported_ty
         | _ ->
             (* At least, the type must be marker as [TM_export_seen] with
                1, since this is at what we initialized its marker just before
                starting descending in its sub-term. *)
             assert false;
      )
    | _ (* Other markers. *) -> assert false


  (* ************************************************************************ *)
  (** {b Descr}: Local function exporting a [column_type] to a QML type. This
      function introduces the distinction made by QML that considers that there
      exist 2 kinds of types: records (having 1 row) and columns (having
      several rows). Hence, this function returns a QML type begin either a
      [QmlAst.TypeRecord] or a [QmlAst.TypeSum].                              *)
  (* ************************************************************************ *)
  and rec_export_column_type column =
    let column = W_CoreTypes.column_type_repr column in
    let (col_rows, col_ending) = column.W_Algebra.ct_value in
    (* Here we make the distinction made by QML between record types that have
       only 1 row and **no column variable** and sum types that have several
       rows, each of them having **no row variable**. *)
    match (col_rows, col_ending) with
    | ([just_one_row], W_Algebra.Closed_column) ->
        (* Ok, just 1 row, hence leads to a record type. *)
        let (qml_row_fields, qml_row_ending) =
          rec_export_row_type just_one_row in
        QmlAst.TypeRecord (QmlAst.TyRow (qml_row_fields, qml_row_ending))
    | _ -> (
        (* Ok, several rows and/or column variable, hence leads to a sum type.
           Attention, to be well-formed and keep the existence of a principal
           type, even if the column is closed, since there are several rows,
           rows forming a sum type must not end by a row variable.
           Attention again, if the column has only one case, then if it has a
           column variable then the row representing it case must not contain
           a row variable otherwise the type has both row and column variables.
           We recover only the "fields" part of each exported row and check by
           the way the well-formation of the row.
           We have the same remark about well-formed columns than we had above
           with well-formed rows. So we first manage the "seen/not seen"
           stuff. *)
        let qml_column_ending =
          (match col_ending with
           | W_Algebra.Var_column var -> Some var.W_Algebra.cv_public_identity
           | W_Algebra.Closed_column -> None) in
        (* Ok, if we get there, that's we didn't already see the column, or it
           was closed to we couldn't notice if we already saw it. Anyway, we
           must convert the fields of the row. If some cycles appear later,
           they will be revealed later.
           So, export the rows forming the cases of the sum. *)
        let qml_rows_fields_only =
          List.map
            (fun row ->
               let (qml_row_fields, qml_row_ending) =
                 rec_export_row_type row in
               if qml_row_ending <> None then (
                 (* If the sum has a column variable then the error reports
                    a type with both row and column variables, otherwise a sum
                    type with row variable(s). *)
                 let err_kind =
                   (if qml_column_ending = None then
                      IFCTR_sum_case_with_row_variable
                    else IFCTR_row_and_column_vars) in
                 (* We must cleanup the type in order to be able to print it in
                    an error message. If we don't clean it, we will get
                    assertion failures because the exportation set its own
                    markers. *)
                 W_CoreTypes.cleanup_simple_type initial_ty ;
                 raise (Ill_formed_column_type (initial_ty, err_kind))
               ) ;
               (* Ok, no row variable is ending the row, hence this row is a
                  well-formed candidate to be part of a sum type. *)
               qml_row_fields)
            col_rows in
        (* Finally, build our resulting QML sum type. *)
        QmlAst.TypeSum (QmlAst.TyCol (qml_rows_fields_only, qml_column_ending))
      )



  (* ************************************************************************ *)
  (** {b Descr}: Local function exporting a [row_type] to a QML type. This
      function doesn't directly plug the exported row into a [QmlAst.TyRow]
      because it can also be used to export the rows of a sum type. And in
      this last case, we don't need the [QmlAst.TyRow] at all.
      Exportation of the row returns hence separately the fields part and the
      ending part. When used to create a record type, we will manually group
      these 2 parts in a [QmlAst.TyRow]. When used to create a sum type, we will
      ensure that the ending part is not a row variable.                      *)
  (* ************************************************************************ *)
  and rec_export_row_type row =
    let row = W_CoreTypes.row_type_repr row in
    let (row_fields, row_ending) = row.W_Algebra.rt_value in
    (* Since row are well-sorted, a row variable is always preceded by the
       same set of elements. Hence, if we already saw it ending variable, we
       have also already seen its preceding elements and there is no need to
       explore the whole row again, this means that we are in the case of a
       cyclic row.
       So, we start by inspecting if we already saw the possible ending row
       variable, and by the way, we synthesize the corresponding QML row
       ending. *)
    let qml_row_ending =
      (match row_ending with
       | W_Algebra.Var_row var -> Some var.W_Algebra.rv_public_identity
       | W_Algebra.Closed_row -> None) in
    (* Ok, if we get there, that's we didn't already see the row, or it was
       closed to we couldn't notice if we already saw it. Anyway, we must
       convert the fields of the row. If some cycles appear later, they will be
       revealed later.
       So, export the fields of the row. *)
    let qml_row_fields =
      List.map
        (fun (field_name, field_ty) ->
           (field_name, (rec_export_simple_type field_ty)))
        row_fields in
    (* Finally, return the couple with the fields and ending parts. *)
    (qml_row_fields, qml_row_ending) in



  (* ************************************************************************ *)
  (** {b Descr}: Effective body of the function [simple_type_to_qml_type] that
        transforms a [simple_type] into a QML type. This function is the only
        entry point available to types exportations.
      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  try
    let exported_ty = rec_export_simple_type initial_ty in
    (* Don't forget to cleanup the type structure. *)
    begin match initial_ty.W_Algebra.sty_mark with
    | W_Algebra.TM_export_cyclic _ident -> () (* We won't cleanup cyclic mark*)
    | _ -> W_CoreTypes.cleanup_simple_type initial_ty
    end;
    exported_ty
  with killed_by ->
    (* In any case, even if an error occurred, don't forget to cleanuo the type
       otherwise printing routines will encounter unexpected remaining
       markers. *)
    W_CoreTypes.cleanup_simple_type initial_ty ;
    raise killed_by



(* ************************************************************************** *)
(* types_scheme -> QmlTypeVars.FreeVars.t                                     *)
(** {b Descr}: Build a [QmlTypeVars.FreeVars.t] representing the generalized
    variables of the type scheme from a [types_scheme]. This function is just
    an helper used to factorize code by [type_scheme_to_qml_type_scheme] and
    [type_scheme_to_annotmap_type_scheme] and not intended to exported.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let scheme_quantification_to_qml_quantification sch =
  (* First, add type variables to the initial empty quantification. *)
  let quantification =
    List.fold_left
      (fun accu ty_being_var ->
         (* Type variables in schemes are encoded as [simple_type]s, but by
            construction invariant, they must have a [SType_var] structure
            otherwise there is something broken somewhere. *)
         let qml_ty_var =
           (match
              (W_CoreTypes.simple_type_repr ty_being_var).W_Algebra.sty_desc
            with
            | W_Algebra.SType_var var -> var.W_Algebra.tv_qml
            | _ -> assert false) in
         QmlTypeVars.FreeVars.add_ty qml_ty_var accu)
      QmlTypeVars.FreeVars.empty sch.W_Algebra.ty_parameters in
  (* Now, add row variables in the previously built quantification. *)
  let quantification' =
     List.fold_left
      (fun accu row_var ->
         QmlTypeVars.FreeVars.add_row row_var.W_Algebra.rv_public_identity accu)
       quantification sch.W_Algebra.row_parameters in
  (* Finally, add column variables in the previously built quantification. *)
  List.fold_left
    (fun accu col_var ->
       QmlTypeVars.FreeVars.add_col col_var.W_Algebra.cv_public_identity accu)
    quantification' sch.W_Algebra.column_parameters



(* ************************************************************************** *)
(* types_scheme -> (simple_type, unit) QmlGenericScheme.tsc                   *)
(** {b Descr}: Converts a [types_scheme] into a data structure representing it
    suitable for an annotation map. In effect, annotation maps are polymorphic
    on the "type of types" embedded inside, but not on the "schemes". Hence,
    inside the annotation map, schemes quantification is always represented by
    lists of QML-AST-like variables. Since our internal schemes represent
    quantification by list of [type_variable]s, [row_variable]s and
    [column_variable]s, we can't directly embed them in the scheme
    representation of annotation maps.
    So, this function creates the quantification of the scheme as QML variables
    list before embedding them and the [simple_type] representing the scheme's
    body inside a [QmlGenericScheme.tsc] suitable for insertion in an
    annotation map.
    @raise Ill_formed_column_type
    @raise Cyclic_type
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let type_scheme_to_annotmap_type_scheme sch =
  (* Build the quantification as QML variables from the variables marked as
     generalized in the body of the scheme. *)
  let qml_quantification = scheme_quantification_to_qml_quantification sch in
  (* Now, simply embed the scheme's body and the quantification. Sharing (not
     physical) is not broken since QML variables recorded in the quantification
     really correspond to public identities if the [type_variable]s present in
     the body of the scheme. *)
  QmlGenericScheme.import qml_quantification sch.W_Algebra.body ()
