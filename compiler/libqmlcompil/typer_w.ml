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
   @author Fran�ois Pessaux
*)



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_TypingEnv.mli]. We export this function
    to make it visible from [pass_Typing.ml].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_toplevel_tydefs_schemes_env_memo () =
  W_TypingEnv.reset_toplevel_tydefs_schemes_env_memo ()



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_TypingEnv.mli]. We export this function
    to make it visible from [pass_Typing.ml].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_toplevel_valdefs_schemes_env_memo () =
  W_TypingEnv.reset_toplevel_valdefs_schemes_env_memo ()



let convert_to_qml ~expr ~inferred_ty ~inference_result_annotmap =
  #<If:TYPER $minlevel 9>
  OManager.printf "Starting convert_to_qml@." ;
  #<End> ;
  try
    (* Convert the type obtained after inference. *)
    let qml_ty = W_PublicExport.simple_type_to_qml_type inferred_ty in
    #<If:TYPER $minlevel 9>
    OManager.printf "Converted QML type: %a@." QmlPrint.pp#ty qml_ty ;
    OManager.printf "Ready to convert annotmap.@." ;
    QmlAnnotMap.iteri
      ~f_for_key: (fun k -> OManager.printf "KEY: %s@." (Annot.to_string k))
      ~f_for_ty:
        (fun opt_ty ->
          OManager.printf "TYPE: " ;
          match opt_ty with
          | None -> OManager.printf "--@."
          | Some t -> OManager.printf "%a@." W_PrintTypes.pp_simple_type t)
      ~f_for_tsc:
        (fun opt_tsc ->
          OManager.printf "TSC: " ;
          match opt_tsc with
          | None -> OManager.printf "--@."
          | Some s ->
              let (_, body, _) = QmlGenericScheme.export_unsafe s in
              OManager.printf "%a@." W_PrintTypes.pp_simple_type body)
      ~f_for_tsc_inst:
        (fun opt_tsc ->
          OManager.printf "TSC_INST: " ;
          match opt_tsc with
          | None -> OManager.printf "--@."
          | Some s ->
              let (_, body, _) = QmlGenericScheme.export_unsafe s in
              OManager.printf "%a@." W_PrintTypes.pp_simple_type body)
      inference_result_annotmap ;
    #<End> ;
    (* Now, convert the annotation map obtained after inference into a QML
       annotation map. *)
    let qml_annotmap =
      W_AnnotMap.annotmap_to_qml_annotmap inference_result_annotmap in
    (qml_ty, qml_annotmap)
  with
  | W_PublicExport.Cyclic_type _
  | W_PublicExport.Ill_formed_column_type _ ->
      W_ReportErrors.report_cyclic_or_ill_formed_type_in_expr
        inference_result_annotmap expr



(* ************************************************************************** *)
(** {b Descr}: See documentation in [w_Exceptions.mli]. We export this function
    to make it visible from [pass_Typing.ml].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let reset_type_exception () =
  W_Exceptions.reset_type_exception ()



(* ************************************************************************** *)
(** {b Descr}: Function allowing to populate the type of exceptions from a list
    of (assumed to sums) types obtained from modules the current one depends
    on. Each module manipulated exceptions, defined some, and these exceptions
    definitions must be known to ensure that the current compilation unit uses
    them with a consistent type, a consistent structure. We especially want to
    avoid using an exception parameter with a type different from the type it
    had in the exception definition.
    ATTENTION: This function assumes that [Typer_w.reset_type_exception] was
    previously called.
    This function is especially made visible for [pass_Typing.ml].
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let init_type_exception gamma tys =
  let env = W_TypingEnv.from_qml_typing_env gamma in
  W_TypingEnv.new_empty_variables_mapping () ;
  List.iter
    (fun qml_ty ->
       let ty =
         W_TypingEnv.qml_type_to_simple_type
           env ~is_type_annotation: false qml_ty in
       W_Exceptions.enrich_type_exception env ty)
    tys ;
  W_TypingEnv.release_variables_mapping ()



(* ************************************************************************** *)
(** {b Descr}: Function allowing to retrieve the type of exceptions as a QML
    (assumed to be a sum) type. This function is made visible for
    [pass_Typing.ml] in order to allow this latter so save the structure of the
    type of exceptions we inferred during the typechecking of the current
    compilation unit. Hence, all modules depending on the present compilation
    unit will be able, to know our exceptions definitions.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let get_type_exception_description () =
  let ty = W_Exceptions.type_exception () in
  W_PublicExport.simple_type_to_qml_type ty



(* ************************************************************************** *)
(** {b Descr}: The main function of the typechecker that performs the type
    inference of an expression.                                               *)
(* ************************************************************************** *)
let type_of_expr ?options:_ ?annotmap ~bypass_typer ~gamma expr =
  #<If:TYPER $minlevel 10> (* <---------- DEBUG *)
  OManager.printf "@[TYPING:@ %a@]@." QmlPrint.pp#expr expr ;
  #<End> ; (* <---------- END DEBUG *)
  (* Gory detail to workaround the fact that while typechecking we don't have
     the global annotation map with positions of the items of source code. And
     we need this information to reveal precise location of typing error: to
     report errors, use the annotmap we got as input and not the one we
     obtained after type inference since the first one is the one that contains
     source locations. Moreover, in case the inference failed, anyway we may
     not yet have the annotations map of the type inference since this latter
     failed !
     If we were provided no annotations map, then use an empty one.
     So we make this "global" annotation map available here by side effect. *)
  W_ReportErrors.set_annotmap_for_error_report annotmap ;
  (* Create the typing environment under a form suitable for the type
     inference from the QML environment we were provided. *)
  let typing_env = W_TypingEnv.from_qml_typing_env gamma in
  try
    (* Now, really infer the type fo the expression. *)
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "typer_w: infer start@." ;
    #<End> ; (* <---------- END DEBUG *)
    W_TypingEnv.new_empty_variables_mapping () ;
    let (ty, infered_annotmap) =
      W_Infer.infer_expr_type ~bypass_typer typing_env expr in
    W_TypingEnv.release_variables_mapping () ;
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "typer_w: infer end with type %a@."
      W_PrintTypes.pp_simple_type ty ;
    #<End> ; (* <---------- END DEBUG *)
    (* Now, convert the obtained type and the annotations map obtained after
       inference into a QML type and annotations map. *)
    W_PublicExport.prepare_export ();
    let (qml_ty, qml_infered_annotmap) =
      convert_to_qml
        ~expr ~inferred_ty: ty ~inference_result_annotmap: infered_annotmap in
    let qml_more_gamma = W_PublicExport.finalize_export () in
    #<If:TYPER $minlevel 9> (* <---------- DEBUG *)
    OManager.printf "typer_w: annotmap export stop@." ;
    #<End> ; (* <---------- END DEBUG *)
    (* Merge the initially provided and inferred annotations maps, preferring to
       keep the types of the second and the positions of the first.*)
    let result_qml_annotmap =
      (match annotmap with
       | Some ann -> QmlAnnotMap.overwrite ann qml_infered_annotmap
       | None ->
           (* No initially provided annotation map, hence the result is simply
              the one we obtained after the inference. *)
           qml_infered_annotmap) in
    #<If:TYPER $minlevel 12> (* <---------- DEBUG *)
    QmlPrint.debug_QmlAst_annotmap qml_infered_annotmap ;
    #<End> ; (* <---------- END DEBUG *)
    ((gamma, qml_more_gamma), result_qml_annotmap, qml_ty)
  with killed_by ->
    (
      (* First, to allows opatop to continue after the error was reported, we
         must reset the current binding level, in case where the exception who
         killed us prevented the correct matching of [begin_definition] and
         [end_definition] calls. This will prevent next phrases to be (wrongly)
         typechecked starting with a binding level greater that 0. *)
      W_CoreTypes.reset_toplevel_binding_level_on_error () ;
      (* Same thing for the current variables mapping. *)
      W_TypingEnv.reset_empty_variables_mapping_on_error () ;
      (* Now, have a look at who killed us to issue an error message. *)
      match killed_by with
      | W_Unify.Unification_simple_type_conflict (ty1, ty2, detail) ->
          (* Case of error during unification but with no more precise error
             diagnosis. *)
          (* Recover by side effect the annotation map that really contains
             source locations. *)
          let annotmap_for_err_report =
            W_ReportErrors.get_annotmap_for_error_report () in
          let err_ctxt =
            QmlError.Context.annoted_expr annotmap_for_err_report expr in
          W_PrintTypes.pp_simple_type_prepare_sequence [ty1; ty2];
          QmlError.error err_ctxt
            "@[Types@ @{<red>%a@}@ and@ @{<red>%a@}@ are@ not@ compatible@]%a"
            W_PrintTypes.pp_simple_type_start_sequence ty1
            W_PrintTypes.pp_simple_type_end_sequence ty2
            W_ReportErrors.pp_unification_conflict_detail detail
      | W_InferErrors.Infer_detailled_unification_conflict
            (context, ty1, ty2, detail) ->
          (* Case of error during unification with a precise error context (i.e.
             expression of pattern, in other words language construct)
             description. *)
          W_ReportErrors.report_unification_conflict_with_context
            typing_env (context, ty1, ty2, detail)
      | W_InferErrors.Infer_private_type_not_opaque (name, body, whole_ty,
                                                     prv_ty) ->
          let annotmap_for_err_report =
            W_ReportErrors.get_annotmap_for_error_report () in
          let err_ctxt =
            QmlError.Context.annoted_expr annotmap_for_err_report body in
          QmlError.error err_ctxt
            ("@[Definition@ @{<red>%s@}@ of@ type@ @{<red>%a@}@ could@ " ^^
             "not@ be@ generalized@ because@ it@ contains@ a@ private@ " ^^
             "type@ @{<red>%a@}@ not@ turned@ opaque@]@\n" ^^
             "@[<2>@{<bright>Hint@}:@\n" ^^
             "@[Definition@ probably@ contains@ a@ @{<bright>@@wrap@}@ " ^^
             "directive@ not@ coerced@ into@ a@ named@ @{<bright>private@}@ " ^^
             "type.@ Consider@ adding@ a@ type@ annotation.@]@]")
            (Ident.to_string name)
            W_PrintTypes.pp_simple_type_start_sequence whole_ty
            W_PrintTypes.pp_simple_type_end_sequence prv_ty
      | W_InferErrors.Infer_bypass_type_not_found (bp_key, bp_expr) ->
          let annotmap_for_err_report =
            W_ReportErrors.get_annotmap_for_error_report () in
          let err_ctxt =
            QmlError.Context.annoted_expr annotmap_for_err_report bp_expr in
          QmlError.error err_ctxt
            "Unable@ to@ type@ bypass@  @{<red>%s@}.@\n"
            (BslKey.to_string bp_key)
      | W_SchemeGenAndInst.Non_generalizable_type_var (global_ty, var_ty)
      | W_SchemeGenAndInst.Non_generalizable_row_var (global_ty, var_ty)
      | W_SchemeGenAndInst.Non_generalizable_column_var (global_ty, var_ty) -> (
          (* Let's make only one case for these 3 similar exceptions and make
             the error message slightly differ according to the kind of faulty
             variable. *)
          let var_kind_str =
            (match killed_by with
            | W_SchemeGenAndInst.Non_generalizable_type_var _ -> "type"
            | W_SchemeGenAndInst.Non_generalizable_row_var _ -> "row"
            | W_SchemeGenAndInst.Non_generalizable_column_var _ -> "column"
            | _ -> assert false) in
          let annotmap_for_err_report =
            W_ReportErrors.get_annotmap_for_error_report () in
          let err_ctxt =
            QmlError.Context.annoted_expr annotmap_for_err_report expr in
          QmlError.error err_ctxt
            ("@[In@ type@ @{<red>%a@},@ %s@ variable@ @{<red>%a@}@ cannot@ " ^^
             "be@ generalized.@]@.")
            W_PrintTypes.pp_simple_type_start_sequence global_ty
            var_kind_str
            W_PrintTypes.pp_simple_type_end_sequence var_ty
         )
      | other_exn -> raise other_exn
    )

let type_of_type ~gamma ty =
  W_TypingEnv.new_variables_mapping (W_TypingEnv.get_saved_mapping ()) ;
  let env = W_TypingEnv.from_qml_typing_env gamma in
  let wty = W_TypingEnv.qml_type_to_simple_type env ty
    ~is_type_annotation:false in
  W_TypingEnv.release_variables_mapping ();
  W_PublicExport.simple_type_to_qml_type wty


(* ************************************************************************** *)
(** {b Descr}: The main module to provide as argument to [QmlMakeTyper] in
    order to get a high-level typechecker.                                    *)
(* ************************************************************************** *)
module Main : QmlTypes.QML_LOW_LEVEL_TYPER =
struct
  let type_of_expr =
    (type_of_expr :
       ?options: QmlTypes.options ->
         ?annotmap: QmlAst.annotmap ->
           bypass_typer: QmlTypes.bypass_typer ->
             gamma: QmlTypes.gamma ->
               QmlAst.expr -> ((QmlTypes.gamma * QmlTypes.gamma) * QmlAst.annotmap * QmlAst.ty))

  let type_of_type = type_of_type
end

let clean_info _ = W_TypeInfo.clean_type_info ()

let clean_mapped_variables = W_TypingEnv.clean_saved_mapping
