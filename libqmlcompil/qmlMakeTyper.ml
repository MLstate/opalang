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
   Top level typing loop.
*)



(* depends *)
module Format = Base.Format
module List = BaseList

(* shorthands *)
module Q = QmlAst
module QT = QmlTypes

(* aliases *)
module TypeIdent = QmlAst.TypeIdent
module Db = QmlAst.Db

(* refactoring in progress *)

(*
    The signature of the Typer constructor has been review a little :

    There is no reason to switch of DbSchema constructor in the middle of a typing loop.
    Anyway, we have until now just on dbGen, and we use to call everytime type_of_expr with
    the same function in arg.

    Otherwise, the bypass_typer must be provided with a
    dynamic passing for this function, but without passing it
    everytime in everycall : so now, the function is in the env

    To get a function bypass_typer, use Libbsl, build your bypassmap, and use a partial call of the
    function ByPassMap.bypass_typer mymap ~if_not_found
*)

(** New functionnaly system for annoation
    Beware : this functionnaly annot map is automatically updated in the annot part of the env
    if you don't need everyannot, use the syntax [{ env with annot = IntMap.empty }] for
    the env passed as argument. *)

(** avoid cyclic dependencies between QmlTypes and DbGen *)
type public_env = QmlDbGen.Schema.t QT.public_env

(** The type sig is specified because it is used in qmlBlender *)
module type HIGH_LEVEL_TYPER =
sig
  (** Error Managment *)
  (** The module is a functor, so it can raises some unbound yet exception but, we keep trace of it
      in the second part of the exception constructor
      The code_elt provided is here to give an information to locate the place where the exception was raised *)
  (** To upgrade the pretty printing functionnality of exception unbound yet here,
      give a specialized Printexc parameter module *)

  (** Main types : Moved in QmlTypes *)
  type gamma = QT.gamma
  type schema = QmlDbGen.Schema.t
  type bypass_typer = QT.bypass_typer
  type env = public_env

  (** All default values are specified on the right (empty or dummy version for bypass_typer) *)
  val initial :
    bypass_typer: bypass_typer ->
    ?exception_handler:(env -> exn -> unit) -> (** fun _ e -> raise e *)
    ?display:bool ->               (** false *)
    explicit_instantiation : bool ->
    (** not an optional argument, because it has to be the same
        as the OPA compilation option of the same name, it's called
        in many places in OPA, and the default value in OPA changes
        (so we risk getting out of sync with OPA and having obscure errors
        or inefficiencies, if we have a (different) default value here, too) *)
    value_restriction : [`disabled|`normal|`strict] ->
    (** The set of toplevel identifiers that are visible outside the package.
        It will be used to raise an error if a value has a type containing a
        @private type and this value is not marked also by a @private. This
        is to avoid private types escaping from their scope. *)
    exported_values_idents: IdentSet.t ->
    unit -> env

  val map_expr : env -> Q.expr -> Q.ty
  val map_elt : env -> Q.code_elt -> QT.typed_code_elt
  val map     : env -> Q.code -> QT.typed_code_elt list

  val fold_expr : env -> Q.expr -> env
  val fold_elt : env -> Q.code_elt -> env
  val fold : env -> Q.code -> env

  val fold_map_expr : env -> Q.expr -> env * Q.ty
  val fold_map_elt : env -> Q.code_elt -> env * QT.typed_code_elt
  val fold_map : env -> Q.code -> env * QT.typed_code_elt list

  (** *)
  val type_newtype_for_separation : more_gamma:QT.gamma -> env -> Q.code_elt -> Q.typedef list -> gamma * env
end

module Make ( LLTyper  : QT.QML_LOW_LEVEL_TYPER ) : HIGH_LEVEL_TYPER =
struct
  (** Error Managment : see if we want to catch error of ByPassTyper and Schema or not *)
  (** The module is a functor, so it can raises some unbound yet exception but, we keep trace of it
      in the second part of the exception constructor *)
  (** To upgrade the pretty printing functionnality of exception unbound yet here,
      give a specialized Printexc parameter module *)

  (* the wrapper with annot checks *)
  let type_of_expr ~options ~annotmap ~bypass_typer ~gamma expr =
    (* check input annotmap *)
    assert
      (
       let check_opts =
         {QmlAnnotCheckup.default with
          QmlAnnotCheckup.freshness_only = true;
          QmlAnnotCheckup.dump_not_found = true}
       in
       QmlAnnotCheckup.expr ~options:check_opts annotmap expr
      );
    (* call the typer *)
    let (gamma, annotmap, t) =
      LLTyper.type_of_expr ~options ~annotmap ~bypass_typer ~gamma expr
    in
    (* check the result annotmap *)
    assert
      (
       let config_basic_annotations =
         (* don't be strict if the poweruser typer testing flag is set
            (e.g., for dynamic typers that override the [annot_config]) *)
         not (Base.debug_getenv_toggle "TYPER_OVERRIDE")
       in
       let check_opts =
         {QmlAnnotCheckup.default with
          (* don't spam if we don't fail, unless in debug mode below: *)
          QmlAnnotCheckup.dump_not_found = config_basic_annotations}
       in
#<<    let check_opts = if Q.QmlConsole.is_typer_debug_on () >>#;
#<<    then { check_opts with QmlAnnotCheckup.dump_not_found = true; QmlAnnotCheckup.dump_found = true; } >>#;
#<<    else check_opts in >>#;
       QmlAnnotCheckup.expr ~options:check_opts annotmap expr
       (* do not fail if the option permits missing annot contents *)
       || not config_basic_annotations
      );
    (* give unchanged result if everything OK *)
    (gamma, annotmap, t)

  type typed_code_elt = (Q.ty, QT.Scheme.t) Q.maped_code_elt
  type gamma = QT.gamma
  type schema = QmlDbGen.Schema.t
  type bypass_typer = QT.bypass_typer
  type env = public_env

  let initial
      ~bypass_typer ?(exception_handler=(fun _ e -> raise e))
      ?(display=false) ~explicit_instantiation ~value_restriction
      ~exported_values_idents () =
    {
      QmlTypes.exported_values_idents = exported_values_idents ;
      QmlTypes.gamma = QmlTypes.Env.empty ;          (* Initial gamma. *)
      QmlTypes.schema = QmlDbGen.Schema.initial ;    (* Initial DB scheme. *)
      QmlTypes.annotmap = QmlAnnotMap.empty ;        (* Initial annotmap. *)
      QmlTypes.bypass_typer = bypass_typer ;
      QmlTypes.exception_handler = exception_handler ;
      QmlTypes.had_error = false ;
      QmlTypes.display = display ;
      QmlTypes.options = {
        QmlTypes.explicit_instantiation = explicit_instantiation ;
        QmlTypes.value_restriction = value_restriction ;
      }
   }

  let exception_handler env (code_elt, (e, x)) =
    match e with
    | QmlTyperException.Exception _ ->
        env.QT.exception_handler
          env (QT.Exception (QT.TyperError (code_elt, (e, x))))
    | _ ->
        (* reraise any non-typer exceptions (assert failures, etc.) *)
        raise e


  let type_newtype_gen ~more_gamma: more_gamma env code_elt ty_defs =
    let compare_record (a, _) (b, _) = String.compare a b in
    let sort_record = List.sort compare_record in
    let sort_sum li =
      let li2 = List.map sort_record li in
      List.sort (List.make_compare compare_record) li2 in
    try
      let gamma = env.QT.gamma in
      let l =
        List.concat_map
          (fun { Q.ty_def_name = ti; Q.ty_def_params = vars;
                 Q.ty_def_body = te ; Q.ty_def_visibility = visibility } ->
             let add_ti ti visibility = [(ti, (vars, te), visibility)] in
             match te with
             | _ when TypeIdent.is_already_known ti -> add_ti ti visibility
             | _ ->
                 (* [TODO] Attention, here the body of the definition is
                    allowed to use only type constructors that are visible
                    from the currently compiled package. *)
                 add_ti (TypeIdent.new_type_ident ti) visibility)
          ty_defs in
      let tirec = List.map (fun (ti, (vars, _), _) -> (ti, vars)) l in
      let (more_gamma, gamma), l =
        List.fold_left_map
          (fun (more_gamma, gamma) (ti, (vars, te), visibility) ->
             (* /!\ may raise TypeidentNotFound *)
             let te = QT.type_of_type ~typedef: true ~tirec gamma te in
             let te = QmlAstWalk.Type.map
               (fun t ->
                  match t with
                  | Q.TypeRecord (Q.TyRow (li, c) ) ->
                      let li2 = sort_record li in
                      Q.TypeRecord (Q.TyRow(li2, c))
                  | Q.TypeSum (Q.TyCol (li, v)) ->
                      let li2 = sort_sum li in
                      Q.TypeSum (Q.TyCol (li2, v))
                  | Q.TypeSumSugar _ ->
                      assert false      (* Case solved by type_of_type. *)
                  | _ -> t)
               te in
             let def_scheme = QT.Scheme.definition ~typevar: vars ti te in
             (* Here we must the @private and @abstract directives by
                exploiting the visibility information of the type definition. *)
             let gamma =
               QT.Env.TypeIdent.add ti (def_scheme, visibility) gamma in
             let more_gamma =
               QT.Env.TypeIdent.add ti (def_scheme, visibility) more_gamma in
             if env.QT.display then (
               (* Reset the type vars to avoid variables names to be continuously
                  incremented and not restarted at "'v0" for this new type
                  definition. *)
               QmlPrint.pp#reset_typevars ;
               let ((vars, _, _), ty) = QT.Scheme.export def_scheme in
               let def_for_print = {
                 QmlAst.ty_def_options = QmlAst.ty_def_options ;
                 QmlAst.ty_def_visibility = visibility ;
                 QmlAst.ty_def_name = ti ;
                 QmlAst.ty_def_params = vars ;
                 QmlAst.ty_def_body = ty } in
               OManager.printf "%a@." QmlPrint.pp#typedef def_for_print
             );
             (more_gamma, gamma), (ti, def_scheme)) (more_gamma, gamma) l in
      (more_gamma, { env with QT.gamma = gamma }, (Q.M_NewType l))
    with e ->
      exception_handler env (code_elt, (e, [])) ;
      (more_gamma, { env with QT.had_error = true },
       Q.M_Failure (code_elt, (e, [])))



  let type_newtype_for_separation ~more_gamma env code_elt l =
    let (more_gamma, env, _) = type_newtype_gen ~more_gamma env code_elt l in
    (more_gamma, env)

  let type_newtype env code_elt l =
    let more_gamma = QT.Env.empty in
    let (_, env, stuff) = type_newtype_gen ~more_gamma env code_elt l in
    (env, stuff)

  let fold_map_elt env code_elt =
    let exception_handler_with_env = exception_handler in
    let exception_handler = exception_handler env in
    let rec get_let_component_old id t =
      match t with
      | Q.TypeRecord (Q.TyRow (lt, _)) ->
          List.assoc id lt
      | Q.TypeSum (Q.TyCol ([lt], _)) ->
          List.assoc id lt
      | t when t = Q.typeNull ->
          Q.typeNull (* for typer "off" (Qmltyper.NoTyper) *)
      | _ -> assert false
    in
    let get_let_component s t annotmap ident letrec =
      match t with
      | Q.TypeRecord (Q.TyRow (_lt, _))
      | Q.TypeSum (Q.TyCol ([_lt], _)) -> (
          match letrec with
          | Q.LetIn (_, vals, _)
          | Q.LetRecIn (_, vals, _) ->
              let e = List.assoc ident vals in
              QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap
          | _ -> assert false
        )
      | _ ->
          get_let_component_old s t
    in
    let type_bndlist ?(env=env) valrec letrec fields =
      let gamma = env.QT.gamma in
      let bypass_typer = env.QT.bypass_typer in
      (* Keep this annotmap under the hand because this is the one that has locations
         information. The one that will be returned after typechecking doesn't have
         some anymore. So, in case of error, keeping this annotmap allows us to report
         the error accurately in the source code instead of crudely citing the guilty
         part of source code. *)
      let initial_annotmap_with_locs = env.QT.annotmap in
      let options = env.QT.options in
      let exported_values_idents = env.QT.exported_values_idents in
      let typer =
        type_of_expr
          ~options ~annotmap: initial_annotmap_with_locs  ~bypass_typer ~gamma in
      let (gamma, annotmap, t) = typer letrec in
      let fold_map gamma (ident, exp) (s, _, _) =
        let ty = get_let_component s t annotmap ident letrec in
        (* Ensure that if the bound ident is exported outside the package, then
           its type doesn't make any reference to a type private to the
           package. *)
        if IdentSet.mem ident exported_values_idents then (
          try QmlTypesUtils.Inspect.check_no_private_type_escaping gamma ty with
          | QmlTypesUtils.Inspect.Escaping_private_type prv_ty ->
              let err_ctxt =
                QmlError.Context.annoted_expr initial_annotmap_with_locs exp in
              QmlError.error err_ctxt
                ("@[Definition@ of@ @{<red>%a@}@ is@ not@ private@ but@ its@ type@ "^^
                 "contains@ a@ type@ private@ to@ the@ package.@ Type@ %a@ must@ " ^^
                 "not@ escape@ its@ scope.@]@\n" ^^
                 "@[<2>@{<bright>Hint@}:@\nAdd@ a@ @@private@ directive@ on@ this@ " ^^
                 "definition.@]@\n")
                QmlPrint.pp#ident ident QmlPrint.pp#ty prv_ty
        ) ;
        (* If interface printing is requested, then print the type of the bound
           value. *)
        if env.QT.display then (
          (* Reset the type vars to avoid variables names to be continuously
             incremented and not restarted at "'v0" for this new value
             definition. *)
          QmlPrint.pp#reset_typevars ;
          OManager.printf "@[<2>val %a :@\n%a@]@."
            QmlPrint.pp#ident ident QmlPrint.pp#ty ty) ;
        (** If we have free type vars in the top level of the qml language,
            e.g. due to value restriction, then use [generalize]
            instead of [quantify] *)
        let sch =
          let is_expansive = QmlAstUtils.is_expansive_with_options options.QT.value_restriction in
          if not (QT.FreeVars.is_type_empty (QT.freevars_of_ty ty)) && is_expansive exp
          then (
            let context = QmlError.Context.annoted_expr annotmap exp in
            QmlError.serror context
              "Value restriction error@\n@[<2>This expression is not generalizable but it has type@ %a .@]"
              QmlPrint.pp_value_restriction#ty_new_scope ty
          );
          QT.Scheme.quantify ty
        in
        let gamma = QT.Env.Ident.add ident sch gamma in
        gamma, (ident, ty)
      in
      let gamma, maped =
        List.fold_left_map2 fold_map gamma valrec fields
      in
      { env with QT.gamma = gamma ; QT.annotmap = annotmap }, maped
    in
    match code_elt with
    | Q.Database (_label, ident, p, opts) ->
        env, Q.M_Database (ident, p, opts)
    | Q.NewDbValue (_label, Db.Db_TypeDecl (p, ty)) ->
        env, Q.M_NewDbValue (p, ty)
    | Q.NewDbValue (_label, Db.Db_Alias (p, p')) ->
        env, Q.M_DbAlias (p, p')
    | Q.NewDbValue (_label, Db.Db_Default (p, _dflt)) ->
        env, Q.M_DbDefault p
    | Q.NewDbValue (_, Db.Db_Constraint (p,c)) ->
        env, Q.M_DbConstraint (p,c)
    | Q.NewDbValue (_, Db.Db_Virtual (p, e)) ->
        env, Q.M_DbVirtual(p, e)
    | Q.NewType (_, l) ->
        type_newtype env code_elt l

    | Q.NewVal (_, val_list) | Q.NewValRec (_, val_list) ->
        begin
          try
            let (fields, letin, _, _) = QmlAstCons.UnValRec.make_let code_elt in
            begin
              try
                let env, maped = type_bndlist ~env val_list letin fields in
                env, Q.M_NewVal maped
              with
              | e ->
                  exception_handler_with_env env (code_elt, (e, []));
                  { env with QT.had_error = true }, Q.M_Failure (code_elt, (e, []))
                    (* this try ... with has a better env for error reporting (more positions) *)
            end
          with
          | e ->
              exception_handler (code_elt, (e, []));
              { env with QT.had_error = true }, Q.M_Failure (code_elt, (e, []))
        end

  let fold_map_expr env expr =
    let code_elt, rebuilder = QmlAstCons.UnValRec.make_code_elt_maped expr in
    let env, maped = fold_map_elt env code_elt in
    let ty = rebuilder maped in
    env, ty

  let map_expr env expr =
    let _, ty = fold_map_expr env expr in
    ty

  let map_elt env code_elt =
    let _, maped = fold_map_elt env code_elt in
    maped

  let map env code =
    let _, maped = Base.List.fold_left_map fold_map_elt env code in
    maped

  let fold_expr env expr =
    let env, _ = fold_map_expr env expr in
    env

  let fold_elt env code_elt =
    let env, _ = fold_map_elt env code_elt in
    env

  let fold = List.fold_left fold_elt

  let fold_map = Base.List.fold_left_map fold_map_elt
end
