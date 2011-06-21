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
   Authors
   2009, Louis Gesbert            <Louis.Gesbert@mlstate.com>
   2009, Mathieu Barbin           <Mathieu.Barbin@mlstate.com>
*)

(* depends *)
module String = BaseString
module List = BaseList

(* shorthands *)
module QT = QmlTypes
(**
    QmlBlender : DbGenInitPass + First Typing : For Compilers.

    Resolution of DbGen with a first typing, and a first optional alpha conv.
    assert : in the returned ast,
    expressions : do not contain Path or Write Nodes any more
    top ast : no more NewDbVal definitions
*)

(**
    To be able to use this with differents typers, we take a HighLevel Typer as argument
    So, it will be possible to have it with NewSubTyper, or HM(X) -- and more interresting,
    the Dytyper.

    Note for mikolaj from THE FUNCTOR MAN :
    the fact that everything is functorized here is hidden by the construction just down after
    the definition of the functor of some public instance of this :
    QML2LLVM WILL USE THESE INSTANCES AND NOT THE FUNCTORS : see module OfficialDbGenBlender
*)

(** After the blend, there are no more any Database & NewDbValue in the code *)
(** Please, keep synchrone with QmlAst *)
(** There is no other good way to do this with ocaml (problems of construtors) *)
(*
  Prototype of a possible typing guard :

  type qml_only_db =
  | Database of string list * QmlAst.Db.options list
  | NewDbValue of string list * QmlAst.ty option * QmlAst.expr option

  type qml_only_typedef =
  | NewType of QmlAst.typeident * (QmlAst.typevar list * QmlAst.ty)

  type qml_only_val =
  | NewVal of (QmlAst.ident * QmlAst.expr) list
  | NewValRec of (QmlAst.ident * QmlAst.expr) list
*)

module AstSplitter :
sig

  (** Given a set of idents, returns the minimal initial code defining them and
      the rest of the code, plus a function mapping these identifiers to their
      alpha-converted equivalent *)
  val code :
      IdentSet.t -> QmlAlphaConv.t option ->
        opa_alphaconv:(string -> string) -> QmlAst.code ->
          (Ident.t -> Ident.t) * QmlAst.code * QmlAst.code
end =
struct
  let code idset alpha_opt ~opa_alphaconv code =
    let missing_idents msg ids =
      Base.error (Printf.sprintf "%s%s\n" msg (String.concat_map ", " Ident.original_name (IdentSet.elements ids))) in
    let alpha_rev = match alpha_opt with
      | None -> (fun _ -> None)
      | Some a -> QmlAlphaConv.rev_ident a
    in
    let rec aux idmaps idset init code =
      if IdentSet.is_empty idset
      then idmaps, List.rev init, code
      else match code with
        | ((QmlAst.NewVal (_, defs)) | (QmlAst.NewValRec (_, defs)) as c)::cr ->
            if QmlAstWalk.UseDb.code_elt ~ignore_declaration:true c
            then missing_idents "These identifiers are needed by database accesses, but they seem to themselves depend on the database (maybe this is due to an invalid default DB value definition): " idset
            else
              let ids_def =
                List.filter_map
                  (fun (id,_) ->
                    (if IdentSet.mem id idset then Some (id, id)
                    else match alpha_rev id with
                    | Some orig_id ->
                        (* for [opa_alphaconv], we assume blender alpha was performed, too, so we only affect the following case: *)
                        if IdentSet.mem orig_id idset then
                          Some (orig_id, id)
                        else begin
                          try
                            let opa_id sid =
                              Ident.source (opa_alphaconv (Ident.original_name sid))
                            in
                            let filtered =
                              IdentSet.filter (fun sid -> Ident.equal (opa_id sid) orig_id) idset
                            in
                            if not (IdentSet.is_empty filtered) then
                              Some (IdentSet.choose filtered, id)
                            else
                              None
                          with
                          | Not_found -> None
                        end
                    | None -> None))
                  defs
              in
              let idmaps, idset =
                List.fold_left
                  (fun (map,set) (orig,id) -> IdentMap.add orig id map, IdentSet.remove orig set)
                  (idmaps, idset)
                  ids_def in
              aux idmaps idset (c::init) cr
        | c::cr -> aux idmaps idset (c::init) cr
        | [] -> missing_idents "Database accesses depend on the following identifiers, however, no definition for them was found in the input: " idset
    in
    let map, init, user = aux IdentMap.empty idset [] code in
    (fun id -> Option.default id (IdentMap.find_opt id map)), init, user
end

exception Exception of string

(* FIXME: use OManager, for real error, no more Exceptions *)
let error fmt =
  let k s = raise (Exception s) in
  Format.ksprintf k fmt

(** The type returned by DbGenBlender, from scratch *)
type qml_milkshake =
    {
      (** contains gamma, schema, annotmap, etc.. *)
      env : QmlTyper.env;

      (** the current alpha conv at end of the blend *)
      alphaconv : QmlAlphaConv.t option;

      (** non value definition of the program *)
      code_dbfiles : QmlAst.code;
      code_dbdefs : QmlAst.code;
      code_typedefs : QmlAst.code;

      (** code of the program *)
      code : QmlAst.code;
    }

type blender_options =
    {
      initial_env : QmlTyper.env;
      (** You can supply an initial env to build upon. Gamma, annotmap,
          bypass_typer will be taken from there (other options are
          ignored). This is only used in blender_initial. *)

      typer_off : bool; (** false by default *)
      (**
          If typer_off is set to true, the part NewVal & NewValRec of the code will not be typed
          It will just take the minimal needed by DbGen which is : type definitions, and db val definitions.
          The Schema will be built correctly -- and the dbgen pass on the code will be ok
          /!\ Remember by using this option that the returned QmlTyper.env.gamma will be quasi empty
      *)

      sort_first : bool; (** true by default *)
      (**
         Only affects blender_initial.

         If true, sorts the definitions before anything else, losing the order
         of type definitions and newvals. Then the rest is done by passes.

         If false, preprocess, alphaconvert and typing are done in a single
         fold, which is more sound w.r.t. type definitions (an expr can't be
         typed t if t hasn't been defined yet). The returned blender is still
         sorted.
      *)

      ordered_input : bool; (** true by default *)
      (**
         Only affects blender_initial.

         If false, means in particular that type definitions are unordered and a
         dependency analysis should be done in the blender. (At the moment, the
         dependency analysis is a crude one.) It's typically the case for OPA code.

         If true, then the order is somehow preserved while sorting. It's typically
         the right choice for QML.
      *)

      alphaconv_opt: QmlAlphaConv.t option; (** default is YES, a new alpha conv (Some AlphaConv.next ()) *)
      (**
         Use (Some custom) if you have already done an alpha conv before the blender.
         with a normal use, this option is not used, because the blender does
         the first alpha-conv. (Some new-alpha by default)
         Use also None if you dont want any alpha conv during the blend pass.

         /!\ Beware : the alpha conv during the blender assure the fact that dbGen will
         be insensible to any redefinition of an initial value in the user code,
         e.g val some = 5, val the_database = "buggy !"
         So if you dont use an alpha conv, you are no more protected to this kind of problemes.

         This is only used in blender_initial.
      *)

      display_schema: string option; (** Output schema in a file & Display it on screen *)
      opa_alphaconv : string -> string (* remove when opa and blender alpha-convertsions are merged *)
    }

module MakeDbGenBlender (BSLDbGen : DbGenByPass.S with type ValInitial.env =  DbGenByPass.I_Fun.env) ( HighTyper : QmlMakeTyper.HIGH_LEVEL_TYPER ) :
sig
  (** Managment of errors :
      The only possible exceptions raised comes from : LowTyper / HighTyper / QmlDbGen / AlphaConv
      So, it is only public exceptions defined upper *)

  val default_options : ?value_restriction:[ `disabled | `normal | `strict ] -> unit -> blender_options

  (* An auxiliary function used in blend_initial: only sorts and processes type definitions *)
  (* val blend_initial_part1 : options:blender_options -> QmlAst.code -> qml_milkshake *)

  (* An other auxiliary function used in blend_initial: does the rest ! *)
  (* val blend_initial_part2 : options:blender_options -> qml_milkshake -> qml_milkshake *)

  (* Initial treatments: Alphaconv, preprocess & typing of code (calls blend_initial_part.* ) *)
  val blend_initial : options:blender_options -> QmlAst.code -> qml_milkshake

  (* Final treatments: database access code generation *)
  val blend_final : options:blender_options -> qml_milkshake -> qml_milkshake

  (* Initial + final *)
  val full_blend : options:blender_options -> QmlAst.code -> qml_milkshake

end =
struct


  module DbGen = QmlDbGen.DbGen ( BSLDbGen )

  let do_verbose =
    let c = Chrono.make () in
    fun ?(time=true) msg ->
      let timing = c.Chrono.stop(); c.Chrono.read() in
      #<If:TESTING> () #<Else>
        if time && timing >= 0.5 then OManager.verbose "[%02.2fs]" timing
      #<End>;
      OManager.verbose "%s" msg;
      c.Chrono.reset();
      c.Chrono.start()

  let default_options ?(value_restriction=`disabled) () =
    let alpha = QmlAlphaConv.next () in
    (** now the safety of ident is done in Ident.next () *)
    {
     initial_env =
     HighTyper.initial
       () ~explicit_instantiation:true ~value_restriction
       ~exported_values_idents:IdentSet.empty ;
      typer_off = false;
      sort_first = true;
      ordered_input = true;
      alphaconv_opt = Some alpha;
      display_schema = None;
      opa_alphaconv = fun s -> s
    }

  (**
      Please : if you hack this code, follow the strict guideline fixed here :
      override the scope of any variable when it is updated
  *)

  let prepare_initial_env options =
    let env = options.initial_env in
    let env =
      HighTyper.initial
        ~bypass_typer:env.QT.bypass_typer ~gamma:env.QT.gamma
        ~annotmap:env.QT.annotmap
        ~explicit_instantiation:env.QT.options.QT.explicit_instantiation
        ~value_restriction:env.QT.options.QT.value_restriction
        ~display:env.QT.display
        ~exception_handler:env.QT.exception_handler
        ~handle_exception:env.QT.handle_exception ~fatal_mode:true
        ~unique_types:env.QT.unique_types
        ~multiargument_arrow:env.QT.options.QT.multiargument_arrow
        ~exported_values_idents: env.QT.exported_values_idents () in
    env

  (** Folds on code *and* expressions hidden in the env (DB defaults) *)
  let extended_fold code_folder expr_folder env code =
    QmlDbGen.Schema.fold_expr expr_folder (code_folder env code) (env.QT.schema)

  let extended_foldmap code_folder expr_folder schema acc code =
    let acc, code = code_folder acc code in
    let acc, schema = QmlDbGen.Schema.foldmap_expr expr_folder acc schema in
    schema, acc, code


  (* ------------------------------------------------------------ *)
  (* Initial Blender Part 1 (just to register type definitions)   *)
  (* ------------------------------------------------------------ *)
  let blend_initial_part1 ~options code =
    let verbose ?(time=true) s = if OManager.is_verbose () then do_verbose ~time s else () in

    let env = prepare_initial_env options in

    (* 1°: sorting things out *)
    let code_defs, code_dbfiles, code_dbdefs, code =
      verbose "I-1) Sorting top-level nodes";
      let sort_user = QmlAstSort.add QmlAstSort.empty code in
      let code_defs = QmlAstSort.Get.new_type sort_user
      and code_dbfiles = QmlAstSort.Get.database sort_user
      and code_dbdefs = QmlAstSort.Get.new_db_value sort_user
      and user_code = QmlAstSort.Get.new_val sort_user
      in code_defs, code_dbfiles, code_dbdefs, user_code
    in

    (* pre-2: dependency analysis on type definitions *)
    QT.check_no_duplicate_type_defs code_defs;
    let code_defs =
      if options.ordered_input then code_defs
      else QT.dependency code_defs
    in

    (* 2°: getting type definitions into Gamma *)
    let env =
      verbose "I-2) registering type definitions";
      HighTyper.fold env code_defs
        (* at this point, local type definitions are in the global
           environnement, but abstract, and when the typer encounters the
           corresponding LetTypeIn, it will have the effect of an "open" *)
    in
    { env = env; alphaconv = None; code_typedefs = code_defs; code_dbfiles = code_dbfiles; code_dbdefs = code_dbdefs; code = code }

  (* ------------------------------------------------------------ *)
  (* Initial Blender Part 2 (DB, type all code)                   *)
  (* ------------------------------------------------------------ *)

  (* Does the first part of the job: alphaconv, preprocess & type *)
  let blend_initial_part2 ~options milkshake =
    let verbose ?(time=true) s = if OManager.is_verbose () then do_verbose ~time s else () in

    let { env = env;(* alphaconv = alphaconv;*) code_typedefs = code_defs
        ; code_dbfiles = code_dbfiles; code_dbdefs = code_dbdefs; code = code }
        = milkshake
    in

    (* 3°: building the DB schema from the definitions *)
    let env, code_dbfiles, code_dbdefs =
      verbose "I-3) registering database definitions";
      let schema = env.QT.schema in
      let (schema, gamma) =
        List.fold_left
          (fun (schema, gamma) c ->
             match c with
             | QmlAst.Database (label, ident, p, opts) ->
                 QmlDbGen.Schema.register_db_declaration
                   schema gamma (label, ident, p, opts)
             | _ -> assert false)
          (schema, env.QT.gamma)
          code_dbfiles
      in
      let env = { env with QT.gamma = gamma } in
      let schema =
        List.fold_left
          (fun s c ->
             match c with
             | QmlAst.NewDbValue (label, value) ->
                 let s, o = QmlDbGen.Schema.register_new_db_value ~name_default_values:false
                   s env.QT.gamma (label, value) in
                 assert (o = None);
                 s
             | _ -> assert false)
          schema
          code_dbdefs
      in
      match QmlDbGen.Schema.finalize schema with
      | Some s -> { env with QT.schema = s }, code_dbfiles, code_dbdefs
      | None -> env, code_dbfiles, code_dbdefs
    in

    (* hack for S2 only, S3 does no longer use this part of the blender *)
    let _ = OManager.flush_errors () in

    (* 3b°: displaying the schema if asked *)
    let _ =
      let disp_schema filename =
        try
          let oc = open_out filename in
          verbose (Printf.sprintf "I-3b) schema is finalized -- production of \"%s\"..." filename);
          QmlDbGen.Schema.to_dot env.QT.schema oc;
          close_out oc;
          let _ =
            let id = Unix.fork() in
            if id = 0 then Unix.execvp "sh" [|"sh";"-c";Printf.sprintf "dot -Tpng %s | display" filename|]
            else at_exit (fun () -> ignore(Unix.waitpid [] id)) in
          ()
        with
          | e ->
              OManager.warning ~wclass:WarningClass.dbgen_schema
                "an error occured while trying to display the db schema@\n%s"
                (Printexc.to_string e)
      in
      match options.display_schema with
        | None -> ()
            (* #<If:DEBUG_DB> *)
            (*   if not (QmlDbGen.Schema.is_empty env.schema) then disp_schema "/tmp/opa-dump-schema" *)
            (* #<End> *)
        | Some filename -> disp_schema filename
    in

    (* 4°: code preprocessing (for DB paths) *)
    let code =
      verbose "I-4) database access preprocessing";
       let _, code =
         QmlDbGen.Schema.preprocess_paths_ast env.QT.schema code in
       code
    in

    (* 5°: alpha-conversion *)
    let alpha_opt, env, code =
      match options.alphaconv_opt with
      | None ->
          verbose "I-5) *skip* (alpha-conversion disabled)";
          None, env, code
      | Some alpha ->
          (* First pass just to gather db identifiers to use as OPA ids *)
          let _, alpha, _ =
            extended_foldmap QmlAlphaConv.code
              (fun alpha e -> alpha, e)
              env.QT.schema alpha code_dbfiles
          in
          verbose "I-5) alpha-converting the code";
          let schema, alpha, code =
            extended_foldmap QmlAlphaConv.code
              (fun alpha e -> alpha, QmlAlphaConv.expr alpha e)
              env.QT.schema alpha code in
          Some alpha, { env with QT.schema = schema}, code
    in

    (* 6°: typing *)
    let env =
      if options.typer_off then
        (verbose "I-6) *skip* (typing disabled)";
         env)
      else
        (verbose "I-6) typing";
         extended_fold HighTyper.fold HighTyper.fold_expr env code)
    in

    (* 6bis: typing has been done, we can set the unsafe option of the typer to consider local types as concrete *)
    let env = { env with QT.options = { env.QT.options with QT.concrete_abstract = true } } in

    { env; alphaconv = alpha_opt; code_typedefs = code_defs ; code_dbfiles; code_dbdefs; code }

  (* ------------------------------------------------------------ *)
  (* Initial Blender                                              *)
  (* ------------------------------------------------------------ *)

  (* Does the first part of the job: alphaconv, preprocess & type *)
  let blend_initial ~options code =
    let verbose ?(time=true) s = if OManager.is_verbose () then do_verbose ~time s else () in

    verbose ~time:false "-- Initial Blender starting --";

    let milkshake =

      if options.sort_first then

        (* 1-2°: sorting, and getting type definitions into gamma *)
        let milkshake = blend_initial_part1 ~options code in

        (* 4-7°: DB, alpha, typing *)
        let milkshake = blend_initial_part2 ~options milkshake in

        milkshake

      else (* not sort_first *)

        let env = prepare_initial_env options in

        (* 1°: one-fold everything *)
        let (env, alpha_opt), code =
          verbose "I-1) One-fold blending: registering types, db-definitions, pre-processing, alpha-converting, typing";
          List.fold_left_map
            (fun (env, alpha_opt) elt -> match elt with
               | QmlAst.Database (label, ident, p, opts) ->
                   (* FIXME: 'database' has to come first *)
                   let (schema, gamma) = QmlDbGen.Schema.register_db_declaration env.QT.schema env.QT.gamma (label, ident, p, opts) in
                   ({ env with QT.schema = schema; QT.gamma = gamma },
                    alpha_opt), (* FIXME: call alphaconv, type and mono on the schema *)
                   elt
               | QmlAst.NewDbValue (label, value) ->
                   let schema, o = QmlDbGen.Schema.register_new_db_value ~name_default_values:false env.QT.schema env.QT.gamma (label, value) in
                   assert (o = None);
                   ({ env with QT.schema = schema },
                    alpha_opt), (* FIXME: call alphaconv, type and mono on the schema *)
                   elt
               | QmlAst.NewType _ ->
                   (HighTyper.fold env [elt], alpha_opt), elt
               | QmlAst.NewVal _ | QmlAst.NewValRec _ ->
                   let elt =
                     let _, elt = QmlDbGen.Schema.preprocess_paths_code_elt env.QT.schema elt in
                     elt
                   in
                   let alpha_opt, elt = match options.alphaconv_opt with
                     | None -> None, elt
                     | Some alpha -> let alpha, elt = QmlAlphaConv.code_elt alpha elt in Some alpha, elt
                   in
                   let env = if options.typer_off then env else HighTyper.fold env [elt]
                   in (* FIXME: mono is now done here in the "sort_first" version *)
                   (env, alpha_opt), elt)
            (env, options.alphaconv_opt)
            code
        in

        (* 2°: finalising schema *)
        let env =
          verbose "I-2) Finalising database schema";
          match QmlDbGen.Schema.finalize env.QT.schema with
            | Some s -> { env with QT.schema = s }
            | None -> env
        in

        (* 3°: sorting things out *)
        let code_defs, code_dbfiles, code_dbdefs, code =
          verbose "I-3) Sorting top-level nodes";
          let sort_user = QmlAstSort.add QmlAstSort.empty code in
          let code_defs = QmlAstSort.Get.new_type sort_user
          and code_dbfiles = QmlAstSort.Get.database sort_user
          and code_dbdefs = QmlAstSort.Get.new_db_value sort_user
          and user_code = QmlAstSort.Get.new_val sort_user
          in code_defs, code_dbfiles, code_dbdefs, user_code
        in

        { env = env; alphaconv = alpha_opt; code_typedefs = code_defs ; code_dbfiles = code_dbfiles; code_dbdefs = code_dbdefs ; code = code }
    in
    verbose "-- Initial Blender ending --";

    milkshake


  (* ------------------------------------------------------------ *)
  (* Final Blender                                                *)
  (* ------------------------------------------------------------ *)
  let blend_final ~options milkshake =
    let verbose ?(time=true) s = if OManager.is_verbose () then do_verbose ~time s else () in

    verbose ~time:false "-- Final Blender starting --";
      (* check alpha conv conflict !!! *)
      assert( options.alphaconv_opt = None ||
          milkshake.alphaconv = None ||
          options.alphaconv_opt==milkshake.alphaconv);
    let { env = env; alphaconv = alpha_opt; code = code } = milkshake in
    let alpha_opt = match alpha_opt with
      |None -> options.alphaconv_opt
      | _ -> alpha_opt
    in
    let nodb = QmlDbGen.Schema.is_empty env.QT.schema in

    let env, code =
      if nodb then
        (verbose "III-1..3) No DB used: skipping database access handling";
         env, code)
      else begin
        let annotmap = env.QT.annotmap in

        (* 1°: split the code to insert DbGen at the right place *)
        let dbgen_idmap, init_code, user_code =
          (verbose "III-1) splitting code";
           let idents_in_schema =
             QmlDbGen.Schema.fold_expr
               (fun acc e ->
                 IdentSet.union acc (QmlAstUtils.FreeVars.expr e))
               IdentSet.empty env.QT.schema
           in
           let static_idents =
             IdentSet.from_list
               (BSLDbGen.ValInitial.list_of_idents BSLDbGen.ValInitial.empty)
           in
           AstSplitter.code (IdentSet.union static_idents idents_in_schema) alpha_opt ~opa_alphaconv:options.opa_alphaconv code)
        in

        (* 2°: generate database accessors from the schema *)
        let dbinfo, db_gamma, annotmap, dbgen_code =
          verbose "III-2) generation of database accessors";
          let dbinfo, db_gamma, annotmap_opt, dbgen_init_code, dbgen_accessors_code =
            DbGen.initialize
              ~annotmap:(Some annotmap)
              ~valinitial_env:dbgen_idmap
              env.QT.schema
          in
          let dbgen_code = dbgen_init_code @ dbgen_accessors_code in
          let annotmap =
            Option.default_map annotmap (fun am -> QmlAnnotMap.merge (QT.process_annotmap ~gamma:env.QT.gamma am) annotmap) annotmap_opt in
          dbinfo, db_gamma, annotmap, dbgen_code
        in

        (* 3°: expanding database accesses *)
        let annotmap, user_code, gamma =
          verbose "III-3) rewriting of database access";
          let annotmap_opt, user_code, gamma =
            DbGen.replace_path_ast
              env.QT.schema dbinfo env.QT.gamma ~annotmap:(Some annotmap) ~valinitial_env:dbgen_idmap
              user_code in
          let annotmap =
            Option.default_map annotmap (fun am -> QmlAnnotMap.merge (QT.process_annotmap ~gamma:env.QT.gamma am) annotmap) annotmap_opt in
          annotmap, user_code, gamma
        in

        (* 4°: post-processing dbGen type information *)
        let gamma =
          verbose "III-4a) post-processing database accessors type information";
          let db_gamma = QT.process_gamma ~gamma db_gamma in
          verbose "III-4b) updating gamma";
          let gamma = QT.Env.append gamma db_gamma in
          gamma
        in

        { env with QT.annotmap = annotmap; gamma = gamma },
        init_code @ dbgen_code @ user_code
      end
    in

    (* 4°: retyping for debug *)
    let env =
      if options.typer_off || not (Base.debug_getenv_toggle "BLENDER_RETYPE_DBGEN") then env else
        (verbose "III-5) re-typing for validation";
         (* let env = prepare_env_for_monorph env in *)
         HighTyper.fold env code)
    in

    verbose "-- Final Blender ending --";

    { milkshake with env = env; alphaconv = alpha_opt; code = code }

  let full_blend ~options code =
    let milkshake = blend_initial ~options code in
    let options = {options with
      alphaconv_opt = milkshake.alphaconv;
    }
    in
    blend_final ~options milkshake
end

module OfficialDbGenBlender = MakeDbGenBlender ( DbGenByPass.BSLDbGenFun ) ( QmlTyper.OfficialTyper )
module DynamicallyChangeableTyperDbGenBlender = MakeDbGenBlender ( DbGenByPass.BSLDbGenFun ) ( QmlTyper.DynamicallyChangeableTyper.HighTyper )
module DyDbGenBlender = DynamicallyChangeableTyperDbGenBlender

module OfficialOpaDbGenBlender = MakeDbGenBlender ( DbGenByPass.BSLDbGenFunOpa ) ( QmlTyper.OfficialTyper )

(**
   Examples :

   ...
   ...
   you got your bypass_typer
   ...
   you got the init_code
   ...
   you got the user_code
   ...

   let qml_milkshake = OfficialDbGenBlender

*)

(** Sugar of interface, for a class of rewriters *)
module Sugar :
sig
  (** Tranform the code according to the function passed as argument
      This is used by qlm2ocaml by the different passes *)
  val process_code :
    process_code:(QT.gamma ->
                    QmlAst.annotmap ->
                      QmlAst.code ->
                        (QT.gamma * QmlAst.annotmap) * QmlAst.code) ->
    qml_milkshake ->
    qml_milkshake
end =
struct
  let process_code ~process_code milk =
    let gamma = milk.env.QT.gamma in
    let annotmap = milk.env.QT.annotmap in
    let code = milk.code in
    let (gamma, annotmap), code = process_code gamma annotmap code in
    { milk with
        env = { milk.env with
                  gamma = gamma ;
                  QT.annotmap = annotmap ;
              } ;
        code = code
    }
end
