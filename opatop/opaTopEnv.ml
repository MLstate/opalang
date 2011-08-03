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
(* CF mli *)

(* type alias *)
type filename = string
type contents = string
type item_number = int

(* depends *)
module String = Base.String

(* refactoring *)

(* shorthands *)
module Q = QmlAst
module CP = ConsoleParser
module P = OpaTopProperties

(* Selection of modules *)
module Schema = QmlDbGen.Schema
module DbGen = QmlDbGen.DbGen(QmlDbGen.DbGenByPass.BSLDbGenAlpha)
module Typer = QmlTyper.DyTyper.HighTyper

(* go *)

(* error reporting *)

exception OManagerException

let set_OManager_at_exit () =
  let at_exit =
    { OManager.at_exit =
        (fun i ->
           if P.greedy_get () then
             raise OManagerException
           else
             exit i)
    } in
  OManager.CompilerAsLib.at_exit at_exit

(* output *)
(* FIXME: clarification in OManager,
   let rename the formatter OManager.stderr and OManager.stdout,
   and use there OManager.stdout instead of P.stdout (and remove P.stdout)
*)
let std = P.stdout

(*
  Private Console Parser
*)
let t_CP = CP.create ()
let console_parser () = t_CP

(* printer *)
let pp =
  #<If:OPATOP_ANNOT>
    QmlPrint.pp_annotation
  #<Else>
    (QmlPrint.pp :> QmlPrint.base_printer_with_sugared_types)
  #<End>

(* env *)

type schema = Building of Schema.t | Finalized of Schema.t

type env = {
  (*
    Db stuff
  *)
  dbinfo : QmlDbGen.dbinfo StringListMap.t option;
  open_db : bool;
  schema : schema;

  (*
    Environments
  *)
  bypass_map : OpaTopBsl.bypass_map ;
  env_types : QmlTyper.env ;
  env_values : OpaTopEval.env ;

  (*
    Context of input
  *)
  input_filename : filename ;   (** Name of the file parsed. *)
  input_item_number : item_number ;  (** The number of toplevel item parsed.
         Attention, this is not the line number since some items may span on
         several lines. *)
}

(*
  Directive handler. In practice, updated with OpaTopDirective.handler
*)
let directive_handler =
  ref ( ( ConsoleParser.Directive.empty ()) : env ConsoleParser.Directive.handler )

let set_directive_handler handler =
  directive_handler := handler

let cache_initial = ref ( None : env option )

let ty_null = Q.TypeConst Q.TyNull
let value_null = OpaTopValue.t_null ()

let find_opt ident env =
  let ty =
    match QmlTypes.Env.Ident.find_opt ident env.env_types.QmlTypes.gamma with
    | None -> None
    | Some type_scheme ->
        let ty = QmlTypes.Scheme.instantiate type_scheme in
        Some ty
  in
  let value = IdentMap.find_opt ident env.env_values in
  match ty, value with
  | None, None -> None
  | _, _ ->
      let ty = Option.default ty_null ty in
      let value = Option.default value_null value in
      Some (ty, value)

let iter iter env =
  let iter ident value =
    let ty =
      match QmlTypes.Env.Ident.find_opt ident env.env_types.QmlTypes.gamma with
      | Some type_scheme ->
          let ty = QmlTypes.Scheme.instantiate type_scheme in
          ty
      | None ->
          ty_null
    in
    iter ident ty value
  in
  IdentMap.iter iter env.env_values

let fold fold env acc =
  let fold ident value acc =
    let ty =
      match QmlTypes.Env.Ident.find_opt ident env.env_types.QmlTypes.gamma with
      | Some type_scheme ->
          let ty = QmlTypes.Scheme.instantiate type_scheme in
          ty
      | None ->
          ty_null
    in
    fold ident ty value acc
  in
  IdentMap.fold fold env.env_values acc

let set_schema env schema =
  { env with
      schema = Finalized schema;
      open_db = true
  }

let set_filename env filename =
  { env with
      input_filename = filename
  }

let set_item_number env item_number =
  { env with
      input_item_number = item_number
  }


let schema env = match env.schema with Building s | Finalized s -> s

let types env = env.env_types
let values env = env.env_values
let bypass_map env = env.bypass_map

(* INPUT *)
(*
  Note: the code is splitten for lisibility, and avoid a too big function.
*)

(*
  New design : simple.

  We have only 2 functions, working on expressions,
  one for the typer, one for the evaluater.

  No more typer off, the typer should work, that's all.
  Whenever we need to access types or values, we can use the annotmap.
*)

let try_infer typer env arg =
  try typer env arg with
  | (QmlTyperException.Exception _ | QmlTypes.Exception _) as exn ->
      OManager.error "%a"
        (QmlTyperErrHandling.pp_report_from_typer_exception
           env.QmlTypes.annotmap) exn



let fold_type_wrap fold env arg =
  let env_types = env.env_types in
  let env_types = (try_infer fold) env_types arg in
  { env with env_types = env_types }

let fold_type_expr env expr = fold_type_wrap Typer.fold_expr env expr
let fold_type_elt env elt = fold_type_wrap Typer.fold_elt env elt

(*
  FIXME: use a functionnal annotmap for values.
  Currently, this is done with a side effect in an imperative
  table in OpaTopEval.
*)
let fold_eval_expr env expr =
  let env_values = env.env_values in
  let _ = OpaTopEval.eval env_values expr in
  env

(* after a UnValRec, the env need to be enriched *)
let fold_eval_val_list env val_list =
  if P.noeval_get() then env else
  let fold env_values (id, expr) =
    let annot = Q.QAnnot.expr expr in
    let value =
      match OpaTopEval.getValueOfAnnot annot with
      | None ->
          (* TODO: citation, with field0(expr) *)
          OManager.i_error "id,expr : (%s, %a), annotation not found"
            (Ident.to_string id) pp#expr expr
      | Some value -> value
    in
    let env_values = IdentMap.add id value env_values in
    env_values
  in
  let env_values = List.fold_left fold env.env_values val_list in
  let env =
    { env with
        env_values = env_values ;
    }
  in
  env

let input_code_elt_Database env database =
  match database with
  | Q.Database (label, ident, p, opts) -> (
      match env.schema with
      | Building schema ->
          let (schema, gamma) =
            Schema.register_db_declaration schema env.env_types.QmlTypes.gamma (label, ident, p, opts)
          in
          { env with
              schema = Building schema ;
              open_db = true ;
              env_types = { env.env_types with QmlTypes.gamma = gamma } ;
          }
      | _ ->
          OManager.error
            "DB: The schema of the database is already @{<bright>finalized@}@\nYou cannot change its file anymore"
    )
  | _ -> assert false

let input_code_elt_NewDbValue env newDbValue =
  match env.schema with
  | Building schema -> (
      let (env, dbdef, schema) =
        match newDbValue with
        | Q.NewDbValue (label, (Q.Db.Db_Default _ as dbdef)) ->
            let schema, o =
              Schema.register_new_db_value ~name_default_values:false
                schema env.env_types.QmlTypes.gamma (label, dbdef)
            in
            assert (o = None);
            (* Note: we retype all previous defaults, for each new default,
               which is unnecessarily costly, but has very simple code. *)
            let env = Schema.fold_expr fold_type_expr env schema in
            (env, dbdef, schema)
        | Q.NewDbValue (label, dbdef) ->
            let schema, o =
              Schema.register_new_db_value ~name_default_values:false
                schema env.env_types.QmlTypes.gamma (label, dbdef)
            in
            assert (o = None);
            (env, dbdef, schema)
        | _ -> assert false
      in
      let env, _ =
        Q.Db.foldmap_expr (fun env e -> let env = fold_eval_expr env e in env, e) env dbdef in
      { env with
          schema = Building schema ;
          open_db = true ;
      }
    )
  | Finalized _ ->
      OManager.error
        "DB: The schema of the database is already @{<bright>finalized@}@\nYou cannot add new db declarations anymore"

let dbgen_pass env code_elt =
  match env.dbinfo, env.schema with
  | Some dbinfo, Finalized schema -> (

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "dbgen_pass: HOOK-A-01";
        #<End>
      in

      let annotmap = env.env_types.QmlTypes.annotmap in
      let gamma = env.env_types.QmlTypes.gamma in
      let annotmap_opt, code_elt, gamma =
        DbGen.replace_path_code_elt schema dbinfo gamma ~annotmap:(Some annotmap) code_elt
      in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "dbgen_pass: HOOK-A-02";
        #<End>
      in

      let dbgen_annotmap = match annotmap_opt with Some a -> a | None -> assert false in
      let annotmap =
        QmlAnnotMap.merge annotmap dbgen_annotmap
      in
      let env_types =
        { env.env_types with QmlTypes.
            annotmap;
            gamma;
        }
      in
      let env =
        { env with
            env_types = env_types
        }
      in
      if OpaTopProperties.dddbgen_get ()
      then (
        OManager.printf "/* ================================================ */@\n";
        OManager.printf "/* dddben : db read/write resolution */@\n";
        OManager.printf "%a@\n" pp#code_elt code_elt;
        OManager.printf "/* ================================================ */@\n";
        OManager.printf "@.";
        ()
      );
      env, code_elt
    )
  | _ ->
      (* the expression does not use the db, because the db is still not finalized *)
      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "dbgen_pass: HOOK-B-01";
        #<End>
      in
      env, code_elt

(*
  TODO: merge pos for a better error report,
  by using a pos IdentMap.t instead of a IdentSet
*)
let check_no_duplicate val_list =
  let fold_check check (ident, _) =
    if IdentSet.mem ident check
    then OManager.error "The ident @{<bright>%s@} is bound several time in this @{<bright>val and@}" (Ident.to_string ident)
    else IdentSet.add ident check
  in
  let _ = List.fold_left fold_check IdentSet.empty val_list in
  ()

let input_code_elt_NewType env code_elt = fold_type_elt env code_elt

let initialize_dbgen env val_list =
  let env =
    match env.dbinfo, env.open_db with
    | None, true ->
        let use_db = List.exists (fun (_, exp) -> QmlAstWalk.UseDb.expr exp) val_list in
        if not use_db then env
        else (
          let env =
            match env.schema with
            | Finalized _ -> env
            | Building schema -> (
                OManager.verbose "I guess you've finished your db-declarations: DB schema finalisation";
                let env =
                  Schema.fold_expr fold_type_expr env schema
                in
                match (
                  Schema.finalize schema
                ) with
                | None -> OManager.error "cannot finalize the database schema, the database is not defined"
                | Some schema -> { env with schema = Finalized schema }
              )
          in
          let annotmap = env.env_types.QmlTypes.annotmap in
          let dbinfo, dbgen_gamma, dbgen_annotmap, dbgen_init_code, dbgen_accessors_code =
            DbGen.initialize ~annotmap:(Some annotmap) (schema env)
          in
          let dbgen_code = dbgen_init_code @ dbgen_accessors_code in
          let dbgen_annotmap = match dbgen_annotmap with Some a -> a | None -> assert false in
          let annotmap =
            QmlAnnotMap.merge annotmap dbgen_annotmap
          in
          let gamma = QmlTypes.Env.append env.env_types.QmlTypes.gamma dbgen_gamma in
          let env_types =
            { env.env_types with QmlTypes.
                annotmap = annotmap ;
                gamma = gamma ;
            }
          in
          let env =
            { env with
                dbinfo = Some dbinfo ;
                env_types = env_types ;
            }
          in

          if OpaTopProperties.dddbgen_get ()
          then (
            OManager.printf "/* ================================================ */@\n";
            OManager.printf "/* dddben : Initial dbgen code */@\n";
            OManager.printf "%a@\n" pp#code dbgen_code ;
            OManager.printf "/* ================================================ */@\n";
            OManager.printf "@." ;
            ()
          );

          (* no typing of dbgen init code *)
          (* no dbgen of dbgen init code *)
          let just_eval_dbgen_code_elt env code_elt =
            let _, letrec, _, _ = QmlAstCons.UnValRec.make_let code_elt in
            let env = fold_eval_expr env letrec in
            let env =
              match code_elt with
              | Q.NewVal (_, val_list) | Q.NewValRec (_, val_list) ->
                  fold_eval_val_list env val_list
              | _ -> env
            in
            env
          in
          let env = List.fold_left just_eval_dbgen_code_elt env dbgen_code in
          env
        )

    | _ ->
        (* the db was already finalized *)
        env
  in
  env

let input_code_elt_Values env code_elt =
  match code_elt with
  | Q.NewVal (_, val_list) | Q.NewValRec (_, val_list) -> (

      (* dbgen 1: generate init code if needed and not already done *)
      let env = initialize_dbgen env val_list in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-01";
        #<End>
      in

      (* syntactic check: opatop does not use name analysis for simplicity *)
      let _ = check_no_duplicate val_list in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-02";
        #<End>
      in

      if OpaTopProperties.dddbgen_get ()
      then (
        match env.schema with
        | Finalized _ ->
            OManager.printf "/* ================================================ */@\n";
            OManager.printf "/* dddben : UN-Preprocess paths code elt */@\n";
            OManager.printf "%a@\n" pp#code_elt code_elt ;
            OManager.printf "/* ================================================ */@\n";
            OManager.printf "@." ;
            ()
        | _ -> ()
      );

      (* dbgen 2: path preprocessing for helping the typer *)
      let _, code_elt =
        Schema.preprocess_paths_code_elt (schema env) code_elt
      in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-03";
        #<End>
      in

      if OpaTopProperties.dddbgen_get ()
      then (
        match env.schema with
        | Finalized _ ->
            OManager.printf "/* ================================================ */@\n";
            OManager.printf "/* dddben : Preprocess paths code elt */@\n";
            OManager.printf "%a@\n" pp#code_elt code_elt ;
            OManager.printf "/* ================================================ */@\n";
            OManager.printf "@." ;
            ()
        | _ -> ()
      );

      (* typing *)
      let env = fold_type_elt env code_elt in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-04";
        #<End>
      in

      (* dbgen 3: resolve db read/write *)
      let env, code_elt = dbgen_pass env code_elt in

      (* dddbgen is already in dbgen_pass *)

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-05";
        #<End>
      in

      (* evaluation *)
      (* eval as a let(rec)in (it is enough thanks to side-effect in Eval) *)
      (* The Letrec is val letrec = let rec a = ... and b = ... in { a = a; b = b } *)
      let _, letrec, _, _ = QmlAstCons.UnValRec.make_let code_elt in

      let _ =
        #<If:OPATOP_UNVALREC>
          OManager.printf "/* ================================================ */@\n";
          OManager.printf "UNVALREC:@\n";
          OManager.printf "%a@\n" pp#expr letrec ;
          OManager.printf "/* ================================================ */@\n";
          OManager.printf "@.";
          ()
        #<End>
      in

      let env = fold_eval_expr env letrec in

      (* extract the bindings / annotation returned after dbgen pass *)
      let val_list =
        match code_elt with
        | Q.NewVal (_, val_list) | Q.NewValRec (_, val_list) -> val_list
        | _ -> assert false
      in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-06";
        #<End>
      in

      let env = fold_eval_val_list env val_list in

      let _ =
        #<If:OPATOP_HOOK>
          prerr_endline "input_code_elt_Values: HOOK-07";
        #<End>
      in

      env
    )

  | _ -> assert false

(*
  assert: this function is called once the env is updated, and annotmap enriched.
*)
let dump_code_elt env code_elt =
  if P.dump_get () then (
    let annotmap = env.env_types.QmlTypes.annotmap in
    let dump_val ident annot =
      let ty =
        match QmlAnnotMap.find_ty_opt annot annotmap with
        | Some ty -> ty
        | None -> ty_null
      in
      let value =
        match OpaTopEval.getValueOfAnnot annot with
        | Some value -> value
        | None -> value_null
      in
      pp#reset_typevars;
      Format.fprintf !std "%s : %a = %a@."
        (Ident.to_string ident)
        pp#ty ty
        OpaTopValue.pp value
    in
    match code_elt with
    | Q.NewVal (_, val_list) | Q.NewValRec (_, val_list) -> (
        let iter (ident, expr) = dump_val ident (Q.QAnnot.expr expr) in
        List.iter iter val_list
      )
    | _ ->
        (* other cases: just an echo of the parsed element *)
        Format.fprintf !std "%a@." pp#code_elt code_elt
  )

let input_code_elt env code_elt =
  let fold =
    match code_elt with
    | Q.Database _ -> input_code_elt_Database
    | Q.NewDbValue _ -> input_code_elt_NewDbValue
    | Q.NewType _ -> input_code_elt_NewType
    | Q.NewVal _
    | Q.NewValRec _ -> input_code_elt_Values
  in
  try
    let env = fold env code_elt in
    let _ = dump_code_elt env code_elt in
    env
  with

  (* Non-standard Exceptions *)

  | Pervasives.Exit ->
      (* The server has been exited, and an error message has already been printed. *)
      env

  | Invalid_argument s ->
      OManager.printf "Invalid_argument: %S@." s;
      env

  (* Standard Errors *)
  | OManagerException ->
      (* A corresponding error message has already been printed by OManager *)
      (*
        If we end-up there, that means that we are in greedy-mode, so, simply
        ignore the error, and continue the execution with the previous env
      *)
      env



(* env -> contents -> env *)
let input_contents env str =
  let env = { env with input_item_number = succ env.input_item_number } in
  let filename =
    Printf.sprintf "%s, input #%d" env.input_filename (#<If:TESTING>0#<Else>env.input_item_number#<End>) in
  let annotmap_n_code_opt =
    try Some (OpaTopParser.parse ~filename str)
    with OManagerException -> None in
  match annotmap_n_code_opt with
  | Some code ->
      List.fold_left input_code_elt env code
  | None -> env



let input_directive env = function
  | CP.Directive dir -> (
      match CP.Directive.parse !directive_handler env dir with
      | Some env -> env
      | None -> Format.fprintf !std "[!] unknown directive %S (ignored)@." dir; env
    )
  | CP.Code contents -> input_contents env contents

let prompt () =
  if P.prompt_get () then (
    Format.fprintf !std "# ";
    Format.pp_print_flush !std ();
    ()
  )

let input_line env ic =
  let input =
    let input = String.ltrim (Pervasives.input_line ic) in
    (* bsl preprocessing *)
    if String.is_substring "##" input 0
    then (
      let input = String.rtrim input in
      let dot, input =
        if String.is_suffix ";;" input then (true, Filename.chop_suffix input ";;") else false, input in
      let line = env.input_item_number in
      let filename = env.input_filename in
      let input = BslLib.BSL.ByPassMap.Browser.preprocess_line env.bypass_map ~filename ~line input in
      (* FIXME: remove this print when everything is checked *)
      Format.fprintf !std "%s@." input;
      input^(if dot then ";;" else "")
    )
    else
      input
  in
  match CP.accumulate t_CP input with
  | None -> env
  | Some input ->
      let env = input_directive env input in
      prompt ();
      env

let input_loop env ic =
  CP.reset t_CP ;
  let _ =
    let unix_ic = Unix.descr_of_in_channel ic in
    let tty = Unix.isatty unix_ic in
    P.prompt_set tty
  in
  prompt ();
  let rec aux env =
    try
      let env = input_line env ic in
      aux env
    with
    | End_of_file -> (
        match CP.flush t_CP with
        | None -> env
        | Some input -> input_directive env input
      )
  in
  aux env

let input_file env filename =
  let env = { env with input_filename = filename } in
  OManager.verbose "load file @{<bright>%S@} ..." filename;
  let ic = open_in filename in
  let prompt = P.prompt_get () in
  P.prompt_set false;
  let env = input_loop env ic in
  P.prompt_set prompt;
  close_in ic;
  env

(* not exported *)
let restart () =
  let dbinfo = None in
  let open_db = false in
  let schema = Building Schema.initial in

  (* Feature: An error during bypass_map would be fatal *)
  let bypass_map = OpaTopBsl.bypass_map () in
  let bypass_typer = OpaTopBsl.bypass_typer bypass_map in

  let env_types =
    Typer.initial
      ~bypass_typer ~explicit_instantiation:true
      ~value_restriction: (P.value_restriction_get ())
      ~exported_values_idents: IdentSet.empty () in
  let env_values = IdentMap.empty in
  (* Reset (in fact, init to "empty") the type of exceptions. *)
  Typer_w.reset_type_exception () ;

  let input_filename = "stdin" in
  let input_item_number = 0 in

  let env = {
    dbinfo = dbinfo ;
    open_db = open_db ;
    schema = schema ;

    bypass_map = bypass_map ;
    env_types = env_types ;
    env_values = env_values ;

    input_filename = input_filename ;
    input_item_number = input_item_number ;
  }
  in
  (* From there, errors may be not fatal *)
  set_OManager_at_exit () ;
  env

let start () =
  ConsoleParser.reset t_CP ;
  match cache_initial.contents with
  | Some env -> env
  | None ->
      let env = restart () in
      cache_initial.contents <- Some env;
      env
