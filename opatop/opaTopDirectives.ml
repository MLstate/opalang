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

(* refactoring *)

(* shorthands *)
module D = ConsoleParser.Directive
module P = OpaTopProperties
module Format = Base.Format

type env = OpaTopEnv.env
type directive_action = env D.action
type directive = env D.directive

let std = P.stdout

(* directives *)
(* guidelines : alphabetic order, except for help which is at end *)

let make_manager set name =
  let func env args =
    let _ =
      match args with
      | [] -> set true
      | [ "on" ] -> set true
      | [ "off" ] -> set false
      | _ -> Format.fprintf !std "use : #%s on / off@." name
    in
    env
  in
  func

(* a *)

let assert_action = make_manager P.assert_set "assert"
let assert_ = "#assert +\\(.+\\)$", 1, assert_action

(* b *)

let bypass_action env _ =
  (* OpaTop.dump_bymap env ; *)
  env

let bypass = "#bypass$", 0, bypass_action

(* d *)
(*
let dbgen_action = make_manager P.dbgen_set "dbgen"
let dbgen = "#dbgen +\\(.+\\)$", 1, dbgen_action
*)

let dddbgen_action = make_manager P.dddbgen_set "dddbgen"
let dddbgen = "#dddbgen +\\(.+\\)$", 1, dddbgen_action

(* e *)

let env_action env _ =
  let iter ident ty value =
    Format.fprintf !std "%s : %a = %a@\n"
      (Ident.to_string ident) QmlPrint.pp#ty ty OpaTopValue.pp value
  in
  Format.fprintf !std "Dump environment...@\n";
  OpaTopEnv.iter iter env;
  Format.pp_print_flush !std ();
  env

let env = "#env$", 0, env_action

let envgrep_action env = function
  | [ str ] ->
      let reg = Str.regexp (".*"^str) in
      let iter ident ty value =
        let out = Format.sprintf "%s : %a = %a"
          (Ident.to_string ident) QmlPrint.pp#ty ty OpaTopValue.pp value
        in
        if Str.string_match reg out 0 then
          Format.fprintf !std "%s@\n" out
        else ()
      in
      Format.fprintf !std "Dump environment...@\n";
      OpaTopEnv.iter iter env;
      Format.pp_print_flush !std ();
      env

  | _ -> assert false

let envgrep = "#envgrep +\\(.*\\)$", 1, envgrep_action

(*
let eval_action = make_manager P.eval_set "eval"
let eval = "#eval +\\(.+\\)$", 1, eval_action
*)

(* i *)

let import_db_action env = function
  | [ file ] -> (
      Format.fprintf !std "loading the schema for database %S ...@." file;
      (* Fixme: add warnings if there are already DB definitions / DB is already open *)
      try
        failwith "import-schema temporarily disabled"
        (* let schema = QmlDbGen.DbImport.get_schema Official_database.do_simpleread_on_db file in *)
        (* OpaTopEnv.set_schema env schema *)
      with
      | e ->
          Format.fprintf !std "@[<2>cannot import database :@\n%s@]@." (Printexc.to_string e);
          env
    )
  | _ -> assert false

let import_db = "#import-db +\"\\(.+\\)\"$", 1, import_db_action

(* l *)

let load_action env = function
  | [ file ] ->
      OpaTopEnv.input_file env file

  | _ -> assert false (* regexp error *)

let load = "#load +\"\\(.+\\)\"$", 1, load_action

let lookup_action env = function
  | [ var ] -> (
      let var =
        (* we don't lookup internals *)
        Ident.source var
      in
      match OpaTopEnv.find_opt var env with
      | Some (ty, value) ->
          Format.fprintf !std "%s : %a = %a@."
            (Ident.original_name var) QmlPrint.pp#ty ty OpaTopValue.pp value ;
          env
      | None ->
          Format.fprintf !std "var %s is not in environment@." (Ident.original_name var);
          env
    )
  | _ -> assert false (* regexp error *)

let lookup = "#lookup +\\(.+\\)$", 1, lookup_action

(* n *)

let noeval_action = make_manager P.noeval_set "noeval"
let noeval = "#noeval +\\(.+\\)$", 1, noeval_action

(* q *)

let quit_action _ _ =
  Format.fprintf !std "quit@.";
  exit 0

let quit = "#quit$", 0, quit_action

(* r *)

let reset_action _ _ =
  Format.fprintf !std "Environment is reset@.";
  OpaTopEnv.start ()

let reset = "#reset", 0, reset_action

let restricted_bypass_action = make_manager P.restricted_bypass_set "restricted-bypass"
let restricted_bypass = "#restricted-bypass +\\(.+\\)$", 1, restricted_bypass_action

(* s *)

let schema_action env = function
  | [ file ] -> (
      Format.fprintf !std "export db-schema in file %S ...@." file;
      let schema = OpaTopEnv.schema env in
      try
        let oc = open_out file in
        QmlDbGen.Schema.to_dot schema oc;
        close_out oc;
        let _ = Sys.command (Printf.sprintf "dot -Tpng %s | display &" file) in
        env
      with
      | e ->
          Format.fprintf !std "@[<2>cannot export schema :@\n%s@]@." (Printexc.to_string e);
          env
    )
  | _ -> assert false (* regexp error *)

let schema = "#schema +\"\\(.+\\)\"$", 1, schema_action



(** {b Descr}: Stuff to enable switching between available typecheckers when
    working in an OPA toplevel. Handles the directive forcing to use the
    typechecker whose name is given as a string in the directive. *)
let set_typer_action env = function
  | [ typer ] ->
      (* If not typechecker is found for the given name, emit an error
         message instead of remaining silent. *)
      if not (OpaTopProperties.switch_typechecker typer) then
        Format.fprintf !std "No available typechecker named \"%s\".@\n" typer ;
      env
  | _ -> assert false (* Regexp error *)

let set_typer = "#set-typer +\\(.+\\)$", 1, set_typer_action



(* t *)

(*
let typer_action = make_manager P.typer_set "typer"
let typer = "#typer +\\(.+\\)$", 1, typer_action
*)

(* FIXME : add extern types *)
let types_action env _ =
  let iter _ (typescheme, _, visibility) =
    match visibility with
    | QmlAst.TDV_public ->
        Format.fprintf !std "%a@\n" QmlPrint.pp#tsc typescheme
    | QmlAst.TDV_private _ ->
        Format.fprintf !std "@@private %a@\n" QmlPrint.pp#tsc typescheme
    | QmlAst.TDV_abstract _ ->
        Format.fprintf !std "@@abstract %a@\n" QmlPrint.pp#tsc typescheme in
  Format.fprintf !std "Dump types definitions...@\n";
  QmlTypes.Env.TypeIdent.iter iter (OpaTopEnv.types env).QmlTypes.gamma ;
  Format.pp_print_flush !std () ;
  env

let types = "#types$", 0, types_action

(* help *)

let help_action env _ =
  let pp spec doc = Format.fprintf !std "%-32s   %s@\n" spec doc in
  Format.fprintf !std "---@\n";
  Format.fprintf !std "@[<2>opatop directives:@\n";

  (* a *)

  pp "#assert on / off ;;"
    "toggle assertion mode";

  (* b *)

  pp "#bypass ;;"
    "Dump all available external primitives";

  (* d *)

  (* pp "#dbgen on / off ;;" *)
  (*   "Toggle DbGen mode"; *)

  pp "#dddbgen on / off ;;"
    "Toggle dump of returned dbGen code";

  (* e *)

  pp "#env ;;"
    "Dump the environment (types + values)";

  pp "#envgrep regexp ;;"
    "Dump the environment combined with a grep";

  (* pp "#eval on / off ;;" *)
  (*   "Toggle evaluation mode"; *)

  (* h *)

  pp "#help ;;"
    "Print this help menu for directives";

  (* i *)

  pp "#import-db \"db_prefix\" ;;"
    "Try to import an existing database with its definitions";

  (* l *)

  pp "#load \"file.opa\" ;;"
    "Load a file";

  pp "#lookup %s ;;"
    "Find a variable in the environment";

  (* n *)

  pp "#noeval on / off ;;"
    "Disable evaluation";

  (* q *)

  pp "#quit ;;"
    "Quit opatop";

  (* r *)

  pp "#reset ;;"
    "Reset the environment";

  (* pp "#restricted_bypass on / off ;;" *)
  (*   "Toggle restricted bypass mode"; *)

  (* s *)

  pp "#schema \"file.dot\" ;;"
    "Export the current db-schema in a dot format file";

  Format.fprintf !std "@[<4>";

  pp "#set-typer <typer> ;; "
    "Switch the type-checker used, choose between:";

  Format.fprintf !std "%a@]@\n"
    (Format.pp_list "@ /@ " Format.pp_print_string) QmlTyper.available_typer_list;

  (* t *)

  (* pp "#typer on / off ;;" *)
  (*   "Toggle typer mode"; *)

  pp "#types ;;"
    "Dump types definitions from the environment";

  (* extra *)

  pp "[ctr] + 'c'"
    "Interrupt";

  pp "[ctr] + 'd'"
    "Quit";


  Format.fprintf !std "@]@\n";

  (* return the env *)

  env


let help = "#help$", 0, help_action

(* handler *)
(* add there all directives, in alphabetic order *)

let all_directives = [
  assert_ ;
  bypass ;
  (* dbgen ; *)
  dddbgen ;
  env ;
  envgrep ;
  (* eval ; *)
  import_db ;
  help ;
  load ;
  lookup ;
  noeval ;
  quit ;
  reset ;
  restricted_bypass ;
  schema ;
  set_typer ;
  types ;
]

let handler =
  List.fold_left D.add (D.empty ()) all_directives
