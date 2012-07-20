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

(**
   Common library for any Js compiler

   @author Mathieu Barbin
   @author Maxime Audouin
*)

(* depends *)
module List = Base.List
module Format = BaseFormat

(* alias *)
module J = Qml2jsOptions
module BPI = BslPluginInterface
module JA = JsAst

(** some type are shared with qml2ocaml, some not *)

type env_js_output =
    {
      (** path/name without build directory * contents *)
      generated_files : (string * string) list ;
    }

let wclass =
  let doc = "Javascript compiler warnings" in
  WarningClass.create ~name:"jscompiler" ~doc ~err:false ~enable:true ()

type nodejs_module = string

type linked_file =
| ExtraLib of nodejs_module
| Plugin of nodejs_module (* without the .opp extension *)

type loaded_file = linked_file * string

let nodejs_module_of_linked_file = function
  | ExtraLib m -> m
  | Plugin m -> m ^ ".opp"

let system_path =
  try Sys.getenv InstallDir.name
  with Not_found -> "."

let static_path =
  Filename.concat system_path InstallDir.lib_opa

let stdlib_path =
  Filename.concat system_path InstallDir.opa_packages

let stdlib_qmljs_path =
  Filename.concat stdlib_path "stdlib.qmljs"

let plugin_object name =
  (* pluginNodeJsPackage.js *)
  name ^ BslConvention.Suffix.nodejspackage ^ ".js"

let path_of_plugin plugin =
  (* plugin_path/plugin.opp *)
  let name = plugin.BPI.basename in
  let plugin_path = match plugin.BPI.path with
    | Some path -> path
    | None -> Filename.concat stdlib_path (name ^ ".opp") (* guess *)
  in
  plugin_path

let filename_of_plugin plugin =
  (* plugin_path/plugin.opp/pluginNodeJsPackage.js *)
  let plugin_path = path_of_plugin plugin in
  let name = plugin.BPI.basename in
  Filename.concat plugin_path (plugin_object name)

(**
   PASSES :
   -------
  // Command line passes
   returns a : env_bsl, env_blender

   val js_generation : argv_options -> env_js_input -> env_js_output
   val js_treat : argv_options -> env_js_output -> int

   NEEDED from any instance of a js-compiler :
   val qml_to_js : qml_to_js
*)

module JsTreat :
sig
  val js_bslfilesloading : Qml2jsOptions.t -> BslLib.env_bsl ->
    loaded_file list * JsAst.code
  val js_generation : Qml2jsOptions.t -> BslLib.env_bsl ->
    BPI.plugin_basename list ->
    loaded_file list -> J.env_js_input -> env_js_output
  val js_treat : Qml2jsOptions.t -> env_js_output -> int
end =
struct
  open Qml2jsOptions

  let js_bslfilesloading env_opt env_bsl =
    (* TODO: Replace file content by checksum? *)

    (* 1) extra libraries *)
    let extra_lib = List.filter_map (function
      | `server (lib, conf) -> Some (lib, conf)
      | _ -> None
    ) env_opt.extra_lib
    in
    let loaded_files =
      let fold acc (extra_lib, conf) =
        let () =
          (*
            TODO: refactor so that conf is not ignored,
            and optimization pass applied
          *)
          ignore conf
        in
        let get t =
          let contents = File.content (Filename.concat t "main.js") in
          (ExtraLib (Filename.basename t), contents)::acc
        in
        match File.get_locations ~dir:true env_opt.extra_path extra_lib with
        | [] ->
            OManager.error (
              "Cannot find extra-lib @{<bright>%s@} in search path@\n"^^
              "@[<2>@{<bright>Hint@}:@\nPerhaps a missing @{<bright>-I@} ?@]" ) extra_lib
        | [t] -> get t
        | (t::_) as all ->
            OManager.warning ~wclass:WarningClass.bsl_loading (
              "extra-lib @{<bright>%s@} is found in several places@\n%s\n"^^
              "I will use this one : @{<bright>%s@}" ) extra_lib (String.concat " " all) t ;
            get t
      in
      List.fold_left fold [] extra_lib
    in

    (* 2) loaded bsl containing js files order : since the generated
       code contains call to bypass of bsl, it is too dangerous to put
       the extra-libs between bsl and the generated code *)
    let loaded_files =
      let plugins = env_bsl.BslLib.all_plugins in
      let fold acc loader =
        let filename = filename_of_plugin loader in
        let content = File.content filename in
        (Plugin loader.BPI.basename, content) :: acc
      in
      List.fold_left fold loaded_files plugins
    in
    let ast = List.flatten (List.rev_map (fun (file, content) ->
      (*
        TODO: we must take care about conf,
        and not parse file tagged as Verbatim
      *)
      try
        JsParse.String.code ~throw_exn:true content
      with JsParse.Exception error -> (
        let _ = File.output "jserror.js" content in
        OManager.error "JavaScript parser error on file '%s'\n%a\n"
          (nodejs_module_of_linked_file file) JsParse.pp error;
      )
    ) loaded_files)
    in
    List.rev loaded_files, ast

  let write_main env_opt filename contents =
    let filename = Filename.concat env_opt.compilation_directory filename in
    OManager.verbose "writing file @{<bright>%s@}" filename ;
    let success = File.output filename contents in
    if not success then OManager.error "cannot write file @{<bright>%S@}" filename

  (* Write a package.json package descriptor that can be understood by
     node and npm. *)
  let write_package_json env_opt =
    let filename = Filename.concat env_opt.compilation_directory "package.json" in
    OManager.verbose "writing file @{<bright>%s@}" filename ;
    let package_name = Filename.basename env_opt.compilation_directory in
    let package_desc = JsUtils.basic_package_json
      ~version:env_opt.package_version package_name "a.js" in
    match File.pp_output filename Format.pp_print_string package_desc with
    | None -> ()
    | Some error ->
      OManager.error "Couldn't output package: %s\n" error

  module S =
  struct
    type t = {
      (* Packages and plugins required by file *)
      plugin_requires : BPI.plugin_basename list;
      opx_requires : string list;

      generated_code : JsAst.code;
    }
    let pass = "ServerJavascriptCompilation"
    let pp fmt {opx_requires} =
      Format.fprintf fmt "opx: %a"
        (Format.pp_list "@\n@\n" Format.pp_print_string) opx_requires
  end

  module R = ObjectFiles.Make(S)

  let get_js_init env_js_input = List.flatten (
    List.map
      (fun (_, x) -> match x with
       | `ast ast -> ast
       | `string str ->
           OManager.i_error "JS INIT CONTAINS UNEXPECTED PROJECTION : %s\n" str
      )
      env_js_input.Qml2jsOptions.js_init_contents)

  (* JS statement to require library [lib] *)
  let require_stm name lib =
    let call = JsCons.Expr.call ~pure:false
      (JsCons.Expr.native "require")
      [(JsCons.Expr.string lib)] in
    match name with
    | Some name ->
      JsCons.Statement.var
        (JsCons.Ident.native name)
        ~expr:call
    | None ->
      JsCons.Statement.expr call

  let fix_projection env_bsl (index, code) =
    (* HACK: Projections do not take into account which plugins
       produced their corresponding bypasses. Therefore, they can't
       prefix the bypass identifier with the plugin module. This
       function fixes that when outputting the projections in the
       final code, but it makes lots of fragile assumptions and should
       be replaced later with something better. Notice that we also add
       the bypass to the exported object. *)
    let key = BslKey.of_string index in
    let bymap = env_bsl.BslLib.bymap in
    let bypass = Option.get (BslLib.BSL.ByPassMap.find_opt bymap key) in
    let plugin_name = BslLib.BSL.ByPass.plugin_name bypass in
    let compiled = BslLib.BSL.ByPass.compiled_implementation
      ~lang:BslLanguage.nodejs bypass in
    let compiled = match compiled with
      | Some compiled -> compiled
      | None ->
        (* Shouldn't happen, since this bypass was correctly projected *)
        OManager.error "Couldn't find bypass for index %s" index
    in
    let repr =
      BslLib.BSL.Implementation.CompiledFunction.compiler_repr compiled
    in
    let field = BslKey.to_string key in
    JsWalk.ExprInStatement.map
      (fun expr ->
        match expr with
        | JA.Je_ident (_, ident) when JsIdent.to_string ident = repr ->
          JsCons.Expr.dot
            (JsCons.Expr.native ("__opa_" ^ plugin_name))
            field
        | _ -> expr)
      code

  let compilation_generation env_opt env_bsl plugin_requires env_js_input =
    let js_init = List.map
      (fix_projection env_bsl) (get_js_init env_js_input) in
    let js_code = js_init @ env_js_input.js_code in

    let opx_requires = ObjectFiles.fold_dir ~packages:true
      (fun requires opx -> opx :: requires) [] in

    let save = {S.
                plugin_requires;
                opx_requires;
                generated_code = js_code;
               } in
    R.save save;

    let runtime_requires =
      List.filter_map (fun extra_lib ->
        match extra_lib with
        | `server (name, _) -> Some (require_stm None name)
        | _ -> None
      ) env_opt.extra_lib in

    (* Add needed plugins *)
    let plugin_requires = List.map (fun plugin_name ->
      require_stm (Some ("__opa_" ^ plugin_name)) (plugin_name ^ ".opp")
    ) plugin_requires in

    (* Add package dependencies
       NB by not reversing this we were getting bugs in the order of
       the requires *)
    let opx_requires = List.rev_map (fun opx ->
      require_stm None (Filename.basename opx)
    ) opx_requires in

    let requires = runtime_requires @ plugin_requires @ opx_requires in
    let linked_code = requires @ js_code in
    let content = Format.to_string JsPrint.scoped_pp_min#code linked_code in
    let filename = "a.js" in
    let build_dir = env_opt.compilation_directory in
    OManager.verbose "create/enter directory @{<bright>%s@}" build_dir ;
    let success = File.check_create_path build_dir in
    if not success then
     OManager.error "cannot create or enter in directory @{<bright>%s@}"
       build_dir;
    write_main env_opt filename content;
    match ObjectFiles.compilation_mode () with
    | `compilation -> write_package_json env_opt
    | _ -> ()

  let depends_dir env_opt =
    Printf.sprintf "%s_depends" (File.from_pattern "%" env_opt.target)

  let modules_dir env_opt =
    Filename.concat (depends_dir env_opt) "node_modules"

  let is_standard file = String.is_prefix stdlib_path file

  let maybe_install_node_module env_opt path =
    if not (is_standard path) then
      let short_name = Filename.basename path in
      let dest_name = Filename.concat (modules_dir env_opt) short_name in
      let success = File.copy_rec ~force:true path dest_name = 0 in
      if not success then
        OManager.error "Couldn't copy module %s to %s" short_name dest_name

  let get_target env_opt = env_opt.target

  (* Write shell script incantation to check dependencies,
     set load path, etc *)
  let write_launcher_header oc =
    Printf.fprintf oc "#!/usr/bin/env bash

/*usr/bin/env true

export NODE_PATH=\"$NODE_PATH:node_modules:/usr/local/lib/node_modules:%s:%s:%s\"
%s
*/

var dependencies = ['mongodb', 'formidable', 'nodemailer', 'simplesmtp', 'imap'];

%s

" stdlib_qmljs_path stdlib_path static_path LaunchHelper.script LaunchHelper.js

  let linking_generation_static env_opt loaded_files env_js_input =
    (* When linking statically, we just produce a big file
       concatenating all JS files. First we add the runtime and
       plugins, then packages, then BSL projections and finally the
       current code *)
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc]
      0o700 (get_target env_opt) in
    let fmt = Format.formatter_of_out_channel oc in

    write_launcher_header oc;

    List.iter (fun (file, content) ->
      Format.fprintf fmt "// From %s\n"
        (nodejs_module_of_linked_file file);
      Format.fprintf fmt "%s\n" content
    ) loaded_files;

    R.iter_with_dir ~deep:true ~packages:true
      (fun package_dir saved ->
        Format.fprintf fmt "// From %s\n"
          (Filename.basename package_dir);
        Format.fprintf fmt "%a\n" JsPrint.pp_min#code
          saved.S.generated_code);

    let projections = get_js_init env_js_input in
    List.iter (fun (_, code) ->
      Format.fprintf fmt "%a\n" JsPrint.pp_min#statement code
    ) projections;

    Format.fprintf fmt "// Main program\n";
    Format.fprintf fmt "%a\n" JsPrint.pp_min#code
      env_js_input.js_code;

    close_out oc

  (* Output the application launcher and the main object for
     "dynamic" linking *)
  let create_main_files env_opt =
    let launcher_oc = open_out_gen [Open_wronly; Open_creat; Open_trunc]
      0o700 (get_target env_opt) in
    write_launcher_header launcher_oc;

    let success = File.check_create_path ~rights:0o700 (modules_dir env_opt) in
    if not success then
      OManager.error "Couldn't create directory %s" (modules_dir env_opt);
    let main_file_name = "main.js" in
    let main_path = Filename.concat (depends_dir env_opt) main_file_name in
    let relative_main_path = Filename.concat
      (Filename.basename (depends_dir env_opt)) main_file_name in

    Printf.fprintf launcher_oc "require('./%s');\n" relative_main_path;
    close_out launcher_oc;

    let status = File.copy ~force:true
      (Filename.concat env_opt.compilation_directory "a.js")
      main_path in

    if status = 1 then
      OManager.error "Couldn't copy object to %s" main_path

  (* Install required dependencies in the application
     node_modules directory *)
  let install_dependencies env_opt env_bsl =
    List.iter (fun plugin ->
      match plugin.BPI.path with
      | Some path -> maybe_install_node_module env_opt path
      | None -> ()
    ) env_bsl.BslLib.all_plugins;

    R.iter_with_dir ~packages:true ~deep:true
      (fun path _saved ->
        maybe_install_node_module env_opt path
      )

  let linking_generation_dynamic env_opt env_bsl =
    (* "Dynamic" here is somewhat of a misnomer. Compared to "static"
       mode, where we produce just a single file, what we actually do
       is the following:

       - The "main" file (i.e. the one given as -o) will be just the
       launcher script, which will load the rest of the program.

       - The rest of the program will reside in directory
       [app]_depends, where app is the name of the launcher w/o the
       extension. app_depends/main.js will contain the application
       code. Standard dependencies (i.e. the stdlib, runtime and
       normal bsl) will be required from their installed
       locations. Other dependencies, such as additional plugins and
       packages, will be copied to app_depends/node_modules, following
       NodeJs conventions.

    *)

    create_main_files env_opt;
    install_dependencies env_opt env_bsl


  let linking_generation env_opt env_bsl plugin_requires loaded_files env_js_input =
    compilation_generation env_opt env_bsl plugin_requires env_js_input;
    if env_opt.static_link then
      linking_generation_static env_opt loaded_files env_js_input
    else
      linking_generation_dynamic env_opt env_bsl

  let js_generation env_opt env_bsl plugin_requires loaded_files env_js_input =
    begin match ObjectFiles.compilation_mode () with
    | `compilation ->
      compilation_generation env_opt env_bsl plugin_requires env_js_input
    | `init -> ()
    | `linking ->
      linking_generation env_opt env_bsl plugin_requires
        loaded_files env_js_input
    | `prelude -> assert false
    end;
    { generated_files = [get_target env_opt, ""] }

  let js_treat env_opt env_js_output =
    if not env_opt.exe_run
    then 0
    else
      let args = env_opt.exe_argv in
      let args = args @ ( List.map fst env_js_output.generated_files ) in
      let prog = fst (List.hd env_js_output.generated_files) in
      let prog = Filename.concat (Sys.getcwd ()) prog in
      OManager.verbose "building finished, will run @{<bright>%s@}" prog ;
      let command = String.concat " " (prog::args) in
      OManager.verbose "exec$ %s" command ;
      let args = Array.of_list (prog::args) in
      let run () = Unix.execvp prog args in
      Unix.handle_unix_error run ()
end

module Sugar :
sig
  val for_opa : val_:(string -> QmlAst.ident) ->
                ?bsl:JsAst.code ->
                closure_map:Ident.t IdentMap.t ->
                renaming_server:QmlRenamingMap.t ->
                renaming_client:QmlRenamingMap.t ->
                bsl_lang:BslLanguage.t ->
                (module Qml2jsOptions.JsBackend) ->
                Qml2jsOptions.t ->
                BslLib.env_bsl ->
                QmlTyper.env ->
                QmlAst.code ->
                J.env_js_input
  val dummy_for_opa : (module Qml2jsOptions.JsBackend) -> unit
end
=
struct
  let for_opa ~val_ ?bsl:bsl_code ~closure_map ~renaming_server ~renaming_client ~bsl_lang back_end argv env_bsl env_typer code =
    let module M = (val back_end : Qml2jsOptions.JsBackend) in
    let env_js_input = M.compile ~val_ ?bsl:bsl_code ~closure_map ~renaming_server ~renaming_client ~bsl_lang argv env_bsl env_typer code in
    env_js_input
  let dummy_for_opa backend =
    let module M = (val backend : Qml2jsOptions.JsBackend) in
    M.dummy_compile ()
end

