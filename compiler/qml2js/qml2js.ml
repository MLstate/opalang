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

(** some type are shared with qml2ocaml, some not *)

type env_js_output =
    {
      generated_files : (string * string) list ; (** path/name without build directory * contents *)
    }

let wclass =
  let doc = "Javascript compiler warnings" in
  WarningClass.create ~name:"jscompiler" ~doc ~err:false ~enable:true ()

type nodejs_module = string

type linked_file =
| ExtraLib of nodejs_module
| Plugin of nodejs_module (* without the .opp extension *)

let nodejs_module_of_linked_file = function
  | ExtraLib m -> m
  | Plugin m -> m

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
  let name = plugin.BslPluginInterface.basename in
  let plugin_path = match plugin.BslPluginInterface.path with
    | Some path -> path
    | None -> Filename.concat stdlib_path (name ^ ".opp") (* guess *)
  in
  plugin_path

let filename_of_plugin plugin =
  (* plugin_path/plugin.opp/pluginNodeJsPackage.js *)
  let plugin_path = path_of_plugin plugin in
  let name = plugin.BslPluginInterface.basename in
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
  val js_bslfilesloading : Qml2jsOptions.t -> BslLib.env_bsl -> (linked_file * string) list * JsAst.code
  val js_generation : Qml2jsOptions.t -> (linked_file * string) list -> J.env_js_input -> env_js_output
  val js_treat : Qml2jsOptions.t -> env_js_output -> int
end =
struct
  open Qml2jsOptions

  (* Return [`require name] if [name] should be required whe
     using the package; [`copy path] if path should be installed
     locally before requiring *)
  let require_of_linked_file = function
    | ExtraLib name -> `require name
    | Plugin name ->
      match BslPluginTable.get name with
      | Some plugin ->
        let path = path_of_plugin plugin in
        if String.is_prefix stdlib_path path then
          `require (name ^ ".opp")
        else
          `copy path
      | None ->
        `require (name ^ ".opp")

  let take_n n =
    let rec aux acc i rest =
      if i >= n then List.rev acc, rest else
        match rest with
        | [] -> List.rev acc, []
        | t::q -> aux (t::acc) (succ i) q
    in aux [] 0

  let js_bslfilesloading env_opt env_bsl =
    (* 1) extra libraries *)
    let extra_lib = List.filter_map (function
      | `server (lib, conf) -> Some (lib, conf)
      | _ -> None
    ) env_opt.extra_lib
    in
    let generated_files =
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

    (* 2) loaded bsl containing js files
       order : since the generated code contains call to bypass of bsl, it is too dangerous to
       put the extra-libs between bsl and the generated code *)
    let generated_files =
      let fold acc loader =
        (* TODO figure out what filter_bsl was being used for *)
        let filename = filename_of_plugin loader in
        let content = File.content filename in
        (Plugin loader.BslPluginInterface.basename, content) :: acc
      in
      List.fold_left fold generated_files env_bsl.BslLib.plugins
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
    ) generated_files)
    in
    List.rev generated_files, ast

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
      generated_files : (linked_file * string) list;
    }
    let pass = "ServerJavascriptCompilation"
    let pp fmt {generated_files} =
      let pp_file fmt (file, content) =
        Format.fprintf fmt "// FILE : %s@\n%s"
          (nodejs_module_of_linked_file file) content
      in
      Format.fprintf fmt "%a" (Format.pp_list "@\n@\n" pp_file) generated_files
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

  let compilation_generation env_opt generated_files env_js_input =
    let js_init = get_js_init env_js_input in
    let save = {S.generated_files} in
    R.save save;
    let js_init = JsUtils.export_to_global_namespace (List.map snd js_init) in
    let code = js_init @ env_js_input.js_code in
    let content = Format.to_string JsPrint.scoped_pp_min#code code in
    let filename = "a.js" in
    let build_dir = env_opt.compilation_directory in
    OManager.verbose "create/enter directory @{<bright>%s@}" build_dir ;
    let success = File.check_create_path build_dir in
    let _ = if not success then OManager.error "cannot create or enter in directory @{<bright>%s@}" build_dir in
    write_main env_opt filename content;
    match ObjectFiles.compilation_mode () with
    | `compilation -> write_package_json env_opt
    | _ -> ()

  let depends_dir env_opt =
    Printf.sprintf "%s_depends" (File.from_pattern "%" env_opt.target)

  let install_node_module env_opt path =
    let short_name = Filename.basename path in
    let dest_path = Filename.concat (depends_dir env_opt) "node_modules" in
    let dest_name = Filename.concat dest_path short_name in
    let _ = File.copy_rec ~force:true path dest_name = 0 in
    ()

  let linking_generation_js_init env_opt generated_files oc =
    let load_oc =
      (* Channel to output libraries *)
      if env_opt.static_link then
        oc
      else (
        let _ = File.check_create_path ~rights:0o700 (depends_dir env_opt) in
        let load_file_name = "load.js" in
        let load_path = Filename.concat (depends_dir env_opt) load_file_name in
        let relative_load_path = Filename.concat
          (Filename.basename (depends_dir env_opt)) load_file_name in
        let load_oc =
          open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o600 load_path
        in
        Printf.fprintf oc "require('./%s');\n" relative_load_path;
        load_oc
      )
    in
    let generated_files = List.rev generated_files in
    let generated_files =
      R.fold_with_name ~packages:true ~deep:true
        (fun _package generated_files {S. generated_files = opxgenfiles} ->
           let generated_files = List.fold_left
             (fun generated_files ((file, content) as opxgenfile) ->
                try
                  let c = List.assoc file generated_files in
                  if content <> c then
                    OManager.warning ~wclass "Two files named %s has not the same content\n%!"
                      (nodejs_module_of_linked_file file);
                  generated_files
                with Not_found -> opxgenfile::generated_files
             ) generated_files opxgenfiles
           in
           generated_files
        ) generated_files
    in
    List.iter
      (fun (file, content) ->
        #<Ifstatic:JS_IMP_DEBUG 1>
        Printf.fprintf load_oc "console.log('Load file %s')" filename;
        #<End>
        if env_opt.static_link then (
          Printf.fprintf oc "///////////////////////\n";
          Printf.fprintf oc "// From %s\n" (nodejs_module_of_linked_file file);
          Printf.fprintf oc "///////////////////////\n";
          Printf.fprintf oc "%s" content;
          Printf.fprintf oc "\n";
        ) else
          let name = match require_of_linked_file file with
            | `require name -> name
            | `copy path ->
              install_node_module env_opt path;
              Filename.basename path
          in
          Printf.fprintf load_oc "require('%s');\n" name;
      ) (List.rev generated_files);
    load_oc

  let get_target env_opt = env_opt.target

  let linking_generation env_opt generated_files env_js_input =
    compilation_generation env_opt generated_files env_js_input;
    let min_node_version = "v0.6.0"
    and max_node_version = "v0.8.0" in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o700 (get_target env_opt) in
    Printf.fprintf oc "#!/usr/bin/env bash

/*usr/bin/env true
%s
if [ $? -ne 0 ]; then exit $?; fi;
NODE_PATH=\"$NODE_PATH:node_modules:/usr/local/lib/node_modules:%s:%s:%s\" node \"$0\" \"$@\"; exit $?;

if (process.version < '%s') {
    console.error('Your version of node seems to be too old. Please upgrade to a more recent version of node (>= %s)');
    process.exit(1);
} else if (process.version > '%s') {
    console.warning('This version of node ('+process.version+') has not been tested with Opa. Use it at your own risks.');
}
*/

" LaunchHelper.script stdlib_qmljs_path stdlib_path static_path min_node_version
      min_node_version max_node_version;
    let is_from_stdlib opx = String.is_prefix stdlib_path opx in
    let load_oc = linking_generation_js_init env_opt generated_files oc in
    let js_file opx = Filename.concat opx "a.js" in
    let read_append opx =
      Printf.fprintf oc "///////////////////////\n";
      Printf.fprintf oc "// From package %s \n" opx;
      Printf.fprintf oc "///////////////////////\n";
      #<Ifstatic:JS_IMP_DEBUG 1>
      Printf.fprintf oc "console.log('Load package %s')" opx;
      #<End>
      let ic = open_in (js_file opx) in
      let chunk = 10000 in
      let str = String.create chunk in
      let rec aux () =
        match input ic str 0 chunk with
        | 0 -> ()
        | len -> output oc str 0 len; aux ()
      in aux(); close_in ic
    in
    let link opx =
      let short_name = Filename.basename opx in
      if env_opt.static_link then
        read_append opx
      else if is_from_stdlib opx then
        Printf.fprintf load_oc "require('%s');\n" short_name
      else (
        install_node_module env_opt opx;
        Printf.fprintf load_oc "require('%s');\n" short_name
      )
    in
    ObjectFiles.iter_dir ~deep:true ~packages:true link;
    read_append env_opt.compilation_directory;
    close_out oc;
    if env_opt.static_link then () else close_out load_oc

  let js_generation env_opt generated_files env_js_input =
    begin match ObjectFiles.compilation_mode () with
    | `compilation -> compilation_generation env_opt generated_files env_js_input
    | `init -> ()
    | `linking -> linking_generation env_opt generated_files env_js_input
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

