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
  val js_bslfilesloading : Qml2jsOptions.t -> BslLib.env_bsl -> (string * string) list * JsAst.code
  val js_generation : Qml2jsOptions.t -> (string * string) list -> J.env_js_input -> env_js_output
  val js_treat : Qml2jsOptions.t -> env_js_output -> int
end =
struct
  open Qml2jsOptions

  let take_n n =
    let rec aux acc i rest =
      if i >= n then List.rev acc, rest else
        match rest with
        | [] -> List.rev acc, []
        | t::q -> aux (t::acc) (succ i) q
    in aux [] 0

  let js_bslfilesloading env_opt env_bsl =
    (* 1) extra libraries *)
    let generated_files = [] in
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
          let contents = File.content t in
          (t, contents)::acc
        in
        match File.get_locations env_opt.extra_path extra_lib with
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
      List.fold_left fold generated_files env_opt.extra_lib
    in

    (* 2) loaded bsl containing js files
       order : since the generated code contains call to bypass of bsl, it is too dangerous to
       put the extra-libs between bsl and the generated code *)
    let generated_files =
      let fold acc loader =
        (* TODO figure out what filter_bsl was being used for *)
        let filename =
          Filename.concat
            loader.BslPluginInterface.path
            (loader.BslPluginInterface.basename ^
               BslConvention.Suffix.nodejspackage ^
               ".js") in
        let content = File.content filename in
        (filename, content) :: acc
      in

    (*
    let generated_files =
      let filter_bsl =
        if env_opt.command_line then
          fun (filename, content, conf) ->
            let filename = Filename.basename filename in
            let b:bool = List.for_all
              (fun s -> not (String.is_contained s filename))
              ["bslClient.js"; "bslClientOnly.js"; "bslJson.js"; "syslog.js";
               "jquery"; "jQuery"; "Anchors";
               "json2.js"; "ojs.js"]
              (* every file that need functionality that won't be available in
               * js or rhino should end up in this match *)
            in if b then
              let () =
                (*
                  TODO: refactor so that conf is not ignored,
                  and optimization pass applied
                *)
                ignore conf
              in
              Some (filename, content)
            else None
        else
          fun (filename, content, _conf) -> Some (filename, content)
      in
      let fold acc loader =
        List.rev_filter_map_append filter_bsl loader.BslPluginInterface.nodejs_code acc
      in*)
      List.fold_left fold generated_files env_bsl.BslLib.plugins
    in
    let ast = List.flatten (List.rev_map (
                              fun (filename,content) ->
                                (*
                                  TODO: we must take care about conf,
                                  and not parse file tagged as Verbatim
                                *)
                                try
                                  JsParse.String.code ~throw_exn:true content
                                with JsParse.Exception error -> (
                                  let _ = File.output "jserror.js" content in
                                  OManager.error "JavaScript parser error on file '%s'\n%a\n"
                                    filename JsParse.pp error;
                                )
                            ) generated_files) in
    List.rev generated_files, ast

  let write env_opt (filename, contents) =
    let filename = Filename.concat env_opt.compilation_directory filename in
    OManager.verbose "writing file @{<bright>%s@}" filename ;
    let success = File.output filename contents in
    if not success then OManager.error "cannot write file @{<bright>%S@}" filename

  module S =
  struct
    type t = {
      generated_files : (string * string) list;
    }
    let pass = "ServerJavascriptCompilation"
    let pp fmt {generated_files} =
      let pp_file fmt (filename, content) =
        Format.fprintf fmt "// FILE : %s@\n%s" filename content in
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

  let fix_exports statement =
    JsUtils.export_to_global_namespace
      (JsUtils.globalize_native_ident statement)

  let compilation_generation env_opt generated_files env_js_input =
    let js_init = get_js_init env_js_input in
    let save = {S.generated_files} in
    R.save save;
    let js_init = List.map (fun (_, elt) -> fix_exports elt) js_init in
    let code = env_js_input.js_code @ js_init in
    let content = Format.to_string JsPrint.scoped_pp_min#code code in
    let filename = "a.js" in
    let build_dir = env_opt.compilation_directory in
    OManager.verbose "create/enter directory @{<bright>%s@}" build_dir ;
    let success = File.check_create_path build_dir in
    let _ = if not success then OManager.error "cannot create or enter in directory @{<bright>%s@}" build_dir in
    write env_opt (filename, content)

  let depends_dir env_opt =
    Printf.sprintf "%s_depends" (File.from_pattern "%" env_opt.target)

  (* Copy included BSL files to the build path where
     they can be loaded dynamically *)
  let copy_js_file env_opt filename content =
    let dest = Filename.concat (depends_dir env_opt)
      (Filename.basename filename) in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o600 dest in
    try
      let content =
        List.map fix_exports
          (JsParse.String.code content ~throw_exn:true) in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" JsPrint.pp_min#code content;
      close_out oc;
      dest
    with
    | JsParse.Exception e ->
      let e = Format.to_string JsParse.pp e in
      OManager.error (
        "There was a problem when parsing file %s:" ^^
        "%s\nThis is the PP result:\n%s"
      ) filename e content

  let linking_generation_js_init env_opt stdlib_path generated_files oc =
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
    if env_opt.static_link then
      ()
    else
      begin match stdlib_path with
      | Some path -> Printf.fprintf load_oc "var __stdlib_path = '%s/';\n" path;
      | None -> ()
      end;
    let generated_files = List.rev generated_files in
    Format.printf "generated 1: %a\n" (Format.pp_list ", " Format.pp_print_string)
      (List.map fst generated_files);
    let generated_files =
      R.fold_with_name ~packages:true ~deep:true
        (fun _package generated_files {S. generated_files = opxgenfiles} ->
           let generated_files = List.fold_left
             (fun generated_files ((filename, content) as opxgenfile) ->
                try
                  let c = List.assoc filename generated_files in
                  if content <> c then
                    OManager.warning ~wclass "Two files named %s has not the same content\n%!"
                      filename;
                  generated_files
                with Not_found -> opxgenfile::generated_files
             ) generated_files opxgenfiles
           in
           generated_files
        ) generated_files
    in
    Format.printf "generated 2: %a\n" (Format.pp_list ", " Format.pp_print_string)
      (List.map fst generated_files);
    List.iter
      (fun (filename, content) ->
        #<Ifstatic:JS_IMP_DEBUG 1>
        Printf.fprintf load_oc "console.log('Load file %s')" filename;
        #<End>
        if env_opt.static_link then (
          Printf.fprintf oc "///////////////////////\n";
          Printf.fprintf oc "// From %s\n" filename;
          Printf.fprintf oc "///////////////////////\n";
          Printf.fprintf oc "%s" content;
          Printf.fprintf oc "\n";
        ) else
          let filename = copy_js_file env_opt filename content in
          Printf.fprintf load_oc "require('./%s');\n" (Filename.basename filename);
      ) (List.rev generated_files);
    load_oc

  let get_target env_opt = env_opt.target

  let linking_generation env_opt generated_files env_js_input =
    compilation_generation env_opt generated_files env_js_input;
    let min_node_version = "v0.6.0"
    and max_node_version = "v0.8.0" in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o700 (get_target env_opt) in
    Printf.fprintf oc "#!/usr/bin/env bash

/*usr/bin/env bash <<'EOF'

if ! which node &>/dev/null; then
    echo \"--> node.js missing, please install nodejs from: http://nodejs.org\"
    exit 1
fi;

if ! which npm &>/dev/null; then
    echo \"--> npm missing, please install npm from: http://npmjs.org/\"
    exit 1
fi

function check-node-dependencies() (
    MISSING=\"\"
    for i in $1; do
        if ! npm list | grep -q \"$i\" && ! npm list -g | grep -q \"$i\"; then
            MISSING=\"$MISSING $i\"
        fi
    done
    if [ \"$MISSING\" != \"\" ]; then
        echo \"--> some node modules are missing, please run: npm install -g$MISSING\"
        exit 1;
    fi
)

check-node-dependencies \"mongodb formidable nodemailer simplesmtp imap\" || exit $?

EOF

if [ $? -ne 0 ]; then exit $?; fi;
NODE_PATH=\"$NODE_PATH:/usr/local/lib/node_modules\" node \"$0\" \"$@\"; exit $?;

if (process.version < '%s') {
    console.error('Your version of node seems to be too old. Please upgrade to a more recent version of node (>= %s)');
    process.exit(1);
} else if (process.version > '%s') {
    console.warning('This version of node ('+process.version+') has not been tested with Opa. Use it at your own risks.');
}
*/

" min_node_version min_node_version max_node_version;
    let stdlib_path =
      ObjectFiles.fold_dir ~deep:true ~packages:true
        (fun acc opx ->
          if "stdlib.core.opx" = Filename.basename opx then
            Some (Filename.dirname opx)
          else
            acc) None
    in
    let is_from_stdlib =
      match stdlib_path with
      | Some path -> fun opx -> String.is_prefix path opx
      | None -> fun _ -> false
    in
    let load_oc =
      linking_generation_js_init env_opt stdlib_path generated_files oc
    in
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
      let short_name = js_file (Filename.basename opx) in
      if env_opt.static_link then
        read_append opx
      else if is_from_stdlib opx then
        Printf.fprintf load_oc "require(__stdlib_path + '%s');\n" short_name
      else
        let dest_name = Filename.concat (depends_dir env_opt) short_name in
        let _ = File.copy ~force:true (js_file opx) dest_name = 0 in
        Printf.fprintf load_oc "require('./%s');\n" short_name
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

