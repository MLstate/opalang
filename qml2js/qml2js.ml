(*
    Copyright © 2011, 2012 MLstate

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
      in
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

  let name_generation ?index env_opt =
    match index with
    | None -> env_opt.target
    | Some (i, n) -> Printf.sprintf "js_%d_%s" (i * n) env_opt.target

  let write env_opt (filename, contents) =
    let filename = Filename.concat env_opt.compilation_directory filename in
    OManager.verbose "writing file @{<bright>%s@}" filename ;
    let success = File.output filename contents in
    if not success then OManager.error "cannot write file @{<bright>%S@}" filename

  module S =
  struct
    type t = {
      generated_files : (string * string) list;
      js_init : (BslInterface.unicity_index * JsAst.code_elt) list;
    }
    let pass = "ServerJavascriptCompilation"
    let pp fmt {js_init; generated_files} =
      let pp_file fmt (filename, content) =
        Format.fprintf fmt "// FILE : %s@\n%s" filename content in
      let pp_js_init fmt (name, elt) =
        Format.fprintf fmt "// BSLIndex : %s@\n%a" name JsPrint.pp#statement elt
      in
      Format.fprintf fmt "%a" (Format.pp_list "@\n@\n" pp_file) generated_files;
      Format.fprintf fmt "%a" (Format.pp_list "@\n" pp_js_init) js_init;
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
    let save = {S.generated_files; js_init = get_js_init env_js_input} in
    R.save save;
    let content = Format.to_string JsPrint.scoped_pp_min#code env_js_input.js_code in
    let filename = "a.js" in
    let build_dir = env_opt.compilation_directory in
    OManager.verbose "create/enter directory @{<bright>%s@}" build_dir ;
    let success = File.check_create_path build_dir in
    let _ = if not success then OManager.error "cannot create or enter in directory @{<bright>%s@}" build_dir in
    write env_opt (filename, content)

  let fix_exports statement =
    JsUtils.export_to_global_namespace
      (JsUtils.globalize_native_ident statement)

  (* Copy included BSL files to the build path where
     they can be loaded dynamically *)
  let copy_js_file env_opt filename content =
    let dest = Filename.concat env_opt.compilation_directory
      (Filename.basename filename) in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o700 dest in
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

  let linking_generation_js_init env_opt generated_files env_js_input oc =
    let load_oc =
      (* Channel to output libraries *)
      if env_opt.static_link then
        oc
      else
        let load_path = Filename.concat env_opt.compilation_directory "_load.js" in
        let load_oc =
          open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o700 load_path
        in
        Printf.fprintf oc "require('%s');\n"
          (Filename.concat (Sys.getcwd ()) load_path);
        load_oc
    in
    let js_init =
      let js_init = get_js_init env_js_input in
      List.fold_left (fun a (k, v) -> StringMap.add k v a) StringMap.empty js_init
    in
    let generated_files = List.rev generated_files in
    let generated_files, js_init_map =
      R.fold_with_name ~packages:true ~deep:true
        (fun _package (generated_files, js_init_map) {S. generated_files = opxgenfiles; js_init} ->
           let generated_files = List.fold_left
             (fun generated_files ((filename, content) as opxgenfile) ->
                try
                  let c = List.assoc filename generated_files in
                  if content <> c then
                    OManager.warning ~wclass "Two file named %s has not the same content\n%!"
                      filename;
                  generated_files
                with Not_found -> opxgenfile::generated_files
             ) generated_files opxgenfiles
           in
           let js_init_map = List.fold_left
             (fun js_init_map (index, elt) ->
                StringMap.add index elt js_init_map
             ) js_init_map js_init
           in generated_files, js_init_map
        ) (generated_files, js_init)
    in
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
    Printf.fprintf load_oc "///////////////////////\n";
    Printf.fprintf load_oc "// BSL JS INIT\n";
    Printf.fprintf load_oc "///////////////////////\n";
    let fmt = Format.formatter_of_out_channel load_oc in
    StringMap.iter
      (fun index elt ->
         Format.fprintf fmt "// index : %s\n" index;
         if env_opt.static_link then
           Format.fprintf fmt "%a\n" JsPrint.pp#statement elt
         else
           Format.fprintf fmt "%a\n" JsPrint.pp#statement
             (fix_exports elt)
      ) js_init_map;
    load_oc

  let get_target env_opt = env_opt.target

  let linking_generation env_opt generated_files env_js_input =
    compilation_generation env_opt generated_files env_js_input;
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
        echo \"--> some node modules are missing, please run: npm install$MISSING\"
        exit 1;
    fi
)

check-node-dependencies \"mongodb formidable nodemailer imap\" || exit $?

EOF

if [ $? -ne 0 ]; then exit $?; fi;
NODE_PATH=\"$NODE_PATH:/usr/local/lib/node_modules\" node \"$0\" \"$@\"; exit $?;
*/

";
    let load_oc =
      linking_generation_js_init env_opt generated_files
        env_js_input oc;
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
      if env_opt.static_link
      then
        read_append opx
      else
        Printf.fprintf load_oc "require('%s');\n" (js_file opx)
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

