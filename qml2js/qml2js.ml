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
          (File.from_pattern "%b.js" t, contents)::acc
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
              let ppjs =
                let ppenv = Pprocess.fill_with_sysenv Pprocess.empty_env in
                (* TODO modifier ppenv avec des choses *)
                let ppopt = Pprocess.default_options ppenv in
                Pprocess.process ~name:filename Pplang.js_description ppopt in
              let content = ppjs content in
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
                              fun (_,content) ->
                                (*
                                  TODO: we must take care about conf,
                                  and not parse file tagged as Verbatim
                                *)
                                JsParse.String.code content
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

(*   let map (name, elts) = *)
(* -        name, ( *)
(* -          match elts with *)
(* -          | `ast elts -> *)
(* -              let code = List.map snd elts in *)
(* -              JsPrint.code code *)
(* -          | `string s -> s *)
(* -        ) *)
(* -      in *)
(* -      List.rev_map_append map env_js_input.js_init_contents generated_files *)

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
    let _debug =
      Format.fprintf (Format.formatter_of_out_channel (open_out (Filename.concat env_opt.compilation_directory "obj.js"))) "%a" S.pp save
    in
    R.save save;
    let content = JsPrint.code env_js_input.js_code in
    let filename = "a.js" in
    let build_dir = env_opt.compilation_directory in
    OManager.verbose "create/enter directory @{<bright>%s@}" build_dir ;
    let success = File.check_create_path build_dir in
    let _ = if not success then OManager.error "cannot create or enter in directory @{<bright>%s@}" build_dir in
    write env_opt (filename, content)

  let linking_generation_js_init generated_files env_js_input oc =
    let generated_files, js_init =
      let js_init = get_js_init env_js_input in
      let to_map l = List.fold_left (fun a (k, v) -> StringMap.add k v a) StringMap.empty l in
      (to_map generated_files, to_map js_init)
    in
    let generated_map, js_init_map =
      R.fold_with_name ~packages:true ~deep:true
        (fun package (generated_map, js_init_map) {S. generated_files; js_init} ->
           let generated_map = List.fold_left
             (fun generated_map (filename, content) ->
                StringMap.replace filename
                  (function
                   | None -> content
                   | Some c ->
                       OManager.verbose "Load file %s from %a"
                         filename ObjectFiles.Package.pp package;
                       if content <> c then
                         OManager.warning ~wclass "Two file named %s has not the same content\n%!"
                           filename;
                       content
                  ) generated_map
             ) generated_map generated_files
           in
           let js_init_map = List.fold_left
             (fun js_init_map (index, elt) ->
                StringMap.add index elt js_init_map
             ) js_init_map js_init
           in generated_map, js_init_map
        ) (generated_files, js_init)
    in
    StringMap.iter
      (fun filename content ->
         Printf.fprintf oc "///////////////////////\n";
         Printf.fprintf oc "// From %s\n" filename;
         Printf.fprintf oc "///////////////////////\n";
         Printf.fprintf oc "%s" content;
         Printf.fprintf oc "\n";
      ) generated_map;
    Printf.fprintf oc "///////////////////////\n";
    Printf.fprintf oc "// BSL JS INIT\n";
    Printf.fprintf oc "///////////////////////\n";
    let fmt = Format.formatter_of_out_channel oc in
    StringMap.iter
      (fun index elt ->
         Printf.fprintf oc "// index : %s\n" index;
         Format.fprintf fmt "%a\n" JsPrint.pp#statement elt;
      ) js_init_map


  let linking_generation env_opt generated_files env_js_input =
    compilation_generation env_opt generated_files env_js_input;
    let oc = open_out env_opt.target in
    linking_generation_js_init generated_files env_js_input oc;
    let read_append opx =
      Printf.fprintf oc "///////////////////////\n";
      Printf.fprintf oc "/** From packages %s \n" opx;
      Printf.fprintf oc "///////////////////////\n";
      let ic = open_in (Filename.concat opx "a.js") in
      let chunk = 10000 in
      let str = String.create chunk in
      let rec aux () =
        match input ic str 0 chunk with
        | 0 -> ()
        | len -> output oc str 0 len; aux ()
      in aux(); close_in ic
    in
    ObjectFiles.iter_dir ~deep:true ~packages:true read_append;
    read_append env_opt.compilation_directory;
    close_out oc

  let js_generation env_opt generated_files env_js_input =
    begin match ObjectFiles.compilation_mode () with
    | `compilation -> compilation_generation env_opt generated_files env_js_input
    | `init -> OManager.verbose "JAVASCRIPT INIT COMPILATION TODO"
    | `linking -> linking_generation env_opt generated_files env_js_input
    | `prelude -> assert false
    end;
    { generated_files = [] }

  let js_treat env_opt env_js_output =
    if not env_opt.exe_run
    then 0
    else
      let prog = env_opt.js_exe in
      let args = env_opt.exe_argv in
      let args = args @ ( List.map fst env_js_output.generated_files ) in
      OManager.verbose "building finished, will run @{<bright>%s@}" prog ;
      OManager.verbose "going to directory @{<bright>%s@}" env_opt.compilation_directory ;
      Sys.chdir env_opt.compilation_directory ;
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

