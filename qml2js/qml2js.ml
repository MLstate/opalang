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

(* alias *)
module J = Qml2jsOptions

(** some type are shared with qml2ocaml, some not *)

type env_js_output =
    {
      generated_files : (string * string) list ; (** path/name without build directory * contents *)
    }

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
        List.rev_filter_map_append filter_bsl loader.BslPluginInterface.js_code acc
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

  let js_generation env_opt generated_files env_js_input =
    let name_generation ?index () =
      match index with
      | None -> env_opt.target
      | Some (i, n) -> Printf.sprintf "js_%d_%s" (i * n) env_opt.target
    in

    let generated_files = List.rev generated_files in

    (* some more init given by the specific implementation of the backen (bypass projection : bsl_js_init.js) *)
    let generated_files =
      let map (name, elts) =
        name, (
          match elts with
          | `ast elts ->
              let code = List.map snd elts in
              JsPrint.code code
          | `string s -> s
        )
      in
      List.rev_map_append map env_js_input.js_init_contents generated_files
    in

    let generated_files =
      match env_opt.split_js_value with
      | None ->
          let file = name_generation (), JsPrint.code env_js_input.js_code in
          file::generated_files
      | Some n ->
          let rec aux acc i rest =
            let filename = name_generation ~index:(i, n) () in
            match take_n n rest with
            | code, rest ->
                let acc =
                  let file = filename, JsPrint.code code in
                  file::acc in
                begin
                  match rest with
                  | [] -> acc
                  | _ :: _ -> aux acc (succ i) rest
                end
          in
          aux generated_files 1 env_js_input.js_code
    in
    let last =
      match generated_files with
      | (last,_)::_ -> last
      | [] -> Filename.concat env_opt.compilation_directory "empty.js" in

    let generated_files = List.rev generated_files in

    (* keep split, or merge them all *)
    let generated_files =
      if env_opt.split then generated_files else
        begin
          OManager.verbose "append files into %s" last ;
          let fold buf (filename, contents) =
            OManager.verbose "append -- @{<bright>%s@}" filename;
            FBuffer.addln buf contents in
          let buf = FBuffer.create 1048 in
          let buf = List.fold_left fold buf generated_files in
          [ last, FBuffer.contents buf ]
        end
    in

    let _ =
      let write (filename, contents) =
        let filename = Filename.concat env_opt.compilation_directory filename in
        OManager.verbose "writing file @{<bright>%s@}" filename ;
        let success = File.output filename contents in
        if not success then OManager.error "cannot write file @{<bright>%S@}" filename
      in
      let caller_wd = Sys.getcwd () in
      let build_dir = Filename.concat caller_wd env_opt.compilation_directory in
      OManager.verbose "create/enter directory @{<bright>%s@}" build_dir ;
      let success = File.check_create_path build_dir in
      let _ = if not success then OManager.error "cannot create or enter in directory @{<bright>%s@}" build_dir in
      List.iter write generated_files
    in
    { generated_files = generated_files }

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
  let for_opa ~val_ ?bsl:bsl_code ~closure_map ~renaming_server ~renaming_client back_end argv env_bsl env_typer code =
    let module M = (val back_end : Qml2jsOptions.JsBackend) in
    let env_js_input = M.compile ~val_ ?bsl:bsl_code ~closure_map ~renaming_server ~renaming_client argv env_bsl env_typer code in
    env_js_input
  let dummy_for_opa backend =
    let module M = (val backend : Qml2jsOptions.JsBackend) in
    M.dummy_compile ()
end


let wclass =
  let doc = "Javascript compiler warnings" in
  WarningClass.create ~name:"jscompiler" ~doc ~err:false ~enable:true ()
