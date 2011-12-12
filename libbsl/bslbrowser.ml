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
   External Primitive libraries browser.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(**
   This application is a command-line tool to browse quickly the primitives
   and the external types defines in a bypass plugin.

   It is used also as a code-sample of the utilisation of the BSL, and
   was initially developped when libbsl was not yet used by backends.

   It is meant to be used conjointly with :
   + {b bslregister}

   For every bypass lib built with {b bslregister}, you'll get a file
   named ["mylib.bypass"], which is a marshalled structure containing
   all the bypass definitions from the plugin. Once the lib built,
   you can use {b bslbrowser} with this bypass file as argument
   for browsing its content.
*)

(* depends *)
module Arg           = Base.Arg
module String        = Base.String
module Format        = Base.Format

(* shorthands *)
module D             = BslDirectives
module BPI           = BslPluginInterface
module BI            = BslInterface
module BSL           = BslLib.BSL
module TS            = BslTinyShell

(* from BSL *)
module ByPass        = BSL.ByPass
module ByPassMap     = BSL.ByPassMap
module HLParser      = BslLib.HLParser

(* operators *)
let (|>) = InfixOperator.(|>)

let cwd = Sys.getcwd ()

(* no error or warning *)

(* debug *)

let debug fmt =
  OManager.printf ("@{<cyan>[Bsl]@}@ @[<2>"^^fmt^^"@]@.")

(* options utils *)

let spliter g =
  List.map String.trim
    (String.slice_chars "{} ,;" g)


(* options state *)


(* b *)


let bypass_plugins = MutableList.create ()
let bypass_plugins_add_file files =
  List.iter (
    fun file ->
      if String.is_suffix ".bypass" file
      then MutableList.add bypass_plugins (BslDynlink.MarshalPlugin file)
      else MutableList.add bypass_plugins (BslDynlink.SharedObject file)
  )
    (spliter files)


(* d *)


let dump = ref false


(* p *)


let pp_bsl = ref false


(* r *)


let rules = MutableList.create ()


(* s *)


let search_types = ref false


(* === *)

let plugin_inclusion file =
  let inclusion = BslConvention.inclusion ~cwd file in
  bypass_plugins_add_file inclusion.BslConvention.plugin ;
  ()

(* following guidelines for command line tools *)


let (!>) = Format.sprintf

(* TODO: add an option opa_code, et js_code *)
let spec = [

  (* d *)

  "--dump",
  Arg.Set dump,
  !>
    " Dump some infos about loaded plugins" ;

  (* p *)


  "--plugin",
  Arg.String plugin_inclusion,
  !>
    "<opp> Take the following argument as an opa plugin (opp)" ;


  "--pp-bsl",
  Arg.Set pp_bsl,
  !>
    " Apply the bslregister preprocessing on the input, and echo on stdout" ;


  (* t *)


  "-t",
  Arg.Set search_types,
  !>
    " Search in types instead of keys for applying regexps" ;

]


let anon_fun file =
  match File.extension file with
  | bypass when bypass = BslConvention.Extension.bypass ->
      bypass_plugins_add_file file

  | opp when opp = BslConvention.Extension.plugin ->
      plugin_inclusion file

  | regexp -> MutableList.add rules regexp


let usage_msg =
  !> "@{<bright>%s@} <Opa External Libraries Browser> %s\nuse: %s [options] [regexps] [bsl-plugins]"
    Sys.argv.(0) BuildInfos.version_id
    Sys.argv.(0)


let parse () =
  let spec = (
    WarningClass.Arg.options @
    (OManager.Arg.version "bslbrowser" :: OManager.Arg.options) @
    BslLib.Arg.options @
    spec
  )

  |> Arg.add_bash_completion
  |> Arg.sort
  |> Arg.align

  in
  Arg.parse spec anon_fun usage_msg


(* ============= *)
(* ============= *)

(** {6 searching for a bypass} *)

(**
   Searching a [regexp] in the lib, matching keys or types.

   @param p where to print the output.
   Used for the pipe mode (calling e.g. grep)
*)
let full_bypass_search ~p:print_endline bymap ?(typ=false) str =
  let found = ref false in
  let searching_for = if typ then "type" else "key" in
  OManager.printf "searching in bypass %s for @{<bright>%S@} ...@." searching_for str;
  let regexp = Str.regexp (".*"^str) in
  let f_iter key bypass =
    let key = BslKey.to_string key in
    let strtyp = Format.sprintf "%a" BslTypes.pp (ByPass.definition_type bypass) in
    let found () =
      found := true;
      print_endline (Format.sprintf "+found fun \"%s\" : %s" key strtyp);
      let iter_imp impl = print_endline (Format.sprintf "%a" BSL.Implementation.pp impl) in
      List.iter iter_imp (ByPass.all_implementations bypass) in
    let tomatch = if typ then strtyp else key in
    if Str.string_match regexp tomatch 0 then found () else ()
  in
  ByPassMap.iter f_iter bymap; if not (!found) then print_endline "Not-found" else ()


let align s =
  let rat = 18 and len = String.length s in
  if len >= rat then s else s^(String.make (rat-len) ' ')


let sprintf_fun ?(typ=None) bypass =
  Format.sprintf "\t%s : %a %a" (align (ByPass.name bypass))
    (Option.pp_default (ByPass.definition_type bypass) BslTypes.pp) typ
    BslLanguage.pp_list (ByPass.langs bypass)


let draw_module ~p:print_endline ?(full=false) name list =
  let iter = function
    | BI.HFunction bypass -> print_endline (sprintf_fun bypass)
    | BI.HModule _ -> print_endline (Printf.sprintf "\t%s : <module>" (align name))
  in
  print_endline (Printf.sprintf "  + module <%s>" (if name = "" then "PLUGIN" else name));
  if full then List.iter iter list else ()


let consmod a = function
  | "" -> a
  | o -> o^"."^a


let iter_types ~p:print_endline elt opt arg =
  let _found = ref false in
  let regexp = Str.regexp (".*"^arg) in
  let found (m, typ) = _found:=true;
    let lgf = ref false in
    let lang ml pp =
      if opt = [] || List.mem (TS.LANG ml) opt
      then (
        lgf:=true;
        print_endline (Format.sprintf "%s : %a" (String.concat "/" m) pp typ)
      )
    in
    lang "ml"  BslTypesGeneration.Ocaml.pp_definition ;
    lang "opa" BslTypesGeneration.Opa.pp_definition ;
    if (not !lgf) && (opt <> [])
    then
      let wlg = function
        | TS.LANG t ->
            let m = Printf.sprintf "lang %s : no type definition" t in
            print_endline m
        | o ->
            let m = TS.clash_option o in
            print_endline m
      in List.iter wlg opt
  in
  let iter (_, m, typ) =
    match typ with
    | BslTypes.External (_, name, _) ->
        if Str.string_match regexp name 0 then found (m, typ)
    | _ -> assert false
  in
  List.iter iter (ByPassMap.types (ByPassMap.Browser.bymap elt));
  if not !_found
  then print_endline (Printf.sprintf "'%s' does not match any type in the loaded plugins" arg)


(* the regexp is for filtering files *)
let show_code_factory fc_extract extract plugins ?(regexp=None) () =
  let regexp_match =
    match regexp with
    | None -> (fun _ -> true)
    | Some regexp ->
        let regexp = Str.regexp regexp in
        (fun s -> Str.string_match regexp s 0) in
  let iter fc =
    let f, c = fc_extract fc in
    if regexp_match (File.chop_extension (Filename.basename f))
    then (
      print_endline (Printf.sprintf "---------- File : %S -----------" f);
      print_endline c
    )
  in
  List.iter (fun plugin -> List.iter iter (extract plugin)) plugins


let js_code p = show_code_factory (fun (f, c, _) -> f, c) (fun plugin -> plugin.BPI.js_code) p
let opa_code p = show_code_factory (fun fc -> fc) (fun plugin -> plugin.BPI.opa_code) p


(* We propose here 2 versions for the browser *)
(* One use the bypass_library representation - the other use the Browser API *)

(* WITH bypass_library *)

let search_lib
    ~p:print_endline       (* for pipe mode *)
    ?(verbose=false)       (* e.g. ls -v *)
    ?(rec_mod=false)
    ?(module_only=false)   (* for printing only the tree *)
    ?(from_type=false)
    ?(module_name="")
    ?(not_found=false)
    ( lib : ByPassMap.Browser.bypass_library list)
    str
    =

  let private_found = ref false in
  let regexp = Str.regexp (".*"^str) in

  (* regroup found items *)
  let foundmap = ListHashtbl.create 10 in
  let modmap = Hashtbl.create 10 in

  let found name ?typ bypass =
    let p =
      if verbose then
        fst (
          List.fold_left
            (fun (acc,prev) lang ->
               (Printf.sprintf "%s%s%s" acc prev (
                  match ByPass.implementation ~lang bypass with
                  | Some t -> Format.sprintf "%a" BSL.Implementation.pp t
                  | None -> "Error when try to get implementation of this bypass")),"\n\n"
            )
            ("",Printf.sprintf "Key : %s\n\n" (BslKey.to_string (ByPass.key bypass)))
            (ByPass.langs bypass))
      else sprintf_fun ~typ:typ bypass in
    ListHashtbl.add foundmap name p in

  let rec browse_decl name kind =
    let found = found name in
    if from_type
    then
      begin
        match kind with
        | BI.HModule (locname, decl) ->
            if rec_mod then List.iter (browse_decl (consmod locname name)) decl

        | BI.HFunction bypass ->
            let typ = ByPass.definition_type bypass in
            let strtyp = Format.to_string BslTypes.pp typ in
            if Str.string_match regexp strtyp 0 then found ~typ bypass else ()
      end
    else
      begin
        let ok n = Str.string_match regexp n 0 in
        match kind with
        | BI.HFunction bypass ->
            if ok (ByPass.name bypass) && not (module_only) then found bypass

        | BI.HModule (locname, decl) ->
            let link = consmod locname name in
            let deep () = List.iter (browse_decl link) decl in
            let full_draw_module = locname = str in
            if ok locname then (
              private_found := true;
              Hashtbl.add modmap (
                fun () -> draw_module ~p:print_endline ~full:full_draw_module locname decl
              ) ()
            );
            if rec_mod then deep ()
      end
  in
  List.iter (browse_decl module_name) lib;
  let iter where fcts =
    private_found := true;
    if where = ""
    then print_endline "+ in bypervasives :"
    else print_endline (Printf.sprintf "+ in module <%s> :" where);
    List.iter print_endline fcts
  in
  ListHashtbl.iter_list iter foundmap;
  Hashtbl.iter (fun t () -> t ()) modmap;
  if not !private_found && not_found then print_endline "Not-found" else ()

exception Syntax_error
let deferror = function
  | Some t -> t
  | None -> raise Syntax_error

let command_of_string s =
  try
    let n, com = BslTinyShell.parse_bsltinyshell_ccom s in
    if n < String.length s then raise Syntax_error
    else com
  with
  | Trx_runtime.SyntaxError _ -> raise Syntax_error

open ByPassMap

(** {6 Main tiny shell loop} *)

(**
   Reacting to tiny shell commands.

   Note for hackers:

   The [elt] is the pwd in the browsing tree.
   The function action returns the new pwd after the execution of the command.
   All command which have no effect on the tree location returns the same elt.
*)
let action
    ( plugins : BPI.plugin list )
    ( bymap : ByPassMap.t )
    ( elt : ByPassMap.Browser.elt )
    ( com : TS.command )  : ( ByPassMap.Browser.elt ) =

  let aux print_endline = function

    | TS.QMLINIT, _, reg ->
        opa_code
          plugins
          ~regexp:(if reg = "" then None else Some reg)
          () ;
        elt

    | TS.KTYPES, opt, arg ->
        iter_types
          ~p:print_endline
          elt
          opt
          arg;
        elt

    | TS.QUIT, _, _ -> exit 0

    | TS.CD, _, arg -> (
        match arg with
        | "" -> Browser.root elt
        | ".." -> Browser.parent elt
        | arg -> (
            match Browser.Path.cd elt (deferror (Browser.Path.of_string arg)) with
            | Some elt -> elt
            | None ->
                let message = Printf.sprintf "[!] No such module or function in %s"
                  (Browser.Path.to_string (Browser.Path.pwd elt)) in
                print_endline message ;
                elt
          )
      )

    | TS.PWD, _, _ ->
        print_endline (Browser.Path.to_string (Browser.Path.pwd elt));
        elt

    | TS.LS, opt, reg ->
        let lib = ByPassMap.Browser.export_children elt in
        search_lib
          ~p:print_endline
          ~verbose:(List.mem TS.VERBOSE opt)
          ~module_name:(Browser.elt_name elt)
          ~from_type:(List.mem TS.TYPES opt)
          ~module_only:(List.mem TS.MODULES opt)
          lib reg;
        elt

    | TS.HELP, _, _ ->
        BslTinyShell.help ();
        elt

    | TS.REGEXP_HELP, _, _ ->
        BslTinyShell.regexp ();
        elt

    | TS.FIND, opt, reg -> (
        let lib = ByPassMap.Browser.export_children elt in
        search_lib
          ~p:print_endline
          ~verbose:(List.mem TS.VERBOSE opt)
          ~module_name:(Browser.elt_name elt)
          ~from_type:(List.mem TS.TYPES opt)
          ~module_only:(List.mem TS.MODULES opt)
          ~rec_mod:true
          ~not_found:true
          lib reg;
        elt
      )

    | TS.KEY, opt, reg ->
        full_bypass_search
          bymap
          ~p:print_endline
          ~typ:(List.mem TS.TYPES opt)
          reg;
        elt

  in

  match ( com : TS.command ) with
  | TS.BslShell com -> aux print_endline com

  | TS.Pipe (exec, com) -> (
      (* Example : ls -v | grep int *)
      let ic, oc = Unix.open_process exec in
      let print s = Printf.fprintf oc "%s\n" s; flush oc in
      let elt = aux print com in
      let _ =
        try
          close_out oc;
          while true do print_endline (input_line ic) done
        with
        | End_of_file -> close_in ic
      in
      elt
    )


(**
   Main loop.

   During the main loop, we do not want to stop with the first fatal error.
   We take benefits of {b bslbrowser} to show, as a code sample, how to interract
   with [OManager]
*)

exception IDE_Exception
let at_exit = { OManager.at_exit = (fun _ -> raise IDE_Exception) }
let _ = OManager.CompilerAsLib.at_exit at_exit

let tiny_shell
    ( plugins : BPI.plugin list )
    ( bymap : ByPassMap.t )
    ( elt : ByPassMap.Browser.elt ) =

  let line = ref 0 in
  BslRegisterParserState.init_file ~filename:"stdin" ;

  let () =
    Printf.fprintf stdout "Opa-plugin-browser %s %s\n"
      BuildInfos.opa_version_name BuildInfos.version_id ;
    Printf.fprintf stdout "Type \"help\" for more information.\n%!"
  in

  let rec aux elt =
    try
      Printf.fprintf stdout "bslbrowser:%s$ " (Browser.Path.to_string (Browser.Path.pwd elt)); flush stdout;
      let input = incr(line); input_line stdin in
      BslRegisterParserState.init_line ~line_number:!line ;

      match HLParser.bypasslang_directive input with
      | D.Directive (pos, dirtags, d) -> (
          let dirtags = BslTags.parse ~pos dirtags in
          OManager.printf "@{<bright>bslregister@} would parse a directive in a bypass-lang file@\n";
          OManager.printf "@[<4>Tags are :@\n%a@]@\n" BslTags.pp dirtags;
          OManager.printf "TODO: print there what would be inserted by bslregister (debug)@\n";
          BslRegisterParserState.set_last_directive d;
          (* TODO: get back a debug like this
          List.iter (
            fun lg ->
              let src, head =
                string_of_bypass_directive
                  ~lang:lg
                  ~type_path_map:BslKeyMap.empty
                  ~current_path:(Browser.Path.to_list (Browser.Path.pwd elt))
                  (dirtags, d)
              in
              print_endline (sprintf "In language %s, the following string is produced :\nsrc:%s\nhead:%s" (Language.to_string lg) src head)
          )
            [Language.ml; Language.js; Language.c]
          ;
          *)
          aux elt
        )

      | _ -> (
          match HLParser.opalang_directive input with

          | D.Source (_, "") ->
              aux elt

          | D.Source _ ->
              let new_elt = action plugins bymap elt (command_of_string input) in
              aux new_elt

          | D.Directive (_, _, opa_directive) -> (
              match opa_directive with
              | D.FormatDefinition name ->
                  print_endline (Printf.sprintf "##format %s = <abstr>" name);
                  aux elt

              | D.Include (fmt, link) -> (
                  match Browser.Path.of_string link with
                  | None ->
                      OManager.printf "[!] Invalid path syntax in @{<bright>%S@}@\n" link

                  | Some path -> (
                      match Browser.Path.cd (Browser.root elt) path with
                      | Some elt2 ->
                          print_endline (Browser.include_format elt2 fmt)
                      | None ->
                          OManager.printf "[!] No such module or function in @{<bright>%s@}@\n"
                            (Browser.Path.to_string (Browser.Path.pwd elt))
                    )
                ); aux elt

            | D.IncludeType str -> (
                let regexp = Str.regexp str in
                let fold_as_iter _ bslty =
                  match bslty with
                  | BslTypes.External (_, name, _) ->
                      if Str.string_match regexp name 0
                      then Format.fprintf Format.std_formatter "%a@\n"
                        BslTypesGeneration.Opa.pp_definition bslty
                  | _ -> ()
                in
                ByPassMap.fold_types bymap fold_as_iter () ;
              ); aux elt
            )
        )
    with
    | IDE_Exception -> aux elt
    | Syntax_error ->
        OManager.printf "[!] Syntax Error.@\n@[<2>@{<red>Hint@}:@\ntry '@{<bright>help@}'@]@." ;
        aux elt
    | End_of_file -> print_newline (); exit 0
    | e ->
        let backtrace = Printexc.get_backtrace () in
        OManager.printf "@{<red>Hint@}:@[<2>@\n%s@]@\n@{<red>Backtrace@}:@\n%s@\n"
          (Printexc.to_string e) backtrace;
        aux elt

  in aux elt


let pp_opabsl browser =
  let preprocess line input =
    BSL.ByPassMap.Browser.preprocess_line browser ~filename:"stdin" ~line input in
  try
    let rec aux i =
      print_endline (preprocess i (input_line stdin));
      aux (succ i)
    in aux 0
  with
  | End_of_file -> exit 0


let iter_rules bymap browser =
  let continue = ref true in
  let lib = BSL.ByPassMap.Browser.export_children browser in
  let p = print_endline in
  let iter regexp =
    (
      if !search_types then
        full_bypass_search bymap ~p ~typ:true regexp
      else
        search_lib ~p ~rec_mod:true ~from_type:false ~not_found:true lib regexp
    );
    continue := false
  in
  MutableList.iter iter rules;
  !continue


(* === *)

(* Main *)
let _ =
  parse ();

  OManager.this_is_tool "bslbrowser";
  HLParser.add_iformat HLParser.default_opa_iformats;

  MutableList.iter (
    fun plugin ->
      OManager.printf "loading primitives from file %S@." (BslDynlink.file plugin) ;
      BslDynlink.load_bypass_plugin plugin
  )
    bypass_plugins;

  let plugins = BslPluginTable.finalize () in
  List.iter (fun plugin -> BSL.RegisterInterface.dynload plugin.BPI.dynloader) plugins;

  (if !dump then
     let iter plugin =
       OManager.printf "%a@\n" BslPluginInterface.pp plugin
     in
     List.iter iter plugins ;
     exit 0
  );

  (* then we build the bymap of bypass *)
  (* in the command line option, there is maybe a restriction on languages *)
  let bymap = BSL.RegisterTable.build_bypass_map () in

  (* for browsing facilities, we build the browser version *)
  let browser = BSL.ByPassMap.Browser.init bymap in
  (if !pp_bsl then pp_opabsl bymap);

  if not (iter_rules bymap browser) then exit 0
  else
    let _ = tiny_shell plugins bymap browser in
    ()
