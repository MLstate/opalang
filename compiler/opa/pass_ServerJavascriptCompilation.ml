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
(* CF mli *)

(* depends *)
module Format = Base.Format
module List = Base.List

(* refactoring in progress *)

(* alias *)
module JsSerializer = Qmljs_Serializer.JsSerializer
module QmlSerializer = Qmljs_Serializer.QmlSerializer

(* shorthands *)
module BPI = BslPluginInterface
module O = OpaEnv

(* -- *)

(*
  Compositionality.
  Used to know what js files have already been registred, to avoid duplication
  of js insertion.
*)
module S =
struct
  type t = {
    (**
       Indexed by the name of the file.
    *)
    extralibs : (string, unit) Hashtbl.t ;

    (**
       Indexed by a plugin_id (module_name)
    *)
    plugins : (string, unit) Hashtbl.t ;
  }

  let pass = "JavascriptCompilation"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = ObjectFiles.Make(S)

(*
  Traduction of options.
*)
let pass_OpaOptionsToJsOptions _backend options =
  let argv_options = Qml2jsOptions.Argv.default () in
  { argv_options with Qml2jsOptions.
    command_line = false;
    cps = options.O.cps_client;
    cps_toplevel_concurrency = options.O.cps_toplevel_concurrency ;
    qml_closure = options.O.closure;
    extra_lib = options.O.extrajs;
    alpha_renaming = options.O.js_local_renaming;
    check_bsl_types = options.O.js_check_bsl_types;
    cleanup = options.O.js_cleanup;
    inlining = options.O.js_local_inlining;
    global_inlining = options.O.js_global_inlining;
    no_assert = options.O.no_assert;
  }


(*
  A external root elemt (plugin and extra libs)
*)
let make_root key content =
  let content = [ JsSerializer.Verbatim content ] in
  let code_elt = { JsSerializer.
    ident = JsSerializer.KI_key key ;
    root = true ;
    definition = `Nothing ;
    content = content ;
  } in
  code_elt

(*
  Serialization of the client code
*)
let client_serialization
    ~client_roots
    rev_code ( env_js_input : Qml2jsOptions.env_js_input ) =
  (*
    bsl projection: They are no longer roots since the generation
    of bypass projection uses Ident.
  *)
  let rev_code =
    List.fold_left (
      fun rev_code (_name, elts) ->
        match elts with
        | `ast elts ->
            List.fold_left (
              fun rev_code (unicity_index, js_elt) ->
                let js_elt = JsUtils.globalize_native_ident js_elt in
                let js_elt =
                  JsSerializer.serialize
                    ~client_roots
                    ~key:unicity_index
                    js_elt in
                js_elt :: rev_code
            ) rev_code elts
        | `string s ->
            make_root (Digest.string s) s :: rev_code
    ) rev_code env_js_input.Qml2jsOptions.js_init_contents
  in

  (*
    serialization of the compiled client code.
  *)
  let rev_code =
    List.fold_left (
      fun rev_code ( code_elt : JsAst.code_elt ) ->
        (*
          No need to globalize native ident, done in the compiler directly.
        *)
        let code_elt =
          JsSerializer.serialize
            ~client_roots
            code_elt in
        code_elt :: rev_code
    ) rev_code env_js_input.Qml2jsOptions.js_code
  in
  rev_code

(*
  A special function for parsing and serializing external js files,
  such than bsl files, and/or extralibs
*)
let parse_js_content ~optimized_conf ~key_prefix ~filename ~content =
  let parsed_code =
    try JsParse.String.code ~throw_exn:true content
    with JsParse.Exception e ->
      OManager.error (
          "External Javascript serialization@\n"^^
          "Cannot serialize external js-code @{<bright>%s@}@\n"^^
          "File %S: %a@\n"
        )
          key_prefix
          filename
          JsParse.pp e in

  let parsed_code = if optimized_conf.BslJsConf.localrenaming then Imp_Renaming.rename parsed_code else parsed_code in
  (* cleanup does not always reaches a fixpoint on the first try, it is worth applying it twice *)
  let parsed_code = if optimized_conf.BslJsConf.cleanup then Imp_CleanUp.clean ~use_shortcut_assignment:true parsed_code else parsed_code in
  let parsed_code = if optimized_conf.BslJsConf.cleanup then Imp_CleanUp.clean ~use_shortcut_assignment:true parsed_code else parsed_code in
  let parsed_code = List.map JsUtils.globalize_native_ident parsed_code in
  parsed_code

let serialize_js_content
    ~client_roots
    ~key_prefix ~parsed_code
    rev_code
    =
  (*
    We use a counter for distinguing statements from external files.
    We assume that if we parse 2 time the same plugin, or external files,
    the order returned by the parser is the same.
  *)
  let count = ref 0 in
  let fold rev_code js_elt =
    let key =
      incr(count) ;
      key_prefix ^ "_item_" ^ (string_of_int !count)
    in
    let js_elt =
      JsSerializer.serialize
        ~client_roots
        ~key
        js_elt in
    js_elt :: rev_code
  in
  List.fold_left fold rev_code parsed_code

(*
  Process all the code.
  Handle the serialization of extralibs, plugins, code, and reinjection of the client code
  in the server code.
*)
let full_serialize
    ~options
    ~closure_map
    ~renaming_server
    ~renaming_client
    ~client_roots
    ~typing:_
    ~bsl_pp
    ~bsl_client
    ~client
    =

  let back_end = options.OpaEnv.js_back_end in
  let jsoptions = pass_OpaOptionsToJsOptions back_end options in

  (* compositionality -- load *)
  let all_extralibs = Hashtbl.create 16 in
  let all_plugins = Hashtbl.create 16 in
  let this_extralibs = Hashtbl.create 4 in
  let this_plugins = Hashtbl.create 4 in

  let () =
    let iter t =
      Hashtbl.iter (Hashtbl.add all_extralibs) t.S.extralibs ;
      Hashtbl.iter (Hashtbl.add all_plugins) t.S.plugins ;
    in
    R.iter ~deep:true iter
  in
  let register_extralib lib_id =
    Hashtbl.add all_extralibs lib_id () ;
    Hashtbl.add this_extralibs lib_id () ;
    ()
  in
  let register_plugin plugin_id =
    Hashtbl.add all_plugins plugin_id () ;
    Hashtbl.add this_plugins plugin_id () ;
    ()
  in
  (* --- *)

  let rev_ast : ([ `unparsed of JsSerializer.jsast_code_elt | `parsed of JsAst.code] * string) list = [] in

  (* 1) extra libs *)
  (*
    Each extra lib is traduced as a [JsSerializer.code_elt]
  *)
  let rev_ast =
    List.fold_left
      (fun rev_ast (extra_lib, conf) ->
         (*
           Avoid to register several time the same extra lib with different packages:
           1. detected at compile time if we have already compiled the same elt
           2. detected at runtime for independant packages (key)
         *)
         if Hashtbl.mem all_extralibs extra_lib
         then rev_ast
         else (
           register_extralib extra_lib ;
           let filename, content, hash = ObjectFiles.find_js_file_content_digest extra_lib in
           let key_prefix = File.concat "extralib" hash in
           match conf with
           | BslJsConf.Verbatim ->
               let code_elt = make_root key_prefix content in
               (`unparsed code_elt, key_prefix) :: rev_ast
           | BslJsConf.Optimized optimized_conf ->
               (`parsed (parse_js_content ~optimized_conf ~key_prefix ~filename ~content), key_prefix) :: rev_ast
         )
      ) rev_ast jsoptions.Qml2jsOptions.extra_lib in


  (* 2) plugins *)
  (*
    Each plugin is also traduced as a list of [JsSerializer.code_elt]
  *)
  let rev_ast =
    List.fold_left
      (fun rev_ast plugin ->
         let plugin_id = plugin.BPI.self_module_name in
         if Hashtbl.mem all_plugins plugin_id
         then rev_ast
         else (
           register_plugin plugin_id ;
           let fold rev_ast (filename, content, conf) =
             let key_prefix = plugin_id ^ filename in
             let content = bsl_pp content in
             match conf with
             | BslJsConf.Verbatim ->
                 let code_elt = make_root key_prefix content in
                 (`unparsed code_elt, key_prefix) :: rev_ast
             | BslJsConf.Optimized optimized_conf ->
                 (`parsed (parse_js_content ~optimized_conf ~key_prefix ~filename ~content), key_prefix) :: rev_ast in
           List.fold_left fold rev_ast plugin.BPI.js_code
         )
      ) rev_ast bsl_client.BslLib.plugins in

  let bsl_and_plugin_ast =
    List.flatten (
      List.filter_map
        (function
         | `parsed parsed -> Some parsed
         | `unparsed _ -> None)
        (List.rev_map fst rev_ast)
    ) in

  (* compilation of js code *)
  let env_js_input_val_ name =
    try
      let name = Hashtbl.find Opacapi.table name in
      OpaMapToIdent.val_ ~side:`client name
    with Not_found ->
      OManager.error "Function %S not registered in Opacapi@\n" name in
  let env_js_input =
    Qml2js.Sugar.for_opa
      ~bsl:bsl_and_plugin_ast
      ~val_:env_js_input_val_
      ~closure_map
      ~renaming_server
      ~renaming_client
      back_end
      jsoptions
      bsl_client
      client.QmlBlender.env
      client.QmlBlender.code
  in

  let rev_code : JsSerializer.jsast_code = [] in
  let rev_code = List.fold_left
    (fun rev_code (ast,key_prefix) ->
       match ast with
       | `parsed parsed_code ->
           serialize_js_content
             ~client_roots
             ~key_prefix
             ~parsed_code
             rev_code
       | `unparsed code_elt ->
           code_elt :: rev_code
    ) rev_code (List.rev rev_ast) in

  (* 3) client code *)
  let rev_code =
    client_serialization
      ~client_roots
      rev_code env_js_input
  in

  (* compositionality -- save *)
  let () =
    let t = { S.
       extralibs = this_extralibs ;
       plugins = this_plugins ;
    } in
    R.save t
  in
  (* -- *)

  rev_code


(*
  Serialize the js, and reinject it in the server code.
*)
let reinjection ~options ~server ~rev_code =
  let server_code = server.QmlBlender.code in
  let server_code =
    QmlSerializer.insert_code
      ~kind:options.OpaEnv.js_serialize
      (List.rev rev_code)
      server_code in
  { server with QmlBlender.
      code = server_code ;
  }

(*
  Main function, exported to be used by the pass.
*)
let process
    ~options
    ~closure_map
    ~renaming_server
    ~renaming_client
    ~client_roots
    ~typing
    ~bsl_pp
    ~bsl_client
    ~server
    ~client
    =

  if client.QmlBlender.code = [] then (
    R.save {S.extralibs = Hashtbl.create 0; S.plugins = Hashtbl.create 0};
    Qml2js.Sugar.dummy_for_opa options.OpaEnv.js_back_end;
    server
  ) else (

    let rev_code = full_serialize
      ~options
      ~closure_map
      ~renaming_server
      ~renaming_client
      ~client_roots
      ~typing
      ~bsl_pp
      ~bsl_client
      ~client
    in
    let server = reinjection ~options ~server ~rev_code in
    server
  )
