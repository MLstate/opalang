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
(* depends *)
module Format = BaseFormat
module Hashtbl = BaseHashtbl
module List = BaseList
module String = BaseString

(* alias *)

(* shorthands *)
module BPI = BslPluginInterface
module O = OpaEnv
module SA = SurfaceAst

(* -- *)

let debug fmt =
  OManager.printf ("@{<cyan>[Bsl]@}@ @[<2>"^^fmt^^"@]@.")

(*
  The plugins are accumulated during the compilation, as well as the extralib and extrapaths
  needed for using them.

  A warning is nevertheless produced by the bypass typer when a bypass is normaly not in
  the scope of the current package.
  This appears only in autobuild mode, e.g. compiling a package B after a package A,
  when :
  -the package A imports the plugin P
  -the package B does not imports the plugin P
  -B uses bypass from P

  This situation works because of the side effects accumulated there,
  but the user is just warned that if he would try to compile the package B separatly,
  it would not work, and invite him to add the import-plugin P in the package B.
*)

(*
  The plugin basename, without the opp extesion
*)
type plugin_name = string

module S =
struct
  type entry = {
    plugin_name : plugin_name ;
    extralib : string ;
    extrapath : string ;
    bypass : string ;
  }
  type t = entry list
  let pass = "BslLoading"
  let pp_entry fmt e =
    Format.fprintf fmt "@[<2>Entry: {@\nplugin_name:%S@\nextralib:%S@\nextrapath:%S@\nbypass:%S@]}@]"
      e.plugin_name
      e.extralib
      e.extrapath
      e.bypass
  let pp = Format.pp_list "@\n" pp_entry

  let make plugin_name extralib extrapath bypass = {
    plugin_name ;
    extralib ;
    extrapath ;
    bypass ;
  }
end

module R = ObjectFiles.Make(S)

module Separation :
sig
  type t
  val create : unit -> t
  val add : t -> S.entry -> unit
  val get : t -> S.t
end =
struct
  type t = S.entry list ref
  let create () = ref []
  let add t s = t := s :: !t
  let get t = !t
end

let already_seen_plugin : (plugin_name, plugin_name) Hashtbl.t = Hashtbl.create 16

(*
  We accumulate the extralib and extrapath implied by the plugin seens,
  and add it in the topologic order of plugins (after finalization).
*)

let extralib_plugin : (plugin_name, string) Hashtbl.t = Hashtbl.create 16
let extrapath_plugin : (plugin_name, string) Hashtbl.t = Hashtbl.create 16

let pp_options fmt options =
  let pp = DebugPrint.pp ~depth:max_int in
  Format.fprintf fmt "cclib:     %a@\n" pp options.O.cclib ;
  Format.fprintf fmt "ccopt:     %a@\n" pp options.O.ccopt ;
  Format.fprintf fmt "mllopt:    %a@\n" pp options.O.mllopt ;
  Format.fprintf fmt "mlcopt:    %a@\n" pp options.O.mlcopt ;
  Format.fprintf fmt "extrapath: %a@\n" pp options.O.extrapath ;
  Format.fprintf fmt "extralibs: %a@\n" pp options.O.extralibs ;
  ()

(*
  Add in the options the needed extralib and extrapath implies by the topologic
  order of plugins given.
  This add at the end of already present lib and path, if there are not already there.
*)
let upgrade_options plugins options =
  let make_tbl list =
    let tab = Hashtbl.create 16 in
    let () = List.iter (fun lib -> Hashtbl.add tab lib ()) list in
    tab
  in

  (* options implied by the dependencies of the plugins *)
  let t_cclib = make_tbl options.O.cclib in
  let t_ccopt = make_tbl options.O.ccopt in
  let t_mllopt = make_tbl options.O.mllopt in
  let t_mlcopt = make_tbl options.O.mlcopt in
  let t_extrapath = make_tbl options.O.extrapath in
  let t_extralibs = make_tbl options.O.extralibs in

  let rev_filter_append present list old = List.fold_left
    (fun rev elt -> if Hashtbl.mem present elt then rev else elt::rev)
    old list
  in

  let upgrade_from_properties (rev_cclib, rev_ccopt, rev_mllopt, rev_mlcopt, rev_extrapath, rev_extralibs) properties =
    let rev_cclib = rev_filter_append t_cclib properties.BslConf.cclib rev_cclib in
    let rev_ccopt = rev_filter_append t_ccopt properties.BslConf.ccopt rev_ccopt in
    let rev_mlcopt = rev_filter_append t_mlcopt properties.BslConf.mlcopt rev_mlcopt in
    let rev_mllopt = rev_filter_append t_mllopt properties.BslConf.mllopt rev_mllopt in
    let rev_extrapath = rev_filter_append t_extrapath properties.BslConf.mlinclude rev_extrapath in
    let rev_extralibs = rev_filter_append t_extralibs properties.BslConf.mllibs rev_extralibs in
    (rev_cclib, rev_ccopt, rev_mllopt, rev_mlcopt, rev_extrapath, rev_extralibs)
  in

  let rev_cclib, rev_ccopt, rev_mllopt, rev_mlcopt, rev_extrapath, rev_extralibs =
    List.fold_left
      (fun rev_stuffs plugin ->
         let conf = plugin.BPI.conf in
         (* All platform *)
         let properties = conf.BslConf.all_platform in
         let rev_stuffs = upgrade_from_properties rev_stuffs properties in

         (* Platform specificities *)
         let platform =
           let open Mlstate_platform in
           match mlstate_platform with
           | Unix -> conf.BslConf.linux
           | Windows -> conf.BslConf.windows
           | Cygwin -> conf.BslConf.cygwin
         in
         let rev_stuffs = Option.fold upgrade_from_properties rev_stuffs platform in
         rev_stuffs
      )
      ([], [], [], [], [], [])
      plugins
  in

  let cclib = options.O.cclib @ (List.rev rev_cclib) in
  let ccopt = options.O.ccopt @ (List.rev rev_ccopt) in
  let mllopt = options.O.mllopt @ (List.rev rev_mllopt) in
  let mlcopt = options.O.mlcopt @ (List.rev rev_mlcopt) in
  let extrapath = options.O.extrapath @ (List.rev rev_extrapath) in
  let extralibs = options.O.extralibs @ (List.rev rev_extralibs) in

  (* options implied by the plugins *)

  let t_bypass_plugins = make_tbl options.O.bypass_plugin in
  let t_extralibs = make_tbl extralibs in
  let t_extrapath = make_tbl extrapath in

  let rev_acc present to_add = List.fold_left
    (fun rev plugin ->
       let plugin_name = plugin.BPI.basename in
       let rev =
         match Hashtbl.find_opt to_add plugin_name with
         | None -> rev
         | Some add ->
             if Hashtbl.mem present add
             then rev
             else add::rev
       in
       rev) [] plugins
  in

  let rev_plugins = rev_acc t_bypass_plugins already_seen_plugin in
  let rev_libs = rev_acc t_extralibs extralib_plugin in
  let rev_path = rev_acc t_extrapath extrapath_plugin in

  let bypass_plugin = options.O.bypass_plugin @ (List.rev rev_plugins) in
  let extralibs = extralibs @ (List.rev rev_libs) in
  let extrapath = extrapath @ (List.rev rev_path) in

  { options
    with OpaEnv.
      cclib ;
      ccopt ;
      mllopt ;
      mlcopt ;
      bypass_plugin ;
      extralibs ;
      extrapath ;
  }

let resolve_entry search_path entry =
  let { S.plugin_name = basename ; extralib ; extrapath ; bypass } = entry in
  match Filename.is_relative extrapath with
  | false -> entry
  | true  ->
      (* Searching plugin in extra path... *)
      let candidates = List.filter_map
        (fun p ->
           let fullname = Filename.concat p extrapath in
           #<If:BSL_LOADING>
             OManager.verbose "Seraching %s on %s => %s : %b" extrapath p fullname (File.is_directory fullname);
           #<End>;
           if File.is_directory fullname then Some fullname else None
        ) search_path in
      let aux extrapath =
        OManager.verbose "Select %s" extrapath;
        {S.plugin_name = basename; extralib; extrapath;
         bypass = Filename.concat extrapath (Filename.basename bypass)} in
      match candidates with
      | [] -> entry
      | [extrapath] -> aux extrapath
      | extrapath::_ as places ->
          OManager.warning ~wclass:WarningClass.bsl_loading
            "@\nThe plugin @{<bright>%S@} is found in several places :\n(%s).@\nI will use @{<bright>%S@}"
            basename
            (String.concat "; " places)
            extrapath ;
          aux extrapath

let process
    ~options
    ~code
    =
  (* Pass *)
  let plugins = options.O.bypass_plugin in
  let back_end = options.O.back_end in
  let js_back_end = options.O.js_back_end in
  let cwd = Sys.getcwd () in
  let search_path = cwd :: ObjectFiles.get_paths () in

  (* Separated compilation: loading *)
  let () =
    let iter (package_name, _) entries =
      let iter_entry entry =
        let { S.plugin_name = basename ; extralib ; extrapath ; bypass } = resolve_entry search_path entry in
        if not (Hashtbl.mem already_seen_plugin basename)
        then (
          BslLib.declare_visibility package_name basename ;
          Hashtbl.add already_seen_plugin basename basename ;
          Hashtbl.add extralib_plugin basename extralib ;
          Hashtbl.add extrapath_plugin basename extrapath ;
          BslDynlink.load_bypass_plugin_cache (BslDynlink.MarshalPlugin bypass) ;
        )
      in
      List.iter iter_entry entries
    in
    R.iter_with_name ~packages:true ~deep:true iter
  in
  let separation = Separation.create () in

  let commandline = FilePos.nopos "command line" in
  let plugins = List.map (fun p -> (p, commandline)) plugins in

  (*
    Collect plugin from code and add then in the plugins list.
    Resolve the found location for these plugins (using also by default
    the location in the InstallDir)
  *)
  let code, imported_plugins =
    let imported_plugins = ref [] in
    let filter = function
      | SA.Package (`import_plugin, name), label ->
          let pos = label.QmlLoc.pos in
          let names = [] in (* maybe give plugin from command line *)
          let targets = ObjectFiles.expand_glob ~mode:`plugin names (name, pos) in
          let () =
            #<If:BSL_LOADING $contains "import">
              debug "import-plugin: %a" (Format.pp_list " ; " (Format.pp_fst Format.pp_print_string)) targets
            #<End>
          in
          imported_plugins := List.rev_append targets !imported_plugins ;
          false
      | _ -> true
    in
    let code = List.tail_map
      (fun (filename, content, code) ->
         let code = List.filter filter code in
         (filename, content, code)) code
    in
    code, !imported_plugins
  in
  let plugins = List.rev_append imported_plugins plugins in

  (*
    Normalization of plugin name: add extension if not present
  *)
  let suffix = "." ^ BslConvention.Extension.plugin in
  let plugins = List.rev_map
    (fun (name, pos) ->
       let name = if String.is_suffix suffix name then name else name^suffix in
       name, pos
    ) plugins in

  (* Register default plug-ins. *)
  let () = OpabslgenPlugin.Self.self_store () in

  let package_name = ObjectFiles.get_current_package_name () in

  (*
    The compiler inserts calls to bypass of the opabsl potentially in every package
  *)
  BslLib.declare_visibility package_name OpabslgenPlugin.Self.basename ;

  (* Search additional plug-ins.*)
  List.iter (
    fun (bypass_plugin, pos) ->
      (* the bypass_plugin is containing the extension opp *)
      let basename = Filename.basename bypass_plugin in
      let basename = File.chop_extension basename in

      (*
        There we can add an information of bypass visibility:
        The current package is in the scope of visibility of the plugin basename.
        This can be used for adding a warning about missing dependencies detected in autobuild.
      *)
      BslLib.declare_visibility package_name basename ;

      if not (Hashtbl.mem already_seen_plugin basename)
      then (
        Hashtbl.add already_seen_plugin basename basename ;

        let filename =
          if Filename.is_relative bypass_plugin
          then
            (*
              We should find it in the searched path
            *)

            let found_files = List.filter_map
              (fun p ->
                 let fullname = Filename.concat p bypass_plugin in
                 if File.is_directory fullname then Some fullname else None
              ) search_path in
            let file = match found_files with
              | [] -> bypass_plugin
              | [fullname] -> fullname
              | fullname::_ ->
                  OManager.warning ~wclass:WarningClass.bsl_loading
                    "%a@\nThe plugin @{<bright>%S@} is found in several places.@\nI will use @{<bright>%S@}"
                    FilePos.pp pos
                    bypass_plugin
                    fullname ;
                  fullname
            in
            file
          else bypass_plugin
        in

        let () =
          if not (File.is_directory filename)
          then
            OManager.error "%a@\nI/O error: cannot find @{<bright>%S@} on %s" FilePos.pp pos filename
              (String.concat "; " search_path);
        in

        let inclusion = BslConvention.inclusion ~cwd filename in
        let extralib = inclusion.BslConvention.extralib in
        let extrapath = inclusion.BslConvention.extrapath in
        let plugin = inclusion.BslConvention.plugin in
        Hashtbl.add extralib_plugin basename extralib ;
        Hashtbl.add extrapath_plugin basename extrapath ;
        BslDynlink.load_bypass_plugin (BslDynlink.MarshalPlugin plugin) ;
        let inclusion = BslConvention.inclusion ~cwd:"" bypass_plugin in
        let extralib = inclusion.BslConvention.extralib in
        let extrapath = inclusion.BslConvention.extrapath in
        let plugin = inclusion.BslConvention.plugin in
        Separation.add separation (S.make basename extralib extrapath plugin) ;
      )
  ) plugins ;

  (* Resolve dependencies. *)
  let plugins = BslPluginTable.finalize () in

  (* upgrade options *)
  let () =
    #<If:BSL_LOADING $contains "options">
      debug "@[<2>options before upgrade: @\n%a@]@\n" pp_options options
    #<End>
  in
  let options = upgrade_options plugins options in
  let () =
    #<If:BSL_LOADING $contains "options">
      debug "@[<2>options after upgrade: @\n%a@]@\n" pp_options options
    #<End>
  in

  (* Link with ObjectFiles *)
  let () =
    let t = List.rev_map (fun p -> p.BPI.self_module_name, p.BPI.uniq_id) plugins in
    ObjectFiles.set_bsl_plugins t
  in

  (*
    Actually load plugins.
    There is already a mecanism for avoiding multiple loading in the RegisterInterface.
  *)
  List.iter (fun loader -> BslLib.BSL.RegisterInterface.dynload loader.BPI.dynloader) plugins;

  (*
    TODO(Mathieu) : if needed only.
    It is actually possible to remove this
    by coding a table export in libbsl
  *)
  let back_end_dynload =
    match back_end with
    | `qmlflat -> Flat_Compiler.dynloader in
  let js_back_end_dynload =
    let module M = (val js_back_end : Qml2jsOptions.JsBackend) in
    M.dynloader in
  (* Register plug-ins with actual backend.*)
  List.iter
    (fun plugin ->
       (* ML back-end *)
       back_end_dynload plugin ;
       (* js back-end *)
       js_back_end_dynload plugin ;
    ) plugins;
  let bymap = BslLib.BSL.RegisterTable.build_bypass_map () in (* Build public map.*)
  let bsl = { BslLib.bymap = bymap ; plugins = plugins } in

  (* Separated compilation: saving *)
  let () = R.save (Separation.get separation) in

  options, code, bsl
