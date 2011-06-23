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
   Computing dependencies graphs of the framework.
   @author Mathieu Barbin
*)

(**
   This application takes *.mllib and *.depends and generates :
   - a global graph, showing deps between libs
   - a local graph for each lib, showing deps between modules into the lib
   - any local submodule is shown as a node of a local graph, with an extra
   graph for the local subdirectory.
*)

(* depends *)
module Arg = Base.Arg
module Char = Base.Char
module String = Base.String

let error fmt =
  Printf.kfprintf (fun _ -> exit 1) stderr (fmt^^"\n%!")

type lib_name = string
type repo_name = string
type module_name = string
type path = string

module Lib =
struct
  type t = { lib : lib_name ; sublib : bool ; repo : repo_name option }
  let compare t t'= Pervasives.compare t.lib t'.lib
  let hash t = Hashtbl.hash t.lib
  let equal t t' = compare t t' = 0
end

module LibMap = BaseMap.Make(Lib)
module LibSet = BaseSet.Make(Lib)

module Module =
struct
  type t = { name : module_name ; path : path ; sublib : t option ; lib : Lib.t }
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = Pervasives.(=)
end

module ModuleMap = BaseMap.Make(Module)
module ModuleSet = BaseSet.Make(Module)

module PathMap = StringMap

(**
   Parsing everything, building a big env.
   Mllib should be folded before depends files
*)
type env = {
  libs : ModuleSet.t LibMap.t ;
  modules : Module.t PathMap.t StringMap.t ;
  depends : ModuleSet.t ModuleMap.t ;
}

let empty = {
  libs = LibMap.empty ;
  modules = StringMap.empty ;
  depends = ModuleMap.empty ;
}

let clustered = ref false
let flat_libs = ref false

module Graphs =
struct
  module GM = Graph.Imperative.Digraph.Concrete(Module)
  module GL = Graph.Imperative.Digraph.Concrete(Lib)

  module ModuleDot =
  struct
    include GM
    let graph_attributes _t = [
      `Concentrate true;
    ]
    let default_vertex_attributes _t = []
    let vertex_name m = Printf.sprintf "%S" m.Module.name
    let vertex_attributes m =
      if !flat_libs && String.contains m.Module.path '/'
      then [ `Style `Filled; `Fillcolor 0x9999bb ]
      else []
    let get_subgraph m =
      if not (!clustered && !flat_libs) then None
      else
        match String.split_char '/' m.Module.path with
        | _libroot,"" -> None
        | _libroot,subpath -> Some { Graph.Graphviz.DotAttributes. sg_name = subpath; sg_attributes = [ `Label subpath ] }
    let default_edge_attributes _t = []
    let edge_attributes _e = []
  end

  module GMDot = Graph.Graphviz.Dot(ModuleDot)

  module LibDot =
  struct
    include GL
    let graph_attributes _t = [
      `Concentrate true;
      `Rankdir `LeftToRight;
    ]
    let default_vertex_attributes _t = []
    let vertex_name l = Printf.sprintf "%S" l.Lib.lib
    let vertex_attributes l =
      (if Base.String.is_contained "old" l.Lib.lib
       then [ `Color 0xff0000; `Fontcolor 0xff0000 ]
       else []) @
      let color = 0xffffff in
      (*
        | Some "libqml" -> 0xeeee66
        | Some "qml2llvm" -> 0x66aaaa
        | Some "opa" -> 0xaa66aa
        | Some "appserver" -> 0x66aa66
      *)
      [`Style `Filled; `Fillcolor color]
    let get_subgraph l =
      if not !clustered then None
      else
        Option.map
          (fun repo -> {
             Graph.Graphviz.DotAttributes.sg_name = repo;
             Graph.Graphviz.DotAttributes.sg_attributes = [ `Label repo ];
           })
          l.Lib.repo
    let default_edge_attributes _t = []
    let edge_attributes _e = []
  end

  module GLDot = Graph.Graphviz.Dot(LibDot)

  (**
     Transforming the information into graphs
  *)
  type graphs = {
    g_libs : (lib_name, GM.t) Hashtbl.t ;
    global : GL.t ;
  }

  let create () =
    let g_libs = Hashtbl.create 10 in
    let global = GL.create () in
    {
      g_libs = g_libs ;
      global = global ;
    }

  (* create or get graphs for a lib *)
  let getlib g lib =
    try
      Hashtbl.find g.g_libs lib.Lib.lib
    with
    | Not_found ->
        let lg = GM.create () in
        Hashtbl.add g.g_libs lib.Lib.lib lg;
        lg

  let compute env =
    let graphs = create () in

    (* vertices *)
    let _ = LibMap.iter (
      fun lib set ->
        if not lib.Lib.sublib then GL.add_vertex graphs.global lib ;
        let g = getlib graphs lib in
        ModuleSet.iter (fun m -> GM.add_vertex g m) set ;
        ()
    ) env.libs in

    (* edges *)
    (*
      module_ depends on depend,
      add corresponding edges to the graphs
    *)
    let rec iter_dep module_ depend =
      let lib1 = module_.Module.lib in
      let lib2 = depend.Module.lib in
      if lib1 = lib2
      then (
        let g = getlib graphs lib1 in
        match module_.Module.sublib, depend.Module.sublib with
        | Some sub1, Some sub2 ->
            if sub1 = sub2
            then
              (* internal dependency *)
              GM.add_edge g depend module_
            else
              (* internal dependency betwen submod *)
              GM.add_edge g sub2 sub1
        | Some module_, None ->
            GM.add_edge g depend module_
        | None, Some depend ->
            GM.add_edge g depend module_
        | None, None ->
            GM.add_edge g depend module_
      )
      else (
        (* global dependency *)
        match module_.Module.sublib, depend.Module.sublib with
        | None, None ->
            GL.add_edge graphs.global lib2 lib1
        | Some module_, None ->
            iter_dep module_ depend
        | None, Some depend ->
            iter_dep module_ depend
        | Some module_, Some depend ->
            iter_dep module_ depend
      )
    in

    let iter module_ set =
      let iter depend = iter_dep module_ depend in
      ModuleSet.iter iter set
    in
    let () = ModuleMap.iter iter env.depends in
    graphs

  let handle_open_out file =
    Printf.printf "generating graph %S\n%!" file ;
    try open_out file
    with
    | Sys_error s ->
        error "<!> cannot open_out %S : %s" file s

  let handle_close_out file oc =
    try close_out oc
    with
    | Sys_error s ->
        error "<!> cannot close_out %S : %s" file s

  let global = "odep__all.dot"
  let lib name = Printf.sprintf "odep__lib_%s.dot" name
  let output ?(output_dir="") graphs =
    let success = File.check_create_path output_dir in
    if not success then error "<!> cannot create directory %S" output_dir;
    let path s = Filename.concat output_dir s in
    let global = path global in
    let oc = handle_open_out global in
    GLDot.output_graph oc graphs.global ;
    handle_close_out global oc ;
    let iter libname graph =
      let filename = path (lib libname) in
      let oc = handle_open_out filename in
      GMDot.output_graph oc graph ;
      handle_close_out filename oc ;
    in
    Hashtbl.iter iter graphs.g_libs
end

module Trim =
struct
  let parse line =
    let line =
      match String.findi '#' line with
      | None -> line
      | Some i -> String.sub line 0 i
    in
    let line = String.trim line in
    let length = String.length line in
    if length = 0
    then None
    else Some line
end

let debug = ref None
let log level fmt =
  let f = Format.std_formatter in
  match !debug with
  | Some i when i = level ->
      Format.fprintf f fmt
  | _ ->
      Format.ifprintf f fmt

let lines_foldi fold acc filename =
  if not (File.is_regular filename)
  then (
    Printf.eprintf "<!> file %S not found (ignored)\n%!" filename ;
    acc
  )
  else File.lines_foldi fold acc filename

module Mllib =
struct
  let parse line =
    let line = Trim.parse line in
    match line with
    | None -> None
    | Some line ->
        let path = Filename.dirname line in
        let module_name = Filename.basename line in
        Some (path, module_name)

  let fold env filename =
    (* the name of the lib, is the basename without extension *)
    let libname = File.chop_extension (Filename.basename filename) in
    let repo =
      match
        try String.slice '/' (Unix.readlink filename)
        with Unix.Unix_error _ -> []
      with
      | "repos"::repo::_ -> Some repo
      | _ -> None in
    let fold env line i =
      match parse line with
      | None -> env
      | Some (path, module_name) ->
          let path_elts = String.slice '/' path in
          match path_elts with
          | [] ->
              Printf.eprintf (
                "<!> file %S, line %d\n"^^
                "  module %s with empty path ignored\n%!"
              )
                filename i module_name
              ;
              env
          | [path] ->
              let lib = { Lib.lib = libname ; sublib = false ; repo = repo } in
              let module_ = { Module.
                              lib = lib ;
                              name = module_name ;
                              path = path ;
                              sublib = None ;
                            } in
              let _ =
                log 10 (
                  "mllib:%S, line %d\n"^^
                  "--> lib %S - module %S - path %S\n%!"
                )
                  filename i
                  libname module_name path
              in
              let set = Option.default ModuleSet.empty (LibMap.find_opt lib env.libs) in
              let set = ModuleSet.add module_ set in
              let libs = LibMap.add lib set env.libs in
              let pathmap = Option.default PathMap.empty (StringMap.find_opt module_name env.modules) in
              let pathmap = PathMap.add path module_ pathmap in
              let modules = StringMap.add module_name pathmap env.modules in
              { env with libs = libs ; modules = modules }
          | _ :: subpath -> (
              (* we create 2 modules in this case, one for the module, and one for the sub-lib *)
              let lib = { Lib.lib = libname ; sublib = false ; repo = repo } in
              let submod_name = if !flat_libs then module_name else String.concat "." subpath in
              let submod = { Module.
                             lib = lib ;
                             name = submod_name ;
                             path = path ;
                             sublib = None ;
                           } in

              let libs = env.libs in

              let set = Option.default ModuleSet.empty (LibMap.find_opt lib libs) in
              let set = ModuleSet.add submod set in
              let libs = LibMap.add lib set libs in

              let modules = env.modules in

              let pathmap = Option.default PathMap.empty (StringMap.find_opt submod_name modules) in
              let pathmap = PathMap.add path submod pathmap in
              let modules = StringMap.add submod_name pathmap modules in

              let libs, modules =
                if !flat_libs then libs, modules else
                  let sublib_name = String.concat "." path_elts in
                  let sublib = { Lib.lib = sublib_name ; sublib = true ; repo = repo } in
                  let module_ = { Module.
                                  lib = sublib ;
                                  name = module_name ;
                                  path = path ;
                                  sublib = Some submod ;
                                } in

                  let set = Option.default ModuleSet.empty (LibMap.find_opt sublib libs) in
                  let set = ModuleSet.add module_ set in
                  let libs = LibMap.add sublib set libs in

                  let pathmap = Option.default PathMap.empty (StringMap.find_opt module_name modules) in
                  let pathmap = PathMap.add path module_ pathmap in
                  let modules = StringMap.add module_name pathmap modules in

                  libs, modules
              in

              { env with libs = libs ; modules = modules }
            )
    in
    lines_foldi fold env filename
end

module Depends =
struct
  let parse line =
    match Trim.parse line with
    | None -> None
    | Some line -> (
        let left, right = String.split_char ':' line in
        let left = String.trim left in
        let depends = String.slice_chars " \t\r\n" right in
        match Mllib.parse left with
        | None -> None
        | Some (path, module_name) ->
            let module_name = File.chop_extension module_name in
            let module_name = String.capitalize module_name in
            Some ((path, module_name), depends)
      )

  let fold env filename =
    let fold env line i =
      match parse line with
      | None -> env
      | Some ((path, module_name), depends) -> (
          match StringMap.find_opt module_name env.modules with
          | None -> env
          | Some pathmap -> (
              match PathMap.find_opt path pathmap with
              | Some ( module_ as parent ) ->
                  let set = Option.default ModuleSet.empty (ModuleMap.find_opt module_ env.depends) in
                  let set =
                    let fold_depends set module_name =
                      match StringMap.find_opt module_name env.modules with
                      | Some pathmap -> (
                          let ambigous = PathMap.elts pathmap in
                          match ambigous with
                          | [] ->
                              Printf.eprintf (
                                "<!> file %S, line %d\n"^^
                                "module %s has no declared path (internal error)\n"^^
                                "  the dependencies defined in this line will be ignored\n%!"
                              )
                                filename i module_name
                              ;
                              set
                          | [ module_ ] ->
                              let _ =
                                log 20 (
                                  "depends:%S, line %d\n"^^
                                  "--> module %s depends on %s/%s\n%!"
                                )
                                  filename i parent.Module.name module_.Module.path module_.Module.name
                              in
                              ModuleSet.add module_ set
                          | _ ->
                              Printf.eprintf (
                                "<!> file %S, line %d\n"^^
                                "  the reference to module %s is ambigous\n"^^
                                "  this module name can refer to:\n"
                              )
                                filename i module_name
                              ;
                              List.iter (
                                fun module_ ->
                                  Printf.eprintf (
                                    "    + %s/%s from lib %s\n%!"
                                  )
                                    module_.Module.path module_.Module.name
                                    module_.Module.lib.Lib.lib
                              ) ambigous
                              ;
                              Printf.eprintf (
                                "  the dependencies defined in this line will be ignored\n%!"
                              )
                              ;
                              set
                        )
                      | None ->
                          let _ =
                            log 21 (
                              "depends:%S, line %d\n"^^
                              "--> unbound module %s\n%!"
                            )
                              filename i module_name
                          in
                          set
                    in
                    List.fold_left fold_depends set depends
                  in
                  let depends = ModuleMap.add module_ set env.depends in
                  { env with depends = depends }
              | None ->
                  Printf.eprintf (
                    "<!> file %S, line %d\n"^^
                    "  the module %s with path %S is not defined in any mllib\n"^^
                    "  the dependencies defined in this line will be ignored\n%!"
                  )
                    filename i module_name path
                  ;
                  env
            )
        )
    in
    lines_foldi fold env filename
end

(* d *)

let depends = ref []

(* m *)

let mllib = ref []

(* o *)

let output_dir = ref ""

let spec = [

  (* c *)

  "--clustered",
  Arg.Set clustered,
  " Group libraries by repository in the graph"
  ;

  (* d *)

  "--debug",
  Arg.Int (fun i -> debug := Some i),
  " Activate some debug logs (take a level as arg)"
  ;

  "--dir",
  Arg.Set_string output_dir,
  " Specify an output directory, default is ."
  ;

  (* f *)

  "--flat-libs",
  Arg.Set flat_libs,
  " Show sub-directories as clusters within the graph of the parent lib"
  ;

]

let usage_msg = Printf.sprintf "bld ocaml dependencies graphs generator\nuse: %s *.mllib *.depends\n" Sys.argv.(0)

let anon_fun file =
  match File.extension file with
  | "depends" ->
      depends := file :: !depends
  | "mllib" ->
      mllib := file :: !mllib
  | _ ->
      Printf.eprintf "I don't know what to do with arg %S\n%s%!" file usage_msg ;
      exit 1

let parse () =
  Arg.parse spec anon_fun usage_msg

let _ =
  parse ();
  let env = empty in
  let env = List.fold_left Mllib.fold env !mllib in
  let env = List.fold_left Depends.fold env !depends in
  let graphs = Graphs.compute env in
  let output_dir = !output_dir in
  let () = Graphs.output ~output_dir graphs in
  ()
