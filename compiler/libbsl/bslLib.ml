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
(* CF mli *)

(* dependencies, refactoring *)
module Format = BaseFormat
module Hashtbl = BaseHashtbl
module List = BaseList
module String = BaseString

(* ast shorthand *)
module B = BslTypes

(* interfaces *)
module BI = BslInterface

(* alias *)
module IFormat = BslIncludeFormats.IFormat

(* debug *)

let debug fmt =
  OManager.printf ("@{<cyan>[Bsl]@}@ @[<2>"^^fmt^^"@]@.")

(* template *)
(*
  let _ =
    #<If:BSL_VERBOSE $minlevel 1>
      debug "do some %s of level %d@\n" "debug" 1
    #<End>
  in
*)

module HLParser =
struct
  let nopos = FilePos.nopos "BslLib.HLParser"
  let wrap_trx trx_rule str =
    try
      let n, d = trx_rule str in
      if n < String.length str then None
      else Some d
    with
    | BslTags.Exception _
    | BslIncludeFormats.IFormat.Exception _
    | Trx_runtime.SyntaxError _
      -> None

  let opalang_directive str =
    match wrap_trx BslRegisterParser.parse_bslregisterparser_opalang str with
    | Some d -> d
    | None -> BslDirectives.Source (nopos, str)

  let bypasslang_directive str =
    match wrap_trx BslRegisterParser.parse_bslregisterparser_bypasslang str with
    | Some d -> d
    | None -> BslDirectives.Source (nopos, str)

  let bslty str = wrap_trx BslRegisterParser.parse_bslregisterparser_bslty str

  let mk_default_format = List.map
    (fun (t, r , sep) -> t, Printf.sprintf "##format %s \"%s\"%s" t r
       (if sep = "" then "" else (Printf.sprintf " \"%s\"" sep)))
  let default_qml_format = mk_default_format
    [
      "function"       ,"val #n = %%#k%% : #t"     ,"";
      "functions"      ,"let #n = %%#k%% : #t in"  ,"";
      "bind-module"    ,"#m = #m"                  ,"";
      "bind"           ,"#n = #n ; "               ,"";
      "import-module"  ,"let #n = %%#k%% : #t in"  ,"";
      "in-module"      ,"#{import-module}"         ,"";
      "sub-module"     ,"let #m = \n#{#rec, import-module}\n { #{bind-module bind} } in"       ,"";
      "module"         ,"val #m = \n#{ sub-module, import-module }\n  { #{bind-module bind} }" ,"";
      "plot_hierarchy" ,"module #m #{#rec}"        ,"";
    ]
  let default_opa_iformats = mk_default_format
    [
      "opa-function"    ,"#n = %%#k%% : #t"  ,"";
      "opa-bind-module" ,"#m = #m"           ,"";
      "opa-bind"        ,"#n = #n"           ,"";
      "opa-in-module"   ,"#{opa-function}"  ,"";
      "opa-module"      ,"#m = \n#{#rec opa-function}\n  { #{opa-bind-module opa-bind} }" ,"";
    ]

  let show_iformats fmt () =
    List.iter (
      fun (n, f) ->
        Format.fprintf fmt " + format %S :@[<2>@\n%s@]@\n" n f
    )
      default_opa_iformats

  let add_iformat list = List.iter (fun (_, f) -> let _ = opalang_directive f in ()) list

end

module Arg =
struct
  module Arg = Base.Arg
  let options = [
    "--show-iformats",
    Arg.Unit (fun () -> HLParser.show_iformats OManager.oformatter.contents (); exit 0),
    " Show default formats for ##include directive (opa files)" ;
  ]
end

(** this is uggly, should be split, and goes into BslTypesUtils, at least in 3 pieces *)
let map_type_from_type_map_and_path type_path_map current_path ?(definition=false) (typ : BslTypes.t) =
  let module_path = String.concat "." current_path in
  let map_type_key name =
    let parts = List.map String.lowercase (String.slice '.' name) in
    let nkey = BslKey.normalize_string (String.concat "_" parts) in
    let rec find_first_success rest_rev_path =
      let nkey = String.concat "_" (List.rev_map String.lowercase (nkey::rest_rev_path)) in
      let key = BslKey.normalize nkey in
      let _ =
        #<If:BSL_REGISTER $minlevel 5>
          debug "try ukey \"%s\" for key \"%s\"" nkey name
        #<End>
      in
      match BslKeyMap.find_opt key type_path_map with
      | Some (_, _, type_value) -> Some (rest_rev_path, nkey, type_value)
      | None -> (
          match rest_rev_path with
          | [] -> None
          | _::q -> find_first_success q
        )
    in
    let rec warning_other_success ?(accu=[]) rest_rev_path =
      match rest_rev_path with
      | [] ->
          if accu = [] then () else (
            OManager.printf
              "%a@\nin module-path @{<bright>%s@}, reference to type @{<bright>%s@} is ambigues :@\nOther interpretation are @[<4>@\n%a@]@\n"
              BslTypes.pp_context typ
              module_path name
              (Format.pp_list "@\n" (fun fmt t -> BslTypes.pp_context fmt (snd t))) accu;
            OManager.error "Hint: You can precise the path to this type.@\n"
          )

      | _::rest_rev_path ->
          let nkey = String.concat "_" (List.rev_map String.lowercase (nkey::rest_rev_path)) in
          let key = BslKey.normalize nkey in
          let _ =
            #<If:BSL_REGISTER $minlevel 5>
              debug "for the warning. try ukey \"%s\" for key \"%s\"" nkey name
            #<End>
          in
          match BslKeyMap.find_opt key type_path_map with
          | Some (_, _, type_value) ->
              warning_other_success ~accu:((nkey, type_value)::accu) rest_rev_path
          | None ->
              warning_other_success ~accu rest_rev_path in

    let _defkey, type_value =
      match find_first_success (List.rev current_path) with
      | Some (rest_path, defkey, type_value) -> warning_other_success rest_path; defkey, type_value
      | None ->
          OManager.printf "%a@\n" BslTypes.pp_context typ;
          OManager.error
            "In module-path @{<bright>%s@}@ the named-type %s is unbound@\n" module_path name
    in
    type_value in
  let map = BslTypes.Walk.map_up
    (function
     | (B.External (_, n, vs)) as context ->
         (** verification des parametres : juste un decompte *)
         let maped = map_type_key n in (
           match maped with
           | B.External (_, _, vs') ->
               let lena = List.length vs and lenb = List.length vs' in
               if lena <> lenb then (
                 OManager.printf "%a@\n" BslTypes.pp_context context ;
                 OManager.printf "Conflict with this previous type definition:@\n%a@\n"
                   BslTypes.pp_context maped;
                 OManager.error
"The extern-type constructor %s expects %d argument(s),@ but is here applied to %d arguments@\n"
                   n lenb lena
               )
               else (BslTypes.specialize vs maped)
           | typ2 ->
               OManager.printf "%a@\n" BslTypes.pp_context context ;
               OManager.printf "Conflict with this previous type definition:@\n%a@\n" BslTypes.pp_context typ2;
               OManager.error "extern-type \"%s\" is no compatible with this type@\n" n
         )
     | t -> t)
  in
  if definition then
    match typ with
    | B.External _ -> typ
    | _ ->
        OManager.printf "%a@\n" BslTypes.pp_context typ ;
        OManager.error "This kind of type is not allowed in an extern defintion"
  else map typ


(* See the documentation in the interface and the manual *)
let meta_comment t = "(* "^t^" *)"
let meta_code = function
  | BI.MetaCode t -> t
  | BI.MetaComment t -> meta_comment t

(* IMPLEMENTATION OF BSL *)

(*
  Bypass visibility. CF documentation
*)
module BypassVisibility :
sig
  val declare_visibility : ObjectFiles.package_name -> BslPluginInterface.plugin_basename -> unit
  val mem : ObjectFiles.package_name -> BslPluginInterface.plugin_basename -> bool
end =
struct
  let visibility : (ObjectFiles.package_name, StringSet.t) Hashtbl.t = Hashtbl.create 16
  let declare_visibility package_name plugin_basename =
    let plugins = Option.default StringSet.empty (Hashtbl.find_opt visibility package_name) in
    Hashtbl.replace visibility package_name (StringSet.add plugin_basename plugins)
  let mem package_name plugin_basename =
    try
      let set = Hashtbl.find visibility package_name in
      StringSet.mem plugin_basename set
    with
    | Not_found -> false
end
let declare_visibility = BypassVisibility.declare_visibility

(* BSL is a functor : see the manual *)
module MakeLibBSL
  (ML_ITrans : BI.ML_ITRANS)
  (ML_CTrans : BI.ML_CTRANS)
  (JS_CTrans : BI.JS_CTRANS) :
  BI.BSLINTROSPECTION with type ml_ctrans_env = ML_CTrans.env
                    and type js_ctrans_env = JS_CTrans.env
=
struct
  type ml_ctrans_env = ML_CTrans.env
  type js_ctrans_env = JS_CTrans.env

  module Implementation =
  struct
    type function_name = string
    type compiler_repr =
      | Ident of Ident.t
      | String of string

    type compiled =
        {
          c_lang : BslLanguage.t;
          c_type : BslTypes.t ;
          c_tags : BslTags.t ;
          compiler_repr : compiler_repr ;
          origin_file : string;
          is_transtype : bool;
          module_name : string;
(*        ext : string; *)
        }
    type interpreted =
        {
          i_lang : BslLanguage.t;
          i_type : BslTypes.t;
          i_tags : BslTags.t ;
          obj : Obj.t;
        }
    type t = Compiled of compiled | Interpreted of interpreted

    let lang = function
      | Compiled c -> BslLanguage.formate `compiled c.c_lang
      | Interpreted i -> BslLanguage.formate `interpreted i.i_lang

    let trans_type = function
      | Compiled c -> c.c_type
      | Interpreted i -> i.i_type

    let bsltags = function
      | Compiled c -> c.c_tags
      | Interpreted i -> i.i_tags

    module CompiledFunction =
    struct
      let compiler_detailed_repr c = c.compiler_repr
      let string_of_repr = function
        | String s -> s
        | Ident _ -> assert false (* this function isn't called in js
                                   * and this branch is never executed in ml *)
      let compiler_repr c = string_of_repr (compiler_detailed_repr c)
      let origin_file c = c.origin_file
      let is_transtype c = c.is_transtype
      let origin_module c = c.module_name
    end

    let dynamic_repr i = assert (i.i_lang = BslLanguage.mli); i.obj

    let pp_compiled_repr f = function
      | String s -> Format.fprintf f "String \"%s\"" s
      | Ident i -> Format.fprintf f "Ident %s" (Ident.to_string i)

    let pp fmt = function
      | Compiled c ->
          Format.fprintf fmt
            "impl:@{<red>%s@} (compiled)@[<4>@\nfile=%S@\nfun=%a@\ntype=%a@\nis_transtype:%b@\ntags=@[<2>%a@]@]"
            (BslLanguage.to_string c.c_lang)
            c.origin_file
            pp_compiled_repr c.compiler_repr
            BslTypes.pp c.c_type
            c.is_transtype
            BslTags.pp c.c_tags

      | Interpreted i ->
          Format.fprintf fmt
            "impl:@{<red>%s@} (interpreted)@[<4>@\ntype=%a@\ntags=@[<2>%a@]@]"
            (BslLanguage.to_string i.i_lang)
            BslTypes.pp i.i_type
            BslTags.pp i.i_tags
  end

  module ByPass =
  struct
    open Implementation
    type t =
        {
          key : BslKey.t;
          name : string;
          def_type : BslTypes.t;
          impl : Implementation.t list;
          plugin_name : BslPluginInterface.plugin_basename ;
        }

    let key t = t.key
    let skey t = BslKey.to_string t.key
    let name t = t.name

    let format t fmt =
      let def = "(* format error *)" in
      match HLParser.opalang_directive (Printf.sprintf "##include \"%s\" ." fmt) with
      | BslDirectives.Directive (_, _, BslDirectives.Include (fmt, _)) -> (
          match IFormat.fprinter_opt_of_fmt fmt with
          | Some fprinter ->
              fprinter (t.name, (BslKey.to_string t.key),
                        (Format.to_string BslTypes.pp t.def_type))
          | None -> def
        )
      | _ -> def

    let implementation t ~lang =
      let rec find = function
        | [] -> None
        | t::q -> if BslLanguage.compare (Implementation.lang t) lang = 0 then Some t else find q
      in find t.impl

    let all_implementations t = t.impl
    let compiled_implementation t ~lang =
      let rec find = function
        | [] -> None
        | (Interpreted _)::q -> find q
        | (Compiled c)::q -> if BslLanguage.compare c.c_lang lang = 0 then Some c else find q
      in find t.impl

    let interpreted_implementation t ~lang =
      let rec find = function
        | [] -> None
        | (Compiled _)::q -> find q
        | (Interpreted i)::q -> if BslLanguage.compare i.i_lang lang = 0 then Some i else find q
      in find t.impl

    let gen_lang f t =
      let fold accu i = (f i)::accu in
      List.fold_left fold [] t.impl

    let langs = gen_lang Implementation.lang

    let implemented_in t ~lang =
      List.exists (fun v -> BslLanguage.compare lang (Implementation.lang v) = 0) t.impl

    let implemented_in_any, implemented_in_all =
      let mk f =
        fun t ~lang ->
          f (fun u -> List.exists (
               fun v -> BslLanguage.compare u (Implementation.lang v) = 0
             ) t.impl) lang
      in (mk List.exists), (mk List.for_all)

    let definition_type t = t.def_type

    let plugin_name t = t.plugin_name
  end

  module ByPassMap =
  struct
    type _module_table = (string * ((ByPass.t, _module_table) BI.kind)) list

    type t =
        {
          (* unit = elt - optimisation of construct - deconstruct, this is a cache for the browser *)
          elt_root : unit option;
          types : (string * string list * BslTypes.t) list;
          typesmap : BI.typesmap ; (** with module access *)
          map : ByPass.t BslKeyMap.t; (** link with complete key-names *)
          js_init : (BI.unicity_index * JsAst.code_elt) list;
          ocaml_init : string;
          root_node : _module_table;
          ml_ctrans_env : ML_CTrans.env ;
          js_ctrans_env : JS_CTrans.env
        }

    let empty () =
      {
        elt_root = None ;
        types = [] ;
        typesmap = BslKeyMap.empty ;
        map = BslKeyMap.empty ;
        js_init = [];
        ocaml_init = "";
        root_node = [] ;
        ml_ctrans_env = ML_CTrans.empty () ;
        js_ctrans_env = JS_CTrans.empty ()
      }

    let ml_ctrans_env t = t.ml_ctrans_env
    let js_ctrans_env t = t.js_ctrans_env

    let types t = t.types
    let typesmap t = t.typesmap

    let fold_types t fct acc = List.fold_left (fun acc (_, _, t) -> fct acc t) acc t.types

    let opa_types t =
      let buf = FBuffer.create 1024 in
      let buf =
        fold_types t (
          fun buf t -> FBuffer.printf buf "%a@\n" BslTypesGeneration.Opa.pp_definition t
        ) buf
      in
      FBuffer.contents buf

    (** Generated compiler needs some extra code (transcription of type) *)
    let ocaml_init t = t.ocaml_init
    let js_init t = t.js_init

    let find_opt t ?lang key =
      match BslKeyMap.find_opt key t.map with
      | None -> None
      | Some bypass ->
          begin
            match lang with
            | None -> Some bypass
            | Some lang ->
                if ByPass.implemented_in_all bypass ~lang:[lang]
                then Some bypass else None
          end

    let find_opt_implementation t ~lang key =
      match BslKeyMap.find_opt key t.map with
      | None -> None
      | Some bypass -> ByPass.compiled_implementation bypass ~lang

    let find_opt_interpreted_implementation t ~lang key =
      match BslKeyMap.find_opt key t.map with
      | None -> None
      | Some bypass -> ByPass.interpreted_implementation bypass ~lang

    let iter fct t = BslKeyMap.iter fct t.map
    let fold fct t = BslKeyMap.fold fct t.map
    let pp fmt t =
      BslKeyMap.pp "@\n"
        (fun f k _t -> Format.fprintf f "key : %a" BslKey.pp k)
        fmt
        t.map
    let get_map t = t.map

    (** Uncps the type of fun (remove extra continuation)*)
    let uncps_type key def_type =
      let error ty =
        BslError.error (BslError.Context.type_ ty)
          "%a\nBad typed %s [cps-bypass] must be of the form : ..., continuation(...) -> void" BslTypes.pp ty (BslKey.to_string key)
      in
      let rec uncps_type check def_type =
        let rec aux = function
          | BslTypes.OpaValue (_, t) -> aux t
          | t -> t in
        match def_type with
        | BslTypes.Fun (pos1, args, BslTypes.Void _) ->
            let args, ret = List.split_at (List.length args - 1) args in
            let ret =
              assert (List.length ret = 1);
              List.hd ret
            in
            (match uncps_type false ret with
             | BslTypes.External (_, "continuation", [ret] ) ->
                 let proj_args = List.map (uncps_type false) args in
                 BslTypes.Fun (pos1, proj_args, ret)
             | _ -> error def_type
            )
        | BslTypes.Option (p, t) -> BslTypes.Option (p, uncps_type false t)
        | BslTypes.OpaValue (p, t) -> BslTypes.OpaValue (p, uncps_type false t)
        | BslTypes.External (p, s, t) -> BslTypes.External (p, s, List.map (uncps_type false) t)
        | x when not check -> x
        | _ -> error def_type
      in uncps_type true def_type

    let bypass_typer_factory build t key =
      match BslKeyMap.find_opt key t.map with
      | None -> None
      | Some bypass ->
          Return.set_checkpoint (fun label ->
          let () =
            if ObjectFiles.Arg.is_separated ()
            then
              (* check if the visibility is valid, if not warning *)
              let package_name = ObjectFiles.get_current_package_name () in
              let plugin = bypass.ByPass.plugin_name in
              if not (BypassVisibility.mem package_name plugin)
              then (
                OManager.serror (
                  "The bypass @{<bright>%a@} is not visible from this package.@\n"^^
                  "@[<2>@{<bright>Hint@}:@\nadd an @{<bright>import-plugin %s@} in the package '@{<bright>%s@}'@]"
                )
                  BslKey.pp key plugin package_name
                ;
                Return.return label None
              )
          in
          let impls = ByPass.all_implementations bypass in
          let is_cps impl =
            (Implementation.bsltags impl).BslTags.cps_bypass in
          let is_cps =
            List.fold_left
              (fun cps impl ->
                 let icps = is_cps impl in
                 if cps != icps then
                   OManager.error
                     "Tag cps-bypass must be present on all implementation for %s"
                     (BslKey.to_string key)
                 else cps && icps)
              (is_cps (List.hd impls)) (List.tl impls)
          in
          let def_type = (ByPass.definition_type bypass) in
          let def_type =
            if is_cps then uncps_type key def_type
            else def_type
          in
          Return.return label (Some (build (def_type))))

    let bypass_typer ?typeident t = bypass_typer_factory (BslTypes.to_ty ?typeident) t
    let bsl_bypass_typer t = bypass_typer_factory (fun t -> t) t

    let bsl_bypass_tags t ~lang key =
      match BslKeyMap.find_opt key t.map with
      | Some bypass -> (
          match ByPass.implementation ~lang bypass with
          | Some impl -> Some (Implementation.bsltags impl)
          | None -> None
        )
      | None -> None

    let bsl_bypass_cps t ~lang key =
      let cps_key = Printf.sprintf "%s_cps" (BslKey.to_string key) in
      let cps_key = BslKey.normalize cps_key in
      match BslKeyMap.find_opt cps_key t.map with
      | Some bypass when (ByPass.implementation ~lang bypass <> None) -> (
          match bsl_bypass_tags t ~lang cps_key with
          | Some tags when tags.BslTags.cps_bypass ->
              let ty = Option.get (bsl_bypass_typer t key) in
              let ty = BslTypes.purge_opavalue ty in
              let ty_cps = Option.get (bsl_bypass_typer t cps_key) in
              let ty_cps = BslTypes.purge_opavalue ty_cps in
              if BslTypes.compare ~normalize:true ty ty_cps != 0 then
                OManager.error
                  "Found cps bypass (%a) %a for %a but type is not compatible\nExp Type : %a\n\nCps type : %a\n%!"
                  BslLanguage.pp lang
                  BslKey.pp cps_key BslKey.pp key
                  BslTypes.pp ty BslTypes.pp ty_cps
              else
                Some cps_key
          | Some _ ->
              OManager.i_error
                "Found cps bypass (%a) %a for %a but is not tagged as [cps-bypass]"
                BslLanguage.pp lang
                BslKey.pp cps_key BslKey.pp key
          | None ->
              OManager.i_error "The bypass %a was found but these tags was not found in lang %s" BslKey.pp cps_key (BslLanguage.to_string lang)
        )
      | Some _ -> None
      | None -> None

(*     let bsl_bypass_cps t ~lang key = *)

    module Browser =
    struct
      type bypass_library = (ByPass.t, string) BI.hierarchy
      (*
         This data structure simulate a file system.
         Starting from each elt you can browse the all structure.
      *)
      type path_elt = Backward | Forward of string
      type path = path_elt list

      (* uniq identifiant for every module or function :
         ~/module1/module2/function or ~/module3 *)
      type ('a, 'b) _kind_elt =
          {
            bymap : t;
            name : string;
            (* the reference is here for facility in rec construction *)
            root : elt ref;
            kind : ('a, 'b) BI.kind;
            pwd : path;
          }
      and module_elt = elt list
      and public_elt = (ByPass.t, module_elt) BI.kind
      and elt = (ByPass.t, module_elt) _kind_elt

      module Path =
      struct
        (* Note: optimisation with list :
           the path are reversed in the implementation *)
        type step = path_elt
        let root = []
        let is_root = function
          | [] -> true
          | _ -> false
        let step s = Forward s
        let backward = function
          | [] -> []
          | _::q -> q
        let forward p = function
          | Backward -> backward p
          | forward -> forward::p

        let build =
          let rec aux accu = function
            | [] -> Some accu
            | ".."::q -> aux (backward accu) q
            | s::q ->
                let withchars chars s = List.exists (String.contains s) chars in
                if withchars ['.'; '/'; '#'; '\\'] s
                then None
                else aux (forward accu (step s)) q
          in aux []

        let of_string s =
          let elts =
            String.slice_chars "~/,.#" s in
          build elts

        let to_string = function
          | [] -> "/"
          | els ->
              let aux = function
                | Backward -> ".."
                | Forward s -> s in
              let fold accu step = Printf.sprintf "%s/%s" (aux step) accu in
              List.fold_left fold "" els

        let to_list =
          let rec aux = function
            | Backward -> ".."
            | Forward s -> s in
          let fold accu step = (aux step)::accu in
          List.fold_left fold []

        let remove_backward init path =
          let safe_forward p s = match p, s with
          | Backward , [] -> []
          | Backward, _::q -> q
          | Forward s, tl -> s::tl
          in
          List.fold_right safe_forward (path@init) []

        let cd elt path =
          let safe_path = List.rev (remove_backward elt.pwd path) in
          (* stoping fold_left *)
          let rec stop_fold accu = function
            | [] -> Some accu
            | t::q ->
                begin
                  match accu.kind with
                  | BI.Function _ -> None (* a function has no child *)
                  | BI.Module accu ->
                      begin
                        match List.find_opt (fun e -> String.compare e.name t = 0) accu with
                        | None -> None
                        | Some elt -> stop_fold elt q
                      end
                end
          in
          stop_fold !(elt.root) safe_path

        let pwd elt = elt.pwd
      end

      (* constructor of the data structure from the bypassmap struct *)
      let init t = match t.elt_root with Some elt -> Obj.magic elt | None ->
        let wait_root : elt ref = ref (Obj.magic ()) in (* Because it is not trivial to produce an empty elt *)
        let rec aux path (name, kind) =
          (* #<< debug browserstructure (sprintf "building init from %s (path=%s)" name (Path.to_string path)); >>#; *)
          let pwd = Path.forward path (Path.step name) in
          let maped_kind : (ByPass.t, module_elt) BI.kind =
            match kind with
            | BI.Function by -> BI.Function by
            | BI.Module table -> BI.Module (List.map (aux pwd) table)
          in
          { bymap = t; name = name ; root = wait_root ; kind = maped_kind ; pwd = pwd }
        in
        let root_children = List.map (aux Path.root) t.root_node in
        let root = { bymap = t; name = "" ; root = wait_root ; kind = BI.Module root_children ; pwd = Path.root } in
        wait_root := root;
        root
      let bymap elt = { elt.bymap with elt_root = Some (Obj.magic !(elt.root)) }
      let root elt = !(elt.root)
      let elt_name elt = elt.name
      let elts e = e
      let children = List.map (fun e -> e.name, e)
      let is_root e = Path.is_root e.pwd

      let parent e =
        match Path.cd e [Backward] with
        | Some e -> e
        | None -> assert false (* Parent of root is root *)

      let public_elt e = e.kind

      (** Get Initial Code : this not include the types definition *)
      let rec export_bypass elt =
        (* #<< debug browserstructure (sprintf "export bypass from %s (%s)" elt.name (Path.to_string elt.pwd)); >>#; *)
        match elt.kind with
        | BI.Function by -> BI.HFunction by
        | BI.Module m -> BI.HModule (elt.name, (List.map export_bypass (elts m))) (* elts : impl depends *)
      let export_children elt =
        (* #<< debug browserstructure (sprintf "export children of %s (%s)" elt.name (Path.to_string elt.pwd)); >>#; *)
        match elt.kind with
        | BI.Function by -> [BI.HFunction by]
        | BI.Module m -> List.map export_bypass (elts m)

      (* TODO : rewrite with Format *)

      let include_format =
        let default_sep = "\n" in
        let fprint fprinter name by =
          fprinter (name, (ByPass.skey by),
                    (Format.to_string BslTypesGeneration.Opa.pp (ByPass.definition_type by)))
        in
        let super_concat s f l = String.concat s (List.filter ((<>) "") (List.map f l))
        in
        let rec mfmt_printer mfmt (name, eltlist) =
          super_concat "" (
            function
              | `Mfmt_name -> name
              | `Mfmt_const s -> s
              | `Mfmt_iter (mfmt_opt, fprinter, sep) ->
                  let mfmt = match mfmt_opt with Some mfmt -> mfmt | None -> mfmt in
                  super_concat (Option.default default_sep sep) (
                    function elt -> match elt.kind with
                    | BI.Function by -> fprint fprinter elt.name by
                    | BI.Module eltlist -> mfmt_printer mfmt (elt.name, eltlist)
                  ) eltlist
          ) mfmt
        in
        fun elt fmt -> match elt.kind with
        | BI.Function by -> (
            match IFormat.fprinter_opt_of_fmt fmt with
            | Some fprinter -> fprint fprinter elt.name by
            | None -> assert false
                (* TODO
                   Mathieu, Mon Aug 16 22:18:01 CEST 2010
                   After the refactoring of libbsl, I have no idea in what case
                   this could happens.
                   Please, when somebody find his way to this assert false,
                   provide an error message, or tell me how to reproduce.
                   Thx.
                   was : *)
                (*
                  BslRegisterParser.error (sprintf "function \"%s\"\nThis path has type function but is here used with a module format" elt.name)
                *)
          )

        | BI.Module eltlist -> mfmt_printer (IFormat.mfmt_of_fmt fmt) (elt.name, eltlist)

      let preprocess_line t ~filename ~line input =
        let comment fmt = let k s = "/* " ^ s ^ " */" in Printf.ksprintf k fmt in
        let root = init t in
        BslRegisterParserState.init_file ~filename ;
        BslRegisterParserState.init_line ~line_number:line ;
        match HLParser.opalang_directive input with
        | BslDirectives.Source (_, s) -> s
        | BslDirectives.Directive (_, _, BslDirectives.FormatDefinition name) ->
            comment "##format %s = <abstr>" name

        | BslDirectives.Directive (_, _, BslDirectives.Include (fmt, link)) -> (
              match Path.of_string link with
              | None -> comment "invalid path name : \"%s\"" link
              | Some path -> (
                  match Path.cd root path with
                  | Some elt2 -> include_format elt2 fmt
                  | None -> comment "[!] module or function \"%s\" not found" link
                )
          )

        | BslDirectives.Directive (pos, _, BslDirectives.IncludeType str) -> (
            let regexp = Str.regexp str in
            let buf = FBuffer.create 1024 in
            let buf = fold_types t
              (fun buf t ->
                 let name = match t with
                   | BslTypes.External (_, name, _) -> name
                   | _ ->
                       OManager.printf "Assertion Failure@\n%a%a@\n%!"
                         FilePos.pp_citation pos BslTypes.pp_context t;
                       assert false
                 in
                 if Str.string_match regexp name 0
                 then FBuffer.printf buf "%a@\n" BslTypesGeneration.Opa.pp_definition t else buf
              ) buf in
            FBuffer.contents buf
          )
    end
  end

  (* The data here are managed with side effects with the functions register *)
  (* <!> The types definitions are in a topologic order,
     so we must use a SortedHashtbl (cf base) *)
  (* ===================================================================================================== *)
  type _imperativ_data_module = (string, (BslKey.t, _imperativ_data_module) BI.kind) Hashtbl.t             (* *)
  let _imperativ_module_table : _imperativ_data_module = Hashtbl.create 128                             (* *)
  let _bypass_table : (BslKey.t, ByPass.t) Hashtbl.t = Hashtbl.create 512                               (* *)
  let _types_table : (BslKey.t, (string * string list * BslTypes.t)) SortHashtbl.t =                (* *)
    SortHashtbl.create 128                                                                              (* *)
  let _loaded : (string, unit) Hashtbl.t = Hashtbl.create 16                                            (* *)
  (* ===================================================================================================== *)

  (* The modules where are the side effects *)
  module RegisterTable =
  struct
    open ByPassMap

    type building_env =
        {
          ml_ctrans : ML_CTrans.env ;
          js_ctrans : JS_CTrans.env ;
          (* c_ctrans  : C_CTrans.env  ; *)

          generated_ml : FBuffer.t ;
          generated_js : (BI.unicity_index * JsAst.code_elt) list (*reversed*)(*FBuffer.t*) ;
          (* generated_c : FBuffer.t ; *)
        }

    (* During the building of the bymap, we generate transcription code for qml2ocaml compilers and
       for qml interpreters written in ocaml, with the informationn of type transcription in
       the 2 spezialized modules in arg of the big functor MakeLibBSL *)

    let identity_coerced _ = () (* todo : warning *)

    let trans_name =
      let fresh = ref (-1) in
      (fun _name -> incr(fresh); Printf.sprintf "bslp%d" !fresh) (* todo voir pour le caractere compositionnel du truc...*)
    let trans_ident key = Ident.source ("bsl_" ^ BslKey.to_string key)

    let _meta_code_warning =
      let __t = Hashtbl.create 10 in
      (fun f typ ->
         if Hashtbl.mem __t typ then () else
           (Hashtbl.add __t typ ();
            #<If:BSL_PROJECTION $minlevel 1>
              debug "%a@\nSpecialized BSL : In your transtyping module@\nCTrans.%s returns (MetaCode \"\") on this type@\n"
              BslTypes.pp_context typ f
            #<Else>
              ignore f; ignore typ
            #<End>))

    let re_compiled_generate_ml building bslkey bsltags impl (inputs : _ list option) output =
      let env = building.ml_ctrans in
      if BslTags.never_projected bsltags then building, None else
      let trans_type = ref false in
      let trans_output = ref false in

      let ml_arg = Printf.sprintf "x%d" and ml_ret = "r" in
      let proj_arg = Printf.sprintf "p%d" in

      let env, (trans_out, typed_out) =
        let env, maped_output = ML_CTrans.qml_of_ocaml ~bslkey ~bsltags ~env output (BI.MetaIdent ml_ret) in
        env,
        (match maped_output with
         | None -> ml_ret, output
         | Some (BI.MetaCode "") ->
             (match output with B.OpaValue _ -> () | _ -> _meta_code_warning "qml_of_ocaml" output);
             ml_ret, output
         | Some (BI.MetaComment comment) ->
             trans_output := true;
             Printf.sprintf "%s (* : %s *)" ml_ret comment, output
         | Some (BI.MetaCode conv) ->
             trans_type:=true; trans_output := true;
             conv, BslTypes.opavalue output
        )
      in

      let new_impl = trans_name bslkey in
      let buf = building.generated_ml in

      match inputs with
      | None ->
          if !trans_type then
            let buf = FBuffer.addln buf (Printf.sprintf "let %s = %s" new_impl trans_out) in
            let type_of_new_impl = typed_out in
            (* we must save the new generated function in ocaml init *)
            { building with
                ml_ctrans = env ;
                generated_ml = buf ;
            },
            Some (Implementation.String new_impl, type_of_new_impl)
          else
            { building with ml_ctrans = env }, None

      | Some inputs ->

          let env, projected_args = List.fold_left_map_i
            (fun i env typ ->
               let param = ml_arg i in
               let env, meta_code = ML_CTrans.ocaml_of_qml ~bslkey ~bsltags ~env typ (BI.MetaIdent param) in
               env,(
                 match meta_code with
                 | None -> param, typ, None
                 | Some (BI.MetaCode "") ->
                     (match typ with B.OpaValue _ -> () | _ -> _meta_code_warning "ocaml_of_qml" typ);
                     param, typ, None
                 | Some (BI.MetaComment comment) ->
                     (Printf.sprintf "( %s (* : %s *) )" param comment), typ, None
                 | Some (BI.MetaCode conv) ->
                     trans_type:=true;
                     let p = proj_arg i in
                     p, BslTypes.opavalue typ, Some (Printf.sprintf "  let %s = %s in" p conv) (* --- *)
               )) env inputs in

          let ml_ret2 = "r2" in
          let ctrans_return = ML_CTrans.return bslkey bsltags env (BI.MetaIdent ml_ret2) in
          let more_args = ML_CTrans.more_args bslkey bsltags env in
          let more_code = ML_CTrans.more_code bslkey bsltags env in

          let () =
            if List.exists Option.is_some [ ctrans_return ; more_args ; more_code ]
            then trans_type := true
          in

          if !trans_type
          then
            begin
              let typed_args = List.map (fun (_, t, _) -> t) projected_args in
              let formel_params = List.mapi (fun i _ -> ml_arg i) inputs in
              let env, coercion = List.fold_left_map2
                (fun env a t-> ML_CTrans.runtime_ocaml_coercion ~bslkey ~bsltags t ~env a) env formel_params typed_args in
              let params =
                if inputs = [] && more_args = None then "()"
                else String.concat " " coercion in
              let more_args = Option.default "" more_args in
              let buf = FBuffer.printf buf "let %s %s %s =\n" new_impl params more_args in
              let buf = Option.fold FBuffer.addln buf more_code in

              (* projections : p_i *)
              let buf =
                let fold buf (_, _, var) = Option.fold FBuffer.addln buf var in
                List.fold_left fold buf projected_args in
              let buf =
                let projected_args = (if inputs = [] then "()" else String.concat_map " " (fun (p, _, _) -> p) projected_args) in
                let buf = FBuffer.printf buf "  let %s = %s %s in\n" ml_ret impl projected_args in
                if !trans_output then
                  let return =
                    match ctrans_return with
                    | None -> ml_ret2
                    | Some ret -> ret
                  in
                  FBuffer.addln buf (Printf.sprintf "  let %s = %s in\n  %s" ml_ret2 trans_out return)
                else
                  let return =
                    match ML_CTrans.return bslkey bsltags env (BI.MetaIdent ml_ret) with
                    | None -> ml_ret
                    | Some ret -> ret
                  in
                  FBuffer.printf buf "  %s\n" return in
              let type_of_new_impl = BslTypes.Fun (BslTypes.pos typed_out, typed_args, typed_out) in
              let buf = FBuffer.addln buf "" in
              (* we must save the new generated function in ocaml init *)
              { building with
              ml_ctrans = env ;
                  generated_ml = buf ;
              },
              Some (Implementation.String new_impl, type_of_new_impl)
            end
          else { building with ml_ctrans = env }, None

    let re_compiled_generate_js building bslkey bsltags impl (inputs : _ list option) output : building_env * (Implementation.compiler_repr * BslTypes.t) option =
      let impl = JsParse.String.expr ~globalize:true impl in
      let env = building.js_ctrans in
      if BslTags.never_projected bsltags then building, None else
      let trans_type = ref false in
      let js_arg = Printf.sprintf "x%d"
      and js_ret = "by_ret" in

      let env, ((vars_out,trans_out), typed_out) =
        let env, maped_output = JS_CTrans.qml_of_js ~bslkey ~bsltags output ~env (BI.MetaIdent js_ret) in
        env,
        (match maped_output with
         | None ->
             ([],JsCons.Expr.native js_ret), output
         | Some conv ->
             trans_type:=true;
             conv, BslTypes.opavalue output
        )
      in
      let new_impl = trans_ident bslkey in
      let unicity_index = BslKey.to_string bslkey in
      let buf = building.generated_js in
      match inputs with
      | None ->
          if !trans_type then
            let js_ret = JsCons.Ident.native js_ret in
            let local_vars = js_ret :: vars_out in
            let assign = JsCons.Expr.assign_ident js_ret impl in
            let code_elt0 =
              let jsident = JsCons.Ident.ident new_impl in
              let trans_out = JsCons.Expr.maybe_scope local_vars (JsCons.Expr.comma [assign] trans_out) in
              JsCons.Statement.var jsident ~expr:trans_out in
            let buf = (unicity_index,code_elt0) :: buf in
            let type_of_new_impl = typed_out in
            { building with
                js_ctrans = env ;
                generated_js = buf
            },
            Some (Implementation.Ident new_impl, type_of_new_impl)
          else
            { building with js_ctrans = env }, None
      | Some inputs ->
          let (local_vars,env), projected_args = List.fold_left_map_i
            (fun i (local_vars,env) typ ->
               let param = js_arg i in
               let env, meta_code = JS_CTrans.js_of_qml ~bslkey ~bsltags typ ~env (BI.MetaIdent param) in
               match meta_code with
               | None -> (local_vars, env), (JsCons.Expr.native param, typ)
               | Some (vars_out,trans) ->
                   trans_type:=true;
                   (vars_out @ local_vars, env), (trans, BslTypes.opavalue typ)
            )
            (vars_out,env) inputs in

          let more_args = JS_CTrans.more_args bslkey bsltags env in

          trans_type := !trans_type || Option.is_some more_args ;
          if !trans_type
          then
            begin
              let jsident = JsCons.Ident.ident new_impl in
              let formal_params = List.mapi (fun i _ -> JsCons.Ident.native (js_arg i)) inputs in
              let formal_params =
                Option.default_map formal_params
                  (fun more_args -> formal_params @
                     (List.map (fun x -> JsCons.Ident.native x) more_args)
                  )
                  more_args
              in
              let body =
                JsCons.Expr.call
                  ~pure:true
                  impl
                  (List.map fst projected_args)
              in
              let code_elt =
                JsCons.Statement.deprecated_function
                  jsident
                  formal_params
                  (JsCons.Ident.native js_ret :: local_vars)
                  (
                    JsCons.Expr.deprecated_letin [
                      JsCons.Ident.native js_ret,
                      body
                    ]
                      (JS_CTrans.map_result bslkey bsltags env trans_out)
                  )
              in
              let buf = (unicity_index,code_elt) :: buf in

              let type_of_new_impl =
                let args = List.map snd projected_args in
                BslTypes.Fun (BslTypes.pos typed_out, args, typed_out)
              in
              (* we must save the new generated function in js init *)
              { building with
                  js_ctrans = env ;
                  generated_js = buf
              },
              Some (Implementation.Ident new_impl, type_of_new_impl)
            end
          else { building with js_ctrans = env }, None

    (** : TODO ? add env like in CTrans *)
    let re_interpreted_generate stdtyp obj =
      match ML_ITrans.qml_of_ocaml stdtyp with
      | None -> obj
      | Some trans -> Obj.repr (trans (Obj.obj obj))

    (** building functions API *)

    let build_types () = let fold _ typ accu = typ::accu in SortHashtbl.fold_right fold _types_table []
    let build_typesmap () = SortHashtbl.fold_right BslKeyMap.add _types_table BslKeyMap.empty
    let build_language_init building =
      let ml_ctrans_env, ml_code = ML_CTrans.conversion_code building.ml_ctrans in
      let js_ctrans_env, js_code = JS_CTrans.conversion_code building.js_ctrans in
      let ocaml_init = ml_code ^ "\n" ^ FBuffer.contents building.generated_ml in
      let js_init = js_code @ List.rev building.generated_js in
      { building with
          ml_ctrans = ml_ctrans_env ;
          js_ctrans = js_ctrans_env
      },
      ocaml_init, js_init
      (* continue with other language when it will be necessary *)

    open Implementation

    let build_bypass_map
        ?(ml_ctrans=ML_CTrans.empty ())
        ?(js_ctrans=JS_CTrans.empty ())
        ?filter () =
      let filter = Option.default (fun _ -> true) filter in
      let types = build_types () and typesmap = build_typesmap () in
      let building =
        {
          ml_ctrans = ml_ctrans ;
          js_ctrans = js_ctrans ;
          generated_ml = FBuffer.create 1024 ;
          generated_js = [];
        } in
      let building, map =
        let fold_bypass key bypass ((building, map) as env) =
          if not (filter bypass) then env
          else
            let input, output =
              match bypass.ByPass.def_type with
              | BslTypes.Fun (_, input, output) -> Some input, output
              | t -> None, t
            in
            let fold_map building impl =
              match impl with
              | Compiled compiled ->
                  begin
                    let function_name =
                      match compiled.compiler_repr with
                      | Implementation.String s -> s
                      | Implementation.Ident _ -> assert false
                    in
                    let re_compiled_generate =
                      match compiled.c_lang with
                      | ml when BslLanguage.compare ml BslLanguage.ml = 0 -> re_compiled_generate_ml
                      | js when BslLanguage.compare js BslLanguage.js = 0
                          || BslLanguage.compare js BslLanguage.nodejs = 0
                          -> re_compiled_generate_js
                      | _ -> (fun building _key _bsltags _fct _input _output -> building, None)
                        (** with llvmtrans, do the same with CCTrans MLCTrans, in arg of the functor MakeLibBSL etc... *)
                    in
                    let bsltags = bsltags impl in
                    let building, transtype = re_compiled_generate building key bsltags function_name input output in
                    building, (
                      match transtype with
                      | None -> impl
                      | Some (new_impl, qmltyp) ->
                          let compiled = { compiled with compiler_repr = new_impl ; c_type = qmltyp ; is_transtype = true } in
                          Compiled compiled
                    )
                  end
              | Interpreted interpreted ->
                  begin
                    match interpreted.i_lang with
                    | mli when BslLanguage.compare mli BslLanguage.mli = 0 ->
                        if ML_ITrans.auto_transtype
                        then (* "YAGNI(or todo) auto_transtype in libBSL in interpreted-mode" *) assert false
                        else (building, impl)
                    | _ -> assert false
                  end
            in
            let building, impl = List.fold_left_map fold_map building bypass.ByPass.impl in
            let bypass = { bypass with ByPass.impl = impl } in
            building, BslKeyMap.add key bypass map
        in
        let sorted_bypass = Hashtbl.fold BslKeyMap.add _bypass_table BslKeyMap.empty in
        BslKeyMap.fold fold_bypass sorted_bypass (building, BslKeyMap.empty) in
      let building, ocaml_init, js_init = build_language_init building in
      let root =
        let rec from_mod _mod =
          let unsorted =
            let fold name h accu =
              match from_kind name h with
              | None -> accu
              | Some t -> t::accu in
            Hashtbl.fold fold _mod [] in
          List.sort Pervasives.compare unsorted
        and from_kind name = function
          | BI.Function s ->
              ( try
                  let by = Hashtbl.find _bypass_table s in
                  if filter by then Some (name, BI.Function by) else None
                with
                | Not_found ->
                    OManager.printf "Assertion Failure@\nBslKey %S is not found during building@\n%!" (BslKey.to_string s);
                    assert false
              )
          | BI.Module a -> Some (name, BI.Module (from_mod a))
        in from_mod _imperativ_module_table
      in
      { elt_root = None; types=types; typesmap = typesmap; map=map; ocaml_init=ocaml_init; js_init=js_init; root_node=root ;
        ml_ctrans_env = building.ml_ctrans ; js_ctrans_env = building.js_ctrans }

    let build_restrict_map_any ?ml_ctrans ?js_ctrans ?(filter=fun _ -> true) ~lang () =
      build_bypass_map ?ml_ctrans ?js_ctrans ~filter:(fun t -> filter (ByPass.key t) && ByPass.implemented_in_any t ~lang) ()
    let build_restrict_map_all ?ml_ctrans ?js_ctrans ?(filter=fun _ -> true) ~lang () =
      build_bypass_map ?ml_ctrans ?js_ctrans ~filter:(fun t -> filter (ByPass.key t) && ByPass.implemented_in_all t ~lang) ()
  end

  (* The RegisterInterface must not be used by user; It is provided to talk with generated code only *)
  module RegisterInterface =
  struct
    open BslTypes
    open Implementation
    module MultiLoading =
    struct
      let is_loaded = Hashtbl.mem _loaded
      let first_load load = Hashtbl.add _loaded load ()
    end
      (* Error : the error is abstract, so, you won't need to write a long try with. If you really need to catch this exception, you can print the error with string_of_error *)
    type error =
      | UnknownLang of string
      | UnknownExt of string
      | DefinitionError of BslTypes.t
      | NotRegistredType of (string * BslTypes.t)
      | RegisterFailure of string
      | MultiImpl of BslLanguage.t * BslKey.t
      | FileFun of (string * string)
      | ExtensionLangClash of BslLanguage.t * BslLanguage.t
      | FailureKeyType of (BslKey.t * BslTypes.t)
      | PathFunctionOnModule of (string * BslKey.t)
      | PathFunctionOnFunction of (string * BslKey.t)
      | PathModuleOnFunction of (string * BslKey.t)
      | List of error list
      | TypeClash of BslTypes.t * BslTypes.t
      | Redefinition of string
      | TypeRedefinition of BslTypes.t * BslTypes.t
      | BslTagsError of BslTags.error
    exception RegisterError of error

    (*
      Guidelines about error reporting:
      Since this is not a fatal error, the people which will catch the exception
      will already print a citation.
      The pp_error should only print some precise context.
    *)
    let pp_error fmt err =
      let rec aux fmt error =
        let (!!) s = Format.fprintf fmt s in
        match error with
        | UnknownLang lang ->
            !! "unknown lang @{<bright>%S@}" lang

        | UnknownExt ext ->
            !! "unknown extenstion @{<bright>%S@}" ext

        | DefinitionError _ ->
            !! "You cannot define such a type"

        | NotRegistredType (skey, typ) ->
            !! "In: %a@\nThe type with key:@{<bright>%S@} is not registred" BslTypes.pp typ skey

        | RegisterFailure f -> Format.pp_print_string fmt f

        | FileFun (file, fct) ->
            !! "Register of %S from %S error" fct file

        | MultiImpl (lang, key) ->
            !! "Bypass with key @{<bright>%a@} has@ more than one implementation in @{<bright>%a@}"
              BslKey.pp key BslLanguage.pp lang

        | ExtensionLangClash (ext, lang) ->
            !!  "File of extension @{<bright>%a@} with lang @{<bright>%a@} ?"
              BslLanguage.pp ext BslLanguage.pp lang

        | TypeClash (t1, t2) ->
            !! "Type clash between type @{<bright>%a@}@ and type @{<bright>%a@}"
              BslTypes.pp t1 BslTypes.pp t2

        | FailureKeyType (key, _) ->
            !! "Error by registering key:@{<bright>%a@}" BslKey.pp key

        | PathFunctionOnModule (fct, key) ->
            !! "Invalid module path@\nCannot define function @{<bright>%s@}@ with key:@{<bright>%a@}@\nThere is a module here with the same name"
              fct BslKey.pp key

        | PathFunctionOnFunction (fct, key) ->
            !! "module path @{<bright>%S@} already binded@\nCannot register function @{<bright>%a@} there"
              fct BslKey.pp key

        | PathModuleOnFunction (m, key) ->
            !! "Invalid module path,@ cannot create module @{<bright>%s@},@ the parent has a function with the same name : @{<bright>%a@}"
              m BslKey.pp key

        | List err ->
            Format.pp_list "@\n" aux fmt err

        | Redefinition infos ->
            !! "This key is already binded in the register table :@\n%s" infos

        | TypeRedefinition (t1, t2) ->
            !! "Multiple type definition@\n%a AND %a@\n" BslTypes.pp_citation t1 BslTypes.pp_citation t2

        | BslTagsError e -> BslTags.pp_error fmt e
      in aux fmt err
    let error err = raise (RegisterError err)
    (* inspection of type during the registering *)
    let rec inspection_register_type ?(definition=false) ?(ml_runtime="") ?(path=[]) typ =
      match typ with
      | External (_, skey, par) ->
          List.iter inspection_register_type par;
          let key = BslKey.normalize skey in
          begin
            match SortHashtbl.find_opt _types_table key with
            | None ->
                if definition then SortHashtbl.add _types_table key (ml_runtime, path, typ)
                else raise (RegisterError (NotRegistredType (skey, typ)))
            | Some (_, _, ((External (_, _, par2)) as typ2)) ->
                let arity = List.length par in
                let arity2 = List.length par2 in
                if arity <> arity2
                then raise (RegisterError (TypeClash (typ, typ2)))
                else ()
            | Some (_, _, typ2) -> raise (RegisterError (TypeClash (typ, typ2)))
          end
      | t ->
          if definition then raise (RegisterError (DefinitionError t))
          else (
            BslTypes.Walk.iter_nonrec inspection_register_type t
          )

    (* Registration of hierarchy *)
    let current_plugin_name = ref ""

    let register_imperativ_hierarchy mod_ link (fct, key) =
      let rec aux mod_ = function
        | [] ->
            begin
              try
                begin
                  match Hashtbl.find mod_ fct with
                  | BI.Module _  -> raise (RegisterError (PathFunctionOnModule (fct, key)))
                  | BI.Function _ -> raise (RegisterError (PathFunctionOnFunction (fct, key)))
                end
              with
              | Not_found -> Hashtbl.add mod_ fct (BI.Function key)
            end
        | t::q ->
            (* must find if there is also a module named t in mod_ *)
            begin
              try
                begin
                  match Hashtbl.find mod_ t with
                  | BI.Module m -> aux m q
                  | BI.Function key -> raise (RegisterError (PathModuleOnFunction (t, key)))
                end
              with
              | Not_found ->
                  let new_mod = Hashtbl.create 10 in
                  Hashtbl.replace mod_ t (BI.Module new_mod);
                  aux new_mod q
            end
      in aux mod_ link

    let unsafe_register_primitive ~ks ~ty ~ips ?obj () =
      (* FIXME: this was a quick backward, clean-this up *)
      (* BEGIN OF HACK *)
      (*
        Explanation about the hack:
        In bsl-v1, the following code (after the hack) was expected
        unparsed bsltags, where now, bslregister apply the parse
        function of BslTags.
        For limiting the changes of the following code, we recreate
        the unparsed type from the parsed type in bslregister,
        and reparsing there.
        This should be corrected, for cleaning the code.
        It has no impact on performance, because tags are very small.
      *)
      let module_ =
        match List.rev_map BslKey.normalize_string ks with
        | [] -> assert false
        | last::tl -> Some (List.rev tl, last)
      in
      let type_ = ty in
      let impl = ips in
      let do_obj = true in (* remove if not needed *)
      let skey = String.concat_map "_" BslKey.normalize_string ks in
      let key = BslKey.normalize skey in
      (* END OF HACK *)
      let ml_dirtags = ref BslTags.default in
      let fold_impl (accu, langs) (lang, file, dirtags, impl_fun) =
        let dirtags =
          try
            BslTags.parse dirtags
          with
          | BslTags.Exception e -> error (List [BslTagsError e; FailureKeyType (key, type_)])
        in
        (if lang = BslLanguage.ml then ml_dirtags := dirtags);
        if List.mem lang langs
        then error (List [MultiImpl (lang, key); FileFun (file, impl_fun)])
        else
          begin
            let ext = File.extension file in
            (
              match BslLanguage.of_string ext with
              | None -> error (List [UnknownExt ext; FileFun (file, impl_fun)])
              | Some ext ->
                  if BslLanguage.compare lang ext <> 0
                  then error (List [ExtensionLangClash (ext, lang); FileFun (file, impl_fun)])
                  else ()
            );
            let module_name = String.capitalize (Filename.chop_extension file) in
            (** the auto transtype is done @ building time of the bymap *)
            let new_imp = Compiled
              {
                c_lang = lang ; c_type = type_ ; c_tags = dirtags ; compiler_repr = Implementation.String impl_fun;
                origin_file = file ; is_transtype = false ; module_name = module_name
              } in
            (new_imp::accu, lang::langs)
          end
      in
      (* we check the definition typ and we register it - in the same time, we register in the module table *)
      (* first : do the implementation (because, on error, the table will include wrong keys *)
      let maped_impl = fst (List.fold_left fold_impl ([], []) impl) in
      let maped_impl =
        match do_obj, obj with
        | false, _
        | _, None -> maped_impl
        | _, Some obj ->
            let interpretedML = { i_lang = BslLanguage.mli; obj = obj; i_type = type_ ; i_tags = !ml_dirtags } in
            (Interpreted interpretedML)::maped_impl in
      let name =
        (
          let link, short_key =
            match module_ with
            | None -> [], skey
            | Some (lk, sk) -> lk, sk
          in
          let strlink = String.concat "." link in
          let infos = Format.sprintf "link=%S@ key=%a@ fun=%S@ type=%a" strlink BslKey.pp key short_key BslTypes.pp type_ in
          (* #<< debug browserstructure (Printf.sprintf "imperativ_module_table : %s" infos); >>#; *)
          (* If there is a previous binding of this key, we will produce an error *)
          (
            if Hashtbl.mem _bypass_table key
            then error (Redefinition infos)
            else ()
          );
          try
            inspection_register_type type_ ;
            register_imperativ_hierarchy _imperativ_module_table link (short_key, key);
            short_key
          with
          | RegisterError e -> error (List [e; FailureKeyType (key, type_)])
        ) in
      (* We add the register in the table *)
      let bypass = { ByPass.
        key      = key;
        name     = name;
        def_type = type_;
        impl     = maped_impl ;
        plugin_name = !current_plugin_name ;
      } in
      Hashtbl.add _bypass_table key bypass

    let unsafe_register_type ~ks ~ty =
      inspection_register_type ~definition:true ~path:ks ty

    let register ~uniq_id ~plugin_name =
      if MultiLoading.is_loaded uniq_id then None else (
        MultiLoading.first_load uniq_id;
        current_plugin_name := plugin_name ;
        Some { BslPluginInterface.
          register_primitive = unsafe_register_primitive ;
          register_type      = unsafe_register_type ;
        }
      )

    let dynload loader_dynload = loader_dynload register
    let dynload_no_obj = dynload
  end
end

(* warning : some function will be called, do not put assert false everywhere *)
module Dummy_ML_ITrans : BI.ML_ITRANS =
struct
  let auto_transtype = false
  let record_clash _ _ = assert false
  let unbound_record _ = assert false
  type qml = unit
  let qml_of_ocaml _ = None
  let ocaml_of_qml _ = None
  let type_of_record _ = assert false
  let get_field _ _ = assert false
  let get_field_opt _ _ = assert false
  let build_record _ _ = assert false
end
module Dummy_ML_CTrans : BI.ML_CTRANS =
struct
  type env = unit
  let empty () = ()
  let qml_of_ocaml ~bslkey:_ ~bsltags:_ _ ~env  _  = env, None
  let ocaml_of_qml ~bslkey:_ ~bsltags:_ _ ~env  _  = env, None
  let runtime_ocaml_coercion ~bslkey:_ ~bsltags:_ _ ~env s = env, s
  let conversion_code env = env, "(* dummy code (bslLib.ml) ! *)"
  let more_args _ _ _ = None
  let more_code _ _ _ = None
  let return _ _ _ _ = None
end
module Dummy_JS_CTrans : BI.JS_CTRANS =
struct
  type env = unit
  let empty () = ()
  let qml_of_js ~bslkey:_ ~bsltags:_ _ ~env _ = env, None
  let js_of_qml ~bslkey:_ ~bsltags:_ _ ~env _ = env, None
  let conversion_code env = env, ["",JsCons.Statement.comment "/* dummy code (bslLib.ml)! */"]
  let more_args _ _ _ = None
  let map_result _ _ _ e = e
end

module LibBSLForQml2Ocaml (ML_CTrans : BI.ML_CTRANS) : BI.BSLINTROSPECTION with type ml_ctrans_env = ML_CTrans.env =
  MakeLibBSL (Dummy_ML_ITrans) (ML_CTrans) (Dummy_JS_CTrans)
module LibBSLForQml2Js (JS_CTrans : BI.JS_CTRANS) : BI.BSLINTROSPECTION with type js_ctrans_env = JS_CTrans.env =
  MakeLibBSL (Dummy_ML_ITrans) (Dummy_ML_CTrans) (JS_CTrans)
module LibBSLForQmlTopLevel (ML_ITrans : BI.ML_ITRANS) : BI.BSLINTROSPECTION =
  MakeLibBSL (ML_ITrans) (Dummy_ML_CTrans) (Dummy_JS_CTrans)
module BSL : BI.BSLINTROSPECTION =
  MakeLibBSL (Dummy_ML_ITrans) (Dummy_ML_CTrans) (Dummy_JS_CTrans)
type env_bsl = {
  bymap : BSL.ByPassMap.t ;
  all_plugins : BslPluginInterface.plugin list ;
  direct_plugins : BslPluginInterface.plugin list ;
}
(* ========================================= *)

(** The option complete is used because some time, we need to prefix all the module hierarchy before a field
    module Toto =
    .. module Titi =
    ....type t = { a : int }

    let ( q : Toto.Titi.t ) = .. in
    q.Toto.Titi.a *)
let record_path_map_of_typesmap ?(complete=false) ?(runtime=false) typesmap =
  let rec aux accu = function
    | [] -> ""
    | [n] -> if complete then accu^n else accu
    | t::q -> aux (Printf.sprintf "%s%s." accu (String.capitalize t)) q in
  BslKeyMap.fold (fun key (mlruntime, link, _) acc ->
                    let link = if runtime then mlruntime::link else link in
                    StringMap.add (BslKey.to_string key) (aux "" link) acc) typesmap StringMap.empty



let ml_function_projection ~inputs:aux ~outputs:aux2 inputs output (BI.MetaIdent id) =
  let trans = ref false in
(*  let len = List.length inputs in *)
  let args = List.map (fun _ -> Ident.stident (Ident.next "y")) inputs in
  let ml_ret = "r" in
  let proj x typ =
    match aux typ (BI.MetaIdent x) with
    | Some (BI.MetaCode conv) -> trans := true ; Printf.sprintf "(%s)" conv
    | Some (BI.MetaComment comment) -> Printf.sprintf "( %s (* : %s *) )" x comment
    | None -> x in
  let proj_output =
    match aux2 output (BI.MetaIdent ml_ret) with
    | Some (BI.MetaCode conv) -> trans := true ; conv
    | Some (BI.MetaComment comment) -> Printf.sprintf "( %s (* : %s *) )" ml_ret comment
    | None -> ml_ret in
  let call = List.map2 proj args inputs in
  let metacode = Printf.sprintf "let f' %s =\n  let %s = %s %s in\n  %s in\nf'"
    (if args = [] then "()" else String.concat " " args) ml_ret id (if call = [] then "()" else String.concat " " call) proj_output in
  if !trans then Some (BI.MetaCode metacode) else None


let js_function_projection ~inputs:aux ~outputs:aux2 inputs output (BI.MetaIdent id) =
  let trans = ref false in
  let args = List.map (fun _ -> Ident.stident (Ident.next "y")) inputs in
  let js_ret = "r" in
  let proj x typ =
    match aux typ (BI.MetaIdent x) with
    | Some (BI.MetaCode conv) -> trans := true; Printf.sprintf "(%s)" conv
    | Some (BI.MetaComment comment) -> Printf.sprintf "(%s /* : %s */)" x comment
    | None -> x in
  let proj_output =
    match aux2 output (BI.MetaIdent js_ret) with
    | Some (BI.MetaCode conv )-> trans := true; conv
    | Some (BI.MetaComment comment) -> Printf.sprintf "(%s /* : %s*/)" js_ret comment
    | None -> js_ret in
  let call = List.map2 proj args inputs in
  let metacode = Printf.sprintf "(function (%s) {\n  var %s = %s(%s);\n  return %s;\n})"
    (String.concat "," args) js_ret id (String.concat "," call) proj_output in
  if !trans then Some (BI.MetaCode metacode) else None
