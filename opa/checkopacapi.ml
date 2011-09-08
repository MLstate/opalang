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
   Checking consistency between opabsl, stdlib et opacapi.
   @author Mathieu Barbin
*)

(**
   OPA COMPILER INTERFACE:

   The OPA compiler needs to insert some call to identifiers and external primitives
   defined in the stdlib, and in the opabsl. The set of these identifers and primitives
   is called : opa-compiler-interface.

   We want to be able, by reading the definition of a bypass, or by reading the definition
   of a identifier in the stdlib, to know if it is part of this compiler interface.
   It is necessary to have a better control on dependency between the stdlib, and the compiler.
   (consistency, maintenance, etc.)

   For that purpose, we decorate the opa-compiler-interface in the stdlib and opabsl,
   and we use opacapi in the compiler.

   STDLIB:
   {[
   @opacapi myfunction()=....
   ]}

   OPABSL:
   {[
   ##register [opacapi] mybypass : ...
   ]}

   OPACAPI:
   {[
   mybypass = "mybypass"
   myfunction = "myfunction"
   ]}

   COMPILER:
   {[
   let myfunction = Opacapi.myfunction in
   ...
   ]}

   This check will ensure that :
   -the set of all identifier defined in opacapi is strictly equal to the set of identifiers
   decorated with the directive [@opacapi] in the stdlib,
   -the set of all bypass defined in opacapi is strictly equal to the set of bypasses
   decorated with the property [opacapi] in the opabsl

   If a bypass is not in opacapi, nor present in the stdlib, there will be a notification.
   Since it is not yet clear what we want to do with such cases, currently a warning
   is printed.

   A file is generated indicating for each bsl-key the list of location where it is used.
*)

(* depends *)
module Arg = Base.Arg
module Format = Base.Format

(* shorthand *)
module BPI = BslPluginInterface
module D = SurfaceAstDecons
module Q = QmlAst
module SA = SurfaceAst

(* -- *)

let validation_ok = ref true

(* f *)

let files = MutableList.create ()

(* t *)

let target = ref None

let (|>) = InfixOperator.(|>)
let (!>) = Format.sprintf

let spec = [

(* o *)
  "-o",
  Arg.String (fun t -> target := Some t),
  !>
    " specify a target for the trace file"

]

let anon_fun file = MutableList.add files file

let usage_msg =
  !> "@{<bright>%s@} <Opa Compiler Interface Validator> %s\nuse: %s [options] stdlib-files"
    Sys.argv.(0) BuildInfos.version_id
    Sys.argv.(0)

let parse () =
  let spec = (
    (OManager.Arg.version "checkopacapi" :: OManager.Arg.options) @
    spec
  )

  |> Arg.add_bash_completion
  |> Arg.sort
  |> Arg.align

  in
  Arg.parse spec anon_fun usage_msg

(**
   Folding input file, and applying a fold on the parsed code
*)

(**
   pplib specification
*)
let pplib_spec = [
  "OPA_VERSION", "S3" ;
  "OPA_CPS", "" ;
  "OPA_BADOP", "1" ;
]

let pprocess =
  let ppenv = Pprocess.fill_with_sysenv Pprocess.empty_env in
  let ppenv = List.fold_left (fun ppenv (var, value) -> Pprocess.add_env var value ppenv) ppenv pplib_spec in
  let ppopt = Pprocess.default_options ppenv in
  (fun s -> Pprocess.process Pplang.opa_description ppopt s)

let fold
    ( fold : (SurfaceAst.nonuid, SurfaceAst.parsing_directive) SurfaceAst.code -> 'acc -> 'acc )
    filename acc =
  (* print_endline filename; *)
  match File.content_opt filename with
  | None ->
      OManager.error "[!] I/O error: cannot read file @{<bright>%S@}" filename
  | Some content ->
      let content = pprocess content in
      let code = OpaParser.code ~cache:true ~filename content in
      fold code acc

(**
   Specialized fold, for gathering opacapi directives
*)
let is_opacapi e =
  D.Look.at
    ~through:[
      D.Remove.Basic.access_directive ;
      D.Remove.Basic.expand ;
      D.Remove.Basic.coerce ;
      D.Remove.Basic.deprecated ;
      D.Remove.Basic.magic_directive ;
      D.Remove.Basic.slicer_directive ;
    ]
    ~at:[D.Remove.Basic.opacapi]
    e

let stdlib acc code =
  let fold_elt acc (elt, _) =
    match elt with
    | SA.NewVal (binds, _) ->
        let fold acc ((p, _), e) =
          match p with
          | SA.PatVar ident | SA.PatAs (_, ident) ->
              if is_opacapi e
              then StringSet.add ident.SA.ident acc
              else acc
          | _ -> acc
        in
        List.fold_left fold acc binds
    | SA.NewType typedefs ->
        let fold acc (typedef, _) =
          if typedef.SA.ty_def_options.Q.opacapi
          then
            let (SA.Typeident ident) = typedef.SA.ty_def_name in
            StringSet.add ident acc
          else
            acc
        in
        List.fold_left fold acc typedefs
    | _ -> acc
  in
  List.fold_left fold_elt acc code

(**
   Check strict equality between 2 StringSet, with errors reporting.
*)
let report elt name present absent =
  validation_ok := false ;
  OManager.printf (
    "[!] The %s @{<bright>%s@} is present in @{<bright>%s@} but not in @{<bright>%s@}@."
  )
    elt name present absent

let strict_equality elt name1 name2 set1 set2 =
  let iter name1 name2 set1 set2 =
    let error name = report elt name name1 name2 in
    StringSet.iter (fun s -> if not (StringSet.mem s set2) then error s) set1
  in
  iter name1 name2 set1 set2;
  iter name2 name1 set2 set1

module LocMap = ListMap.Make(BslKey)


let core_types =
  [
    "char";
    "float";
    "int";
    "OPA.Init.value";
    "string";
    "tuple_2";
    "void";
  ]

let _ =
  (* Part 1: bsl VS opacapi *)
  OpabslgenPlugin.Self.self_store ();
  parse ();
  let plugins = BslPluginTable.finalize () in
  let module B = BslLib.BSL in
  List.iter (fun loader -> B.RegisterInterface.dynload loader.BPI.dynloader) plugins;
  let bymap = B.RegisterTable.build_bypass_map () in
  let opabsl =
    let fold key bypass acc =
      let fold acc impl =
        let tags = B.Implementation.bsltags impl in
        if tags.BslTags.opacapi
        then StringSet.add (BslKey.to_string key) acc
        else acc
      in
      let impls = B.ByPass.all_implementations bypass in
      List.fold_left fold acc impls
    in
    B.ByPassMap.fold fold bymap StringSet.empty
  in

  let opacapi =
    Hashtbl.fold
      (fun key _ acc -> StringSet.add (BslKey.to_string key) acc)
      Opacapi.Opabsl.table StringSet.empty in
  strict_equality "bypass" "opabsl" "opacapi" opabsl opacapi;
  (* Get the code of stdlib *)
  let codes = MutableList.fold_right (fold (fun hd tl -> hd::tl)) files [] in
  (* Part 2: stdlib VS opacapi *)
  let stdlib = List.fold_left stdlib StringSet.empty codes in
  let stdlib = List.fold_left (fun acc ident -> StringSet.add ident acc) stdlib core_types  in
  (* THIS LINE IS AN HACK - SHOW COMMIT WHERE I AM INTRODUCES*)
  let stdlib = StringSet.add "``" stdlib in
  let opacapi =
    Hashtbl.fold
      (fun ident _ acc -> StringSet.add ident acc)
      Opacapi.table StringSet.empty in
  strict_equality "ident" "stdlib" "opacapi" stdlib opacapi;
  if not !validation_ok then exit 1;
  (* Part 3: collect position where bypass are used *)
  let opacapi_pos = FilePos.nopos "opacapi" in
  let locmap = LocMap.empty in
  let locmap =
    Hashtbl.fold
      (fun key _ acc -> LocMap.append key opacapi_pos acc)
      Opacapi.Opabsl.table locmap in
  let locmap =
    let fold acc (e, annot) =
      match e with
      | SA.Bypass key ->
          let pos = QmlLoc.pos annot in
          LocMap.append key pos acc
      | _ -> acc
    in
    let fold = OpaWalk.Code.fold fold in
    List.fold_left fold locmap codes
  in
  (* Part 4: generate the log *)
  let oc, close_out =
    match !target with
    | Some file -> (
        try
          Pervasives.open_out file
        with
        | Sys_error s ->
            OManager.error "cannot open_out %s : %s" file s
      ), Pervasives.close_out_noerr

    | None ->
        Pervasives.stdout, (fun _ -> ())
  in
  let fmt = Format.formatter_of_out_channel oc in
  let iter key locs =
    Format.fprintf fmt "@[<2>%a:@\n%a@]@\n"
      BslKey.pp key (Format.pp_list "@\n" FilePos.pp_pos) locs
  in
  LocMap.iter iter locmap;
  (* Part 5: notify bypass which are not used *)
  let both_unused = ref [] in
  let client_unused = ref [] in
  let server_unused = ref [] in
  let all_bypass = ref 0 in
  let unused_bypass = ref 0 in
  let iter key bypass =
    incr(all_bypass);
    match LocMap.find_opt key locmap with
    | Some [] | None ->
        (* This means that a bypass is registred, but never used in the stdlib. *)
        let table =
          if B.ByPass.implemented_in bypass ~lang:BslLanguage.js
          then
            if B.ByPass.implemented_in bypass ~lang:BslLanguage.ml
            then both_unused
            else client_unused
          else
            if B.ByPass.implemented_in bypass ~lang:BslLanguage.ml
            then server_unused
            else (
              OManager.printf "checkopacapi: %a@\n" BslKey.pp key ;
              assert false
          )
        in
        incr(unused_bypass);
        table := key :: !table
    | _ -> ()
  in
  B.ByPassMap.iter iter bymap ;
  let notify kind table =
    let num = ref 0 in
    let list = List.rev_map (fun t -> incr(num); t) !table in
    if !num  > 0
    then (
      Format.fprintf fmt (
        "Warning: the %d following bypass implemented @@%s are unused:@\n"
      ) !num kind;
      List.iter (fun key -> Format.fprintf fmt "%a@\n" BslKey.pp key) list
    );
    !num
  in
  let num_both = notify "both" both_unused in
  let num_client = notify "client" client_unused in
  let num_server = notify "server" server_unused in
  let percent =
    (float_of_int !unused_bypass) /. (float_of_int !all_bypass) *. 100.
  in
  Format.pp_print_flush fmt ();
  close_out oc;
  if !unused_bypass > 0 then
    OManager.printf (
      "@{<yellow>Warning@}: opabsl contains %f %% of unused bypass: (%d / %d)@\n"^^
      "  %d both@\n"^^
      "  %d client@\n"^^
      "  %d server@\n"^^
      "cf file @{<bright>opacapi.validation@} for details@\n"
    )
      percent
      !unused_bypass
      !all_bypass
      num_both
      num_client
      num_server
  ;
  exit 0
