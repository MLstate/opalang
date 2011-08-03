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
(*
  TODO:
  use positions for error reporting, update for Format instead of string concatenation.
  Optimize ExtendRecord followed by Record
  Handle pattern-matching for the "rest" of a record
  optimize access/pattern-matching when type is statically known
  share vtable when type is statically fully known even after record extension
*)

(* depends *)
module Format = Base.Format

(* refactoring in progress *)

(* alias *)
module FCons = Flat_Common.FCons
module ServerLib = Flat_Common.ServerLib

(* shorthands *)
module E = Flat_Env
module P = Qml2ocamlOptions
module Q = QmlAst

(* -- *)

(* type alias *)
type label = Flat_Common.label

(* field registration *)
let register_field_name label =
  let _ = Flat_Shared.field label in
  ()

let compile_bindings env ~recursive:_ bindings =
  let rec aux acc = function
    | [] -> List.rev acc
    | (id, expr)::tl ->
        let expr = Flat_ExprGeneration.expr env expr in
        (* A hack to overcome "variables that cannot be generalized". *)
        let expr =
          if env.E.options.P.top_magic && ( not ( DebugVariables.default DebugVariables.qmlc_no_magic )) then
            match expr with
            | Ocaml.Abs _
            | Ocaml.Function _
            | Ocaml.Var _ -> expr
            | _ -> Ocaml.make_magic expr
          else expr
        in
        aux ((FCons.param id, expr)::acc) tl
  in
  aux [] bindings

let compile_elt_aux env ~recursive make_Val bindings =
  let bindings = compile_bindings env ~recursive bindings in
  make_Val bindings

(*
  We build a list for each val, so that we can insert toplevel definition between
  ocamltoplevel values if we want.
*)
let compile_elt env ocaml_code elt =
  let code =
    match elt with
    | Q.NewVal (_, bindings)    ->
        let val_ = compile_elt_aux env ~recursive:false Ocaml.make_Letand bindings in
        [val_]
    | Q.NewValRec (_, bindings) ->
        let val_ = compile_elt_aux env ~recursive:true Ocaml.make_Letrecand bindings in
        [val_]
    | _ -> []
  in
  code :: ocaml_code

(*
  FIXME
    do not take ocaml_code in argument
    simplify interface, no more returned env (unused)
*)
let compile (env, _) code =
  (* imperative initialization *)
  (* QmlPatternAnalysis.QmlOnion.typer_env_initialize env.typing ; *)

  (* compilation *)
  let code = List.fold_left (compile_elt env) [] code in

  (* finalization, insertion of generated shared values *)
  let code =
     let fold acc definitions = List.rev_append definitions acc in
    let code = List.fold_left fold [] code in
    let code = Flat_Shared.Let.insert code in
    code
  in

  (* compositionnality *)

  (* dispose, reset *)
  let () =
    Flat_Bsl.reset () ;
    Flat_Field.reset () ;
    Flat_VTable.reset () ;
    Flat_Simple.reset () ;
    Flat_Shared.reset () ;
    (* TODO: QmlPatternAnalysis dispose *)
    ()
  in

  env, code

(* FIXME: cf remark for simplifying the interface of backend *)
let empty_code = []
let get_code code = code

(*
  Building : now : build from the table
  which should be full because of a previous dynloading
  todo : use a export/import feature from bsl
*)
let build_bymap ?filter options bsl =
  let typesmap = BslLib.BSL.ByPassMap.typesmap bsl in
  let ml_ctrans = Flat_Bsl.build_ctrans_env ~typesmap options in
  Flat_Bsl.FlatBSL.RegisterTable.build_restrict_map_all ~ml_ctrans ?filter ~lang:[BslLanguage.ml] ()

let ocaml_init bymap =
  Flat_Bsl.FlatBSL.ByPassMap.ocaml_init bymap

let env_initial = Flat_Env.initial

let back_end_factory :
    (Flat_Bsl.FlatBSL.ByPassMap.t, Flat_Env.env, Ocaml.code) Qml2ocaml.back_end_factory
    = { Qml2ocaml.
    build_bymap = build_bymap ;
    ocaml_init = ocaml_init ;
    env_initial = env_initial ;
    empty_code = empty_code ;
    compile = compile ;
    get_code = get_code
 }

let dynloader plugin =
  Flat_Bsl.FlatBSL.RegisterInterface.dynload plugin.BslPluginInterface.dynloader

let qml_to_ocaml options bsl blender =
  Qml2ocaml.qml_to_ocaml_factory back_end_factory options bsl blender

let back_end = { Qml2ocaml.
    dynloader = dynloader ;
    qml_to_ocaml = qml_to_ocaml ;
  }

(* options *)
module Arg =
struct
  let options = Flat_Options.options
end

(* warnings *)
let warning_set = Flat_Warnings.warning_set
