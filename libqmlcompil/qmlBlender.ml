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
   Authors
   2009, Louis Gesbert            <Louis.Gesbert@mlstate.com>
   2009, Mathieu Barbin           <Mathieu.Barbin@mlstate.com>
*)

(* depends *)
module String = BaseString
module List = BaseList

(* shorthands *)
module QT = QmlTypes

(** The type returned by DbGenBlender, from scratch *)
type qml_milkshake =
    {
      (** contains gamma, schema, annotmap, etc.. *)
      env : QmlTyper.env;

      (** code of the program *)
      code : QmlAst.code;
    }

module HighTyper = QmlTyper.DynamicallyChangeableTyper.HighTyper

  (* ------------------------------------------------------------ *)
  (* Initial Blender Part 1 (just to register type definitions)   *)
  (* ------------------------------------------------------------ *)
  let blend_initial_part1 env code =

    (* 1°: sorting things out *)
    let code_defs, code_dbfiles, code_dbdefs, code =
      (* verbose "I-1) Sorting top-level nodes"; *)
      let sort_user = QmlAstSort.add QmlAstSort.empty code in
      let code_defs = QmlAstSort.Get.new_type sort_user
      and code_dbfiles = QmlAstSort.Get.database sort_user
      and code_dbdefs = QmlAstSort.Get.new_db_value sort_user
      and user_code = QmlAstSort.Get.new_val sort_user
      in code_defs, code_dbfiles, code_dbdefs, user_code
    in

    assert (code_dbfiles = []);
    assert (code_dbdefs = []);

    (* pre-2: dependency analysis on type definitions *)
    QT.check_no_duplicate_type_defs code_defs;

    (* 2°: getting type definitions into Gamma *)
    let env =
      (* verbose "I-2) registering type definitions"; *)
      HighTyper.fold env code_defs
        (* at this point, local type definitions are in the global
           environnement, but abstract, and when the typer encounters the
           corresponding LetTypeIn, it will have the effect of an "open" *)
    in
    { env = env; code = code }

  (* ------------------------------------------------------------ *)
  (* Initial Blender Part 2 (DB, type all code)                   *)
  (* ------------------------------------------------------------ *)

  (* Does the first part of the job: alphaconv, preprocess & type *)
  let blend_initial_part2 milkshake =

    let { env ; code } = milkshake in

    (* 5°: alpha-conversion *)
    let code =
      let alpha = QmlAlphaConv.next () in
      (* verbose "I-5) alpha-converting the code"; *)
      let _, code = QmlAlphaConv.code alpha code in
      code
    in

    (* 6°: typing *)
    let env =
      (* verbose "I-6) typing"; *)
      HighTyper.fold env code
    in

    (* 6bis: typing has been done, we can set the unsafe option of the typer to consider local types as concrete *)
    let env = { env with QT.options = { env.QT.options with QT.concrete_abstract = true } } in

    { env ; code }

  (* ------------------------------------------------------------ *)
  (* Initial Blender                                              *)
  (* ------------------------------------------------------------ *)

  (* Does the first part of the job: alphaconv, preprocess & type *)
  let full_blend ( env : QmlTyper.env ) ( code : QmlAst.code ) : qml_milkshake =

    (* verbose ~time:false "-- Initial Blender starting --"; *)

    let milkshake =

        (* 1-2°: sorting, and getting type definitions into gamma *)
        let milkshake = blend_initial_part1 env code in

        (* 4-7°: DB, alpha, typing *)
        let milkshake = blend_initial_part2 milkshake in

        milkshake

    in
    (* verbose "-- Initial Blender ending --"; *)

    milkshake

(** Sugar of interface, for a class of rewriters *)
module Sugar :
sig
  (** Tranform the code according to the function passed as argument
      This is used by qlm2ocaml by the different passes *)
  val process_code :
    process_code:(QT.gamma ->
                    QmlAst.annotmap ->
                      QmlAst.code ->
                        (QT.gamma * QmlAst.annotmap) * QmlAst.code) ->
    qml_milkshake ->
    qml_milkshake
end =
struct
  let process_code ~process_code milk =
    let gamma = milk.env.QT.gamma in
    let annotmap = milk.env.QT.annotmap in
    let code = milk.code in
    let (gamma, annotmap), code = process_code gamma annotmap code in
    {
      env = { milk.env with
                gamma = gamma ;
                QT.annotmap = annotmap ;
            } ;
      code ;
    }
end
