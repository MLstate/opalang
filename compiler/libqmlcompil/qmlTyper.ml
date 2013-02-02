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

(** See file qmlMakeTyper.ml *)


(* ************************************************************************** *)
(** {b Descr}: This file provides some instances of typecheckers built
    from low level typecheckers. In particular, it provides the currently
    used W-based typechecker.
    The effective current typechecker module is referenced via the module
    [OfficialTyper], this indirection making easier changing the current
    typechecker without having to change its name everywhere in the remaining
    of the compiler.                                                          *)
(* ************************************************************************** *)

(**
   TODO :
   The interfaces cmi of all modules implementing a typer are hidden to be sure
   that there is not at all any dependency to a particular typer
   (like it is done in dbGen_private and schema_private)

   Precisely, that means that Typer_new_subtyping.cmi is not copied in
   MLSTATELIBS/libqmlcompil
*)

type env = QmlMakeTyper.public_env

(* module NonRecTyper = QmlMakeTyper.Make ( Typer_un_rec.Main ) *)



(* ************************************************************************** *)
(** {b Descr}: Typechecker based on unification in place, generalization by
    binding level, etc.                                                       *)
(* ************************************************************************** *)
module WTyper = QmlMakeTyper.Make ( Typer_w.Main )



module NoTyperLowLevel = struct
  let type_of_expr ?options:_ ?(annotmap=QmlAnnotMap.empty)
      ~bypass_typer:_ ~gamma expr =
    let f_gen annot annotmap expr =
      QmlAnnotMap.add_ty (annot expr) QmlAst.typeNull annotmap in
    let fe = f_gen QmlAst.QAnnot.expr in
    let fp = f_gen QmlAst.QAnnot.pat in
    let annotmap = QmlAstWalk.ExprPatt.fold fe fp annotmap expr in
    ((gamma, QmlTypes.Env.empty), annotmap, QmlAst.typeNull)
  let type_of_type ~gamma ty =
    ignore gamma;
    ty
end
module NoTyper = QmlMakeTyper.Make ( NoTyperLowLevel )



(* ************************************************************************** *)
(** {b Descr}: The name of the module representing the typechecker really
    called by {b s3Passes} to trigger typechecking. Having this indirection
    allows to change the underlying high-level typer without changing
    everywhere in the remaining of the code.                                  *)
(* ************************************************************************** *)
module OfficialTyper = (WTyper : QmlMakeTyper.HIGH_LEVEL_TYPER)



(* ************************************************************************** *)
(** {b Descr}: Type describing the available typecheckers. Add here a new
    typechecker if you need it.  Attention: must be in accordance with the
    list of typecheckers names [available_typer_list] below: adding a type
    constructor representing a typechecker must be collerated with adding a
    name in the list [available_typer_list] below.                            *)
(* ************************************************************************** *)
type available_typer = [ `off | `w_based ]



(* ************************************************************************** *)
(** {b Descr}: The list of available typecheckers names as a list of strings.
    Attention: must be in accordance with the type [available_typer]
    above: adding a typechecker name must be collerated with adding a type
    constructor in the type [available_typer] above.                          *)
(* ************************************************************************** *)
let available_typer_list = ["off"; "w_based" ]



(* ************************************************************************** *)
(** {b Descr}: Returns an optional type of typechecker corresponding to the
    name received as argument. If no available typechecker exists with this
    name, returns [None].                                                     *)
(* ************************************************************************** *)
let available_typer_of_string : string -> available_typer option = function
  | "off" -> Some `off
  | "w_based" -> Some `w_based
  | _ -> None



(* ************************************************************************** *)
(** {b Descr}: Returns the name a a typechecker corresponding to the type of
    typechecker received as argument.
    Attention: relies on consistence between the type describing available
    typecheckers and names of available typecheckers.                         *)
(* ************************************************************************** *)
let string_of_available_typer : available_typer -> string = function
  | `off -> "off"
  | `w_based -> "w_based"



(** New : Dynamic Typer *)
module DynamicallyChangeableTyper :
sig
  val switch_typer : available_typer -> unit
  val get_current_typer : unit -> available_typer
  module HighTyper : QmlMakeTyper.HIGH_LEVEL_TYPER
end =
struct

  let _current_typer : available_typer ref = ref `w_based
  let switch_typer e = _current_typer := e
  let get_current_typer () = !_current_typer

  module LowLevelDynamicTyper : QmlTypes.QML_LOW_LEVEL_TYPER =
  struct
    let type_of_expr ?(options=QmlTypes.default_options) ?(annotmap=QmlAnnotMap.empty) ~bypass_typer =
      match !_current_typer with
      | `off ->
          NoTyperLowLevel.type_of_expr ~options ~annotmap ~bypass_typer
      | `w_based ->
          (* Select the W-based typechecker, i.e. not constraints-based
             inference engine. *)
          Typer_w.type_of_expr ~options ~annotmap ~bypass_typer

    let type_of_type ~gamma code =
      match !_current_typer with
      | `off -> code
      | `w_based ->
          Typer_w.type_of_type ~gamma code
  end

  module HighTyper = QmlMakeTyper.Make ( LowLevelDynamicTyper )
end

module DyTyper = DynamicallyChangeableTyper
