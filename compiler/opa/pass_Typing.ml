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
(* shorthands *)
module P = Passes


module S =
struct
  type t = (QmlAst.ty, unit) QmlGenericScheme.tsc IdentMap.t
  let pass = "pass_Typing"
  let pp f map =
    IdentMap.iter
      (fun k v ->
         Format.fprintf f "@[<2>%s ->@ %a@]@\n" (Ident.to_string k) QmlPrint.pp#tsc v
      ) map
end

module R = ObjectFiles.Make(S)



(* ************************************************************************** *)
(** {b Descr}: Module used to make the type of exceptions persistent along
    separate compilation. In effect, to be safe, we must remind the type of
    exceptions encountered in modules a module depends on.
    The aim is to prevent a guy raising { A : int } and a guy catching
    { A = x } then using x as a string.
    The persistent information stored is the QML type, assumed by invariant to
    be a sum type always with a column variable (i.e. opened sum) representing
    the structure of the type of exceptions.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
module SExceptions =
struct
  type t = QmlAst.ty
  let pass = "pass_TypingExceptions"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module RExceptions = ObjectFiles.Make(SExceptions)



let process_code ?(save = true) env =
  (* ATTENTION ! Since the W-based typechecker uses global memoization tables
     to speed-up type schemes importation from QML, we must empty them for
     each new package compilation otherwise we will retain and confuse the
     schemes renamed by different separate compilation-purpose refreshes. *)
  Typer_w.reset_toplevel_tydefs_schemes_env_memo () ;
  Typer_w.reset_toplevel_valdefs_schemes_env_memo () ;
  (* Also reset the type of exceptions, to drop cases coming from usages of
     other packages we don't depend on. *)
  Typer_w.reset_type_exception () ;
  let typerEnv = env.P.typerEnv in
  let schema = typerEnv.QmlTypes.schema in
  let code = env.P.qmlAst in
  let initial_gamma = typerEnv.QmlTypes.gamma in
  let (rebuilt_gamma, stdlib_map) =
    let (map, map_stdlib) =
      R.fold_with_name ~deep: true
        (fun package (acc_map, acc_stdlib) map ->
           let map =
             IdentMap.map (QmlRefresh.refresh_typevars_from_tsc package) map in
           let acc_map = IdentMap.safe_merge acc_map map in
           let acc_stdlib =
             if ObjectFiles.compiler_package package then
               IdentMap.safe_merge acc_stdlib map
             else acc_stdlib in
           (acc_map, acc_stdlib))
        (QmlTypes.Env.Ident.to_map initial_gamma, IdentMap.empty) in
    QmlTypes.Env.Ident.from_map map initial_gamma,
    map_stdlib in
  let typerEnv = { typerEnv with QmlTypes.gamma = rebuilt_gamma } in
  (* Restore the structure of the type "exception". We get the list of types
     assumed to be sums, that each module we depend on created. *)
  let exn_tys_list =
    RExceptions.fold_with_name ~deep: true
      (fun _package accu_exn_ty exn_ty ->
         exn_ty :: accu_exn_ty)
      [] in
  (* Inject this structure inside the typechecker by cascading unifications
     of all the types found for type "exception" in all the modules we depend
     on. *)
  Typer_w.init_type_exception rebuilt_gamma exn_tys_list ;

  let typerEnv = QmlTyper.OfficialTyper.fold typerEnv code in
  let typerEnv =
    if ObjectFiles.compilation_mode() = `init then typerEnv
    else
      QmlDbGen.Schema.fold_expr
        QmlTyper.OfficialTyper.fold_expr typerEnv schema in
  let final_gamma = typerEnv.QmlTypes.gamma in
  (* we remove anything from the gamma that does not come from this compilation,
   * i.e. that is in rebuilt gamma but not in initial gamma *)
  let diff_map =
    IdentMap.diff2
      (QmlTypes.Env.Ident.to_map final_gamma)
      (QmlTypes.Env.Ident.to_map rebuilt_gamma)
      (QmlTypes.Env.Ident.to_map initial_gamma) in

  (* Now, recover the structure of the sum type representing the type
     "exception". *)
  let exception_ty_structure = Typer_w.get_type_exception_description () in

  if save then (
    R.save diff_map ;
    RExceptions.save exception_ty_structure   (* Save the type "exception". *)
  ) ;

  let stdlib_map =
    if ObjectFiles.stdlib_packages (ObjectFiles.get_current_package ()) then
      QmlTypes.Env.Ident.to_map final_gamma
    else stdlib_map in
  let stdlib_gamma =
    QmlTypes.Env.Ident.from_map stdlib_map env.P.stdlib_gamma in
  let diff_gamma = QmlTypes.Env.Ident.from_map diff_map final_gamma in
  let typerEnv = { typerEnv with QmlTypes.gamma = diff_gamma } in
  { env with
      P.typerEnv = typerEnv ; qmlAst = code ; stdlib_gamma = stdlib_gamma }
