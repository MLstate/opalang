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
(* THIS FILE HAS A DOCUMENTED MLI *)

(* depends *)
module Format = Base.Format

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst

let contains_tracker e =
  QmlAstWalk.Expr.exists
    (function
     | Q.Directive (_, `tracker _, _, _) -> true
     | _ -> false) e

module Printer =
struct
  let code fmt code =
    Format.fprintf fmt "/* printer: --print code */@\n@\n" ;
    QmlPrint.pp#code fmt code ;
    Format.fprintf fmt "@."

  let light_ident fmt code =
    Format.fprintf fmt "/* printer: --print light_ident */@\n@\n" ;
    QmlPrint.pp_light_ident#code fmt code ;
    Format.fprintf fmt "@."

  let very_light_ident fmt code =
    Format.fprintf fmt "/* printer: --print very_light_ident */@\n@\n" ;
    QmlPrint.pp_very_light_ident#code fmt code ;
    Format.fprintf fmt "@."

  let code_with_type fmt annotmap code =
    Format.fprintf fmt "/* printer: --print code_with_type */@\n@\n" ;
    (new QmlPrint.printer_with_type annotmap)#code fmt code ;
    Format.fprintf fmt "@."

  let code_for_ei fmt annotmap code =
    Format.fprintf fmt "/* printer: --print code_for_ei */@\n@\n" ;
    (new QmlPrint.printer_for_ei annotmap)#code fmt code ;
    Format.fprintf fmt "@."

  (* <!> beware opatrack uses this formating.
     Do not change it or please update opatrack *)
  let size fmt code =
    let i =
      QmlAstWalk.CodeExpr.fold
        (QmlAstWalk.Expr.fold (fun acc _ -> acc + 1)) 0 code in
    Format.fprintf fmt
      "%d declarations@\n%d nodes@." (List.length code) i

  let declaration fmt code =
    Format.fprintf fmt "/* printer: --print declaration */@\n@\n" ;
    QmlPrint.pp_declaration#code fmt code ;
    Format.fprintf fmt "@."

  let annotation fmt code =
    Format.fprintf fmt "/* printer: --print annotation */@\n@\n";
    QmlPrint.pp_annotation#code fmt code;
    Format.fprintf fmt "@."

  (* ************************************************************************ *)
  (** {b Descr}: Printer for position in source code.
      {b Visibility} : Not exported outside this module.                      *)
  (* ************************************************************************ *)
  let position fmt code =
    Format.fprintf fmt "/* printer: --print position */@\n@\n";
    QmlPrint.pp_position#code fmt code;
    Format.fprintf fmt "@."

  let tracked fmt code =
    Format.fprintf fmt "/* printer: --print tracked */" ;
    let bind kw rec_ (s, e) =
      let val_ = if !kw then (kw := false; "val"^rec_) else "and" in
      Format.fprintf fmt "@\n@\n@\n@\n@\n/* %s %s */@\n@\n%s %s =@ %a@\n"
        val_ (Ident.stident s) val_ (Ident.stident s) QmlPrint.pp#expr e
    in
    let is_tracked = List.exists (fun (_, e) -> contains_tracker e) in
    List.iter
      (function
         | Q.NewVal (_, b) ->
             if is_tracked b then List.iter (bind (ref true) "") b
         | Q.NewValRec (_, b) ->
             if is_tracked b then List.iter (bind (ref true) " rec") b
         | _ -> ()
      ) code;
    Format.fprintf fmt "@."

  let gamma fmt gamma =
    Format.fprintf fmt "/* printer: --print gamma*/@.";
    QmlTypes.Env.pp fmt gamma;
    Format.fprintf fmt "@."
end

let define = PassHandler.define_printer
let code_id = define "code"
let light_ident_id = define "light_ident"
let very_light_ident_id = define "very_light_ident"
let with_type_id = define "with_type"
let for_ei_id = define "for_ei"
let size_id = define "size"
let declaration_id = define "declaration"
let annotation_id = define "annotation"
let position_id = define "position"
let tracked_id = define "tracked"
let gamma_id = define "gamma"

let printers extract _ =
  let make_code fct fmt env =
    let _, _, code = extract env in
    fct fmt code in
  let make_ac fct fmt env =
    let annotmap, _, code = extract env in
    fct fmt annotmap code in
  let make_gamma fct fmt env =
    let _, gamma, _ = extract env in
    fct fmt gamma in
  [
    code_id, make_code Printer.code ;
    light_ident_id, make_code Printer.light_ident ;
    very_light_ident_id, make_code Printer.very_light_ident ;
    declaration_id, make_code Printer.declaration ;
    annotation_id, make_code Printer.annotation;
    (* Source code positions printer registered. *)
    position_id, make_code Printer.position;
    size_id, make_code Printer.size ;
    with_type_id, make_ac Printer.code_with_type;
    for_ei_id, make_ac Printer.code_for_ei;
    gamma_id, make_gamma Printer.gamma;
    (* waiting for flexibility in passhander options *)
    (* tracked_id, make Printer.tracked ; *)
  ]

module Tracker =
struct
  let pp_print_expr = QmlPrint.pp#expr
  let pp_print_code_elt = QmlPrint.pp#code_elt

  let directive iter =
    QmlAstWalk.CodeExpr.iter
      (QmlAstWalk.Expr.iter
         (function
          | Q.Directive (_, `tracker t, [e], _) -> iter.PassTracker.track (PassTracker.filename t) pp_print_expr e
          | _ -> ()))

  (* We keep the full code_elt for each val
     it is a duplication, but it speed up searching
     anyway, the folder _tracks can be cleaned *)
  let val_ iter = List.iter
    (function
       | Q.NewVal (_, binds)
       | ( Q.NewValRec (_, binds) ) as elt -> List.iter
           (fun (s, _) ->
              let filename = Ident.stident s in
              iter.PassTracker.track filename pp_print_code_elt elt
           ) binds
       | _ -> ())
end

let define = PassHandler.define_tracker
let directive_id = define "track"
let val_id = define "val"

let trackers extract _ =
  let make fct fmt env = fct fmt (extract env) in
  [
    directive_id, make Tracker.directive ;
    val_id, make Tracker.val_ ;
  ]

(* iterator on `track directive with something else that expr *)
module WIP =
struct
  (* other iterators : wip *)
  type ('tracked, 'env) iter_tracker =
      ( 'env -> QmlAst.code ) ->
        (PassTracker.t  -> 'tracked PassHandler.printer -> 'tracked -> unit) -> 'env -> unit

  let pp_print_expr = QmlPrint.pp#expr
  let pp_print_annot fmt annot =
    Format.pp_print_string fmt (Annot.to_string annot)
  let pp_print_ty = QmlPrint.pp#ty

  let iter_tracker extract iter env =
    QmlAstWalk.CodeExpr.iter
      (QmlAstWalk.Expr.iter
         (function
          | Q.Directive (_, `tracker t, [e], _) -> iter.PassTracker.track (PassTracker.filename t) pp_print_expr e
          | _ -> ()))
      (extract env)

  let iter_annot_tracker extract iter env =
    QmlAstWalk.CodeExpr.iter
      (QmlAstWalk.Expr.iter
         (function
          | Q.Directive (_, `tracker t, [e], _) -> iter t pp_print_annot (Q.QAnnot.expr e)
          | _ -> () ))
      (extract env)

  let iter_ty_tracker extract_annotmap extract_code iter env =
    QmlAstWalk.CodeExpr.iter
      (QmlAstWalk.Expr.iter
         (function
          | Q.Directive (_, `tracker t, [e], _) -> (
              let annot = Q.QAnnot.expr e in
              match QmlAnnotMap.find_ty_opt annot (extract_annotmap env) with
              | Some ty -> iter t pp_print_ty ty
              | None -> iter t
                  (fun fmt _ ->
                     Format.fprintf fmt "Annot Not Found: %a. The expr is:@\n%a"
                       pp_print_annot annot pp_print_expr e)
                    (QmlAst.TypeConst QmlAst.TyNull)
            )
          | _ -> () )
      )
      (extract_code env)

end
