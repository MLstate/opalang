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
(*
   @author François Pessaux
*)

(* ************************************************************************** *)
(** {b Descr}: This module implements manipulation of annotation maps used
    internally by the typechecker. Such annotation maps contains typing
    information as manipulated by the typechecker and a different from
    annotation map of the QML side. Bridges between the 2 kinds of annotation
    map are provided here to allows communication between the 2 worlds: the
    world of low and the world of high levels typecheckers.                   *)
(* ************************************************************************** *)



type annotmap = W_Algebra.simple_type QmlAnnotMap.gen_annotmap



let empty_annotmap = (QmlAnnotMap.empty : annotmap)



(* ************************************************************************** *)
(*  (simple_type, 'a) QmlGenericScheme.tsc ->                                 *)
(*    (QmlAst.ty, unit) QmlGenericScheme.tsc                                  *)
(* ************************************************************************** *)
let annotmap_type_scheme_to_qml_type_scheme annotmap_sch =
  (* We directly recover and keep the quantification as QML variables from the
     scheme of the annotation map. We are sure that the variables are those
     present inside the body of the scheme. *)
  let (qml_quantification, body, _) =
    QmlGenericScheme.export_unsafe annotmap_sch in
  (* Now convert the body of the [types_scheme] into a QML type. *)
  let qml_body = W_PublicExport.simple_type_to_qml_type body in
  QmlGenericScheme.import qml_quantification qml_body ()



(* ************************************************************************** *)
(** {b Descr}: Transforms an annotation map into a QML annotation map. This
    consists in getting the [simple_type]s stored in the annotation map,
    transform these [simple_type]s into QML types, then putting these latter
    in the result QML annotation map at the same place.
    We do the same thing for type schemes stored in the annotation map.
    This function is used to establish the connection between annotation maps
    from the typechecker side to the QML side.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let annotmap_to_qml_annotmap annotmap =
  QmlAnnotMap.map_ty_tsc
    ~ty:W_PublicExport.simple_type_to_qml_type
    ~tsc:annotmap_type_scheme_to_qml_type_scheme
    annotmap
