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
   A few utils to refresh types, annotations and anything that cannot be unmarshalled safely
*)
val find_t : ObjectFiles.Package.t -> QmlAst.TypeVar.t -> QmlAst.TypeVar.t
val find_r : ObjectFiles.Package.t -> QmlAst.RowVar.t -> QmlAst.RowVar.t
val find_c : ObjectFiles.Package.t -> QmlAst.ColVar.t -> QmlAst.ColVar.t
val refresh_typevars_from_ty : ObjectFiles.Package.t -> QmlAst.ty -> QmlAst.ty
val refresh_typevars_from_expr : ObjectFiles.Package.t -> QmlAst.expr -> QmlAst.expr
val refresh_typevars_from_tsc : ObjectFiles.Package.t -> (QmlAst.ty, unit) QmlGenericScheme.tsc -> (QmlAst.ty, unit) QmlGenericScheme.tsc
val refresh_typevars_from_code : ObjectFiles.Package.t -> QmlAst.code -> QmlAst.code
val refresh_gamma : ObjectFiles.Package.t -> QmlTypes.gamma -> QmlTypes.gamma
val refresh_expr : ObjectFiles.Package.t -> annotmap_old:QmlAst.annotmap -> QmlAst.annotmap -> QmlAst.expr -> QmlAst.annotmap * QmlAst.expr
val refresh_annotmap : ObjectFiles.Package.t -> QmlAst.annotmap -> QmlAst.annotmap
val refresh_schema : ObjectFiles.Package.t -> annotmap_old:QmlAst.annotmap -> QmlAst.annotmap -> QmlDbGen.Schema.t -> QmlAst.annotmap * QmlDbGen.Schema.t
val refresh_schema2 : ObjectFiles.Package.t -> refreshed_annotmap_old:QmlAst.annotmap -> QmlAst.annotmap -> QmlDbGen.Schema.t -> QmlAst.annotmap * QmlDbGen.Schema.t
val refresh_expr_no_annotmap : ObjectFiles.Package.t -> QmlAst.expr -> QmlAst.expr

val refresh_pat : ObjectFiles.Package.t -> annotmap_old:QmlAst.annotmap -> QmlAst.annotmap -> QmlAst.pat -> QmlAst.annotmap * QmlAst.pat

(*val restrict_annotmap_gen : ('acc -> 'expr -> 'acc) -> annotmap -> 'expr -> annotmap*)
val restrict_annotmap_expr : QmlAst.annotmap -> ?acc:QmlAst.annotmap -> QmlAst.expr -> QmlAst.annotmap
val restrict_annotmap_fold_expr :
  ((QmlAst.annotmap -> QmlAst.expr -> QmlAst.annotmap) ->
     (QmlAst.annotmap -> 't -> QmlAst.annotmap)) ->
   QmlAst.annotmap -> ?acc:QmlAst.annotmap -> 't -> QmlAst.annotmap

val restrict_annotmap_pat : QmlAst.annotmap -> ?acc:QmlAst.annotmap -> QmlAst.pat -> QmlAst.annotmap
val restrict_annotmap_fold_pat :
  ((QmlAst.annotmap -> QmlAst.pat -> QmlAst.annotmap) ->
     (QmlAst.annotmap -> 't -> QmlAst.annotmap)) ->
   QmlAst.annotmap -> ?acc:QmlAst.annotmap -> 't -> QmlAst.annotmap

(**
   Saving the current substitution
   Should be used by linking only
*)
val save : unit -> unit
val load : unit -> unit
val clear : unit -> unit
