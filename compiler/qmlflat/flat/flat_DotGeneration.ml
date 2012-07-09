(*
    Copyright © 2011 MLstate

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

(* depends *)
module Format = Base.Format
module List = Base.List

(* refactoring in progress *)

(* alias *)
module ServerLib = Flat_Common.ServerLib

(* shorthands *)
module E = Flat_Env
module P = Qml2ocamlOptions
module Q = QmlAst

(* -- *)

type label = Flat_Common.label

module FieldAccess :
sig
  (**
     Compilation into a simple dot.
  *)
  val unoptimized : Ocaml.expr -> label -> Ocaml.expr

  (**
     A unoptimized case using a cache for speeding up multiple access.
     A cache is generated, and can be accessed using the [Flat_Shared.Let.caches]
     getter, and generated at toplevel before the value containing this field access.
  *)
  val with_cache : Ocaml.expr -> label -> Ocaml.expr

  (**
     Compilation when we know statically the vtable.
  *)
  val closed_record :
    context:QmlError.context ->
    vtable:label list ->
    Ocaml.expr ->
    label ->
    Ocaml.expr

  (**
     The genericall dot function. Depending on type informations and env options
     will select the correct implementation.
  *)
  val dot :
    env:Flat_Env.env ->
    context:QmlError.context ->
    ty_expr:QmlAst.ty option ->
    Ocaml.expr ->
    label ->
    Ocaml.expr

end =
struct
  let unoptimized value label =
    let field = Flat_Shared.field label in
    let dot = Ocaml.Cons.app3 ServerLib.dot field value in
    let comments = Printf.sprintf "Unoptimized case for field %s" label in
    Ocaml.Comments (comments, dot)

  let with_cache value label =
    let cache_var = Flat_Shared.cache () in
    let field = Flat_Shared.field label in
    let search_field_exn_cache = Ocaml.Cons.app4 ServerLib.dot_with_cache cache_var field value in
    let comments = "Cache case" in
    Ocaml.Comments (comments, search_field_exn_cache)

  let closed_record ~context ~vtable value label =
    (*In a closed_record , we need to
      - compute statically the vtable corresponding to all these fields
      - determine statically the offset of the field which has been accessed
      - perform a direct access to this field (and if necessary lazy evaluation)
    *)
    let fields = List.sort String.compare vtable in
    let index =
      match List.findi (fun field -> String.compare field label = 0) fields with
      | None ->
          QmlError.i_error None context (
            "field @{<bright>%s@} does not exist in this record."^^
            "The typer says it should have the following fields @{<bright>{ %a }@}"
          )
            label (Format.pp_list " ; " Format.pp_print_string) fields
      | Some index -> index
    in
    let unsafe_get = Ocaml.Cons.app3 ServerLib.unsafe_get (Ocaml.Cons.int index) value in
    let comments = Printf.sprintf "Eliminated vtable for field %s" label in
    Ocaml.Comments (comments, unsafe_get)

  let dot ~env ~context ~ty_expr value label =
    let unoptimized () = unoptimized value label in
    let with_cache () =
      if Flat_Options.no_cache ()
      then unoptimized ()
      else with_cache value label
    in
    let gamma = env.E.typing.QmlTypes.gamma in
    let analyze_shape typ =
      let rec aux typ = match typ with
      (* Record cases *)
      | Q.TypeRecord ( Q.TyRow (fields, None) ) ->
          let vtable = List.map fst fields in
          closed_record ~context ~vtable value label

      | Q.TypeRecord ( Q.TyRow (_, Some _) ) -> with_cache ()

      (* A Named type may be a Record *)
      | Q.TypeName (_ty, _typeident) -> (
          try
            let typ =
              QmlTypesUtils.Inspect.get_deeper_type_until
                gamma (fun _ -> false) typ in aux typ
          with QmlTyperException.Exception _ -> assert false
        )

      | Q.TypeVar _ -> with_cache ()

      (* Any other type may not be a Record, so should not be in a Dot *)
      | _ ->
          Flat_Env.context_error context
            "field %s does not exist in the type %a"
            label
            QmlPrint.pp#ty typ
      in
      aux typ
    in
    if Flat_Options.no_optim ()
    then unoptimized ()
    else
      (* we'll look in what says the typer *)
      match ty_expr with
      | None -> (* no luck, the typer does not say anything *) with_cache ()
      | Some typ -> analyze_shape typ

end

let compile = FieldAccess.dot
