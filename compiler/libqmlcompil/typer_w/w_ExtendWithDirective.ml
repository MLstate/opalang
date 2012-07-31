(*
    Copyright © 2012 MLstate

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
   @author Niki Vazou
*)

(* ************************************************************************** *)
(** {b Descr}: This module implements the @extendwith directive stuff.        *)
(* ************************************************************************** *)



exception Extend_with_failure of bool



(* ************************************************************************** *)
(** {b Descr} : This function adds a field to a record or a sum containing 
    only one case. If the type is a typename, it unfolds it to apply
    this process.
    This function process the type in surface, i.e. it does not open the
    records nested in the type sub-terms. This means that if the type is not
    a record of a sum with only one case, this function generates an error.
    This function is used to implements typechecking of the @extendwith
    directive.
    @raise Extend_with_failure
    {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let extend_record_simple_type env initial_ty field_name field_type =
  let rec rec_extend seen_expansions ~dont_expand ty =
    (* First, get the canonical representation of the type. *)
    let ty = W_CoreTypes.simple_type_repr ty in
    match ty.W_Algebra.sty_mark with
    | W_Algebra.TM_row_open_seen -> ty
    | W_Algebra.TM_not_seen -> (
        (* Mark the type as already seen before going on to prevent looping. *)
        ty.W_Algebra.sty_mark <- W_Algebra.TM_row_open_seen ;
        match ty.W_Algebra.sty_desc with
        | W_Algebra.SType_var _ ->
            (* Trying to open something not being a record type. This
               should not happen except on type variables not yet instantiated.
               If it is a type variable not yet instantiated, then may be if the
               user add the right type constraint this variable will get
               instantiated and the opening will succeed. So, emit an error
               message suggesting to explicitly coerce the typed expression. *)
            raise (Extend_with_failure true)
        | W_Algebra.SType_named named -> (
            (* Since a name is not a structure, we must directly open the
               manifest representation of this named type. *)
            match named.W_Algebra.nst_unwinded with
            | Some manifest ->
                let opened_manifest =
                  rec_extend seen_expansions ~dont_expand: false manifest in
                (* Attention, by opening a named type, we must forget this
                   name because this name represents a type **non** opened. So
                   we return a new type that is directly the opened manifest
                   representation of the initial type. *)
                opened_manifest
            | None ->
                if dont_expand then (
                  (* The named type has no manifest representation. Hence it is
                     purely abstract and has no structure. Extending "fails". *)
                  raise (Extend_with_failure false)
                )
                else (
                  let (_, seen_expansions') =  (* TODO: May be dont_expand no more useful if this function is refactored according to new unification with heights. *)
                    W_TypeAbbrevs.incrementally_expand_abbrev
                      env seen_expansions ty in
                  ty.W_Algebra.sty_mark <- W_Algebra.TM_not_seen ;
                  rec_extend seen_expansions' ~dont_expand: true ty
              )
          )
        | W_Algebra.SType_sum_of_records sumcases_column -> (
            let sumcases_column =
              W_CoreTypes.column_type_repr sumcases_column in
            let (col_rows, _) = sumcases_column.W_Algebra.ct_value in
            (* Not that we do not take into account the ending of the column.
               This means that it is possible that it is a column variable that
               will be later instantiated by some other rows. If at the present
               moment we really have one unique row, then opening will work,
               other rows will be added afterwards although if they were added
               before (before the unification constraint arrived sooner) the
               opening would have failed.
               This is another case where this construct doesn't commute ! *)
            match col_rows with
            | [unique_row] -> (
                let unique_row = W_CoreTypes.row_type_repr unique_row in
                let (row_fields, row_ending) = unique_row.W_Algebra.rt_value in
                (* Rebuild a row type, adding the new field and if applied
                   deleting a previous with the same name. *)
                let new_fields = (field_name, field_type)::
                  (List.filter (fun (x, _) -> not(x=field_name)) row_fields) in
                let extended_row = {
                  W_Algebra.rt_value = (new_fields, row_ending)
                } in
                (* Finally, rebuild a column replacing its row and forcing
                   it ending to be closed. *)
                let final_ty =
                  { W_Algebra.sty_desc =
                      W_Algebra.SType_sum_of_records
                        { W_Algebra. ct_value =
                            ([extended_row], W_Algebra.Closed_column) };
                    W_Algebra.sty_link = None ;
                    W_Algebra.sty_mark = W_Algebra.TM_not_seen }
                in
                W_TypeInfo.add_linked_object
                  final_ty.W_Algebra.sty_desc
                  ty.W_Algebra.sty_desc ;
                final_ty
              )
            | _ ->
                (* If the column has several cases, then it is not a record,
                   it is rather a sum. Hence, in this case we emit an error. *)
                raise (Extend_with_failure false)
          )
        | W_Algebra.SType_forall _
        | W_Algebra.SType_arrow (_, _) ->
            raise (Extend_with_failure false)
      )
    | _ (* Other markers. *) -> assert false in
  (* Effective body of the function [extend_record_simple_type]. *)
  let extended_ty =
    rec_extend W_TypeAbbrevs.empty_memory ~dont_expand: false initial_ty in
  W_CoreTypes.cleanup_simple_type initial_ty ;
  extended_ty
