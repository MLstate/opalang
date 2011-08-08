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
   @author François Pessaux
*)

(* ************************************************************************** *)
(** {b Descr}: This module implements the @opensums directive stuff.          *)
(* ************************************************************************** *)



(* ************************************************************************** *)
(** {b Descr}: Exception raised when typing to add a column variable to make
    an opened sum type on a type not being a sum. Two cases can arise: either
    the type is a non-yet instantiated variable and in this case may be there
    is no yet enough constraints to make it appearing as a sum. In this case
    the user has to be suggested to add constraint. Or the type is explicitly
    something else than a sum type and this means that the program is
    ill-typed.
    The boolean value argument of this exception tells if we are in the first
    case (convention a bit ad-hoc, but this was simply to avoid creating a
    type just to precise the exception...).                                   *)
(* ************************************************************************** *)
exception Open_sum_failure of bool



(** @raise Open_sum_failure *)
let open_sum_simple_type env initial_ty =
  let rec rec_open_in_simple_type seen_expansions ~dont_expand ty =
    (* First, get the canonical representation of the type. *)
    let ty = W_CoreTypes.simple_type_repr ty in
    match ty.W_Algebra.sty_mark with
    | W_Algebra.TM_col_open_seen opened_as -> opened_as
    | W_Algebra.TM_not_seen -> (
        (* Mark the type as already seen before going on to prevent looping. *)
        let tmp_ty = W_CoreTypes.type_variable () in
        ty.W_Algebra.sty_mark <- W_Algebra.TM_col_open_seen tmp_ty ;
        let opened_ty =
          (match ty.W_Algebra.sty_desc with
           | W_Algebra.SType_arrow (args_tys, res_ty) ->
               (* Not very formalized, but... the idea is to open sums in
                  positive position of the arrow. In effect, this allows to
                  tell that a function returning a sum type indeed returns a
                  wider sum type.
                  This remains consistent since this arises in covariant
                  position.
                  Applying the same opening in negative position would be unsafe
                  because it would means that we force a function taking a
                  particular sum (i.e. fixed list of cases) to exhibit a type
                  telling that it can handle more cases than it really can ! *)
               let res_ty' =
                 rec_open_in_simple_type
                   seen_expansions ~dont_expand: false res_ty in
               { W_Algebra.sty_desc =
                   W_Algebra.SType_arrow (args_tys, res_ty') ;
                 W_Algebra.sty_link = None ;
                 W_Algebra.sty_mark = W_Algebra.TM_not_seen }
           | W_Algebra.SType_named named -> (
               (* Since a name is not a structure, we must directly open the
                  manifest representation of this named type. *)
               match named.W_Algebra.nst_unwinded with
               | Some manifest ->
                   let opened_manifest =
                     rec_open_in_simple_type
                       seen_expansions ~dont_expand: false manifest in
                   (* Attention, by opening a named type, we must forget this
                      name because this name represents a type **non** opened.
                      So we return a new type that is directly the opened
                      manifest representation of the initial type. *)
                   opened_manifest
               | None ->
                   if dont_expand then (
                     (* The named type has no manifest representation. Hence it
                        is purely abstract and has no structure. Opening
                        "fails" and we simply return the original type
                        untouched. *)
                     ty
                   )
                   else (
                     let (_, seen_expansions') =  (* TODO: May be dont_expand no more useful if this function is refactored according to new unification with heights. *)
                       W_TypeAbbrevs.incrementally_expand_abbrev
                         env seen_expansions ty in
                     ty.W_Algebra.sty_mark <- W_Algebra.TM_not_seen ;
                     rec_open_in_simple_type
                       seen_expansions' ~dont_expand: true ty
                   )
             )
         | W_Algebra.SType_sum_of_records sumcases_column ->
             rec_open_in_column_type seen_expansions sumcases_column
         | W_Algebra.SType_var _ | W_Algebra.SType_forall _ ->
             (* We return the type without modification. *)
             ty) in
        tmp_ty.W_Algebra.sty_link <- Some opened_ty ;
        (* Return our opened copy. *)
        opened_ty
      )
    | _ (* Other markers. *) -> assert false


  and rec_open_in_column_type seen_expansions column =
    let column = W_CoreTypes.column_type_repr column in
    let (col_rows, col_ending) = column.W_Algebra.ct_value in
    let col_rows' =
      List.map (rec_open_in_row_type seen_expansions) col_rows in
    let col_ending' =
      (match col_ending with
       | W_Algebra.Var_column _ ->
           (* Column is already opened, hence no need to open it again. *)
           col_ending
       | W_Algebra.Closed_column ->
           (* Ok, the column is closed, so we really must create a new
              column variable to open it. *)
           W_CoreTypes.column_ending_variable ()) in
    let opened_column = { W_Algebra.ct_value = (col_rows', col_ending') } in
    (* Finally, rebuild the sum type, with the row inductively transformed and
       ending it by our opened ending if it was not already the case. *)
    { W_Algebra.sty_desc = W_Algebra.SType_sum_of_records opened_column ;
      W_Algebra.sty_link = None ;
      W_Algebra.sty_mark = W_Algebra.TM_not_seen }


  and rec_open_in_row_type seen_expansions row =
    let (row_fields, row_ending) =
      (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
    let row_fields' =
      List.map
        (fun (field_name, field_ty) ->
           (field_name,
            rec_open_in_simple_type
              seen_expansions ~dont_expand: false field_ty))
        row_fields in
    { W_Algebra.rt_value = (row_fields', row_ending) } in


  (* Effective body of the function [open_sum_simple_type].
     We first check for the legal cases on which to apply the opening. If the
     root structure of the type is not compatible with opening, then we raise
     an error. Otherwise, we recursively apply opening and nested part that
     are not suitable for opening are left as is. This means that we only open
     subterms that can be as long as the root of the type was suitable for
     opening. *)
  let initial_ty = W_CoreTypes.simple_type_repr initial_ty in
  match initial_ty.W_Algebra.sty_desc with
  | W_Algebra.SType_var _ ->
      (* Trying to open a type variable not yet instantiated, then may be if the
         user add the right type constraint this variable will get instantiated
         and the opening will succeed. So, emit an error message suggesting to
         explicitly coerce the typed expression. *)
      raise (Open_sum_failure true)
  | W_Algebra.SType_sum_of_records _ | W_Algebra.SType_arrow (_, _)
  | W_Algebra.SType_named _ ->
      let opened_ty =
        rec_open_in_simple_type
          W_TypeAbbrevs.empty_memory ~dont_expand: false initial_ty in
      W_CoreTypes.cleanup_simple_type initial_ty ;
      opened_ty
  | W_Algebra.SType_forall _ -> raise (Open_sum_failure false)
