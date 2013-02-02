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


type print_level =
 | Function
 | Function_With_N_Arguments
 | Function_With_Some_SubType
 | Complete_Type

type context = {
  mutable print_level : print_level ;
  mutable print_fake_record : bool ;
  mutable error_type1 : W_Algebra.simple_type ;
  mutable error_type2 : W_Algebra.simple_type ;
}
(* depends *)
module Format = Base.Format

let context =
  { print_level = Complete_Type;
    print_fake_record = false ;
    error_type1 = W_CoreTypes.type_int () ;
    error_type2 = W_CoreTypes.type_int () ;
  }


let type_variables_counter = ref 0
let row_variables_counter = ref 0
let column_variables_counter = ref 0

let print_only_function _ =
  context.print_level <- Function

let print_function_with_n_args _ =
  context.print_level <- Function_With_N_Arguments

let print_subtype_of_function _ =
  context.print_level <- Function_With_Some_SubType

let set_error_type1 t =
  context.error_type1 <- t
let set_error_type2 t =
  context.error_type2 <- t

(* ************************************************************************** *)
(** {b Descr}: List of abbreviations assigned to type during pretty-print of
    a type. This list is filled and cleared by side effect.
    Abbreviations are reminded to finally, at the end of a scheme, type or
    sequence printing be "explained". In other words, once a single type, a
    type scheme or a sequence of types are printed, all the abbreviation are
    displayed giving their name and the type they represent. "Explaining" them
    at the end allows to factorize them and have all abbreviations for all
    types printed once instead of havign them printed for each type they appear
    in.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let abbreviations = ref ([] : W_Algebra.simple_type list)



(* ************************************************************************** *)
(** {b Descr}: Lists of types, row variables and column variables encountered
    during pretty-print in which the markers [W_Algebra.TM_print_final_abbrevd]
    and [W_Algebra.VM_print_final_abbrevd] must be changed in place by
    [W_Algebra.TM_print_sequence_abbrevd] and
    [W_Algebra.VM_print_sequence_abbrevd].
    These lists are filled and cleared by side effect.
    See also [toggle_final_abbrevd_for_sequence] to get an explanation about
    the reason of this and how it works.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let tys_to_reprint_if_sequence = ref ([] : W_Algebra.simple_type list)
let row_vars_to_reprint_if_sequence = ref ([] : W_Algebra.row_variable list)
let col_vars_to_reprint_if_sequence = ref ([] : W_Algebra.column_variable list)



(* ************************************************************************** *)
(* int_to_base_26 : int -> string *)
(** {b Descr}: Transforms an integer to a string compound of only a-z chars.
    This is used to write variables names. In fact, that only an
    integer->base 26 printer.                                                 *)
(* ************************************************************************** *)
let rec int_to_base_26 i =
  if i >= 26 then
    let ch = (i mod 26) + (Char.code 'a') in
    (int_to_base_26 (i / 26)) ^ Char.escaped (Char.chr ch)
  else
    let ch = (i mod 26) + (Char.code 'a') in
    Char.escaped (Char.chr ch)



let create_type_variable_name _debug_var_desc =
  (* <---------- DEBUG
  let debug_name =
    (match _debug_var_desc with
     | W_Algebra.SType_var bla ->
         "(id:" ^ (string_of_int bla.W_Algebra.tv_debug_count) ^ ")" ^
           "(qml:" ^ (QmlAst.TypeVar.to_string bla.W_Algebra.tv_qml) ^ ")" ^
           (if bla.W_Algebra.tv_level = W_CoreTypes.generic_binding_level then
              "GEN"
            else ("_lev:" ^ (string_of_int bla.W_Algebra.tv_level)))
     | _ -> "(other)") in
  <---------- END DEBUG *)
  let name =
    (* <---------- DEBUG
    debug_name ^
    <---------- END DEBUG *)
    "'" ^ (int_to_base_26 !type_variables_counter) in
  incr type_variables_counter ;
  name



let create_row_variable_name _is_generalized =
  let name =
(*    (if is_generalized then "" else "_") ^ *)
    "'r." ^ (int_to_base_26 !row_variables_counter) in
  incr row_variables_counter ;
  name



let create_column_variable_name _is_generalized =
  let name =
(*    (if is_generalized then "" else "_") ^ *)
    "'c." ^ (int_to_base_26 !column_variables_counter) in
  incr column_variables_counter ;
  name


(* ************************************************************************** *)
(** {b Descr}: Function that prepares the final pretty-printing of a sequence of
    [simple_type]. Since we allows several type definition with the same
    original ident (but different package) we should print type name with their
    packages if several name from different packages will be printed.
    {b Visibility}: Exported outside this module.  *)
(* ************************************************************************** *)
let original_ident = Hashtbl.create 16

let pp_simple_type_prepare_sequence tys =
  List.iter
    (fun ty ->
       let rec aux ty =
         match ty.W_Algebra.sty_desc with
         | W_Algebra.SType_var _ -> ()
         | W_Algebra.SType_arrow (args, bd) ->
             List.iter aux args; aux bd
         | W_Algebra.SType_named n ->
             begin try
               Hashtbl.add original_ident (Ident.original_name n.W_Algebra.nst_name)
                 (Ident.get_package_name n.W_Algebra.nst_name)
             with _ -> () end
         | W_Algebra.SType_sum_of_records {W_Algebra.ct_value = (ct, _)} ->
             List.iter
               (function {W_Algebra.rt_value = (rt, _)} -> List.iter (fun (_, st) -> aux st) rt)
               ct
         | W_Algebra.SType_forall tsh ->
             List.iter aux tsh.W_Algebra.ty_parameters;
             aux tsh.W_Algebra.body
       in aux ty
    ) tys

let pp_type_ident fmt ident =
  let o = (Ident.original_name ident) in
  match BaseList.uniq (Hashtbl.find_all original_ident o) with
  | [] | [_] -> Format.fprintf fmt "%s" o
  | _l ->
      try
        Format.fprintf fmt "%s from package %s" o (Ident.get_package_name ident)
      with _ -> Format.fprintf fmt "%s" o


(* ************************************************************************** *)
(** {b Descr}: Function that prepares the final pretty-printing of a
    [simple_type]. Since type algebra allows cyclic types, we must make a first
    pass to detect cycles and give them a name. Hence this pass descends on the
    type and if a type appears to be already seen in a recursive context, it
    creates an abbreviation ("abbrev") for it, i.e. a nickname that will be used
    to represent this type everywhere it appears instead of going on printing
    it by descending on its structure. This especially prevents pretty-printing
    from looping and make types representation more compact.
    So, each time a type is seen as recursive, an abbreviation is created and
    this type is marked as [TM_print_final_abbrevd] with this abbreviation
    reminded. Note that type variables are labeled using the same marking
    mechanism.
    This function **must** then be called before invoking the "real" final
    pretty-print routine.
    Note that since this function modifies the types markers, types must be
    cleaned up after the final pretty-print routine call.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec __prepare_print_simple_type ty =
  let ty = W_CoreTypes.simple_type_repr ty in
  match ty.W_Algebra.sty_mark with
  | W_Algebra.TM_print_sequence_abbrevd _ ->
      (* Can occur if we are printing several types in sequence to keep sharing
         between visible. *)
      ()
  | W_Algebra.TM_print_prep_seen xtimes ->
      (* Increment the number of times the type was seen. *)
      incr xtimes
  | W_Algebra.TM_export_cyclic _
  | W_Algebra.TM_not_seen | W_Algebra.TM_print_prep_seen_not_rec-> (
      (* The type was never seen of just seen but as non-recursive. We will
         tell that temporarily, if it seen without telling if it is
         recursive. *)
      ty.W_Algebra.sty_mark <- W_Algebra.TM_print_prep_seen (ref 1) ;
      (* We now descend on it structure. If at the return of the sub-term it
         appears that the type is marked [TM_print_prep_seen] more than 1,
         then this means that this type appears in its sub-terms, and hence is
         recursive. *)
      (match ty.W_Algebra.sty_desc with
       | W_Algebra.SType_var _ -> ()
       | W_Algebra.SType_arrow (args_tys, res_ty) ->
           List.iter __prepare_print_simple_type args_tys ;
           __prepare_print_simple_type res_ty
       | W_Algebra.SType_named named ->
           (* We never print the real representation of a named type. *)
           (* <---------- DEBUG
           (match named.W_Algebra.nst_unwinded with
            | None -> ()
            | Some t -> __prepare_print_simple_type t) ;
           <---------- END DEBUG *)
           List.iter __prepare_print_simple_type named.W_Algebra.nst_args
       | W_Algebra.SType_sum_of_records sumcases_col ->
           __prepare_print_column_type sumcases_col
       | W_Algebra.SType_forall scheme ->
           __prepare_print_scheme_parameters
             (scheme.W_Algebra.ty_parameters, scheme.W_Algebra.row_parameters,
              scheme.W_Algebra.column_parameters) ;
           __prepare_print_simple_type scheme.W_Algebra.body
      ) ;
      (* Now we check if the current type is recursive, i.e if it appeared in
         the subtree below it. If not, then we don't need to alias is with a
         "where" construction and we turn back its marker as "seen but non
         recursive" (i.e. [TM_print_prep_seen_not_rec]). *)
      (match ty.W_Algebra.sty_mark with
       | W_Algebra.TM_print_prep_seen xtimes' ->
           if !xtimes' = 1 then
             ty.W_Algebra.sty_mark <- W_Algebra.TM_print_prep_seen_not_rec
       | _ ->
           (* At least, the type must be marker as [TM_print_prep_seen] with
              1, since this is at what we initialized its marker just before
              starting descending in its sub-term. *)
           assert false)
    )
  | _ (* Other markers. *) -> assert false



(* ************************************************************************** *)
(** {b Descr}: Function that prepares the final pretty-printing of a [row_type].
    For more information, read the comment in the header of the function
    [__prepare_print_simple_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __prepare_print_column_type column =
  (* First, get the canonical representation of the column. *)
  let (col_records, col_ending) =
    (W_CoreTypes.column_type_repr column).W_Algebra.ct_value in
  (match col_ending with
   | W_Algebra.Var_column v ->
       (match v.W_Algebra.cv_mark with
        | W_Algebra.VM_print_prep_seen xtimes -> incr xtimes
        | W_Algebra.VM_print_sequence_abbrevd _ ->
            (* Can occur if we are printing several types in sequence to keep
               sharing between visible. *)
            ()
        | W_Algebra.VM_not_seen
        | W_Algebra.VM_print_prep_seen_not_rec ->
            (* By side effect, mark the column as seen once from now. *)
            v.W_Algebra.cv_mark <- W_Algebra.VM_print_prep_seen (ref 1)
        | _ (* Other markers. *) -> assert false)
   | W_Algebra.Closed_column -> ()) ;
  (* Recurse the inspection in each record forming the cases of the sum. *)
  List.iter __prepare_print_row_type col_records ;
  (* Now, revert the marker to tell we saw this column ending but not in a
     recursive context. If the marker indicates that this variable was already
     abbreviated for sequence print, then we don't touch it to prevent sharing
     of the name from being broken between all the sequentially displayed
     columns. *)
  (match col_ending with
   | W_Algebra.Var_column v -> (
       match v.W_Algebra.cv_mark with
       | W_Algebra.VM_print_sequence_abbrevd _ -> ()
       | W_Algebra.VM_print_prep_seen xtimes' ->
           if !xtimes' = 1 then
             v.W_Algebra.cv_mark <- W_Algebra.VM_print_prep_seen_not_rec
       | _ -> assert false
     )
   | _ -> ())



(* ************************************************************************** *)
(** {b Descr}: Function that prepares the final pretty-printing of a
    [column_type].
    For more information, read the comment in the header of the function
    [__prepare_print_simple_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __prepare_print_row_type row =
  (* First, get the canonical representation of the row. *)
  let (row_fields, row_ending) =
    (W_CoreTypes.row_type_repr row).W_Algebra.rt_value in
  (* Since row are well-sorted, a row variable is always preceded by the
     same set of elements. Hence, if we already saw it ending variable, we have
     also already seen its preceding elements and there is no need to explore
     the whole row again. *)
  (match row_ending with
   | W_Algebra.Var_row v ->
       (match v.W_Algebra.rv_mark with
        | W_Algebra.VM_print_prep_seen xtimes -> incr xtimes
        | W_Algebra.VM_print_sequence_abbrevd _ ->
            (* Can occur if we are printing several types in sequence to keep
               sharing between visible. *)
            ()
        | W_Algebra.VM_not_seen
        | W_Algebra.VM_print_prep_seen_not_rec ->
            (* By side effect, mark the row as seen once from now. *)
            v.W_Algebra.rv_mark <- W_Algebra.VM_print_prep_seen (ref 1)
        | _ (* Other markers. *) -> assert false)
   | W_Algebra.Closed_row -> ()) ;
  (* Recurse the inspection in each field of the row. *)
  List.iter
    (fun (_, field_ty) -> __prepare_print_simple_type field_ty) row_fields ;
  (* Now, revert the marker to tell we saw this row ending but not in a
     recursive context. If the marker indicates that this variable was already
     abbreviated for sequence print, then we don't touch it to prevent sharing
     of the name from being broken between all the sequentially displayed
     rows. *)
  (match row_ending with
   | W_Algebra.Var_row v -> (
       match v.W_Algebra.rv_mark with
       | W_Algebra.VM_print_sequence_abbrevd _ -> ()
       | W_Algebra.VM_print_prep_seen xtimes' ->
           if !xtimes' = 1 then
             v.W_Algebra.rv_mark <- W_Algebra.VM_print_prep_seen_not_rec
       | _ -> assert false
     )
   | _ -> ())



(* ************************************************************************** *)
(** {b Descr}: Function that prepares the final pretty-printing of a
    [type_scheme].
    For more information, read the comment in the header of the function
    [__prepare_print_simple_type].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __prepare_print_scheme_parameters
    (ty_parameters, row_parameters, column_parameters) =
  (* Important: start by taking care of the scheme parameter in case some
     variables of the scheme do not appear in the scheme's body (pathological
     case like type ('a) t = int) ! In effect, in this case, only descending on
     the scheme's body won't "see" the variables since they do not appear in
     its body, then assertions will fail during the "final" pretty-print
     routine call. *)
  List.iter __prepare_print_simple_type ty_parameters ;
  List.iter
    (fun row_var ->
       (* Set the variable's marker as [VM_print_prep_seen_not_rec] to tell
          that we saw the variable, but actually not in a recursive context. *)
       row_var.W_Algebra.rv_mark <- W_Algebra.VM_print_prep_seen_not_rec)
    row_parameters ;
  (* Do the same thing for column variables. *)
  List.iter
    (fun col_var ->
       col_var.W_Algebra.cv_mark <- W_Algebra.VM_print_prep_seen_not_rec)
    column_parameters



(* ************************************************************************** *)
(** {b Descr}: Function that changes in place the markers
    [W_Algebra.TM_print_final_abbrevd] of types, and
    [W_Algebra.VM_print_final_abbrevd] of row variables and column variables in
    the lists received as arguments into the marker
    [W_Algebra.TM_print_sequence_abbrevd] and
    [W_Algebra.VM_print_sequence_abbrevd] keeping the abbreviation string
    unchanged.
    This is used to make so that when printing several types in sequence, each
    time we print a new type in this sequence, even if it was previously
    printed in the sequence, then instead of using its abbreviation that was
    possibly created, the first occurrence of this type won't use it and will
    be printed normally. However, if the type appears several time during this
    print step, then next occurrences will be printed using the abbreviation,
    which is the *same* than the one used in previous prints of the sequence.
    This way, identity sharing between several prints of types is kept and each
    individual printed type will expose its structure.
    In other words, this avoids the behaviour like:
      "
       Types { A ; ~a } and { B ; ~b } are incompatible.
       Bla bla { <~a> } and { < ~b> } bla bla bla.
      "
    that occurred before because while printing the first message, abbreviations
    were created for the types, and then when printing the second message, the
    printing routine directly found these abbreviations hence used them to
    represent the types instead of showing again their structure.
    Now the messages look like:
      "
       Types { A ; ~a } and { B ; ~b } are incompatible.
       Bla bla { A ; ~a } and { B ; ~b } bla bla bla.
      "
    clearly showing again the form of the types, but still keeping the variables
    ~a and ~b shared between the messages.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let toggle_final_abbrevd_for_sequence in_tys in_row_vars in_col_vars =
  List.iter
    (fun t ->
      match t.W_Algebra.sty_mark with
      | W_Algebra.TM_print_final_abbrevd abb_name ->
          t.W_Algebra.sty_mark <-
            W_Algebra.TM_print_sequence_abbrevd abb_name
      | _ -> ())
    in_tys ;
  List.iter
    (fun v ->
      match v.W_Algebra.rv_mark with
      | W_Algebra.VM_print_final_abbrevd abb_name ->
          v.W_Algebra.rv_mark <- W_Algebra.VM_print_sequence_abbrevd abb_name
      | _ -> ())
    in_row_vars ;
  List.iter
    (fun v ->
      match v.W_Algebra.cv_mark with
      | W_Algebra.VM_print_final_abbrevd abb_name ->
          v.W_Algebra.cv_mark <- W_Algebra.VM_print_sequence_abbrevd abb_name
      | _ -> ())
    in_col_vars



(* ************************************************************************** *)
(** {b Descr} : Tests if the type [ty] is "void", i.e "{}", in other words
    a closed sum with one unique case being the empty closed record type.
    ATTENTION: this function is only intended to be used to lighten the
    pretty print of types, avoiding to write ": { }" for the type of record
    fields that are of type void. For instance, this allows to print
      { a : int ; b }
    instead of
      { a : int ; b : { } }
    ATTENTION: This function assumes that [W_CoreTypes.xxx_type_repr] was
    already called on [ty] since it is used by [__pp_fields_of_row]. Do not
    try to use it in other and more general contexts, this may give
    unpredictable results.

    {b Args} :
     - [ty] : The type to test an determines if it represents "void".

    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let is_void ty =
  match ty.W_Algebra.sty_desc with
  | W_Algebra.SType_sum_of_records
        { W_Algebra.ct_value = ([uniq], W_Algebra.Closed_column) } -> (
      match uniq with
      | { W_Algebra.rt_value = ([], W_Algebra.Closed_row) } -> true
      | _ -> false
    )
  | _ -> false


let plurial n = if n>1 then "s" else ""

let rec __pp_simple_type prio ppf ty =
  (* Morally, no need to "repr" since this has already been done by
     [__prepare_print_simple_type], but... *)
  let ty = W_CoreTypes.simple_type_repr ty in
  match ty.W_Algebra.sty_mark with
  | W_Algebra.TM_print_final_abbrevd abb_name ->
      Format.fprintf ppf "%s" abb_name
  | W_Algebra.TM_print_sequence_abbrevd _
  | W_Algebra.TM_print_prep_seen_not_rec
  | W_Algebra.TM_print_prep_seen _ -> (
      (* If the type is seen recursive, we introduce an abbrev and turn the
         type's marker as "abbreviated". If the type is a variable, then we
         generate a variable name, and turn the type's marker as abbreviated
         ... to this variable name.
         Otherwise, we simply descend printing on the type's structure. *)
      let (need_abbrev, register_abbrev) =
        (match ty.W_Algebra.sty_mark with
         | W_Algebra.TM_print_sequence_abbrevd abb_name -> (
             (* Now that the type will be re-print once for this new printing
                sequence step, we can ask for it to be printed next as its
                abbreviation. *)
             ty.W_Algebra.sty_mark <-
               W_Algebra.TM_print_final_abbrevd abb_name ;
             tys_to_reprint_if_sequence := ty :: !tys_to_reprint_if_sequence ;
             (* Handle the special case where the type is a variable. Since
                this variable already has an abbrev, we don't want to create
                another one. However, since we say we don't need abbrev, we will
                descent in the type structure to print it and this descent
                doesn't print variables since they are expected to be handled
                and printed by assigning them an abbrev the first time they are
                encountered. *)
             (match ty.W_Algebra.sty_desc with
              | W_Algebra.SType_var _ -> Format.fprintf ppf "%s" abb_name
              | _ -> ()) ;
             (false, false)
           )
         | W_Algebra.TM_print_prep_seen_not_rec ->
             (* Simple match that performs ty =?= SType_var. *)
             (match ty.W_Algebra.sty_desc with
              | W_Algebra.SType_var _ -> (true, false)
              | _ -> (false, false))
         | W_Algebra.TM_print_prep_seen xtimes ->
             (* An abbrev is needed if the type remained tagged as seen several
                time (i.e. > 1), i.e. seen several times in its sub-terms.
                However, if the type a a variable, we generate an "abbrev" that
                is rather its name, but we don't want to register it. *)
             if !xtimes > 1 then
               (match ty.W_Algebra.sty_desc with
                | W_Algebra.SType_var _ ->
                    (* Variable seen recursively, so don't register its
                       abbreviation that is rather its name. *)
                    (true, false)
                | _ ->
                    (* The type is seen recursively and is not a variable, hence
                       we want to register its abbreviation. *)
                    (true, true))
             else (false, false)
         | _ -> assert false) in
      if need_abbrev then (
        (* Ok, introduce an abbreviation. *)
        let abb_name = create_type_variable_name ty.W_Algebra.sty_desc in
        (* Turn the marker of the type to say it is now abbreviated. *)
        ty.W_Algebra.sty_mark <- W_Algebra.TM_print_final_abbrevd abb_name ;
        tys_to_reprint_if_sequence := ty :: !tys_to_reprint_if_sequence ;
        (* And simply print the abbreviation. *)
        Format.fprintf ppf "%s" abb_name ;
        (* Finally, remind the abbreviation to be able to summarize and explain
           them later (if it's not a variable, since we treated variables and
           real cycles the same way, we don't want to "summarize and explain"
           variables !). *)
        if register_abbrev then abbreviations := ty :: !abbreviations
      )
      else (
        (* Descend on the type's structure. *)
        match ty.W_Algebra.sty_desc with
        | W_Algebra.SType_var _ -> ()                       (* Done above. *)
        | W_Algebra.SType_arrow (args_tys, res_ty) ->(
            (*print depending on print level*)
            if (  ty.W_Algebra.sty_desc == (context.error_type1).W_Algebra.sty_desc
               || ty.W_Algebra.sty_desc == (context.error_type2).W_Algebra.sty_desc
               )
             then (
               if context.print_level = Function
                then Format.fprintf ppf "function"
               else if context.print_level = Function_With_N_Arguments
               then
                 let n = List.length args_tys in
                 Format.fprintf ppf "%d-argument%s function" n (plurial n)
             )
           else if not (context.print_level == Complete_Type)
             then (
               let err_ty1 = (context.error_type1).W_Algebra.sty_desc in
               let err_ty2 = (context.error_type2).W_Algebra.sty_desc in
               match ( W_SubTerms.locate_subterms err_ty1
                         ty.W_Algebra.sty_desc
                     , W_SubTerms.locate_subterms err_ty2
                         ty.W_Algebra.sty_desc) with
                | (Some (_, str), None) ->
                   Format.fprintf ppf "function with %s %a"
                    str (__pp_simple_type 1) context.error_type1
                | (None,  Some (_, str)) ->
                   Format.fprintf ppf "function with %s %a"
                    str (__pp_simple_type 1) context.error_type2
               | (Some (_, str1), Some (_, str2)) ->
                   Format.fprintf ppf "function with %s %a and %s %a"
                    str1 (__pp_simple_type 1) context.error_type1
                    str2 (__pp_simple_type 1) context.error_type2
              | _ ->
                    let tmp = context.print_level in
                    context.print_level <- Complete_Type;
                    Format.fprintf ppf "%a" (__pp_simple_type 2) ty;
                    context.print_level <- tmp
            )
           else (
            if prio >= 2 then Format.fprintf ppf "@[<1>("
            else Format.fprintf ppf "@[" ;
            Format.fprintf ppf "%a@ ->@ %a"
              (__pp_comma_separeted_simple_types 2) args_tys
              (__pp_simple_type 1) res_ty ;
            if prio >= 2 then Format.fprintf ppf ")@]"
            else Format.fprintf ppf "@]"
          )
        )
        | W_Algebra.SType_named { W_Algebra.nst_name = name ;
                                  W_Algebra.nst_args = args ;
                                  W_Algebra.nst_unwinded = _manifest } ->
            (* We never print the real representation of a named type. *)
            Format.fprintf ppf "%a@," pp_type_ident name;
            (* Only if there are parameters to the type constructor, print them
               separated by a comma and enclosed between parentheses. Since
               arguments of the constructor are always enclosed by parens,
               priority lowers to 0. *)
            if args <> [] then
              Format.fprintf ppf "@[<1>(%a)@]"
                (__pp_comma_separeted_simple_types 0) args ;
            (* <---------- DEBUG
            (match _manifest with
             | None -> ()
             | Some t ->
               Format.fprintf ppf "@[<1>=@ %a@]" (__pp_simple_type 0) t)
            <---------- END DEBUG *)
        | W_Algebra.SType_sum_of_records sumcases_column ->
            __pp_column_type ppf sumcases_column
        | W_Algebra.SType_forall scheme ->
            Format.fprintf ppf "@[!%a%a@]"
              __pp_scheme_parameters
              (scheme.W_Algebra.ty_parameters,
               scheme.W_Algebra.row_parameters,
               scheme.W_Algebra.column_parameters)
              (__pp_simple_type prio) scheme.W_Algebra.body
      )
    )
  | _ (* Other markers. *) -> assert false



(* ************************************************************************** *)
(** {b Descr}: Simply prints the rows of a column type, separating each by a
    trailing "/", except for the last one. This allows to avoid spurious
    trailing "/" in case the column hosting the rows is closed.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __pp_rows_of_column ppf = function
  | [] -> ()
  | [last_row] -> Format.fprintf ppf "%a" __pp_row_type last_row
  | h :: q ->
      Format.fprintf ppf "%a /@ " __pp_row_type h ;
      __pp_rows_of_column ppf q



and __pp_column_type ppf column =
  (* No need to "repr" since it was already done by
     [__prepare_print_column_type]. *)
  let (col_rows, col_ending) = column.W_Algebra.ct_value in
  Format.fprintf ppf "@[" ;
  let print_col_ending_as =
    (match col_ending with
     | W_Algebra.Closed_column -> ""
     | W_Algebra.Var_column v -> (
         match v.W_Algebra.cv_mark with
         | W_Algebra.VM_print_sequence_abbrevd abb_name ->
             (* Now that the column will be re-print once for this new printing
                sequence step, we can ask for it to be printed next as its
                abbreviation. *)
             v.W_Algebra.cv_mark <-
               W_Algebra.VM_print_final_abbrevd abb_name ;
             col_vars_to_reprint_if_sequence :=
               v :: !col_vars_to_reprint_if_sequence ;
             abb_name
         | W_Algebra.VM_print_prep_seen _
         | W_Algebra.VM_print_prep_seen_not_rec  ->
             let abb_name =
               create_column_variable_name
                 (v.W_Algebra.cv_level = W_CoreTypes.generic_binding_level) in
             v.W_Algebra.cv_mark <- W_Algebra.VM_print_final_abbrevd abb_name ;
             col_vars_to_reprint_if_sequence :=
               v :: !col_vars_to_reprint_if_sequence ;
             abb_name
         | W_Algebra.VM_print_final_abbrevd abb_name ->
             (* The column ending has already be aliased, hence this means that
                it has already be seen. *)
             abb_name
         | _ (* Other markers. *) -> assert false)) in
  Format.fprintf ppf "%a" __pp_rows_of_column col_rows ;
  (* Only if something was printed before and we still have to print something
     we must print a separating "/". *)
  if (col_rows <> []) && (print_col_ending_as <> "") then
    Format.fprintf ppf " /@ " ;
  if print_col_ending_as <> "" then
    Format.fprintf ppf "%s" print_col_ending_as ;
  Format.fprintf ppf "@]"



(* ************************************************************************** *)
(** {b Descr}: Simply prints the fields of a row type, separating each by a
    trailing ";", except for the last one. This allows to avoid spurious
    trailing ";" in case the row hosting the fields is closed.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __pp_fields_of_row ppf = function
  | [] -> ()
  | [(last_field_label, last_field_ty)] ->
      Format.fprintf ppf "@[%s" last_field_label ;
      if not (is_void last_field_ty) then
        Format.fprintf ppf ":@ %a%s" (__pp_simple_type 0) last_field_ty
         (if context.print_fake_record then "; ..." else "");
      Format.fprintf ppf "@]"
  | (field_label, field_ty) :: q ->
      Format.fprintf ppf "@[%s" field_label  ;
      if not (is_void field_ty) then
        Format.fprintf ppf ":@ %a" (__pp_simple_type 0) field_ty ;
      Format.fprintf ppf ";@]@ " ;
      __pp_fields_of_row ppf q



and __pp_row_type ppf row =
  (* No need to "repr" since it was already done by
     [__prepare_print_row_type]. *)
  let (row_fields, row_ending) = row.W_Algebra.rt_value in
  Format.fprintf ppf "@[<2>{ " ;
  let print_row_ending_as =
    (match row_ending with
     | W_Algebra.Closed_row -> ""
     | W_Algebra.Var_row v -> (
         match v.W_Algebra.rv_mark with
         | W_Algebra.VM_print_sequence_abbrevd abb_name ->
             (* Now that the row will be re-print once for this new printing
                sequence step, we can ask for it to be printed next as its
                abbreviation. *)
             v.W_Algebra.rv_mark <- W_Algebra.VM_print_final_abbrevd abb_name ;
             row_vars_to_reprint_if_sequence :=
               v :: !row_vars_to_reprint_if_sequence ;
             abb_name
         | W_Algebra.VM_print_prep_seen _
         | W_Algebra.VM_print_prep_seen_not_rec  ->
             let abb_name =
               create_row_variable_name
                 (v.W_Algebra.rv_level = W_CoreTypes.generic_binding_level) in
             v.W_Algebra.rv_mark <- W_Algebra.VM_print_final_abbrevd abb_name ;
             row_vars_to_reprint_if_sequence :=
               v :: !row_vars_to_reprint_if_sequence ;
             abb_name
         | W_Algebra.VM_print_final_abbrevd abb_name ->
             (* The row ending has already be aliased, hence this means that
                it has already be seen. *)
             abb_name
         | _ (* Other markers. *) -> assert false)) in
  (* Sort the fields in alphabetical order on their name to ease finding a field
     name or compare 2 rows in case of error. *)
  let sorted_row_fields =
    List.sort (fun (n1, _) (n2, _) -> compare n1 n2) row_fields in
  Format.fprintf ppf "%a" __pp_fields_of_row sorted_row_fields ;
  (* Only if something was printed before and we still have to print something
     we must print a separating ";". *)
  if (sorted_row_fields <> []) && (print_row_ending_as <> "") then
    Format.fprintf ppf ";@ " ;
  if print_row_ending_as <> "" then
    Format.fprintf ppf "%s" print_row_ending_as ;
  Format.fprintf ppf " }@]"



(* ************************************************************************** *)
(** {b Descr}: Internal function (not to be exported or called at toplevel)
    that prints a list of [W_Algebra.simple_type] separating them by a ","
    character.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
and __pp_comma_separeted_simple_types prio ppf = function
  | [] -> ()
  | [last] -> Format.fprintf ppf "%a" (__pp_simple_type prio) last
  | h :: q ->
      Format.fprintf ppf "%a,@ %a"
        (__pp_simple_type prio) h (__pp_comma_separeted_simple_types prio) q



(* ************************************************************************** *)
(** {b Descr}: Just an helper providing a pretty printer for the list of
    generalized variables of a type scheme.                                   *)
(* ************************************************************************** *)
and __pp_scheme_parameters
    ppf (ty_parameters, row_parameters, column_parameters) =
  if ty_parameters <> [] || row_parameters<> [] || column_parameters <> [] then
    (
      Format.fprintf ppf "@[<1>for all@ ";
      (* For type variables, directly take benefit from the [simple_type] pretty
         print function since by construction, generalized type variables are
         [simple_type]s always being ... [SType_var]. *)
      List.iter
        (fun v ->
          (* No need to deal with [VM_print_sequence_abbrevd] and
             [VM_print_final_abbrevd] since here types are variables, hence
             are not subject to abbreviation. *)
          Format.fprintf ppf "%a@ " (__pp_simple_type 0) v)
        ty_parameters ;
      List.iter
        (fun v ->
           let abb_name =
             create_row_variable_name
               (v.W_Algebra.rv_level = W_CoreTypes.generic_binding_level) in
           (* Set the marker so that printing routine won't believe that the
              row variable being already seen it must be printed as an
              abbreviation when we will be printing the body of the type
              forall. *)
           v.W_Algebra.rv_mark <- W_Algebra.VM_print_sequence_abbrevd abb_name ;
           Format.fprintf ppf "%s@ " abb_name)
        row_parameters ;
      List.iter
        (fun v ->
           let abb_name =
             create_column_variable_name
               (v.W_Algebra.cv_level = W_CoreTypes.generic_binding_level) in
           (* Same remark than for row variables about printing not as an
              abbreviation when we will be printing the body of the type
              forall. *)
           v.W_Algebra.cv_mark <- W_Algebra.VM_print_sequence_abbrevd abb_name ;
           Format.fprintf ppf "%s@ " abb_name)
        column_parameters ;
      Format.fprintf ppf "@].@ "
    )



(* ************************************************************************** *)
(** {b Descr}:  Used to initiate pretty-print of abbreviations. In other words,
    because the type to print of mandatorily [TM_print_final_abbrevd], if we
    use the regular pretty-print routine, we will get printed ... its
    abbreviation name ! Which is not very interesting !
    So, for the first time, we descent on the type's structure. Then next,
    we fall back on the regular pretty-print routine.
    Note by the way that the type we enter in is mandatorily marked as
    [TM_print_final_abbrevd] because it has been explored first by the
    regular pretty-print routine [__pp_simple_type] and registered as
    abbreviated, hence marked accordingly. In effect, the regular pretty-print
    routine is called before to print the global and initial type (in which
    there is a part that is the abbreviated one we are processing).
    This function must only be called by [__pp_abbreviations].
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let __explain_simple_type_abbrev prio ppf abbreved_ty =
  let abbreved_ty = W_CoreTypes.simple_type_repr abbreved_ty in
  match abbreved_ty.W_Algebra.sty_mark with
  | W_Algebra.TM_print_sequence_abbrevd _
  | W_Algebra.TM_print_final_abbrevd _ -> (
      match abbreved_ty.W_Algebra.sty_desc with
      | W_Algebra.SType_var _ ->
          (* Since variables are never abbreviated, this should never happen. *)
          assert false
      | W_Algebra.SType_arrow (args_tys, res_ty) ->
          if prio >= 2 then Format.fprintf ppf "@[<1>("
          else Format.fprintf ppf "@[" ;
          Format.fprintf ppf "%a@ ->@ %a"
            (__pp_comma_separeted_simple_types 2) args_tys
            (__pp_simple_type 1) res_ty ;
          if prio >= 2 then Format.fprintf ppf ")@]"
          else Format.fprintf ppf "@]"
      | W_Algebra.SType_named { W_Algebra.nst_name = name ;
                                W_Algebra.nst_args = args ;
                                W_Algebra.nst_unwinded = _manifest } ->
          (* We never print the real representation of a named type. *)
          Format.fprintf ppf "%a" pp_type_ident name ;
          (* Only if there are parameters to the type constructor, print them
             separated by a comma and enclosed between parentheses. Since
             arguments of the constructor are always enclosed by parens,
             priority lowers to 0. *)
          if args <> [] then
            Format.fprintf ppf "@[<1>(%a)@]"
              (__pp_comma_separeted_simple_types 0) args ;
          (* <---------- DEBUG
          (match _manifest with
           | None -> ()
           | Some t -> Format.fprintf ppf "@[<1>=@ %a@]" (__pp_simple_type 0) t)
          <---------- END DEBUG *)
      | W_Algebra.SType_sum_of_records sumcases_column ->
          __pp_column_type ppf sumcases_column
      | W_Algebra.SType_forall scheme ->
          Format.fprintf ppf "@[!%a%a@]"
            __pp_scheme_parameters
            (scheme.W_Algebra.ty_parameters,
             scheme.W_Algebra.row_parameters,
             scheme.W_Algebra.column_parameters)
            (__pp_simple_type prio) scheme.W_Algebra.body
    )
  | _ (* Other markers. *) -> assert false



let rec __pp_abbreviations ppf abbreviated_tys =
  (* Clear the found abbreviations list *)
  abbreviations := [] ;
  List.iter
    (fun abbreviated_ty ->
       (* Note that repr has already be done, so no need to do it again. *)
       match abbreviated_ty.W_Algebra.sty_mark with
       | W_Algebra.TM_print_final_abbrevd abb_name
       | W_Algebra.TM_print_sequence_abbrevd abb_name ->
           Format.fprintf ppf "@\n  @[where %s = %a@]"
             abb_name (__explain_simple_type_abbrev 0) abbreviated_ty
       | _ -> assert false)
  abbreviated_tys ;
  (* Check if during abbreviations printing we have found hidden new ones. *)
  let abbreviated_tys2 = !abbreviations in
  (* If yes, go on printing. This must terminate because each time we find
     something new, we mark it as [TM_print_final_abbrevd] so when all is
     marked [TM_print_final_abbrevd], we do not have anything to do. *)
  if abbreviated_tys2 <> [] then
    __pp_abbreviations ppf (List.rev abbreviated_tys2)



(* ************************************************************************** *)
(** {b Descr}: Pretty printer for a simple type. This function first resets the
    known type variables to make sure that the type is printed with variables
    starting from 'a, then 'b and so on, hence preventing having raving
    variables names due to the fact that we previously encountered tons of
    type variables in other types. Then, it simply calls the recursive type
    pretty printer.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let pp_simple_type ppf ty =
  type_variables_counter := 0 ;
  row_variables_counter := 0 ;
  column_variables_counter := 0 ;
  abbreviations := [] ;
  (* Do the pre-print stuff. *)
  __prepare_print_simple_type ty ;
  (* And now really print. *)
  Format.fprintf ppf "@[%a@]" (__pp_simple_type 0) ty ;
  (* Print the abbreviations if some. *)
  __pp_abbreviations ppf (List.rev !abbreviations) ;
  (* Don't forget to cleanup the type. *)
  W_CoreTypes.cleanup_simple_type ty ;
  (* Since we are not printing a sequence of types, we can directly forget all
     the types that should be re-printed once even if they have an already
     known abbreviations. *)
  tys_to_reprint_if_sequence := [] ;
  row_vars_to_reprint_if_sequence := [] ;
  col_vars_to_reprint_if_sequence := []


let pp_fake_simple_type ppf t =
  context.print_fake_record <- true ;
  pp_simple_type ppf t ;
  context.print_fake_record <- false

let (pp_simple_type_start_sequence, pp_simple_type_continue_sequence,
     pp_simple_type_end_sequence, pp_nothing_end_sequence) =
  let printed_tys = ref [] in
  (
   (* *********************************************************************** *)
   (** {b Descr} Body of the [pp_simple_type_start_sequence] function. This
       function works in combinaison with [pp_simple_type_continue_sequence] and
       [pp_simple_type_end_sequence] to allow printing several separate types
       without breaking the sharing between them, i.e. with making visible the
       fact that some parts are common to them.
       This function must be called to initiate and print the first of these
       types.
       {b Visibility}: Exported outside this module.                          *)
   (* *********************************************************************** *)
   (fun ppf ty ->
     type_variables_counter := 0 ;
     row_variables_counter := 0 ;
     column_variables_counter := 0 ;
     abbreviations := [] ;
     printed_tys := [] ;
     (* Do the pre-print stuff. *)
     __prepare_print_simple_type ty ;
     (* And now really print. *)
     Format.fprintf ppf "@[%a@]" (__pp_simple_type 0) ty ;
     printed_tys := ty :: !printed_tys ;
     (* To make so that types previously printed in the sequence will be
        re-printed once without abbreviation in a future, toggle their marker
        [W_Algebra.TM_print_final_abbrevd] into
        [W_Algebra.TM_print_sequence_abbrevd]. *)
     toggle_final_abbrevd_for_sequence
       !tys_to_reprint_if_sequence !row_vars_to_reprint_if_sequence
       !col_vars_to_reprint_if_sequence ;
     (* Now, forget theses types since they are changed in place. *)
     tys_to_reprint_if_sequence := [] ;
     row_vars_to_reprint_if_sequence := [] ;
     col_vars_to_reprint_if_sequence := []),



   (* *********************************************************************** *)
   (** {b Descr} Body of the [pp_simple_type_continue_sequence] function. This
       function works in combinaison with [pp_simple_type_start_sequence] and
       [pp_simple_type_end_sequence].
       More info in the header of [pp_simple_type_start_sequence].
       This function must be called to print all types between (exclusive) the
       first and the last ones of the sequence to print.
       {b Visibility}: Exported outside this module.                          *)
   (* *********************************************************************** *)
   (fun ppf ty ->
     (* Do the pre-print stuff and keep in mind the already seen types and
        variables from the previous calls. *)
     __prepare_print_simple_type ty ;
     (* And now really print. *)
     Format.fprintf ppf "@[%a@]" (__pp_simple_type 0) ty ;
     printed_tys := ty :: !printed_tys ;
     (* To make so that types previously printed in the sequence will be
        re-printed once without abbreviation in a future, toggle their marker
        [W_Algebra.TM_print_final_abbrevd] into
        [W_Algebra.TM_print_sequence_abbrevd]. *)
     toggle_final_abbrevd_for_sequence
       !tys_to_reprint_if_sequence !row_vars_to_reprint_if_sequence
       !col_vars_to_reprint_if_sequence ;
     (* Now, forget theses types since they are changed in place. *)
     tys_to_reprint_if_sequence := [] ;
     row_vars_to_reprint_if_sequence := [] ;
     col_vars_to_reprint_if_sequence := []),



   (* *********************************************************************** *)
   (** {b Descr}: Body of the [pp_simple_type_end_sequence] function. This
       function works in combinaison with [pp_simple_type_start_sequence] and
       [pp_simple_type_continue_sequence].
       More info in the header of [pp_simple_type_start_sequence].
       This function must be called to print the last type of the sequence to
       print. In particular, this has the effect of printing the abbreviations
       values and flushing the "already seen" types by cleaning the markers of
       all of them.
       It is hence **really** important to end the printing sequence by calling
       this function.
       {b Visibility}: Exported outside this module.                          *)
   (* *********************************************************************** *)
   (fun ppf ty ->
     (* Do the pre-print stuff. Like in [pp_simple_type_continue_sequence], we
        keep in mind the already seen types and variables from the previous
        calls. *)
     __prepare_print_simple_type ty ;
     (* And now really print. *)
     Format.fprintf ppf "@[%a@]" (__pp_simple_type 0) ty ;
     (* Print the abbreviations if some. *)
     __pp_abbreviations ppf (List.rev !abbreviations) ;
     printed_tys := ty :: !printed_tys ;
     (* Don't forget to cleanup **all** the printed types. *)
     List.iter W_CoreTypes.cleanup_simple_type !printed_tys ;
     (* Clean the list of printed types, this may save a bit of memory by
        directly releasing these types and leaving the GC getting them. *)
     printed_tys := [] ;
     (* Since we are at the end of a sequence of types, we can directly forget
        all the types that should be re-printed once even if they have an
        already known abbreviations. *)
     tys_to_reprint_if_sequence := [] ;
     row_vars_to_reprint_if_sequence := [] ;
     col_vars_to_reprint_if_sequence := []),



   (* *********************************************************************** *)
   (** {b Descr}: Body of the [pp_nothing_end_sequence] function. This
       function works in combinaison with [pp_simple_type_start_sequence] and
       [pp_simple_type_continue_sequence].
       This function must be called when a print sequence was started but it
       appears that we have nothing more to print although the last call was
       not [pp_simple_type_end_sequence]. This happens for example when
       [W_ReportErrors.pp_incompatible_types_if_more_precise] is called but
       has nothing more precise to print. If the present function is not
       called, there may remain some uncleared markers in the previously
       printed types, and hence cause assert failures later.
       {b Visibility}: Exported outside this module.                          *)
   (* *********************************************************************** *)
   (fun () ->
      (* Cleanup **all** the printed types. *)
      List.iter W_CoreTypes.cleanup_simple_type !printed_tys ;
      (* Clean the list of printed types, this may save a bit of memory by
         directly releasing these types and leaving the GC getting them. *)
      printed_tys := [] ;
      (* Since we are at the end of a sequence of types, we can directly forget
         all the types that should be re-printed once even if they have an
         already known abbreviations. *)
      tys_to_reprint_if_sequence := [] ;
      row_vars_to_reprint_if_sequence := [] ;
      col_vars_to_reprint_if_sequence := [])
  )



(* ************************************************************************** *)
(** {b Descr}: Pretty printer for a type scheme. It displays the scheme by
    explicitly prefixing its body by the list of generalized variables.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let pp_scheme ppf scheme =
  type_variables_counter := 0 ;
  row_variables_counter := 0 ;
  column_variables_counter := 0 ;
  abbreviations := [] ;
  let scheme_params =
    (scheme.W_Algebra.ty_parameters,
     scheme.W_Algebra.row_parameters,
     scheme.W_Algebra.column_parameters) in
  (* Do the pre-print stuff. *)
  __prepare_print_scheme_parameters scheme_params ;
  __prepare_print_simple_type scheme.W_Algebra.body ;
  (* And now really print. *)
  Format.fprintf ppf "@[%a" __pp_scheme_parameters scheme_params ;
  Format.fprintf ppf "%a@]" (__pp_simple_type 0) scheme.W_Algebra.body ;
  (* Print the abbreviations if some. *)
  __pp_abbreviations ppf (List.rev !abbreviations) ;
  (* Don't forget to cleanup the scheme's body. Also do so on the types
     representing generalized variables in case they would not be bound in the
     type (pathological case like type ('a) t = int). *)
  List.iter W_CoreTypes.cleanup_simple_type scheme.W_Algebra.ty_parameters ;
  W_CoreTypes.cleanup_simple_type scheme.W_Algebra.body



let print_simple_type = pp_simple_type Format.std_formatter
let print_scheme = pp_scheme Format.std_formatter
