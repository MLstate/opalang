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
(** {b Descr}: Records the types that have already been expanded during a
    processing. In fact, using this structure, we try to remember on which
    manifest type a *type name with its arguments* was expanded. This means
    that the search key is a type name and a particular list of effective
    arguments. The result of a search is the manifest part of the searched
    type expression.
    In fact, since expansion is done in place, instead of creating an explicit
    map mapping (type name, args list) ---> type being the manifest
    representation, we directly record all the [W_Algebra.named_simple_type]
    we encountered and expanded since they finally contain all the required
    information, key and value once really expanded in place.
    This data-structure allows to avoid looping in case of expanding cyclic
    types and allows to preserve sharing when seeing several occurrences of a
    same named type expression.
    {b Visibility}: Exported opaque outside this module.                      *)
(* ************************************************************************** *)
type expansions_memory = W_Algebra.named_simple_type list




let empty_memory = []



(* ************************************************************************** *)
(** {b Descr}: Function to search for a type name and its arguments in the
    already seen expansions. In other words, this is a kind "compare" on
    [W_Algebra.named_simple_type]. Two elements are equal if they have the
    same name of type and if they are instantiated with arguments that are
    physically equal.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let compare_expansions ex1 ex2 =
  (QmlAst.TypeIdent.equal ex1.W_Algebra.nst_name ex2.W_Algebra.nst_name)
  &&
  (List.for_all2
     (fun arg1_ty arg2_ty -> arg1_ty == arg2_ty)
     ex1.W_Algebra.nst_args ex2.W_Algebra.nst_args)



(* [TODO-REFACTOR] Documentation ! Works by side effect on the type.
   Relies on the fact that simple_type_repr was already called.
   Relies on the fact that [ty] is a [W_Algebra.SType_named]. *)
let _incrementally_expand_abbrev env seen_expansions ty =
  match ty.W_Algebra.sty_desc with
  | W_Algebra.SType_named nty_desc -> (
      match nty_desc.W_Algebra.nst_unwinded with
      | Some already_expanded ->
          (* If this named type has already been unwinded in place, do not do
             it again, simply used the already recorded type. *)
          (already_expanded, seen_expansions)
      | None -> (
          (* So bas, this named type hasn't be already unwinded in place. Try
             to unwind it now. *)
          (try
            (* First, search if a type shaped liek this has already be expanded
               somewhere else. *)
            let found =
              List.find
                (fun ex -> compare_expansions ex nty_desc) seen_expansions in
            nty_desc.W_Algebra.nst_unwinded <- found.W_Algebra.nst_unwinded ;
            (* The type was already seen and previously expanded, so no newly
               seen expansion. Return the untouched list. *)
            let ty' =
              (match found.W_Algebra.nst_unwinded with
              | None -> ty
              | Some t -> t) in
            (ty', seen_expansions)
          with
          | Not_found -> (
              try
                let ty_def_sch =
                  W_TypingEnv.find_type nty_desc.W_Algebra.nst_name env in
                let effective_args =
                  W_TypingEnv.automatically_add_type_construtor_arguments_if_omitted
                    nty_desc.W_Algebra.nst_args
                    (List.length ty_def_sch.W_Algebra.ty_parameters) in
                let ty_vars_mappings =
                  List.combine
                    ty_def_sch.W_Algebra.ty_parameters effective_args in
                let expanded_ty =
                  W_SchemeGenAndInst.specialize_with_given_variables_mapping
                    ~deep: true ty_vars_mappings [] [] ty_def_sch in
                (* Attention, unification strongly rely for the named type once
                   expanded to be the same type with an effective manifest
                   representation. So, do not make a link. Instead, set the
                   manifest representation directly inside the type. *)
                nty_desc.W_Algebra.nst_unwinded <- Some expanded_ty ;
                (* Return the list of already seen expansions with this newly
                   seen expansion done. *)
                (expanded_ty, (nty_desc :: seen_expansions))
              with (Not_found | W_TypingEnv.Importing_qml_abstract_ty) ->
                (ty, seen_expansions)
             )
          )
         )
     )
  | _ -> assert false



let rec _expand_abbrev_n_times nb_expansion env seen_expansions ty =
  if nb_expansion = 0 then (ty, seen_expansions)
  else (
    let (ty', seen_expansions') =
      _incrementally_expand_abbrev env seen_expansions ty in
    if ty == ty' then (ty, seen_expansions')
    else _expand_abbrev_n_times (nb_expansion - 1) env seen_expansions' ty'
   )


(* ************************************************************************** *)
(** {b Descr}: See .mli file for documentation.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let rec fully_expand_abbrev env seen_expansions ty =
  let ty = W_CoreTypes.simple_type_repr ty in
  match ty.W_Algebra.sty_desc with
  | W_Algebra.SType_named nty_desc -> (
      let seen_expansions' =
        (match nty_desc.W_Algebra.nst_unwinded with
        | None ->
            (* The named type was not yet unwinded. Try to unwind it. *)
            snd (_incrementally_expand_abbrev env seen_expansions ty)
        | Some _ -> seen_expansions) in
      (* Ok, at this point, [ty] was unwinded if possible. So if it manifest
         representation remained [None] then it means that it was no more
         possible to unwind it. *)
      match nty_desc.W_Algebra.nst_unwinded with
      | None -> ty
      | Some manifest_ty -> (
          (* The name type is already or has been unwinded. If its manifest
             representation is still a named type, then recurse to unwind it
             until it is no more a named type or until we can't unwind it
             anymore. *)
          let manifest_ty = W_CoreTypes.simple_type_repr manifest_ty in
          match manifest_ty.W_Algebra.sty_desc with
          | W_Algebra.SType_named _ ->
              fully_expand_abbrev env seen_expansions' manifest_ty
          | _ -> manifest_ty
         )
     )
  | _ -> ty  (* Other types than named types, do nothing. *)



(* ************************************************************************** *)
(* {b Descr}: See .mli file.
   {b Visibility}: Exported outside this module.                              *)
(* ************************************************************************** *)
let deep_exact_occur_expand_abbrev env initial_seen_expansions ~ty_var ~in_ty =

  (* Local function to descend onto the structure of [in_ty]. *)
  let rec rec_check seen_expansions ty =
    let ty = W_CoreTypes.simple_type_repr ty in
    (* If the type we arrive on is exactly the variable, then we have a positive
       occur check. Hence, following all expansions, the variable and the type
       appear to be the same. *)
    if ty == ty_var then true
    else
      (* We are only interested in following named types. *)
      match ty.W_Algebra.sty_desc with
      | W_Algebra.SType_named nty_desc -> (
          (* Ok, the type in which we are looking to see if it is the same
             than the variable is a named types. Let's dissecate it.
             First, ensure that the type variable really appears among the named
             type arguments, otherwise no need to search, we won't find it in
             its structure if it has a manifest structure. *)
          let var_belong_to_args =
            List.exists
              (fun arg_ty ->
                let arg_ty = W_CoreTypes.simple_type_repr arg_ty in
                arg_ty == ty_var)
              nty_desc.W_Algebra.nst_args in
          (* If the variable doesn't belong to the named type arguments, then
             no occurrence is possible. *)
          if not var_belong_to_args then false
          else (
            try
              (* First, have a look in expansions we already saw. *)
              let found =
                List.find
                  (fun ex -> compare_expansions ex nty_desc) seen_expansions in
              (match found.W_Algebra.nst_unwinded with
              | None -> false   (* ALready seen but expanded to nothing. *)
              | Some ty' ->
                  (* Inspect the already seen expansion. *)
                  rec_check seen_expansions ty')
            with Not_found -> (
              (* We never saw previously any expansion for this named type.
                 Let's seen if we can expand it. *)
              try
                let ty_def_sch =
                  W_TypingEnv.find_type nty_desc.W_Algebra.nst_name env in
                let body =
                  W_CoreTypes.simple_type_repr ty_def_sch.W_Algebra.body in
                (* If the body of the scheme bound to the expansion for the
                   named type is not a type variable or another named type, then
                   this means that the type in fact has a structure and is not
                   simply an alias, hence if the type variable appears in this
                   structure then we do not have an exact equality between the
                   initial type and the initial variable and the test is then
                   negative. *)
                (match body.W_Algebra.sty_desc with
                | W_Algebra.SType_var _ | W_Algebra.SType_named _ ->
                    (* Possibly we are following a crude alias. So, we really
                       instantiate the scheme to go on following aliases.*)
                    let effective_args =
                      W_TypingEnv.automatically_add_type_construtor_arguments_if_omitted
                        nty_desc.W_Algebra.nst_args
                        (List.length ty_def_sch.W_Algebra.ty_parameters) in
                    let ty_vars_mappings =
                      List.combine
                        ty_def_sch.W_Algebra.ty_parameters effective_args in
                    let expanded_ty =
                      W_SchemeGenAndInst.specialize_with_given_variables_mapping
                        ~deep: true ty_vars_mappings [] [] ty_def_sch in
                    (* Recurse on the expansion, with an extended set of already
                       seen expansions. *)
                    rec_check (nty_desc :: seen_expansions) expanded_ty
                | _ ->
                    (* Case where the expansion is neither a variable nor a
                       named type. Hence, it is a structured type and the test
                       gets negative. *)
                    false)
                  with (Not_found | W_TypingEnv.Importing_qml_abstract_ty) ->
                    false
             )
           )
         )
      | _ -> false in

  (* Effective body of the function [deep_exact_occur_expand_abbrev]. *)
  rec_check initial_seen_expansions in_ty


let expand_abbrev_n_times nb_expansion env seen_expansions ty =
  let (t, m) = _expand_abbrev_n_times nb_expansion env seen_expansions ty in
  W_TypeInfo.addrec_linked_object t.W_Algebra.sty_desc ty.W_Algebra.sty_desc ;
  (t, m)

let incrementally_expand_abbrev env seen_expansions ty =
  let (t, m) = _incrementally_expand_abbrev env seen_expansions ty in
  W_TypeInfo.addrec_linked_object t.W_Algebra.sty_desc ty.W_Algebra.sty_desc ;
  (t, m)
