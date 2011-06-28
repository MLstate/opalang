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

(* ************************************************************** *)
(** {b Descr}: This module provides pretty-printing functions for
    [QmlTyperException.t] information embedded in an exception
    [QmlTyperException.Exception].                                *)
(* ************************************************************** *)




(* depends *)
module Format = Base.Format

(* alias *)

(* shorthand *)
module Q = QmlAst
module TExc = QmlTyperException


(* -- *)



(* ************************************************************************** *)
(** {b Descr}: See .mli file.
    Prints an ending \n.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let pp_typer_error ~type_printer ?(highlight_printer = Base.identity)
    ?(ident_printer = QmlPrint.pp#ident) ppf (_loc, e) =
  let print_ty ty = highlight_printer type_printer ty in
  match e with
  | TExc.InvalidExpression ->
      Format.fprintf ppf
        ("The@ expression@ is@ invalid@ (e.g.@ a@ record@ with@ twice@ the@ " ^^
         "same@ field).@\n")
  | TExc.InvalidType (t, kind) -> (
      Format.fprintf ppf "The@ type@  %a @ is@ invalid" print_ty t ;
      match kind with
      | `duplicate_field ->
          (* E.g. type a = {b : int; b: int}. *)
          Format.fprintf ppf
            (",@ because@ two@ fields@ in@ the@ record@ have@ the@ same@ " ^^
               "name.@\n")
      | `duplicate_field_with_diff_ty_in_sum_cases ->
          (* E.g. type t = {a : float} / {a : int}. *)
          Format.fprintf ppf
            (",@ because@ two@ records@ in@ the@ union@ have@ a@ same@ " ^^
               "field@ with@ different@ types.@\n")
      | `not_a_record ->
          Format.fprintf ppf
            ",@ because@ a@ part@ of@ the@ union@ is@ not@ a@ record.@\n"
      | `record_not_closed ->
          Format.fprintf ppf
            ",@ because@ a@ part@ of@ the@ union@ contains@ row@ variables.@\n"
      | `abstract_in_ty_annotation ->
          Format.fprintf ppf
            (",@ because@ type@ expression@ contains@ \"external\"@ which@ " ^^
             "is@ only@ legal@ in@ type@ definitions.@\n")
      | `other -> Format.fprintf ppf "@\n"
    )
  | TExc.InvalidTypeDefinition (typedef, ty) ->
      Format.fprintf ppf
        "The@ definition@ of@ type@  %a @ as@  %a @ is@ invalid.@\n"
        print_ty typedef print_ty ty
  | TExc.InvalidTypeUsage (name, vl, vr) ->
      Format.fprintf ppf
        ("The@ type@ instanciation@ for@  @{<red>%a@} @ is@ invalid.@ " ^^
         "According@ to@ the@ definition@ of@ this@ type,@ it@ should@ " ^^
         "have@ @{<green>%d@}@ argument(s).@ However,@ here,@ it@ is@ " ^^
         "applied@ to@ @{<red>%d@}@ argument(s).@\n")
        QmlPrint.pp#typeident name (List.length vl) (List.length vr)
  | TExc.InvalidUnification (t1, t2, ts_opt) -> (
      Format.fprintf ppf
        ("This@ expression@ has@ type@  %a .@\nHowever,@ according@ to@ " ^^
         "the@ context,@ it@ seems@ that@ it@ should@ have@ type@  %a .@\n")
        print_ty t1 print_ty t2 ;
      match ts_opt with
      | Some (t3, t4) ->
          (* [TODO] Don't print different types, if they look the same. *)
          Format.fprintf ppf
            ("The@ types@ are incompatible,@ because@ type@  %a@ and@ " ^^
             "type@  %a @ are@ incompatible.@\n")
              print_ty t3 print_ty t4
      | None -> ())
  | TExc.InternalError s -> Format.fprintf ppf "Internal@ Error:@ %s@\n" s
  | TExc.IdentifierNotFound (eid, _) ->
      Format.fprintf ppf
        "The value@  @{<red>%a@} @ is@ not@ defined.@\n" ident_printer eid
  | TExc.TypeIdentNotFound tid ->
      Format.fprintf ppf
        "The type@  @{<red>%a@} @ is@ not@ defined.@\n"
        QmlPrint.pp#typeident tid
  | TExc.MatchNamedTypeProblem ->
      Format.fprintf ppf
        ("No@ named@ type@ found@ for@ a@ pattern@ match@ and@ row@ " ^^
         "variables@ prevent@ the@ creation@ of@ the@ anonymous@ sum.@\n")
  | TExc.UnableToTypeBypass bsl ->
      Format.fprintf ppf
        "Unable@ to@ type@ bypass@  @{<red>%s@}.@\n" (BslKey.to_string bsl)
  | TExc.NotImplementedYet s ->
      Format.fprintf ppf " @{<red>%s@} @ not@ yet@ implemented.@\n" s
  | TExc.DuplicateTypeDefinitions s ->
      Format.fprintf ppf
        "There@ are@ duplicate@ definitions@ for@ type@  @{<red>%s@} .@\n" s
  | TExc.ExpansiveExprAtTopLevel ->
      Format.fprintf ppf
        ("Cannot@ define@ this@ expansive@ expression,@ try@ to@ " ^^
         "explicit@ arguments.@\n")



(* ************************************************************************** *)
(** {b Descr}: Collects the cases missing between the 2 sums passed as
    argument, [~complete] being considered as the sum with the most cases and
    [incomplete] the one with the less cases.
    The missing cases are returned as a column, i.e. as the sum type hosting
    all the cases found missing.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let get_missing_cases ~complete: (Q.TyCol (al, aopt))
    ~incomplete: (Q.TyCol (bl, bopt)) =
  let rl =
    List.fold_left
      (fun (acc: (string * 'a) list list) (fields: (string * 'a) list) ->
         (* If there is one case of [bl] containing [fields], then drop
            [fields], otherwise keep it. *)
         if List.exists
           (fun fields' ->
              List.for_all
                (fun (one_name, _) ->
                   List.exists
                     (fun (one_name', _) -> one_name = one_name')
                     fields')
                fields)
           bl
         then acc
         else fields :: acc)
      [] al in
  let ropt =
    match (aopt, bopt) with
    | (None, None) -> None
    | (_, Some _) -> None
    | ((Some _ as opt), None) -> opt in
  Q.TyCol (rl, ropt)



(* ************************************************************************** *)
(** {b Descr}: Prints the fields names separated by a comma and starting by
    "a field named" or "fields named" depending on if the list contains one
    or several fields names.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let pp_fields_names ppf = function
  | [] -> Format.fprintf ppf "no@ field"
  | [x]-> Format.fprintf ppf "a@ field@ named@ @{<red>%s@}" x
  | h :: q ->
      Format.fprintf ppf "fields@ named@ @{<red>%s@}" h ;
      List.iter (fun n ->  Format.fprintf ppf ",@ @{<red>%s@}" n) q



(* ************************************************************************** *)
(** {b Descr}: Convert an integer considered as expressing an ranking order
    into a redeable string. This is used to handle the English exceptions
    "1st", "2nd" and "3rd" for ranks 1, 2 and 3.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let order_number_to_string = function
  | 1 -> "1st"
  | 2 -> "2nd"
  | 3 -> "3rd"
  | n -> (string_of_int n) ^ "th"



(* ************************************************************************** *)
(** {b Descr}: Find strings among the list [l] with spelling close to the
    string [typo]. Spelling-based neighbours are returned in a list of strings
    sorted by increasing distance, i.e. with string closest from [typo] first.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let get_closest_names typo l =
  let distances =
    List.map (fun name -> (name, Sed.sed Sed.qwerty_distance name typo)) l in
  List.sort compare distances



let resolve_type gamma typ =
  match typ with
  | Q.TypeName (ty, typeident) -> (
      match
        QmlTypes.Env.TypeIdent.find_opt
          ~visibility_applies: true typeident gamma with
      | None -> typ
      | Some tsc -> QmlTypes.Scheme.specialize ~typeident ~ty tsc
    )
  | _ -> assert false



(* This function assumes that we are rid of [TypeIdent]. *)
let rec pp_advice gamma ppf = function
  | QmlTyperException.InvalidTypeDefinition _ ->
      Format.fprintf ppf
        ("@[Diagnosis:@ possibly,@ a@ type@ variable@ appears@ in@ the@ " ^^
         "body@ of@ the@ definition@ but@ not@ in@ the@ list@ of@ " ^^
         "parameters,@ or@ the@ definition@ is@ trivially@ cyclic@ like@ " ^^
         "in@ @{<red>type t = t@},@ or@ it@ contains@ several@ times@ a@ " ^^
         "same@ variable,@ or@ the@ definition@ is@ a@ sum@ containing@ " ^^
         "several@ cases@ having@ the@ same@ labels@ like@ " ^^
         "@{<red>{ l } / { l }@}@ or@ " ^^
         "@{<red>{ l : bool } / { l : int }@}.@]@\n")
  | QmlTyperException.InvalidUnification
        (
          (*Looking for*)(Q.TypeRecord (Q.TyRow (found, _))
                          |
                          Q.TypeSum (Q.TyCol ([found], _))),
          (*Found*)      (Q.TypeRecord (Q.TyRow ([name, _], _))
                          |
                          Q.TypeSum (Q.TyCol ([[name, _]], _))),
                         _) ->    (* Here, a field is missing. *)
      Format.fprintf ppf
        ("@[Diagnosis:@ It@ seems@ that@ you@ are@ expecting@ this@ record@ " ^^
         "to@ have@ a@ field@ named@ @{<red>%s@}.@ However,@ according@ " ^^
         "to@ the@ definition@ of@ this@ record@ and/or@ with@ previous@ " ^^
         "uses@ of@ this@ record,@ this@ record@ doesn't@ have@ any@ " ^^
         "field@ with@ this@ name.")
        name ;
      (* Try to print suggestions if some are found. *)
      (match get_closest_names name (List.map fst found) with
       | [] ->  ()    (* No idea or empty record. *)
       | [name, _] ->
           Format.fprintf ppf "@ Perhaps@ you@ meant@ @{<red>%s@}?" name
       | (name_1, _) :: (name_2, _) :: _ ->
           Format.fprintf ppf
             ("@ Perhaps@ you@ meant@ @{<red>%s@}@ or@ " ^^
              "@{<red>%s@}?") name_1 name_2) ;
      Format.fprintf ppf "@]@\n"
  | QmlTyperException.IdentifierNotFound (name, bound_in_scope) ->
      let name = Ident.original_name name in
      let bound_in_scope =
        List.map Ident.original_name bound_in_scope in
      (match get_closest_names name bound_in_scope with
       | [] -> Format.fprintf ppf "@\n"            (* No idea. *)
       | [(name, _)] ->
           Format.fprintf ppf
             ("@[Diagnosis:@ This@ identifier@ doesn't@ exist.@ Perhaps@ " ^^
              "you@ meant@ @{<red>%s@}?@]@\n")
             name
       | (name_1, _) :: (name_2, _) :: _ ->
           Format.fprintf ppf
             ("@[Diagnosis:@ This@ identifier@ doesn't@ exist.@ Perhaps@ " ^^
              "you@ meant@ @{<red>%s@}@ or@ @{<red>%s@}?@]@\n")
             name_1 name_2)
  | QmlTyperException.InvalidUnification (
        (Q.TypeRecord (Q.TyRow (have_fields, _))
         |
         Q.TypeSum (Q.TyCol ([have_fields], _))),
        (Q.TypeRecord (Q.TyRow (expected_fields, _))
         |
         Q.TypeSum (Q.TyCol ([expected_fields], _))),
        _) ->
      (* Here, we may have several fields missing. *)
      let expected_names = List.map fst expected_fields in
      let have_names = List.map fst have_fields in
      (match BaseList.subtract expected_names have_names with
       | [] ->
           (* No missing name. The type error is due to a field having the
              wrong type.*)
           Format.fprintf ppf
             ("@[Diagnosis:@ It@ seems@ that@ some@ of@ the@ fields@ of@ " ^^
              "this@ record@ do@ not@ have@ the@ expected@ type.@]@\n")
       | l  ->
           let plural_or_singular_end_of_sentence =
             (match l with
              | [] -> assert false
              | [_] -> "such a field"
              | _ -> "such fields") in
           Format.fprintf ppf
             ("@[Diagnosis:@ It@ seems@ that@ you@ are@ expecting@ this@ " ^^
              "record@ to@ have@ %a.@ However,@ according@ to@ the@ " ^^
              "definition@ of@ this@ record@ and/or@ with@ previous@ uses@ " ^^
              "of@ this@ record,@ this@ record@ doesn't@ have@ %s.@]@\n")
             pp_fields_names l plural_or_singular_end_of_sentence)
  | QmlTyperException.InvalidUnification
        ((Q.TypeSum t1), (Q.TypeSum t2), None) ->
      (* Note: we're only doing this with [None], to make sure that we're
         not displaying conflicting error messages. *)
      (* Missing cases *)
      let v1 = get_missing_cases ~complete: t1 ~incomplete: t2 in
      (match v1 with
       | Q.TyCol ([], None) -> ()
       | Q.TyCol ([], Some _) ->
           Format.fprintf ppf
             ("@[Diagnosis:@ This@ expression@ is@ an@ open@ sum.@ To@ " ^^
              "access@ its@ contents,@ you@ need@ to@ use@ " ^^
              "pattern-matching@ with@ a@ catch-all@ pattern,@ to@ make@ " ^^
              "sure@ that@ all@ possible@ cases@ are@ handled.@]@\n")
       | _ ->
           Format.fprintf ppf
             ("@[Diagnosis:@ This@ expression@ is@ a@ sum@ and@ could@ be@ " ^^
              "evaluated@ to@ records@ with@ several@ distinct@ " ^^
              "structures.@ While@ you@ handle@ some@ of@ the@ possible@ " ^^
              "structures,@ it@ seems@ that@ you@ have@ forgotten@ to@ " ^^
              "handle@ some@ cases,@ such@ as@ @{<red>%a@}.@]@\n")
             QmlPrint.pp#ty (Q.TypeSum v1))
  | QmlTyperException.InvalidUnification
      (Q.TypeSum t1, Q.TypeRecord (Q.TyRow (expected_fields, _dots)), _) ->
      (* Missing cases. *)
      let t2 = Q.TyCol ([expected_fields], None) in
      let v1 = get_missing_cases ~complete: t1 ~incomplete: t2 in
      (match v1 with
       | Q.TyCol ([], None) -> ()     (* Probably a deeper type error. *)
       | Q.TyCol ([], Some _) ->
           Format.fprintf ppf
             ("@[Diagnosis:@ This@ expression@ is@ an@ open@ sum.@ To@ " ^^
              "access@ its@ contents,@ you@ need@ to@ use@ " ^^
              "pattern-matching@ with@ a@ catch-all@ pattern,@ to@ make@ " ^^
              "sure@ that@ all@ possible@ cases@ are@ handled.@]@\n")
       | _ ->
           Format.fprintf ppf
             ("@[Diagnosis:@ It@ seems@ that@ you@ are@ using@ this@ " ^^
              "expression@ as@ if@ it@ were@ a@ record@ containing@ %a.@ " ^^
              "However,@ this@ expression@ is@ a@ sum@ and@ could@ be@ " ^^
              "evaluated@ to@ other@ records,@ with@ structures@ such@ as@ " ^^
              "@{<red>%a@}.@]@\n")
             pp_fields_names (List.map fst expected_fields)
             QmlPrint.pp#ty (Q.TypeSum v1)
      )
  | QmlTyperException.InvalidUnification
      ((Q.TypeArrow (t1, _t2)), (Q.TypeArrow (u1, _u2)), _) ->
      let len1 = List.length t1 in
      let len2 = List.length u1 in
      if len1 < len2 then
        Format.fprintf ppf
          ("Diagnosis:@ This@ expression@ is@ a@ function@ and@ it@ takes@ " ^^
           "@{<red>%d@}@ arguments.@ However,@ it@ seems@ that@ you@ are@ " ^^
           "applying@ only@ @{<red>%d@}@ argument(s).@\n")
          len2 len1
      else
        if len1 > len2 then
          Format.fprintf ppf
            ("@[Diagnosis:@ This@ expression@ is@ a@ function@ and@ it@ " ^^
             "takes@ only@ @{<red>%d@}@ arguments.@ However,@ it@ seems@ " ^^
             "that@ you@ are@ applying@ @{<red>%d@}@ argument(s).@]@\n")
            len2 len1
        else
          let can_unify a b =
            let tv = Q.TypeVar (Q.TypeVar.next()) in
            (QmlMoreTypes.unifiable ~gamma tv a) &&
            (QmlMoreTypes.unifiable ~gamma tv b) in
          let rec aux t u i =
            match (t, u) with
            | ([], []) ->
                (* should possibly look at t2 and u2 *)
                Format.fprintf ppf
                  ("@[Diagnosis:@ This@ expression@ is@ a@ function.@ It@ " ^^
                   "seems@ that@ the@ result@ of@ this@ function@ has@ an@ " ^^
                   "incorrect@ type.@]@\n")
            | (t1 :: ts, u1 :: us) ->
                begin
                  if can_unify t1 u1 then
                    (* Ok, the error is probably not at this argument. *)
                    aux ts us (i + 1)
                  else
                    (* Well, the error is here. *)
                    match (ts, us) with
                    | ([], []) ->
                        Format.fprintf ppf
                          ("@[Diagnosis:@ This@ expression@ is@ a@ " ^^
                           "function.@ The@ %s@ argument@ seems@ to@ have@ " ^^
                           "an@ incorrect@ type.@]@\n")
                          (order_number_to_string i)
                    | (t2 :: _, u2 :: _) ->
                        if can_unify t1 u2 && can_unify t2 u1 then
                          (* Ok, we may have exchanged two arguments. *)
                          Format.fprintf ppf
                            ("@[Diagnosis:@ This@ expression@ is@ a@ " ^^
                             "function.@ You@ may@ have@ switched@ " ^^
                             "arguments@ %d@ and@ %d.@]@\n")
                            i (i + 1)
                        else
                          Format.fprintf ppf
                            ("@[Diagnosis:@ This@ expression@ is@ a@ " ^^
                             "function.@ The @{<red>%s@}@ argument@ seems@ " ^^
                             "to@ have@ an@ incorrect@ type.@]@\n")
                            (order_number_to_string i)
                    | _ ->
                        (* We have checked that the two lists have distinct
                           values. That's an internal error. *)
                        assert false
                end
            | _ ->
                (* We have checked that the two lists have distinct values.
                   That's an internal error. *)
                assert false in
          aux t1 u1 1
  | QmlTyperException.InvalidUnification ((Q.TypeArrow _), _, _)
  | QmlTyperException.InvalidUnification (_, (Q.TypeArrow _), _) ->
      (* Not a function. *)
      Format.fprintf ppf
        ("@[Diagnosis:@ It@ seems@ that@ you@ expect@ this@ expression@ to@ " ^^
         "be@ a@ function,@ although@ it@ isn't.@]@\n")
  | QmlTyperException.MatchNamedTypeProblem ->
      Format.fprintf ppf
        ("@[Diagnosis:@ You@ may@ wish@ to@ add@ a@ coercion@ around@ this@ " ^^
         "expression@ for@ a@ more@ detailed@ diagnosis.@]@\n")
  | _ ->  ()



(* ************************************************************************** *)
(** {b Descr}: Prints an error message describing the typechecking error kind
    and an advice to solve it is some is available.
    Prints an ending \n
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let pp_description_and_advice gamma ppf err_descr =
  (* Printf the core of the error message description. *)
  Format.fprintf ppf "@[Description:@ " ;
  pp_typer_error
    ~ident_printer:
      (fun ppf i -> Format.fprintf ppf "%s" (Ident.original_name i))
    (* Create a new printer for each exception so that the same scope of
       typevars in used throughout the message, but is not the same as in
       other messages. *)
    ~type_printer:  (new QmlPrint.opa_printer)#ty
    ~highlight_printer: (fun pr ppf v -> Format.fprintf ppf "@{<red>%a@}" pr v)
    ppf err_descr ;
  Format.fprintf ppf "@]" ;
  (* Print the more detailled message (advice) if some can be guessed. This
     always prints an ending if a more detailled message is printed\n. *)
  (match snd err_descr with
   | QmlTyperException.InvalidUnification
       ((Q.TypeName(_, _) as old_ty1), (Q.TypeName (_, _) as old_ty2), opt) ->
       let ty1 = resolve_type gamma old_ty1 in
       let ty2 = resolve_type gamma old_ty2 in
       pp_advice
         gamma ppf (QmlTyperException.InvalidUnification (ty1, ty2, opt))
   | QmlTyperException.InvalidUnification
         ((Q.TypeName (_, _) as old_ty1), ty2, opt) ->
       let ty1 = resolve_type gamma old_ty1 in
       pp_advice
         gamma ppf (QmlTyperException.InvalidUnification (ty1, ty2, opt))
   | QmlTyperException.InvalidUnification
         (ty1, (Q.TypeName (_, _) as old_ty2), opt) ->
       let ty2 = resolve_type gamma old_ty2 in
       pp_advice
         gamma ppf (QmlTyperException.InvalidUnification (ty1, ty2, opt))
   | otherwise -> pp_advice gamma ppf otherwise)



(* Doesn't print ending \n. *)
let pp_position_or_code ppf at_location =
  match at_location with
  | `Expr_loc e ->
      (*
      (* FPE says: what's this garbage ? To be inspected later ! *)
      let clean s =
        Base.String.map
          (fun c ->
             if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
               (c >= '0' && c <= '9')
             then c
             else '_')
          s in
      (
        match e.Q.e with
        | Q.LetRecIn ( [], _) | Q.LetRecIn ( [_], _) -> ()
        | Q.LetRecIn (bindings, _) ->
            prerr_endline
              "\nyou can show the dependencies graph with the following dot script :\n just copy and paste the following digraph in a new file, and run :\ndisplay file.dot.";
            prerr_endline "digraph a {";
            let set = IdentSet.from_list (List.map fst bindings) in
            List.iter
              (fun (ident, expr) ->
                 QmlAstWalk.Expr.iter_down
                   (fun e ->
                      match e.Q.e with
                      | Q.Ident i ->
                          if IdentSet.mem i set then
                            prerr_endline
                              (Format.sprintf
                                 "%s -> %s"
                                 (clean (Ident.original_name ident))
                                 (clean (Ident.original_name i)))
                      | _ -> ())
                   expr
              )
              bindings ;
            prerr_endline "}\n"
        | _ -> ()
      ) ;
      *)
      let pos = Q.Pos.expr e in
      (* If no position available, then print the code instead. *)
      if not (FilePos.is_empty pos) then
        Format.fprintf ppf "%s" (FilePos.to_string pos)
      else
        Format.fprintf ppf
          "  inside expression:@\n  <<@\n  %a@\n  >>" QmlPrint.pp#expr e
  | `Pat_loc pat ->
      let pos = Q.Pos.pat pat in
      if not (FilePos.is_empty pos) then
        Format.fprintf ppf "%s" (FilePos.to_string pos)
      else
        Format.fprintf ppf
          "  inside pattern:@\n  <<@\n  %a@\n  >>" QmlPrint.pp#pat pat
  | `Ty_loc t ->
      (* Always print the code since no location are stored in annotmap for
         types. *)
      Format.fprintf ppf "  inside type expression:@\n  <<@\n  %a@\n  >>"
        QmlPrint.pp#ty t
  | `No_loc -> Format.fprintf ppf "No position available"



(* ************************************************************************** *)
(** {b Descr}: Global error message printing routine for typechecking errors.
    It prints the complete error message by invoking the sub-routines dealing
    with each sub-part of the message.
    The complete message includes error's location, error's kind description
    and an advice to solve if some is available.
    Prints an ending \n
    {b Visibility} : Not exported outside this module.                        *)
(* ************************************************************************** *)
let pp_typer_exception_error gamma _annotmap ppf err_descr =
  let ((at_location, loc_set), _) = err_descr in
  (* Print the location taken from the expression's one, then print the message
     describing the typer error and its possible advice. *)
  Format.fprintf ppf
    "@[%a@\n%a@]"
    pp_position_or_code at_location
    (pp_description_and_advice gamma) err_descr ;
  (* Get extra location that are not "unknown" to eventually print them. *)
  let extra_locations =
    QmlTyperException.LocSet.fold
      (fun loc acc ->
         match loc with
         | `Expr_loc _ | `Pat_loc _ | `Ty_loc _ -> loc :: acc
         | `No_loc -> acc)
      loc_set [] in
  match extra_locations with
  | [] -> ()     (* We have no better hint. *)
  | l  ->
      Format.fprintf ppf "@[The@ problem@ seems@ to@ originate@ from@\n" ;
      List.iter
        (fun extra_loc ->
           Format.fprintf ppf
             "- %a@\n" pp_position_or_code extra_loc)
        l ;
      Format.fprintf ppf ".@]@\n"



(* ************************************************************************** *)
(** {b Descr}: Function that checks in the list of exceptions raised hence
    issuing the coming error message if there is a precise source code location
    available, i.e. something locatign in term of line and column number in a
    source file.
    This is a bit heavy and time-consuming processing, but this is only called
    in case of error. And, in such a case, we are not anymore worried of
    efficiency since the compilation will end.
    We need this to tray as far as we can to give accurate error position even
    when an exception doesn't embedd a code element having a location. This is
    especially the case for type definitions in which there is no available
    location in the core exception describing the error. The most precise
    location is on the definition itself (which is forgotten when we treat the
    exception that finely describes the error). To avoid losing the location,
    at the moment where we still have one, if it appears that there is no more
    location in the nested and deeper exceptions, then we print the one we
    have. Otherwise, we keep the oen of the nested and deeper exceptions.
    This permits to have as far as we can locations and to avoid repeatition if
    it appears that we have some in the nested exceptions. Thsi makes messages
    more precise without making them too verbose.
    REMARK: This is not very satisfactory, but until a deeper refactoring of
    exceptions and errors raising, that better than nothing !
    {b Visibility} : Exported outside this module.                            *)
(* ************************************************************************** *)
let rec exist_precise_main_location annotmap = function
  | [] -> false
  | h :: q -> (
      match h with
      | QmlTypes.Exception (QmlTypes.TyperError (code_elt, (e, excn))) ->
          let ctxt = QmlError.Context.code_elt code_elt in
          let loc = QmlError.Context.get_pos ctxt in
          (not (FilePos.is_empty loc)) ||
          (exist_precise_main_location annotmap (e :: excn)) ||
          (exist_precise_main_location annotmap q)
      | QmlTyperException.Exception ((at_location, _), _) ->
          let loc_is_empty =
            (match at_location with
             | `Expr_loc e ->
                 FilePos.is_empty (Q.Pos.expr e)
             | `Pat_loc pat ->
                 FilePos.is_empty (Q.Pos.pat pat)
             | _ -> true) in
          (not loc_is_empty) ||
          (exist_precise_main_location annotmap q)
      | _ -> false
    )



(* ************************************************************************** *)
(** {b Descr}: See .mli file.
    Prints an ending \n.
    {b Visibility} : Exported outside this module.                            *)
(* ************************************************************************** *)
let rec pp_report_from_typer_exception gamma annotmap ppf = function
  | QmlTypes.Exception (QmlTypes.TyperError (code_elt, (e, excn))) ->
      let enqued_excs = e :: excn in
      (* Hack especially rotten to handle source location in type definitions.
         In fact, currently and until deeper refactoring, [`Ty_loc _] do not
         permit to access accurate position in the annotation maps since there
         is not annot key available. This results in poor location in error
         messages. So, just for these case, instead of throwing the
         [code_elt] embedded in the present [QmlTypes.Exception], we print its
         location.
         Otherwise, we let the regular processing acting. *)
      if not (exist_precise_main_location annotmap enqued_excs) then (
        match code_elt with
        | Q.NewType (_, _) ->
            let ctxt = QmlError.Context.code_elt code_elt in
            let loc = QmlError.Context.get_pos ctxt in
            if not (FilePos.is_empty loc) then
              Format.fprintf ppf "%s@\n" (FilePos.to_string loc)
        | _ -> ()) ;
      List.iter (pp_report_from_typer_exception gamma annotmap ppf) enqued_excs
  | QmlTyperException.Exception err_descr ->
      pp_typer_exception_error gamma annotmap ppf err_descr
  | e -> Format.fprintf ppf "%s@\n" (Printexc.to_string e)



let typechecking_exception_handler env exn =
  match exn with
  | QmlTypes.Exception _ ->
      OManager.error "%a"
        (pp_report_from_typer_exception
           env.QmlTypes.gamma env.QmlTypes.annotmap) exn
  | _ -> raise exn



(* As a last resort, register a printer to still get some info from uncaught
   typer exceptions. Do not call [Printexc.to_string] with the current uncaught
   exception to build the final message string otherwise the register mechanism
   will make us stupidly looping ! *)
let _ =
  Printexc.register_printer
    (function
     | QmlTyperException.Exception ((loc, _), e) ->
         pp_typer_error
           ~type_printer: (new QmlPrint.opa_printer)#ty Format.str_formatter
           (loc, e) ;
         Some
           ("\nPlease report. Uncaught QmlTyperException.Exception: " ^
              Format.flush_str_formatter ())
     | _ -> None)
