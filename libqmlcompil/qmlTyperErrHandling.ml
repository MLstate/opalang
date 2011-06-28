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
  | TExc.IdentifierNotFound (eid, _) ->
      Format.fprintf ppf
        "The value@  @{<red>%a@} @ is@ not@ defined.@\n" ident_printer eid
  | TExc.TypeIdentNotFound tid ->
      Format.fprintf ppf
        "The type@  @{<red>%a@} @ is@ not@ defined.@\n"
        QmlPrint.pp#typeident tid
  | TExc.UnableToTypeBypass bsl ->
      Format.fprintf ppf
        "Unable@ to@ type@ bypass@  @{<red>%s@}.@\n" (BslKey.to_string bsl)
  | TExc.DuplicateTypeDefinitions s ->
      Format.fprintf ppf
        "There@ are@ duplicate@ definitions@ for@ type@  @{<red>%s@} .@\n" s



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



(* This function assumes that we are rid of [TypeIdent]. *)
let rec pp_advice ppf = function
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
  | _ ->  ()



(* ************************************************************************** *)
(** {b Descr}: Prints an error message describing the typechecking error kind
    and an advice to solve it is some is available.
    Prints an ending \n
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let pp_description_and_advice ppf err_descr =
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
  pp_advice ppf (snd err_descr)



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
let pp_typer_exception_error ppf err_descr =
  let ((at_location, loc_set), _) = err_descr in
  (* Print the location taken from the expression's one, then print the message
     describing the typer error and its possible advice. *)
  Format.fprintf ppf
    "@[%a@\n%a@]"
    pp_position_or_code at_location pp_description_and_advice err_descr ;
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
let rec pp_report_from_typer_exception annotmap ppf = function
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
      List.iter (pp_report_from_typer_exception annotmap ppf) enqued_excs
  | QmlTyperException.Exception err_descr ->
      pp_typer_exception_error ppf err_descr
  | e -> Format.fprintf ppf "%s@\n" (Printexc.to_string e)



let typechecking_exception_handler env exn =
  match exn with
  | QmlTypes.Exception _ ->
      OManager.error "%a"
        (pp_report_from_typer_exception env.QmlTypes.annotmap) exn
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
