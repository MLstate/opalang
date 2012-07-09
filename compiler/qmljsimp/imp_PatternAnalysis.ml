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
(* depends *)
module Format = Base.Format
module List = Base.List

(* alias *)

(* shorthands *)
module Q = QmlAst

(* -- *)
type flag = [`closed | `open_]
type colvar = flag
type rowvar = flag

let pp_flag fmt = function
  | `closed -> Format.pp_print_string fmt "`closed"
  | `open_ -> Format.pp_print_string fmt "`open"

type label = Annot.label

module T =
struct
  type pat =
    | Const of label * QmlAst.const_expr
    | Var   of label * Ident.t
    | Any   of label
    | As    of label * pat * Ident.t

  let rec pp fmt = function
    | Const (_, const) -> QmlPrint.pp#const fmt const
    | Var   (_, ident) -> Format.pp_print_string fmt (Ident.stident ident)
    | Any    _ -> Format.pp_print_string fmt "_"
    | As    (_, pat, ident) -> Format.fprintf fmt "%a as %s" pp pat (Ident.stident ident)
end

type pat =
  | Fields of label * (string * pat) array * rowvar * colvar
  | Const  of label * QmlAst.const_expr
  | Var    of label * Ident.t
  | Any    of label
  | As     of label * pat * Ident.t

let more_flag sep var fmt = function
  | `closed -> ()
  | `open_ -> Format.fprintf fmt " %s %s" sep var

let rec pp fmt = function
  | Fields (_, fields, rowvar, colvar) ->
      Format.fprintf fmt (
        "{ %a%a }%a"
      )
        (Format.pp_list " ; " pp_field_pat) (Array.to_list fields)
        (more_flag ";" "...") rowvar
        (more_flag "/" ".") colvar

  | Const  (_, const) -> QmlPrint.pp#const fmt const
  | Var    (_, ident) -> Format.pp_print_string fmt (Ident.stident ident)
  | Any     _ -> Format.pp_print_string fmt "_"
  | As     (_, pat, ident) ->
      Format.fprintf fmt "%a as %s" pp pat (Ident.stident ident)

and pp_field_pat fmt (field, pat) =
  match pat with
  | Fields (_, [||], _, _) ->
      Format.pp_print_string fmt field
  | _ ->
      Format.fprintf fmt "%s = %a" field pp pat

type ('pat, 'right_hand) matching = ('pat * 'right_hand) list

type 'right_hand t =
  | Trivial of (T.pat, 'right_hand) matching
  | Pat     of (pat, 'right_hand) matching

module Projection :
sig
  val trivial : QmlAst.pat -> T.pat option
  val pat : gamma:QmlTypes.gamma -> annotmap:QmlAst.annotmap -> ty:QmlAst.ty -> QmlAst.pat -> pat
end =
struct
  (* ************************************************************************ *)
  (** {b Descr}: Returns the list of labels (strings) contained in a record
      pattern. It doesn't descend recursively in the sub-patterns. In other
      words, it returns the list of field names the record pattern has.
      {b Visibility}: Not exported outside this module.                       *)
  (* ************************************************************************ *)
  let labels_of_record_pat p =
    match p with
    | Q.PatRecord (_, names, _) -> List.map fst names
    | _ -> assert false



  (* ************************************************************************ *)
  (** {b Descr}: Find in the list of cases of a sum, the (first) one
      containing all the labels of the list [labels]. If no case is found,
      this function returns [None].
      {b Visibility}: Not exported outside this module.                       *)
  (* ************************************************************************ *)
  let find_case_having_labels labels sum_cases =
    (* Local function trying to see if one case has all the labels. *)
    let deal_one_case case =
      (* All the labels must belong to the case. *)
      List.for_all
        (fun lbl -> List.exists (fun (lb, _) -> lb = lbl) case)
        labels in
    (* Now, check each case until we find one matching. *)
    let rec find_in_cases = function
      | [] -> None
      | h :: q -> if deal_one_case h then Some h else find_in_cases q in
    (* And now, really do the job. *)
    find_in_cases sum_cases



  exception Not_trivial
  let trivial p =
    let rec aux p =
      match p with
      | Q.PatRecord _ -> raise Not_trivial
      | Q.PatConst (label, const) ->
          T.Const (label, const)
      | Q.PatVar (label, ident) ->
          T.Var (label, ident)
      | Q.PatAny label ->
          T.Any label
      | Q.PatCoerce (_, pat, _) -> aux pat
      | Q.PatAs (label, pat, id) ->
          let pat = aux pat in
          T.As (label, pat, id)
    in
    try
      Some (aux p)
    with
    | Not_trivial -> None

  let cmp (fa, _) (fb, _) = String.compare fa fb
  let sort tl = List.sort cmp tl

  let pat ~gamma ~annotmap:_ ~ty pat =
    let rec aux level_all_cases_ty p =
      match p with
      | Q.PatRecord _ ->
          record p level_all_cases_ty
      | Q.PatConst (label, const) ->
          Const (label, const)
      | Q.PatVar (label, ident) ->
          Var (label, ident)
      | Q.PatAny label ->
          Any label
      | Q.PatCoerce (_, pat, _) -> aux level_all_cases_ty pat
      | Q.PatAs (label, pat, ident) ->
          let pat = aux level_all_cases_ty pat in
          As (label, pat, ident)

    and record a expected_sum_ty =
      (* We are processing a pattern being a record. It is a case of a sum.
         This sum can be opened or closed depending on the sequence of record
         patterns at this level (presence or absence of catchall making the
         difference. However, this opening or closing is explicit in the type
         given to patterns of this level.
         So, to know if the column is opened or closed, we will inspect the
         type of the current level of pattern.
         If the patterns at this level have a named type, then we must inspect
         this named type definition to know if it corresponds to a closed or
         opened sum type. To do so, we first "expand" the type then only
         inspect its structure after. *)
      let expanded_ty =
        QmlTypesUtils.Inspect.follow_alias_noopt_private gamma expected_sum_ty in
      let (sum_cases, colvar) =
        (match expanded_ty with
         | Q.TypeSum (Q.TyCol (cases, opt_var)) ->
             let ending = if opt_var = None then `closed else `open_ in
             (cases, ending)
         | Q.TypeRecord (Q.TyRow (case, _)) ->
             (* If the type of the pattern is a record, then because QML types
                algebra cosiders that record do not have column variables, we
                will say that the corresponding column ending is closed. *)
             ([case], `closed)
         | Q.TypeVar _ ->
             (* May arise because some passes make code transformation and do
                not type again. Say that the corresponding column ending is
                open since this is the most restrictive choice. In effect, this
                will trigger more checks in coming processing which is
                satisfactory since we do not know the type here. *)
             ([], `open_)
         | Q.TypeName _ | Q.TypeConst _
         | Q.TypeArrow _ | Q.TypeSumSugar _ | Q.TypeAbstract | Q.TypeForall _ ->
             (* Assuming the typechecking is already done and successfull, a
                record pattern can't have these types. *)
             OManager.printf "ty:%a@." QmlPrint.pp#ty expanded_ty;
             assert false) in

      (* Local function processing sequentially all the patterns representing
         the fields of a record pattern. *)
      let rec fields current_record_list_of_fields_tys p =
        match p with
        | Q.PatRecord (label, fields, rowvar) ->
            let map (field, pat) =
              let sub_level_cases_ty =
                (try List.assoc field current_record_list_of_fields_tys
                 with Not_found ->
                   (* The type of this field was not found in the list of fields
                      making up the current sum case. We are missing information,
                      so be restrictive and say that the type is a variable (i.e.
                      we don't known anything about it). *)
                   QmlAst.TypeVar (QmlAst.TypeVar.next ())) in
              let pat = aux sub_level_cases_ty pat in
              field, pat
            in
            let fields = List.rev_map map fields in
            let fields = sort fields in
            Fields (label, Array.of_list fields, rowvar, colvar)

        | Q.PatCoerce (_, pat, _) ->
            fields current_record_list_of_fields_tys pat

        | Q.PatConst _ -> assert false
        | Q.PatVar _ -> assert false
        | Q.PatAny _ -> assert false
        | Q.PatAs _ -> assert false

      in

      (* Effective body of the [record] function. We must find the case
         corresponding to this sequence of fields in the sum type.
         By invariant, we always arrive here with a pattern that is a record,
         and this pattern represents one case of pattern matching. *)
      (match a with

       | Q.PatRecord (label, [], rowvar) ->
           Fields (label, [||], rowvar, colvar)

       | Q.PatRecord _ ->
           (* Get the list of all the fields names in this record pattern. *)
           let labels = labels_of_record_pat a in
           (* Recover the list of fields and types that represent the type of
              this record among the cases found as belonging to the current
              sum type. *)
           let list_of_fields_tys =
             match find_case_having_labels labels sum_cases with
             | None ->
                 (* No sum case with these labels found. In this case, we
                    will use an empty list of fields name and the function
                    processing fields ([fields]) will have to fallback to
                    restrictive approximation if it needs the type of a field
                    that won't obviously appears in our list. *)
                 []
             | Some case -> case
           in
           fields list_of_fields_tys a

       | _ ->
           (*
             Internal error
             If we end-up there, that means that there is a bug in structure and
             call interaction between the function [pat], [aux], [fields], and [record]
           *)
           assert false
      ) in
    aux ty pat

end

let analysis ~gamma ~annotmap ~ty patterns =
  let is_trivial =
    let rec aux acc = function
      | [] -> Some ( List.rev acc )
      | (pat, e) :: tl -> (
          match Projection.trivial pat with
          | Some trivial -> aux ( (trivial, e)::acc ) tl
          | None -> None
        )
    in
    aux [] patterns
  in
  match is_trivial with
  | Some patterns ->
      Trivial patterns
  | None ->
      let patterns = List.tail_map (fun (pat, e) -> Projection.pat ~gamma ~annotmap ~ty pat, e) patterns in
      Pat patterns
