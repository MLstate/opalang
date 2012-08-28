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

module String      = Base.String

(* TODO: a safe module InfixOperators, not in base.ml
   open InfixOperators
*)
let (@*) = InfixOperator.(@*)
let (|>) = InfixOperator.(|>)

(* <!> If you add anything there, update *parse* and *parsed_t* functions *)
type t = {
  backend_restriction         : StringSet.t option ;
  no_projection               : StringSet.t option ;
  opaname                     : bool ;
  raise_                      : bool ;
  restricted                  : string list option option ;
  second_order                : bool ;
  cps_bypass                  : bool ;
  pure                        : bool ;
  opacapi                     : bool ;
}

type parsed_t = ( string * string option ) list

let default = {
  backend_restriction = None ;
  no_projection = Some StringSet.empty ;
  opaname = true ;
  raise_ = false ;
  restricted = None ;
  second_order = false ;
  cps_bypass = false ;
  pure = false ;
  opacapi = false ;
}

(* for error messages, please put all tags there *)
let known_tags = [
  "backend" ;
  "opaname" ;
  "normalize" ;
  "raise" ;
  "restricted" ;
  "no-projection";
  "cps-bypass" ;
  "opacapi" ;
]

let is_default t = ( t == default )

let string_of_restricted = function
  | None -> "no restriction"
  | Some None -> "compiler only (any pass)"
  | Some Some list -> String.concat_map ~left:"compiler only (pass(es) : " ~right:")" ", " (fun s -> s) list


type 'a pprinter = 'a Base.Format.pprinter
let pp = Format.fprintf
let pp_string = Format.pp_print_string
let pp_list = Base.Format.pp_list
let pp_meta_string fmt s = Format.fprintf fmt "%S" s

let pp_option name pp' fmt = function
  | None -> pp_string fmt name
  | Some o -> pp fmt "%s:%a" name pp' o


let pp_tag fmt (tag, attribute) = pp_option tag pp_string fmt attribute

let pp_meta_tag fmt (tag, attribute) =
  Format.fprintf fmt "(%S, %a)" tag (Option.pp_meta pp_meta_string) attribute

let pp_meta fmt parsed_t =
  Format.fprintf fmt "[ %a ]" (pp_list " ; " pp_meta_tag) parsed_t

(* following guidelines about errors *)
type error =
  | Unknown_tag of (string * string option)
  | RaiseInconsistency

exception Exception of error

let pp_error fmt = function
  | Unknown_tag ((typo, _) as tag) ->
      Format.fprintf fmt "Unknown tag [ %a , ... ]@\n" pp_tag tag ;
      Format.fprintf fmt "%a" (HintUtils.pp_suggestion known_tags) typo
  | RaiseInconsistency ->
      Format.fprintf fmt "Inconsistency on tag [ raise , ...]@\n" ;
      Format.fprintf fmt "This tag is not compatible with @{<bright>cps-bypass@}, nor @{<bright>no-projection(:cps?)@}"

let error_unknown_tag tag = raise (Exception (Unknown_tag tag))

let check_tags tags =
  let inconsistency () = raise (Exception RaiseInconsistency) in
  let () =
    if tags.raise_
    then
      let () =
        match tags.no_projection with
        | None -> inconsistency ()
        | Some set -> if StringSet.mem "cps" set then inconsistency ()
      in
      let () =
        if tags.cps_bypass then inconsistency ()
      in
      ()
  in
  ()

let parse_aux =
  let fold t = function
    | "no-projection", None ->
        { t with no_projection = None }
    | "no-projection", Some s ->
        (match t.no_projection with
           | None -> t
           | Some old -> { t with no_projection = Some (StringSet.add s old) })
    | "opaname", _ ->
        (* opaname is now set by default, but we keep the syntax for backward compatibility *)
        { t with opaname = true }
    | "normalize", _ ->
        { t with opaname = false }
    | "raise", _ ->
        { t with raise_ = true }
    | "restricted", opt ->
        begin
          let restricted =
            match opt with
            | None -> Some None
            | Some pass ->
                begin
                  match t.restricted with
                  | None -> Some (Some [pass])
                  | Some None -> Some None
                  | Some (Some other_pass) -> Some (Some (pass::other_pass))
                end
          in
          { t with restricted = restricted }
        end

    | "backend", opt ->
        let opt = Option.default "anybackend" opt in
        { t with
            backend_restriction =
              Some (StringSet.add opt (Option.default StringSet.empty t.backend_restriction))
        }

    (* internal attributes : should not be parsed, but are generated by bslregister *)
    | "second-order", _ ->
        { t with second_order = true }
    | "cps-bypass", _ ->
        { t with cps_bypass = true }

    | "opacapi", _ ->
        { t with opacapi = true }

    | "pure", _ ->
        { t with pure = true }

    | tag ->
        error_unknown_tag tag

  in
  List.fold_left fold default

let parse ?pos parsed_t =
  match pos with
  | None -> parse_aux parsed_t
  | Some pos -> (
      try
        let tags = parse_aux parsed_t in
        check_tags tags ;
        tags
      with
      | Exception error ->
          OManager.printf "%a" FilePos.pp_citation pos ;
          OManager.error "%a@\n" pp_error error
    )

(* reversing : used for code generation. *)

let parsed_t_backend t acc =
  let tag = "backend" in
  match t.backend_restriction with
  | None -> acc
  | Some set ->
      StringSet.fold (
        fun backend acc -> (tag, Some backend) :: acc
      )  set acc

let parsed_t_no_projection t acc =
  let no_projection = "no-projection" in
  match t.no_projection with
  | None -> (no_projection, None) :: acc
  | Some set ->
      if StringSet.is_empty set
      then acc
      else
        StringSet.fold (
          fun elt acc -> (no_projection, Some elt) :: acc
        ) set acc

let parsed_t_opaname t acc =
  let normalize = "normalize" in
  match t.opaname with
  | false -> (normalize, None) :: acc
  | true -> acc

let parsed_t_raise t acc =
  let raise_ = "raise" in
  match t.raise_ with
  | true -> (raise_, None) :: acc
  | false -> acc

let parsed_t_restricted t acc =
  let restricted = "restricted" in
  match t.restricted with
  | None -> acc
  | Some None -> (restricted, None) :: acc
  | Some (Some passes) ->
      List.fold_left (
        fun acc pass -> (restricted, Some pass) :: acc
      ) acc passes


let parsed_t_second_order t acc =
  let second_order = "second-order" in
  if t.second_order
  then (second_order, None) :: acc
  else acc


let parsed_t_cps_bypass t acc =
  let cps_bypass = "cps-bypass" in
  if t.cps_bypass
  then (cps_bypass, None) :: acc
  else acc

let parsed_t_opacapi t acc =
  let opacapi = "opacapi" in
  if t.opacapi
  then (opacapi, None) :: acc
  else acc

let parsed_t_pure t acc =
  let pure = "pure" in
  if t.pure
  then (pure, None) :: acc
  else acc

let parsed_t t =
  let (||>) acc f = f t acc in
  []
  ||> parsed_t_backend
  ||> parsed_t_no_projection
  ||> parsed_t_opaname
  ||> parsed_t_raise
  ||> parsed_t_restricted
  ||> parsed_t_second_order
  ||> parsed_t_cps_bypass
  ||> parsed_t_opacapi
  ||> parsed_t_pure
  |> List.sort Pervasives.compare


(** {6 Printing} *)

let pp fmt t =
  let parsed_t = parsed_t t in
  pp fmt "[@[<4>@ %a @]]" (pp_list " ;@ " pp_tag) parsed_t


module Q = QmlAst

let authorized_bypass ~restriction tags =
  #<If:BSL_NO_RESTRICTION>
    true
  #<Else>
    match restriction with
    | None -> (
        match tags.restricted with
        | None -> true
        | Some _ -> false
      )
    | Some pass -> (
        match tags.restricted with
        | None
        | Some None -> true
        | Some (Some authorized_passes) -> List.mem pass authorized_passes
      )
  #<End>


let authorized_bypass_as_expr tags bypass =
  match bypass with
  | Q.Bypass _ ->
      authorized_bypass ~restriction:None tags
  | Q.Directive (_, `restricted_bypass restriction, _, _) ->
      authorized_bypass ~restriction:(Some restriction) tags
  | _ -> assert false (* this function is not meant to be called with anything else *)

type passname = string

let do_projection {no_projection = no_projection} pass_name =
  match no_projection with
  | None -> false
  | Some s -> not (StringSet.mem pass_name s)


let never_projected {no_projection = no_projection} =
  match no_projection with
    | None -> true
    | Some _ -> false


let string_of_no_projection = function
  | None -> "never project"
  | Some s ->
      if StringSet.is_empty s then
        "always project"
      else
        String.concat_map ~left:"always project but for the following passes: " ", "
          (fun x -> x) (StringSet.elements s)
