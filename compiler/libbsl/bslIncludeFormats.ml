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
   Inclusion format for opa files during the bslregister process.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(**
   This module has no mli because it would duplicate the type definitions.

   This module implements the inclusion format feature for macro
   production of bypass and external types definitions in an opa file,
   from definitions registred in bypass files.

   Example:
   {[
   ##include opa-function bslpervasives
   ]}
   Is expanded as :
   {[
   `+` = %%bslpervasives.add_int%% : int, int -> int
   `-` = %%bslpervasives.sub_int%% : int, int -> int
   etc...
   ]}
*)

(** {6 Types of format} *)

(**
   It looks like scary, but don't worry, you don't need to manipulate this
   type directly, the types are exported just so that the parser and BslLib
   can build them.

   For a high level manipulation of inclusion formats, cf module [IFormat]
*)

type fmt_fvalue = string * string * string
type fmt_fprinter = fmt_fvalue -> string
type fmt_sep = string option
type 'a mfmt_elt = [ `Mfmt_name | `Mfmt_const of string | `Mfmt_iter of 'a]
type mfmt_iter = mfmt option * fmt_fprinter * fmt_sep  (* None means #rec *)
and mfmt = [ `Mfmt_name | `Mfmt_const of string | `Mfmt_iter of mfmt_iter ] list
type ffmt = fmt_fprinter * fmt_sep
type fmt = Mfmt of mfmt | Ffmt of ffmt | Fmt_const of string


(** {6 High level API} *)

(**
   The IFormat has a local table for storing format definitions.
   It works together with the [BslRegisterParser], via the
   [BslRegisterParserState] interface.
*)

module IFormat :
sig

  (** {6 Error report} *)
  (**
     IFormat is a generic lib, so the errors does not uses OManager.
  *)
  type error
  exception Exception of error
  val pp_error : error LangPrint.pprinter

  val pp_show_format : Format.formatter -> unit -> unit
  val add : string -> fmt -> unit
  val find_opt : string -> fmt option
  val reset : unit -> unit

  val empty : fmt
  val fmt_fprinter_empty : fmt_fprinter
  val fmt_sep_empty : fmt_sep
  val mfmt_empty : mfmt
  val mfmt_of_fmt : fmt -> mfmt
  val fprinter_opt_of_fmt : fmt -> fmt_fprinter option
  val concat : fmt -> fmt -> fmt
  val opt_list_to_iter : fmt option list -> mfmt_iter

end =
struct

  type error =
    | FormatTypeClash of string * string
    | MoreThanOne of string

  exception Exception of error

  let format_type_clash t1 t2 = raise (Exception (FormatTypeClash (t1, t2)))
  let more_than_one s = raise (Exception (MoreThanOne s))

  let pp_error fmt = function
    | FormatTypeClash (t1, t2) ->
        Format.fprintf fmt "Format type clash, cannot concat a format(%s)@ and a format(%s)@\n" t1 t2
    | MoreThanOne kind ->
        Format.fprintf fmt "More than one @{<bright>format(%s)@} in this iteration block" kind

  let tbl = Hashtbl.create 1

  let pp_show_format fmt () =
    Hashtbl.iter (fun name _ -> Format.fprintf fmt "+ %s@\n" name) tbl

  let add name fmt =
    (* #<<   debug dddformat (sprintf "Adding format <%s> in my env" name);  >>#; *)
    Hashtbl.add tbl name fmt
  let find_opt name = try Some(Hashtbl.find tbl name) with Not_found -> None
  let reset () =
    (* #<<   debug dddformat "Reseting my format-env";  >>#; *)
    Hashtbl.clear tbl

  let empty = Fmt_const ""
  let fmt_fprinter_empty = ((fun _ -> "") : fmt_fprinter)
  let fmt_sep_empty = (None : fmt_sep)
  let mfmt_empty = ([] : mfmt)

  let mfmt_of_fmt = function
    | Mfmt mfmt -> mfmt
    | Ffmt (fprinter, sep) -> [`Mfmt_iter(Some mfmt_empty, fprinter, sep)]
    | Fmt_const str -> [`Mfmt_const str]

  let fprinter_opt_of_fmt = function
    | Mfmt _ -> None
    | Ffmt (fprinter, _) -> Some fprinter
    | Fmt_const str -> Some (fun _ -> str)

  let concat =
    fun fmt1 fmt2 -> match fmt1, fmt2 with
    | Mfmt mfmt1, Mfmt mfmt2 -> Mfmt (mfmt1 @ mfmt2)
    | Mfmt mfmt, Fmt_const str -> Mfmt (mfmt @ [`Mfmt_const str])
    | Mfmt _, Ffmt _ -> format_type_clash "module" "function"
    | Ffmt _, Mfmt _ -> format_type_clash "function" "module"
    | Ffmt (fprinter1, sep1), Ffmt (fprinter2, sep2) ->
        let fprinter = fun v -> (fprinter1 v) ^ (fprinter2 v) in
        let sep = match sep1, sep2 with
          | None, None -> None
          | Some s, None
          | None, Some s -> Some s
          | Some _, Some _ ->
              (* Mathieu Mon Aug 16 21:09:37 CEST 2010
                 After the refactoring of libbsl, I have no idea in what case this can happen.
                 TODO:(who find a example causing this assert false)
                   add a corresponding error message
              *)
              assert false
        in
        Ffmt (fprinter, sep)
    | Ffmt (fprinter, sep), Fmt_const str ->
        let fprinter = fun v -> (fprinter v) ^ str in
        Ffmt (fprinter, sep)
    | Fmt_const str, Mfmt mfmt -> Mfmt ((`Mfmt_const str)::mfmt)
    | Fmt_const str, Ffmt (fprinter, sep) ->
        let fprinter = fun v -> str ^ (fprinter v) in
        Ffmt (fprinter, sep)
    | Fmt_const str1, Fmt_const str2 -> Fmt_const (str1 ^ str2)

  let opt_list_to_iter fmt_opt_list =
    let mfmt_opt_list, fmt_opt_list =
      List.partition
        (fun fmt_opt -> match fmt_opt with Some Mfmt _ -> true | None -> true | _ -> false) fmt_opt_list in
    let mfmt_opt = match mfmt_opt_list with
      | [] -> Some mfmt_empty
      | [None] -> None
      | [Some Mfmt mfmt] -> Some mfmt
      | _ -> more_than_one "module"
    in
    let ffmt_list, const_list =
      List.partition (fun fmt_opt -> match fmt_opt with Some Ffmt _ -> true | _ -> false) fmt_opt_list in
    let fprinter, sep = match ffmt_list, const_list with
      | [], [] -> fmt_fprinter_empty, fmt_sep_empty
      | [], [Some Fmt_const sep] -> fmt_fprinter_empty, Some sep
      | [Some Ffmt ffmt], [] -> ffmt
      | [Some Ffmt (fprinter, _)], [Some Fmt_const sep] -> fprinter, Some sep
      | _::_::_, _ -> more_than_one "function"
      | _, _ -> more_than_one "separator"
    in
    (mfmt_opt, fprinter, sep)
end
