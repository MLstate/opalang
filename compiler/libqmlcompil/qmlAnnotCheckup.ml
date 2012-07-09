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

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst

(* -- *)

type options = {
  dump_found : bool ;
  dump_not_found : bool ;
  pat_not_found_are_ok : bool ;
  freshness_only : bool ;
  dump_position : bool
}

let default = {
  dump_found = false ;
  dump_not_found = true ;
  pat_not_found_are_ok = false ;
  freshness_only = false ;
  dump_position = false
}

type 'a annotation_checker = ?options:options -> QmlAst.annotmap -> 'a -> bool

let _annots_found = ref AnnotSet.empty

(* let init () = prerr_endline "=== ANNOT CHECKUP ===" *)
let short s =
  let len = 40 in
  let t = String.length s in
  if t > 3 * len then (String.sub s 0 len)^" ... (expr too long)  ..."^(String.sub s (t - 1 - len) len) else s

let check_gen_annot get_label options annots printer ?(pattern_case=false) pointer exp =
  let label = get_label exp in
  let pos = Annot.pos label in
  let annot = Annot.annot label in
  (if AnnotSet.mem annot !_annots_found
  then
    begin
      if options.dump_not_found then
        OManager.verbose "AnnotCheckup: %s | %s"
          (Ansi.print `red (Printf.sprintf "[NON FRESH ANNOT : a %s]" (Annot.to_string annot)))
          (short (printer exp));
      false
    end
  else
    begin
      _annots_found := AnnotSet.add annot !_annots_found;
      true
    end)
  &&
  (options.freshness_only ||
  let position =
    if options.dump_position
    then Ansi.print `green (Printf.sprintf "/ position : %s"
      (FilePos.to_string pos))
    else ""
  in
  match QmlAnnotMap.find_ty_opt annot annots with
  | Some ty ->
      let _ =
        if options.dump_found
        then
          let found = Ansi.print `green (Printf.sprintf "[FOUND : a %s]" (Annot.to_string annot)) in
          OManager.verbose "AnnotCheckup: %s | %s : %s %s %s %s" found
            (short (printer exp)) (Format.to_string QmlPrint.pp#ty ty) "" "" position
        else if options.dump_position && FilePos.is_empty pos
        then OManager.verbose "AnnotCheckup: %s | %s"
          (Ansi.print `red (Printf.sprintf "[POS-NOT-FOUND : a %s]" (Annot.to_string annot))) (short (printer exp)) in
        true
  | None ->
      if options.pat_not_found_are_ok && pattern_case then true
      else
        let _ =
          if options.dump_not_found
          then
            let not_found = Ansi.print `red (Printf.sprintf "[NOT-FOUND : a %s]" (Annot.to_string annot)) in
            OManager.verbose "AnnotCheckup: %s | in %s : %s %s"
                 not_found pointer (short (printer exp)) position in
        false
  )

let check_expr ?(options=default) annots =
  check_gen_annot Q.Label.expr options annots (Format.to_string QmlPrint.pp#expr0) "an EXPR"
let check_pat ?(options=default) annots =
  check_gen_annot Q.Label.pat options annots (Format.to_string QmlPrint.pp#pat0) ~pattern_case:true "a PAT"

let expr ?(options=default) annots e =
  let _ok = ref true in
  let ok t = _ok := !_ok && t in
  let iter_expr e = ok (check_expr ~options annots e) in
  let iter_pat p = ok (check_pat ~options annots p) in
  let _ = QmlAstWalk.ExprPatt.iter_down iter_expr iter_pat e in
  !_ok

let pat ?options annots p =
  let _ok = ref true in
  let ok t = _ok := !_ok && t in
  let iter_pat p = ok (check_pat ?options annots p) in
  let _ = QmlAstWalk.Pattern.iter_down iter_pat p in
  !_ok

let code_elt ?options annots code_elt =
  let _ok = ref true in
  let ok t = _ok := !_ok && t in
  let iter_check = (fun e -> ok (expr ?options annots e) ; e ) in
  let _ = QmlAstWalk.Top.map_expr iter_check code_elt in
  !_ok

let code ?options annots code =
  let _ok = ref true in
  let ok t = _ok := !_ok && t in
  let iter c = ok (code_elt ?options annots c) in
  let _ = List.iter iter code in
  !_ok

let expr ?options annots e =
  _annots_found := AnnotSet.empty;
  expr ?options annots e

let pat ?options annots e =
  _annots_found := AnnotSet.empty;
  pat ?options annots e

let code_elt ?options annots e =
  _annots_found := AnnotSet.empty;
  code_elt ?options annots e

let code ?options annots e =
  _annots_found := AnnotSet.empty;
  code ?options annots e
