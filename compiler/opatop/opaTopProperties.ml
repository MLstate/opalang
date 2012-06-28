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
(* CF mli *)

let generic_make_property printer start ?(quiet=false) mess =
  let t = ref start in
  let set state =
    t := state ;
    if not quiet then printer mess state
  and get () = !t
  in
  set, get

let make_property ?(start=false) =
  generic_make_property
    (fun mess state ->
       OManager.unquiet "%s is %s@\n" mess (if state then "on" else "off"))
    start

let make_int_property ?(start=0) =
  generic_make_property
    (fun mess state ->
       OManager.unquiet "%s level is %d@\n" mess state)
    start

let assert_set, assert_get = make_property ~start:true "assert"
let dddbgen_set, dddbgen_get = make_property "dddbgen"
let dump_set, dump_get = make_property "dump" ~quiet:true
let greedy_set, greedy_get = make_property "greedy" ~quiet:true
let noeval_set, noeval_get = make_property "noeval" ~quiet:true

let prompt_set, prompt_get = make_property ~start:false ~quiet:true "prompt"

let restricted_bypass_set, restricted_bypass_get =
  make_property ~start:true "restricted_bypass"

let value_restriction_set, value_restriction_get = (*make_int_property "value_restriction" ~quiet:false*)
  generic_make_property
    (fun mess state ->
       OManager.unquiet "%s is %s@\n"
         mess (match state with
               | `disabled -> "disabled"
               | `normal -> "normal"
               | `strict -> "strict"))
    `disabled
    ~quiet:true
    "value_restriction"



let switch_typechecker ty_ch_name =
  match QmlTyper.available_typer_of_string ty_ch_name with
  | None -> false
  | Some t -> QmlTyper.DyTyper.switch_typer t ; true



let stdout = ref Format.std_formatter
