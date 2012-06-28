(*
    Copyright © 2011, 2012 MLstate

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
module Char = BaseChar
module String = BaseString

type uniq = Fresh.t_fresh
type t =
  | Source of string
  | FakeSource of string
      (* like source, except that
       * they can be 'renamed' (to_string can do operations on them)
       * because they won't ever be used to refer to external identifiers
       * (like "Pervasives") *)
  | Internal of uniq

type t' = t

(** compare : uniqs and strings are comparable with Pervasives *)
let compare i1 i2 =
  match i1,i2 with
  | Source s1, Source s2 -> compare s1 s2
  | Source _, _ -> -1
  | _, Source _ -> 1
  | FakeSource s1, FakeSource s2 -> compare s1 s2
  | FakeSource _, _ -> -1
  | _, FakeSource _ -> 1
  | Internal (i1,_,_,package1), Internal (i2,_,_,package2) ->
      (match compare i1 i2 with
       | 0 -> String.compare package1 package2
       | c -> c)

let equal x y =
  match x, y with
  | Source s1, Source s2 -> s1 = s2
  | FakeSource s1, FakeSource s2 -> s1 = s2
  | Internal (i1,_,_,package1), Internal (i2,_,_,package2) ->
      i1 = i2 && package1 = package2
  | _ -> false

let hash = function
  | Source s -> Hashtbl.hash s
  | FakeSource s -> Hashtbl.hash s
  | Internal (i, _, _, package) -> Hashtbl.hash i + Hashtbl.hash package

module IHashtbl = Hashtbl.Make (struct type t = t' let hash = hash let equal = equal end)

let _alpha_protection = ref false

let active_alpha_protection () = _alpha_protection := true

let source x =
  if !_alpha_protection
  then assert false (* [ qml_Ast.ml; #54190 ] no source allowed after an alpha conv *)
  else Source x

let fake_source x = FakeSource x

let pattern = "^\\(\\([-+^@!&]+[-.+^*/<>=&|]*\\)\\|\\([*/<>=]+[-.+^*/<>=&|]*\\)\\|\\([|][-.+^*/<>=&|]+\\)\\)$"
let regexp = Str.regexp pattern

let is_operator_string s =
  (* the regexp was taken from libqmlcompil/qmlMainParser/qmlMainParser.trx -- but is now desynchronized
     it's be better to have something simpler, like:
     match s.[0] with '_' | 'a'..'z' | 'A'..'Z' -> false | _ -> true
     but the main point is the synchronisation with the parser (printed code should reparse) *)
  Str.string_match regexp s 0

let is_operator = function
  | Source s -> is_operator_string s
  | _ -> false

let maybe_digest n =
  let digest s = String.sub (Digest.to_hex (Digest.string s)) 0 8 in
  if Base.String.is_word n && not (is_operator_string n) then n else digest n

let print n id d =
  if n=0 then
    Printf.sprintf "__%s%s%s" id (if d = "" then "" else "_") d
  else
    Printf.sprintf "_v%d_%s%s%s" n id (if d = "" then "" else "_") d

let original_name = function
  | FakeSource n
  | Source n -> n
  | Internal (_, _, n, _) -> n

let start_with_n_underscore s =
  let i = ref 0 in
  let n = String.length s in
  while !i < n && s.[!i] = '_' do incr i done;
  !i

let renaming_should_warn_when i =
  let s = original_name i in
  (* not warning on xmlns: it is a bit hacky, we should be able
   * to say that we don't want warnings for a specific ident instead *)
  if String.is_prefix "xmlns:" s then
    `never
  else
    match start_with_n_underscore s with
    | 0 -> `unused
    | 1 -> `used
    | _ -> `never

(** see note *)
let to_string =
  #<If:TESTING>
    original_name (* making sure we don't have _v34_f in tests refs *)
  #<Else>
    function
    | FakeSource n
    | Source n -> if Base.String.is_word n || is_operator_string n then n else "`" ^ n ^ "`"
    | Internal (_, id, n, d) ->
        let n = print id n d in
        if Base.String.is_word n then n else "`" ^ n ^ "`"
  #<End>

let opa_syntax ?(dont_protect_operator=false) id =
  #<If:TESTING>
    original_name id (* making sure we don't have _v34_f in tests refs *)
  #<Else>
    let n =
      match id with
      | FakeSource n
      | Source n -> n
      | Internal (_, id, n, d) -> print id n d
    in
    if Base.String.is_word n || (dont_protect_operator && is_operator_string n)
    then n else "`" ^ n ^ "`"
  #<End>

let to_uniq_string = function
  | FakeSource _
  | Source _ -> assert false
  | Internal (_, id, n, d) -> print id n d

(** Fixed : don't allow anonymous internal *)
(** /!\ Keep the name of ident safe for qml, and ocaml generation (it would break compilers) *)
let next =
  let get = Fresh.fresh_named_factory (fun i -> i) in
  fun ?(filename="") ?(descr="") n ->
    (* the description need to contain the package name for separate compilation *)
    let descr = (* TODO: remove this check once s2 is removed *)
      if ObjectFiles.Arg.is_separated () then
        ObjectFiles.get_current_package_name ()
      else
        filename ^ descr in
    let fresh = get ~name:n ~descr () in
    Internal fresh

let get_package_name = function
  | Internal (_,_,_,d) -> d
  | FakeSource s
  | Source s -> Base.invalid_argf "Ident.get_package_name: %s" s
let safe_get_package_name = function
  | Internal (_,_,_,d) -> Some d
  | FakeSource _
  | Source _ -> None

let nextf = fun ?filename ?descr fmt -> Printf.ksprintf (next ?filename ?descr) fmt


let escape =
  let valid_chars = function
    | '_'
    | 'a'..'z'
    | 'A'..'Z'
    | '0'..'9' -> true | _ -> false in
  let escape_char = '\'' in
  String.escape ~valid_chars ~escape_char

(** BIG BIG warning : do not print ` in function stident used in libconvert !
    or some ident will have really ` in it *)
let stident = function
  | Source n -> n
  | FakeSource n -> "s"^escape n
  | Internal (_, id, n, d) -> print id (maybe_digest n) (maybe_digest d)

let memo_stident = IHashtbl.create 1024
let stident id =
  try
    IHashtbl.find memo_stident id
  with
  | Not_found ->
      let s = stident id in
      IHashtbl.add memo_stident id s ;
      s

let refresh ?(map=fun s -> s) y =
  match y with
  | Source n -> next (map n)
  | FakeSource s -> next (map s)
  | Internal (_, _, n, d) -> next ~descr:d (map n)
let refreshf ~map y = refresh ~map:(Printf.sprintf map) y

let concrete_string = function
  | Source n -> Printf.sprintf "Source(%s)" n
  | FakeSource s -> Printf.sprintf "FakeSource(%s)" s
  | Internal (argh, i, n, d) -> Printf.sprintf "Internal(%d, %d, %s , %s)" argh i n d

let light_ident = function
  | FakeSource n
  | Source n -> n
  | Internal (_, n, id, _) ->
    if n=0 then
      Printf.sprintf "__%s" id
    else
      Printf.sprintf "_v%d_%s" n id
