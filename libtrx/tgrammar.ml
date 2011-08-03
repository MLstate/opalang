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
(*
   @author Henri Binsztok
   @author Adam Koprowski
*)

(* depends*)
module List = BaseList
module String = BaseString

(* alias *)
module B = Base
(* FIXME, This module seroiusly needs a clean-up... I hope to have time one day... *)

(* TODO
 * autoriser une expression vide (on peut bien écrire !"é#ù£" {{ ... }})
 * ajouter de gardes une fois traduit en Qml
 * syntaxe pour un au plus parmi     [[toto truc muche]]
 * syntaxe extensible = générer les opérateurs infixes au minimum, voire générer le parser en cours de route...
 *)

(* FIXME, Any should become a separate construct; it does not belong to range  *)
type range = One of char | Range of char * char | Any

type rule_name = NoName
               | PrimaryName of string
               | SecondaryName of string

type memo_type = MemoNone      (* no memoization *)
               | MemoFail      (* memoization of failures *)
               | MemoSuccess   (* memoization of success *)
               | MemoFull      (* memoization of both failure and success *)
               | MemoNoInfo    (* no memoization information for rule *)

type rule_annots =
    { rule_name : rule_name
    ; rule_memo : memo_type
    }

type location =
    { file_name : string
    ; line_number : int
    }

(* type code = string (\* FIXME: Qml.expression *\) *)
type ident = string (* FIXME: abstrait *)
type fun_ident = ident (* FIXME: abstrait *)
type code_ident = ident (* FIXME: abstrait *)
type code_type = string
type filename = string

module Grammar =
struct
  type 'a expression = 'a sequence list
  and 'a sequence =
      'a item list (* FIXME * [`LIST | `ONEOFEACH] *) * (string * bool) StringMap.t (* labeled items *)
      * (bool * 'a (* code *) * location option * bool (* if true production gives option with none indicating that we should backtrack*)) option (* bool = ajouter les positions... *)
  and 'a item = [`AND | `NOT | `NORMAL] * 'a primary * [`QUESTION | `STAR | `PLUS | `NORMAL]
  and 'a primary =
    | Ident of ident
    | Paren of 'a expression
    | Literal of string * bool (* case agnostic *)
    | Class of range list
  type 'a grammar =
      { start : string
      ; grammar : 'a expression StringMap.t
      }
end

module PreGrammar =
struct
  type 'a definition =
      { expression : 'a expression
      ; debug : bool
      ; mark : bool  (* FIXME: le "+" comme "debug" devraient être dans la StringMap : pas ici *)
      ; retain_cache: bool (* Only relevant for incremental parsing: if true then cache for this rule will be
                              kept in the next parsing of the input; otherwise it will be cleared (only
                              few top-level rules should be marked for retaining cache, since it has to be
                              updated and this can be quite costly) *)
      ; rule_type : string option
      ; origin : location option (* location from which this rule originated (if available) *)
      }
  and 'a expression =
    | Expr of 'a sequence list
    | App of fun_ident * 'a sequence list list (* application de fonction, ne doit plus rester dans une grammaire typecheckée *)
  and 'a sequence = 'a item list (* FIXME * [`LIST | `ONEOFEACH] *) * (string * bool) StringMap.t *
    (bool (* ajouter les positions... *) * 'a (* code *) * location option * bool (* if true then production gives option with none indicating that we should backtrack *)) option
  and 'a item = [`AND | `NOT | `NORMAL] * 'a primary * [`QUESTION | `STAR | `PLUS | `NORMAL]
  and 'a primary =
    | Ident of ident
    | Paren of 'a expression
    | Literal of string * bool (* case agnostic *)
    | Class of range list

  type include_type = Incl | Read
  type include_def =
      { it : include_type
      ; gl : string list (* liste des définitions "globales", i.e. fonctions de l'include ou open *)
      }
  type 'a pre_grammar =
      { pheader : 'a header list
      ; poptions : (code_ident * code_ident) list
      ; pextra : (code_ident * code_type) list
      ; incl : include_def StringMap.t (* stringset *)
      ; funs : ('a gfun * rule_annots) StringMap.t (* fun_ident_map *)
      ; defs : ('a definition * rule_annots) StringMap.t }
  and 'a gfun =
      { vars : ident list (* liste des idents bindés *)
      ; expr : 'a expression }
  and 'a header_code =
      [ `inside of 'a
      | `normal of 'a
      | `types of 'a
      | `decls of 'a
      | `file of filename (* utile ? *)
      ]
  and 'a header = 'a header_code * location option
end

module G = Grammar
module P = PreGrammar

let rec def_map_to_string ?cf dm =
  StringMap.fold (
    fun x y acc ->
      Printf.sprintf "%s%s%s%s%s%s <- %s" acc (if y.P.mark & (not y.P.debug) & (not (acc = "")) then ";" else "") (if acc = "" then "" else "\n\n") (if y.P.debug then "%" else "") (if y.P.mark then "+" else "") x (def_to_string ?cf y)
  ) dm ""
and def_to_string ?cf d =
  expr_to_string ?cf d.P.expression
and expr_to_string ?cf = function
  | P.Expr sl -> seq_list_to_string ?cf sl
  | _ -> assert false
and seq_list_to_string ?cf = function
  | [hd] -> seq_to_string ?cf hd
  | (hd :: tl) -> seq_to_string ?cf hd ^ " / " ^ seq_list_to_string ?cf tl
  | _ -> assert false
and seq_to_string ?cf = function
  | il, map, None -> item_list_to_string ?cf ~map:map ~i:1 il
  | il, map, Some (b, code, _loc, c) ->  Printf.sprintf "%s %s" (item_list_to_string ?cf ~map:map ~i:1 il) (match cf with None -> "CODE" | Some f -> f b c code)
and item_list_to_string ?cf ?map ?i = function
  | [hd] -> (item_to_string ?cf ?map ?i hd)
  | (hd :: tl) ->
      begin
        match i with
        | None -> (item_to_string ?cf ?map ?i hd) ^ " " ^ (item_list_to_string ?cf ?map ?i tl)
        | Some l -> (item_to_string ?cf ?map ?i hd) ^ " " ^ (item_list_to_string ?cf ?map ~i:(l+1) tl)
      end
  | _ -> assert false
and item_to_string ?cf ?map ?i (pre, content, post) =
  let option = match map with
  | None -> None
  | Some m ->
      StringMap.fold (
        fun key (ind,b) acc ->
          match i with
          | None -> acc
          | Some l -> if (ind = string_of_int (l)) then Some (key,b) else acc
      ) m None in
  match option with
  | None -> pre_to_string pre ^ primary_to_string ?cf content ^ post_to_string post
  | Some (name,bool) ->  let tmp = if bool then " :_ " else " : " in pre_to_string pre ^ primary_to_string ?cf content ^ post_to_string post ^ tmp ^ name ^ " "
and pre_to_string = function
  | `AND -> "&"
  | `NOT -> "!"
  | _ -> ""
and post_to_string = function
  | `QUESTION -> "?"
  | `STAR -> "*"
  | `PLUS -> "+"
  | `NORMAL -> ""
and primary_to_string ?cf = function
  | P.Ident i -> i
  | P.Paren e -> "(" ^ expr_to_string ?cf e ^ ")"
  | P.Literal (s, b) ->
      Printf.sprintf "\"%s\"%s" (String.escaped s) (if b then "~" else "")
  | P.Class rl -> Printf.sprintf "%s" (range_list_to_string rl)
and range_list_to_string = function
  | [Any] -> "."
  | liste ->
      let rec aux bool = function
      | [hd] -> range_to_string bool hd
      | hd :: tl -> range_to_string true hd ^ aux true tl
      | _ -> assert false
      in Printf.sprintf "[%s]" (aux false liste)
and range_to_string b = function
  | One c ->
      begin
        match c with
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | ']' -> "\\]"
        | '[' -> "\\["
        | '\'' -> "\\'"
        | '\\' -> "\\\\"
        | c ->
            if (b && ((c = '-') || (c = '"'))) then Printf.sprintf "\\%c" c
            else Printf.sprintf "%c" c
      end
  | Range (c1, c2) -> Printf.sprintf "%c-%c" c1 c2
  | Any -> assert false

let make_const name =
  (([`NORMAL, P.Literal (name, false), `NORMAL], StringMap.empty, None) : 'a P.sequence)

type 'a grammar =
    { start : string
    ; grammar : ('a P.definition * rule_annots) StringMap.t
    ; header : 'a P.header list
    ; extra : (code_ident * code_type) list
    ; options : (code_ident * code_ident) list
    }

let rec map_grammar (f:'a -> 'b) g =
  let rec aux_definition (def,msg_error) =
  { def with P.expression = aux_expression def.P.expression }, msg_error
  and aux_expression = function
    | P.Expr sl -> P.Expr (List.map aux_sequence sl)
    | P.App _ -> raise (B.NotImplemented "map_grammar1")
  and aux_sequence = function
    | (il, map, None) -> List.map aux_item il, map, None
    | (il, map, Some (b, code, loc, c)) -> List.map aux_item il, map, Some (b, f code, loc, c)
  and aux_item (pre, p, suf) = pre, aux_primary p, suf
  and aux_primary = function
    | P.Ident i -> P.Ident i
    | P.Paren e -> P.Paren (aux_expression e)
    | P.Literal (s, b) -> P.Literal (s, b)
    | P.Class rl -> P.Class rl
  and aux_header_code = function
    | `normal code -> `normal (f code)
    | `inside code -> `inside (f code)
    | `types code -> `types (f code)
    | `decls code -> `decls (f code)
    | `file f -> `file f
  and aux_header = function
    | code, loc -> aux_header_code code, loc
  in
  { start = g.start
  ; grammar = StringMap.map aux_definition g.grammar
  ; header = List.map aux_header g.header
  ; extra = g.extra
  ; options = g.options
  }

let empty_pre_grammar =
  { P.pheader = []
  ; P.pextra = []
  ; P.poptions = []
  ; P.incl = StringMap.empty
  ; P.funs = StringMap.empty
  ; P.defs = StringMap.empty }

(* pour le traitement des includes, on n'ajoute que si nécessaire *)
let add_definition preg (name, def) =
  if StringMap.mem name preg.P.defs then
    ((* Printf.eprintf "definition %s exists, skipping\n" name ;*) preg)
  else
    { preg with P.defs = StringMap.add name def preg.P.defs }

(* FIXME: factoriser *)
let add_function preg (name, def) =
  if StringMap.mem name preg.P.funs then
    ((* Printf.eprintf "function %s exists, skipping\n" name ;*) preg)
  else
    { preg with P.funs = StringMap.add name def preg.P.funs }

let get_expression def =
  match def.P.expression with
  | P.Expr e -> e
  | _ -> assert false

let ml_identifier = B.String.map (function '.' -> '_' | c -> c)

let string_of_chars = B.String.of_chars
let int_of_chars l =
  int_of_string (string_of_chars l)

let while_primary is_plus =
  fun f -> fun _pos ->
    let rec aux acc pos =
      match f pos with
      | Some (np, nr) -> aux (nr::acc) np
      | None -> pos, acc in
    match aux [] _pos with
    | (_, []) as r -> if is_plus (*option=`PLUS*) then None else Some r
    | (np, r) -> Some (np, List.rev r)

let match_char c l =
  let rec aux = function
    | [] -> false
    | Any::_ -> true
    | (One c1)::tl -> c1 = c or aux tl
    | (Range (c1, c2))::tl -> (c >= c1 && c <= c2) or aux tl
  in aux l

let str2memo_type = function
  | "none" -> MemoNone
  | "fail" -> MemoFail
  | "success" -> MemoSuccess
  | "full" -> MemoFull
  | s -> failwith (Printf.sprintf "Unknown memoization option: '%s' (should be: 'none', 'fail', 'success' or 'full')" s)

let prefix2str = function
  | `AND -> "&"
  | `NOT -> "!"
  | `NORMAL -> ""

let suffix2str = function
  | `QUESTION -> "?"
  | `STAR -> "*"
  | `PLUS -> "+"
  | `NORMAL -> ""

let rangeChar2str = function
  | '"' -> "\\\""
  | '-' -> "\\-"
  | c -> Char.escaped c

let range2str = function
  | One c -> Printf.sprintf "%s" (rangeChar2str c)
  | Range (c1, c2) -> Printf.sprintf "%s-%s" (rangeChar2str c1) (rangeChar2str c2)
  | Any -> failwith "Unexpected Any in a non-trivial range"

let rec primary2str = function
  | P.Ident i -> i
  | P.Paren exp -> "(" ^ expr2str exp ^ ")"
  | P.Literal (l, cs) -> Printf.sprintf "\"%s\"%s" (String.escaped l) (if cs then "" else "~")
  | P.Class [Any] -> "."
  | P.Class cs -> Printf.sprintf "[%s]" (List.to_string range2str cs)

and item2str (prefix, primary, suffix) =
  Printf.sprintf "%s%s%s" (prefix2str prefix) (primary2str primary) (suffix2str suffix)

 (* FIXME add support for printing code *)
 (* FIXME add support for printing named items *)
and sequence2str (items, labels, _code) =
  if not (StringMap.is_empty labels) then
    failwith "item labels not supported in printing yet"
  else
    String.concat_map " " item2str items

and expr2str = function
  | P.Expr es -> String.concat_map " / " sequence2str es
  | P.App _ -> failwith "tgrammar:expr2str -> App"

let rule2str (name, (exp, _)) =
  Printf.sprintf "%s <- %s ;\n\n" name (expr2str exp.P.expression)

let grammar2str peg =
  List.to_string rule2str (StringMap.to_list peg.grammar)
