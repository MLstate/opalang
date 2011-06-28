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
    @author Adam Koprowski
**)

(**
 * A tool for converting TRX grammar to Coq format acceptable by certified TRX
**)

module T = Tgrammar
module P = T.PreGrammar

let module_prefix = ref ""

let prodName s = 
  if String.is_prefix !module_prefix s then
    let at = String.length !module_prefix + 1 in
    "P_" ^ String.sub s at (String.length s - at)
  else
    s

let prod2str s = Printf.sprintf "%s => \"%s\"" (prodName s) (prodName s)

let header () = Printf.sprintf "
(** -- begin LICENCE
    (c) 2006-2009 MLstate
    All rights reserved.
    This file is confidential and intended solely for the addressee(s).
    Any unauthorized use or dissemination is prohibited.
    end LICENCE -- 
**)

(*
 * WARNING! Grammar generated automatically by %s from %s
 * =============== Edit at your own risk ==================
 *)
Require Import MLstate.TRX.TRX.

Set Implicit Arguments.
" Sys.argv.(0) Sys.argv.(1)

let domain peg = 
  let prods = StringMap.keys peg.T.grammar in
  Printf.sprintf "
Module Domain <: Enumeration.

  Inductive prod : Set := %s
  .

  Open Scope string_scope.
  Definition prod_to_string (p : prod) : String.string :=
  match p with%s
  end.

  Definition A := prod.

  Lemma eqA_dec : forall x y : A, {x = y} + {x <> y}.
  Proof.
    decide_enumeration_equality.
  Qed.

  Identity Coercion iprod2prod : A >-> prod.

  Definition A_enum : enumeration prod.
  Proof.
    provide_enumeration (%s::nil).
  Qed.

End Domain."
    (List.to_string (fun s -> "\n  | " ^ prodName s) prods)
    (List.to_string (fun s -> "\n  | " ^ prod2str s) prods)
    (String.concat_map "::" prodName prods)

let make_char s = Printf.sprintf "\"%s\"%%char" s

let coq_char = function
  | c when Char.code c < 32 -> Printf.sprintf "\"%03d\"%%char" (Char.code c)
  | '"' -> make_char "\"\""
  | c -> make_char (String.make 1 c)

let string2coq s = 
  let escape_char = function
    | '"' -> "\"\""
    | '\n' | '\t' | '\r' -> failwith (Printf.sprintf "Unsupported escape sequence in string: %s" s)
    | c -> String.make 1 c
  in
  let chars = String.char_list_of_string s in
  let coq_chars = List.map escape_char chars in
  String.concat "" coq_chars

let prefix2str = function
  | `AND -> "[&]"
  | `NOT -> "[!]"
  | `NORMAL -> ""

let suffix2str = function
  | `QUESTION -> "[?]"
  | `STAR -> "[*]"
  | `PLUS -> "[+]"
  | `NORMAL -> ""

let primary_as_seq p =
  let item = `NORMAL, p, `NORMAL in
  [item], StringMap.empty, None

let rec range2str = function
  | [T.Any] -> "[.]"
  | [T.Range (c1, c2)] -> Printf.sprintf "[|%s -- %s|]" (coq_char c1) (coq_char c2)
      (* TODO, improve that: *)
  | [T.One c] when Char.code c < 32 || c == '"' -> Printf.sprintf "%s" (coq_char c)
  | [T.One c] -> Printf.sprintf "\"%c\"" c
  | ls -> 
      let es = expr2str (P.Expr (List.map (fun s -> primary_as_seq (P.Class [s])) ls)) in
      Printf.sprintf "(%s)" es

and primary2str = function
  | P.Ident i -> prodName i
  | P.Paren exp -> "(" ^ expr2str exp ^ ")"
  | P.Literal (l, true) -> 
      if String.length l = 1 then
	primary2str (P.Class [T.One l.[0]])
      else
	Printf.sprintf "\"%s\"" (string2coq l)
  | P.Literal (l, false) -> failwith "case-insensitive literals for now unsupported by certified TRX :|"
  | P.Class cs -> range2str cs

(* TODO Smarter parenthesization, i.e. put them only where neccessary *)
and item2str (prefix, primary, suffix) = 
  prefix2str prefix ^ primary2str primary ^ suffix2str suffix

and seq2str (items, _, _) = String.concat_map "; " item2str items

and expr2str = function
  | P.Expr es -> String.concat_map " / " seq2str es
  | P.App _ -> failwith "trx2cert :: expr2str :: App"

let production2str name (expr, _) acc = 
  Printf.sprintf "%s\n    | %s => %s" acc (prodName name) (expr2str expr.P.expression)

let grammar peg name = 
  let module_name = String.uncapitalize name in
  let moduleName = String.capitalize name in
  module_prefix := moduleName;
  Printf.sprintf "
Module %s_Grammar <: PEG_spec.

  Module PD <: PEG_domain.
    Module Export PS := Domain.
    Definition prod_type (p : prod) : Type := True.
  End PD.

  Module Export PE := ParsingExpressions PD.
  Open Scope peg_scope.

  Definition prod_coercion (p : prod) : pexp := nonTerminal p.
  Coercion prod_coercion : prod >-> pexp.

  Definition preproduction (p : prod) : pexp :=
    match p with%s
    end.

  Definition production : prod -> PExp True :=
    fun p => promote (preproduction p).

  Definition start := %s.

End %s_Grammar.

Module %s_Parser := TRX %s_Grammar.

Definition build_%s_parser := %s_Parser.build_parser.
"
  moduleName
  (StringMap.fold production2str peg.T.grammar "")
  (prodName peg.T.start) moduleName moduleName moduleName module_name 
  moduleName

let _ =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [grammar].trx\n" Sys.argv.(0)
  else
    let grammarFn = Sys.argv.(1) in
    try 
      let peg, _ = Pgrammar.read_grammar ~verbose:true None grammarFn in
      Printf.printf "%s%s\n%s\n" (header ()) (domain peg) (grammar peg (File.module_name grammarFn))
    with
    | Pgrammar.GrammarParse err -> 
	Printf.eprintf "Failed while parsing the input grammar: {%s}!\n" grammarFn
