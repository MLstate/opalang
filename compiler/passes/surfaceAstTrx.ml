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
(*
   @author Adam Koprowski
**)

(* HACK : please, clean-up in opa lang *)
module Parser_utils = OpaParserUtils

open Trx_ast
(*open SurfaceAstHelper*)

let opt_compile_ranges = true

(* ================= naming conventions =============== *)

let runtime_module = "Parser_private"
let input_name = "__input__"
let partial_flag_name = "__partial__"
let range_type = Printf.sprintf "%s.range" runtime_module

let position_var_name = Printf.sprintf "pos__%s"

let seq_it_name =
  let r = ref 0 in
  fun i ->
    Pervasives.incr r;
    Printf.sprintf "__seq_%d_%d" i !r

let choice_fun_name =
  let r = ref 0 in
  fun () ->
    Pervasives.incr r;
    Printf.sprintf "__choice_%d" !r

(* ================== AST manipulation ================ *)

module C = SurfaceAstCons.StringCons
let iterator_res () = C.T.name ~tyl:[C.T.name "itextrator"; C.T.fresh ()] Opacapi.Types.tuple_2
let option_iterator_res () = C.T.name ~tyl:[iterator_res ()] Opacapi.Types.option
let coerce_as_option_iterator_res e = C.E.coerce e (option_iterator_res ())
let none () = coerce_as_option_iterator_res (C.E.simple_record "none")
let some e = coerce_as_option_iterator_res (C.E.record1 "some" e)
let match_opt e pe1 pe2 = C.E.match_opt ~ty:(iterator_res ()) e pe1 pe2
let match_option e pe1 pe2 = C.E.match_option ~ty:(iterator_res ()) e pe1 pe2

let (!) = C.E.ident
let (&) = C.E.applys
let (<.>) = C.E.dot

 (* FIXME, this should go to SurfaceAstCons or similar module *)
let opa_eq e1 e2 = (!"Int" <.> "==") & [e1; e2]
let opa_ge e1 e2 = (!"Int" <.> ">=") & [e1; e2]
let opa_and e1 e2 = (!"&&") & [e1; e2]
let opa_or e1 e2 = (!"||") & [e1; e2]

let itextrator_next e = (!"Itextrator" <.> "next") & [e]

(* ============== TRX values manipulation ============= *)

let success_exp it v = some (C.E.tuple_2 it v)
let failure_exp () = none ()

let success_pat_gen it p = C.P.some (C.P.tuple_2 (C.P.var it) p)
let success_pat it v = success_pat_gen it (C.P.var v)
let success_wild_pat () = C.P.some (C.P.any ())
let failure_pat () = C.P.none ()

(* ================ auxiliary functions =============== *)

let call_runtime_fun f args = (!runtime_module <.> f) & args

let rec exists_expr p ((Expr seqs, _): _ Trx_ast.expr) = List.exists (exists_seq p) seqs
and exists_seq p ((seq_node, _) : _ Trx_ast.seq) =
  List.exists (exists_item p) seq_node.seq_items ||
  Option.exists p seq_node.seq_code
and exists_item p (({item_primary=(primary_node,_); _}, _): _ Trx_ast.item) =
  match primary_node with
  | Parens e -> exists_expr p e
  | Class _
  | Any
  | Literal _ -> false
  | Code e
  | Rule e
  | DynamicLiteral e -> p e

(* TODO, can we somehow improve this to also ignore results (if not needed) in sub-parsers? *)
let is_name_used_in_production_or_following_subrules code_opt subrules argName =
  let predicate = OpaWalk.Expr.appears_str argName in
  (* when there is no code associated with TRX expression, we will just take a
     substring of the input, which does not use any intermediate results *)
  Option.exists predicate code_opt || List.exists (exists_item predicate) subrules

(* ==================== translation =================== *)
let ( |> ) x f = f x
let translate_class cl =
  let make_range r = C.T.coerce_name r range_type in
  let translate_range = function
    | `ONE c -> make_range (C.E.record1 "one" (C.E.int c))
    | `RANGE (c1, c2) -> make_range (C.E.record ["from", C.E.int c1; "to", C.E.int c2])
  in
  cl |> List.map translate_range |> C.E.list

let compile_class input cl =
  let char = "char" in
  let new_it = "new_it" in
  let condition_for = function
    | `ONE c -> opa_eq !char (C.E.int c)
    | `RANGE (c1, c2) -> opa_and (opa_ge !char (C.E.int c1)) (opa_ge (C.E.int c2) !char)
  in
  let rec match_class = function
    | [] -> C.E.false_ ()
    | [x] -> condition_for x
    | x::xs -> opa_or (condition_for x) (match_class xs)
  in
  let check_class =
    let new_it = C.T.coerce_name !new_it Opacapi.Types.itextrator in
    C.E.if_ (match_class cl) (success_exp new_it !char) (failure_exp ())
  in
  match_opt (itextrator_next input)
    (C.P.none (), failure_exp ())
    (C.P.some (C.P.tuple_2 (C.P.var new_it) (C.P.var char)), check_class)

let rec effective_seq_suffix is =
  let effective_item i =
    match i.item_prefix with
    | `NORMAL -> true
    | `AND | `NOT -> false
  in
  match is with
  | [] -> false
  | (x, _loc)::xs -> effective_item x || effective_seq_suffix xs

let rec translate_expression ~nested ~res_needed input (Expr es, loc) =

  let rec aux = function
    | [] -> [], failure_exp ()
    | [s] -> [], translate_seq ~nested ~res_needed input s
    | s::ss ->
        let funs, last_exp = aux ss in
        let n = choice_fun_name () in
        let this_exp =
          match_option (translate_seq ~nested ~res_needed input s)
          (!n & []) (fun s -> some !s)
        in
        funs @ [n, last_exp], this_exp
  in
  let generate () =
    let funs, exp = aux es in
    let make_choice_fun (n, exp) = n, C.E.lambda [] exp in
    C.E.letins (List.map make_choice_fun funs) exp
  in
  SurfaceAstCons.with_label loc generate

and translate_seq ~nested ~res_needed org_input (seq, loc) =
  let rec aux input i used_names res_needed = function
  | [] ->
      let result =
        if res_needed then
          let get_substring = (!"Text" <.> "itsub") & [org_input; input] in
          Option.default get_substring seq.seq_code
        else
          C.E.void ()
      in
      if nested then
        success_exp input result
      else
        call_runtime_fun "check_partial_lazy" [!partial_flag_name; input; (C.E.lambda []result)]

  | (x, loc)::xs ->
      let item_name = x.item_name in
      let it_name = seq_it_name i in
       (* we need this item's result if it's default or derived name is used in the production *)
      let item_needed = res_needed && Option.default_map false (is_name_used_in_production_or_following_subrules seq.seq_code xs) item_name in
      let translated_item = translate_item ~nested:(nested || effective_seq_suffix xs) ~res_needed:item_needed input (x, loc) in
      let generate () =
        let provide_position_var result name =
          let var_name = position_var_name name in
          if is_name_used_in_production_or_following_subrules seq.seq_code xs var_name then
            let pos = (!"Itextrator" <.> "pos") & [input] in
            C.E.letin var_name pos result
          else
            result
        in
        let item_name_pattern =
          match item_name with
          | None -> C.P.any ()
          | Some v -> C.P.var v in
        let result =
           (* used names are extended with the optional explicit item name *)
          let use_name n =
            if StringSet.mem n used_names then
              failwith (Printf.sprintf "Use of the name %s ambiguous in a parser!" n);
            StringSet.add n used_names
          in
          let used_names = Option.default_map used_names use_name item_name in
           (* we compute the result *)
          let res = aux !it_name (i+1) used_names res_needed xs in
           (* if production uses [pos__name], where [name] is the explicit rule name,
              then we make this variable available *)
          let res = Option.default_map res (provide_position_var res) item_name in
          res
        in
        match_opt translated_item
          (failure_pat (), failure_exp ())
          (success_pat_gen it_name item_name_pattern, result)
      in
      SurfaceAstCons.with_label loc generate
  in
  (* A small improvement to the heuristic that does not produce result when it is not needed.
     We evaluate user provided productions even if it seems we don't need the result, as they
     may contain side-effects and it would be baffling for the user if they were not executed *)
  let res_needed' = res_needed || seq.seq_code <> None in
  let generate () = aux org_input 1 StringSet.empty res_needed' seq.seq_items in
  SurfaceAstCons.with_label loc generate

and translate_item ~nested ~res_needed input ((i, loc) as item) =
  let r = translate_suffix ~nested:(nested || i.item_prefix <> `NORMAL) ~res_needed input item in
  let generate () =
    let void = C.E.void () in
    match i.item_prefix with
    | `AND -> match_opt r (failure_pat (), failure_exp ()) (success_pat "_it" "r", success_exp input !"r")
    | `NOT -> match_opt r (failure_pat (), success_exp input void) (success_wild_pat (), failure_exp ())
    | `NORMAL -> r
  in
  SurfaceAstCons.with_label loc generate

and translate_suffix ~nested ~res_needed input (i, loc) =
  let this_nested =
    match i.item_suffix with
    | `NORMAL | `QUESTION -> false
    | `PLUS | `STAR -> true
  in
  let is_nested = nested || this_nested in
  let primary input = translate_primary ~nested:is_nested ~res_needed input i.item_primary in
  let generate () =
    match i.item_suffix with
    | `NORMAL -> primary input
    | `QUESTION ->
        let it_name = "it" in
        let res_name = "r" in
        match_opt (primary input)
          (failure_pat (), success_exp input (C.E.none ()))
          (success_pat it_name res_name, success_exp !it_name (C.E.some !res_name))
    | `PLUS | `STAR ->
        let input_var = "input_it" in
        let f = C.E.lambda_var input_var (primary !input_var) in
        let fun_name = "primary_list" ^ if res_needed then "" else "_no_res" in
        call_runtime_fun fun_name [C.E.bool (i.item_suffix = `PLUS); f; input]
  in
  SurfaceAstCons.with_label loc generate

and translate_primary ~nested ~res_needed input (p, loc) =
  let generate p =
    match p with
    | Parens e ->
        translate_expression ~nested ~res_needed input e

    | DynamicLiteral e ->
        let funName = "parse_literal" in
        call_runtime_fun funName [input; e]

    | Literal (l, cs) ->
(*
         (* we treat 1-character literals "x" as a class [x] *)
        if Cactutf.length l = 1 then
          let cl = Class [`ONE (Cactutf.get l 0)] in
          translate_primary ~res_needed input (cl, loc)
        else
*)
          let funName = if cs then "parse_literal" else "parse_literal_case_insensitive" in
          call_runtime_fun funName [input; C.E.string l]

    | Any ->
        itextrator_next (C.T.coerce_name input Opacapi.Types.itextrator)

    | Class cl ->
        if opt_compile_ranges then
          SurfaceAstCons.with_label loc (fun () -> compile_class input cl)
        else
          call_runtime_fun "parse_range" [input; translate_class cl]

      (* TODO do we really need distinction between Rule & Code tags? Consider
              implementing Rule by means of Code (possibly renaming it) *)
    | Rule id ->
        let partial =
          if nested then
            C.E.true_ ()
          else
            !partial_flag_name
        in
        let coerced_id = C.T.coerce_name id Opacapi.Types.Parser.general_parser in
        coerced_id & [partial; input]

    | Code code ->
        let user_parser_var = Parser_utils.fresh_name () in
        let user_parser =  C.T.coerce_name code Opacapi.Types.Parser.general_parser in
        let partial =
          if nested then
            C.E.true_ ()
          else
            !partial_flag_name
        in
        let call_parser = !user_parser_var & [partial; input] in
        C.E.letin user_parser_var user_parser call_parser

  in
  SurfaceAstCons.with_label' loc generate p

(* --- main entry point --- *)
let translate_rule e =
  let rule_exp = translate_expression ~nested:false ~res_needed:true !input_name e in
  let args = List.map C.P.ident [partial_flag_name; input_name] in
  let parser_code = C.E.lambda args rule_exp in
  let coerced_parser_code = C.T.coerce_name parser_code Opacapi.Types.Parser.general_parser in
  let () =
    #<If:SA_TRX>
      Format.eprintf "TRX <<\n%a\n>>@." OpaPrint.string#expr coerced_parser_code
    #<End>
  in coerced_parser_code
