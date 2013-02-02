(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(*
    @author Adam Koprowski
**)

module T = Tgrammar
module P = Tgrammar.PreGrammar
module OcamlG = Ocaml.Cons
module List = BaseList
module String = BaseString

let pr = Printf.sprintf
let prErr = Printf.eprintf

let log fmt = Printf.eprintf (fmt^^"\n")
let error fmt = Printf.eprintf ("[31m"^^fmt^^"[0m\n")

 (* we count parts of the sequence starting from 1 (they can be accessed in semantic
    actions via __1, __2 etc. variables *)
let first_pos = 1
let argValue i = pr "__%d" i

(* FIXME, start using those constants consistenly!!! *)
let main_param_cache = "cache"
let main_param_pos_update = "pos_update"
let main_param_filename = "_filename"
let main_param_text = "_text"
let main_param_start = "_start"

let trx_runtime fn = ["Trx_runtime"; fn]

(* TODO, Line number directives #... in generated files; for generated parts
         put appropriate directives; otherwise they refer to wrong parts
         of .trx files *)
(* TODO, incremental parsing: return updated cache also for failed parses...
   (possibly by boxing all exceptions; even better not returning this value but
   doing in-place modifications in cache arguments) *)
(* TODO, grammar optimizations: choice re-ordering etc. *)
(* TODO, handle left-recursion *)
(* TODO, rules that produce no result don't need memoization of the result, only yes/no - use it *)
(* TODO, switch to camlp4 for Ocaml AST/pretty-printing? *)
(* TODO, optimization: get rid of string_of_chars for X+ productions *)
(* TODO, think about many different back-ends, we may want TRX-ocaml, TRX-QML, TRX-JS, ... TRX should be properly designed for that. *)

(* =========================================================================================================== *)
(* =========================================== CMD LINE arguments: =========================================== *)
(* =========================================================================================================== *)

let default_imperative_errors = false
let default_opt_errors = true
let default_opt_gen_res = true
let default_opt_inline_literals = true
let default_opt_inline_ranges = true
let default_opt_unfold_starplus = false

let grammarFn = ref None
let imperative_errors = ref default_imperative_errors
let opt_errors = ref default_opt_errors
let opt_gen_res = ref default_opt_gen_res
let opt_inline_literals = ref default_opt_inline_literals
let opt_inline_ranges = ref default_opt_inline_ranges
let opt_unfold_starplus = ref default_opt_unfold_starplus
let map_location_to_trx_file = ref true
let pp_grammar = ref false
let functorize = ref false
let binary = ref None
let incremental = ref false
let auto_ast = ref false
let rule_deps = ref false
let list_start = ref false
let memo_default = ref None
let debug_mode = ref false
let main = ref None
let basename = ref None
let no_mli = ref false
let analyze_grammar = ref false

(* =========================================================================================================== *)
(* ============================================= Results/patterns ============================================ *)
(* =========================================================================================================== *)

type parsingContext =
    { peg : string Tgrammar.grammar
    ; input : string
    ; gen_err : bool
    ; gen_res : bool
    }

let var = Ocaml.make_Var
let pvar v = Ocaml.PatVar (Ident.source v)
let vars = Ocaml.make_Varl
let pany = Ocaml.PatAny

let fail_id = List.map Ident.source (trx_runtime "Fail")
let ok_id = List.map Ident.source (trx_runtime "Ok")

let pair p1 p2 = OcamlG.tuple [p1; p2]
let pat_pair p1 p2 = OcamlG.pat_tuple [p1; p2]

let fail gen_err err lai =
  let result =
    if gen_err then
      Ocaml.Constructor (fail_id, [err])
    else
      OcamlG.none
  in
  if !incremental then
    pair result lai
  else
    result

let ok gen_err pos res err lai =
  let pos_res = OcamlG.tuple [pos; res] in
  let result =
    if gen_err then
      Ocaml.Constructor (ok_id, [pos_res; err])
    else
      OcamlG.some pos_res
  in
  if !incremental then
    pair result lai
  else
    result

let pat_fail gen_err err lai =
  let result =
    if gen_err then
      Ocaml.PatConstructor (fail_id, [err])
    else
      OcamlG.pat_none
  in
  if !incremental then
    pat_pair result lai
  else
    result

let pat_ok gen_err pos res err lai =
  let pos_res = OcamlG.pat_tuple [pos; res] in
  let result =
    if gen_err then
      Ocaml.PatConstructor (ok_id, [OcamlG.pat_tuple [pos_res; err]])
    else
      OcamlG.pat_some pos_res
  in
  if !incremental then
    pat_pair result lai
  else
    result

let map_return_value gen_err value return =
  OcamlG.make_match value
    [ pat_ok gen_err (pvar "pos") (pvar "res")   (pvar "err") (pvar "lai"),
      None (* guard *),
          ok gen_err (var "pos")  (return "res") (var "err")  (var "lai")
    ; pat_fail gen_err (pvar "err") (pvar "lai"),
      None (* guard *),
          fail gen_err (var "err")  (var "lai")
    ]

let map_lai_value gen_err value return =
  OcamlG.make_match value
    [ pat_ok gen_err (pvar "pos") (pvar "res") (pvar "err") (pvar "lai"),
      None (* guard *),
          ok gen_err (var "pos")  (var "res")  (var "err")  (return "lai")
    ; pat_fail gen_err (pvar "err") (pvar "lai"),
      None (* guard *),
          fail gen_err (var "err")  (var "lai")
    ]

let map_lai_value_to_max ctx value lai =
  map_lai_value ctx value
    (fun old_lai -> OcamlG.app3 (var "max") (var old_lai) lai)

(* =========================================================================================================== *)
(* =========================== Graph for computing dependencies between productions ========================== *)
(* =========================================================================================================== *)

module V : Graph.Sig.COMPARABLE with type t = string =
struct
  type t = string
  let equal u v = (u = v)
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
end

(** We will compute a directed graph where nodes are strings, which are names of functions for parsing
    productions. If opt_no_err is turned off then there is exactly one parsing function for every production
    and it has the prefix "try_" followed by production name (including module names etc.). With this
    optimization turned on, on top of this function there is another one with suffix "_no_err".
    The arrows correspond to dependencies, i.e. if there is arrow from A to B, then it means that parsing
    function A may call B. We use this dependency analysis to output only required functions (and to avoid
    unused variable warnings). *)
module G = Graph.Imperative.Digraph.ConcreteBidirectional (V)
module Neigh = Graph.Oper.Neighbourhood (G)
module SCC = Graph.Components.Make (G)
module Dot = Graph.Graphviz.Dot (
  struct
    include G
    let graph_attributes _g = []
    let default_vertex_attributes _g = []
    let vertex_name v = v
    let vertex_attributes _v = []
    let get_subgraph _v = None
    let default_edge_attributes _g = []
    let edge_attributes _edge = []
  end
)

(* parsing functions dependency graph *)
let dep_g = G.create ()

(** given: a dependency graph [g], a list of starting productions [all_prods] and a list of all generated
    production functions [all_prods], returns [all_prods] filtered to those functions that are really
    required in the final parser (as computed from dependencies). *)
let compute_needed_prods g start_names all_prods =
  let rec compute_reqs to_visit reqs =
    match to_visit with
    | [] -> reqs
    | x::xs ->
        if StringSet.mem x reqs then
          compute_reqs xs reqs
        else
          let n = G.succ g x in
          compute_reqs (xs @ n) (StringSet.add x reqs)
  in
  let required = compute_reqs start_names StringSet.empty in
  let req_fun (_pn, fn, _) = StringSet.mem fn required in
  List.filter req_fun all_prods

let partition_definitions g start_names all_prods =
  let needed = compute_needed_prods g start_names all_prods in
  let prod_for_name v (_, fn, _) = fn = v in
  let remove_unneeded v =
    if not (List.exists (prod_for_name v) needed) then
      G.remove_vertex dep_g v
  in
  G.iter_vertex remove_unneeded dep_g;
  let sccs = Array.to_list (SCC.scc_array dep_g) in
  let find_prod fn =
    match List.find_all (prod_for_name fn) needed with
    | [x] -> x
    | _ -> assert false
  in
  List.map (List.map find_prod) sccs

(* =========================================================================================================== *)
(* ========================================== Interface generation =========================================== *)
(* =========================================================================================================== *)

let find_start_prods peg =
  let prods = StringMap.to_list peg.T.grammar in
  let filter_start_prods (fn, (def, _)) =
    if def.P.mark then
      Some fn
    else
      None
  in
  List.filter_map filter_start_prods prods

let parse_function_name fn = Ident.source ("parse_" ^ String.lowercase fn)

let trx_annotate_location loc code =
  if !map_location_to_trx_file then
    match loc with
    | None -> code
    | Some r -> Ocaml.LineAnnot (r.T.line_number, r.T.file_name, code)
  else
    code

let generate_interface grammarFn peg =
  let interface_comment = Printf.sprintf "
*******************
*** DO NOT EDIT ***
*******************

This file was automatically generated by TRX from the grammar: %s
" grammarFn
  in
  let reader_module_name = "R" in
  let generate_signature fn =
    let parens s = "(" ^ s ^ ")" in
    let trx_pos_type = Ocaml.TypeName ([], ["Trx_runtime"; "pos"]) in
    let type_string = Ocaml.TypeConst Ocaml.TypeString in
    let filename_arg = Ocaml.TypeLabel (true, main_param_filename, type_string) in
    let start_arg = Ocaml.TypeLabel (true, main_param_start, trx_pos_type) in
    let input_arg =
      if !functorize then
        Ocaml.TypeName ([Ocaml.TypeVar "'input_a"; Ocaml.TypeVar "'input_b"], [reader_module_name; "input"])
      else
        type_string
    in
    let rule_def, _ = StringMap.find fn peg.T.grammar in
    let extra_args =
      if !auto_ast then
        []
      else
        List.map (fun (_name, t) -> Ocaml.TypeVerbatim (parens t)) peg.T.extra
    in
    let result_type =
      let final_type =
        if !auto_ast then
          Ocaml.TypeName ([], ["Trx_auto_ast"; "auto_ast"])
        else
          match rule_def.P.rule_type with
          | None -> failwith ("Unknown type for rule: " ^ fn ^ "; please specify the rule type in the grammar")
          | Some t -> Ocaml.TypeVerbatim (parens t)
      in
      Ocaml.TypeTuple [trx_pos_type; final_type]
    in
    let args = extra_args @ [filename_arg; start_arg; input_arg; result_type] in
    Ocaml.Val (parse_function_name fn, OcamlG.type_arrows args)
  in
  let start_prods = find_start_prods peg in
  let interface_funs = List.map generate_signature start_prods in
  let declarations =
    if !functorize then
       (* FIXME This is not good; use AST instead, but no appropriate construct for now in ocamllang/ocaml.ml *)
      [ Ocaml.Verbatim (Printf.sprintf "module Make : functor (%s : Reader.S) -> sig" reader_module_name)] @
      interface_funs @
      [ Ocaml.Verbatim "end" ]
    else
      interface_funs
  in
  let user_code =
    let gen_user_code acc (h, loc) =
      match h with
      | `types c | `decls c -> acc @ [trx_annotate_location loc (Ocaml.Verbatim c)]
      | `normal _ | `file _ | `inside _ -> acc
    in
    List.fold_left gen_user_code [] peg.T.header
  in
  Ocaml.Comment (interface_comment) :: user_code @ declarations

(* =========================================================================================================== *)
(* ========================================== Actual parsing logic =========================================== *)
(* =========================================================================================================== *)

let grammar_extras peg =
  if !auto_ast then
    []
  else
    List.map fst peg.T.extra

let add_flag_suffix gen_err id =
  let id = if gen_err then id else id ^ "_noerr" in
  id

let call_fun args = OcamlG.app_list args
let call_runtime fn args =
  OcamlG.app_list (vars (trx_runtime fn) :: args)

let call_string_sub p1 p2 = OcamlG.app_list [var "_get_sub"; p1; p2]
let call_emptyError pos = call_runtime "emptyError" [pos]
let call_option_to_res_msg res pos err = call_runtime "option_to_res_msg" [res; pos; err]
let call_option_to_res_err res pos err = call_runtime "option_to_res_err" [res; pos; err]
let call_range_to_error range = call_runtime "range_to_error" [range]
let call_while_primary ctx plus f pos =
  let fn = add_flag_suffix ctx.gen_err "while_primary" in
  let fn =
    if !opt_gen_res && not ctx.gen_res then
      fn ^ "_nores"
    else
      fn
  in
  call_runtime fn [plus; f; pos]

let call_process_literal pos literal case = call_runtime "process_literal" [var "_get_char"; var "_len"; pos; literal; case]
let call_process_range pos cl = call_runtime "process_range" [var "_get_char"; var "_len"; pos; cl]
let call_gen_syntax_error err =
  let input_string =
    if !functorize then
      call_fun [var "_get_sub"; OcamlG.int 0; var "_len"]
    else
      var main_param_text
  in
  let pos2loc () = call_fun [vars ["FilePos"; "get_pos_no_cache"]; input_string] in
  call_runtime "gen_syntax_error" [pos2loc (); err]

let call_setMainConstruct res pos err = call_runtime "setMainConstruct" [res; pos; err]
let call_decorateConstruct res pos err = call_runtime "decorateConstruct" [res; pos; err]
let call_update_cache cache update_pos = call_runtime "update_memoization_cache" [cache; update_pos]

let call_gatherErrors gen_err res =
  if !imperative_errors && gen_err then
    call_runtime "gatherErrors" [res]
  else
    res

let call_addErrorInfo err res =
  if !imperative_errors then
  Ocaml.Sequence (
    call_runtime "push_errInfo" [err],
    res
  )
  else
    call_runtime "addErrorInfo" [err; res]

exception Final of Ocaml.expr

let default_code_for_seq items =
  let rec aux pos res = function
    | [] ->
        begin match res with
        | Some pos ->
            (* there is only one argument that can be used as a result of a
               sequence, so we use it *)
            Some (false, var (argValue pos), false, pos)
        | None -> None
        end
    | i::is ->
        match i with
        | `NORMAL, _, _ -> (* this is a candidate for a productive argument *)
            begin match res with
            | None -> aux (pos + 1) (Some pos) is
            | Some _ -> None
            end
        | _ ->
            aux (pos + 1) res is
  in
  aux first_pos None items

let update_indent v =
  let indent_var = var "indent" in
  Ocaml.SetRef (indent_var, OcamlG.plus (Ocaml.GetRef indent_var) (OcamlG.int v))

let optional_rule_name annots =
  match annots.T.rule_name with
  | T.NoName -> OcamlG.none
  | T.PrimaryName def_name
  | T.SecondaryName def_name -> OcamlG.some (OcamlG.string def_name)

let grammar_rules peg =
  let prods = StringMap.to_list peg.T.grammar in
  let prod_sig (name, (_def, annots)) =
    let full_name = optional_rule_name annots in
    OcamlG.tuple [OcamlG.string name; full_name]
  in
  OcamlG.list (List.map prod_sig prods)

(* counter for position counters *)
let inputCnt = ref 0
(* name of the actually processed parsing function *)
let generated_prod = ref ""

let newInputVar () =
  let res = pr "input_%d" !inputCnt in
  incr inputCnt;
  res

let parsing_function gen_err id = add_flag_suffix gen_err ("try_" ^ id)

let memoization_table (id, err) = "memo_" ^ id ^ (if err then "_err" else "")

(* ############################## Ranges ############################## *)

let compile_class ctx cls =
  let current_char_var = "c" in
  let current_char = var current_char_var in
  let encode_range = function
    | T.Any -> OcamlG.true_
    | T.One c -> OcamlG.equal current_char (OcamlG.char c)
    | T.Range (c1, c2) ->
        OcamlG.band
          (OcamlG.ge current_char (OcamlG.char c1))
          (OcamlG.le current_char (OcamlG.char c2))
  in
  let rec encode_class = function
    | [] -> OcamlG.false_
    | [x] -> encode_range x
    | x::xs -> OcamlG.bor (encode_range x) (encode_class xs)
  in
  let trivial_class =
    let rec aux = function
      | [] -> false
      | T.Any::_xs -> true
      | _x::xs -> aux xs
    in
    aux cls
  in
  let class_cnd = encode_class cls in
  let check_eof = OcamlG.lt (var ctx.input) (var "_len") in
  let return x = (* TODO: refactor this pattern *)
    if !auto_ast then
      Ocaml.ConstructorPV ([Ident.source "Class"], [x])
    else
      x
  in
  let process_class =
    let new_pos = call_fun [var "succ"; var ctx.input] in
    let res = OcamlG.tuple [new_pos; return current_char] in
    if trivial_class then
      OcamlG.some res
    else
      Ocaml.Cond (class_cnd, OcamlG.some res, OcamlG.none)
  in
  let expected s = Ocaml.Constructor (List.map Ident.source (trx_runtime "Expected"), [OcamlG.string s]) in
  let err =
    let rec aux = function
      | [] -> []
      | T.Any :: _ -> raise (Final (expected "any character"))
      | T.One c::cs -> expected (pr "'%c'" c)::aux cs
      | T.Range (c1, c2)::cs -> expected (pr "['%c'-'%c']" c1 c2) :: aux cs
    in
    let l =
      try
        List.sort Pervasives.compare (aux cls)
      with
        Final c -> [c]
    in
    OcamlG.list l
  in
  let process =
    let get_char = call_fun [var "_get_char"; var ctx.input] in
    Ocaml.make_Letin (Ocaml.pf current_char_var) get_char process_class
  in
  let res = Ocaml.Cond (check_eof, process, OcamlG.none) in
  if ctx.gen_err then
    call_option_to_res_err res (var ctx.input) err
  else
    res

let recognize_class ctx cls =
  let range id cs = Ocaml.ConstructorPV ([Ident.source "Tgrammar"; Ident.source id], List.map OcamlG.char cs) in
  let encode_range = function
    | T.Any -> range "Any" []
    | T.One c -> range "One" [c]
    | T.Range (c1, c2) -> range "Range" [c1; c2]
  in
  let range_enc = OcamlG.list (List.map encode_range cls) in
  let range_var = "range" in
  let res = call_process_range (var ctx.input) (var range_var) in
  let res =
    if !auto_ast then
      let return res = Ocaml.ConstructorPV ([Ident.source "Class"], [var res]) in
      map_return_value ctx.gen_err res return
    else
      res
  in
  let err = call_range_to_error (var range_var) in
  let result =
    if ctx.gen_err then
      call_option_to_res_err res (var ctx.input) err
    else
      res
  in
  Ocaml.make_Letin (Ocaml.pf range_var) range_enc result

(* ############################## Literals ############################## *)

let match_literal ctx literal case offset success failure =
  let n = String.length literal in
  let end_offset = OcamlG.plus (var ctx.input) (OcamlG.int (n + offset)) in
  let within_input = OcamlG.le end_offset (var "_len") in
  let cmp_equal =
    let rec aux i =
      if i = String.length literal then
        OcamlG.true_
      else
        let this_pos = OcamlG.plus (var ctx.input) (OcamlG.int (i + offset)) in
        let input_char = call_fun [var "_get_char"; this_pos] in
        let literal_char = OcamlG.char (String.get literal i) in
        let cmp_this_char =
          if case then
            OcamlG.equal input_char literal_char
          else
            call_fun [vars ["Base"; "Char"; "equal_insensitive"]; input_char; literal_char]
        in
        OcamlG.band cmp_this_char (aux (i + 1))
    in
    aux 0
  in
  Ocaml.Cond (OcamlG.band within_input cmp_equal, success end_offset, failure)

let compile_literal ctx literal case =
  let literalStr = OcamlG.string (if case then literal else String.lowercase literal) in
  let return x =
    if !auto_ast then
      Ocaml.ConstructorPV ([Ident.source "Literal"], [var ctx.input; x])
    else
      x
  in
  let produce_result end_offset = OcamlG.some (OcamlG.tuple [end_offset; return literalStr]) in
  match_literal ctx literal case 0 produce_result OcamlG.none

let recognize_literal ctx literal case =
  let res = call_process_literal (var ctx.input) (OcamlG.string literal) (OcamlG.bool case) in
  if !auto_ast then
    let return res = Ocaml.ConstructorPV ([Ident.source "Literal"], [var ctx.input; var res]) in
    map_return_value ctx.gen_err res return
  else
    res

module CharOrder : (OrderedTypeSig.S with type t = char) =
struct
  type t = char
  let compare = Char.compare
end
module CharMap = BaseMap.Make (CharOrder)

let literals_choice ctx lits =
  let prefixes (x, _) (y, _) = String.is_prefix y x in
  let rec check_lit = function
    | [] -> ()
    | x::xs ->
        match List.filter (prefixes x) xs with
        | [] -> check_lit xs
        | y::_ -> failwith (Printf.sprintf "Literal %s shadows %s; please re-order them!" (fst y) (fst x))
  in
  let rec make_choice def pos matched options =
    let result s code pos =
      let pos_code = OcamlG.plus (var ctx.input) (OcamlG.int pos) in
      let posArg = argValue first_pos in
      let ss = OcamlG.string s in
      let v =
        if !auto_ast then
          Ocaml.ConstructorPV ([Ident.source "Literal"], [pos_code; ss])
        else
          match code with
          | None -> ss
          | Some c ->
              let argNeeded = String.is_contained posArg c in
              let res = Ocaml.Verbatim ("(" ^ c ^ ")") in
              if argNeeded then
                Ocaml.make_Letin (Ocaml.pf posArg) ss res
              else
                res
        in
        OcamlG.some (OcamlG.tuple [pos_code; v])
    in
    let build_match_map s code map =
      if s = "" then
        map
      else
        let c = s.[0] in
        let rest = String.right s (-1) in
        let cset = Option.default StringMap.empty (CharMap.find_opt c map) in
        CharMap.add c (StringMap.add rest code cset) map
    in
    let rec next_branch str options old_def =
      let accepting = StringMap.find_opt "" options in
      let def =
        match accepting with
        | Some code -> result matched code (pos + String.length str)
        | None -> old_def
      in
(*      Printf.eprintf "<%s> --- <%s> --- %s (def = %s)\n" matched str (String.concat_map ", " (fun s -> "<" ^ s ^ ">") (StringMap.keys options)) (if def = OcamlG.none then "-" else "X");*)
      if str <> "" && accepting <> None then
        old_def, `MatchString (str, options)
      else if StringMap.is_empty options || (accepting <> None && StringMap.size options = 1) then
        def, `Terminal
      else
        let match_map = StringMap.fold build_match_map options CharMap.empty in
        if CharMap.size match_map > 1 then
          if String.length str > 0 then
            def, `MatchString (str, options)
          else
            def, `Branch match_map
        else
          let c, opts = CharMap.random match_map in
          next_branch (str ^ String.make 1 c) opts def
    in
(*    Printf.eprintf "---------\n";*)
    match next_branch "" options def with
    | def, `Terminal ->
(*        Printf.eprintf "TERMINAL\n";*)
        def
    | def, `MatchString (str, options) ->
(*        Printf.eprintf "MATCH_STRING (%s, ...)\n" str;*)
        if str = "" then
          def
        else
          let rest = make_choice def (pos + String.length str) (matched ^ str) options in
          match_literal ctx str true pos (fun _ -> rest) def
    | def, `Branch match_map ->
(*        Printf.eprintf "BRANCH: {%s}\n" (String.concat_map ", " (String.make 1) (CharMap.keys match_map));*)
        let end_offset = OcamlG.plus (var ctx.input) (OcamlG.int pos) in
        let within_input = OcamlG.lt end_offset (var "_len") in
        let do_match =
          let build_match_case c vs cases =
            let pattern = Ocaml.PatConst (Ocaml.Char c) in
            let new_matched = matched ^ String.make 1 c in
            let value = make_choice def (pos + 1) new_matched vs in
            let guard = None in
            (pattern, guard, value) :: cases
          in
          let input_char = call_fun [var "_get_char"; end_offset] in
          let default_case = Ocaml.PatAny, None, def in
          let cases = CharMap.fold build_match_case match_map [] @ [default_case] in
          OcamlG.make_match input_char cases
        in
        Ocaml.Cond (within_input, do_match, def)
  in
  check_lit lits;
  let res = make_choice OcamlG.none 0 "" (StringMap.from_list lits) in
  if ctx.gen_err then
    let err_msg = String.concat_map " or " (fun l -> "\"" ^ fst l ^ "\"") lits in
    call_option_to_res_msg res (var ctx.input) (OcamlG.string err_msg)
  else
    res

(* ############################## Choice ############################## *)

let rec generate_exp ctx = function
  | P.App _ -> failwith "Unexpected App"
  | P.Expr [] -> assert false
  | P.Expr [s] ->
      let res = generate_seq ctx s in
      call_gatherErrors ctx.gen_err res
  | P.Expr (s::ss) ->
      let get_literals lits e =
        match lits with
        | None -> None
        | Some ls ->
            match e with
            | [_, P.Literal (l, true), _], _, c ->
                begin match c with
                | Some (false, ".sub", _, _)
                | None -> Some ((l, None) :: ls)
                | Some (false, c, _, false) -> Some ((l, Some c)::ls)
                | _ -> None
                end
            | _ -> None
      in
      match List.fold_left get_literals (Some []) (s::ss) with
      | None ->
          let res = generate_exp ctx (P.Expr ss) in
          let res =
            if ctx.gen_err then
              call_addErrorInfo (var "err") res
            else
              res
          in
          let return x =
            if !auto_ast then
              let decorate res = Ocaml.ConstructorPV ([Ident.source "Choice"], [res]) in
              map_return_value ctx.gen_err x (fun res -> decorate (var res))
            else
              x
          in
          let ge = ctx.gen_err in
          OcamlG.make_match (generate_seq ctx s) [
            pat_fail ge (pvar "err") (pvar "lai"),
            None,
            map_lai_value_to_max ge res (var "lai")
            ;
            pvar "ok",
            None,
            call_gatherErrors ge (return (var "ok"))
          ]
      | Some lits ->
          literals_choice ctx lits

(* ############################## Sequence ############################## *)

and generate_seq ctx (items, map, code) =
  (* if there is no provided [code] and there is only one part of the sequence that
     can be used as a result - use it *)
(*  jlog ~color:`green ~level:3 (pr "generate_seq [gen_err: %b]" ctx.gen_err);*)
  (* We substitute proper variables into [_pos_beg] and [_pos_end] variables,
   * substitute for positional variables (named parts of a sequence and catch
   * exceptions thrown from productions' code.
   *)
  let code_uses_arg argNo =
    match code with
    | None ->
        begin match default_code_for_seq items with
        | None -> false
        | Some (_, _, _, a) -> a == argNo
        end
    | Some (_, c, _, _) ->
        let used_in_label _key (i, _) b = b || (int_of_string i = argNo) in
        String.is_contained (argValue argNo) c || StringMap.fold used_in_label map false
  in
  let code_transform input =
    if !auto_ast then begin
      let make_arg i = var (argValue (first_pos + i)) in
      let args = List.init (List.length items) make_arg in
      let node = Ocaml.ConstructorPV ([Ident.source "Seq"], [var ctx.input; var input; OcamlG.list args]) in
      Some (false, node, false)
    end else
      match code with
      | None ->
          Option.map (fun (r, c, b, _) -> (r, c, b)) (default_code_for_seq items)
      | Some (range, ".sub", _, _) ->
          let get_substring = call_string_sub (var ctx.input) (OcamlG.minus (var input) (var ctx.input)) in
          Some (range, get_substring, false)
      | Some (range, c, loc, backtraceable) ->
          let subst_vars = function
            | "_pos_beg" -> ctx.input
            | "_pos_end" -> input
            | s -> s
          in
          let tokens = snd (Tokenizer.parse_tokenizer_tokens c) in
          (* substitute into _pos_beg and _pos_end variables *)
          let tokens = List.map subst_vars tokens in
          let code = String.concat "" tokens in
          (* substitute into named variables *)
          let is_var_used v = List.mem v tokens in
          let map = StringMap.filter_keys is_var_used map in
          let gen_label (key, (i, b)) =
            if b then
              failwith "TRX: unsupported type of labels"
            else
              let argNo = int_of_string i in
              Ocaml.pf key, var (argValue argNo)
          in
          let user_code =
            let code = OcamlG.verbatim (pr "( %s )" code) in
            trx_annotate_location loc code
          in
          let user_code =
            if StringMap.is_empty map then
              user_code
            else
              let labels = List.map gen_label (StringMap.to_list map) in
              Ocaml.Letin (labels, user_code)
          in
          Some (range, user_code, backtraceable)
  in
  let rec aux input argNo = function
    | [] ->
        let err = call_emptyError (var input) in
        let res_code, backtraceable =
          match code_transform input with
          | None -> OcamlG.unit, false
          | Some (range, base, backtraceable) ->
              let c =
                if range then
                  OcamlG.tuple [OcamlG.tuple [var ctx.input; var input]; base]
                else
                  base
              in
              c, backtraceable
        in
        let ge = ctx.gen_err in
        if backtraceable then
          OcamlG.make_match res_code [
            OcamlG.pat_none,
            None,
            fail ge err (var input)
            ;
            OcamlG.pat_some (pvar "res"),
            None,
            ok ge (var input) (var "res") err (var input)
          ]
        else
          let res = ok ge (var input) res_code err (var input) in
          call_gatherErrors ge res
    | i::is ->
        let argResult = argValue argNo in
        let inputVar = newInputVar () in
        let res = aux inputVar (argNo+1) is in
        let res =
          if ctx.gen_err then
            call_addErrorInfo (var "err") res
          else
            res
        in
        let genArg_res =
          match ctx.gen_res with
          | false -> false
          | true ->
              if !auto_ast then
                true
              else if !opt_gen_res then
                code_uses_arg argNo
              else
                true
        in
        let ge = ctx.gen_err in
        let item_ctx = { ctx with input=input; gen_res=genArg_res } in
        OcamlG.make_match (generate_item item_ctx i) [
          pat_fail ge (pvar "err") (pvar "lai"),
          None,
          call_gatherErrors ge (fail ge (var "err") (var "lai"))
          ;
          pat_ok ge (pvar inputVar) (pvar argResult) (pvar "err") (pvar "lai"),
          None,
          map_lai_value_to_max ge res (var "lai")
        ]
  in
  aux ctx.input 1 items

(* ############################## Item ############################## *)

and generate_item ctx (prefix, primary, suffix) =
  let predicate_res pred =
    let res =
      if !auto_ast then
        Ocaml.ConstructorPV ([Ident.source pred], [])
      else
        OcamlG.unit
    in
    res
  in
  let ge = ctx.gen_err in
  match prefix with
  | `NORMAL -> generate_suffix ctx (primary, suffix)
  | `AND ->
      let r = generate_suffix ctx (primary, suffix) in
      OcamlG.make_match r [
        pat_fail ge (pvar "err") (pvar "lai"),
        None,
        fail ge (var "err") (var "lai")
        ;
        pat_ok ge pany pany (pvar "err") (pvar "lai"),
        None,
        ok ge (var ctx.input) (predicate_res "And") (var "err") (var "lai")
      ]
  | `NOT ->
      let r = generate_suffix ctx (primary, suffix) in
      OcamlG.make_match r [
        pat_fail ge (pvar "err") (pvar "lai"),
        None,
        ok ge (var ctx.input) (predicate_res "Not") (var "err") (var "lai")
        ;
        pat_ok ge pany pany (pvar "err") (pvar "lai"),
        None,
        fail ge (var "err") (var "lai")
      ]

(* ############################## Suffix ############################## *)

and generate_suffix ctx (primary, suffix) =
  match suffix with
  | `NORMAL ->
      generate_primary ctx primary
  | `QUESTION ->
      let r = generate_primary ctx primary in
      let inputVar = newInputVar () in
      let return v =
        let ret =
          if !auto_ast then
            Ocaml.ConstructorPV ([Ident.source "Option"], [v])
          else
            v
        in
        ret
      in
      let ge = ctx.gen_err in
      OcamlG.make_match r [
        pat_fail ge (pvar "err") (pvar "lai"),
        None,
        ok ge (var ctx.input) (return OcamlG.none) (var "err") (var "lai")
        ;
        pat_ok ge (pvar inputVar) (pvar "r") (pvar "err") (pvar "lai"),
        None,
        ok ge (var inputVar) (return (OcamlG.some (var "r"))) (var "err") (var "lai")
      ]
  | `STAR | `PLUS ->
      let inputVar = newInputVar () in
      let f = OcamlG.lambda (Ident.source inputVar) (generate_primary { ctx with input = inputVar } primary) in
      let res = call_while_primary ctx (OcamlG.bool (suffix = `PLUS)) f (var ctx.input) in
      if !auto_ast then
        let return v =
          let cons =
            match suffix with
            | `STAR -> "Star"
            | `PLUS -> "Plus"
            | _ -> assert false
          in
          Ocaml.ConstructorPV ([Ident.source cons], [var v])
        in
        map_return_value ctx.gen_err res return
      else
        res

(* ############################## Primary ############################## *)

and generate_primary ctx = function
  | P.Ident id ->
      let pf = parsing_function ctx.gen_err id in
      G.add_edge dep_g !generated_prod pf;
      (*jlog ~color:`green ~level:3 (pr "Function dependency: %s --> %s" !generated_prod pf);*)
      call_fun (List.map var (pf :: grammar_extras ctx.peg @ [main_param_filename; main_param_text; ctx.input]))
  | P.Paren exp ->
      generate_exp ctx exp
  | P.Class cl ->
      if !opt_inline_ranges then
        compile_class ctx cl
      else
        recognize_class ctx cl
  | P.Literal (literal, case) ->
      let res =
        if !opt_inline_literals then
          compile_literal ctx literal case
        else
          recognize_literal ctx literal case
      in
      if ctx.gen_err then
        call_option_to_res_msg res (var ctx.input) (OcamlG.string ("\"" ^ literal ^ "\""))
      else
        res

(* ############################## Rule entry ############################## *)

let funPreliminaries () =
  (* let _len = String.length _text *)
  [ Ocaml.pf "_len",
    call_fun [vars [if !functorize then "R_" else "String"; "length"]; var main_param_text]

  (* let _get_char = String.unsafe_get _text *)
  ; Ocaml.pf "_get_char",
    if !functorize then
      call_fun [vars ["R_"; "get"]; var "_text"]
    else
      call_fun [vars ["String"; "unsafe_get"]; var "_text"]

  ; Ocaml.pf "_get_sub",
    call_fun [vars [if !functorize then "R_" else "String"; "sub"]; var "_text"]
(*
  ; Ocaml.pf "_input",
    call_fun [var "_get_sub"; OcamlG.int 0; var "_len"]
*)
  ] @
    begin if !debug_mode then
      [Ocaml.pf "indent", Ocaml.MakeRef (OcamlG.int 0)]
    else
      []
    end

let generate_prod peg gen_err (name, (def, annots)) =
  let funName = parsing_function gen_err name in
  (*jlog ~color:`green ~level:3 (pr "Generating function: %s [gen_err: %b]" funName gen_err);*)
  G.add_vertex dep_g funName;
  generated_prod := funName;
  let inputVar = "input" in
  let ctx =
    { peg = peg
    ; input = inputVar
    ; gen_err = gen_err
    ; gen_res = true
    }
  in
  let funBody = generate_exp ctx def.P.expression in
  let funBody =
    if !auto_ast then
      let full_name = optional_rule_name annots in
      let return v = Ocaml.ConstructorPV ([Ident.source "Rule"],
                                          [var inputVar; var "pos"; OcamlG.string name; full_name; var v])
      in
      map_return_value gen_err funBody return
    else
      funBody
  in
  let expected s = Ocaml.Constructor (List.map Ident.source (trx_runtime "Expected"), [OcamlG.string ("<" ^ s ^ ">")]) in
  let funBody =
    if gen_err then
      match annots.T.rule_name with
      | T.NoName -> funBody
      | T.PrimaryName def_name -> call_setMainConstruct funBody (var inputVar) (expected def_name)
      | T.SecondaryName def_name -> call_decorateConstruct funBody (var inputVar) (OcamlG.list [expected def_name])
    else
      funBody
  in
  let funBody =
    if annots.T.rule_memo = T.MemoNone then
      funBody
    else
      let hashTbl = var (memoization_table (name, gen_err)) in
      let memo_val = call_fun [vars ["Hashtbl"; "find"]; hashTbl; var inputVar] in
      let lookup_memo =
        if !debug_mode then
          (* FIXME *)
          let debug = Ocaml.Verbatim (pr "Printf.eprintf \"%%s<%s pos='%%d' />\\n\" (String.make !indent ' ') %s" name inputVar)
          in
          let res_vname = "res" in
          let res_var = var res_vname in
          Ocaml.make_Letin (Ocaml.pf res_vname) memo_val (Ocaml.Sequence (debug, res_var))
        else
          memo_val
      in
      let do_memo res = call_fun [vars ["Hashtbl"; "add"]; hashTbl; var inputVar; res] in
      let ge = ctx.gen_err in
      let failure_memo res =
        OcamlG.make_match res [
          pat_fail ge pany pany (*FIXME*),
          None,
          do_memo res
          ;
          pany,
          None,
          OcamlG.unit
        ]
      in
      let success_memo res =
        OcamlG.make_match res [
          pat_ok ge pany pany pany pany (*FIXME*),
          None,
          do_memo res
          ;
          pany,
          None,
          OcamlG.unit
        ]
      in
      let memo res =
        match annots.T.rule_memo with
        | T.MemoNone -> assert false
        | T.MemoFail -> failure_memo res
        | T.MemoSuccess -> success_memo res
        | T.MemoFull -> do_memo res
        | _ -> assert false
      in
      let update_memo_with res =
        let snd =
          if !debug_mode then
            let debug_end = Ocaml.Verbatim (pr "Printf.eprintf \"%%s</%s>\\n\" (String.make !indent ' ')" name) in
            Ocaml.Sequence (update_indent (-1), Ocaml.Sequence (debug_end, res))
          else
            res
        in
        Ocaml.Sequence (memo res, snd)
      in
      let update_memo res =
        let res_vname = "res" in
        let res_var = var res_vname in
        let res = Ocaml.make_Letin (Ocaml.pf res_vname) res (update_memo_with res_var) in
        if !debug_mode then
          let debug_beg = Ocaml.Verbatim (pr "Printf.eprintf \"%%s<%s pos='%%d'>\\n\" (String.make !indent ' ') %s" name inputVar)
          in
          Ocaml.Sequence (debug_beg, Ocaml.Sequence (update_indent 1, res))
        else
          res
      in
      let handle_not_memoized = [ pvar "Not_found", None, update_memo funBody ] in
      Ocaml.Try (lookup_memo, handle_not_memoized)
  in
  let funWithPreliminaries =
(*
    let rec letin (lets, body) =
      match lets with
      | [] -> body
      | x::xs ->
          Ocaml.Letin ([x], letin (xs, body))
    in
*)
    Ocaml.Letin (funPreliminaries (), funBody)
  in
  let argList = grammar_extras peg @ [main_param_filename; main_param_text; inputVar] in
  let funWithArgs = Ocaml.Abs (List.map Ocaml.pf argList, funWithPreliminaries) in
  name, funName, trx_annotate_location def.P.origin funWithArgs

(* ############################## Main processing ############################## *)

let empty_hashtbl = call_fun [vars ["Hashtbl"; "create"]; OcamlG.int 128]

let generate_memo_tables peg prod_funs_part =
  let cache_field fn = Ocaml.Dot (var main_param_cache, fn) in
  let make_memo_table (prod_conf, keep_cache) =
    let new_hashTbl =
      if !incremental && keep_cache then
        call_update_cache (cache_field (memoization_table prod_conf)) (var main_param_pos_update)
      else
        empty_hashtbl
    in
    memoization_table prod_conf, new_hashTbl
  in
  let memo_tables_conf =
    let for_memo (pn, _, _) =
      let def, annots = StringMap.find pn peg.T.grammar in
      let keep_cache = def.P.retain_cache in
      if annots.T.rule_memo = T.MemoNone then
        None
      else if !opt_errors then
        Some [(pn, true), keep_cache; (pn, false), keep_cache]
      else
        Some [(pn, true), keep_cache]
    in
    let allMemos = List.concat (List.filter_map for_memo (List.concat prod_funs_part)) in
    List.uniq (List.sort Pervasives.compare allMemos)
  in
  let memo_tables = List.map make_memo_table memo_tables_conf in
  let memo_tables_retain = List.filter_map (fun (conf, keep) -> if keep then Some (memoization_table conf) else None) memo_tables_conf in
  memo_tables, memo_tables_retain

(* FIXME, This can go to Trx_runtime *)
let generate_parse_with () =
  let input_arg = main_param_text in
  let start_arg = main_param_start in
  let parse_with_f =
    let result = OcamlG.tuple [var "pos"; var "res"] in
    let call_parse f = call_fun [var f; var input_arg; var start_arg] in
    let body =
      if !opt_errors then
          OcamlG.make_match (call_parse "f_noerr") [
            pat_ok false (pvar "pos") (pvar "res") pany pany,
            None,
            result
            ;
            pat_fail false pany pany,
            None,
            OcamlG.make_match (call_parse "f_err") [
              pat_ok true pany pany pany pany,
              None,
              Ocaml.Assert OcamlG.false_
              ;
              pat_fail true (pvar "err") pany,
              None,
              call_gen_syntax_error (var "err")
            ]
          ]
      else
        OcamlG.make_match (call_parse "f") [
          pat_ok true (pvar "pos") (pvar "res") pany pany,
          None,
          result
          ;
          pat_fail true (pvar "err") pany,
          None,
          call_gen_syntax_error (var "err")
        ]
    in
    body
  in
  let f_args =
    let args =
      begin if !opt_errors then
        ["f_noerr"; "f_err"]
      else
        ["f"]
      end @
        [ input_arg; start_arg ]
    in
    List.map Ocaml.pf args
  in
  Ocaml.make_Let (Ocaml.pf "parse_with") (Ocaml.Abs (f_args,
    Ocaml.Letin (funPreliminaries (), parse_with_f)))

let generate_main start_prods f =
  if not (List.exists (fun v -> f = v) start_prods) then
    failwith (Printf.sprintf "Unknown starting rule: %s" f);
  let filename = OcamlG.app_list [vars ["Array"; "get"]; vars ["Sys"; "argv"]; Ocaml.Cons.int 1] in
  let input = OcamlG.app_list [vars ["File"; "content"]; filename] in
  let do_parse = OcamlG.app_list [Ocaml.Cons.var (parse_function_name f); input] in
  Ocaml.Let [ Ocaml.Pat Ocaml.Cons.Pattern.any, do_parse ]

let generate_empty_cache memo_tables_retain =
  let empty_cache_field fn = fn, empty_hashtbl in
  let cache_val = Ocaml.Record (None,List.map empty_cache_field memo_tables_retain) in
  Ocaml.Let [ Ocaml.pf "empty_cache", Ocaml.Abs ([Ocaml.Pat Ocaml.Cons.pat_unit], cache_val) ]

let generate_cache_type memo_tables_retain =
  let cache_rule_type_param s = "'" ^ s ^ "_restype" in
  let rules = List.map cache_rule_type_param memo_tables_retain in
  let type_int = Ocaml.TypeConst Ocaml.TypeInt in
  let cache_record_field fn =
    false, fn, Ocaml.TypeName ([type_int; Ocaml.TypeName ([], [cache_rule_type_param fn])], ["Hashtbl"; "t"])
  in
  let cache_record = Ocaml.TypeRecord (List.map cache_record_field memo_tables_retain) in
  Ocaml.Type [rules, "cache", cache_record]

let generate_prepare_cache memo_tables =
  let clear_one (cache, _) = call_fun [vars ["Hashtbl"; "clear"]; var cache] in
  let clear_all = OcamlG.sequence (List.map clear_one memo_tables) in
  Ocaml.make_Let (Ocaml.pf "prepare_cache") (OcamlG.lambda (Ident.source "()") (*FIXME*) clear_all)

let generate_try_parse_functions dep_g prod_funs_part =
  (* check for strong component that access itself *)
  let is_rec = function
    | [(_,fn,_)] -> not (G.mem_vertex dep_g fn) || List.exists (G.V.equal fn) (G.succ dep_g fn)
    | _ -> true
  in
  let let_ rec_ l = if rec_ then Ocaml.Letrec l else Ocaml.Let l in
  List.map (fun part ->
    let_ (is_rec part) (List.map (fun (_pn, fn, body) -> 
      Ocaml.pf fn, body
    ) part)
  ) prod_funs_part

let generate_parse_functions peg start_prods =
  let make_parse_fn fn =
    let extras_names = grammar_extras peg in
    let fun_args =
      let cache_params =
        if !incremental then
          [main_param_cache; main_param_pos_update]
        else
          []
      in
      let fname = Ocaml.Opt (main_param_filename, None, Some (OcamlG.string "")) in
      let start = Ocaml.Opt (main_param_start, None, Some (OcamlG.int 0)) in
      List.map Ocaml.pf extras_names @ [fname; start; Ocaml.pf main_param_text] @ List.map Ocaml.pf cache_params
    in
    let body =
      let try_with err = call_fun (List.map var (parsing_function err fn :: extras_names @ [ main_param_filename ])) in
      let try_parse =
        if !opt_errors then
          [try_with false; try_with true]
        else
          [try_with true]
      in
      let text =
        if !functorize then
          call_fun [vars ["R_"; "make"]; var main_param_text]
        else
          var main_param_text
      in
      let args = try_parse @ [text; var main_param_start] in
      Ocaml.Sequence (
        call_fun [var "prepare_cache"; OcamlG.unit],
        call_fun (var "parse_with" :: args)
      )
    in
    Ocaml.make_Let (Ocaml.Cons.param (parse_function_name fn)) (Ocaml.Abs (fun_args, body))
  in
  List.map make_parse_fn start_prods

let generate_parser_for peg =
  let prods = StringMap.to_list peg.T.grammar in
  let gen_prods gen_err = List.map (generate_prod peg gen_err) prods in
  (* generate all parsing functions (imperatively computes dependencies) *)
  let start_prods = find_start_prods peg in
  let gen_entries flag = List.map (parsing_function flag) start_prods in
  let flags =
    if !opt_errors then
      [false; true]
    else
      [true]
  in
  let entry_fns = List.concat (List.map gen_entries flags) in
  let all_prods_funs =  List.concat (List.map gen_prods flags) in
  (* compute all parsing functions required for the final parser *)
  let prod_funs_part = partition_definitions dep_g entry_fns all_prods_funs in
  let memo_tables, memo_tables_retain = generate_memo_tables peg prod_funs_part in
  let cache_type = generate_cache_type memo_tables_retain in
  let parse_with = generate_parse_with () in
  let prepare_cache = generate_prepare_cache memo_tables in
  let empty_cache = generate_empty_cache memo_tables_retain in
  let grammar_rules = Ocaml.Let [ Ocaml.pf "_grammar_rules", grammar_rules peg ] in
  let parse_functions = generate_try_parse_functions dep_g prod_funs_part in
  let parse_entry_functions = generate_parse_functions peg start_prods in
  let memo_tables_decl = List.map (fun (t, e) -> Ocaml.make_Let (Ocaml.pf t) e) memo_tables in
  let declarations =
    (if !incremental then [ cache_type; empty_cache ] else [] ) @
    grammar_rules :: parse_with :: memo_tables_decl @ [prepare_cache] @ parse_functions @ parse_entry_functions
  in
  let declarations =
    match !main with
    | None -> declarations
    | Some f -> declarations @ [ generate_main start_prods f ]
  in
  if !functorize then
    let reader_arg = "R_", Some (vars ["Reader"; "S"]) in
    [Ocaml.DeclareFunctor ("Make", [reader_arg], None, Ocaml.Structure declarations)]
  else
    declarations

let generate_parser peg =
  let gen_user_code (inside, outside) (h, loc) =
    match h with
    | `inside c -> inside @ [trx_annotate_location loc (Ocaml.Verbatim c)], outside
    | `normal c | `types c -> inside, outside @ [trx_annotate_location loc (Ocaml.Verbatim c)]
    | `file _ | `decls _ -> inside, outside
  in
  let inside, outside = List.fold_left gen_user_code ([], []) peg.T.header in
  let includes = [] in
  let prelude =
    if !auto_ast then
      includes
    else
      includes @ inside @ outside
  in
  prelude @ generate_parser_for peg

let show_rule_deps peg =
  ignore (generate_parser_for peg);
  Dot.output_graph stdout dep_g

let optimize_memoization peg =
  let skip_memoization def =
    match def.P.expression with
(*
    | P.Expr [[_; _], _, _] -> true (* two items *)
    | P.Expr [[_], _, _; [_], _, _] -> true (* a choice between two single items *)
*)
    | P.Expr [[item], _, _] -> (* a single item *)
        begin match item with
        | _, P.Literal _, _ -> true (* a single literal *)
        | _, P.Class _, _ -> true (* a single class *)
        | _ -> false
        end
    | _ -> false
  in
  let optimize_rule _ (def, annots) n =
    if annots.T.rule_memo <> T.MemoNone && skip_memoization def then
      n + 1, (def, { annots with T.rule_memo = T.MemoNone })
    else
      n, (def, annots)
  in
  let _n, g = StringMap.fold_map optimize_rule peg.T.grammar 0 in
  (*jlog ~color:`green ~level:2 (pr "Turning off memoization for %d (out of the total of %d) trivial rules"
                                 n (StringMap.size peg.T.grammar));*)
  { peg with T.grammar = g }

(* =========================================================================================================== *)
(* ============================================ Cmd. line + main ============================================= *)
(* =========================================================================================================== *)

let usage_msg = Printf.sprintf "%s: Ocaml parser generator for the Opa project\nUsage: %s [options] syntax_file.[trx|prx]\n" Sys.argv.(0) Sys.argv.(0)

let parse_args () =
  let anon_fun s = grammarFn := Some s in
  Arg.parse (Arg.align [
    ("--no-opt-errors",
    Arg.Unit (fun _ -> opt_errors := false),
    " switches off optimization of error handlings");

    ("--no-opt-gen-res",
    Arg.Unit (fun _ -> opt_gen_res := false),
    " turns off optimization exhibiting fact that some parsing results are not needed");

    ("--no-opt-inline-literals",
    Arg.Unit (fun _ -> opt_inline_literals := false),
    " turns off optimization by inlining literal recognition");

    ("--no-opt-inline-ranges",
    Arg.Unit (fun _ -> opt_inline_ranges := false),
    " turns off optimization by inlining range recognition");

    ("--opt-unfold-star-plus",
    Arg.Unit (fun _ -> opt_unfold_starplus := true),
    " turns on star/plus unfolding (enabling their memoization); always off when no memoization used!");

    ("--not-map-location-to-trx-file",
    Arg.Unit (fun _ -> map_location_to_trx_file := false),
    " turns off mapping locations in generated file to the original .trx via #... annotations");

    ("--imperative-errors",
    Arg.Unit (fun _ -> imperative_errors := true),
    " turns on imperative error handling (for faster compilation)");

    ("--memoization",
    Arg.String (fun s -> memo_default := Some (T.str2memo_type s)),
    " LEVEL sets default memoization level LEVEL=[none | fail | success | full]");

    ("--incremental",
    Arg.Unit (fun _ -> failwith "support for incremental temporarily disabled..." (*incremental := true*)),
    " generates a parser with support for incremental parsing");

    ("--binary",
    Arg.String (fun s -> binary := Some s),
    " FILE outputs binary grammar to the specified file");

    ("--functorize",
    Arg.Unit (fun () -> functorize := true),
    " produces a parser as a functor");

    ("--auto-ast",
    Arg.Unit (fun () -> auto_ast := true),
    " instead of grammar productions builds an AST reflecting the grammar");

    ("--debug-mode",
    Arg.Unit (fun () -> debug_mode := true),
    " make parser print debug output");

    ("--rule-deps",
    Arg.Unit (fun _ -> rule_deps := true),
    " instead of producing a parser show rule dependencies");

    ("--pretty-print",
    Arg.Unit (fun _ -> pp_grammar := true),
    " instead of producing a parser just print the (normalized) grammar");

    ("--list-start",
    Arg.Unit (fun _ -> list_start := true),
    " instead of producing a parser just list starting productions of the grammar");

    ("--output-basename",
    Arg.String (fun s -> basename := Some s),
    " filename (without extension) of the output files for the parser");

    ("--verbose",
    Arg.Unit (fun () -> ()),
    " deprecated");

    ("--more-verbose",
    Arg.Unit (fun () -> ()),
    " deprecated");

    ("--no-mli",
    Arg.Unit (fun _ -> no_mli := true),
    " do not generate interface (.mli) file");

    ("--analyze-grammar",
    Arg.Unit (fun _ -> analyze_grammar := true),
    " does not produce a parser but instead just analyzes the grammar");

    ("--main",
    Arg.String (fun s -> main := Some s),
    " RULE produces parser with 'main' function parsing with given production")

  ]) anon_fun
    (usage_msg^"Options:")

let set_bool_option opt v var =
  match v with
  | "true" -> var := true
  | "false" -> var := false
  | _ ->
      (error "Unknown value in the grammar for the boolean option <%s=%s> (should be 'false' or 'true')" opt v;
      exit 2)

let rec process_options opts =
  let process_option (opt, v) =
    let set_bool = set_bool_option opt v in
    match opt with
    | "opt-errors" -> set_bool opt_errors
    | "opt-inline-literals" -> set_bool opt_inline_literals
    | "opt-inline-ranges" -> set_bool opt_inline_ranges
    | "imperative-errors" -> set_bool imperative_errors
    | "memoization" -> () (* This option is taken care of in Pgrammar.read_grammar *)
    | _ ->
       (error "Unknown option in the grammar file: <%s=%s>" opt v; exit 2)
  in
  match opts with
  | [] -> ()
  | x::xs ->
      process_option x;
      process_options xs

let print_options () = ()
  (*let memo2str = function
    | None -> "unspecified (defaults to 'success' unless overriten in the grammar)"
    | Some T.MemoNone -> "none"
    | Some T.MemoFail -> "fail"
    | Some T.MemoSuccess -> "success"
    | Some T.MemoFull -> "full"
    | _ -> assert false
  in
  let memoOpt2str label m = pr "\n    --%-20s = %s" label (memo2str !m) in
  jlog ~color:`green ~level:2 (pr
  "TRX with:
    --incremental          = %b
    --imperative-errors    = %b
    --opt-errors           = %b
    --opt-no-res           = %b
    --opt-inline-literals  = %b
    --opt-inline-ranges    = %b
    --debug-mode           = %b
    --functorize           = %b
    --auto--ast            = %b%s\n"
    !incremental !imperative_errors !opt_errors !opt_gen_res !opt_inline_literals
    !opt_inline_ranges !debug_mode !functorize !auto_ast
    (memoOpt2str "memoization" memo_default))*)

let non_verbose f =
  let res = f () in
  res

let is_verbose () = false

let print_start_prods peg =
  log "Start productions:";
  let print_prod p = log " > %s" p in
  List.iter print_prod (Pgrammar.start_definitions peg.T.grammar)

let write_to_file fn code =
  let out = open_out fn in
  let optimized_code = OcamlUtils.optimize code in
  OcamlPrint.Output.code out optimized_code;
  close_out out

let _ =
  parse_args ();
  print_options ();
  if !rule_deps && (!opt_inline_literals || !opt_inline_ranges) then
    prErr "Optimization options will be ignored as only printing graph dependencies";
  if !memo_default = Some T.MemoNone then
    opt_unfold_starplus := false;
  match !grammarFn with
  | None -> prErr "Missing grammar file!\n"
  | Some grammarFn ->
      match !binary with
      | Some f ->
          Pgrammar.output_binary_grammar ~verbose:(is_verbose ()) ~input:grammarFn f
      | None ->
          try
            let baseName =
              match !basename with
              | None -> Filename.chop_extension grammarFn
              | Some s -> s
            in
            let fn_ml, fn_mli = baseName ^ ".ml", baseName ^ ".mli" in
            (*jlog ~color:`green ~level:2 (pr "TRX applied to grammar {%s} will generate code in {%s} and interface in {%s} " grammarFn fn_ml fn_mli);*)
            let read () = Pgrammar.read_grammar ~analyze:!analyze_grammar ?memo_default:!memo_default ~verbose:(is_verbose ()) ~unfold_starplus:!opt_unfold_starplus None grammarFn in
            let peg, _used = non_verbose read in
            let peg = optimize_memoization peg in
            let retain_cache (def, _) = def.P.retain_cache in
            if !incremental && StringMap.is_empty (StringMap.filter_val retain_cache peg.T.grammar) then
              prErr "Grammar for incremental parsing needs at least one <icache> rule!"
            else if !analyze_grammar then begin
              Printf.printf "Grammar analysis:\n";
              Pgrammar.analyze_grammar peg.T.grammar
            end else
              process_options peg.T.options;
              parse_args (); (* re-parse cmd. line arguments so that they take precedence over grammar options *)
              if !rule_deps then
                show_rule_deps peg
              else if !list_start then
                print_start_prods peg
              else if !pp_grammar then
                print_string (T.grammar2str peg)
              else begin
                if !no_mli then
                  ()
                else
                  write_to_file fn_mli (generate_interface grammarFn peg);
                write_to_file fn_ml (generate_parser peg)
              end
          with
            Pgrammar.GrammarParse err ->
              prErr "Failed while parsing the input grammar: {%s} -> %s!\n" grammarFn err;
              exit 2
