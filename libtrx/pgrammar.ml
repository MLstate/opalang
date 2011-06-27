(*
    Copyright Â© 2011 MLstate

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

(* FIXME, This module seroiusly needs a clean-up... I hope to have time one day... *)

(* depends*)
module List = BaseList
module String = BaseString

(* alias *)
module B = Base
module T = Tgrammar
module P = T.PreGrammar

(**)

exception GrammarParse of string
exception GrammarCheck of string

(* TODO: change this printf *)
let log fmt =
  Printf.eprintf (fmt^^"\n")


(* FIXME: déséquilibre file / input *)
(* FIXME: les extra des include sont ajoutés !!! *)
(* FIXME: il faudrait établir une map : definition -> file pour afficher la liste des définitions dont la redéfinition n'a pas été possible (dans les include) : on paie ici le choix d'un namespace unique... *)

(* FIXME: add optional path *)
let find_file f =
  File.content f

let module_name_of_name n =
  let chop s = File.chop_extension s in
  String.capitalize (chop (Filename.basename n))

let add_globals pg cur_mod new_mod =
  List.fold_left (
    fun pg x ->
      { pg with
          P.defs = StringMap.add ((module_name_of_name new_mod) ^ "_" ^ x)
          (StringMap.find ((module_name_of_name cur_mod) ^ "_" ^ x) pg.P.defs) pg.P.defs
      }
  ) pg

(**
|parse_pre_grammar|


The trx module system is the following:
1. each file is a module (by default "file.trx" -> "File")
2. each file contains rules
3. each file opens itself and opened modules
4. there is only one module level
5. the search order is first itself, then opened modules by order


Currently,
[
Identifier <- (
        / Module [.] Name {{ __1 ^ "_" ^ __3 }}
        / Name {{ module_name ^ "_" ^ __1 }}
        ) Spacing {{ __1 }}
]

What to do when a rulename is defined in multiple modules?
Highest priority = module itself, then opened modules by order of definition (including recursives open ?)

*)
let parse_pre_grammar ?(name="Main") ?(stoppable=false) ~verbose input =
  let already_read = ref StringSet.empty in
  let rec load pg input name module_name =
    FilePos.add_file name input;
    if StringSet.mem name !already_read then
      pg
    else begin
      if verbose then log "parsing %s (stoppable:%b)" name stoppable;
      let input_len = String.length input in
      let result =
        try
          let lastp, pg = Trxparse.parse_trxparse_grammar pg name (module_name_of_name module_name) stoppable input in
          if lastp = input_len then begin
            if verbose then log "(%s) read %d/%d bytes" name lastp input_len;
            StringMap.fold (
              fun x include_def pg ->
                let pg = { pg with P.incl = StringMap.remove x pg.P.incl } in
                let new_module = match include_def.P.it with P.Read -> x | P.Incl -> name in
                let pg = add_globals pg module_name new_module include_def.P.gl in
                already_read := StringSet.add name !already_read;
                let old_header = pg.P.pheader in
                let peg = load { pg with P.pheader = [] } (find_file x) x new_module in
                { peg with P.pheader = old_header @ peg.P.pheader }
            ) pg.P.incl pg
          end else
            (* FIXME, Adam, this should be handled in the grammar *)
            raise (GrammarParse (B.sprintf "error parsing '%s': only %d out of %d bytes processed" name lastp input_len))
        with
        | Trx_runtime.SyntaxError (pos, err) ->
            raise (GrammarParse (B.sprintf "error parsing '%s': %s" name (Trx_runtime.show_error input pos err)))
      in
      FilePos.uncache name;
      result
    end
  in
  let pg = load T.empty_pre_grammar input name name in
  (*log "parse_pre_grammar: end" ;*)
  pg

let read_pre_grammar ?stoppable ~verbose name = parse_pre_grammar ?stoppable ~verbose ~name (File.content name)

let rewrite_funs pg =
  (* TODO: recursive functions *)
  let all_functions = pg.P.funs in
  let rec rewrite_fun functions bindings expr =
    (* substitution *)
    let rec aux_expr = function
      | P.Expr sl -> P.Expr (aux_seql sl)
      | P.App (f, vars) ->
          begin match StringMap.find_opt f functions with
          | None ->
              if StringMap.mem f all_functions then
                failwith (B.sprintf "function %s is recursive" f)
              else
                failwith (B.sprintf "function %s is undefined" f)
          | Some (fdef, _) ->
              let functions = StringMap.remove f functions in
              let expected_arity = List.length fdef.P.vars in
              if expected_arity = List.length vars then
                let bindings = List.fold_left2 (fun acc idfun expra -> StringMap.add idfun (aux_seql expra) acc) bindings fdef.P.vars vars in
                rewrite_fun functions bindings fdef.P.expr
              else failwith (B.sprintf "function %s is of arity %d" f expected_arity)
          end
    and aux_seql sl = List.map aux_seq sl
    and aux_seq (il, map, code) = List.map aux_item il, map, code
    and aux_item (pre, pri, suf) = pre, aux_pri pri, suf
    and aux_pri = function
      | P.Paren e -> P.Paren (aux_expr e)
      | P.Ident id as pri ->
          begin match StringMap.find_opt id bindings with
          | None -> pri
          | Some e -> P.Paren (P.Expr e) (** There's no need to apply aux_expr once again here since it's done just above, in let bindings = ... aux_seql ... AND it wouldn't be the right binding environment !! **)
          end
      | pri -> pri
    in aux_expr expr
  in
  { pg with
      P.defs = StringMap.map (fun (def, msg_error) -> { def with P.expression = rewrite_fun all_functions StringMap.empty def.P.expression }, msg_error) pg.P.defs
  }

let dependencies pg =
(*  Printf.eprintf "All definitions: %s" (String.concat_map ", " (fun s -> s) (StringMap.keys pg));*)
  (** ajoute les dépendances de l'expression à set *)
  let rec dep_of_expression set = function
    | [] -> raise (GrammarCheck "empty expression!")
    | expr -> List.fold_left (
        fun acc (pl, _, _) ->
          List.fold_left (
            fun acc (_, primary, _) ->
              match primary with
              | P.Ident s ->
                  if StringMap.mem s pg then StringSet.add s acc
                  else raise (GrammarCheck (B.sprintf "definition '%s' missing!" s))
              | P.Paren (P.Expr e) -> dep_of_expression acc e
              | P.Paren _ -> assert false
              | _ -> acc
          ) acc pl
      ) set expr in
  StringMap.map (fun (def,_msg_error) -> dep_of_expression StringSet.empty (T.get_expression def)) pg

let grammar_error s =
  log "[31m%s[0m" s;
  exit 2

let grammar_analysis pg =

(*
  let prop2str = function
    | `Empty -> "Empty"
    | `NonEmpty -> "NonEmpty"
    | `Fail -> "Fail"
    | `Success -> "Success"
  in
*)

  let rec analyze_def prop def =
(*    jlog (Printf.sprintf "Analyze def: %s, property: %s" def (prop2str prop));*)
    let def, _ = StringMap.find def pg in
    analyze_exp prop def.P.expression

  and analyze_exp prop exp =
(*    jlog (Printf.sprintf "Analyze exp %s, property: %s" (expr_to_string exp) (prop2str prop));*)
    match prop with
    | `Success -> analyze_exp `Empty exp || analyze_exp `NonEmpty exp
    | _ ->
        match exp with
        | P.App _
        | P.Expr [] -> assert false
        | P.Expr [x] -> analyze_seq prop x
        | P.Expr (x::xs) ->
            match prop with
            | `Fail ->
                analyze_seq `Fail x && analyze_exp `Fail (P.Expr xs)
            | `Empty
            | `NonEmpty ->
                analyze_seq prop x || (analyze_seq `Fail x && analyze_exp prop (P.Expr xs))
            | `Success -> assert false

  and analyze_seq prop ((seq, q1, q2) as seqf) =
(*    jlog (Printf.sprintf "Analyze seq %s, property: %s" (if seq = [] then "" else seq_to_string seqf) (prop2str prop));*)
    match seq, prop with
    | _, `Success -> analyze_seq `Empty seqf || analyze_seq `NonEmpty seqf
    | [], `Empty -> true
    | [], `Fail
    | [], `NonEmpty -> false
    | x::xs, `Empty -> analyze_item `Empty x && analyze_seq `Empty (xs, q1, q2)
    | x::xs, `Fail -> analyze_item `Fail x || (analyze_item `Success x && analyze_seq `Fail (xs, q1, q2))
    | x::xs, `NonEmpty ->
        (analyze_item `NonEmpty x && analyze_seq `Success (xs, q1, q2)) ||
          (analyze_item `Success x && analyze_seq `NonEmpty (xs, q1, q2))

  and analyze_item prop ((prefix, primary, suffix) as item) =
(*    jlog (Printf.sprintf "Analyze item %s, property: %s" (item_to_string item) (prop2str prop));*)
    if prefix = `NORMAL then
      analyze_suffix prop (primary, suffix)
    else
      match prop with
      | `NonEmpty -> false
      | `Success -> analyze_item `Empty item || analyze_item `NonEmpty item
      | `Empty
      | `Fail ->
          let p =
            match prefix, prop with
            | `AND, `Empty -> `Success
            | `NOT, `Empty -> `Fail
            | `AND, `Fail -> `Fail
            | `NOT, `Fail -> `Success
            | _ -> assert false
          in
          analyze_item p (`NORMAL, primary, suffix)

  and analyze_suffix prop (primary, suffix) =
    match suffix with
    | `NORMAL -> analyze_primary prop primary
    | `QUESTION -> (* e? := e / empty *)
        let e = `NORMAL, primary, `NORMAL in
        let empty = `NORMAL, P.Literal ("", false), `NORMAL in
        let make_option i = i, StringMap.empty, None in
        analyze_exp prop (P.Expr ([make_option [e]; make_option [empty]]))
    | `STAR ->
        begin match prop with
        | `Empty -> analyze_suffix `Fail (primary, `NORMAL)
        | `NonEmpty -> analyze_suffix `NonEmpty (primary, `NORMAL)
        | `Success -> true
        | `Fail -> false
        end
    | `PLUS -> (* e+ := e; e* *)
        analyze_seq prop ([(`NORMAL, primary, `NORMAL); (`NORMAL, primary, `STAR)], StringMap.empty, None)

  and analyze_primary prop = function
(*    jlog (Printf.sprintf "Analyze primary %s, property: %s" (primary_to_string primary) (prop2str prop));*)
  | P.Paren e -> analyze_exp prop e
  | P.Ident id -> analyze_def prop id
  | P.Literal (l, _) ->
      begin match prop with
      | `Empty -> String.length l = 0
      | `NonEmpty -> String.length l > 0
      | `Success -> true
      | `Fail -> true
      end
  | P.Class _ -> (* character range has the same characteristics as a literal of length 1 *)
      analyze_primary prop (P.Literal ("X", false))
  in
  analyze_def, analyze_exp, analyze_seq, analyze_item, analyze_primary

let analyze_def pg =
  let (analyze_def, _, _, _, _) = grammar_analysis pg in analyze_def

let analyze_exp pg =
  let (_, analyze_exp, _, _, _) = grammar_analysis pg in analyze_exp

let analyze_seq pg =
  let (_, _, analyze_seq, _, _) = grammar_analysis pg in analyze_seq

let analyze_item pg =
  let (_, _, _, analyze_item, _) = grammar_analysis pg in analyze_item

let analyze_primary pg =
  let (_, _, _, _, analyze_primary) = grammar_analysis pg in analyze_primary

let check_wf pg name =

  let rec check_wf_def stack name =
    let def, _ = StringMap.find name pg in
    if List.mem name stack then
      grammar_error (Printf.sprintf "Grammmar contains forbidden left-recursion: %s"
        (String.concat_map " -> " (fun i -> i) (List.rev (name::stack)))
      );
    check_wf_exp (name::stack) def.P.expression

  and check_wf_exp stack = function
    | P.App _ -> assert false
    | P.Expr [] -> ()
    | P.Expr ((x, _, _)::xs) ->
        check_wf_seq stack x;
        check_wf_exp stack (P.Expr xs)

  and check_wf_seq stack = function
    | [] -> ()
    | x::xs ->
        check_wf_item stack x;
        if analyze_item pg `Empty x then
          check_wf_seq stack xs

  and check_wf_item stack (_prefix, prim, suffix) =
    check_wf_primary stack prim;
    match suffix with
    | `STAR
    | `PLUS ->
        if analyze_primary pg `Empty prim then
          grammar_error
            (Printf.sprintf "The expression <%s> in rule <%s> admits empty string, while it is marked with a %s. This would result in a looping parser and hence is forbidden."
              (T.primary_to_string prim)
              (List.hd stack)
              (match suffix with `STAR -> "star (*)" | `PLUS -> "plus (+)" | _ -> assert false)
            )
    | _ -> ()

  and check_wf_primary stack = function
    | P.Paren e -> check_wf_exp stack e
    | P.Ident id -> check_wf_def stack id
    | P.Literal _
    | P.Class _ -> ()

  in
  check_wf_def [] name

let check_grammar pg =
  let dep = dependencies pg in
  let rec add_definition name set =
    if StringSet.mem name set then set
    else
      let name_dep = StringMap.find name dep in
      StringSet.fold add_definition name_dep (StringSet.add name set)
  in
  (** définitions initiales *)
  let starts = StringMap.fold (fun name (def,_msg_error) acc -> if def.P.mark then name::acc else acc) pg [] in
  (** liste des définitions utilisées *)
  let def_used = List.fold_left (fun acc x -> add_definition x acc) StringSet.empty starts in
  let is_loop name = check_wf pg name in
  StringSet.iter is_loop def_used;
  def_used

let start_definitions g =
  StringMap.fold (
    fun name (def,_) acc ->
      if def.P.mark then name::acc else acc
  ) g []

let unfold_star_and_plus peg =
  let star_rule_id = ref 0 in
  let new_rules = ref StringMap.empty in
  let rule_for_unfolded_star rule_id primary suffix =
    let item_from_primary e = `NORMAL, e, `NORMAL in
    let make_sequence seq code = List.map item_from_primary seq, StringMap.empty, Some (false, code, None, false) in
    let non_empty = make_sequence [primary; P.Ident rule_id] "__1::__2" in
    let empty =
      match suffix with
      | `STAR -> make_sequence [P.Literal ("", true)] "[]"
      | `PLUS -> make_sequence [primary] "[__1]"
    in
    let rule_def =
      { P.expression = P.Expr [non_empty; empty]
      ; P.debug = false
      ; P.mark = false
      ; P.retain_cache = false
      ; P.rule_type = None
      ; P.origin = None
      }
    in
    let rule_annots = { T.rule_name = T.NoName; T.rule_memo = T.MemoNoInfo } in
    rule_def, rule_annots
  in
  let unfold_star_plus prefix primary suffix =
    let rule_id = incr star_rule_id; !star_rule_id in
    let rule_name = Printf.sprintf "_starplus_unfolding_%d" rule_id in
    let new_primary = P.Ident rule_name in
    new_rules := StringMap.add rule_name (rule_for_unfolded_star rule_name primary suffix) !new_rules;
    prefix, new_primary, `NORMAL
  in
  let rec process_primary = function
    | P.Paren e -> P.Paren (process_expression e)
    | e -> e
  and process_item (prefix, primary, suffix) =
    let primary = process_primary primary in
    match suffix with
    | (`STAR | `PLUS) as suffix -> unfold_star_plus prefix primary suffix
    | _ -> prefix, primary, suffix
  and process_sequence (items, vars, code) = (List.map process_item items, vars, code)
  and process_expression =  function
    | P.App _ -> failwith "pgrammar::unfold_star_and_plus: unexpected [App] in the grammar"
    | P.Expr ss -> P.Expr (List.map process_sequence ss)
  in
  let process_definition def = { def with P.expression = process_expression def.P.expression } in
  let process_rule (def, annot) = (process_definition def, annot) in
  let new_grammar = StringMap.map process_rule peg.T.grammar in
  let merge_rule _ _ = assert false (* new_rules should have IDs disjoint from the grammar *) in
  { peg with T.grammar = StringMap.merge merge_rule new_grammar !new_rules }

let infer_memoization_options ?(memo_default=T.MemoFull) peg =
  let infer_memoization = function
    | T.MemoNoInfo -> memo_default
    | T.MemoNone
    | T.MemoFail
    | T.MemoSuccess
    | T.MemoFull as memo -> memo
  in
  let process_rule (def, annot) =
    let new_annot = { annot with T.rule_memo = infer_memoization annot.T.rule_memo } in
    def, new_annot
  in
  { peg with T.grammar = StringMap.map process_rule peg.T.grammar }

(* FIXME: détection d'erreurs ici ou teds suffit ? *)
(*   try ... *)
(*   with *)
(*     e -> *)
(*       let _, _, _, last_ok = positions ... *)
(*       raise e *)
let grammar_of_pre_grammar ~memo_default ~unfold_starplus start_opt pg =
  let pg = rewrite_funs pg in
  let used = check_grammar pg.P.defs in
  (* FIXME: ne conserver que used dans pg, renvoyer juste pg en type abstrait ! *)
  let start = match start_opt with
  | Some s -> s
  | _ ->
      let stdefs = start_definitions pg.P.defs in
      if stdefs = [] then raise (GrammarCheck "no start definition")
      else List.hd stdefs
  in
  let g =
    { T.start = start
    ; T.grammar = pg.P.defs
    ; T.extra = pg.P.pextra
    ; T.options = pg.P.poptions
    ; T.header = pg.P.pheader
    }
  in
  let grammar_memo_default () =
    let check_memo memo (opt, v) =
      if opt = "memoization" then
        Some (T.str2memo_type v)
      else
        memo
    in
    List.fold_left check_memo None g.T.options
  in
  let memo_default =
    match memo_default with
    | None -> grammar_memo_default ()
    | Some _ -> memo_default
  in
  let g = if unfold_starplus then unfold_star_and_plus g else g in
  let g = infer_memoization_options ?memo_default g in
  g, used

let read_grammar ?stoppable ?memo_default ?(unfold_starplus=true) ~verbose start name =
  grammar_of_pre_grammar ?memo_default ~unfold_starplus start (read_pre_grammar ?stoppable ~verbose name)

let parse_grammar ?(name="Main") ?stoppable ?memo_default ?(unfold_starplus=true) ~verbose start text =
  grammar_of_pre_grammar ?memo_default ~unfold_starplus start (parse_pre_grammar ~name ?stoppable ~verbose text)

let list_start ~verbose name =
  let pg = read_pre_grammar ~verbose name in
  let stdefs = start_definitions pg.P.defs in
  List.iter (fun s -> log "%s" s) stdefs

(* FIXME: only the grammar *)
let output_binary_grammar ~verbose ?(input="trxparse.trx") ?start output_file =
  let g, _ = read_grammar ~verbose start input in
  let oc = open_out output_file in
  output_value oc (g:'a T.grammar) ;
  close_out oc

let input_binary_grammar input_file =
  let ic = open_in input_file in
  log "input_binary_grammar: begin" ;
  let (grammar:'a T.grammar) = input_value ic in
  log "input_binary_grammar: end" ;
  close_in ic ;
  grammar

(* 2/11/2010 Adam: removing dead, deprecated code: old Henri's TRX interpreter &
   some un-used (non-working?) functions for incremental parsing. Dig deep in
   git history if you think you may need that... *)
