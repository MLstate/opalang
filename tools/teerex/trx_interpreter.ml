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

(* FIXME, proper trace output for --multiple option *)
(* FIXME, in --analyze-memo stop parsing with given settings when it takes longer than the best result so far (timeout) *)

module T = Tgrammar
module P = Tgrammar.PreGrammar
module List = BaseList
module Char = BaseChar

let pr = Printf.sprintf

(* TODO: change this printf *)
let log fmt =
  Printf.eprintf (fmt^^"\n")

let grammar_to_string g =
  pr "Start: {%s}\nProductions:\n%s\n" g.T.start
    (T.def_map_to_string (StringMap.map fst g.T.grammar))

let errorHandling = ref true
let grammarFile = ref None
let inputFile = ref None
let startProd = ref None
let cmd = ref `parsing
let mode = ref `parseOne
let memo_default = ref None

let get_val_or_fail v err =
  match !v with
  | Some s -> s
  | _ -> failwith err

let get_grammar_fn () = get_val_or_fail grammarFile "Unspecified grammar!"

let get_input_fn () = get_val_or_fail inputFile "Unspecified input file!"

let measureTime f =
  let startT = Sys.time () in
  let res = f () in
  let endT = Sys.time () in
  res, endT -. startT

(***************************************************************)
(************************** Error handling *********************)
(***************************************************************)

open Trx_runtime

(***************************************************************)
(************************** Parsing trace **********************)
(***************************************************************)

type traceParseResult =
  | Failure
  | Success of int (* number of consumed characters *)

type traceType =
  | CallBeg of pos * (traceParseResult * Trx_runtime.parseError) ref * bool ref (* result of memoization? *)
  | CallEnd

type tagInfo =
    { def : string
    ; level : int
    }

type traceEntry = tagInfo * traceType

let traceFile = ref None

let trace = MutableList.create ()

let tracingEnabled () =
  match !traceFile with
  | Some _ -> true
  | None -> false

let lev = ref 1

let enableTracing s =
  traceFile := Some s

let rec indent = function
  | 0 -> ""
  | n -> " " ^ indent (n-1)

let write_trace input fn =
  let max_indent = ref 0 in
  let write_tag (ti, tt) =
    match tt with
    | CallBeg (pos, tr, memo) ->
        let pos_str =
          let line, col = FilePos.get_pos_no_cache input pos in
          pr "%d/%d" line col
        in
        let memo_str = if !memo then " memoized=\"true\"" else "" in
        let res_str, len_str =
          match fst !tr with
          | Failure -> "fail", ""
          | Success i -> "ok", pr " consumed=\"%d\"" i
        in
(*	let err_str = Trx_runtime.show_parse_error input (snd !tr) in*)
        max_indent := max !max_indent ti.level;
        Printf.fprintf fn "%s<%s pos=\"%s\" result=\"%s\" %s%s >\n" (indent ti.level) ti.def pos_str res_str len_str memo_str;
(*	Printf.fprintf fn "%s<![CDATA[%s]]>\n" (indent ti.level) err_str*)
    | CallEnd ->
        Printf.fprintf fn "%s</%s>\n" (indent ti.level) ti.def
  in
    Printf.fprintf fn "<trx_interpreter grammar=\"%s\" input=\"%s\">\n"
      (get_grammar_fn ()) (get_input_fn ());
    MutableList.iter write_tag trace;
    Printf.fprintf fn "</trx_interpreter>\n";
    Printf.eprintf "Max nesting: %d\n" !max_indent

(***************************************************************)
(************************* TRX interpreter *********************)
(***************************************************************)

let option_to_res_no_err res pos =
  match res with
  | Some res -> Ok (res, emptyError pos)
  | None -> Fail (emptyError pos)

let addErrorInfo err res =
  if !errorHandling then
    Trx_runtime.addErrorInfo err res
  else
    res

let parse peg input =

  let _len = String.length input in
  let _get_char = String.unsafe_get input in
  let memo = Hashtbl.create 128 in

  let process_literal literal case pos =
    let literal_len = String.length literal in
    let eq = if case then (=) else Char.equal_insensitive in
    let rec aux i =
      if i = literal_len then
        true
      else
        eq (_get_char (pos + i)) (String.unsafe_get literal i) && aux (i + 1)
    in
    let res =
      if pos + literal_len <= _len && aux 0 then
        Some (pos + literal_len, ())
      else
        None
    in
    if !errorHandling then
      option_to_res_msg res pos ("\"" ^ literal ^ "\"")
    else
      option_to_res_no_err res pos
  in

  let process_range ranges pos =
    let encode = function
      | T.Any -> `Any
      | T.One c -> `One c
      | T.Range (c1, c2) -> `Range (c1, c2)
    in
    let res =
      if pos < _len then begin
        let c = _get_char pos in
        let rec aux = function
          | [] -> false
          | T.Any::_ -> true
          | T.One c'::cs -> c = c' || aux cs
          | T.Range (c1, c2)::cs -> (c >= c1 && c <= c2) || aux cs
        in
        if aux ranges then
          Some (pos + 1, ())
        else
          None
      end else
        None
    in
    if !errorHandling then
      let err = range_to_error (List.map encode ranges) in
      option_to_res_err res pos err
    else
      option_to_res_no_err res pos
  in

  let get_def id =
    try
      StringMap.find id peg.T.grammar
    with
        Not_found ->
          log "Could not find a definition for the production: %s" id;
          raise Not_found
  in

  let rec parse_definition id pos =
    let d, def_annots = get_def id in
    let do_parse () =
      let res, memoized =
        try
          Hashtbl.find memo (id, pos), true
        with
          Not_found ->
            parse_expression pos d.P.expression, false
      in
      let res =
        match def_annots.T.rule_name with
        | T.NoName -> res (* decorateConstruct res pos [Expected id] *) (* uncommenting turns on use of rule ids (not only names) for error reporting *)
        | T.PrimaryName def_name -> setMainConstruct res pos (Expected def_name)
        | T.SecondaryName def_name -> decorateConstruct res pos [Expected def_name]
      in
      let success =
        match res with
        | Ok _ -> true
        | Fail _ -> false
      in
      let do_memo =
        match def_annots.T.rule_memo with
        | T.MemoNone -> false
        | T.MemoFail -> not success
        | T.MemoSuccess -> success
        | T.MemoFull -> true
        | T.MemoNoInfo -> assert false
      in
      if do_memo then
        Hashtbl.add memo (id, pos) res;
      res, memoized
    in
      if not (tracingEnabled ()) then
        fst (do_parse ())
      else
        begin
          let tagInfo =
            { def = id
            ; level = !lev
            }
          in
          let tr = ref (Failure, Trx_runtime.emptyError 0) in
          let memo_mark = ref false in
          lev := !lev + 1;
          MutableList.add trace (tagInfo, CallBeg (pos, tr, memo_mark));
          let res, memoized = do_parse () in
          if memoized then
            memo_mark := true;
          tr :=
            begin match res with
            | Fail e -> Failure, e
            | Ok ((pos', _), e) -> Success (pos' - pos), e
            end;
          lev := !lev - 1;
          MutableList.add trace (tagInfo, CallEnd);
          res
        end

  and parse_expression pos = function
  | P.App _ -> failwith "Unexpected App"
  | P.Expr [] -> Fail (emptyError pos)
  | P.Expr (s::ss) ->
      let (items, _, prod) = s in
      match prod with
      | Some (_, _, _, true) ->
(*        Printf.eprintf "WARNING! Skipping sequence with production {| ... |}\n";*)
          Fail (emptyError pos)
      | _ ->
          match parse_sequence pos items with
          | Fail e ->
              let res = parse_expression pos (P.Expr ss) in
              addErrorInfo e res
          | res -> res

  and parse_sequence pos = function
  | [] -> Ok ((pos, ()), emptyError pos)
  | i::is ->
      match parse_item pos i with
      | Fail e -> Fail e
      | Ok ((pos', _), e) ->
          let res = parse_sequence pos' is in
          addErrorInfo e res

  and parse_item pos (prefix, prim, suffix) =
    let rec loop pos required =
      match parse_primary pos prim with
      | Ok ((pos', _), e) ->
          let res = loop pos' false in
          addErrorInfo e res
      | Fail e ->
          if required then
            Fail e
          else
            Ok ((pos, ()), e)
    in
    let suffixr =
      match suffix with
      | `NORMAL -> parse_primary pos prim
      | `STAR -> loop pos false
      | `PLUS -> loop pos true
      | `QUESTION ->
          match parse_primary pos prim with
          | Fail e -> Ok ((pos, ()), e)
          | res -> res
    in
      (* FIXME, Handle errors *)
      match prefix, suffixr with
      | `NORMAL, res -> res
      | `AND, Fail e -> Fail e
      | `AND, Ok (_, e) -> Ok ((pos, ()), e)
      | `NOT, Fail e -> Ok ((pos, ()), e)
      | `NOT, Ok (_, e) -> Fail e

  and parse_primary pos = function
  | P.Paren e ->
      parse_expression pos e
  | P.Literal (l, case) ->
      process_literal l case pos
  | P.Class ranges ->
      process_range ranges pos
  | P.Ident id ->
      parse_definition id pos
  in

   (* FIXME, at the moment if -start switch is not provided then all productions
    * marked with '+' are tried.
    *)
  let rec try_parse print_success = function
  | [] -> None
  | s::ss ->
      (*jlog ~level:2 (pr "Trying to parse with the start production: %s" s);*)
      match parse_definition s 0 with
      | Fail e ->
          let err_str = show_parse_error (FilePos.get_pos_no_cache input) e in
          log "Production %s gives syntax error: %s" s err_str;
          try_parse print_success ss
      | Ok ((pos, _), _e) ->
          if print_success then
            log "Success with: %s" s;
          Some pos
  in
    (*jlog ~level:3 (pr "Parsing with the following grammar:\n%s\n======\n" (grammar_to_string peg));*)
    match !startProd with
    | None -> try_parse true (Pgrammar.start_definitions peg.T.grammar)
    | Some s -> try_parse false [s]

(**************************************************************)
(************************* Main front-end *********************)
(**************************************************************)

let str2memo = function
  | "none" -> T.MemoNone
  | "fail" -> T.MemoFail
  | "success" -> T.MemoSuccess
  | "full" -> T.MemoFull
  | s -> failwith (pr "Unknown memoization option: '%s' (should be: 'none', 'fail', 'success' or 'full')" s)

let usage_msg = Printf.sprintf "%s: parser interpreter for the Opa project\nUsage: %s [options] syntax_file.[trx|prx]\n" Sys.argv.(0) Sys.argv.(0)

let parse_args () =
  let anon_fun s =
    if !grammarFile = None then
      grammarFile := Some s
    else if !inputFile = None then
      inputFile := Some s
    else
      failwith (pr "Don't know what to do with <%s> argument" s)
  in
  Arg.parse (Arg.align [
    ("--no-error-handling",
    Arg.Unit (fun () -> errorHandling := false),
    " turns off error handling (faster, but no error msgs)");

    ("--multiple",
    Arg.Unit (fun () -> mode := `parseMany),
    " parses a number of files; the input file contains a list of files to parse, one per line");

    ("--trace",
    Arg.String (fun s -> enableTracing s),
    " file.[xml] produces an XML trace of parsing in <file>");

    ("--start",
    Arg.String (fun s -> startProd := Some s),
    " prod starting production to be used (if not provided all marked with + will be tried)");

    ("--verbose",
    Arg.Unit (fun () -> ()),
    " deprecated");

    ("--more-verbose",
    Arg.Unit (fun () -> ()),
    " deprecated");

    ("--analyze-memo",
    Arg.Unit (fun () -> cmd := `analyze_memo),
    " instead of parsing peform memoization analysis with given input");

    ("--memoization",
    Arg.String (fun s -> memo_default := Some (str2memo s)),
    " default memoization level for rules with no annotations [none | fail | success | full]");

  ]) anon_fun
    (usage_msg^"Options:")

let load_grammar grammarFn =
  let peg, _ = Pgrammar.read_grammar ?memo_default:!memo_default ~verbose:true None grammarFn in
  peg

let parse_file peg inputFn =
  log  "Parsing <%s>..." inputFn;
  let input = File.content inputFn in
  let all = String.length input in
  let go () = parse peg input in
  let res, t = measureTime go in
  begin match res with
  | Some pos ->
      log "Parsing successful [%d/%d] in %4.2fsec." pos all t
  | None ->
      log "Parsing failed"
  end;
  begin match !traceFile with
  | None -> ()
  | Some fn ->
      write_trace input (open_out fn);
      log "Parsing trace written to <%s>" fn
  end

let parse_files peg inputFn =
  let inc = open_in inputFn in
  log "Parsing all files from <%s>" inputFn;
  let n = ref 0 in
  let go () =
    try
      while true do
        let fn = input_line inc in
        parse_file peg fn;
        incr n
      done
    with
      End_of_file ->
        close_in inc
  in
  let _, t = measureTime go in
  log "Total time of parsing %d files from <%s>: %4.2fsec." !n inputFn t

let parsing peg =
  let inputFn = get_input_fn () in
    match !mode with
    | `parseOne ->
        parse_file peg inputFn
    | `parseMany ->
        parse_files peg inputFn

let analyze_memo peg =
  let i = ref 0 in
  let progress () =
    let rules = StringMap.size peg.T.grammar in
    let total = float_of_int (rules * 3) in
    incr i;
    Printf.eprintf "\rProgress: %3.2f%%...%!" (float_of_int (100 * !i) /. total)
  in
  let parseWithPeg peg =
    let res = measureTime (fun () -> parsing peg) in
    res
  in
  let memo2str = function
    | T.MemoNone -> "none"
    | T.MemoFail -> "fail"
    | T.MemoSuccess -> "success"
    | T.MemoFull -> "full"
    | T.MemoNoInfo -> "noInfo"
  in
  let apply_move move peg =
    let def_name, memo_opt = move in
    let (def, annots) = StringMap.find def_name peg.T.grammar in
    let new_annots = { annots with T.rule_memo = memo_opt } in
    let new_entry = def, new_annots in
    { peg with T.grammar = StringMap.add def_name new_entry peg.T.grammar }
  in
  let try_to_improve_on peg def_name (best_move, best) memo_opt =
    let move = def_name, memo_opt in
    let peg' = apply_move move peg in
    let _, t = parseWithPeg peg' in
    progress ();
    (*if false then
      jlog ~level:2 (pr "Trying to change memo option for def. <%s> to <%s> gives time: %4.3fsec." def_name (memo2str memo_opt) t);*)
    if t < best then
      (Some move, t)
    else
      (best_move, best)
  in
  let try_to_improve peg def_name (_, def_annots) (best_move, best) =
    let opts = [T.MemoNone; T.MemoFail; T.MemoSuccess; T.MemoFull] in
    let opts = List.remove_first def_annots.T.rule_memo opts in
    List.fold_left (try_to_improve_on peg def_name) (best_move, best) opts
  in
  let rec improve (best, peg) =
    i := 0;
    let res, best' = StringMap.fold (try_to_improve peg) peg.T.grammar (None, best) in
    match res with
    | None ->
        log "\nNo improvement..."
    | Some move ->
        let impr = best -. best' in
        log "\nConsider changing memoization option for definition <%s> to <%s>.\nIt resulted in time %4.3fsec. (%4.3fsec./%1.3f%% improvement)"
          (fst move) (memo2str (snd move)) best' impr (100.0 *. impr /. best);
        improve (best', apply_move move peg)
  in
  let _, best = parseWithPeg peg in
  log "Initial grammar gives timing: %4.3fsec. [no. of rules: %d]" best (StringMap.size peg.T.grammar);
  improve (best, peg)

let _ =
    parse_args ();
  let grammarFn = get_grammar_fn () in
  let inputFn = get_input_fn () in
    log "Loading grammar from {%s} and then parsing {%s}\n" grammarFn inputFn;
  try
    let peg = load_grammar grammarFn in
    match !cmd with
    | `parsing -> parsing peg
    | `analyze_memo -> analyze_memo peg
  with
  | Pgrammar.GrammarParse _err ->
      log "Failed while parsing the input grammar: {%s}!\n" grammarFn
