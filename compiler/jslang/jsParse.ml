(*
    Copyright © 2011, 2012 MLstate

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
module J = JsAst
module List = BaseList
open JsLex (* bringing token in the scope *)

(* right now, the parser doesn't insert any positions in the ast *)
let dummy_pos = FilePos.nopos "jsParse"
let nl = Annot.next_label
let merge_pos = FilePos.merge_pos_for_parser
let label () = Annot.next_label dummy_pos
let native_ident = JsCons.Ident.native

(* Redefining the module Stream allows us to 'override' the syntax of
 * streams. The new peek, junk and empty functions look at the first
 * non-newline token (which allows us to write almost the whole parser
 * while implicitely discarding newlines).
 *
 * The new module also carries an imperative state to discard comments
 * implicitely. This has to be set carefully by the various rules. *)

module OriginalStream = Stream

module Stream =
struct

  type 'a t = bool ref * 'a Stream.t
  exception Failure = Stream.Failure
  exception Error = Stream.Error
  let from f = (ref true, Stream.from f)
  let junk_no_newline stream = Stream.junk (snd stream)
  let peek_no_newline stream = Stream.peek (snd stream)

  let wrap stream = (ref true, stream)
  let parse_comments stream =
    fst stream := false

  let ignore_comments stream =
    fst stream := true

  let junk stream =
    (* this function is symmetric with peek below *)
    (match Stream.peek (snd stream) with
    | Some (LT _) -> Stream.junk (snd stream)
    | _ -> ());
    Stream.junk (snd stream)

  (*let peek stream =
    match Stream.npeek 2 stream with
    | [LT; a] -> Some a
    | [LT] -> None
    | a :: _ -> Some a
    | [] -> None*)
  let rec peek stream = (* this Stream.peek makes the parsing really faster *)
    match Stream.peek (snd stream) with
    | Some (LT _) ->
      (* using the invariant that says that you never have two consecutives
       * newlines in the token stream *)
      (match Stream.npeek 2 (snd stream) with
      | _ :: t :: _ -> Some t
      | _ -> None)
    | Some (DocComment _) when !(fst stream) ->
      peek stream
    | o -> o

  (* redefining empty because a stream with only a newline must be considered
   * as empty *)
  let empty s =
    match peek s with
    | None -> ()
    | Some _ -> raise Stream.Failure
end

type tok_stream = JsLex.token Stream.t
type pos = FilePos.pos
type statement = J.statement
type expr = J.expr

(* a handful of parser combinators *)
let rev_list0_aux acc parser_ stream =
  let rec aux acc = parser
    | [< e = parser_; stream >] -> aux (e :: acc) stream
    | [< >] -> acc in
  aux acc stream
let rev_list0 parser_ stream =
  rev_list0_aux [] parser_ stream
let list0 parser_ stream =
  List.rev (rev_list0 parser_ stream)
let rev_list1 parser_ = parser
  | [< v = parser_; r = rev_list0_aux [v] parser_ >] -> r
let list1 parser_ = parser
  | [< v = parser_; l = list0 parser_ >] -> v :: l
let rev_list1_sep parser_ sep stream =
  let rec aux acc = parser
    | [< _op = sep; e = parser_; stream >] -> aux (e :: acc) stream
    | [< >] -> acc in
  match stream with parser
  | [< e = parser_; stream >] -> aux [e] stream

let list1_sep_left_assoc parser_ sep stream =
  let rec aux pos acc = parser
    | [< op = sep; e = parser_; stream >] ->
      let pos' = J.JPos.expr e in
      let pos = merge_pos pos pos' in
      aux pos (J.Je_binop (nl pos,op,acc,e)) stream
    | [< >] -> acc in
  match stream with parser
  | [< e = parser_; stream >] ->
    let pos = J.JPos.expr e in
    aux pos e stream
let list1_sep_right_assoc parser_ sep stream =
  let rec aux pos acc = parser
    | [< op = sep; e = parser_; stream >] ->
      let pos' = J.JPos.expr e in
      let pos = merge_pos pos pos' in
      aux pos ((pos, op, e) :: acc) stream
    | [< >] -> acc in
  match stream with parser
  | [< r = parser_; stream >] ->
    let pos = J.JPos.expr r in
    match aux pos [] stream with
    | [] -> r
    | (pos, op, e) :: t ->
      let pos, op, e =
        List.fold_left (fun (pos1, op1, e1) (pos2, op2, e2) ->
          let pos = merge_pos pos1 pos2 in
          (pos, op2, J.Je_binop (nl pos,op1,e2,e1))
        ) (pos,op,e) t in
      J.Je_binop(nl pos, op, r, e)

let rev_list0_sep parser_ sep stream =
  let rec aux acc = parser
    | [< _ = sep; v = parser_; stream >] -> aux (v :: acc) stream
    | [< >] -> acc in
  match stream with parser
  | [< r = parser_; stream >] -> aux [r] stream
  | [< >] -> []
let list1_sep parser_ sep stream = List.rev (rev_list1_sep parser_ sep stream)
let list0_sep parser_ sep stream = List.rev (rev_list0_sep parser_ sep stream)
let option parser_ = parser
  | [< r = parser_ >] -> Some r
  | [< >] -> None
let option_default default parser_ = parser
  | [< r = parser_ >] -> r
  | [< >] -> default

(* tries to parse using [parser_] but only when there is no newline
 * in the input stream
 * for cases such as [return
 *                   2]
 * which is parsed as [return; 2] and not [return 2] *)
let option_no_newline parser_ stream =
  match Stream.peek_no_newline stream with
  | Some (LT _)-> None
  | _ -> option parser_ stream

let semic stream =
  match Stream.peek_no_newline stream with
  | None -> None
  | Some (Semic pos | LT pos) ->
    Stream.junk_no_newline stream; Some pos
  | Some (Rcurly pos) -> Some pos (* do not discard the bracket! *)
  | _ -> raise Stream.Failure
let ident = parser
  | [< 'Ident (pos, i) >] -> (pos, i)
let native = parser
  | [< 'Ident (pos, i) >] -> (pos, native_ident i)
let comma = parser
  | [< 'Comma _ >] -> ()
let barbar = parser
  | [< 'BarBar _ >] -> J.Jb_lor
let amperamper = parser
  | [< 'AmperAmper _ >] -> J.Jb_land
let bar = parser
  | [< 'Bar _ >] -> J.Jb_or
let chapeau = parser
  | [< 'Chapeau _ >] -> J.Jb_xor
let amper = parser
  | [< 'Amper _ >] -> J.Jb_and
let assignmentoperator = parser
  | [< 'TimesEqual _ >] -> J.Jb_mul_assign
  | [< 'PlusEqual _ >] -> J.Jb_add_assign
  | [< 'PercentEqual _ >] -> J.Jb_mod_assign
  | [< 'MinusEqual _ >] -> J.Jb_sub_assign
  | [< 'LtLtEqual _ >] -> J.Jb_lsl_assign
  | [< 'GtGtGtEqual _ >] -> J.Jb_asr_assign
  | [< 'GtGtEqual _ >] -> J.Jb_lsr_assign
  | [< 'Equal _ >] -> J.Jb_assign
  | [< 'DivEqual _ >] -> J.Jb_div_assign
  | [< 'ChapeauEqual _ >] -> J.Jb_xor_assign
  | [< 'BarEqual _ >] -> J.Jb_or_assign
  | [< 'AmperEqual _ >] -> J.Jb_and_assign
let equalityoperator = parser
  | [< 'EqualEqual _ >] -> J.Jb_eq
  | [< 'EqualEqualEqual _ >] -> J.Jb_seq
  | [< 'BangEqual _ >] -> J.Jb_neq
  | [< 'BangEqualEqual _ >] -> J.Jb_sneq
let relationaloperator = parser
  | [< 'Lt _ >] -> J.Jb_lt
  | [< 'Gt _ >] -> J.Jb_gt
  | [< 'Le _ >] -> J.Jb_leq
  | [< 'Ge _ >] -> J.Jb_geq
  | [< 'Instanceof _ >] -> J.Jb_instanceof
  | [< 'In _ >] -> J.Jb_in
let shiftoperator = parser
  | [< 'LtLt _ >] -> J.Jb_lsl
  | [< 'GtGt _ >] -> J.Jb_lsr
  | [< 'GtGtGt _ >] -> J.Jb_asr
let additiveoperator = parser
  | [< 'Plus _ >] -> J.Jb_add
  | [< 'Minus _ >] -> J.Jb_sub
let multiplicativeoperator = parser
  | [< 'Times _ >] -> J.Jb_mul
  | [< 'Div _ >] -> J.Jb_div
  | [< 'Percent _ >] -> J.Jb_mod
let unaryoperator = parser
  | [< 'PlusPlus pos >] -> (pos, J.Ju_add2_pre)
  | [< 'Delete pos >] -> (pos, J.Ju_delete)
  | [< 'Typeof pos >] -> (pos, J.Ju_typeof)
  | [< 'Void pos >] -> (pos, J.Ju_void)
  | [< 'MinusMinus pos >] -> (pos, J.Ju_sub2_pre)
  | [< 'Plus pos >] -> (pos, J.Ju_add_pre)
  | [< 'Minus pos >] -> (pos, J.Ju_sub_pre)
  | [< 'Tilda pos >] -> (pos, J.Ju_tilde)
  | [< 'Bang pos >] -> (pos, J.Ju_not)
let postfixoperator = parser
  | [< 'PlusPlus pos >] -> (pos, J.Ju_add2_post)
  | [< 'MinusMinus pos >] -> (pos, J.Ju_sub2_post)

let rec statement stream =
  Stream.parse_comments stream;
  match stream with parser
  | [< 'DocComment (pos, lines) >] ->
    J.Js_comment (nl pos, J.Jc_doc (nl pos, lines))
  | [< >] ->
    Stream.ignore_comments stream;
    statement_no_comments stream

and statement_no_comments = parser
  | [< 'Function pos1;
       'Ident (_, name) ?? "expected an identifier after 'function' in a statement";
       'Lparen _; params = list0_sep native comma; 'Rparen _;
       'Lcurly _; body = statements; 'Rcurly pos2 >] ->
    let pos = merge_pos pos1 pos2 in
    let params = List.map snd params in
    J.Js_function (nl pos, native_ident name, params, body)
  | [< 'Lcurly pos1; block = statements;
       'Rcurly pos2 ?? "expected a closing curly brace" >] ->
    let pos = merge_pos pos1 pos2 in
    J.Js_block (nl pos, block)
  | [< 'Semic _; stream >] -> statement stream
  | [< 'Var pos1; l = list1_sep vardeclaration comma; pos2 = semic >] -> (
    let pos2 = match pos2 with
      | Some pos2 -> pos2
      | None ->
        let (pos2, _, _) = List.last l in pos2
    in
    let pos = merge_pos pos1 pos2 in
    match l with
    | [(_,i,o)] -> J.Js_var (nl pos, i, o)
    | _ ->
      let pos = merge_pos pos1 pos2 in
      let decls = List.map (fun (p,i,o) -> J.Js_var (nl p, i, o)) l in
      J.Js_block (nl pos, decls)
  )
  | [< 'If pos1; 'Lparen _; e = expr; 'Rparen _; s1 = statement; stream >] ->
    let o, pos =
      match stream with parser
      | [< 'Else _; s2 = statement >] ->
        Some s2, merge_pos pos1 (J.JPos.stm s2)
      | [< >] -> None, merge_pos pos1 (J.JPos.stm s1) in
    J.Js_if (nl pos,e,s1,o)
  | [< 'Do pos1; s = statement; 'While _; 'Lparen _; e = expr; 'Rparen pos21; pos22 = semic >] ->
    let pos2 =
      match pos22 with
      | Some pos2 -> pos2
      | None -> pos21 in
    let pos = merge_pos pos1 pos2 in
    J.Js_dowhile (nl pos, s, e)
  | [< 'While pos1; 'Lparen _; e = expr; 'Rparen _; s = statement >] ->
    let pos = merge_pos pos1 (J.JPos.stm s) in
    J.Js_while (nl pos, e, s)
  | [< 'For pos1; 'Lparen _; stream >] -> (
      match stream with parser
      | [< 'Var pos1'; (pos2',i,o) = vardeclaration; stream >] -> (
        let pos_var = merge_pos pos1' pos2' in
        match o with
        | Some (J.Je_binop (_,J.Jb_in,e1,e2)) -> (
          match stream with parser
          | [< 'Rparen _; s = statement >] ->
            let s1 = J.Js_var (nl pos_var, i, Some e1) in
            let pos_for = merge_pos pos1 (J.JPos.stm s) in
            let s2 =
              J.Js_forin (nl pos_for, J.Je_ident (nl pos_var,i), e2, s) in
            J.Js_block (nl pos_for, [s1; s2])
        )
        | _ ->
          match stream with parser
          | [< 'In _; e2 = expr; 'Rparen _; s = statement >] ->
            let s1 = J.Js_var (nl pos_var, i, o) in
            let pos_for = merge_pos pos1 (J.JPos.stm s) in
            let s2 =
              J.Js_forin (nl pos_for, J.Je_ident (nl pos_var, i), e2, s) in
            J.Js_block (nl pos_for, [s1; s2])
          | [< 'Comma _; l = list1_sep vardeclaration comma; 'Semic _;
               e2 = option expr; 'Semic _; e3 = option expr; 'Rparen _;
               s = statement >] ->
            let s1 = J.Js_var (nl pos2', i, o) in
            let s1_more = List.map (fun (p,i,o) ->
              J.Js_var (nl p,i,o)
            ) l in
            let pos_for = merge_pos pos1 (J.JPos.stm s) in
            let s2 = J.Js_for (nl pos_for, None, e2, e3, s) in
            J.Js_block (nl pos_for, s1 :: s1_more @ [s2])
          | [< 'Semic _; e2 = option expr; 'Semic _; e3 = option expr;
               'Rparen _; s = statement >] ->
            let s1 = J.Js_var (nl pos_var, i, o) in
            let pos_for = merge_pos pos1 (J.JPos.stm s) in
            let s2 = J.Js_for (nl pos_for, None, e2, e3, s) in
            J.Js_block (nl pos_for, [s1;s2])
      )
      | [< o1 = option expr; stream >] -> (
        match o1 with
        | Some J.Je_binop (_,J.Jb_in,e1,e2) -> (
          match stream with parser
          | [< 'Rparen _; s = statement >] ->
            let pos_for = merge_pos pos1 (J.JPos.stm s) in
            J.Js_forin (nl pos_for, e1, e2, s)
        )
        | _ ->
          match stream with parser
          | [< _ = semic; e2 = option expr; _ = semic; e3 = option expr;
               'Rparen _; s = statement >] ->
            let pos_for = merge_pos pos1 (J.JPos.stm s) in
            J.Js_for (nl pos_for, o1, e2, e3, s)
      )
  )
  | [< 'Continue pos1; o = option_no_newline ident; pos2 = semic >] ->
    let pos2 =
      match pos2 with
      | Some pos2 -> pos2
      | None ->
        match o with
        | Some (pos2, _) -> pos2
        | None -> pos1 in
    let pos = merge_pos pos1 pos2 in
    J.Js_continue (nl pos, Option.map snd o)
  | [< 'Break pos1; o = option_no_newline ident; pos2 = semic >] ->
    let pos2 =
      match pos2 with
      | Some pos2 -> pos2
      | None ->
        match o with
        | Some (pos2, _) -> pos2
        | None -> pos1 in
    let pos = merge_pos pos1 pos2 in
    J.Js_break (nl pos, Option.map snd o)
  | [< 'Return pos1; o = option_no_newline expr; pos2 = semic >] ->
    let pos2 =
      match pos2 with
      | Some pos2 -> pos2
      | None ->
        match o with
        | Some expr -> J.JPos.expr expr
        | None -> pos1 in
    let pos = merge_pos pos1 pos2 in
    J.Js_return (nl pos, o)
  | [< 'With pos1; 'Lparen _; e = expr; 'Rparen _; s = statement >] ->
    let pos = merge_pos pos1 (J.JPos.stm s) in
    J.Js_with (nl pos,e,s)
  | [< 'Switch pos1; 'Lparen _; e = expr; 'Rparen _;
       'Lcurly _; clauses = list0 caseclause; default = option defaultclause;
       'Rcurly pos2 >] ->
    let pos = merge_pos pos1 pos2 in
    J.Js_switch (nl pos,e,clauses,default)
  | [< 'Throw pos1; e = expr; pos2 = semic >] ->
    let pos2 =
      match pos2 with
      | Some pos2 -> pos2
      | None -> J.JPos.expr e in
    let pos = merge_pos pos1 pos2 in
    J.Js_throw (nl pos,e)
  (* the specification seems crazy, where is the problem with a newline here? *)
  | [< 'Debugger _ >] -> (*SDebugger*) failwith "No ast node for \"debugger\""
  | [< 'Try pos1; b = block_stm; stream >] -> (
    let pos2 = J.JPos.stm b in
    let pos = merge_pos pos1 pos2 in
    match stream with parser
    | [< (i,s) = catch_block; o = option finally >] ->
      J.Js_trycatch (nl pos, b, [(i,None,s)], o)
    | [< c = finally >] ->
      J.Js_trycatch (nl pos, b, [], Some c)
  )
  | [< e = expr; stream >] ->
    match stream with parser
    | [< 'Colon _; s = statement >] -> (
      match e with
      | J.Je_ident (label,i) -> (
        match i with
        | J.Native (_, i) -> J.Js_label (label,i,s)
        | _ -> assert false
      )
      | _ -> raise (Stream.Error "Invalid label")
    )
    | [< _ = semic >] -> J.Js_expr (nl (J.JPos.expr e), e)

and block = parser
  | [< 'Lcurly pos1; l = statements; 'Rcurly pos2 >] ->
    (merge_pos pos1 pos2, l)
and block_stm stream =
  let (pos, b) = block stream in
  J.Js_block (nl pos, b)

and vardeclaration = parser
  | [< 'Ident (pos1, i); stream >] ->
    match stream with parser
    | [< 'Equal _; e = assignmentexpr >] ->
      let pos2 = J.JPos.expr e in
      let pos = FilePos.merge_pos_for_parser pos1 pos2 in
      (pos, native_ident i, Some e)
    | [< >] -> (pos1, native_ident i, None)

and caseclause = parser
  | [< 'Case _; e = expr; 'Colon _; l = statements_stm >] -> (e, l)
and defaultclause = parser
  | [< 'Default _; 'Colon _; l = statements_stm >] -> l

and catch_block = parser
  | [< 'Catch _; 'Lparen _; 'Ident (_, i); 'Rparen _; b = block_stm >] -> (native_ident i,b)
and finally = parser
  | [< 'Finally _; b = block_stm >] -> b

and expr stream : expr =
  match rev_list1_sep assignmentexpr comma stream with
  | [] -> assert false
  | [e] -> e
  | e :: l ->
    let pos1 = J.JPos.expr (List.last l) in
    let pos = merge_pos pos1 (J.JPos.expr e) in
    J.Je_comma (nl pos, List.rev l, e)
and assignmentexpr stream : expr =
  list1_sep_right_assoc conditionalexpr assignmentoperator stream
and conditionalexpr : tok_stream -> expr = parser
  | [< e = logicalorexpr; stream >] ->
    match stream with parser
    | [< 'Question _; e2 = assignmentexpr; 'Colon _; e3 = conditionalexpr >] ->
      let pos1 = J.JPos.expr e in
      let pos2 = J.JPos.expr e3 in
      let pos = merge_pos pos1 pos2 in
      J.Je_cond (nl pos, e, e2, e3)
    | [< >] -> e
and logicalorexpr stream : expr =
  list1_sep_left_assoc logicalandexpr barbar stream
and logicalandexpr stream : expr =
  list1_sep_left_assoc bitwiseorexpr amperamper stream
and bitwiseorexpr stream : expr =
  list1_sep_left_assoc bitwisexorexpr bar stream
and bitwisexorexpr stream : expr =
  list1_sep_left_assoc bitwiseandexpr chapeau stream
and bitwiseandexpr stream : expr =
  list1_sep_left_assoc equalityexpr amper stream
and equalityexpr stream : expr =
  list1_sep_left_assoc relationalexpr equalityoperator stream
and relationalexpr stream : expr =
  list1_sep_left_assoc shiftexpr relationaloperator stream
and shiftexpr stream : expr =
  list1_sep_left_assoc additiveexpr shiftoperator stream
and additiveexpr stream : expr =
  list1_sep_left_assoc multiplicativeexpr additiveoperator stream
and multiplicativeexpr stream : expr =
  list1_sep_left_assoc unaryexpr multiplicativeoperator stream
and unaryexpr : tok_stream -> expr = parser
  | [< l = rev_list1 unaryoperator;
       e = postfixexpr ?? "expected an expression after a postfix operator" >] ->
    List.fold_left (fun e (pos2, op) ->
      let pos1 = J.JPos.expr e in
      let pos = merge_pos pos1 pos2 in
      J.Je_unop (nl pos,op,e)
    ) e l
  | [< e = postfixexpr >] -> e
and postfixexpr : tok_stream -> expr = parser
  | [< e = lefthandsideexpr false; o = option_no_newline postfixoperator >] ->
    match o with
    | None -> e
    | Some (pos1, op) ->
      let pos2 = J.JPos.expr e in
      let pos = merge_pos pos1 pos2 in
      J.Je_unop (nl pos, op, e)
and lefthandsideexpr new_ : tok_stream -> expr = parser
  | [< 'New pos1; e = lefthandsideexpr true; stream >] -> (
    let pos2 = J.JPos.expr e in
    match stream with parser
    | [< (pos2, el) = option_default (pos2, []) arguments; stream >] ->
      let pos = merge_pos pos1 pos2 in
      let e = J.Je_new (nl pos, e, el) in
      dot_hashref_call true e stream
  )
  | [< 'Function pos1; name = option native; 'Lparen _;
       params = list0_sep native comma; 'Rparen _; 'Lcurly _;
       body = statements; 'Rcurly pos2; stream >] ->
    let name = Option.map snd name in
    let params = List.map snd params in
    let pos = merge_pos pos1 pos2 in
    (* put the this rule into primaryexpr instead? *)
    let e = J.Je_function (nl pos, name, params, body) in
    dot_hashref_call (not new_) e stream
  | [< e = primaryexpr; r = dot_hashref_call (not new_) e >] -> r
and dot_hashref_call can_call (e : expr) stream : expr =
  let pos1 = J.JPos.expr e in
  match stream with parser
  | [< 'Dot _; 'Ident (pos2, i); stream >] ->
    let pos = merge_pos pos1 pos2 in
    dot_hashref_call can_call (J.Je_dot (nl pos, e, i)) stream
  | [< 'Lbracket _; i = expr; 'Rbracket pos2; stream >] ->
    let pos = merge_pos pos1 pos2 in
    dot_hashref_call can_call (J.Je_binop (nl pos,J.Jb_hashref,e,i)) stream
  | [< 'Lparen _ when can_call; l = list0_sep assignmentexpr comma; 'Rparen pos2; stream >] ->
    (* refusing to parse arguments when under a new because in [new f()], the arguments are given to new_
     * not to f *)
    let pos = merge_pos pos1 pos2 in
    dot_hashref_call can_call (J.Je_call (nl pos,e,l,false)) stream
  | [< >] -> e
and arguments : tok_stream -> pos * expr list = parser
  | [< 'Lparen pos1; l = list0_sep assignmentexpr comma; 'Rparen pos2 >] ->
    (merge_pos pos1 pos2, l)
and primaryexpr : tok_stream -> expr = parser
  | [< 'Null pos >] -> J.Je_null (nl pos)
  | [< 'This pos >] -> J.Je_this (nl pos)
  | [< 'Ident (pos, i) >] -> J.Je_ident (nl pos, native_ident i)
  | [< 'Integer (pos, i) >] -> J.Je_num (nl pos, i)
  | [< 'True pos >] -> J.Je_bool (nl pos, true)
  | [< 'False pos >] -> J.Je_bool (nl pos, false)
  | [< 'String (pos, s) >] -> J.Je_string (nl pos, s, true)
  | [< 'Lbracket pos1; l = list0_sep assignmentexpr comma; 'Rbracket pos2 >] ->
    let pos = merge_pos pos1 pos2 in
    J.Je_array (nl pos, l)
  | [< 'Lcurly pos1; l = list0_sep property_assignment comma; _ = option comma;
       'Rcurly pos2 >] ->
    let pos = merge_pos pos1 pos2 in
    J.Je_object(nl pos, l)
  | [< 'Lparen _; e = expr; 'Rparen _ >] -> e
  | [< 'Regexp (pos,s1,s2) >] -> J.Je_regexp (nl pos,s1,s2)
and statements (stream : tok_stream) : statement list =
  list0 statement stream
and statements_stm (stream : tok_stream) : statement =
  J.Js_block (label(),statements stream)
and property_name : tok_stream -> string = parser
  | [< 'Ident (_, i) >] -> i
  | [< 'String (_, s) >] -> s
  | [< 'Integer (_, i) >] -> i
and property_assignment : tok_stream -> string * expr = parser
  | [< p = property_name; 'Colon _; e = assignmentexpr >] ->
    (p,e)

let code stream =
  let stream = Stream.wrap stream in
  match stream with parser
  | [< r = statements; _ = Stream.empty >] -> r
let expr stream =
  let stream = Stream.wrap stream in
  match stream with parser
  | [< e = expr; _ = Stream.empty >] -> e
let stm stream =
  let stream = Stream.wrap stream in
  match stream with parser
  | [< s = statement; _ = Stream.empty >] -> s

type error =
(** Error while actually trying to parse file *)
| ParseError of int * int * string * string
(** IO error (i.e. couldn't open file) *)
| FileError of string

exception Exception of error
let pp f = function
  | ParseError (start, end_, lexem, s) ->
    Format.fprintf f "Parse error at %d-%d on %S%s"
      start
      end_
      lexem
      (if s = "" then s else ": " ^ s)
  | FileError filename ->
    Format.fprintf f "Could not open file %s" filename

let build_parser ?(throw_exn=false) ?(globalize=fun x -> x)
    (parser_ : JsLex.token OriginalStream.t -> 'a) (stream,lexbuf) : 'a =
  try
    let code = parser_ stream in
    globalize code
  with Stream.Error s ->
    let error = ParseError
      (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf,
       Lexing.lexeme lexbuf, s) in
    if throw_exn then
      raise (Exception error)
    else (
      Format.eprintf "%a@." pp error;
      exit 1
    )

let global_expr ?(globalize=false) expr =
  if globalize then
    JsWalk.Expr.map (
      function
      | J.Je_ident (loc, J.Native (`local, id)) ->
          J.Je_ident (loc, J.Native (`global false, id))
      | e -> e
    ) expr
  else
    expr

let stream_of_file lex_comments file =
  try
    JsLex.stream_of_file ~lex_comments:lex_comments file
  with
  | Unix.Unix_error _ -> raise (Exception (FileError file))

module String =
struct
  let code ?throw_exn str =
    build_parser ?throw_exn code (JsLex.stream_of_string str)
  let expr ?throw_exn ?globalize str =
    build_parser ?throw_exn ~globalize:(global_expr ?globalize)
      expr (JsLex.stream_of_string ~lex_comments:true str)
  let stm ?throw_exn str =
    build_parser ?throw_exn stm (JsLex.stream_of_string ~lex_comments:true str)
end
module File =
struct
  let code ?throw_exn file =
    build_parser ?throw_exn code (stream_of_file true file)
  let expr ?throw_exn ?globalize file =
    build_parser ?throw_exn ~globalize:(global_expr ?globalize) expr
      (stream_of_file true file)
  let stm ?throw_exn file =
    build_parser ?throw_exn stm (stream_of_file true file)
end
