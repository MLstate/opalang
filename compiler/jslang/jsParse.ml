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
open JsLex (* bringing token in the scope *)

(* right now, the parser doesn't insert any positions in the ast *)
let dummy_pos = FilePos.nopos "jsParse"
let nl pos = Annot.next_label pos
let label () = Annot.next_label dummy_pos
let native_ident = JsCons.Ident.native

(* redefining the modules Stream allows us to 'override' the syntax of streams
 * the new peek, junk and empty function look at the first non-newline token
 * (which allows us to write almost the whole parser while implicitely
 * discarding newlines) *)
module Stream =
struct

  type 'a t = 'a Stream.t
  exception Failure = Stream.Failure
  exception Error = Stream.Error
  let from = Stream.from
  let junk_no_newline = Stream.junk
  let peek_no_newline = Stream.peek

  let junk stream =
    (* this function is symmetric with peek below *)
    (match Stream.peek stream with
    | Some (LT _) -> Stream.junk stream
    | _ -> ());
    Stream.junk stream

  (*let peek stream =
    match Stream.npeek 2 stream with
    | [LT; a] -> Some a
    | [LT] -> None
    | a :: _ -> Some a
    | [] -> None*)
  let peek stream = (* this Stream.peek makes the parsing really faster *)
    match Stream.peek stream with
    | Some (LT _) ->
      (* using the invariant that says that you never have two consecutives
       * newlines in the token stream *)
      (match Stream.npeek 2 stream with
      | _ :: t :: _ -> Some t
      | _ -> None)
    | o -> o

  (* redefining empty because a stream with only a newline must be considered
   * as empty *)
  let empty s =
    match peek s with
    | None -> ()
    | Some _ -> raise Stream.Failure
end

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
  let rec aux acc = parser
    | [< op = sep; e = parser_; stream >] -> aux (J.Je_binop (label(),op,acc,e)) stream
    | [< >] -> acc in
  match stream with parser
  | [< e = parser_; stream >] -> aux e stream
let list1_sep_right_assoc parser_ sep stream =
  let rec aux acc = parser
    | [< op = sep; e = parser_; stream >] -> aux ((op, e) :: acc) stream
    | [< >] -> acc in
  match stream with parser
  | [< r = parser_; stream >] ->
    match aux [] stream with
    | [] -> r
    | (op,e) :: t ->
      let op, e =
        List.fold_left (fun (op,e1) (op2,e2) -> (op2, J.Je_binop (label(),op,e2,e1))) (op,e) t in
      J.Je_binop(label(),op,r,e)

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
  | None
  | Some (Semic _ | LT _) -> Stream.junk_no_newline stream
  | Some (Rcurly _) -> () (* do not discard the bracket! *)
  | _ -> raise Stream.Failure
let ident = parser
  | [< 'Ident (_, i) >] -> i
let native = parser
  | [< 'Ident (_, i) >] -> native_ident i
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
  | [< 'PlusPlus _ >] -> J.Ju_add2_pre
  | [< 'Delete _ >] -> J.Ju_delete
  | [< 'Typeof _ >] -> J.Ju_typeof
  | [< 'Void _ >] -> J.Ju_void
  | [< 'MinusMinus _ >] -> J.Ju_sub2_pre
  | [< 'Plus _ >] -> J.Ju_add_pre
  | [< 'Minus _ >] -> J.Ju_sub_pre
  | [< 'Tilda _ >] -> J.Ju_tilde
  | [< 'Bang _ >] -> J.Ju_not
let postfixoperator = parser
  | [< 'PlusPlus _ >] -> J.Ju_add2_post
  | [< 'MinusMinus _ >] -> J.Ju_sub2_post

let rec statement = parser
  | [< 'DocComment (pos, lines) >] ->
    J.Js_comment (nl pos, J.Jc_doc (nl pos, lines))
  | [< 'Function _;
       'Ident (_, name) ?? "expected an identifier after 'function' in a statement";
       'Lparen _; params = list0_sep native comma; 'Rparen _;
       'Lcurly _; body = statements; 'Rcurly _ >] ->
    J.Js_function (label(), native_ident name, params, body)
  | [< 'Lcurly _; block = statements; 'Rcurly _ ?? "expected a closing curly brace" >] ->
    J.Js_block (label(),block)
  | [< 'Semic _; stream >] ->
    statement stream
  | [< 'Var _; l = list1_sep vardeclaration comma; _ = semic >] ->
      (match l with
       | [(i,o)] -> J.Js_var (label (), i, o)
       | _ -> J.Js_block (label(), List.map (fun (i,o) -> J.Js_var (label(),i,o)) l))
  | [< 'If _; 'Lparen _; e = expr; 'Rparen _; s1 = statement; stream >] ->
    let o =
      match stream with parser
      | [< 'Else _; s2 = statement >] -> Some s2
      | [< >] -> None in
    J.Js_if (label(),e,s1,o)
  | [< 'Do _; s = statement; 'While _; 'Lparen _; e = expr; 'Rparen _; _ = semic >] ->
    J.Js_dowhile (label(),s,e)
  | [< 'While _; 'Lparen _; e = expr; 'Rparen _; s = statement >] ->
    J.Js_while (label(),e,s)
  | [< 'For _; 'Lparen _; stream >] -> (
      match stream with parser
      | [< 'Var _; (i,o) = vardeclaration; stream >] ->
        (match o with
        | Some (J.Je_binop (_,J.Jb_in,e1,e2)) ->
          (match stream with parser
          | [< 'Rparen _; s = statement >] ->
              let s1 = J.Js_var (label(), i, Some e1) in
              let s2 = J.Js_forin (label(), J.Je_ident (label(),i), e2, s) in
              J.Js_block (label (), [s1; s2])
          )
        | _ ->
          match stream with parser
          | [< 'In _; e2 = expr; 'Rparen _; s = statement >] ->
              let s1 = J.Js_var (label(), i, o) in
              let s2 = J.Js_forin (label(), J.Je_ident(label(),i), e2, s) in
              J.Js_block (label (), [s1; s2])
          | [< 'Comma _; l = list1_sep vardeclaration comma; 'Semic _;
               e2 = option expr; 'Semic _; e3 = option expr; 'Rparen _;
               s = statement >] ->
              let s1 = J.Js_var (label(), i, o) in
              let s1_more = List.map (fun (i,o) -> J.Js_var (label(),i,o)) l in
              let s2 = J.Js_for (label(), None, e2, e3, s) in
              J.Js_block (label (), s1 :: s1_more @ [s2])
          | [< 'Semic _; e2 = option expr; 'Semic _; e3 = option expr;
               'Rparen _; s = statement >] ->
              let s1 = J.Js_var (label(), i, o) in
              let s2 = J.Js_for (label(), None, e2, e3, s) in
              J.Js_block (label (), [s1;s2])
        )
      | [< o1 = option expr; stream >] -> (
        match o1 with
        | Some J.Je_binop (_,J.Jb_in,e1,e2) -> (
          match stream with parser
          | [< 'Rparen _; s = statement >] ->
              J.Js_forin (label(), e1, e2, s)
        )
        | _ ->
          match stream with parser
          | [< _ = semic; e2 = option expr; _ = semic; e3 = option expr;
               'Rparen _; s = statement >] ->
            J.Js_for (label(), o1, e2, e3, s)
      )
  )
  | [< 'Continue _; o = option_no_newline ident; _ = semic >] -> J.Js_continue (label(), o)
  | [< 'Break _; o = option_no_newline ident; _ = semic >] -> J.Js_break (label(), o)
  | [< 'Return _; o = option_no_newline expr; _ = semic >] -> J.Js_return (label(), o)
  | [< 'With _; 'Lparen _; e = expr; 'Rparen _; s = statement >] -> J.Js_with (label(),e,s)
  | [< 'Switch _; 'Lparen _; e = expr; 'Rparen _;
       'Lcurly _; clauses = list0 caseclause; default = option defaultclause;
       'Rcurly _ >] -> J.Js_switch (label(),e,clauses,default)
  | [< 'Throw _; e = expr; _ = semic >] -> J.Js_throw (label(),e)
      (* the specification seems crazy, where is the problem with a newline here? *)
  | [< 'Debugger _ >] -> (*SDebugger*) failwith "No ast node for \"debugger\""
  | [< 'Try _; b = block_stm; stream >] -> (
    match stream with parser
    | [< (i,s) = catch_block; o = option finally >] ->
      J.Js_trycatch (label(), b, [(i,None,s)], o)
    | [< c = finally >] ->
      J.Js_trycatch (label(), b, [], Some c)
  )
  | [< e = expr; stream >] ->
    match stream with parser
    | [< 'Colon _; s = statement >] ->
      (match e with
      | J.Je_ident (label,i) ->
          (match i with
           | J.Native (_, i) -> J.Js_label (label,i,s)
           | _ -> assert false)
      | _ -> raise (Stream.Error "Invalid label"))
    | [< _ = semic >] -> J.Js_expr (label(), e)

and block = parser
  | [< 'Lcurly _; l = statements; 'Rcurly _ >] -> l
and block_stm stream =
  J.Js_block (label(), block stream)

and vardeclaration = parser
  | [< 'Ident (_, i); stream >] ->
    match stream with parser
    | [< 'Equal _; e = assignmentexpr >] -> (native_ident i, Some e)
    | [< >] -> (native_ident i, None)

and caseclause = parser
  | [< 'Case _; e = expr; 'Colon _; l = statements_stm >] -> (e, l)
and defaultclause = parser
  | [< 'Default _; 'Colon _; l = statements_stm >] -> l

and catch_block = parser
  | [< 'Catch _; 'Lparen _; 'Ident (_, i); 'Rparen _; b = block_stm >] -> (native_ident i,b)
and finally = parser
  | [< 'Finally _; b = block_stm >] -> b

and expr stream =
  match rev_list1_sep assignmentexpr comma stream with
  | [] -> assert false
  | [e] -> e
  | e :: l -> J.Je_comma (label(), List.rev l, e)
and assignmentexpr stream =
  list1_sep_right_assoc conditionalexpr assignmentoperator stream
and conditionalexpr = parser
  | [< e = logicalorexpr; stream >] ->
    match stream with parser
    | [< 'Question _; e2 = assignmentexpr; 'Colon _; e3 = conditionalexpr >] -> J.Je_cond (label(),e,e2,e3)
    | [< >] -> e
and logicalorexpr stream =
  list1_sep_left_assoc logicalandexpr barbar stream
and logicalandexpr stream =
  list1_sep_left_assoc bitwiseorexpr amperamper stream
and bitwiseorexpr stream =
  list1_sep_left_assoc bitwisexorexpr bar stream
and bitwisexorexpr stream =
  list1_sep_left_assoc bitwiseandexpr chapeau stream
and bitwiseandexpr stream =
  list1_sep_left_assoc equalityexpr amper stream
and equalityexpr stream =
  list1_sep_left_assoc relationalexpr equalityoperator stream
and relationalexpr stream =
  list1_sep_left_assoc shiftexpr relationaloperator stream
and shiftexpr stream =
  list1_sep_left_assoc additiveexpr shiftoperator stream
and additiveexpr stream =
  list1_sep_left_assoc multiplicativeexpr additiveoperator stream
and multiplicativeexpr stream =
  list1_sep_left_assoc unaryexpr multiplicativeoperator stream
and unaryexpr = parser
  | [< l = rev_list1 unaryoperator; e = postfixexpr ?? "expected an expression after a postfix operator" >] ->
    List.fold_left (fun e op -> J.Je_unop (label(),op,e)) e l
  | [< e = postfixexpr >] -> e
and postfixexpr = parser
  | [< e = lefthandsideexpr false; o = option_no_newline postfixoperator >] ->
    match o with
    | None -> e
    | Some op -> J.Je_unop(label(),op,e)
and lefthandsideexpr new_ = parser
  | [< 'New _; e = lefthandsideexpr true; el = option_default [] arguments; stream >] ->
      let e = J.Je_new (label(),e,el) in
      dot_hashref_call true e stream
  | [< 'Function _; name = option native; 'Lparen _; params = list0_sep native comma; 'Rparen _; 'Lcurly _; body = statements; 'Rcurly _; stream >] ->
      (* put the this rule into primaryexpr instead? *)
      let e = J.Je_function (label(),name,params,body) in
      dot_hashref_call (not new_) e stream
  | [< e = primaryexpr; r = dot_hashref_call (not new_) e >] -> r
and dot_hashref_call can_call e = parser
  | [< 'Dot _; 'Ident (_, i); stream >] ->
    dot_hashref_call can_call (J.Je_dot (label(),e,i)) stream
  | [< 'Lbracket _; i = expr; 'Rbracket _; stream >] ->
    dot_hashref_call can_call (J.Je_binop (label(),J.Jb_hashref,e,i)) stream
  | [< 'Lparen _ when can_call; l = list0_sep assignmentexpr comma; 'Rparen _; stream >] ->
    (* refusing to parse arguments when under a new because in [new f()], the arguments are given to new_
     * not to f *)
    dot_hashref_call can_call (J.Je_call (label(),e,l,false)) stream
  | [< >] -> e
and arguments = parser
  | [< 'Lparen _; l = list0_sep assignmentexpr comma; 'Rparen _ >] -> l
and primaryexpr = parser
  | [< 'Null pos >] -> J.Je_null (nl pos)
  | [< 'This pos >] -> J.Je_this (nl pos)
  | [< 'Ident (pos, i) >] -> J.Je_ident (nl pos, native_ident i)
  | [< 'Integer (pos, i) >] -> J.Je_num (nl pos, i)
  | [< 'True pos >] -> J.Je_bool (nl pos, true)
  | [< 'False pos >] -> J.Je_bool (nl pos, false)
  | [< 'String (pos, s) >] -> J.Je_string (nl pos, s, true)
  | [< 'Lbracket _; l = list0_sep assignmentexpr comma; 'Rbracket _ >] -> J.Je_array (label(), l)
  | [< 'Lcurly _; l = list0_sep property_assignment comma; _ = option comma;
       'Rcurly _ >] -> J.Je_object(label(), l)
  | [< 'Lparen _; e = expr; 'Rparen _ >] -> e
  | [< 'Regexp (pos,s1,s2) >] -> J.Je_regexp (nl pos,s1,s2)
and statements stream = list0 statement stream
and statements_stm stream = J.Js_block (label(),statements stream)
and property_name = parser
  | [< 'Ident (_, i) >] -> i
  | [< 'String (_, s) >] -> s
  | [< 'Integer (_, i) >] -> i
and property_assignment = parser
  | [< p = property_name; 'Colon _; e = assignmentexpr >] ->
    (p,e)

let code = parser
  | [< r = statements; _ = Stream.empty >] -> r
let expr = parser
  | [< e = expr; _ = Stream.empty >] -> e
let stm = parser
  | [< s = statement; _ = Stream.empty >] -> s

type error = (int * int * string * string)
exception Exception of error
let pp f (start,end_,lexem,s) =
  Format.fprintf f "Parse error at %d-%d on %S%s"
    start
    end_
    lexem
    (if s = "" then s else ": " ^ s)

let build_parser ?(throw_exn=false) ?(globalize=fun x -> x) parser_ (stream,lexbuf) =
  try
    let code = parser_ stream in
    globalize code
  with Stream.Error s ->
    let tuple = (Lexing.lexeme_start lexbuf,Lexing.lexeme_end lexbuf,Lexing.lexeme lexbuf,s) in
    if throw_exn then
      raise (Exception tuple)
    else (
      Format.eprintf "%a@." pp tuple;
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

module String =
struct
  let code ?throw_exn str = build_parser ?throw_exn code (JsLex.stream_of_string str)
  let expr ?throw_exn ?globalize str = build_parser ?throw_exn ~globalize:(global_expr ?globalize) expr (JsLex.stream_of_string str)
  let stm ?throw_exn str = build_parser ?throw_exn stm (JsLex.stream_of_string str)
end
module File =
struct
  let code ?throw_exn file = build_parser ?throw_exn code (JsLex.stream_of_file file)
  let expr ?throw_exn ?globalize file = build_parser ?throw_exn ~globalize:(global_expr ?globalize) expr (JsLex.stream_of_file file)
  let stm ?throw_exn file = build_parser ?throw_exn stm (JsLex.stream_of_file file)
end
