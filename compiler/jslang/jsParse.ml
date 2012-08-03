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
    | Some LT -> Stream.junk stream
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
    | Some LT ->
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
  | Some LT -> None
  | _ -> option parser_ stream

let semic stream =
  match Stream.peek_no_newline stream with
  | None
  | Some (Semic | LT) -> Stream.junk_no_newline stream
  | Some Rcurly -> () (* do not discard the bracket! *)
  | _ -> raise Stream.Failure
let ident = parser
  | [< 'Ident i >] -> i
let native = parser
  | [< 'Ident i >] -> native_ident i
let comma = parser
  | [< 'Comma >] -> ()
let barbar = parser
  | [< 'BarBar >] -> J.Jb_lor
let amperamper = parser
  | [< 'AmperAmper >] -> J.Jb_land
let bar = parser
  | [< 'Bar >] -> J.Jb_or
let chapeau = parser
  | [< 'Chapeau >] -> J.Jb_xor
let amper = parser
  | [< 'Amper >] -> J.Jb_and
let assignmentoperator = parser
  | [< 'TimesEqual >] -> J.Jb_mul_assign
  | [< 'PlusEqual >] -> J.Jb_add_assign
  | [< 'PercentEqual >] -> J.Jb_mod_assign
  | [< 'MinusEqual >] -> J.Jb_sub_assign
  | [< 'LtLtEqual >] -> J.Jb_lsl_assign
  | [< 'GtGtGtEqual >] -> J.Jb_asr_assign
  | [< 'GtGtEqual >] -> J.Jb_lsr_assign
  | [< 'Equal >] -> J.Jb_assign
  | [< 'DivEqual >] -> J.Jb_div_assign
  | [< 'ChapeauEqual >] -> J.Jb_xor_assign
  | [< 'BarEqual >] -> J.Jb_or_assign
  | [< 'AmperEqual >] -> J.Jb_and_assign
let equalityoperator = parser
  | [< 'EqualEqual >] -> J.Jb_eq
  | [< 'EqualEqualEqual >] -> J.Jb_seq
  | [< 'BangEqual >] -> J.Jb_neq
  | [< 'BangEqualEqual >] -> J.Jb_sneq
let relationaloperator = parser
  | [< 'Lt >] -> J.Jb_lt
  | [< 'Gt >] -> J.Jb_gt
  | [< 'Le >] -> J.Jb_leq
  | [< 'Ge >] -> J.Jb_geq
  | [< 'Instanceof >] -> J.Jb_instanceof
  | [< 'In >] -> J.Jb_in
let shiftoperator = parser
  | [< 'LtLt >] -> J.Jb_lsl
  | [< 'GtGt >] -> J.Jb_lsr
  | [< 'GtGtGt >] -> J.Jb_asr
let additiveoperator = parser
  | [< 'Plus >] -> J.Jb_add
  | [< 'Minus >] -> J.Jb_sub
let multiplicativeoperator = parser
  | [< 'Times >] -> J.Jb_mul
  | [< 'Div >] -> J.Jb_div
  | [< 'Percent >] -> J.Jb_mod
let unaryoperator = parser
  | [< 'PlusPlus >] -> J.Ju_add2_pre
  | [< 'Delete >] -> J.Ju_delete
  | [< 'Typeof >] -> J.Ju_typeof
  | [< 'Void >] -> J.Ju_void
  | [< 'MinusMinus >] -> J.Ju_sub2_pre
  | [< 'Plus >] -> J.Ju_add_pre
  | [< 'Minus >] -> J.Ju_sub_pre
  | [< 'Tilda >] -> J.Ju_tilde
  | [< 'Bang >] -> J.Ju_not
let postfixoperator = parser
  | [< 'PlusPlus >] -> J.Ju_add2_post
  | [< 'MinusMinus >] -> J.Ju_sub2_post

let rec statement = parser
  | [< 'Function; 'Ident name ?? "expected an identifier after 'function' in a statement"; 'Lparen; params = list0_sep native comma; 'Rparen; 'Lcurly; body = statements; 'Rcurly >] ->
    J.Js_function (label(), native_ident name, params, body)
  | [< 'Lcurly; block = statements; 'Rcurly ?? "expected a closing curly brace" >] ->
    J.Js_block (label(),block)
  | [< 'Semic; stream >] ->
    statement stream
  | [< 'Var; l = list1_sep vardeclaration comma; _ = semic >] ->
      (match l with
       | [(i,o)] -> J.Js_var (label (), i, o)
       | _ -> J.Js_block (label(), List.map (fun (i,o) -> J.Js_var (label(),i,o)) l))
  | [< 'If; 'Lparen; e = expr; 'Rparen; s1 = statement; stream >] ->
    let o =
      match stream with parser
      | [< 'Else; s2 = statement >] -> Some s2
      | [< >] -> None in
    J.Js_if (label(),e,s1,o)
  | [< 'Do; s = statement; 'While; 'Lparen; e = expr; 'Rparen; _ = semic >] ->
    J.Js_dowhile (label(),s,e)
  | [< 'While; 'Lparen; e = expr; 'Rparen; s = statement >] ->
    J.Js_while (label(),e,s)
  | [< 'For; 'Lparen; stream >] -> (
      match stream with parser
      | [< 'Var; (i,o) = vardeclaration; stream >] ->
        (match o with
        | Some (J.Je_binop (_,J.Jb_in,e1,e2)) ->
          (match stream with parser
          | [< 'Rparen; s = statement >] ->
              let s1 = J.Js_var (label(), i, Some e1) in
              let s2 = J.Js_forin (label(), J.Je_ident (label(),i), e2, s) in
              J.Js_block (label (), [s1; s2])
          )
        | _ ->
          match stream with parser
          | [< 'In; e2 = expr; 'Rparen; s = statement >] ->
              let s1 = J.Js_var (label(), i, o) in
              let s2 = J.Js_forin (label(), J.Je_ident(label(),i), e2, s) in
              J.Js_block (label (), [s1; s2])
          | [< 'Comma; l = list1_sep vardeclaration comma; 'Semic; e2 = option expr; 'Semic; e3 = option expr; 'Rparen; s = statement >] ->
              let s1 = J.Js_var (label(), i, o) in
              let s1_more = List.map (fun (i,o) -> J.Js_var (label(),i,o)) l in
              let s2 = J.Js_for (label(), None, e2, e3, s) in
              J.Js_block (label (), s1 :: s1_more @ [s2])
          | [< 'Semic; e2 = option expr; 'Semic; e3 = option expr; 'Rparen; s = statement >] ->
              let s1 = J.Js_var (label(), i, o) in
              let s2 = J.Js_for (label(), None, e2, e3, s) in
              J.Js_block (label (), [s1;s2])
        )
      | [< o1 = option expr; stream >] -> (
        match o1 with
        | Some J.Je_binop (_,J.Jb_in,e1,e2) -> (
          match stream with parser
          | [< 'Rparen; s = statement >] ->
              J.Js_forin (label(), e1, e2, s)
        )
        | _ ->
          match stream with parser
          | [< _ = semic; e2 = option expr; _ = semic; e3 = option expr; 'Rparen; s = statement >] ->
            J.Js_for (label(), o1, e2, e3, s)
      )
  )
  | [< 'Continue; o = option_no_newline ident; _ = semic >] -> J.Js_continue (label(), o)
  | [< 'Break; o = option_no_newline ident; _ = semic >] -> J.Js_break (label(), o)
  | [< 'Return; o = option_no_newline expr; _ = semic >] -> J.Js_return (label(), o)
  | [< 'With; 'Lparen; e = expr; 'Rparen; s = statement >] -> J.Js_with (label(),e,s)
  | [< 'Switch; 'Lparen; e = expr; 'Rparen; 'Lcurly; clauses = list0 caseclause; default = option defaultclause; 'Rcurly >] -> J.Js_switch (label(),e,clauses,default)
  | [< 'Throw; e = expr; _ = semic >] -> J.Js_throw (label(),e)
      (* the specification seems crazy, where is the problem with a newline here? *)
  | [< 'Debugger >] -> (*SDebugger*) failwith "No ast node for \"debugger\""
  | [< 'Try; b = block_stm; stream >] -> (
    match stream with parser
    | [< (i,s) = catch_block; o = option finally >] ->
      J.Js_trycatch (label(), b, [(i,None,s)], o)
    | [< c = finally >] ->
      J.Js_trycatch (label(), b, [], Some c)
  )
  | [< e = expr; stream >] ->
    match stream with parser
    | [< 'Colon; s = statement >] ->
      (match e with
      | J.Je_ident (label,i) ->
          (match i with
           | J.Native (_, i) -> J.Js_label (label,i,s)
           | _ -> assert false)
      | _ -> raise (Stream.Error "Invalid label"))
    | [< _ = semic >] -> J.Js_expr (label(), e)

and block = parser
  | [< 'Lcurly; l = statements; 'Rcurly >] -> l
and block_stm stream =
  J.Js_block (label(), block stream)

and vardeclaration = parser
  | [< 'Ident i; stream >] ->
    match stream with parser
    | [< 'Equal; e = assignmentexpr >] -> (native_ident i, Some e)
    | [< >] -> (native_ident i, None)

and caseclause = parser
  | [< 'Case; e = expr; 'Colon; l = statements_stm >] -> (e, l)
and defaultclause = parser
  | [< 'Default; 'Colon; l = statements_stm >] -> l

and catch_block = parser
  | [< 'Catch; 'Lparen; 'Ident i; 'Rparen; b = block_stm >] -> (native_ident i,b)
and finally = parser
  | [< 'Finally; b = block_stm >] -> b

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
    | [< 'Question; e2 = assignmentexpr; 'Colon; e3 = conditionalexpr >] -> J.Je_cond (label(),e,e2,e3)
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
  | [< 'New; e = lefthandsideexpr true; el = option_default [] arguments; stream >] ->
      let e = J.Je_new (label(),e,el) in
      dot_hashref_call true e stream
  | [< 'Function; name = option native; 'Lparen; params = list0_sep native comma; 'Rparen; 'Lcurly; body = statements; 'Rcurly; stream >] ->
      (* put the this rule into primaryexpr instead? *)
      let e = J.Je_function (label(),name,params,body) in
      dot_hashref_call (not new_) e stream
  | [< e = primaryexpr; r = dot_hashref_call (not new_) e >] -> r
and dot_hashref_call can_call e = parser
  | [< 'Dot; 'Ident i; stream >] ->
    dot_hashref_call can_call (J.Je_dot (label(),e,i)) stream
  | [< 'Lbracket; i = expr; 'Rbracket; stream >] ->
    dot_hashref_call can_call (J.Je_binop (label(),J.Jb_hashref,e,i)) stream
  | [< 'Lparen when can_call; l = list0_sep assignmentexpr comma; 'Rparen; stream >] ->
    (* refusing to parse arguments when under a new because in [new f()], the arguments are given to new_
     * not to f *)
    dot_hashref_call can_call (J.Je_call (label(),e,l,false)) stream
  | [< >] -> e
and arguments = parser
  | [< 'Lparen; l = list0_sep assignmentexpr comma; 'Rparen >] -> l
and primaryexpr = parser
  | [< 'Null >] -> J.Je_null (label())
  | [< 'This >] -> J.Je_this (label())
  | [< 'Ident i >] -> J.Je_ident (label(), native_ident i)
  | [< 'Integer i >] -> J.Je_num (label(), i)
  | [< 'True >] -> J.Je_bool (label(), true)
  | [< 'False >] -> J.Je_bool (label(), false)
  | [< 'String s >] -> J.Je_string (label(), s, true)
  | [< 'Lbracket; l = list0_sep assignmentexpr comma; 'Rbracket >] -> J.Je_array (label(), l)
  | [< 'Lcurly; l = list0_sep property_assignment comma; _ = option comma; 'Rcurly >] -> J.Je_object(label(), l)
  | [< 'Lparen; e = expr; 'Rparen >] -> e
  | [< 'Regexp (s1,s2) >] -> J.Je_regexp (label(),s1,s2)
and statements stream = list0 statement stream
and statements_stm stream = J.Js_block (label(),statements stream)
and property_name = parser
  | [< 'Ident i >] -> i
  | [< 'String s >] -> s
  | [< 'Integer i >] -> i
and property_assignment = parser
  | [< p = property_name; 'Colon; e = assignmentexpr >] ->
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
