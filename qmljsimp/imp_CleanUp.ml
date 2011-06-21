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
module J = JsAst
module List = Base.List

let rec clean_block_stm acc stm =
  (* not sure how to write that with traverse *)
  match stm with
  | J.Js_block (_,body) -> clean_block_stms acc body
  | J.Js_function (label, name, params, statements) ->
      let statements = clean_block_stms_no_acc statements in
      J.Js_function (label, name, params, statements) :: acc
  | _ ->
      let stm = JsWalk.OnlyStatement.map_nonrec clean_block_stm_no_acc stm in
      stm :: acc

and clean_block_stm_no_acc stm =
  match clean_block_stms_no_acc [stm] with
  | [stm] -> stm
  | l -> JsCons.Statement.block l
and clean_block_stms acc stms =
  List.fold_left clean_block_stm acc stms
and clean_block_stms_no_acc stms =
  List.rev (clean_block_stms [] stms)

(* beware: it doesn't clean statements inside expressions *)
let clean_block_stm = clean_block_stm_no_acc
let clean_block = clean_block_stms_no_acc

(* check if you always leave a block with a return
 * (in a very conservative way) *)
let rec always_return = function
  | J.Js_return _ -> true
  | J.Js_block (_,stms) -> always_return_stms stms
  | J.Js_if (_,_,s,o) -> always_return s && Option.default_map false always_return o
  | _ -> false
and always_return_stms stms =
  List.exists always_return stms

let compare_label = String.compare

let rec compare_expr e1 e2 =
  match e1, e2 with
  | J.Je_this _, J.Je_this _ -> 0
  | J.Je_this _, _ -> -1
  | _, J.Je_this _ -> 1
  | J.Je_ident (_,ident1), J.Je_ident (_,ident2) ->
      JsIdent.compare ident1 ident2
  | J.Je_ident _, _ -> -1
  | _, J.Je_ident _ -> 1
  | J.Je_array (_,el1), J.Je_array (_,el2) ->
      List.make_compare compare_expr el1 el2
  | J.Je_array _, _ -> -1
  | _, J.Je_array _ -> 1
  | J.Je_comma (_,el1,e1), J.Je_comma (_,el2,e2) -> (
      match List.make_compare compare_expr el1 el2 with
      | 0 -> compare_expr e1 e2
      | c -> c
    )
  | J.Je_comma _, _ -> -1
  | _, J.Je_comma _ -> 1
  | J.Je_object (_,sel1), J.Je_object (_,sel2) ->
      List.make_compare (* could sort the fields *)
        (fun (s1,e1) (s2,e2) ->
           match String.compare s1 s2 with
           | 0 -> compare_expr e1 e2
           | c -> c
        ) sel1 sel2
  | J.Je_object _, _ -> -1
  | _, J.Je_object _ -> 1
  | J.Je_string (_,s1,_), J.Je_string (_,s2,_) -> String.compare s1 s2
  | J.Je_string _, _ -> -1
  | _, J.Je_string _ -> 1
  | J.Je_num (_,s1), J.Je_num (_,s2) -> String.compare s1 s2 (* could normalize before comparing *)
  | J.Je_num _, _ -> -1
  | _, J.Je_num _ -> 1
  | J.Je_null _, J.Je_null _ -> 0
  | J.Je_null _, _ -> -1
  | _, J.Je_null _ -> 1
  | J.Je_undefined _, J.Je_undefined _ -> 0
  | J.Je_undefined _, _ -> -1
  | _, J.Je_undefined _ -> 1
  | J.Je_bool (_,b1), J.Je_bool (_,b2) -> Pervasives.compare b1 b2
  | J.Je_bool _, _ -> -1
  | _, J.Je_bool _ -> 1
  | J.Je_regexp (_,l1,r1), J.Je_regexp (_,l2,r2) -> (
      match String.compare l1 l2 with
      | 0 -> String.compare r1 r2
      | c -> c
    )
  | J.Je_regexp _, _ -> -1
  | _, J.Je_regexp _ -> 1
  | J.Je_function (_,name1,params1,body1), J.Je_function (_,name2,params2,body2) -> (
      match Option.make_compare JsIdent.compare name1 name2 with
      | 0 -> (
          match List.make_compare JsIdent.compare params1 params2 with
          | 0 -> List.make_compare compare_stm body1 body2
          | c -> c
        )
      | c -> c
    )
  | J.Je_function _, _ -> -1
  | _, J.Je_function _ -> 1
  | J.Je_dot (_,e1,s1), J.Je_dot (_,e2,s2) -> (
      match compare_expr e1 e2 with
      | 0 -> String.compare s1 s2
      | c -> c
    )
  | J.Je_dot _, _ -> -1
  | _, J.Je_dot _ -> 1
  | J.Je_unop (_,op1,e1), J.Je_unop (_,op2,e2) -> (
      match Pervasives.compare op1 op2 with
      | 0 -> compare_expr e1 e2
      | c -> c
    )
  | J.Je_unop _, _ -> -1
  | _, J.Je_unop _ -> 1
  | J.Je_binop (_,op1,l1,r1), J.Je_binop (_,op2,l2,r2) -> (
      match Pervasives.compare op1 op2 with
      | 0 -> (
          match compare_expr l1 l2 with
          | 0 -> compare_expr r1 r2
          | c -> c
        )
      | c -> c
    )
  | J.Je_binop _, _ -> -1
  | _, J.Je_binop _ -> 1
  | J.Je_cond (_,t1,l1,r1), J.Je_cond (_,t2,l2,r2) -> (
      match compare_expr t1 t2 with
      | 0 -> (
          match compare_expr l1 l2 with
          | 0 -> compare_expr r1 r2
          | c -> c
        )
      | c -> c
    )
  | J.Je_cond _, _ -> -1
  | _, J.Je_cond _ -> 1
  | J.Je_call (_,e1,el1,_), J.Je_call (_,e2,el2,_) -> (
      match compare_expr e1 e2 with
      | 0 -> List.make_compare compare_expr el1 el2
      | c -> c
    )
  | J.Je_call _, _ -> -1
  | _, J.Je_call _ -> 1
  | J.Je_new (_,e1,el1), J.Je_new (_,e2,el2) -> (
      match compare_expr e1 e2 with
      | 0 -> List.make_compare compare_expr el1 el2
      | c -> c
    )
  | J.Je_new _, _ -> -1
  | _, J.Je_new _ -> 1

  | J.Je_runtime (_, e1), J.Je_runtime (_, e2) ->
      JsAstRuntime.compare e1 e2
  | J.Je_runtime _, _ -> -1
  | _, J.Je_runtime _ -> 1

  | J.Je_hole _, J.Je_hole _ ->
      failwith "Screw you, write the comparison yourself"

and compare_stm s1 s2 =
  match s1, s2 with
  | J.Js_var (_,i1,o1), J.Js_var (_,i2,o2) -> (
      match JsIdent.compare i1 i2 with
      | 0 -> Option.make_compare compare_expr o1 o2
      | c -> c
    )
  | J.Js_var _, _ -> -1
  | _, J.Js_var _ -> 1
  | J.Js_function (_,name1,params1,body1), J.Js_function (_,name2,params2,body2) -> (
      match JsIdent.compare name1 name2 with
      | 0 -> (
          match List.make_compare JsIdent.compare params1 params2 with
          | 0 -> List.make_compare compare_stm body1 body2
          | c -> c
        )
      | c -> c
    )
  | J.Js_function _, _ -> -1
  | _, J.Js_function _ -> 1
  | J.Js_return (_,o1), J.Js_return (_,o2) ->
      Option.make_compare compare_expr o1 o2
  | J.Js_return _, _ -> -1
  | _, J.Js_return _ -> 1
  | J.Js_continue (_,o1), J.Js_continue (_,o2) ->
      Option.make_compare compare_label o1 o2
  | J.Js_continue _, _ -> -1
  | _, J.Js_continue _ -> 1
  | J.Js_break (_,o1), J.Js_break (_,o2) ->
      Option.make_compare compare_label o1 o2
  | J.Js_break _, _ -> -1
  | _, J.Js_break _ -> 1
  | J.Js_switch (_,e1,esl1,o1), J.Js_switch (_,e2,esl2,o2) -> (
      match compare_expr e1 e2 with
      | 0 -> (
          match List.make_compare
            (fun (e1,s1) (e2,s2) ->
               match compare_expr e1 e2 with
               | 0 -> compare_stm s1 s2
               | c -> c
            ) esl1 esl2 with
          | 0 -> Option.make_compare compare_stm o1 o2
          | c -> c
        )
      | c -> c
    )
  | J.Js_switch _, _ -> -1
  | _, J.Js_switch _ -> 1
  | J.Js_if (_,e1,t1,o1), J.Js_if (_,e2,t2,o2) -> (
      match compare_expr e1 e2 with
      | 0 -> (
          match compare_stm t1 t2 with
          | 0 -> Option.make_compare compare_stm o1 o2
          | c -> c
        )
      | c -> c
    )
  | J.Js_if _, _ -> -1
  | _, J.Js_if _ -> 1
  | J.Js_throw (_,e1), J.Js_throw (_,e2) ->
      compare_expr e1 e2
  | J.Js_throw _, _ -> -1
  | _, J.Js_throw _ -> 1
  | J.Js_expr (_,e1), J.Js_expr (_,e2) ->
      compare_expr e1 e2
  | J.Js_expr _, _ -> -1
  | _, J.Js_expr _ -> 1
  | J.Js_trycatch (_,s1,iosl1,o1), J.Js_trycatch (_,s2,iosl2,o2) -> (
      match compare_stm s1 s2 with
      | 0 -> (
          match List.make_compare
            (fun (i1,o1,s1) (i2,o2,s2) ->
               match JsIdent.compare i1 i2 with
               | 0 -> (
                   match Option.make_compare compare_expr o1 o2 with
                   | 0 -> compare_stm s1 s2
                   | c -> c
                 )
               | c -> c
            ) iosl1 iosl2 with
          | 0 -> Option.make_compare compare_stm o1 o2
          | c -> c
        )
      | c -> c
    )
  | J.Js_trycatch _, _ -> -1
  | _, J.Js_trycatch _ -> 1
  | J.Js_for (_,l1,l2,l3,s1), J.Js_for (_,r1,r2,r3,s2) -> (
      match Option.make_compare compare_expr l1 r1 with
      | 0 -> (
          match Option.make_compare compare_expr l2 r2 with
          | 0 -> (
              match Option.make_compare compare_expr l3 r3 with
              | 0 -> compare_stm s1 s2
              | c -> c
            )
          | c -> c
        )
      | c -> c
    )
  | J.Js_for _, _ -> -1
  | _, J.Js_for _ -> 1
  | J.Js_forin (_,i1,e1,s1), J.Js_forin (_,i2,e2,s2) -> (
      match compare_expr i1 i2 with
      | 0 -> (
          match compare_expr e1 e2 with
          | 0 -> compare_stm s1 s2
          | c -> c
        )
      | c -> c
    )
  | J.Js_forin _, _ -> -1
  | _, J.Js_forin _ -> 1
  | J.Js_dowhile (_,s1,e1), J.Js_dowhile (_,s2,e2) -> (
      match compare_stm s1 s2 with
      | 0 -> compare_expr e1 e2
      | c -> c
    )
  | J.Js_dowhile _, _ -> -1
  | _, J.Js_dowhile _ -> 1
  | J.Js_while (_,e1,s1), J.Js_while (_,e2,s2) -> (
      match compare_expr e1 e2 with
      | 0 -> compare_stm s1 s2
      | c -> c
    )
  | J.Js_while _, _ -> -1
  | _, J.Js_while _ -> 1
  | J.Js_block (_,sl1), J.Js_block (_,sl2) ->
      List.make_compare compare_stm sl1 sl2
  | J.Js_block _, _ -> -1
  | _, J.Js_block _ -> 1
  | J.Js_with (_,e1,s1), J.Js_with (_,e2,s2) ->(
      match compare_expr e1 e2 with
      | 0 -> compare_stm s1 s2
      | c -> c
    )
  | J.Js_with _, _ -> -1
  | _, J.Js_with _ -> 1
  | J.Js_label (_,l1,s1), J.Js_label (_,l2,s2) -> (
      match compare_label l1 l2 with
      | 0 -> compare_stm s1 s2
      | c -> c
    )
  | J.Js_label _, _ -> -1
  | _, J.Js_label _ -> 1
  | J.Js_comment _, J.Js_comment _ -> 0

let clean_assign ~use_shortcut_assignment stm =
  JsWalk.TStatement.map_up
    (fun s ->
       match s with
       | J.Js_expr (_,e) when not (Imp_Common.does_side_effects e) ->
           JsCons.Statement.block []
       | J.Js_if (label,J.Je_unop(_,J.Ju_not,e),s1,Some s2) ->
           (* if (!e) s1 s2 -> if (e) s2 s1 *)
           J.Js_if (label, e, s2, Some s1)
       | J.Js_if (_, J.Je_unop (_,J.Ju_not,e1),J.Js_expr (_,e2),None) ->
           (* if (!e1) { e2 } -> e1 || e2 *)
           JsCons.Statement.expr (JsCons.Expr.lor_ e1 e2)
       | J.Js_if (_, e1,J.Js_expr (_,e2),None) ->
           (* if (e1) { e2 } -> e1 && e2 *)
           JsCons.Statement.expr (JsCons.Expr.land_ e1 e2)
       | J.Js_if (_, e1,J.Js_expr (_,J.Je_binop (_, J.Jb_assign, assign1, value1)),Some J.Js_expr (_,J.Je_binop (_, J.Jb_assign, assign2, value2))) when compare_expr assign1 assign2 = 0 ->
           (* if (e1) { assign = value1 } else { assign = value2 } -> assign = e1 ? value1 : value2 *)
           JsCons.Statement.expr (JsCons.Expr.assign assign1 (JsCons.Expr.cond e1 value1 value2))
       | J.Js_if (_, e1,J.Js_expr (_,e2), Some J.Js_expr (_,e3)) ->
           (* if (e1) { e2 } else { e3 } -> e1 ? e2 : e3 *)
           JsCons.Statement.expr (JsCons.Expr.cond e1 e2 e3)
       | J.Js_if (label1, e1, J.Js_return (label2,Some e2), Some s2) -> (
           match s2 with
           | J.Js_comment _
           | J.Js_block (_,[])
           | J.Js_block (_,[J.Js_comment _]) ->
               J.Js_if (label1, e1, J.Js_return (label2,Some e2), None)
           | J.Js_block (_,[J.Js_return (_,Some e3)])
           | J.Js_return (_,Some e3) ->
               J.Js_return (label1, Some (J.Je_cond (label2, e1, e2, e3)))
           | _ -> s
         )
       | J.Js_while (label, J.Je_bool (label2,true), s) ->
           let s = clean_block_stm s in
           (match s with
            | J.Js_if (_,e, s1, Some s2) when always_return s2 ->
                JsCons.Statement.block [J.Js_while (label, e, s1); s2]
            | J.Js_if (_,e, s1, Some s2) when always_return s1 ->
                JsCons.Statement.block [J.Js_while (label, JsCons.Expr.unop J.Ju_not e, s2); s1]
            | s -> J.Js_while (label, J.Je_bool (label2,true), s))
       | _ -> s
    )
    (fun e ->
       match e with
       | J.Je_unop (_,J.Ju_not,J.Je_binop (label,op,e1,e2)) -> (
           match op with
           | J.Jb_lt -> J.Je_binop (label, J.Jb_geq, e1, e2)
           | J.Jb_gt -> J.Je_binop (label, J.Jb_leq, e1, e2)
           | J.Jb_leq -> J.Je_binop (label, J.Jb_gt, e1, e2)
           | J.Jb_geq -> J.Je_binop (label, J.Jb_lt, e1, e2)
           | J.Jb_eq -> J.Je_binop (label, J.Jb_neq, e1, e2)
           | J.Jb_neq -> J.Je_binop (label, J.Jb_eq, e1, e2)
           | J.Jb_seq -> J.Je_binop (label, J.Jb_sneq, e1, e2)
           | J.Jb_sneq -> J.Je_binop (label, J.Jb_seq, e1, e2)
           | _ -> e
         )
       | J.Je_binop (label, J.Jb_assign, (J.Je_ident (_,i) as ident),
                     (J.Je_binop (_,op,e1,(J.Je_ident(_,j) as right))
                     |J.Je_binop (_,op,J.Je_ident (_,j), (e1 as right)))) when use_shortcut_assignment && JsIdent.equal i j -> (
           match op, e1, e1 == right with
           | J.Jb_add, J.Je_num (_,("1"|"1.")), _ (* now, that's just dirty *) -> J.Je_unop (label, J.Ju_add2_post, ident)
           | J.Jb_sub, J.Je_num (_,("1"|"1.")), _ -> J.Je_unop (label, J.Ju_sub2_post, ident)
           | J.Jb_mul, _, _ -> J.Je_binop (label, J.Jb_mul_assign, ident, e1)
           | J.Jb_div, _, true -> J.Je_binop (label, J.Jb_div_assign, ident, e1)
           | J.Jb_mod, _, true -> J.Je_binop (label, J.Jb_mod_assign, ident, e1)
           | J.Jb_add, _, true (* BEWARE: addition is not commutative in js
                                * because it is also string concatenation *)
               -> J.Je_binop (label, J.Jb_add_assign, ident, e1)
           | J.Jb_sub, _, true -> J.Je_binop (label, J.Jb_sub_assign, ident, e1)
           | J.Jb_lsl, _, true -> J.Je_binop (label, J.Jb_lsl_assign, ident, e1)
           | J.Jb_lsr, _, true -> J.Je_binop (label, J.Jb_lsr_assign, ident, e1)
           | J.Jb_asr, _, true -> J.Je_binop (label, J.Jb_asr_assign, ident, e1)
           | J.Jb_and, _, _ -> J.Je_binop (label, J.Jb_and_assign, ident, e1)
           | J.Jb_xor, _, _ -> J.Je_binop (label, J.Jb_xor_assign, ident, e1)
           | J.Jb_or,  _, _ -> J.Je_binop (label, J.Jb_or_assign,  ident, e1)
           | _ -> e
         )
       | J.Je_binop (label,op,J.Je_num (_,i1),J.Je_num (_,i2)) -> (
           try
             let i1 = float_of_string i1 in
             let i2 = float_of_string i2 in
             let f = JsCons.Expr.float ~label in
             let b = JsCons.Expr.bool ~label in
             match op with
             | J.Jb_mul -> f (i1 *. i2)
             | J.Jb_div -> f (i1 /. i2)
             (*| J.Jb_mod -> f (i1 % i2) FIXME *)
             | J.Jb_add -> f (i1 +. i2)
             | J.Jb_sub -> f (i1 -. i2)
             | J.Jb_lt -> b (i1 < i2)
             | J.Jb_gt -> b (i1 > i2)
             | J.Jb_leq -> b (i1 <= i2)
             | J.Jb_geq -> b (i1 >= i2)
             (*| J.Jb_lsr -> f (i1 lsr i2)
             | J.Jb_lsl -> f (i1 lsl i2)
             | J.Jb_asr -> f (l1 asr i2) FIXME*)
             | J.Jb_eq -> b (i1 = i2)
             | J.Jb_neq -> b (i1 <> i2)
             | J.Jb_land -> f (if i1 <> 0. then i2 else i1)
             | J.Jb_lor -> f (if i1 <> 0. then i1 else i2)
             (*| J.Jb_and -> f (i1 & i2)*)
                 (*| J.Jb_xor -> i1 *)
             (*| J.Jb_or -> f (i1 | i2)*) (* FIXME *)
             | _ -> e
           with Failure "float_of_string" ->
             e
         )
       | J.Je_binop(label,J.Jb_add,J.Je_string (_,"",style),J.Je_num (_,s)) -> (
           try
             (* "" + 0x321 doesn't give "0x321", we need to do some kind of normalization
              * to convert an int to a string
              * checking that int_of_string works is a weak attempt to do that *)
             ignore (int_of_string s);
             J.Je_string (label,s,style)
           with Failure "int_of_string" -> e
         )
       | J.Je_binop(label,J.Jb_add,J.Je_string (_,s1,style),J.Je_string (_,s2,_)) ->
           (* FIXME: need to normalize the ast: "a" + ("b" + c) won't be seen
            * but BEWARE: addition is commutative (on numbers) but not associative in js
            * (1+(2+"a") -> "12a" vs (1+2)+"a") -> "3a" *)
           J.Je_string (label,s1^s2,style)
       | J.Je_cond (label,a,J.Je_bool (_,false), J.Je_bool (_,true)) ->
           (* a ? false : true -> !a is valid, but a ? true : false -> a is not valid
            * because a may not be a boolean: [function list_empty(l) { return l.nil ? true : false }] *)
           JsCons.Expr.not_ ~label a
       | J.Je_binop (_, J.Jb_assign, (J.Je_ident (_,i) as e), J.Je_ident (_,j)) when JsIdent.equal i j ->
           e
       | J.Je_cond (_,J.Je_bool (_,b), e1, e2) ->
           if b then e1 else e2
       | J.Je_cond (label,J.Je_unop(_,J.Ju_not,e), e1, e2) ->
           J.Je_cond (label, e, e2, e1)
       | J.Je_comma (label,el,e) ->
           let el = List.filter Imp_Common.does_side_effects el in
           if el = [] then e else J.Je_comma (label,el,e)
       | J.Je_function (label,name_opt,params,body) ->
           let body = clean_block body in
           J.Je_function (label,name_opt,params,body)
       | J.Je_call (_, J.Je_function (_, None, [], [J.Js_return (_,Some e)]), [], _) ->
           e
       | _ -> e
    ) stm

let clean_stm ~use_shortcut_assignment stm =
  let stm = clean_assign ~use_shortcut_assignment stm in
  let code = clean_block [stm] in
  code

let clean ~use_shortcut_assignment code =
  (* clean assign generates empty blocks
   * so it better to clean blocks after *)
  let code = List.map (clean_assign ~use_shortcut_assignment) code in
  let code = clean_block code in
  code
