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

(* depends *)
module String = BaseString
module List = BaseList
module Char = BaseChar

(* TODO remove *)
open Printf
open SurfaceAst
module Cons = SurfaceAstCons.StringCons

module Q = QmlAst

let (|>) = InfixOperator.(|>)

(* cf mli *)

(*
 * Summary:
 * - not ast related functions
 * - errors
 * - hints
 * - fresh
 * - annotations
 * - tuples, records
 * - identifiers
 * - constants
 * - functions
 * - operators
 * - datatype
 * - constructs
 * - functions expected from the stdlib
 * - pattern
 * - type const
 * - type vars
 * - types
 * - directives
 * - toplevel
 * - others
 * - actions                               action.trx
 * - css                                   css.trx
 * - inlined css                           parser_xml.trx
 * - trx                                   trx.trx
 *)

(* variant types are a nightmare *)
type ('a,'b) coerced_expr = ('a, [> `coerce ] as 'b) expr
type ('a,'b) coerced_expr_node = ('a, [> `coerce ] as 'b) expr_node

(*
 * not specific
 *)
let cur2 f x y = f(x,y)
let unc2 f (x,y) = f x y
let map_tuple4 f (a,b,c,d) =
  let a = f a
  and b = f b
  and c = f c
  and d = f d in
    (a,b,c,d)

(*
 * Errors
 *)

let filename = ref "dummy_filename"
let offset2linecol offset = FilePos.get_pos !filename offset
let offset2string offset = FilePos.to_string (FilePos.make_pos !filename offset offset)
let annot2loc annot =
  let start = FilePos.get_first_char annot.QmlLoc.pos in
  offset2linecol start

exception Specific_parse_error of (FilePos.pos * string)

(* FIXME: see ErrorUtils later *)
let const_ty_to_string = function
      | TypeConst ty ->
          begin
            match ty with
              | TyInt -> Opacapi.Types.int
              | TyFloat -> Opacapi.Types.float
              | TyString -> Opacapi.Types.string
          end
      | _ -> assert false
let isolate s = sprintf "`%s'" s

let warning1 s annot =
  OManager.printf "@{<yellow>Warning@}:\n%s, %s@." (FilePos.to_string annot.QmlLoc.pos) s

let warning ~level:_ s =
  OManager.printf "@{<yellow>Warning@}: %s@." s

let error1 s annot =
  let str = sprintf "%s, %s" (FilePos.to_string annot.QmlLoc.pos) s in
  raise (Specific_parse_error (annot.QmlLoc.pos,str))

let error_comment = error1 "you start an unterminated comment (the `/*' is not matched by a `*/')."
let error_string = error1 "you start an unterminated string (the `\"' is not matched by a closing `\"')."
let error_char_escape c = error1 (sprintf "the escape \\%s is illegal. The legal escapes are: \\{, \\}, \\n, \\t, \\r, \\', \\\", \\\\, \\<integer>, \\uXXXX, \\UXXXXXXXX." (Char.escaped c))
let error_fun_space = error1 (sprintf "you must not put a space between a function and its parameters.")
let error_directive_not_good_arguments (s,pos) = error1 (sprintf "`@%s' expects arguments." s) pos
let error_directive mess (s,pos) = error1 (sprintf "Directive `@%s' expects %s." s mess) pos
let error_static_record = error_directive "a static record (ie. an expression surrounded by curly braces)"
let error_static_string = error_directive "a static string"
let error_not_a_directive (s,pos) = error1 (sprintf "`@%s' is not a valid directive." s) pos
let error_directive_wrong_arguments_type (s,pos) = error1 (sprintf "`@%s' wasn't given the right arguments." s) pos
let error_directive_number_argument min ?(max=min) got (s,pos) =
  if max = min then error1 (sprintf "`@%s' expected %d argument%s, but received %d." s min (if min < 2 then "" else "s") got) pos
  else error1 (sprintf "`@%s' expected between %d and %d arguments, but received %d." s min max got) pos

let error_char_overflow = error1 "you are giving a too large value: only character codes between 0 and 255 are accepted."
let error_int_overflow = error1 (Printf.sprintf "you are giving a too large integer literal: only values between %d and %d are accepted." min_int max_int)
let error_neither_ident_nor_call = error1 "the term is not a function call (the parenthesized expression is malformed). If it wasn't meant to be a function call, you need to separate the two expressions with spaces."
let error_redefinition_basic_type (ty,label) =
  let s = const_ty_to_string ty in
  error1 (sprintf "you are trying to redefine the basic type %s. If you really want that, use `%s` instead." s s) label
let error_bad_compare = error1 (sprintf "found a binding as condition. Did you mean `==' rather than `=' ?")
let error_static_xml pos =
  error1 "Directive `@xml' expects some static xml." pos
let error_consecutive_arrow pos =
  error1 "You have two consecutive arrows, this is a too ambiguous construct. If this is what you meant, add parentheses." pos
let error_sliced_expr =
  error1 "The directive @sliced_expr expects a static record with exactly the field server and the field client."
let error_db_file_without_slash (s, pos) = error1 (sprintf "the path to database files `%s' should contain a slash, e.g., './%s'" s s) pos
let error_conflicting_type_def_visibility (vis1, vis2, pos) =
   let string_of_visibility = function
     | SurfaceAst.TDV_abstract -> "@abstract"
     | SurfaceAst.TDV_private -> "@private"
     | SurfaceAst.TDV_public ->
         (* Since visibility "public" is the default and neutral element, it
            can't cause a conflict, hence an error. So it should never appear
            in such an error message. *)
         assert false in
   error1
     (sprintf
       "Type definition can't be specified %s and %s at the same time."
       (string_of_visibility vis1) (string_of_visibility vis2))
     pos

(*
 * Hints
 *)

let file_content_of_annot annot =
  let filename = FilePos.get_file annot.QmlLoc.pos in
  let start = FilePos.get_first_char annot.QmlLoc.pos in
  start, filename
type hint = [ `function_call of QmlLoc.annot | `declaration of QmlLoc.annot | `same_indent of (QmlLoc.annot * QmlLoc.annot) | `same_indents of QmlLoc.annot list ]
let hints = ref ([] : hint list)
let hints2 = ref ([] : hint list)
let push_hint v = hints := v :: !hints
let clear_hints () = hints2 := !hints; hints := []

let print_hints () =
  List.iter_right
    (function
       | `function_call pos -> warning1 (sprintf "there is only one space between the callable expression and the left parenthesis. It you wanted to call the expression, remove the space.") pos
       | `declaration annot ->
           let offset, _filename = file_content_of_annot annot in
           let _, column = offset2linecol offset in
           if column <> 0 then
             warning ~level:0 (sprintf "the declaration that begins at %s is indented. Did you forget a %s?\n" (offset2string offset) (isolate "do"))
       | `same_indent (annot1,annot2) ->
           (* FIXME: factorize with the following one *)
           let off1,file1 = file_content_of_annot annot1 in
           let off2,file2 = file_content_of_annot annot2 in
           assert (file1 = file2);
           let line1, col1 = offset2linecol off1 in
           let line2, col2 = offset2linecol off2 in
           if line1 <> line2 && col1 <> col2 then
             warning ~level:0 (sprintf "this local do/binding/open is not correctly indented:\nThe first part is at %s, the second at %s.\n" (offset2string off1) (offset2string off2))
       | `same_indents annots ->
           let offs,files = List.split (List.map file_content_of_annot annots) in
           match files with
             | [] -> ()
             | file :: t ->
                 assert (List.for_all ((=) file) t);
                 let rec aux ~resynchronize = function
                   | [] | [_] -> ()
                   | off1 :: off2 :: t ->
                       let line1, col1 = offset2linecol off1 in
                       let line2, col2 = offset2linecol off2 in
                       if line1 <> line2 then (
                         if col1 <> col2 then (
                           if not resynchronize then
                             warning ~level:0 (sprintf "two elements of a list of bindings are not correctly indented wrt each other:\nthe first part is at %s, the second at %s.\n%!" (offset2string off1) (offset2string off2));
                           aux ~resynchronize:true (off1 :: t)
                         ) else
                           aux ~resynchronize:false (off2 :: t)
                       ) else
                         aux ~resynchronize:false (off1 :: t)
                       in
                 aux ~resynchronize:false offs
    ) (!hints @ !hints2)

(*
 * Generation of fresh stuff
 *)

let fresh_id = SurfaceAstCons.Fresh.id



(* ************************************************************************** *)
(** {b Descr}: Generator for fresh *type* variables to be inserted by the
    parsing.
    ATTENTION, these type variables are labelled differently from those
    generated after parsign passe (i.e. during parsers transformation and
    typechecking) to avoid collision between them. In effect, if a source file
    didn't change, its parse is cached, hence the parser is not ran on it,
    hence the variable counter is not started and during parsers transformation
    and typechecking passes, type variables will get generated startign from 0.
    If unfortunatly such a variable gets created in the same scope than a
    previous one inserted during the parse, they will conflict. This is revealed
    by raising of type error since the 2 variable that should not have been
    related are related, hence unification may encounter incompatible
    instantiations for these 2 variables ... being in fact the same.          *)
(* ************************************************************************** *)
let fresh_variable = Fresh.fresh_factory (sprintf "opaparse_v_%d")



let fresh_name = SurfaceAstCons.Fresh.old_name


(*
 * Dealing with annotations
 *)

let annot pos = {
  QmlLoc.pos = pos ;
  QmlLoc.notes = fresh_id () ;
}

let builtin () = annot (FilePos.nopos "Parser_utils.builtin")
let pos_only filename start stop =
  let pos = FilePos.make_pos filename start stop in
  annot pos
let decorate filename result start stop =
  (result, pos_only filename start stop)

(**Remove annotation from a node.

   This is sometimes useful when a production needs to be split in several
   rules due to precedence issues, as some nodes would need end up annotated
   several times.*)
let undecorate (node, _label) = node
let label (_, label) : QmlLoc.annot = label
let copy_label label =
  {label with QmlLoc.notes = fresh_id ()}
let nlabel e = copy_label (label e)

let union_annot annot1 annot2 =
  let pos = FilePos.merge_pos_for_parser annot1.QmlLoc.pos annot2.QmlLoc.pos in
  {QmlLoc.pos = pos; QmlLoc.notes = fresh_id ()}
let union_annot2 t1 t2 = union_annot (label t1) (label t2)
let union_annot_list args =
  match args with
    | [] -> assert false
    | h :: _ -> union_annot2 h (List.last args)
let union_annot_list_snd args =
  match args with
    | [] -> assert false
    | (_,h) :: _ -> union_annot2 h (snd (List.last args))

let wrap f label = (f label, copy_label label)
let wrap_e f e = (f e, nlabel e)
let map_annot f (a,label) = (f a, label)


(*
 * Dealing with tuples and records, and what is encoded the same way in the AST
 * (function args, for instance)
 *)

(**
   Encode a tuple as a record.

   A tuple is represented as a list of [(name, value)], with type [string option * 'a].
   If [name] is [None], the user hasn't manually given a name to the field. We name the
   first such field [f1], the second one [f2], etc. If [name] is [Some x], we keep name
   [x]. At this stage, we do not check whether a name appears twice.
*)
let encode_tuple_as_record l =
  let rec aux i acc = function
    | []               -> List.rev acc
    | (None, typ)::t   -> aux (i + 1) ((sprintf "f%d" i, typ)::acc) t
    | (Some n, typ)::t -> aux i       ((n, typ)::acc) t
  in aux 1 [] l

let encode_tuple l : (string * _) list =
  let rec aux i acc = function
    | []               -> List.rev acc
    | typ::t   -> aux (i + 1) ((sprintf "f%d" i, typ)::acc) t
  in aux 1 [] l
let encode_record l : (string * _) list = l

let encode_tuple_pos args =
  assert (args <> []);
  (encode_tuple args, union_annot_list args)
let encode_record_pos args : (string * _) list QmlLoc.label =
  assert (args <> []);
  (encode_record args, union_annot_list_snd args)

(* transform a list of arguments/parameters into its ast representation *)
let encode_args_as_record = encode_tuple
let encode_args_as_record_pos = encode_tuple_pos




(*
 * Utils on identifiers
 *)

let var_to_patvar (ident, label) =
  (PatVar {ident=ident;directives=[]}, label)
let var_to_exprvar (ident, label) =
  (Ident ident, label)
let patident s label = (PatVar {ident=s;directives=[]}, label)
let patvar = patident
let ident s label = (Ident s, label)
let fresh_ident_pat label =
  let n = fresh_name () in
    (var_to_exprvar (n, copy_label label), var_to_patvar (n, copy_label label))


(*
 * Utils on types
 *)
let row_t_tuple ?rowvar tyl = let (l2,label) = encode_tuple_pos tyl in (TyRow (l2,rowvar), copy_label label)
let row_t_record ?rowvar styl = let (l2,label) = encode_record_pos styl in (TyRow (l2,rowvar), copy_label label)
let row_t tyl label = (TyRow (encode_tuple tyl,None), copy_label label)
let arrow_t_node tyl ty label = (row_t tyl label, ty)
let arrow_t tyl ty label = (arrow_t_node tyl ty label, copy_label label)
let arrow tyl ty label = TypeArrow (arrow_t_node tyl ty label)
let arrow2 ty1 ty2 label = arrow [ty1] ty2 label
let typenamed name args label =
  (TypeNamed (Typeident name, args), copy_label label)

(* FIXME: should be removed *)
let row_label ty label : 'a row_t = (TyRow (encode_tuple [ty], None), copy_label label)
let row ty e = row_label ty (label e)

let tuple_type ?rowvar (l : _ ty list) : _ ty =
  let (tyr,label) = row_t_tuple ?rowvar l in
  (TypeRecord tyr, label)
let record_type ?rowvar (l : (string * _ ty) list) : _ ty =
  let (tyr,label) = row_t_record ?rowvar l in
  (TypeRecord tyr, label)
let tuple_string l = Printf.sprintf "tuple_%d" (List.length l)
let tuple_name l label =
  typenamed (tuple_string l) [] label



(** Utils on type definitions *)
let merge_type_def_visibility l pos =
  List.fold_left
    (fun accu vis ->
      match (accu, vis) with
      | (SurfaceAst.TDV_public, _) ->vis
      | (_, SurfaceAst.TDV_public) -> accu
      | (SurfaceAst.TDV_private, SurfaceAst.TDV_private) ->
          SurfaceAst.TDV_private
      | (SurfaceAst.TDV_abstract, SurfaceAst.TDV_abstract) ->
          SurfaceAst.TDV_abstract
      | (_, _) -> error_conflicting_type_def_visibility (accu, vis, pos))
    SurfaceAst.TDV_public
    l

let merge_type_def_options ~global ~local =
  { local with Q.
      opacapi = local.Q.opacapi || global.Q.opacapi ;
  }


(*
 * Utils on built in type const
 *)
(* should be factorized with the one in surfaceAstConvert *)
let type_const t label = (TypeConst t, copy_label label)
let tyint label  = type_const TyInt label
let tyfloat label = type_const TyFloat label
let tystring label = type_const TyString label
let tyvoid_ = TypeRecord (TyRow ([], None))
let tyvoid label = (tyvoid_, copy_label label)



(*
 * Utils on type vars
 *)
let tyvar s label = ((TypeVar (Flatvar s)), copy_label label)
let fresh_tyvar label = tyvar (fresh_variable ()) label
let colvar s label = (SumVar (Colvar s), label)
let alpha label = tyvar "a" label
let beta label = tyvar "b" label




(*
 * useful types (for directives at least)
 *)
let tyxhtml label = typenamed Opacapi.Types.xhtml [] label
let tyxml label = typenamed Opacapi.Types.xml [] label
let tytext label = typenamed Opacapi.Types.text [] label
let tybool label = typenamed Opacapi.Types.bool [] label
let tylist ty label = typenamed Opacapi.Types.list [ty] label
let tyaction label = typenamed Opacapi.Types.Dom.transformation [] label
let tyopaty label = typenamed Opacapi.Types.OpaType.ty [] label
let tyoption label ty = typenamed Opacapi.Types.option [ty] label
let tycontinuation label ty = typenamed Opacapi.Types.continuation [ty] label

(*
 * Utils on type directives
 *)

let coerce e ty =
  Directive (`coerce, [e], [ty])
let coerce_expr e ty =
  (coerce e ty, nlabel e)
let coerce_pat p ty =
  (PatCoerce (p,ty), nlabel p)
let may_coerce_expr e o = Option.default_map e (coerce_expr e) o
let may_coerce_pat e o = Option.default_map e (coerce_pat e) o
let coerce_name e name =
  coerce e (typenamed name [] (label e))
let coerce_name_pat p name =
  coerce_pat p (typenamed name [] (label p))
let coerce_name_expr e name =
  (coerce_name e name, nlabel e)

(*
 * Utils on constants
 *)

let void_pat label = coerce_pat (PatRecord ([], `closed), copy_label label) (tyvoid label)
let void label = coerce_expr (Record [], copy_label label) (tyvoid label)
let true_pat label = PatRecord ([("true", void_pat label)], `closed), copy_label label
let true_ label = (Record [("true", void label)], copy_label label)
let false_pat label = PatRecord ([("false", void_pat label)], `closed), copy_label label
let false_ label = (Record [("false", void label)], copy_label label)
let string s label = (Const (CString s), copy_label label)
let string2 p = unc2 string p
let float2 (f,label) = (Const (CFloat f), copy_label label)
let float = cur2 float2
let int2 (i,label) = (Const (CInt (Big_int.big_int_of_int i)), copy_label label)
let int = cur2 int2
let floatint2 (i,label) = float2 (float_of_int i, label)
let intfloat2 (f,label) = int2 (int_of_float f, label)
let bool b label = if b then true_ label else false_ label

(*
 * More Utils on directives
 *)

let directive v le = Directive (v, le, [])
let directive0 v () = directive v []
let directive1 v e = directive v [e]
let directive1' v e = directive (v e) []
let directive2 v (e1, e2) = directive v [e1; e2]
let directive2' v (e1, e2) = directive (v e1) [e2]

let one_declaration_directive (((dir:parsing_directive),l,lt), label) bindings =
  List.map
    (fun (p,e) ->
       let args =
         match dir with
         | `specialize _ ->
             (*
               This directive likes to have its arguments after the final expresssion.
             *)
             e::l
         | _ -> l @ [ e ]
       in
       let e = (Directive (dir, args, lt), copy_label label) in
       (p,e)) bindings
let declaration_directive dirs bindings =
  List.fold_right one_declaration_directive dirs bindings


(*
 * Utils on functions
 *)

(* get the ident or field in a general call (ident, call dot composition) *)
let rec get_meaningful_called_ident (e,_) = match e with
  | Ident(id) -> id
  | Dot (_,field) -> field
  | Apply(f,_) -> get_meaningful_called_ident f
  | _ -> failwith "You are using autobind (i.e. ~) in an ambiguous way"


(* takes (arg1,arg2,...) and e and makes fun(arg1,arg2,...) -> e*)
let args_expr_to_lambda ?zero_ary args e =
  if args = [] then
    match zero_ary with
    | None -> e
    | Some label -> (Lambda ([], e), label)
  else
    let new_args = encode_args_as_record_pos args in
      (Lambda (undecorate new_args, e), union_annot2 new_args e)

(* puts a pos around the result *)
let (&) f args =
  if args = [] then
    f
  else
    let new_args = encode_args_as_record_pos args in
      (Apply (f, new_args), union_annot2 f new_args)

(* doesn't put a pos around the result *)
let (&.) f args =
  if args = [] then
    undecorate f
  else
    let new_args = encode_args_as_record_pos args in
      Apply (f, new_args)

let apply_f_with_holes f args : (_,_) expr =
  let args2,vars =
    let rec aux args vars = function
      | [] -> List.rev args, List.rev vars
      | h :: t ->
          match h with
            | `hole p ->
                let ident,patvar = fresh_ident_pat p in
                  aux (ident :: args) (patvar :: vars) t
            | `expr e ->
                aux (e :: args) vars t
    in
      aux [] [] args
  in
    if vars = [] then
      f & args2
    else
      args_expr_to_lambda vars (f & args2)

(* encode the 'function' construct of caml in opa AST *)
let function_ (l:(string SurfaceAst.pat * (string, 'a) SurfaceAst.expr) list) label =
  let var_expr,var_pat = fresh_ident_pat label in
    Lambda (encode_tuple [var_pat], (Match(var_expr, l), copy_label label))

let lambda_to_lambda (f: (_,_) expr -> (_,_) expr) label : (_,_) expr =
  let expr,pat = fresh_ident_pat label in
    args_expr_to_lambda [pat] (f expr)

let lambda s e =
  let p = var_to_patvar (s,label e) in
    Lambda (encode_tuple [p], e)

let rec dont_alias = function
  | (Ident _, _)
  | (Const _, _) -> true
  | (Dot (e,_), _) -> dont_alias e
  | _ -> false

(* takes a list of `expr expr or hole and gives you a list of expr and the
 * record corresponding to the fresh arguments
 * given [`expr 2, `hole], it gives you [2,x] and 1-uple (x,)
 *)
let hole_processing l =
  let rec aux acc1 acc2 acc3 = function
    | [] -> List.rev acc1, acc2, acc3 (* not reversed because it is will be concatenated in another accumulator *)
    | h :: t ->
        match h with
          | `expr e ->
              if dont_alias e then
                aux (e :: acc1) acc2 acc3 t
              else
                let n = fresh_name () in
                let ident = var_to_exprvar (n,label e) in
                aux (ident :: acc1) acc2 ((n,e) :: acc3) t
          | `hole p ->
              let (ident,patvar) = fresh_ident_pat p in
                aux (ident :: acc1) (patvar :: acc2) acc3 t in
    aux [] [] [] l

let global_hole_processing l =
  let rec aux acc1 acc2 acc3 = function
    | [] -> List.rev acc1, List.rev acc2, List.rev acc3
    | (h,pos) :: t ->
        match h with
          | `dot _
          | `double_dot _ as v -> aux ((v,pos) :: acc1) acc2 acc3 t
          | `function_call args ->
              let exprs,vars,bindings = hole_processing args in
                aux ((`expr exprs,pos) :: acc1) (vars @ acc2) (bindings @ acc3) t
  in
    aux [] [] [] l

let make_function letins double_dot vars e el bindings =
  let body =
    List.fold_left
      (fun acc -> function
         | (`dot i,pos) -> (Dot(acc,i), pos)
         | (`double_dot i,pos) -> (undecorate (double_dot acc i), pos)
         | (`expr el,pos) -> Apply (acc, (encode_tuple el,pos)), pos) e el in
  let func = args_expr_to_lambda vars body in
    undecorate (letins bindings func)

(*
 * applied to [f(_,1+2)], gives back:
 * [ let fresh = 1 + 2 in
 *   fresh2 -> f(fresh2,fresh) ]
 *)
let make_function2 letins double_dot e el =
  let exprs,vars,bindings =
    try
      List.map
        (fun e ->
           map_annot
             (function
                | `function_call a ->
                    `expr
                      (List.map
                         (function
                            | `hole _ -> raise Exit
                            | `expr v -> v
                         ) a)
                | `dot _ | `double_dot _ as v -> v) e) el, [], []
    with
      | Exit -> global_hole_processing el
  in
    match e with
      | `hole p ->
          let (ident,var) = fresh_ident_pat p in
          make_function letins double_dot (var :: vars) ident exprs bindings
      | `expr e ->
          make_function letins double_dot vars e exprs bindings
(*
 * Utils on operators
 *)

(* 1 + 2 + 3 -> parsed as -> (1,[+,2],[+,3] -> this function -> the correct tree of application *)
let extract_e = function
  | `expr e -> e
  | `hole p -> failwith (sprintf "At %s: `_' is not a valid expression: it has to be a direct argument of a function/operator" (FilePos.to_string p.QmlLoc.pos)) (* the parser only accepts holes when they are not alone, so this should not occur *)
let apply_operators associativity ((e1 : [ `expr of (string, 'a) SurfaceAst.expr | `hole of QmlLoc.annot ]),l) =
  match l with
    | [] -> extract_e e1
    | _ ->
        match associativity with
          | `nonassoc ->
              ( match l with
                  | [(op,e2)] -> apply_f_with_holes op [e1;e2]
                  | _ -> failwith "You are using a non associative operator in an ambiguous way" (* FIXME: should be understandable, but unused for now *)
              )
          | `left ->
              extract_e (List.fold_left (fun acc (op,e) -> `expr (apply_f_with_holes op [acc;e])) e1 l)
          | `right ->
              let (ops,es) = List.split l in
              let (new_es,new_e1) = List.extract_last (e1 :: es) in
              let new_l = List.combine ops new_es in
                extract_e (List.fold_right (fun (op,e) acc -> `expr (apply_f_with_holes op [e;acc])) new_l new_e1)




(*
 * Utils on datatype
 *)

let tuple_nocons l =
  let (l2,label) = encode_tuple_pos l in
  l2, copy_label label

let tuple l =
  let r = tuple_nocons l in
  Record (undecorate r), nlabel r

let record_nocons l =
  let (l2,label) = encode_record_pos l in
  l2, copy_label label

let record l =
  let r = record_nocons l in
  Record (undecorate r), nlabel r

let tuple_pat_nocons l =
  let (l2,label) = encode_tuple_pos l in
  l2, copy_label label

let tuple_pat l =
  let r = tuple_pat_nocons l in
  PatRecord (undecorate r, `closed), nlabel r

let record_pat_nocons l =
  let (l2,label) = encode_record_pos l in
  l2, copy_label label

let record_pat l =
  let r = record_pat_nocons l in
  PatRecord (undecorate r, `closed), nlabel r

let simple_record s label = Record [(s, void label)]
let record1 s e = (Record [(s,e)], nlabel e)
let simple_record_expr s label = wrap (simple_record s) label
let simple_record_expr2 = unc2 simple_record_expr

(*let list_pat_of_pat_list l label = List.fold_right list_cons_pat l (list_nil_pat label)*)

let some e  = coerce_name_expr (record1 "some" e) Opacapi.Types.option
let none label = coerce_name_expr (simple_record_expr "none" label) Opacapi.Types.option
let option_expr_of_expr_option o label =
  match o with
    | None -> none label
    | Some e -> some e

(*
 * Utils on constructs
 *)
let letins iel e =
  if iel = [] then e else (LetIn (false,iel,e), union_annot (union_annot_list_snd iel) (label e))
let letin i e1 e2 = letins [(i,e1)] e2
(* could do: let letinpat : pat -> expr -> expr -> expr qui fait "let pat = expr in expr" *)
let dot e s = (Dot (e,s), nlabel e)
let dot_path e path =
  List.fold_left dot e path
let dots l label =
  match l with
    | [] -> assert false
    | h :: t ->
        List.fold_left (fun acc s -> (Dot (acc,s), copy_label label)) (ident h (copy_label label)) t
let dots2 l =
  match l with
    | [] -> assert false
    | h :: t ->
        List.fold_left (fun acc (s2,pos2) -> (Dot (acc,s2), pos2)) (unc2 ident h) t
(* FIXME: redondant with (&) *)
let applys e el =
  match el with
    | [] -> e
    | _ -> let r = tuple_nocons el in (Apply (e, r), union_annot2 e r)
let apply e1 e2 = applys e1 [e2]


(*
 * Functions that are expected to be defined later
 *)

let append e1 e2 = ident "++" (nlabel e1) & [e1;e2]
let stringmap_empty label = dots ["StringMap";"empty"] label
let cssentrymap_empty label = dots ["Css_private";"Entry_map_empty"] label
let stringmap_add pos k v map = dots ["StringMap";"add"] pos & [k;v;map]
let cssentrymap_add pos k v map = dots ["Css_private";"Entry_map_add"] pos & [k;v;map]
let map_add_merge pos k data map = ident Opacapi.internal__add_css_entry pos & [k;data;map]

(*
 * Utils on patterns
 *)

let if_then_else e1 e2 o =
  let e3 = Option.default (void (label e2)) o in
    Match (e1, [(true_pat (label e2), e2);
                   (false_pat (label e3), e3);
               ])

let type_of_record label r =
  Cons.T.record ~label (List.map (fun (s,_) -> (s, fresh_tyvar label)) r)

let rec bind name acc = function
  | (PatConst _,label) as p ->
      `one (fresh_name (), Cons.E.match_ ~label (Cons.E.ident ~label name) [(p,Cons.E.void ~label ())]) :: acc
  | (PatAny,_) -> acc
  | (PatVar v,label) -> `one (v.ident, Cons.E.ident ~label name) :: acc
  | PatRecord (r, rowvar), label ->
      bind_aux_record label name acc rowvar r
  | (PatAs (p,v),label) -> bind name (`one (v.ident,Cons.E.ident ~label name) :: acc) p
  | (PatCoerce (p,ty),label) -> bind name (`one (fresh_name (), Cons.E.coerce ~label (Cons.E.ident ~label name) ty) :: acc) p
and bind_aux_record label name acc rowvar r =
  let bindings, block =
    List.fold_left_map (fun acc (s,p) ->
                      let n,p =
                        match p with
                          | (PatAs (p,s),_) -> s.ident,p
                          | (PatVar s,label) -> s.ident, Cons.P.any ~label ()
                          | _ -> fresh_name (), p in
                      let label = snd p in
                      bind n acc p, (n, Cons.E.dot ~label (Cons.E.ident ~label name) s)
                   ) [] r in
  let bindings = bindings @ (`list block :: acc) in
  if rowvar = `closed then
    (* coercions to make sure we don't have additional fields
     * [match x with ~{a} -> y] -> [a = x.a; y]
     * would work with x = {a b} when it shouldn't
     *)
    let typ = type_of_record label r in
    let n = fresh_name () in
    `one (n, Cons.E.coerce ~label (Cons.E.ident ~label name) typ) :: bindings
  else
    bindings

let create_letins ~label dirs l e2 =
  List.fold_right
    (fun v acc ->
       match v with
       | `one (s,e1) -> (LetIn (false, declaration_directive dirs [s,e1], acc), copy_label label)
       | `list l -> (LetIn (false, declaration_directive dirs l, acc), copy_label label)
    ) l e2

(* transforms [let (a,b) = e1 in e2] in
 * let fresh = e1 in
 * let a = fresh.f1 in
 * let b = fresh.f2 in
 * e2
 *)
let rec bind_in_to_expr_in dirs binding e2 =
  let (p,e1) = binding in
  undecorate (
    match p with
      | (PatVar v, label) ->
        assert( v.directives = [] );
        (LetIn (false,declaration_directive dirs [(v.ident,e1)],e2),copy_label label)
      | (PatAny, label) -> (LetIn (false,declaration_directive dirs [(fresh_name (),e1)],e2),copy_label label)
      | (PatCoerce (p,ty),label) -> (bind_in_to_expr_in dirs (p,Cons.E.coerce ~label e1 ty) e2,label)
      | (_,label) ->
          let n = fresh_name () in
          let bindings = `one (n,e1) :: List.rev (bind n [] p) in
          create_letins ~label dirs bindings e2
  )

let add_recval ~is_recval label (i,e) =
  if is_recval then
    (* avoid putting two @recval on the same binding *)
    let rec aux e =
      match e with
      | (Directive (`coerce, [e], [ty]), label) ->
          (Directive (`coerce, [aux e], [ty]), label)
      | (Directive (`recval, _, _),_) -> e
      | _ -> (Directive (`recval, [e], []), copy_label label) in
    (i, aux e)
  else
    (i,e)
let rec pat_in_to_simple_bindings_aux ~is_recval (p,e) =
  match p with
  | (PatVar v, _) -> assert( v.directives = [] );[(v.ident,e)]
  | (PatAny, _) -> [(fresh_name (),e)]
  | (PatCoerce (p,ty),label) -> pat_in_to_simple_bindings_aux ~is_recval (p,Cons.E.coerce ~label e ty)
  | (_, label) ->
      let n = fresh_name () in
      let bindings = `one (n,e) :: List.rev (bind n [] p) in
      List.concat_map
        (function
         | `one b -> [add_recval ~is_recval label b]
         | `list l -> List.map (add_recval ~is_recval label) l) bindings
let pat_in_to_simple_bindings (p,e) =
  let is_recval =
    match e with
    | (Directive (`recval, [_], []),_) -> true
    | _ -> false in
  pat_in_to_simple_bindings_aux ~is_recval (p,e)

let module_ e = directive1 `module_ e
let parser_ parser_ = directive0 (`parser_ parser_) ()
let open1 e1 e2 = directive2 `open_ (e1, e2)
let open1_pos e1 e2 =
  (open1 e1 e2, nlabel e1)
let open_ el e =
  List.fold_right open1_pos el e
let xml_parser xml_parser =
  directive0 (`xml_parser xml_parser) ()
let magic_to_string e =
  directive1 `magic_to_string e
let magic_to_xml e =
  directive1 `magic_to_xml e
let magic_to_text e =
  directive1 `magic_to_text e
let fun_action e =
  directive1 `fun_action e
let magic_do e = (directive1 `magic_do e , nlabel e)
let computed_string l = directive `string l

(*
 * stuff on list put here because they need coerce_name_expr
 *)
let list_nil label = coerce_name_expr (record [("nil",void label)]) Opacapi.Types.list
let list_nil_pat label = coerce_name_pat (record_pat [("nil",void_pat label)]) Opacapi.Types.list
let list_cons e1 e2 = coerce_name_expr (record [("hd",e1);("tl",e2)]) Opacapi.Types.list
let list_cons_pat e1 e2 = coerce_name_pat (record_pat [("hd",e1);("tl",e2)]) Opacapi.Types.list
let list_pat_of_pat_list ?tl l label : string pat =
  let tl = match tl with None -> list_nil_pat label | Some tl -> tl in
  List.fold_right list_cons_pat l tl
let list_expr_of_expr_list ?tl l label =
  let tl = match tl with None -> list_nil label | Some tl -> tl in
  List.fold_right list_cons l tl
let list_expr_of_expr_list_unsafe l =
  assert (l <> []);
  let pos = union_annot_list l in list_expr_of_expr_list l pos
(*
 * same as above with record stuff
 *)
let default_value_in_expr_record tilda f =
  let set_default =
    if tilda then
      fun i -> var_to_exprvar i
    else
      fun i -> void (label i) in
  List.map (function
              | `binding b -> b
              | `noassign (i, `value p, t) ->
                  (i,may_coerce_expr p t)
              | `noassign (i, `novalue i2, t) ->
                  (i, may_coerce_expr (set_default i2) t)) f
let default_value_in_pat_record tilda f =
  let default_value =
    if tilda then
      fun i -> var_to_patvar i
    else
      fun i -> void_pat (label i) in
  List.map (fun (ident,p,t) ->
              let p = match p with
                | `value p -> p
                | `novalue i -> default_value i in
              (ident, may_coerce_pat p t)) f
let default_value_in_type_record tilda f =
  let default_value =
    if tilda then
      fun i -> typenamed (undecorate i) [] (label i)
    else
      fun i -> tyvoid (label i) in
  List.map (fun (ident,p) ->
              let p = match p with
                | `value p -> p
                | `novalue i -> default_value i in
              (ident, p)) f

let list_constructors_of_string_list l lab =
  match l with
  | [] -> list_nil lab
  | _  -> list_expr_of_expr_list_unsafe (List.map simple_record_expr2 l)


(*
 * Utils for the toplevels
 *)
(* since we don't have directive for the toplevel,
 * open at toplevel is encoded as _ = @toplevel_open(e) *)
let toplevel_open e =
  NewVal ([((PatAny, nlabel e), (Directive (`toplevel_open, [e], []), nlabel e))],false)
let toplevel_opens el =
  List.map toplevel_open el

(*
 * Others
 *)

let bypass s = Bypass (BslKey.normalize s)
(*
 * [e..i] is transformed into [fresh = e
                               fresh.i(fresh)]
 *)
let double_dot e i =
  let name = fresh_name () in
  let fresh_e = var_to_exprvar (name, nlabel e) in
    letin name e (apply (dot fresh_e i) fresh_e)

(*
 * actions
 *)

let action _filename jqs val_css verb e : (_,_) expr_node =
  let a,f,others = undecorate val_css in
  let tl = Option.default [] others in
  let f =
    match f with
      | `identity -> (fun e -> e)
      | `magicToString -> (fun e -> (magic_to_string e, label val_css))
      | `magicToXml -> (fun e -> (magic_to_xml e, label val_css)) in
  let verb_s,verb_label = verb in
    if a = "css" && verb_s <> "set" then
      OManager.error "In file %s, there is an invalid operation (%s) for CSS. you can only set CSS." _filename verb_s;
    let record =
      record
        [("jq", jqs);
         ("subject", coerce_name_expr (record ((a, f e) :: tl)) Opacapi.Types.Dom.Transformation.subject);
         ("verb", ((simple_record verb_s verb_label), verb_label))]
    in
      coerce_name record Opacapi.Types.Dom.transformation

(*
 * xml
 *)

(**
   Reset default namespace to that of xhtml
*)
let around_xhtml expr =
  letin "xmlns:" (dot (ident "Xhtml" (nlabel expr)) "ns_uri") expr

(**
   Reset default namespace to that of xmlns, i.e. no namespace
*)
let around_xmlns expr =
  letin "xmlns:" (dot (ident "Xmlns" (nlabel expr)) "default_ns_uri") expr

type xml_or_xhtml =
  | Xml
  | Xhtml
let xml_stack = Stack.create ()
let push_xml x = Stack.push x xml_stack
let pop_xml () = ignore (Stack.pop xml_stack)
let xhtml_mode () = Stack.top xml_stack = Xhtml
let xml_typename () = if xhtml_mode () then Opacapi.Types.xhtml else Opacapi.Types.xml

let tag_stack = Stack.create ()
let push_tag s = Stack.push s tag_stack
let get_tag()  = Stack.top tag_stack
let pop_tag()  = ignore (Stack.pop tag_stack)

type 'b dom_tag_args = {
  (* Non-special attributes *)
  args : (string * string * (string, 'b) expr) list;

  (* Class names -- why is it special? *)
  class_ : (string, 'b) expr option;

  (* CSS styles -- special probably because we want to preprocess them *)
  style : (string, 'b) expr option;

  (* Namespace bindings of the form [xmlns:bar = "foo"] or [xmlns = "foo"] *)
  xmlns_declaration: (string(*prefix, possibly empty*) * (string, 'b) expr(*unique uri*)) list;

  events : ((string, 'b) expr) list;
  events_expr : (string, 'b) expr option;
  events_options: ((string, 'b) expr) list;
  href:    (string, 'b) expr option
} constraint 'b = [> `coerce ]

(* const -> string s label *)
(* list_append_opa -> append e1 e2 *)

let appendlo l o =
  match o with
    | None -> l
    | Some l2 -> append l l2
(* represents [CSS_build.v(args)] *)
let css_build v args : (_,_) expr =
  dot (ident "Css_build" (label v)) (undecorate v) & args
let css_build_unsafe s args : (_,_) expr =
  match args with
    | [] -> assert false
    | h :: _ -> css_build (s, nlabel h) args
let css_build1 s pos = css_build (s,pos) []
let css_build1' s e = css_build (s,nlabel e) []

let hyphen_to_underscore s = String.replace s "-" "_"
let map_tuple4 f (a,b,c,d) = (f a, f b, f c, f d)



let to_handle (name : string * QmlLoc.annot) : (_,_) expr  =
  coerce_name_expr (record [(undecorate name, void (label name))]) Opacapi.Types.Dom.Event.kind

let hassoc name value =
  coerce_name_expr (record [("name", to_handle name);  ("value", value)]) Opacapi.Types.handle_assoc

let hassoc_event name value =
  coerce_name_expr (hassoc name value) Opacapi.Types.event_handler

let empty_args _label = { args = [];
                         class_ = None;
                         style = None;
                         events = [];
                         events_options = [];
                         events_expr = None;
                         xmlns_declaration = [];
                         href = None
                       }

let is_empty_args {args=_; class_; style; events; events_options; events_expr; xmlns_declaration=_; href} =
  events = [] && events_options = [] && events_expr = None && href = None &&
  class_ = None && style = None

let arg_default (o,label) =
  match o with
    | None -> empty_args label
    | Some o -> o

let create_fragment l label =
  match l with
    | [h] -> h
    | l -> coerce_name_expr (record [("fragment", list_expr_of_expr_list l label)]) (xml_typename ())

let create_textnode str =
  coerce_name_expr (record [("text", string2 str)]) (xml_typename ())

(* if we have some spaces, create a textnode, except if we are at the end *)
let create_fragment_with_spaces (l,label) =
  let aux (o,n) =
    match o with
      | Some p -> [create_textnode p; n]
      | None -> [n] in
  let l2 = BaseList.concat_map aux l in
  match l2 with
  | [] -> assert false
  | [h] -> h
  | _ -> create_fragment l2 label

let create_empty_fragment label = create_fragment [] label

let empty_event_handler label =
  record ["value", (string "" label)]
(*  args_expr_to_lambda [PatAny, label] (void label)*)

let sassoc (ns,name) (value:(_,_) expr) : (_,_) expr =
  coerce_name_expr (record [("namespace", unc2 ident ns);
                            ("name", unc2 string name);
                            ("value", value)]) Opacapi.Types.Xml.attribute

let create_element (ns,tag) args children =
  (* Adapt tag and attributes *)
  let tag_ns : (string, 'a) SurfaceAst.expr =
    let (name, annot) = ns in
      ident ("xmlns:"^name) annot in (* Note: [name] can be empty, it's ok *)
  let other_attributes = List.map (fun (prefix, name, value) -> sassoc (("xmlns:"^prefix, nlabel tag), (name, nlabel tag)) value) args.args in

  (* Create element *)
  let record =
    if xhtml_mode () && not (is_empty_args args) then (
      let events         = list_expr_of_expr_list args.events         (nlabel tag)
      and events_options = list_expr_of_expr_list args.events_options (nlabel tag) in
      let class_ =
        match args.class_ with
        | None -> list_nil (label ns)
        | Some c -> c in
      let style =
        match args.style with
        | None -> list_nil (label ns)
        | Some s -> s in
      let specific_attributes =
        record [("class",class_);
                ("style",style);
                ("events",appendlo events args.events_expr);
                ("events_options", events_options);
                ("href", match args.href with
                 | None   -> coerce_name_expr (simple_record_expr "none" (nlabel tag)) Opacapi.Types.xhtml_href
                 | Some s -> s)
               ] in
      record [("namespace",tag_ns);
              ("tag",unc2 string tag);
              ("args", list_expr_of_expr_list other_attributes (label tag));
              ("specific_attributes", some specific_attributes);
              ("content",list_expr_of_expr_list children (label tag));
             ]
    ) else (
      record [("namespace",tag_ns);
              ("tag",unc2 string tag);
              ("args", list_expr_of_expr_list other_attributes (label tag));
              ("content",list_expr_of_expr_list children (label tag));
              ("specific_attributes", none (label tag));
             ]
    ) in


  (* Declare namespaces.
     Declaration [xmlns = "foo"] is converted into a binding <<`xmlns:` = foo>. *)
  let record = List.fold_left
    (fun record (prefix, uri) -> letin ("xmlns:"^prefix) uri record) record
    args.xmlns_declaration in

  coerce_name_expr record (xml_typename ())

let nstag_to_string ns tag =
    if ns = "" then tag else (sprintf "%s:%s" ns tag)

(* we allow any tags to be closed by </> thus, there is no error if tag2 = "" *)
let tag_mismatch ((ns1,_annot), (tag1,{QmlLoc.pos = pos1})) ((ns2,_annot), (tag2,{QmlLoc.pos = _pos2})) =
  let matched = (tag2 = "") || (tag1 = tag2 && ns1 = ns2) in
    if not matched then
      error1 (sprintf "and %s\n  Open and close tag mismatch <%s> vs </%s>"
                (FilePos.to_string pos1)
                (nstag_to_string ns1 tag1)
                (nstag_to_string ns2 tag2))
        _annot
    else ()

let add_arg src ((prefix,_),name) value =
  let old = arg_default src in
  { old with args = (prefix, fst name, Option.default (unc2 string name) value) :: old.args }




let add_event src name value =
  let name = match name with (s,pos) -> (String.lowercase s, pos) in
  let old = arg_default src in
    { old with events = (hassoc_event name value) :: old.events }
let add_event_option src name value =
  let name = match name with (s,pos) -> (String.lowercase s, pos) in
  let old = arg_default src in
    { old with events_options = (hassoc name value) :: old.events_options }
let add_events src value =
  let old = arg_default src in
    { old with events_expr = Some value }
let action_xml e =
  let e = coerce_name_expr e Opacapi.Types.FunAction.t in
  let e = wrap_e fun_action e in
  coerce_name_expr (record [("expr", e)]) Opacapi.Types.xhtml_event

let concat_xml_text l =
  let string = String.concat "" l in
  let buf    = Buffer.create (String.length string) in
  let _      = String.fold (fun cleaning c ->
      if cleaning then
        if Char.is_space c then true (*Discard char, carry on cleaning*)
        else
          begin
            Buffer.add_char buf c; (*Keep the char, stop cleaning*)
            false
          end
      else
        begin
          Buffer.add_char buf c; (*Keep char, no matter what*)
          c = '\n' || c = '\r'  (*If this is a newline, start cleaning*)
        end
  ) false string in
  Buffer.contents buf

(*
 * Css
 *)
let hexa2 (a,pos) (b,_) = int2 ((Char.hexa_value a) * 16 + Char.hexa_value b, pos)
let remove_percent = function
  | (Record [("percent",(Const (CInt i),_label2))], label) ->
      let i = Big_int.div_big_int (Big_int.mult_int_big_int 255 i) (Big_int.big_int_of_int 100) in
      (Const (CInt i), label)
  | e -> e
let rgba r g b a = record [("r", remove_percent r);("g", remove_percent g);("b", remove_percent b);("a", remove_percent a)]
let color_hexa a b c d e f g h =
  let r = hexa2 a b
  and g = hexa2 c d
  and b = hexa2 e f
  and a = hexa2 g h in
    rgba r g b a
let rgb r g b = rgba r g b (int2 (255,label b))

(*
 * inlined css
 *)

let conv (v,pos) =
  match v with
    | `left -> simple_record_expr "left" pos
    | `right -> simple_record_expr "right" pos
    | `center -> simple_record_expr "center" pos
    | `top -> simple_record_expr "top" pos
    | `bottom -> simple_record_expr "bottom" pos
    | `size s -> record1 "size" s

let background_position ((f,posf) as l) s =
  match (f,s) with
    | (`left|`right|`center|`size _), None ->
        conv l, conv (`center,posf)
    | (`top|`bottom), None ->
        conv (`center,posf), conv l
    | (`size _), Some ((`size _,_) as v) ->
        conv l, conv v
    | (`size _), Some v ->
        conv l, conv v
    | _, Some ((`size _,_) as v) ->
        conv l, conv v
    | _, Some ((c,_) as r) ->
        if List.mem f [`left;`right] || List.mem c [`top;`bottom] then
          conv l, conv r
        else
          conv r, conv l
let hyphen_to_underscore s = String.replace s "-" "_"
let css_build_with_prefix prefix (s,pos) = css_build1 (prefix^s) pos
let simple_record_hyphen2 (s,pos) =
  simple_record_expr (hyphen_to_underscore s) pos
let css_simple_record_hyphen2 (s,pos) =
  simple_record_expr (hyphen_to_underscore (if s = "none" then "css_none" else s)) pos

let list_style_def (s,pos) =
  match s with
    | "disc" | "square" -> simple_record_expr s pos
    | "decimal-leading-zero" -> record1 "decimal" (simple_record_expr "leading" pos)
    | "decimal" -> record1 "decimal" (simple_record_expr "not_leading" pos)
    | "lower-roman" -> record1 "roman" (simple_record_expr "lower_case" pos)
    | "upper-roman" -> record1 "roman" (simple_record_expr "upper_case" pos)
    | "lower-alpha" | "lower-latin" -> record1 "latin" (simple_record_expr "lower_case" pos)
    | "upper-alpha" | "upper-latin" -> record1 "latin" (simple_record_expr "upper_case" pos)
    | "lower-greek" -> simple_record_expr "greek" pos
    | _ -> assert false

(*---------------------------------*)
(*------- deep record update ------*)
(*---------------------------------*)

let find_and_remove p l =
  let rec aux acc = function
    | [] -> raise Not_found
    | h :: t ->
      if p h then h, List.rev_append acc t
      else aux (h :: acc) t in
  aux [] l

type tree = (string * QmlLoc.annot) * tree_aux
and tree_aux =
  | Leaf of (string, parsing_directive) expr
  | Node of tree list

let has_name ((s,_):(string*QmlLoc.annot)) (((s',_),_):tree) = s = s'

let make_path (sl,e) =
  let rec aux = function
    | [] -> assert false
    | [s] -> (s, Leaf e)
    | s :: sl -> (s, Node [aux sl]) in
  aux sl

let pp_pos_annotated f (s:(string * QmlLoc.annot)) =
  FilePos.pp_pos f ((snd s).QmlLoc.pos)

let add_path trees ((sl:(string * QmlLoc.annot) list),e) =
  let rec aux ~error_s (s,t_aux) sl =
    match t_aux with
    | Leaf _ ->
      OManager.serror "@[<v>%a@]@\n@[<2>  This record update tries to redefine a path.@\n(at %a)@]" pp_pos_annotated error_s pp_pos_annotated s;
      (match sl with
      | [] -> []
      | _ -> make_path (sl,e) :: [])
    | Node trees ->
      match sl with
      | [] ->
        OManager.serror "@[<v>%a@]@\n@[<2>  This record update tries to overwrite a path previously defined.@\n(at %a)@]" pp_pos_annotated error_s pp_pos_annotated s;
        trees
      | s :: sl ->
        auxs trees s sl
  and auxs trees s sl =
    try let tree, trees = find_and_remove (has_name s) trees in
        (s, Node (aux ~error_s:s tree sl)) :: trees
    with Not_found ->
      make_path (s :: sl,e) :: trees in
  match sl with
  | [] -> assert false
  | s :: sl ->
    auxs trees s sl

let make_record_tree (l : ((string * QmlLoc.annot) list * (_,_) expr) list) =
  List.fold_left add_path [] l

let rec rewrite_record_update_aux e (((s,annot),t_aux):tree) =
  match t_aux with
  | Leaf v -> v
  | Node ts -> rewrite_record_update (Dot (e,s), annot) ts
and rewrite_record_update (e:(_,_) expr) (ts:tree list) =
  let path, wrapper =
    match e with
    | (Ident _,_) -> e, (fun x -> x)
    | _ ->
      if List.for_all (function (_,Node _) -> false | _ -> true) ts then
        e, (fun x -> x)
      else
        let i = fresh_name () in
        (Ident i, nlabel e), (fun body -> (LetIn (false, [i, e], body), nlabel e)) in
  wrapper (
    (ExtendRecord (
       List.rev_map (fun (((s,_),_) as t) -> (s, rewrite_record_update_aux path t)) ts,
       path
     ),
     nlabel e
    )
  )

let rewrite_long_extend_record fields expr =
  let trees = make_record_tree fields in
  rewrite_record_update expr trees
