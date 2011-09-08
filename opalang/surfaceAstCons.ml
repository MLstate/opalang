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
module List = BaseList

(* FIXME: open *)
open SurfaceAst

module Fresh =
struct
  (**Generate new fresh identifiers for annotations*)
  let id () =
    Fresh.Int.get () (* global ! *)

  (**Generate new type variables*)
  let typevar =
    Fresh.fresh_factory (Printf.sprintf "opa_v_%d")

  (* a name that shouldn't conflict with user defined ones *)
  let dontuse_fresh_ident =
    Fresh.fresh_factory (Printf.sprintf "__%d__")
  let old_name ?name () =
    let n = dontuse_fresh_ident () in
    match name with
    | None -> n
    | Some name -> n ^ "_" ^ name
  let name name =
    old_name ~name ()

  let ident ~descr ~label ~name =
    Ident.next ~filename:(FilePos.get_file label.QmlLoc.pos) ~descr name
end

module Label =
struct
  let label (_, label) : QmlLoc.annot = label
  let copy_label label = {label with QmlLoc.notes = Fresh.id ()}
  let undecorate (e,_label) = e
  let builtin () = {QmlLoc.pos = FilePos.nopos "SurfaceAstCons.Label.builtin" ; QmlLoc.notes = Fresh.id ()}
end

module ExprIdent =
struct
  type ident = Ident.t
  let equal = Ident.equal
  let typ s = OpaMapToIdent.typ s
  let val_ s = OpaMapToIdent.val_ s
  let fresh () = Ident.next "surfaceAstCons"
  let ns_fresh ~label name = Fresh.ident ~descr:"surfaceAstCons" ~label ~name
end

module StringIdent =
struct
  type ident = string
  let equal s s' = String.compare s s' = 0
  let val_ = Base.identity
  let typ = Base.identity
  let fresh = Fresh.typevar
  let ns_fresh ~label:_ s = s
end

let c = Label.copy_label
let encode_tuple l = List.mapi (fun i typ -> Printf.sprintf "f%d" (i+1), typ) l

let position_stack = Stack.create ()
(* making sure that in dev mode, we have Stack.empty exceptions when trying to
 * insert code without position
 * but in release mode, we put Builtin if we don't have any position *)
let () = if BuildInfos.is_release then Stack.push (FilePos.nopos "SurfaceAstCons.release_default_pos") position_stack

let private_with_position' position f x =
  Stack.push position position_stack;
  let r = try `value (f x) with e -> `exn e in
  ignore (Stack.pop position_stack);
  match r with `value r -> r | `exn e -> raise e
let private_with_position position thunk = private_with_position' position thunk ()

let builtin = FilePos.nopos "SurfaceAstCons.builtin"

let with_builtin_position thunk = private_with_position builtin thunk
let with_builtin_position' f x = private_with_position' builtin f x

let with_position' position f x =
  (*assert (position <> QmlLoc.NoPos && position <> QmlLoc.Builtin);*)
  private_with_position' position f x
let with_position position thunk = with_position' position thunk ()

let with_label label thunk = with_position label.QmlLoc.pos thunk
let with_label' label f x = with_position' label.QmlLoc.pos f x

let with_same_pos value thunk = with_label (Label.label value) thunk
let with_same_pos' value f x = with_label' (Label.label value) f x

module MakeCons(Ident : SurfaceAstConsSig.IDENT) =
struct
  open Ident
  type ident = Ident.ident
  type ident' = ident
  module I = Ident
  (* w is just a short name that shouldn't hide anything *)
  (* no need to use a fresh notes, since we will call copy_label on the
   * position *)
  let w () = {QmlLoc.pos = Stack.top position_stack ; QmlLoc.notes = -1 }
  (* the name is short to avoid the temptation of opening the module *)
  module T =
  struct
    let name ?(label=w()) ?(tyl=[]) s = TypeNamed (Typeident (typ s),tyl), c label
    let row_t_node ?row l = TyRow (l, row)
    let row_t ?(label=w()) ?row l = (row_t_node ?row l, c label)
    let args ?label l = row_t ?label (encode_tuple l)
    let arrow ?(label=w()) l ty = ((args ~label l, ty), c label)
    let arrow_1 ?label ty1 ty2 = arrow ?label [ty1] ty2
    let arrow_2 ?label ty1 ty2 ty3 = arrow ?label [ty1;ty2] ty3
    let typedef_node ?(tyvs=[]) visibility name ty : _ typedef_node =
      {
        SurfaceAst.ty_def_options = QmlAst.ty_def_options ;
        SurfaceAst.ty_def_visibility = visibility ;
        SurfaceAst.ty_def_name = Typeident name ;
        SurfaceAst.ty_def_params = tyvs ;
        SurfaceAst.ty_def_body = ty ;
      }
    let typedef ?(label=w()) ?tyvs visibility name ty : _ typedef =
      (typedef_node ?tyvs visibility name ty, c label)
    let record ?(label=w()) ?row l = (TypeRecord (row_t_node ?row l), c label)
    let tuple ?label ?row l = record ?label ?row (encode_tuple l)
    let typevar s = Flatvar s
    let var ?(label=w()) s = (TypeVar (Flatvar s), c label)
    let fresh ?label () = var ?label  (Ident.fresh ())
    let void ?(label=w()) () = (TypeRecord (row_t_node []), c label)
    let coerce ?(label=w()) e ty =
      (Directive (`coerce, [e], [ty]), c label)
    let coerce_name ?label e n = coerce ?label e (name ?label n)
    let external_ ?(label=w()) () = (TypeExternal, c label)
    let string ?(label=w()) () = (TypeConst TyString, c label)
    let int ?(label=w()) () = (TypeConst TyInt, c label)
    let float ?(label=w()) () = (TypeConst TyFloat, c label)
    let bool ?label () = name ?label Opacapi.Types.bool
  end

  module P =
  struct
    let any ?(label=w()) () = (PatAny, c label)
    let record ?(label=w()) ?(row=false) l =
      let rowvar = if row then `open_ else `closed in
      PatRecord (l, rowvar), c label

    let coerce ?(label=w()) p ty = (PatCoerce (p, ty), c label)
    let coerce_name ?label p name = coerce ?label p (T.name ?label name)

    let void ?(label=w()) () = coerce ~label (PatRecord ([], `closed), c label) (T.void ~label ())
    let simple_record ?label s = record ?label [(s,void ?label ())]
    let true_ ?label () = coerce_name ?label (simple_record ?label "true") Opacapi.Types.bool
    let false_ ?label () = coerce_name ?label (simple_record ?label "false") Opacapi.Types.bool
    let bool ?label b = if b then true_ ?label () else false_ ?label ()

    let ident ?(label=w()) ?(directives=[]) ident = (PatVar ({ident;directives}), c label)
    let var = ident

    let string ?(label=w()) s = (PatConst (CString s), c label)

    let record1 ?label s e = record ?label [(s,e)]
    let simple_record ?label s = record ?label [(s,void ?label ())]
    let tuple ?label l = record ?label (encode_tuple l)
    let tuple_2 ?label f1 f2 = coerce_name ?label (tuple ?label [f1;f2]) Opacapi.Types.tuple_2

    (* list *)
    let cons ?label p1 p2 = coerce_name ?label ((*Record.*)record ?label ["hd",p1;"tl",p2]) Opacapi.Types.list
    let nil ?label () = coerce_name ?label ((*Record.*)simple_record ?label "nil") Opacapi.Types.list
    let hd_tl ?label hd tl = coerce_name ?label ((*Record.*)record ?label ["hd",hd;"tl",tl]) Opacapi.Types.list
    let list ?label l = List.fold_right (cons ?label) l (nil ?label ())

    (* option *)
    let none ?label () = coerce_name ?label ((*Record.*)simple_record ?label "none") Opacapi.Types.option
    let some ?label p = coerce_name ?label ((*Record.*)record1 ?label "some" p) Opacapi.Types.option
  end

  module E =
  struct
    let record ?(label=w()) l = (Record l, c label)
    let coerce = T.coerce
    let coerce_name = T.coerce_name
    let void ?(label=w()) () = T.coerce ~label (Record [], c label) (T.void ~label ())
    let simple_record ?label s = record ?label [(s,void ?label ())]
    let true_ ?label () = T.coerce_name ?label (simple_record ?label "true") Opacapi.Types.bool
    let false_ ?label () = T.coerce_name ?label (simple_record ?label "false") Opacapi.Types.bool

    let constant ?(label=w()) const = (Const const, c label)
    let string ?label s = constant ?label (CString s)
    let float ?label f = constant ?label (CFloat f)
    let big_int ?label i = constant ?label (CInt i)
    let int ?label i = big_int ?label (Big_int.big_int_of_int i)
    let bool ?label b = if b then true_ ?label () else false_ ?label ()

    let ident ?(label=w()) i = (Ident i, c label)
    let var = ident

    (* record *)
    let record1 ?label s e = record ?label [(s,e)]
    let tuple ?label l = record ?label (encode_tuple l)
    let tuple_2 ?label e1 e2 = T.coerce_name ?label (tuple ?label [e1;e2]) Opacapi.Types.tuple_2
    let dot ?(label=w()) e s = (Dot (e,s), c label)
    let (<.>) = dot

    (* list *)
    let cons ?label e1 e2 = T.coerce_name ?label (record ?label ["hd",e1;"tl",e2]) Opacapi.Types.list
    let nil ?label () = T.coerce_name ?label (simple_record ?label "nil") Opacapi.Types.list
    let list ?label l = List.fold_right (cons ?label) l (nil ?label ())

    (* option *)
    let none ?label () = T.coerce_name ?label (simple_record ?label "none") Opacapi.Types.option
    let some ?label e = T.coerce_name ?label (record1 ?label "some" e) Opacapi.Types.option

    (* function *)
    let encode_args pl = encode_tuple pl

    (* abstraction *)
    let lambda ?(label=w()) pl e = (Lambda (encode_args pl, e), c label)
    let lambda_var ?label i e = lambda ?label [P.ident ?label i] e
    let lambda_ignore ?label e = lambda ?label [P.any ?label ()] e
    let lambda_void ?label e = lambda ?label [P.void ?label ()] e

    (* application *)
    let applys ?(label=w()) e l = (Apply (e, (encode_tuple l, c label)), c label)
    let apply ?label e1 e2 = applys ?label e1 [e2]
    let apply_void ?label e = apply ?label e (void ?label ())
    let apply2 ?label e1 e2 e3 = applys ?label e1 [e2;e3]

    let eta_expand ?(label=w()) arity e =
      let idents = List.init arity (fun i -> Ident.ns_fresh ~label (Printf.sprintf "eta_%d_%d" i arity)) in
      let pats = List.map (P.var ~label) idents in
      let exps = List.map (var ~label) idents in
      lambda ~label pats (applys ~label e exps)

    let match_ ?(label=w()) e pel = (Match (e, pel), c label)
    let if_ ?label e1 e2 e3 =
      match_ ?label (T.coerce_name ?label e1 Opacapi.Types.bool) [(P.true_ ?label (), e2);(P.false_ ?label (), e3)]
    let if_then ?label e1 e2 = if_ ?label e1 e2 (void ?label ())
    let if_not ?label e1 e3 = if_ ?label e1 (void ?label ()) e3

    (* often needed pattern matchings *)
    let match_opt ?label ?ty e pe1 pe2 =
      let tyl = Option.map (fun x -> [x]) ty in
      match_ ?label (T.coerce ?label e (T.name ?label ?tyl Opacapi.Types.option)) [pe1;pe2]
    let match_option ?(label=w()) ?ty e none some =
      let i = ns_fresh ~label "s" in
      match_opt ~label ?ty e (P.none ~label (), none) (P.some ~label (P.ident ~label i), some i)

    let letgen ?(label=w()) ~rec_ iel e = (LetIn (rec_,iel, e), c label)
    let letrec ?label iel e = letgen ?label ~rec_:true iel e
    let letand ?label iel e = letgen ?label ~rec_:false iel e
    let letin ?label i e1 e2 = letand ?label [(i,e1)] e2
    let letins ?label iel e = List.fold_right (fun (i,e) acc -> letin ?label i e acc) iel e

    let bypass ?(label=w()) s = Bypass (BslKey.normalize s), c label

  end

  (* directives *)
  module D =
  struct
    module T =
    struct
      module Common =
      struct
        let bool_arrow_void ?label () =
          T.arrow ?label [T.bool ?label ()] (T.void ?label ())
        let alpha_arrow_alpha ?label () =
          let v = T.fresh ?label () in
          T.arrow ?label [v] v
      end

      let static_source_content ?label () =
        T.arrow ?label [] (T.record [("modified", T.float ?label ());
                                     ("content", T.string ?label ())])
      let static_binary_content ?label () =
        T.arrow ?label [] (T.record [("modified", T.float ?label ());
                                     ("content", T.name ?label Opacapi.Types.binary)])
      let static_include_directory ?label () =
        T.arrow ?label [] (T.record [("modified", T.float ?label ());
                                     ("content", T.name ?label ~tyl:[T.string();T.string()] Opacapi.Types.stringmap)])
      let assert_message = Common.bool_arrow_void
      let ensure_message = Common.bool_arrow_void
      let doctype = Common.alpha_arrow_alpha
      let deprecated = Common.alpha_arrow_alpha
      let warning = Common.alpha_arrow_alpha
      let assert_ = Common.bool_arrow_void
      let client = Common.alpha_arrow_alpha
      let server = Common.alpha_arrow_alpha
      let no_client_calls = Common.alpha_arrow_alpha
      let ensure = Common.bool_arrow_void
      let fail ?label () = T.arrow ?label [T.string ?label ()] (T.fresh ?label ())
      let force = Common.alpha_arrow_alpha
      let private_ = Common.alpha_arrow_alpha
      let protected = Common.alpha_arrow_alpha
      let slicer = Common.alpha_arrow_alpha
      let translate ?label () = T.arrow ?label [T.string ?label ()] (T.name ?label ~tyl:[T.string ?label ()] "located")
      let unsafe_cast ?label () = T.arrow ?label [T.fresh ?label ()] (T.fresh ?label ())
      let spawn = Common.alpha_arrow_alpha
      let lazy_ = Common.alpha_arrow_alpha
      let magic_to_string ?label () = T.arrow ?label [T.fresh ?label ()] (T.string ?label ())
      let magic_to_xml ?label () = T.arrow ?label [T.fresh ?label ()] (T.name ?label Opacapi.Types.xml)
      let side_annotation = Common.alpha_arrow_alpha
      let visibility_annotation = Common.alpha_arrow_alpha
      let i18n_lang  ?label () = T.arrow ?label [] (T.string ?label ())
    end
    (*module Parser =
    struct
      type basic_reason =
        | Invalid_argument of string * int
        | Invalid_number_of_argument of int * int
        | Invalid_name of string
      type reason = QmlLoc.annot * basic_reason
      exception Invalid_directive of reason list
      let to_string (_:reason list) = assert false
      let directive ~label name l =
        let just_one l =
          match l with
          | [_] -> l
          | _ -> raise (Invalid_directive [label,Invalid_number_of_argument (1,List.length l)]) in
        match name with
          | "static_source_content" -> (Directive (`static_source_content, "@static_source_content", just_one l, Some T.static_source_content), c label)
          | "static_binary_content" -> (Directive (`static_binary_content, "@static_binary_content", just_one l, Some T.static_binary_content), c label)
          | "assert_message" -> (Directive (`assert_message, "@assert_message", just_one l, Some T.assert_message), c label)
          | "ensure_message" -> (Directive (`ensure_message, "@ensure_message", just_one l, Some T.ensure_message), c label)
          | "deprecated" -> (Directive (`deprecated, "@deprecated", just_one l, Some T.deprecated), c label)
    end*)
    let open_ ?(label=w()) e1 e2 =
      (Directive (`open_, [e1;e2], []), c label)
    let doctype (path:string list) ?(label=w()) ?(access=`public) e1 =
      (Directive (`doctype (path, access), [e1], []), c label)
    let string ?(label=w()) l =
      (Directive (`string, l, []), c label)
    let i18n_lang ?(label=w()) () =
      (Directive (`i18n_lang, [], []), c label)
    let side_annotation ?(label=w()) side e =
      (Directive (`side_annotation side, [e], []), c label)
    let visibility_annotation ?(label=w()) visibility e =
      (Directive (`visibility_annotation visibility, [e], []), c label)
    let static_content ?(label=w()) ?factory_helper eval e =
        (Directive (`static_content (e, eval), (match factory_helper with None -> [] | Some x -> [x]),[]),
        c label)
    let static_resource ?(label=w()) ?factory_helper e =
        (Directive (`static_resource e, (match factory_helper with None -> [] | Some x -> [x]),[]),
        c label)
    let server_entry_point ?(label=w()) e =
        (Directive (`server_entry_point,[e], []),
        c label)
    let with_thread_context ?(label=w()) ctx e =
      (Directive (`with_thread_context,[ctx;e], []),
      c label)

  end

  module C =
  struct
    let newval_pel ?(rec_=true) ?(label=w()) pel = (NewVal (pel,rec_), c label)
    let newval ?label ident e = newval_pel ~rec_:false ?label [(P.ident ?label ident, e)]
    let newvalrec ?label ident e = newval_pel ~rec_:true ?label [(P.ident ?label ident, e)]
    let newval_ignore ?label e = newval_pel ~rec_:false ?label [(P.any ?label (), e)]
    let newtype ?(label=w()) typedef = (NewType [typedef], c label)
  end

end

module ExprIdentCons = MakeCons(ExprIdent)
module StringCons = MakeCons(StringIdent)

module Fold =
struct
  let dot e acc =
    let rec aux e acc =
      match acc with
      | [] -> e
      | (h,label) :: t -> aux (ExprIdentCons.E.dot ~label e h) t in
    aux e acc
end

module Refresh =
struct
  let copy_label = Label.copy_label
  let const_expr_node c = c
  let const_expr (c,l) = (const_expr_node c, copy_label l)
  let rec record_node l = List.map (fun (i,e) -> (i, expr e)) l
  and record (r,l) = (record_node r, copy_label l)
  and expr (e,l) = (expr_node e, copy_label l)
  and expr_node = function
    | Apply (e,r) -> Apply (expr e, record r)
    | Lambda (prn, e) -> Lambda (pat_record_node prn, expr e)
    | Const c -> Const (const_expr_node c)
    | (Ident _ as v) -> v
    | LetIn (b,iel,e) -> LetIn (b,List.map (fun (i,e) -> (i,expr e)) iel, expr e)
    | Match (e,pel) -> Match (expr e, List.map (fun (p,e) -> (pat p, expr e)) pel)
    | Record r -> Record (record_node r)
    | ExtendRecord (r,e) -> ExtendRecord (record_node r, expr e)
    | Dot (e,s) -> Dot (expr e, s)
    | (Bypass _ as v) -> v
    | (DBPath _ as v) -> v
    | Directive (d,el,tyl) -> Directive (d, List.map expr el, List.map ty tyl)
  and pat (p,l) = (pat_node p, copy_label l)
  and pat_node = function
    | PatRecord (r, rowvar) -> PatRecord (pat_record_node r, rowvar)
    | PatAny -> PatAny
    | PatConst c -> PatConst (const_expr_node c)
    | PatVar _ as v -> v
    | PatCoerce (p,t) -> PatCoerce (pat p, ty t)
    | PatAs (p,s) -> PatAs (pat p, s)
  and pat_record_node l = List.map (fun (s, p) -> (s, pat p)) l
  and ty (t,l) = (ty_node t, copy_label l)
  and ty_node = function
    | TypeConst _
    | TypeExternal
    | TypeVar _ as v -> v
    | TypeArrow t -> TypeArrow (arrow_node t)
    | TypeRecord r -> TypeRecord (row_node r)
    | TypeSumSugar l -> TypeSumSugar (List.map sum l)
    | TypeNamed t -> TypeNamed (typeinstance_node t)
    | TypeForall (vars, t) -> TypeForall (vars, ty t)
    | TypeModule fields -> TypeModule (fields_t_node fields)
  and typeinstance (v,l) = (typeinstance_node v, copy_label l)
  and typeinstance_node (i,tyl) = (i,List.map ty tyl)
  and arrow (a,l) = (arrow_node a, copy_label l)
  and arrow_node (r,t) = (row r, ty t)
  and sum (s,l) = (sum_node s, copy_label l)
  and sum_node = function
    | SumName t -> SumName (typeinstance_node t)
    | SumRecord t -> SumRecord (row_node t)
    | SumVar _ as v -> v
  and fields_t_node l = List.map (fun (i,t) -> (i,ty t)) l
  and row (r,l) = (row_node r, copy_label l)
  and row_node (TyRow (f,o)) = TyRow (fields_t_node f, o)
  and typedef (a,l) = (typedef_node a, copy_label l)
  and typedef_node ty_def =
    { ty_def with SurfaceAst.ty_def_body = ty ty_def.SurfaceAst.ty_def_body }
end
