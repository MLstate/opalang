(*
    Copyright © 2011, 2012 MLstate

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

(* TODO remove *)
open SurfaceAst
let (|>) = InfixOperator.(|>)

(**)

let opt_of_exn exn f x = try Some (f x) with e when e = exn -> None
let exn_of_opt exn f x =
  match f x with
    | None -> raise exn
    | Some v -> v

(* functions wrapping expressions constructors *)
let apply (e,r) = Apply (e,r)
let lambda (p,e) = Lambda (p,e)

let const e = Const e
let string s = Const(CString s)
let int i = Const(CInt i)
let float f = Const(CFloat f)

let ident i = Ident i
let letin (b,iel,e) = LetIn (b,iel,e)
let match_ (e,pel) = Match (e,pel)
let record r = Record r
let extendrecord (r,e) = ExtendRecord (r,e)
let dot (e,s) = Dot (e,s)
let bypass b = Bypass b
let dbpath (a,b,c) = DBPath(a,b,c)
let directive (a,el,t) = Directive (a,el,t)

(* functions wrapping code_elt constructors *)
let database (ident,sl,ol) = Database (ident,sl,ol)
let newdbdef dd = NewDbDef dd
let newdbvalue (sl,ty) = NewDbDef(QmlAst.Db.Db_TypeDecl(List.map (fun x -> QmlAst.Db.Decl_fld x) sl, ty)) (* transitional *)
let newdbdefault (sl,e) = NewDbDef(QmlAst.Db.Db_Default(List.map (fun x -> QmlAst.Db.Decl_fld x) sl, e)) (* transitional *)
let newtype t = NewType t
let newval (pel,b) = NewVal (pel,b)

(* functions wrapping pat_node constructors *)
let patrecord r = PatRecord (r, `closed)
let patextendrecord r = PatRecord (r, `open_)
let patany = PatAny
let patconst c = PatConst c
let patvar s = PatVar s
let patcoerce (p,ty) = PatCoerce (p,ty)
let patas (p,s) = PatAs (p,s)

(* functions wrapping type_node constructors *)
let typeconst c = TypeConst c
let typevar v = TypeVar v
let typearrow a = TypeArrow a
let typerecord r = TypeRecord r
let typesumsugar sl = TypeSumSugar sl
let typenamed ti = TypeNamed ti
let typeexternal = TypeExternal
let typeforall t = TypeForall t
let typemodule l = TypeModule l

(* functions wrapping other type constructors *)
let flatvar a = Flatvar a
let tyrow (a,b) = TyRow (a,b)
let sumname a = SumName a
let sumrecord a = SumRecord a
let sumvar a = SumVar a

module Annot =
struct
  let map_annot f (a, b) = (a, f b)
  let map_node f (a, b) = (f a, b)
  let to_string annot = FilePos.to_string annot.QmlLoc.pos
  let to_string' (_, annot) = to_string annot
end

module Coerce = struct
  (* extracts 'foo' from '_ -> foo'
   * ie extracts 'foo' from the type of the directive representing '(a:foo)'
   *)
  let extract_coercion_type = function
    | Some ((_,t),_) -> t
    | _ -> assert false
  let extract_coercion_type_node t = fst (extract_coercion_type t)

  let rec remove ((e,_) as e') =
    match e with
      | Directive (`coerce, [e], _) ->
          remove e
      | Directive (`coerce, _, _) ->
          assert false
      | _ ->
          e'
  let uncoerce e =
    let rec aux ((e,label) as e') acc =
      match e with
        | Directive (`coerce as a, [e], [c]) ->
            aux e ((a,c,label) :: acc)
        | _ ->
            e', acc
    in
      aux e []
  let rec recoerce e = function
    | [] -> e
    | (a,c,label) :: t ->
        recoerce (Directive (a,[e],[c]), label) t
  let uncoerce_pat ?(pred=fun _ -> true) p =
    let rec aux ((p,label) as p') acc =
      match p with
        | PatCoerce (p,ty) when pred ty ->
            aux p ((ty,label) :: acc)
        | _ ->
            p', acc
    in
      aux p []
  let rec recoerce_pat p = function
    | [] -> p
    | (ty,label) :: t ->
        recoerce_pat (PatCoerce (p,ty),label) t
end

module Letin = struct
  let rec gather_aux e =
    let (e,coerces) = Coerce.uncoerce e in
      match fst e with
        | LetIn (b,iel,e) ->
            let (coerces2,iel2,e) = gather_aux e in
              coerces @ coerces2, (b,iel) :: iel2, e
        | _ ->
            coerces, [], e
  let gather e =
    let coerces, biell, e = gather_aux e in
    biell, Coerce.recoerce e coerces

  let rec unletin_before_renaming = function
      (* it is important to take care of one case matches because
       * let (a,b) = 2 in 3 is converted as match 2 with (a,b) -> 3 *)
    | (LetIn (_,_,e), _)
    | (Directive (`coerce, [e], _), _)
    | (Directive (`open_ , [_;e], _), _)
    | (Match (_, [(_,e)]),_) -> unletin_before_renaming e
    | (Directive ((`coerce | `open_), _, _),_) -> assert false
    | e -> e
  let rec unletin_aux acc = function
    | (LetIn (b,iel,e), p) -> unletin_aux (`letin (b,iel,p) :: acc) e
    | (Directive (`coerce as a, [e], c), d) -> unletin_aux (`coerce (a,c,d) :: acc) e
    | (Directive (`coerce,  _, _),_) -> assert false
    | (Match (e1, [(p2,e2)]),d) -> unletin_aux (`match_ (e1,p2,d) :: acc) e2
    | e -> e, acc
  let rec unletin_aux_for_deps acc = function
    (* not going through one case matches for module rewriting *)
    | (LetIn (b,iel,e), p) -> unletin_aux_for_deps (`letin (b,iel,p) :: acc) e
    | (Directive (`coerce as a, [e], c), d) -> unletin_aux_for_deps (`coerce (a,c,d) :: acc) e
    | (Directive (`coerce, _, _),_) -> assert false
    | e -> e, acc
  let unletin_for_deps e=
    unletin_aux_for_deps [] e
  let unletin e =
    unletin_aux [] e
end

module Record = struct
  let rec extract_fields (e,_) =
    match e with
      | Record r
      | Directive (`module_, [(Record r, _)], _) -> r
      | Directive (`coerce, [e], _) -> extract_fields e
      | Directive (`coerce, _, _) -> assert false
      | _ -> assert false (* FIXME *)
  let extract_fields_through_letin_before_renaming e =
    match Letin.unletin_before_renaming e |> fst with
      | Record r
      | Directive (`module_, [(Record r, _)], _) -> r
      | _ -> assert false (* FIXME *)
  let field_names r =
    List.map fst r
  let field_content r =
    List.map snd r
  let rec is_record (e,_) =
    match e with
      | Directive (`coerce, [e], _) -> is_record e
      | Directive (`coerce, _, _) -> assert false
      | Record _ -> true
      | _ -> false
  let rec is_record_or_module (e,_) =
    match e with
      | Record _
      | Directive (`module_, _, _) -> true
      | Directive (`coerce, [e], _) -> is_record_or_module e
      | Directive (`coerce, _, _) -> assert false
      | _ -> false
  let is_record_or_module_through_letin_before_renaming e =
    is_record_or_module (Letin.unletin_before_renaming e)
  let map_content f l =
    List.map (fun (k,v) -> (k, f v)) l

  (* CHECK: the fields may not be in the right order ? *)
  let is_tuple r =
    List.for_alli (fun i (s,_) -> s = "f" ^ string_of_int (i+1)) r && r <> []

  let get_tuple r =
    if is_tuple r then
      Some (List.map snd r)
    else
      None

  let rec is_module_before_renaming (e,_) =
    match e with
      | Directive (`module_, _, _) -> true
      | Directive (`coerce, [e], _) -> is_module_before_renaming e
      | Directive (`coerce, _, _) -> assert false
      | _ -> false
  let rec is_module e = SurfaceAstDecons.Look.module_local ~through:[SurfaceAstDecons.Remove.Basic.coerce] e
  let is_module_through_letin e =
    let e, _acc = Letin.unletin_for_deps e in
    is_module e

  (* through record extension and coerce *)
  let rec get_field_opt field e =
    match fst e with
      | Record l ->
          List.assoc_opt field l
      | ExtendRecord (l, e) ->
          ( match List.assoc_opt field l with
              | None -> get_field_opt field e
              | Some _ as v -> v
          )
      | Directive (`coerce, [e], _) ->
          get_field_opt field e
      | _ ->
          None
  let rec get_field field e =
    Option.get (get_field_opt field e)
  (* order of fields not specified if one field appears several times
   * if you say {{hd tl} with hd} then the field hd will be duplicated
   * FIXME
   *)
  let rec get_fields e =
    match fst e with
      | Record l -> List.map fst l
      | ExtendRecord (l, e) -> List.map fst l @ get_fields e
      | Directive (`coerce, [e], _) -> get_fields e
      | _ -> []
  let get_fields_filter fields e =
    List.map (fun field -> get_field field e) fields
  let has_field field e =
    Option.is_some (get_field_opt field e)

  let rec get_field_opt_p field p =
    match fst p with
      | PatRecord (l, _) ->
          List.assoc_opt field l
      | PatCoerce (p,_) ->
          get_field_opt_p field p
      | _ ->
          None

  let rec get_fields_p p =
    match fst p with
      | PatRecord (l, _) -> List.map fst l
      | PatCoerce (p,_) -> get_fields_p p
      | _ -> []

  let has_field_p field e =
    Option.is_some (get_field_opt_p field e)
end

module Basictype =
struct
  let get_string_opt e =
    match Coerce.remove e with
      | (Const (CString s), _) -> Some s
      | _ -> None
  let get_string x = exn_of_opt Exit get_string_opt x
end

module Datatype =
struct
  (** Lists *)
  (* what about extendrecord? *)
  let get_list e =
    let rec aux acc e =
      match fst e with
        | Record [("nil", _)] -> List.rev acc
        | Record [("hd",e1);("tl",e2)] -> aux (e1 :: acc) e2
        | Record [("tl",e1);("hd",e2)] -> aux (e2 :: acc) e1
        | Directive (`coerce, [e], _) -> aux acc e
        | _ -> raise Exit
    in
      aux [] e
  let get_list_opt e = opt_of_exn Exit get_list e

   (** Booleans *)
  let bool e = Record.has_field "true" e
  let bool_p e = Record.has_field_p "true" e
  let is_bool e =
    match Record.get_fields e with
      | ["true"] | ["false"] -> true
      | _ -> false
  let is_bool_p e =
    match Record.get_fields_p e with
      | ["true"] | ["false"] -> true
      | _ -> false
end

module Lambda = struct
  let is_lambda e =
    match Coerce.remove e with
      | (Lambda (_,_),_) -> true
      | _ -> false

  let rec collapse e =
    match e with
       | Lambda (l0,(Lambda(l1,e),_)),a -> collapse (Lambda (l0@l1,e),a)
       | _ -> e
end

module Rec = struct
  module D = SurfaceAstDecons
  let recursive_scope_before_renaming e =
    let e =
      D.Remove.remove
        ~through:[ D.Remove.Basic.access_directive
                 ; D.Remove.Basic.expand
                 ; D.Remove.Basic.opacapi
                 ; D.Remove.Basic.opavalue_directive
                 ; D.Remove.Basic.coerce
                 ; D.Remove.Basic.slicer_directive
                 ; D.Remove.Basic.letin
                 ; D.Remove.Basic.open_
                 ; D.Remove.Basic.async ] e in
    D.Look.module_ e || D.Look.lambda e

  let recursive_scope e =
    let e =
      D.Remove.remove
        ~through:[ D.Remove.Basic.access_directive
                 ; D.Remove.Basic.expand
                 ; D.Remove.Basic.opacapi
                 ; D.Remove.Basic.opavalue_directive
                 ; D.Remove.Basic.coerce
                 ; D.Remove.Basic.slicer_directive (* LOOK AGAIN: not sure *)
                 ; D.Remove.Basic.letin
                 ; D.Remove.Basic.async ] e in
    D.Look.module_ e || D.Look.lambda e
end

module TypeConst =
struct
  let type_of_node = function
    | CInt _ -> TyInt
    | CFloat _ -> TyFloat
    | CString _ -> TyString
  let type_of x = type_of_node (fst x)
end

module TypeRecord =
struct
  let field_names (TyRow (fields,_)) =
    List.map fst fields
  let get_tuple_length s =
    try
      Scanf.sscanf s "tuple_%d" (fun i -> if i > 0 then Some i else None)
    with
      | End_of_file
      | Scanf.Scan_failure _ -> None
  let is_tuple = function
    | (TypeNamed (Typeident s, _), _) ->
        Option.is_some (get_tuple_length (Ident.original_name s))
    | _ -> false
end

module Dot =
struct
  let app_to_dot e =
    let rec aux acc ((e,_) as e') =
      match e with
        | Dot (e, s) -> aux (s :: acc) e
        | Directive (`coerce, [e], _) -> aux acc e
        | _ -> e',acc
    in
      aux [] e

  let app_to_dot_for_renaming e =
    let rec aux acc ((e,_) as e') =
      match e with
        | Dot ((Directive (`toplevel,_,_),_),_) -> e', acc
        | Dot (e, s) -> aux (s :: acc) e
        | Directive (`coerce, [e], _) -> aux acc e
        | _ -> e',acc
    in
      aux [] e


  let app_to_dot_safe e =
    let rec aux acc (*acc_coerce*) ((e,label) as e') =
      match e with
        | Dot (e, s) -> aux ((s,label) :: acc) (*acc_coerce*) e
            (* FIXME? *)
            (*| Directive (`coerce, _, [e], r) -> aux acc (r :: acc_coerce) e*)
        | _ -> e', acc(*, acc_coerce*)
    in
      aux [] (*[]*) e

  let dot_to_app_safe e path =
    List.fold_left (fun e (s,label) -> (Dot (e, s), label)) e path

end
