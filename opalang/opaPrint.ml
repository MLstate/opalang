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
(* FIXME: rebel open *)
open SurfaceAst

(* depends *)
module Format = Base.Format
module List = Base.List

(* shorthand *)
module S = SurfaceAst

(* refactoring in progress *)

(* -- *)

type 'ident printer =
  <
    code : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code LangPrint.pprinter;
  code_elt : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code_elt LangPrint.pprinter;
  code_elt_node : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code_elt_node LangPrint.pprinter;
  expr : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.expr LangPrint.pprinter;
  ty : 'ident SurfaceAst.ty LangPrint.pprinter;
  directive : 'dir. ('ident,[< SurfaceAst.all_directives ] as 'dir) SurfaceAst.directive LangPrint.pprinter;
  variant : 'dir. ([< SurfaceAst.all_directives ] as 'dir) LangPrint.pprinter;
  typevar : 'ident SurfaceAst.typevar LangPrint.pprinter;
  typeident : 'ident SurfaceAst.typeident LangPrint.pprinter;
  ident : 'ident LangPrint.pprinter;
  keyword : string -> bool
  >

module type Familly = sig
  val string : string printer
  val string_and_pos : string printer
  val ident : Ident.t printer
  val readable_ident : Ident.t printer
  val full_ident : Ident.t printer
end

type 'a pprinter = 'a Format.pprinter
let pp = Format.fprintf
let list = Format.pp_list

let string_complete_match r s =
  Str.string_match r s 0 &&
    (String.length s == Str.match_end ())

let regular_ident_regexp =    Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*" (* same as the parser *)
let regular_typeident_regexp = Str.regexp "[a-zA-Z_][a-zA-Z0-9_.]*"
let keyword = function
  | "match" | "with" | "type" | "do" | "if" | "then" | "else" | "as" | "_" | "<-" -> true
  | _ -> false
let basic_type = function
  | "int" | "string" | "float" -> true
  | _ -> false

let operator_regexp = Str.regexp "[-.+\\^*/<>=@|&!$]+"
let unary_minus = "unary_minus"
let unary_minus_float = "unary_minus_float"

let operator_image s =
  if s = unary_minus then "-"
  else if s = unary_minus_float then "-."
  else if s = "$" then "#"
  else s

let is_operator s =
  if s=unary_minus || s = unary_minus_float then true
  else string_complete_match operator_regexp s

let is_ident s =
  (string_complete_match regular_ident_regexp) s && not (is_operator s)

let classify_string keyword s =
  if keyword s then `backquote
  else if is_ident s then `ident
  else if is_operator s then `operator
  else `backquote

let classify_typeident s =
  if keyword s || basic_type s then `backquote
  else if is_ident s then `ident
  else `backquote

type ident_kind = [
| `ident
| `backquote
| `operator
]


module ExprSugar =
struct
  let extract_coercion = function
    | (Directive ((`coerce : [< all_directives ]), [e], [ty]), _) -> Some (e,ty)
    | _ -> None

  let rec sugar_list original_name acc e =
    match extract_coercion e with
    | Some (((Record ["hd",hd;"tl",tl],_) | (Record ["tl",tl;"hd",hd],_)),
            (TypeNamed (Typeident list,_),_)) when original_name list = "list" ->
        sugar_list original_name (hd :: acc) tl
    | Some ((Record ["nil",_],_),
            (TypeNamed (Typeident list,_),_)) when original_name list = "list" ->
        `list (List.rev acc, None) (* [1,2] *)
    | _ ->
        if acc = [] then `no_sugar else `list (List.rev acc, Some e) (* [1,2|e] *)

  let sugar_tuple original_name e =
    match extract_coercion e with
    | Some ((Record l,_), (TypeNamed (Typeident name,_),_)) ->
        (try
           let n = Scanf.sscanf (original_name name) "tuple_%d" (fun x -> x) in
           if n = List.length l && n <> 0 then
             let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
             if List.for_alli (fun i (f,_) -> f = "f"^string_of_int (i+1)) l then
               `tuple (List.map snd l)
             else
               `no_sugar
           else
             `no_sugar
         with
         | End_of_file
         | Scanf.Scan_failure _ -> `no_sugar)
    | _ -> `no_sugar

  let sugar_if = function
    | Match (e1,[
               ((PatRecord ([("true", _)], `closed), _)  | (PatCoerce ((PatRecord ([("true", _)], `closed), _), _), _)), e2 ;
               ((PatRecord ([("false", _)], `closed), _) | (PatCoerce ((PatRecord ([("false", _)], `closed), _), _), _)), e3 ;
             ]), _ -> `if_(e1,e2,e3)
    | _ -> `no_sugar


  let sugar_xhtml original_name e =
    match extract_coercion e with
    | Some (_, (TypeNamed (Typeident name,_),_)) when original_name name = "xhtml" -> `xhtml e
    | _ -> `no_sugar

  let sugar_dom_transformation original_name e =
    match extract_coercion e with
    | Some (_, (TypeNamed (Typeident name,_),_)) when original_name name = "Dom.transformation" -> `action e
    | _ -> `no_sugar

 let sugar_css_properties original_name e =
    match extract_coercion e with
    | Some (_, (TypeNamed (Typeident name,_),_)) when original_name name = "Css.properties" -> `css_props e
    | _ -> `no_sugar

  let rec select e l= match l with
    | [] -> `no_sugar
    | hd :: tl -> match hd e with
      | `no_sugar -> select e tl
      | r -> r


  let resugarer original_name e =
    select e [
      sugar_list original_name [];
      sugar_tuple original_name;
      sugar_if;
      sugar_xhtml original_name;
      sugar_dom_transformation original_name;
(*      sugar_css_properties original_name;  *)
    ]
end

module PatSugar =
struct
  let extract_coercion = function
    | (PatCoerce (p,ty),_) -> Some (p,ty)
    | _ -> None

  let rec sugar_list original_name acc p =
    match extract_coercion p with
    | Some (((PatRecord (["hd", hd ; "tl", tl], `closed), _) | (PatRecord (["tl", tl ; "hd", hd], `closed), _)),
            (TypeNamed (Typeident list,_),_)) when original_name list = "list" ->
        sugar_list original_name (hd :: acc) tl
    | Some ((PatRecord (["nil", _], `closed), _),
            (TypeNamed (Typeident list,_),_)) when original_name list = "list" ->
        Some (List.rev acc, None) (* [1,2] *)
    | _ ->
        if acc = [] then None else Some (List.rev acc, Some p) (* [1,2|e] *)

  let sugar_tuple original_name e =
    let record_tuple l =
      let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
      if List.for_alli (fun i (f,_) -> f = "f"^string_of_int (i+1)) l then
        Some (List.map snd l)
      else
        None
    in
    match e with
    | PatCoerce ((PatRecord (l, `closed), _), (TypeNamed (Typeident name,_),_)),_ ->
        (try
           let n = Scanf.sscanf (original_name name) "tuple_%d" (fun x -> x) in
           if n = List.length l && n <> 0 then
             record_tuple l
           else
             None
         with
         | End_of_file
         | Scanf.Scan_failure _ -> None)
    | PatRecord (l, `closed), _ -> record_tuple l
    | _ -> None

  let resugarer original_name p =
    match sugar_list original_name [] p with
    | Some (head,rest) -> `list (head,rest)
    | None ->
        match sugar_tuple original_name p with
        | Some l -> `tuple l
        | None -> `no_sugar
end

type all_directives = SurfaceAst.all_directives

let userland_visibilities_to_whatever ds =
  (ds : QmlAst.userland_visibility_directive list :> [> all_directives] list)

module Sugar = struct
  exception Fallback
  exception NotARecord

  let rec clear_directives e =  match fst e with
    | Directive(_,[e],_) -> clear_directives e
    | _ -> e

  let clear_magic e = match fst e with
    | Directive(`magic_to_xml,[e],_) -> e
    | _ -> e

  let rec fields_list e = match fst (clear_directives e) with
    | Record fields -> fields
    | _ -> raise NotARecord

  let dot_ f e =
    try List.assoc f (fields_list e)
    with Not_found -> failwith ("dot on "^f)

  let (@) e f = dot_ f e

  let is_string e =  match fst e with
    | Const(CString(_)) -> true
    | _ -> false

  let string e = match fst e with
    | Const(CString(s)) -> s
    | _ -> assert false

  let is_empty_string e = match fst e with
    | Const(CString(s)) -> s=""
    | _ -> false

  (* record pattern are never closed *)
  let iF ?notrecord e l =
    try
      let fs = (fields_list e) in
      try
        snd (
          Base.List.find (fun (pat,_) ->
            List.for_all (fun f -> List.mem_assoc f fs) pat
          ) l
        )
      with Not_found -> raise Fallback
    with NotARecord when notrecord <> None -> (Option.get notrecord)

  let is_nil e = iF e [
    ["nil"],(fun _ -> true);
    [],(fun _ -> false)
  ] e

  let rec list ~map e =
    iF e [
      ["hd";"tl"],(fun e -> (map (e@"hd")) :: (list ~map (e@"tl")));
      ["nil"]    ,(fun _e -> []);
    ] e

  let _1st_char e = FilePos.get_first_char (QmlLoc.pos (snd e))
  let cmp_attr (_,_,e1) (_,_,e2) = Pervasives.compare (_1st_char e1) (_1st_char e2)

  (* insert an expr into braces *)
  let pp_insert_expr ?(sugar=true) ppe f e =
    pp f "{%a}" (ppe sugar) e

  module String = struct

    (* insert an attribute into braces or dquote if needed *)
    let rec pp_expr_or_string ?(quote=true) ppe f e =
      match fst (clear_directives e) with
      | Const(CString(s)) when quote -> pp f "\"%s\"" (QmlPrint.escaped_string s)
      | Const(CString(s)) -> pp f "%s" (QmlPrint.escaped_string s)
      | Directive(`string,l,_) ->  pp_expr ppe f l
      | Directive((#all_directives : [< all_directives]) ,_,_)
      | _ -> pp_insert_expr ppe f e

    and string_elmt ppe f e = pp_expr_or_string ~quote:false ppe f e

    and pp_expr ppe f l = pp f "\"%a\"" (Format.pp_list "" (string_elmt ppe)) l
  end

  module Xhtml = struct

  let pp_namespace ppe f ns =
    if is_empty_string ns then pp f ""
    else pp f "@ namespace=%a" (String.pp_expr_or_string ppe) ns

  let str_empty = Const(CString("")),{QmlLoc.pos=FilePos.nopos "tmp";notes=0}

  let (++) = List.append

  let class_concat e = (* TODO computed string *)
    let l = list ~map:(fun x->x) e in
    if List.for_all is_string l then (
      Const(CString(Base.String.concat " " (List.map string l))),{QmlLoc.pos=FilePos.nopos "tmp";notes=0}
    ) else e


  (* xhtml specific *)
  let collect_spec_attr e =
    let class_ = e@"class" in
    let style  = e@"style" in
    let class_ = if is_nil class_ then [] else [str_empty,"class",class_concat class_] in
    let style  = if is_nil style  then [] else [str_empty,"style",style ] in
    let events = list ~map:(fun e->
      let event = e@"value" in
      str_empty,
      "on" ^ (fst (List.hd (fields_list (e@"name")))),
      iF event [
        ["expr"], (fun e -> clear_directives (e@"expr"));
        [],(fun e-> e);
      ] event
    ) (e@"events") in
    let events_options = list ~map:(fun e->
      str_empty,
      fst (List.hd (fields_list (e@"name")))^":options",
      e@"value"
    ) (e@"events_options") in
    let href =
      let e = e@"href" in
      iF e [
        ["none"],   (fun _ -> []);
        ["untyped"],(fun _ -> [str_empty,"href",e@"untyped"]);
        []         ,(fun _ -> [str_empty,"href",e])
      ] ()
    in
    List.flatten [class_;style;events;events_options;href]

  let collect_args = list ~map:(
    fun e -> (e@"namespace"),string (e@"name"),(e@"value")
  )

  let rec pp_tag ppe f e =
    let tag = string (e@"tag") in
    let pp_attributes f l =
      if l = [] then pp f "" else
      pp f "%a" (Format.pp_list "@ " (pp_attribute ppe)) l
    in
    let args = collect_args (e@"args") in
    let spec_att = e@"specific_attributes" in
    let spec_att = iF spec_att [
      ["none"],(fun _ -> []);
      ["some"],(fun e -> collect_spec_attr (e@"some"))
      ] spec_att
    in
    (* group all attribute and sort by position retrieve original order *)
    let attributes = args ++ spec_att in
    let attributes = List.sort cmp_attr attributes in
    let content = list ~map:(fun x->x) (e@"content") in
    (if content = [] then pp f "<%s%a%a/>" else pp f "@[<hov 2><%s%a%a>")
      tag
      (pp_namespace ppe) (e@"namespace")
      pp_attributes attributes;
    if content <> [] then pp f "%a@]</%s>"
      (Format.pp_list "" (pp_xml ppe)) content
      tag
    else ()

  and pp_attribute ppe f (ns,name,e) =
    if is_empty_string ns then
      pp f "@ %s=%a"
        name
        (String.pp_expr_or_string ppe) e
    else
      pp f "@%a:%s=%a"
        (String.pp_expr_or_string ~quote:true ppe) ns
        name
        (String.pp_expr_or_string ppe) e

  and pp_fragment ?(enclosed=true) ppe f e =
    let l = list ~map:(fun x->x) (e@"fragment") in
    if l = [] then (
      if enclosed then pp f ""
      else pp f "<></>"
    ) else pp f "@[<hov 1><>@,%a</>@]" (Format.pp_list "@," (pp_xml ppe)) l

  and pp_text ?(enclosed=true) _ppe f e =
    (if enclosed then pp f "%s"
     else pp f "<>%s</>") (string (e@"text"))

  and pp_xml ?(enclosed=true) (ppe : bool -> ('a,'b) expr pprinter) f e = iF e [
    ["namespace";"tag";"args";"content";"specific_attributes"] , pp_tag;
    ["fragment"] , pp_fragment ~enclosed;
    ["text"] , pp_text ~enclosed;
    [] , (fun ppe f e -> ppe false f e)
  ] ~notrecord:(fun ppe f e -> if enclosed then pp_insert_expr ppe f (clear_magic e) else ppe false f e)
    ppe f e


  let pp_xml : (bool -> ('ident,[< all_directives ] as 'dir) expr pprinter) ->  ('ident,[< all_directives ] as 'dir) expr pprinter
    =
    fun ppe f e -> pp_xml ~enclosed:false ppe f e

  let magie = Obj.magic pp_xml

  let pp_xml : 'dir. (bool -> ('ident,[< all_directives ] as 'dir) expr pprinter) ->  ('ident,[< all_directives ] as 'dir) expr pprinter
      = magie (* Obj.magic pp_xml WTF . *)

  end

  module Action = struct

    let fallback ppe f e _ = ppe false f e

    let pp_dst original_name ppe e =
      match clear_directives e with
      | Apply((Ident(id),_),([_,e],_)),_  ->
        let op = match original_name id with
          | "Dom_select_id" -> "#"
          | "Dom_select_class" -> "."
          | _ ->  raise Fallback
        in
        fun f () ->
          pp f "%s%a" op (String.pp_expr_or_string ~quote:false ppe) e
      | _ -> raise Fallback

    let str s f () = pp f "%s" s

    let pp_op e =
      iF e [
        ["set"]    , str "=";
        ["prepend"], str "+=";
        ["append"] , str "=+"
      ]

    let pp_content ppe f e =
      let fallback = fallback ppe f e in
      iF e [
        ["content"], (fun ()-> ppe true f (clear_magic (e@"content")));
        [], fallback
      ] ~notrecord:fallback ()

    let pp original_name ppe f e =
      let fallback = fallback ppe f e in
      try
      iF e [
        ["jq";"subject";"verb"], (fun () ->
          (* TODO op before start printing *)
          let pp_op =  pp_op (e@"verb") in
          let pp_dst = pp_dst original_name ppe (e@"jq") in
          pp f "%a %a %a"
            pp_dst ()
            pp_op ()
            (pp_content ppe) (e@"subject")
        );
        [],  fallback
      ] ()
      with Fallback -> fallback ()


    type ('ident,'dir) printer = ('ident -> string) ->
        (bool -> ('ident,[< all_directives ] as 'dir) expr pprinter) ->  ('ident,[< all_directives ] as 'dir) expr pprinter
    let magie = Obj.magic (pp:('ident,'dir) printer)

    let pp :  'dir. ('ident,'dir) printer = magie

  end

  module Css = struct
    let pp ppe f e = ppe f e
  end

end






module Classic = struct
  class virtual ['ident] printer =
  object (self)
    (* priorities
     *
     *)
    val comma = false
    val forall = false
    val typesum = false
    val arrow = false
    val colon = false
    val op = false
    method under_typesum = {< typesum = true >}
    method under_comma = {< comma = true >}
    method private under_forall = {< forall = true >}
    method private under_arrow = {< arrow = true >}
    method under_colon = {< colon = true >}
    method private under_op = {< op = true >}
    method private reset = {< typesum = false;
                      comma = false;
                      forall = false;
                      arrow = false;
                      colon = false;
                      op = false >}

    method keyword = function
    | "match" | "with" | "type" | "do" | "if" | "then" | "else" | "as" | "_" | "<-" -> true
    | _ -> false

    method label : 'a. 'a pprinter -> 'a QmlLoc.label pprinter = fun p f v -> p f (fst v)

    method private const_expr f = function
      | CInt i -> Format.pp_print_string f (Big_int.string_of_big_int i)
      | CFloat float -> Format.pp_print_float f float
      | CString s -> pp f "\"%s\"" (QmlPrint.escaped_string s)

    method private field f s =
      Format.pp_print_string f (
        match classify_string self#keyword s with
        | `ident -> s
        | `operator | `backquote -> "`"^s^"`"
      )

    (*------------------------*)
    (*----- type printer -----*)
    (*------------------------*)
    method ty f ty = self#label self#ty_node f ty
    method ty_node f = function
      | TypeSumSugar _ as ty when typesum -> pp f "(%a)" self#reset#ty_node ty
      | TypeSumSugar _ as ty when forall -> pp f "(%a)" self#reset#ty_node ty
      | TypeArrow _ as ty when arrow -> pp f "(%a)" self#reset#ty_node ty
      | (TypeSumSugar _ | TypeArrow _ | TypeForall _) as ty when comma -> pp f "(%a)" self#reset#ty_node ty
      | TypeConst c -> self#const_ty_node f c
      | TypeVar v -> self#typevar f v
      | TypeArrow r -> self#arrow_t_node f r
      | TypeRecord r -> self#row_t_node f r
      | TypeSumSugar l -> self#typesumsugar f l
      | TypeNamed t -> self#typeinstancenode f t
      | TypeExternal -> Format.pp_print_string f "external"
      | TypeForall p -> self#typeforall f p
      | TypeModule r -> self#typemodule f r

    method private const_ty_node f const =
      Format.pp_print_string f (
        match const with
        | TyInt -> "int"
        | TyFloat -> "float"
        | TyString -> "string"
      )
    method private rowvar f _ = Format.pp_print_string f "..."
    method private colvar f _ = Format.pp_print_string f "..."
    method virtual typeident : 'ident typeident pprinter
    method virtual typevar : 'ident typevar pprinter
    method virtual typeident_original_name : 'ident -> string
    method private row_t f x = self#label self#row_t_node f x
    method private row_t_node f (TyRow (nodes, rowo)) =
      match nodes, rowo with
      | [], None -> Format.pp_print_string f "{}"
      | [], Some rowvar -> pp f "{%a}" self#rowvar rowvar
      | l, None -> pp f "@[<1>{%a}@]" (list ";@ " self#typerecordbinding) l
      | l, Some rowvar -> pp f "@[<1>{%a;@ %a}@]" (list ";@ " self#typerecordbinding) l self#rowvar rowvar
    method private typerecordbinding f (s,ty) =
      match ty with
      | (TypeRecord (TyRow ([],None)),_) -> Format.pp_print_string f s
      | _ -> pp f "%a: %a" self#field s self#reset#ty ty
    method sum_t f v = self#label self#sum_t_node f v
    method private sum_t_node f = function
      | SumName typeinstance -> self#typeinstancenode f typeinstance
      | SumRecord r -> self#row_t_node f r
      | SumVar c -> self#colvar f c
    method private arrow_t f v = self#label self#arrow_t_node f v
    method private arrow_t_node f (row_t,ty) =
      pp f "@[<2>%a ->@ %a@]" self#under_comma#arrow_row_t row_t self#under_arrow#ty ty
    method arrow_row_t f v = self#label self#arrow_row_t_node f v
    method private arrow_row_t_node f (TyRow (l,row)) =
      assert (row = None);
      list ",@ " self#arrowbinding f l
    method private arrowbinding f (_,ty) = self#ty f ty
    method private typeinstance f t = self#label self#typeinstancenode f t
    method private typeinstancenode f (ident,params) =
      match params with
      | [] -> self#typeident f ident
      | _ -> pp f "@[@[<2>%a(%a@])@]" self#typeident ident (list ",@ " self#under_comma#ty) params
    method private typeforall f (vars,ty) =
      pp f "@[<2>forall(@[<h>%a@]) %a@]" (list ",@ " self#typevar) vars self#under_forall#ty ty
    method private typesumsugar f l =
      pp f "@[<v>  %a@]" (list "@ / " self#under_typesum#sum_t) l
    method private typemodule f fields =
      pp f "@[@{<v2>{{@ %a@ @]}}@]" (list "@ " self#typerecordbinding) fields

    (*-------------------------*)
    (*---- pattern printer ----*)
    (*-------------------------*)
    method private is_tilde_field : 'a. ('a -> 'ident option) -> string * 'a -> bool =
      (fun getvar (field, pat) ->
         match getvar pat with
         | Some ident ->
             let ident = self#to_protected_ident ident in
             String.compare field ident = 0
         | None -> false
      )

    method pat f p =
      match PatSugar.resugarer self#typeident_original_name p with
      | `list (head,None) -> pp f "@[<2>[%a]@]" (list ",@ " self#pat) head
      | `list (head,Some rest) -> pp f "@[<2>[%a@ |@ %a]@]" (list ",@ " self#pat) head self#pat rest
      | `tuple pl -> pp f "@[<1>(%a)@]" (list ",@ " self#pat) pl
      | `no_sugar -> self#label self#pat_node f p
    method pat_node f = function
      | PatCoerce _ as p when arrow || comma || colon -> pp f "(%a)" self#reset#pat_node p
      | PatRecord (fields, rowvar) -> self#pat_record f fields rowvar
      | PatAny -> pp f "_"
      | PatConst c -> self#const_expr f c
      | PatVar v -> pp f "%a %a" self#pat_directives  (userland_visibilities_to_whatever v.directives) self#ident v.ident
      | PatCoerce (p,ty) -> pp f "%a : %a" self#under_colon#pat p self#ty ty
      | PatAs (p,i) -> pp f "%a %a as %a" self#pat_directives (userland_visibilities_to_whatever i.directives) self#pat p self#ident i.ident

    method private pat_record_binding f ((s, p) as pat) =
      match p with
      | PatCoerce ((PatRecord ([], _), _), (TypeRecord (TyRow ([], None)), _)), _ -> self#field f s
      | _ ->
          let getvar = function
            | S.PatVar v, _ -> Some v.ident
            | _ -> None
          in
          if self#is_tilde_field getvar pat
          then
            pp f "~%a" self#field s
          else
            pp f "%a =@ %a" self#field s self#pat p

    method private pat_record f fields rowvar =
      match fields with
      | [] ->
          if rowvar = `open_
          then
            Format.pp_print_string f "{ ... }"
          else
            Format.pp_print_string f "{}"
      | _ ->
          let rowvar =  if rowvar = `open_ then " ; ..." else "" in
          let is_tilde_field field =
            let getvar = function
              | S.PatVar v, _ -> Some v.S.ident
              | _ -> None
            in
            self#is_tilde_field getvar field
          in
          if List.for_all is_tilde_field fields
          then
            let pp_field f (field, _) = self#field f field in
            pp f "@[<hv2>~{ %a%s }@]"
              (Format.pp_list "@, " pp_field) fields
              rowvar
          else
            pp f "@[<hv2>{ %a%s }@]"
              (Format.pp_list " ;@ " self#pat_record_binding) fields
              rowvar


    (*-------------------------*)
    (*----- expr printer ------*)
    (*-------------------------*)
    method virtual is_operator : 'ident -> bool
    method virtual to_protected_ident : 'ident -> string
    method virtual to_unprotected_ident : 'ident -> string
    method ident f i = Format.pp_print_string f (self#to_protected_ident i)
    method private unprotected_ident f i = Format.pp_print_string f (self#to_unprotected_ident i)
    method private expr_sugar : 'dir . bool -> ('ident,[< all_directives ] as 'dir) expr pprinter = fun sugar f e ->
      if not sugar then self#label self#expr_node f e
      else  match ExprSugar.resugarer self#typeident_original_name e with
      | `list (head,None) -> pp f "@[<2>[%a]@]" (list ",@ " self#expr) head
      | `list (head,Some rest) -> pp f "@[<2>[%a@ |@ %a]@]" (list ",@ " self#expr) head self#expr rest
      | `tuple el -> pp f "@[<1>(%a)@]" (list ",@ " self#expr) el
      | `if_ (e1,e2,e3) -> pp f "@[<2>if %a@;<1 -2>then@ %a@;<1 -2>else@ %a@]" self#expr e1 self#expr e2 self#expr e3
      | `xhtml  e -> Sugar.Xhtml.pp_xml self#expr_sugar f e
      | `action e -> Sugar.Action.pp self#to_unprotected_ident self#expr_sugar f e
      | `no_sugar -> self#label self#expr_node f e
    method expr : 'dir . ('ident,[< all_directives ] as 'dir) expr pprinter  = self#expr_sugar true
    method expr_node : 'dir. ('ident,[< all_directives ] as 'dir) expr_node pprinter = fun f -> function
        (*| Directive ((`coerce:[< all_directives]),_,_) as e when comma -> pp f "(%a)" self#expr_node e*)
      | Lambda _ | Match _ | LetIn _ as e when op -> pp f "(%a)" self#reset#expr_node e
      | Lambda _ as e when comma -> pp f "(%a)" self#reset#expr_node e
      | Apply ((Ident oper,_LABEL1),([(_,e1);(_,e2)],_LABEL2)) as e when self#is_operator oper ->
          if op || colon then pp f "(%a)" self#reset#expr_node e else
            pp f "@[<2>%a %s@ %a@]" self#under_op#expr e1 (self#to_unprotected_ident oper) self#under_op#expr e2
      | Apply (e,(r,_LABEL)) -> pp f "@[<2>%a(%a)@]" self#apply_expr e (list ",@ " (fun f (_,e) -> self#reset#under_comma#expr f e)) r
      | Lambda (r,e) -> pp f "@[<2>%a ->@ %a@]" (list ",@ " (fun f (_,p) -> self#under_comma#pat f p)) r self#expr e
      | Const c -> self#const_expr f c
      | Ident ident -> self#ident f ident
      | LetIn (true,iel,e) ->
          pp f "@[<v>@[<2>rec %a@]@ %a@]" (list "@]@ and @[" self#binding) iel self#expr e
      | LetIn (false,[bnd],e) ->
          pp f "@[<v>@[<2>%a@]@ %a@]" self#binding bnd self#expr e
      | LetIn (false,iel,e) ->
          pp f "/* encoded let and */@\n@[<v>%a@ %a@]" self#bindings iel self#expr e
      | Match (e,pel) ->
          pp f "@[<v>match %a with@ " self#reset#expr e;
          Format.pp_print_if_newline f ();
          Format.pp_print_string f "| ";
          pp f "%a@ end@]" (list "@ | " self#rule_) pel
      | Record fields -> self#record f fields
      | ExtendRecord (r,e) ->
          pp f "@[<1>{%a with@ %a}@]" self#expr e (list ";@ " self#record_binding) r
      | Dot (e,s) -> pp f "%a.%a" self#apply_expr e self#field s
      | Bypass s -> pp f "%%%%%s%%%%" (BslKey.to_string s)
      | DBPath (elt,kind) -> pp f "%a" (QmlAst.Db.pp_path self#expr) (List.map fst (fst elt), kind)
      | Directive d -> self#directive f d
    method private apply_expr : 'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f -> function
      | (Ident _,_)
      | (Apply _,_)
      | (Directive _ ,_)
      | (Dot _, _) as e -> self#expr f e
      | e -> pp f "(%a)" self#reset#expr e
    method private db_elt : 'dir. ('ident,[< all_directives ] as 'dir) dbelt pprinter = fun f e ->
      self#label self#db_elt_node f e
    method private db_elt_node : 'dir. ('ident,[< all_directives ] as 'dir) dbelt_node pprinter = fun f l ->
      list "" self#db_element f l
    method private db_element : 'dir. ('ident,[< all_directives ] as 'dir) preprocessed_db_element pprinter = fun f e ->
      self#label self#db_element_node f e
    method private db_element_node : 'dir. ('ident,[< all_directives ] as 'dir) preprocessed_db_element_node pprinter = QmlAst.Db.pp_path_elt self#expr
    method variant : 'dir. ([< all_directives ] as 'dir) pprinter = fun f -> function
      | `magic_to_string -> Format.pp_print_string f "magic_to_string"
      | `magic_to_xml -> Format.pp_print_string f "magic_to_xml"
          (* computed string *)
      | `string -> Format.pp_print_string f "string"
          (* internationalization *)
      | `i18n -> Format.pp_print_string f "i18n"
      | `i18n_lang -> Format.pp_print_string f "i18n_lang"
          (* *)
      | `fun_action -> Format.pp_print_string f "fun_action"
      | `magic_do -> Format.pp_print_string f "magic_do"
      | `typeof -> Format.pp_print_string f "typeof"
      | `assert_ -> Format.pp_print_string f "assert_"
      | `deprecated -> pp f "deprecated"
      | `todo -> pp f "todo"
      | `server_entry_point -> Format.pp_print_string f "server_entry_point"
      | `spawn -> Format.pp_print_string f "spawn"
      | `wait -> Format.pp_print_string f "wait"
      | `callcc -> Format.pp_print_string f "callcc"
      | `atomic -> Format.pp_print_string f "atomic"
      | `thread_context -> Format.pp_print_string f "thread_context"
      | `with_thread_context -> Format.pp_print_string f "with_thread_context"
      | `no_client_calls -> Format.pp_print_string f "no_client_calls"
      | `async -> Format.pp_print_string f "async"
      | `side_annotation _ -> Format.pp_print_string f "side_annotation"
      | `visibility_annotation (`public `sync) -> Format.pp_print_string f "exposed"
      | `visibility_annotation (`public `async) -> Format.pp_print_string f "exposed @async"
      | `visibility_annotation (`public `funaction) -> Format.pp_print_string f "exposed"
      | `visibility_annotation `private_ -> Format.pp_print_string f "protected"
      | `static_content (s, eval) -> pp f "static_content[%s][%b]" s eval
      | `static_content_directory (s, eval) -> pp f "static_content_directory[%s][%b]" s eval
      | `static_resource s -> pp f "static_resource[%s]" s
      | `static_resource_directory s -> pp f "static_resource_directory(\"%s\")" s
      | `private_ -> Format.pp_print_string f "private"
      | `package -> Format.pp_print_string f "package"
      | `public -> Format.pp_print_string f "public"
      | `unsafe_cast -> Format.pp_print_string f "unsafe_cast"
      | `fail -> Format.pp_print_string f "fail"
      | `tracker _ -> Format.pp_print_string f "tracker[?]"
      | `expand None -> Format.pp_print_string f "expand"
      | `expand (Some i) -> pp f "expand[%s]" (Big_int.string_of_big_int i)
      | `coerce -> Format.pp_print_string f "coerce"
      | `nonexpansive -> Format.pp_print_string f "nonexpansive"
      | `opensums -> Format.pp_print_string f "opensums"
      | `openrecord -> Format.pp_print_string f "openrecord"
      | `module_ -> Format.pp_print_string f "module_"
      | `module_field_lifting -> Format.pp_print_string f "module_field_lifting"
      | `warncoerce -> Format.pp_print_string f "warncoerce"
      | `js_ident -> Format.pp_print_string f "js_ident"
      | `open_ -> Format.pp_print_string f "open_"
      | `toplevel_open -> Format.pp_print_string f "toplevel_open"
      | `toplevel -> Format.pp_print_string f "toplevel"
      | `from s -> Format.fprintf f "from(%s)" s
      | `local s -> pp f "local[%s]" (Ident.to_string s)
      | `doctype (sl, access) ->
          pp f "doctype([%a], %a)" (list ",@ " Format.pp_print_string) sl self#variant access
      | `parser_ _ -> Format.pp_print_string f "parser_"
      | `xml_parser _ -> Format.pp_print_string f "xml_parser"
      | `create_lazy_record -> Format.pp_print_string f "create_lazy_record"
      | `throw -> Format.pp_print_string f "throw"
      | `catch -> Format.pp_print_string f "catch"
      | `compiletime s -> Format.fprintf f "compiletime(%S)" s
      | `opacapi -> Format.fprintf f "opacapi"
      | `stringifier -> Format.fprintf f "stringifier"
      | `comparator -> Format.fprintf f "comparator"
      | `serializer -> Format.fprintf f "serializer"
      | `xmlizer -> Format.fprintf f "xmlizer"
      | `sliced_expr -> Format.pp_print_string f "sliced_expr"
      | `may_cps -> Format.pp_print_string f "may_cps"
      | `llarray -> Format.pp_print_string f "llarray"
      | `specialize `strict -> Format.pp_print_string f "specialize_strict"
      | `specialize `polymorphic -> Format.pp_print_string f "specialize"
      | `recval -> Format.pp_print_string f "recval"
      | `sugar _ -> Format.pp_print_string f "sugar"
      | `sugar_pat_in _ -> Format.pp_print_string f "sugar_pat_in"
      (* TODO add more qml directive type here instead of duplicating with QmlDirectives.to_string above *)
      | #QmlAst.closure_instrumentation_directive as d -> Format.pp_print_string f (QmlDirectives.to_string d)

    method private string_elmt :  'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f (e,_) ->
      match e with
      | Const(CString(s)) -> pp f "%s" (QmlPrint.escaped_string s)
      | e -> pp f "{%a}" self#expr_node e

    method directive : 'dir. ('ident,[< all_directives ] as 'dir) directive pprinter =
      fun f (variant, exprs, tys) ->
        match variant, exprs, tys with
        | `coerce, [e], [ty] ->
            if comma || colon then pp f "(%a : %a)" self#reset#under_colon#expr e self#ty ty
            else pp f "%a : %a" self#under_colon#expr e self#ty ty
        | `module_, [(Record r,_)], _ -> pp f "@[<v>@[<v2>{{@ %a@]@ }}@]" (list ";@ " self#record_binding) r
        | `string, l, _ ->
            pp f "\"%a\"" (list "" self#string_elmt) l
        | #all_directives,[]   , []  -> pp f "@[<2>@@%a@]" self#variant variant
        | #all_directives,exprs, []  -> pp f "@[<2>@@%a(%a)@]" self#variant variant (list ",@ " self#reset#expr) exprs
        | #all_directives,exprs, tys -> pp f "@[<2>@@%a(%a ; %a)@]" self#variant variant (list ",@ " self#reset#expr) exprs (list ",@ " self#ty) tys


    method private pat_directive f (v:SurfaceAst.all_directives) = pp f "@[<2>@@%a@]" self#variant v

    method private pat_directives f (vs:SurfaceAst.all_directives list) = pp f "%a" (Format.pp_list "@ " self#pat_directive) vs

    method private record_binding  : 'dir. (string * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f ((s, e) as expr) ->
      match e with
      | (Directive ((`coerce : [< all_directives]),[(Record [],_)],[TypeRecord (TyRow ([],None)), _]),_) -> self#field f s
      | (Lambda (r,e),_LABEL) -> pp f "@[<2>%a@]" (self#lambda_binding self#field) (s,r,e)
      | _ ->
          let getvar = function
            | S.Ident i, _ -> Some i
            | _ -> None
          in
          if self#is_tilde_field getvar expr
          then
            pp f "~%a" self#field s
          else
            pp f "@[<2>%a :@ %a@]" self#field s self#reset#expr e

    method private record :
      'dir. ((string * ('ident, [< all_directives ] as 'dir) expr) list) pprinter =
      fun f l ->
        match l with
        | [] -> pp f "{}"
        | _ ->
            let is_tilde_field field =
              let getvar = function
                | S.Ident i, _ -> Some i
                | _ -> None
              in
              self#is_tilde_field getvar field
            in
            if List.for_all is_tilde_field l
            then
              let pp_field f (field, _) = self#field f field in
              pp f "@[<hv>~{ %a }@]" (Format.pp_list "@, " pp_field) l
            else
              pp f "@[<hv>{ %a }@]" (Format.pp_list " ;@ " self#record_binding) l


    method private lambda_binding : 'a 'dir. 'a pprinter -> ('a * (string * 'ident pat) list * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun p f (s,r,e) ->
      pp f "%a(%a) =@ %a" p s (list ",@ " self#reset#under_comma#pat) (List.map snd r) self#expr e
    method private binding : 'dir. ('ident * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (i,e) ->
      match e with
      | (Directive ((`magic_do : [< all_directives ]), [e], _),_) -> pp f "@[<2>do %a@]" self#expr e
      | (Lambda (r,e),_LABEL) -> self#lambda_binding self#ident f (i,r,e)
      | _ -> pp f "%a =@ %a" self#ident i self#expr e
    method bindings : 'dir. ('ident * ('ident, [< all_directives ] as 'dir) expr) list pprinter = fun f iel ->
      let il,el = List.split iel in
      pp f "@[<2>(%a) =@ (%a)@]" (list ",@ " self#ident) il (list ",@ " self#expr) el
    method private rule_ : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (p,e) ->
      pp f "@[<2>%a ->@ %a@]" self#under_arrow#pat p self#expr e
    method private side f = function
      | `server -> Format.pp_print_string f "server"
      | `client -> Format.pp_print_string f "client"
      | `both -> Format.pp_print_string f "both"
      | `prefer_server -> Format.pp_print_string f "prefer_server"
      | `prefer_client -> Format.pp_print_string f "prefer_client"
      | `prefer_both -> Format.pp_print_string f "prefer_both"
      | `both_implem -> Format.pp_print_string f "both_implem"

    (*-------------------------*)
    (*----- decl printer ------*)
    (*-------------------------*)
    method private path_decl_key f = function
      | QmlAst.Db.Decl_fld s -> pp f "/%a" self#field s
      | QmlAst.Db.Decl_int -> pp f "@@fixme<db_decl_int>"
      | QmlAst.Db.Decl_string -> pp f "@@fixme<db_decl_string>"
      | QmlAst.Db.Decl_set [] -> pp f "[_]"
      | QmlAst.Db.Decl_set (_ :: _) -> pp f "@@fixme<db_decl_set>"
    method private path_decl f l = list "" self#path_decl_key f l
    method private db_def : 'dir. (('ident, [< all_directives] as 'dir) expr, 'ident ty) QmlAst.Db.db_def pprinter = fun f -> function
      | QmlAst.Db.Db_TypeDecl (path_decl,ty) -> pp f "%a :@ %a" self#path_decl path_decl self#ty ty
      | QmlAst.Db.Db_Default (path_decl,e) -> pp f "%a =@ %a" self#path_decl path_decl self#expr e
      | QmlAst.Db.Db_Alias (path_decl,path_decl2) -> pp f "%a =@ %a" self#path_decl path_decl self#path_decl path_decl2
      | QmlAst.Db.Db_Constraint _ -> pp f "@@fixme<db_constraint>"
      | QmlAst.Db.Db_Virtual (p, e) -> pp f "%a := %a" self#path_decl p self#expr e
    method code : 'dir. ('ident, [< all_directives ] as 'dir) code pprinter = fun f l ->
      list "@\n@\n" self#code_elt f l
    method code_elt : 'dir. ('ident, [< all_directives ] as 'dir) code_elt pprinter = fun f c ->
      self#label self#code_elt_node f c
    method code_elt_node : 'dir. ('ident, [< all_directives ] as 'dir) code_elt_node pprinter = fun f -> function
      | Database (ident,[],options) -> pp f "database /* %a */ %s" self#ident ident (QmlAst.Db.options_to_string options)
      | Database _ -> pp f "@@fixme<database>"
      | NewDbDef db_def -> pp f "@[<2>db %a@]" self#db_def db_def
      | NewType typedefs -> (
          match typedefs with
          | [one] ->
              pp f "@[<2>%atype %a@]"
                self#typedef_visibility (fst one).SurfaceAst.ty_def_visibility
                (self#typedef ~print_visibility: false) one
          | _ ->
              pp f "@[<v>@[<2>type %a@]@]"
                (list "@]@ and @[" (self#typedef ~print_visibility: true)) typedefs
        )
      | NewVal ([bnd],_) -> pp f "%a" self#pat_binding bnd
      | NewVal (pel,_) ->
          pp f "%a" self#pat_bindings pel
      | Package (`import,s) ->
          pp f "import %s" (String.sub s 1 (String.length s - 2))
      | Package (`import_plugin, s) ->
          pp f "import-plugin %s" (String.sub s 1 (String.length s - 2))
      | Package (`declaration,s) ->
          pp f "package %s" s
    method private pat_binding : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (p,e) ->
      match p, e with
      | (PatVar i,_LABEL1), (Lambda (r,e),_LABEL2) ->
        self#lambda_binding self#ident f (i.SurfaceAst.ident,r,e)
      | _, (Directive ((`visibility_annotation `public b : [< all_directives ]),[e],_),_) -> (
          match b with
          | `async -> pp f "@publish_async %a" self#pat_binding (p,e)
          | `sync -> pp f "@publish %a" self#pat_binding (p, e)
          | `funaction -> assert false
        )
      | _, (Directive (`side_annotation side,[e],_),_) -> pp f "%a %a" self#side side self#pat_binding (p,e)
      | _ -> pp f "%a =@ %a" self#pat p self#expr e
    method private pat_bindings : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) list pprinter = fun f pel ->
      let pl,el = List.split pel in
      pp f "@[<2>(%a) =@ (%a)@]" (list ",@ " self#pat) pl (list ",@ " self#expr) el

    method private typedef_visibility f = function
      | SurfaceAst.TDV_public -> ()
      | SurfaceAst.TDV_abstract _ -> pp f "@@abstract@ "
      | SurfaceAst.TDV_private _ -> pp f "@@private@ "

    method private typedef ~print_visibility f ty_def =
      self#label (self#typedef_node ~print_visibility) f ty_def
    method private typedef_node ~print_visibility f ty_def =
      (* We print the visibility of the definition just before its name only if
         requested. In effect, when the definition defines only one type, we
         prefer to have the visibility printed first, before the token "type".
         So in this case, if the visibility is already printed, no need to print
         it again. *)
      if print_visibility then
        pp f "%a" self#typedef_visibility ty_def.SurfaceAst.ty_def_visibility ;
      pp f "%a" self#typeident ty_def.SurfaceAst.ty_def_name ;
      (match ty_def.ty_def_params with
       | [] -> ()
       | some -> pp f "(%a) =@ " (list ",@ " self#typevar) some) ;
      pp f " =@ %a" self#ty ty_def.SurfaceAst.ty_def_body
  end

end

type binding_directives =
  [ S.access_directive | S.distribution_directive ]

module Js = struct
  class virtual ['ident] printer =
  object (self)
    (* priorities
     *
     *)
    val comma = false
    val forall = false
    val typesum = false
    val arrow = false
    val colon = false
    val op = false
    val function_ = "function"
    method under_typesum = {< typesum = true >}
    method under_comma = {< comma = true >}
    method private under_forall = {< forall = true >}
    method private under_arrow = {< arrow = true >}
    method under_colon = {< colon = true >}
    method private under_op = {< op = true >}
    method private reset = {< typesum = false;
                      comma = false;
                      forall = false;
                      arrow = false;
                      colon = false;
                      op = false;
                           >}

    method keyword = function
    | "recursive" | "and"
    | "function" | "module"
    | "match" | "if" | "as" | "case" | "default"
    | "database"
    | "type"
    | "with"
    | "css"
    | "server" | "client" | "exposed" | "protected" -> true
    | _ -> false

    method private variant_has_at : 'dir. ([< all_directives ] as 'dir) -> bool = function
    | `public
    | `private_
    | `visibility_annotation _ | `side_annotation _ -> false
    | (_ : [< all_directives ]) -> true

    method private binding_directives : 'dir. ([< all_directives ] as 'dir) -> bool  = function
    | a when not(self#variant_has_at a) -> true
    | `specialize _ | `async
    | #QmlAst.slicer_directive | #QmlAst.closure_instrumentation_directive
    | #S.coding_directive | #S.access_directive -> true
    | (_ : [< all_directives ]) -> false


    method label : 'a. 'a pprinter -> 'a QmlLoc.label pprinter = fun p f v -> p f (fst v)

    method private const_expr f = function
      | CInt i -> Format.pp_print_string f (Big_int.string_of_big_int i)
      | CFloat float -> Format.pp_print_float f float
      | CString s -> pp f "\"%s\"" (QmlPrint.escaped_string s)

    method private field f s =
      Format.pp_print_string f (
        match classify_string self#keyword s with
        | `ident -> s
        | `operator | `backquote -> "`"^s^"`"
      )

    (*------------------------*)
    (*----- type printer -----*)
    (*------------------------*)
    method ty f ty = self#label self#ty_node f ty
    method ty_node f = function
      | TypeSumSugar _ as ty when typesum -> pp f "(%a)" self#reset#ty_node ty
      | TypeSumSugar _ as ty when forall -> pp f "(%a)" self#reset#ty_node ty
      | TypeArrow _ as ty when arrow -> pp f "(%a)" self#reset#ty_node ty
      | (TypeSumSugar _ | TypeArrow _ | TypeForall _) as ty when comma -> pp f "(%a)" self#reset#ty_node ty
      | TypeConst c -> self#const_ty_node f c
      | TypeVar v -> self#typevar f v
      | TypeArrow r -> self#arrow_t_node f r
      | TypeRecord r -> self#row_t_node f r
      | TypeSumSugar l -> self#typesumsugar f l
      | TypeNamed t -> self#typeinstancenode f t
      | TypeExternal -> Format.pp_print_string f "external"
      | TypeForall p -> self#typeforall f p
      | TypeModule r -> self#typemodule f r

    method private const_ty_node f const =
      Format.pp_print_string f (
        match const with
        | TyInt -> "int"
        | TyFloat -> "float"
        | TyString -> "string"
      )
    method private rowvar f _ = Format.pp_print_string f "..."
    method private colvar f _ = Format.pp_print_string f "..."
    method virtual typeident : 'ident typeident pprinter
    method virtual typevar : 'ident typevar pprinter
    method virtual typeident_original_name : 'ident -> string
    method private row_t f x = self#label self#row_t_node f x
    method private row_t_node f (TyRow (nodes, rowo)) =
      match nodes, rowo with
      | [], None -> Format.pp_print_string f "{}"
      | [], Some rowvar -> pp f "{%a}" self#rowvar rowvar
      | l, None -> pp f "@[<1>{%a}@]" (list ",@ " self#typerecordbinding) l
      | l, Some rowvar -> pp f "@[<1>{%a,@ %a}@]" (list ",@ " self#typerecordbinding) l self#rowvar rowvar
    method private typerecordbinding f (s,ty) =
      match ty with
      | (TypeRecord (TyRow ([],None)),_) -> Format.pp_print_string f s
      | _ -> pp f "%a %a" self#reset#ty ty self#field s
    method private typemodulebinding f (s,ty) =
      match ty with
      | (TypeRecord (TyRow ([],None)),_) -> Format.pp_print_string f s
      | (TypeArrow (args, ret), _) ->
          pp f "%a %a(%a)" self#reset#ty ret self#field s self#under_comma#arrow_row_t args
      | _ -> pp f "%a: %a" self#field s self#reset#ty ty
    method sum_t f v = self#label self#sum_t_node f v
    method private sum_t_node f = function
      | SumName typeinstance -> self#typeinstancenode f typeinstance
      | SumRecord r -> self#row_t_node f r
      | SumVar c -> self#colvar f c
    method private arrow_t f v = self#label self#arrow_t_node f v
    method private arrow_t_node f (row_t,ty) =
      pp f "@[<2>%a ->@ %a@]" self#under_comma#arrow_row_t row_t self#under_arrow#ty ty
    method arrow_row_t f v = self#label self#arrow_row_t_node f v
    method private arrow_row_t_node f (TyRow (l,row)) =
      assert (row = None);
      list ",@ " self#arrowbinding f l
    method private arrowbinding f (_,ty) = self#ty f ty
    method private typeinstance f t = self#label self#typeinstancenode f t
    method private typeinstancenode f (ident,params) =
      match params with
      | [] -> self#typeident f ident
      | _ -> pp f "@[@[<2>%a(%a@])@]" self#typeident ident (list ",@ " self#under_comma#ty) params
    method private typeforall f (vars,ty) =
      pp f "@[<2>forall(@[<h>%a@]). %a@]" (list ",@ " self#typevar) vars self#under_forall#ty ty
    method private typesumsugar f l =
      pp f "@[<v>or %a@]" (list "@ or " self#under_typesum#sum_t) l
    method private typemodule f fields =
      pp f "module {@\n @[@{<v2>%a@ @]@]@\n}" (list "@\n" self#typemodulebinding) fields

    (*-------------------------*)
    (*---- pattern printer ----*)
    (*-------------------------*)
    method private is_tilde_field : 'a. ('a -> 'ident option) -> string * 'a -> bool =
      (fun getvar (field, pat) ->
         match getvar pat with
         | Some ident ->
             let ident = self#to_protected_ident ident in
             String.compare field ident = 0
         | None -> false
      )

    method pat f p =
      match PatSugar.resugarer self#typeident_original_name p with
      | `list (head,None) -> pp f "@[<2>[%a]@]" (list ",@ " self#pat) head
      | `list (head,Some rest) -> pp f "@[<2>[%a@ |@ %a]@]" (list ",@ " self#pat) head self#pat rest
      | `tuple pl -> pp f "@[<1>(%a)@]" (list ",@ " self#pat) pl
      | `no_sugar -> self#label self#pat_node f p
    method pat_node f = function
      | PatCoerce _ as p when arrow || comma || colon -> pp f "%a" self#reset#pat_node p
      | PatRecord (fields, rowvar) -> self#pat_record f fields rowvar
      | PatAny -> pp f "_"
      | PatConst c -> self#const_expr f c
      | PatVar v -> pp f "%a%a" self#pat_directives  (userland_visibilities_to_whatever v.directives) self#ident v.ident
      | PatCoerce (p,ty) -> pp f "%a %a" self#ty ty self#pat p
      | PatAs (p,i) -> pp f "%a %a as %a" self#pat_directives (userland_visibilities_to_whatever i.directives) self#pat p self#ident i.ident

    method private pat_record_binding f ((s, p) as pat) =
      match p with
      | PatCoerce ((PatRecord ([], _), _), (TypeRecord (TyRow ([], None)), _)), _ -> self#field f s
      | _ ->
          let getvar = function
            | S.PatVar v, _ -> Some v.ident
            | _ -> None
          in
          if self#is_tilde_field getvar pat
          then
            pp f "~%a" self#field s
          else
            pp f "%a :@ %a" self#field s self#pat p

    method private pat_record f fields rowvar =
      match fields with
      | [] ->
          if rowvar = `open_
          then
            Format.pp_print_string f "{ ... }"
          else
            Format.pp_print_string f "{}"
      | _ ->
          let rowvar =  if rowvar = `open_ then ", ..." else "" in
          let is_tilde_field field =
            let getvar = function
              | S.PatVar v, _ -> Some v.S.ident
              | _ -> None
            in
            self#is_tilde_field getvar field
          in
          if List.for_all is_tilde_field fields
          then
            let pp_field f (field, _) = self#field f field in
            pp f "@[<hv2>~{ %a%s @,}@]"
              (Format.pp_list ",@ " pp_field) fields
              rowvar
          else
            pp f "@[<hv2>{ %a%s @,}@]"
              (Format.pp_list ",@ " self#pat_record_binding) fields
              rowvar


    (*-------------------------*)
    (*----- expr printer ------*)
    (*-------------------------*)
    method private flatten_letin : 'dir.
      bool * ('a * ('a,  [< all_directives ] as 'dir) SurfaceAst.expr) list * ('a,  [< all_directives ] as 'dir) SurfaceAst.expr
      -> ((bool * ('a * ('a,  [< all_directives ] as 'dir) SurfaceAst.expr) list)) list
      * ('a,  [< all_directives ] as 'dir) SurfaceAst.expr =
      fun (isrec, binds, expr) ->
      let rec aux expr acc =
        match fst expr with
        | LetIn (isrec, binds, next) -> aux next ((isrec, binds) :: acc)
        | _ -> (List.rev acc, expr)
      in aux expr [(isrec, binds)]


    method private print_binds : 'dir. (((bool * ('a * ('a,  [< all_directives ] as 'dir) SurfaceAst.expr) list)) list  * ('a,  [< all_directives ] as 'dir) SurfaceAst.expr) pprinter
      = fun f (binds, final) ->
        pp f "%a@\n%a" (list "@\n" self#print_group) binds self#expr final;

    method private print_group : 'dir. (bool * ('ident * ('ident, [< all_directives ] as 'dir) expr) list) pprinter = fun f (isrec, group) ->
      if isrec then pp f "recursive ";
      pp f "%a" (list "@\n and " self#binding) group


    method virtual is_operator : 'ident -> bool
    method virtual to_protected_ident : 'ident -> string
    method virtual to_unprotected_ident : 'ident -> string
    method ident f i = Format.pp_print_string f (self#to_protected_ident i)
    method private unprotected_ident f i = Format.pp_print_string f (self#to_unprotected_ident i)
    method private apply : 'dir 'a. 'a pprinter -> 'a list -> ('ident,[< all_directives ] as 'dir) expr pprinter = fun f_arg arg f e -> pp f "@[<2>%a(%a)@]" self#apply_expr e (list ",@ " f_arg) arg
    method private expr_need_block : 'dir . ('ident,[< all_directives ] as 'dir) expr -> bool = fun e ->
      match fst e with
      | Apply _ | Ident _ | Const _ -> false
      | _ -> true
    method private expr_sugar : 'dir . bool -> ('ident,[< all_directives ] as 'dir) expr pprinter = fun sugar f e ->
      if not sugar then self#label self#expr_node f e
      else match ExprSugar.resugarer self#typeident_original_name e with
      | `list (head,None) -> pp f "@[<2>[%a]@]" (list ",@ " self#expr) head
      | `list (head,Some rest) -> pp f "@[<2>[%a@ |@ %a]@]" (list ",@ " self#expr) head self#expr rest
      | `tuple el -> pp f "@[<1>(%a)@]" (list ",@ " self#expr) el
      | `if_ (e1,e2,e3) ->
        (if self#expr_need_block e2 then
          pp f "@[<2>if (%a) {@\n%a@]@\n@[<2>} " (* TODO ?? *)
        else
          pp f "if (%a) %a @[<2>") self#expr e1 self#expr e2;
        (if self#expr_need_block e3 then
          pp f "else {@\n%a@]@\n}" (* TODO ?? *)
        else
          pp f "else %a@]") self#expr e3;

      | `xhtml e  -> Sugar.Xhtml.pp_xml self#expr_sugar f e
      | `action e -> Sugar.Action.pp self#to_unprotected_ident self#expr_sugar f e
      | `no_sugar -> self#label self#expr_node f e
    method expr : 'dir . ('ident,[< all_directives ] as 'dir) expr pprinter  = self#expr_sugar true
    method private expr_string : 'dir . (string,[< all_directives ] as 'dir) expr pprinter  = Obj.magic self#expr
    method expr_node : 'dir. ('ident,[< all_directives ] as 'dir) expr_node pprinter = fun f -> function
        (*| Directive ((`coerce:[< all_directives]),_,_) as e when comma -> pp f "(%a)" self#expr_node e*)
      | Lambda _ | Match _ | LetIn _ as e when op -> pp f "(%a)" self#reset#expr_node e
      | Lambda _ as e when comma -> pp f "(%a)" self#reset#expr_node e
      | Apply ((Ident oper,_LABEL1),([(_,e1);(_,e2)],_LABEL2)) as e when self#is_operator oper ->
          if op || colon then pp f "(%a)" self#reset#expr_node e else
            pp f "@[<2>%a %s@ %a@]" self#under_op#expr e1 (operator_image (self#to_unprotected_ident oper)) self#under_op#expr e2
      | Apply ((Ident oper,_LABEL1),([(_,(Const (CString s), _))],_LABEL2)) when (Obj.magic oper="$") ->
          pp f "#%a" Format.pp_print_string (self#to_protected_ident (Obj.magic s))
      | Apply ((Ident oper,_LABEL1),([(_,e1)],_LABEL2)) as e when self#is_operator oper ->
          if op || colon then pp f "(%a)" self#reset#expr_node e else
            pp f "@[<2>%s%a@]" (operator_image (self#to_unprotected_ident oper)) self#under_op#expr e1
      | Apply (e,(r,_LABEL)) -> pp f "@[<2>%a(%a)@]" self#apply_expr e (list ",@ " (fun f (_,e) -> self#reset#under_comma#expr f e)) r
      | Lambda (r, ((LetIn _, _) as e)) ->
          pp f "@[<h 2>function(%a) {@,@[%a@]@]}"
            (list ",@ " (fun f (_,p) -> self#under_comma#pat f p)) r
            self#expr e
      | Lambda (r,e) -> pp f "@[function(%a) {@,@[%a@]@]}" (list ",@ " (fun f (_,p) -> self#under_comma#pat f p)) r self#expr e
      | Const c -> self#const_expr f c
      | Ident ident -> self#ident f ident
      | LetIn (isrec, binds, expr) ->
          let (binds, final) = self#flatten_letin (isrec, binds, expr) in
          pp f "%a" self#print_binds (binds, final)
      (* | LetIn (true,iel,e) -> *)
      (*     pp f "@[<v>@[<2>rec %a@]@ %a@]" (list "@]@ and @[" self#binding) iel self#expr e *)
      (* | LetIn (false,[bnd],e) -> *)
      (*     pp f "{ @[<v>@[<2>%a@]@ %a@] }" self#binding bnd self#expr e *)
      (* | LetIn (false,iel,e) -> *)
      (*     pp f "/* encoded let and */@\n@[<v>%a@ %a@]" self#bindings iel self#expr e *)
      | Match (e,pel) ->
          pp f "match (%a) {@\n@[%a@]@\n}@,"
            self#reset#expr e
            (list "@\n" self#rule_) pel
      | Record fields -> self#record f fields
      | ExtendRecord (r,e) ->
          pp f "@[<1>{%a with@ %a}@]" self#expr e (list ",@ " self#record_binding) r
      | Dot (e,s) -> pp f "%a.%a" self#apply_expr e self#field s
      | Bypass s -> pp f "%%%%%s%%%%" (BslKey.to_string s)
      | DBPath (elt,kind) ->
          QmlAst.Db.pp_path self#expr f (List.map fst (fst elt), kind)
      | Directive d -> self#directive f d
    method private apply_expr : 'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f -> function
      | (Ident _,_)
      | (Apply _,_)
      | (Directive _ ,_)
      | (Dot _, _) as e -> self#expr f e
      | e -> pp f "(%a)" self#reset#expr e

    method variant : 'dir. ([< all_directives ] as 'dir) pprinter = fun f -> function
      | `magic_to_string -> Format.pp_print_string f "magic_to_string"
      | `magic_to_xml -> Format.pp_print_string f "magic_to_xml"
          (* computed string *)
      | `string -> Format.pp_print_string f "string"
          (* internationalization *)
      | `i18n -> Format.pp_print_string f "i18n"
      | `i18n_lang -> Format.pp_print_string f "i18n_lang"
          (* *)
      | `fun_action -> Format.pp_print_string f "fun_action"
      | `magic_do -> Format.pp_print_string f "magic_do"
      | `typeof -> Format.pp_print_string f "typeof"
      | `assert_ -> Format.pp_print_string f "assert_"
      | `deprecated -> pp f "deprecated"
      | `todo -> pp f "todo"
      | `server_entry_point -> Format.pp_print_string f "server_entry_point"
      | `spawn -> Format.pp_print_string f "spawn"
      | `wait -> Format.pp_print_string f "wait"
      | `callcc -> Format.pp_print_string f "callcc"
      | `atomic -> Format.pp_print_string f "atomic"
      | `thread_context -> Format.pp_print_string f "thread_context"
      | `with_thread_context -> Format.pp_print_string f "with_thread_context"
      | `no_client_calls -> Format.pp_print_string f "no_client_calls"
      | `async -> Format.pp_print_string f "async"
      | `side_annotation `client -> Format.pp_print_string f "client"
      | `side_annotation `server -> Format.pp_print_string f "server"
      | `side_annotation `both -> Format.pp_print_string f "both"
      | `side_annotation `prefer_client -> Format.pp_print_string f "prefer_client"
      | `side_annotation `prefer_server -> Format.pp_print_string f "prefer_server"
      | `side_annotation `both_implem -> Format.pp_print_string f "both_implem"
      | `side_annotation `prefer_both -> Format.pp_print_string f "prefer_both"
      | `visibility_annotation (`public `sync) -> Format.pp_print_string f "exposed"
      | `visibility_annotation (`public `async) -> Format.pp_print_string f "exposed @async"
      | `visibility_annotation (`public `funaction) -> Format.pp_print_string f "exposed"
      | `visibility_annotation `private_ -> Format.pp_print_string f "protected"
      | `static_content (s, _eval) -> pp f "static_source_content(\"%s\")" s
      | `static_content_directory (s, eval) -> pp f "static_content_directory[%s][%b]" s eval
      | `static_resource s -> pp f "static_resource[%s]" s
      | `static_resource_directory s -> pp f "static_resource_directory(\"%s\")" s
      | `private_ -> Format.pp_print_string f "private"
      | `package -> Format.pp_print_string f "package"
      | `public -> Format.pp_print_string f "public"
      | `unsafe_cast -> Format.pp_print_string f "unsafe_cast"
      | `fail -> Format.pp_print_string f "fail"
      | `tracker _ -> Format.pp_print_string f "tracker[?]"
      | `expand None -> Format.pp_print_string f "expand"
      | `expand (Some i) -> pp f "expand[%s]" (Big_int.string_of_big_int i)
      | `coerce -> Format.pp_print_string f "coerce"
      | `nonexpansive -> Format.pp_print_string f "nonexpansive"
      | `opensums -> Format.pp_print_string f "opensums"
      | `openrecord -> Format.pp_print_string f "openrecord"
      | `module_ -> Format.pp_print_string f "module"
      | `module_field_lifting -> Format.pp_print_string f "module_field_lifting"
      | `warncoerce -> Format.pp_print_string f "warncoerce"
      | `js_ident -> Format.pp_print_string f "js_ident"
      | `open_ -> Format.pp_print_string f "open_"
      | `toplevel_open -> Format.pp_print_string f "toplevel_open"
      | `toplevel -> Format.pp_print_string f "toplevel"
      | `from s -> Format.fprintf f "from(%s)" s
      | `local s -> pp f "local[%s]" (Ident.to_string s)
      | `doctype (sl, access) ->
          pp f "doctype([%a], %a)" (list ",@ " Format.pp_print_string) sl self#variant access
      | `parser_ _ -> Format.pp_print_string f "parser_"
      | `xml_parser _ -> Format.pp_print_string f "xml_parser"
      | `create_lazy_record -> Format.pp_print_string f "create_lazy_record"
      | `throw -> Format.pp_print_string f "throw"
      | `catch -> Format.pp_print_string f "catch"
      | `compiletime s -> Format.fprintf f "compiletime(%S)" s
      | `opacapi -> Format.fprintf f "opacapi"
      | `stringifier -> Format.fprintf f "stringifier"
      | `comparator -> Format.fprintf f "comparator"
      | `serializer -> Format.fprintf f "serializer"
      | `xmlizer -> Format.fprintf f "xmlizer"
      | `sliced_expr -> Format.pp_print_string f "sliced_expr"
      | `may_cps -> Format.pp_print_string f "may_cps"
      | `llarray -> Format.pp_print_string f "llarray"
      | `specialize `strict -> Format.pp_print_string f "specialize_strict"
      | `specialize `polymorphic -> Format.pp_print_string f "specialize"
      | `recval -> Format.pp_print_string f "recursive"
      | `sugar _ -> Format.pp_print_string f "sugar"
      | `sugar_pat_in _ -> Format.pp_print_string f "sugar_pat_in"
          (* TODO add more qml directive type here instead of duplicating with QmlDirectives.to_string above *)
      | #QmlAst.closure_instrumentation_directive as d -> Format.pp_print_string f (QmlDirectives.to_string d)

    method private string_elmt :  'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f (e,_) ->
      match e with
      | Const(CString(s)) -> pp f "%s" (QmlPrint.escaped_string s)
      | e -> pp f "{%a}" self#expr_node e

    method directive : 'dir. ('ident,[< all_directives ] as 'dir) directive pprinter =
      fun f (variant, exprs, tys) ->
        match variant, exprs, tys with
        | `coerce, [e], [ty] ->
            pp f "@[<h>(%a) %a@]" self#ty ty self#under_colon#expr e
        | `module_, [m], _ ->
            self#module_binding (fun _ _ -> ()) f ((), m)
        | `string, l, _ -> Sugar.String.pp_expr self#expr_sugar f l
        | `magic_to_string, [e], _ -> self#expr f e
        | `fun_action, [e], _ -> self#expr f e
        | `magic_to_xml, [e], _ -> pp f "XmlConvert.of_alpha(%a)" self#expr e
        | `sugar expr, [_], [] -> self#expr_string f expr
        | `sugar_pat_in (pat,e1,e2), [_], [] -> pp f "%a@\n%a" self#pat_binding_string (pat,e1) self#expr_string e2
(* self#apply (fun f -> function | `hole -> pp f "_" | `expr e -> self#reset#under_comma#expr f e) args f e*)
        | `recval, exprs, [] -> pp f "recursive %a" (list ",@ " self#reset#expr) exprs
        | #all_directives,[]   , []  -> pp f "@[<2>@@%a@]" self#variant variant
        | #all_directives,exprs, []  -> pp f "@[<2>@@%a(%a)@]" self#variant variant (list ",@ " self#reset#expr) exprs
        | #all_directives,exprs, tys -> pp f "@[<2>@@%a(%a ; %a)@]" self#variant variant (list ",@ " self#reset#expr) exprs (list ",@ " self#ty) tys


    method private pat_directive f (v:SurfaceAst.all_directives) = pp f "@[<2>@@%a@]" self#variant v

    method private pat_directives f (vs:SurfaceAst.all_directives list) =
      match vs with | [] -> ()
      | _ -> pp f "%a " (Format.pp_list "@ " self#pat_directive) vs

    method private record_binding  : 'dir. (string * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f ((s, e) as expr) ->
      match e with
      | (Directive ((`coerce : [< all_directives]),[(Record [],_)],[TypeRecord (TyRow ([],None)), _]),_) -> self#field f s
      | (Lambda (r,e),_LABEL) -> pp f "@[<2>%a@]" (self#lambda_binding self#field) (s,r,e)
      | _ ->
          let getvar = function
            | S.Ident i, _ -> Some i
            | _ -> None
          in
          if self#is_tilde_field getvar expr
          then
            pp f "~%a" self#field s
          else
            pp f "@[<2>%a :@ %a@]" self#field s self#reset#expr e



    method private record :
      'dir. ((string * ('ident, [< all_directives ] as 'dir) expr) list) pprinter =
      fun f l ->
        match l with
        | [] -> pp f "{}"
        | _ ->
            let is_tilde_field field =
              let getvar = function
                | S.Ident i, _ -> Some i
                | _ -> None
              in
              self#is_tilde_field getvar field
            in
            if List.for_all is_tilde_field l
            then
              let pp_field f (field, _) = self#field f field in
              pp f "@[<hv>~{ %a @,}@]" (Format.pp_list ",@ " pp_field) l
            else
              pp f "@[<hv>{ %a @,}@]" (Format.pp_list ",@ " self#record_binding) l

    (* start box indentation is handled via binding_aux or pat_binding *)
    method private lambda_binding : 'a 'dir. 'a pprinter -> ('a * (string * 'ident pat) list * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun p f (s,r,e) ->
      match fst e with
      | LetIn _
      | Record _
      | ExtendRecord _
      | Match _ ->
          pp f "%s %a(%a) {@,%a@,@]}" function_ p s (list "," self#reset#under_comma#pat) (List.map snd r) self#expr e
      | _  -> pp f "%s %a(%a) {@,%a@]@,}" function_ p s (list "," self#reset#under_comma#pat) (List.map snd r) self#expr e


    method private module_binding : 'id 'dir. 'id pprinter -> ('id * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun p f (s,e) ->
      match fst e with
      | Lambda (r,e)-> self#lambda_binding p f (s,r,e)
      | Record r ->
        pp f "module %a {@\n%a@]@\n}" p s
          (list "@\n" (self#binding_aux false (fun f s -> pp f "%s" s))) r
      | _ -> pp f "%a = module %a" p s self#expr e

    method private binding_aux : 'id 'dir. bool -> 'id pprinter -> ('id * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun semic ipp f (i,e) ->
      let ipp = Obj.magic ipp in (* WTF *)
      let semic = if semic then ";" else "" in
      let rec aux e =
        match e with
(*        | (Directive ((`coerce: [< all_directives ]), [e], [ty]),_) ->
            pp f "(%a)" self#ty ty;
            aux e *) (* sync with sugar *)
        | (Directive (v,[e],_),_) when self#binding_directives v ->
          let at = if self#variant_has_at v then "@" else "" in
          pp f "%s%a " at self#variant v;
          aux e
        | _ ->
          match e with
          | (Lambda (r,e),_LABEL) -> self#lambda_binding ipp f (i,r,e)
          | (Directive ((`module_ : [< all_directives ]), [e], _),_) ->
            self#module_binding ipp f (i,e)
          | (Directive ((`magic_do : [< all_directives ]), [e], _),_) -> pp f "%a%s@]" self#expr e semic
          | e ->
              let recursive, e = match e with
                | (Directive ((`recval : [< all_directives ]), [e], []), _) -> "recursive", e
                | _ -> "", e
              in
              if self#expr_need_block e then pp f "%s %a = {@\n%a@]@\n}" recursive ipp i self#expr e
              else pp f "%a =@ %a%s@]" ipp i self#expr e semic

      in
      pp f "@[<4>";
      aux e

    method private binding : 'dir. ('ident * ('ident, [< all_directives ] as 'dir) expr) pprinter =
      self#binding_aux true self#ident

    method bindings : 'dir. ('ident * ('ident, [< all_directives ] as 'dir) expr) list pprinter = fun f iel ->
      let il,el = List.split iel in
      pp f "@[<2>(%a) =@ (%a)@]" (list ",@ " self#ident) il (list ",@ " self#expr) el
    method private rule_ : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (p,e) ->
      pp f "@[<2>";
      (match p with
       | (PatAny, _) -> pp f "default"
       | p -> pp f "case %a" self#under_arrow#pat p);
      pp f ":@ %a@]" self#expr e
    method private side f = function
      | `server -> Format.pp_print_string f "server"
      | `client -> Format.pp_print_string f "client"
      | `both -> Format.pp_print_string f "both"
      | `prefer_server -> Format.pp_print_string f "prefer_server"
      | `prefer_client -> Format.pp_print_string f "prefer_client"
      | `prefer_both -> Format.pp_print_string f "prefer_both"
      | `both_implem -> Format.pp_print_string f "both_implem"

    (*-------------------------*)
    (*----- decl printer ------*)
    (*-------------------------*)
    method private path_decl_key f = function
      | QmlAst.Db.Decl_fld s -> pp f "/%a" self#field s
      | QmlAst.Db.Decl_int -> pp f "@@fixme<db_decl_int>"
      | QmlAst.Db.Decl_string -> pp f "@@fixme<db_decl_string>"
      | QmlAst.Db.Decl_set [] -> pp f "[_]"
      | QmlAst.Db.Decl_set (_ :: _) -> pp f "@@fixme<db_decl_set>"
    method private path_decl f l = list "" self#path_decl_key f l
    method private db_def : 'dir. (('ident, [< all_directives] as 'dir) expr, 'ident ty) QmlAst.Db.db_def pprinter = fun f -> function
      | QmlAst.Db.Db_TypeDecl (path_decl,ty) -> pp f "%a %a" self#ty ty self#path_decl path_decl
      | QmlAst.Db.Db_Default (path_decl,e) -> pp f "%a =@ %a" self#path_decl path_decl self#expr e
      | QmlAst.Db.Db_Alias (path_decl,path_decl2) -> pp f "%a =@ %a" self#path_decl path_decl self#path_decl path_decl2
      | QmlAst.Db.Db_Constraint _ -> pp f "@@fixme<db_constraint>"
      | QmlAst.Db.Db_Virtual (p, e) -> pp f "%a := %a" self#path_decl p self#expr e
    method code : 'dir. ('ident, [< all_directives ] as 'dir) code pprinter = fun f l ->
      list "@\n@\n" self#code_elt f l
    method code_elt : 'dir. ('ident, [< all_directives ] as 'dir) code_elt pprinter = fun f c ->
      self#label self#code_elt_node f c
    method code_elt_node : 'dir. ('ident, [< all_directives ] as 'dir) code_elt_node pprinter = fun f -> function
      | Database (ident,[name],options) -> pp f "database /* %a */ %s %s" self#ident ident name (QmlAst.Db.options_to_string options)
      | Database (ident,[],options) -> pp f "database /* %a */ %s" self#ident ident (QmlAst.Db.options_to_string options)
      | Database _ -> pp f "@@fixme<database>"
      | NewDbDef db_def -> pp f "@[<2>database %a@]" self#db_def db_def
      | NewType typedefs -> (
          match typedefs with
          | [one] ->
              pp f "@[<2>%atype %a@]"
                self#typedef_visibility (fst one).SurfaceAst.ty_def_visibility
                (self#typedef ~print_visibility: false) one
          | _ ->
              pp f "@[<v>@[<2>type %a@]@]"
                (list "@]@ and @[" (self#typedef ~print_visibility: true)) typedefs
        )
      | NewVal ([bnd],false) -> pp f "%a" self#pat_binding bnd
      | NewVal (pel,false) ->
          pp f "/* encoding of a let and */@\n%a" self#pat_bindings pel
      | NewVal (pel,true) ->
          pp f "@[<v>@[<2>recursive %a@]@]" (list "@]@ and @[" self#pat_binding) pel
      | Package (`import,s) ->
          pp f "import %s" (String.sub s 1 (String.length s - 2))
      | Package (`import_plugin, s) ->
          pp f "import-plugin %s" (String.sub s 1 (String.length s - 2))
      | Package (`declaration,s) ->
          pp f "package %s" s
    (* TODO merge with binding_aux *)
    method private pat_binding : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (p,e) ->
      let rec aux e =
        match e with
        | (Directive (v,[e],_),_) when self#binding_directives v ->
          let at = if self#variant_has_at v then "@" else "" in
          pp f "%s%a " at self#variant v;
          aux e
(*        | (Directive ((`coerce: [< all_directives ]), [e], [ty]),_) ->
            pp f "(%a)" self#ty ty;
            aux e *) (* make sync with sugar *)
        | _ ->
          match p, e with
          | (PatVar i,_LABEL1), (Lambda (r,e),_LABEL2) ->
            self#lambda_binding self#ident f (i.SurfaceAst.ident,r,e)
          | (PatVar i,_LABEL1), (Directive ((`module_ : [< all_directives ]), [e], _),_) ->
            self#module_binding self#ident f (i.SurfaceAst.ident,e)
          | _, (Directive ((`magic_do : [< all_directives ]),[e],_),_) ->  pp f "%a;@]" self#expr e

          | _,e -> (if self#expr_need_block e then pp f "%a = {@\n%a@]@\n}"
            else pp f "%a =@ %a@]") self#pat p self#expr e
      in
      pp f "@[<hv 4>";
      aux e
    method private pat_binding_string : 'dir. (string pat * (string, [< all_directives ] as 'dir) expr) pprinter = Obj.magic self#pat_binding
    method private pat_bindings : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) list pprinter = fun f pel ->
      let pl,el = List.split pel in
      pp f "@[<2>(%a) =@ (%a)@]" (list ",@ " self#pat) pl (list ",@ " self#expr) el

    method private typedef_visibility f = function
      | SurfaceAst.TDV_public -> ()
      | SurfaceAst.TDV_abstract _ -> pp f "@@abstract@ "
      | SurfaceAst.TDV_private _ -> pp f "@@private@ "

    method private typedef ~print_visibility f ty_def =
      self#label (self#typedef_node ~print_visibility) f ty_def
    method private typedef_node ~print_visibility f ty_def =
      (* We print the visibility of the definition just before its name only if
         requested. In effect, when the definition defines only one type, we
         prefer to have the visibility printed first, before the token "type".
         So in this case, if the visibility is already printed, no need to print
         it again. *)
      if print_visibility then
        pp f "%a" self#typedef_visibility ty_def.SurfaceAst.ty_def_visibility ;
      pp f "%a" self#typeident ty_def.SurfaceAst.ty_def_name ;
      (match ty_def.ty_def_params with
       | [] -> ()
       | some -> pp f "(%a)" (list ",@ " self#typevar) some) ;
      pp f " =@ %a" self#ty ty_def.SurfaceAst.ty_def_body
  end

end

module type SGeneric = sig
  class virtual ['a] printer :
  object ('b)
    method virtual is_operator : 'a -> bool
    method virtual to_protected_ident : 'a -> String.t
    method virtual to_unprotected_ident : 'a -> string
    method virtual typeident : 'a SurfaceAst.typeident pprinter
    method virtual typeident_original_name : 'a -> string
    method virtual typevar : 'a SurfaceAst.typevar pprinter

    method directive : ('a, [< all_directives ]) SurfaceAst.directive pprinter
    method expr : ('a, [< all_directives ]) SurfaceAst.expr pprinter
    method expr_node : ('a, [< all_directives ]) SurfaceAst.expr_node pprinter
    method pat : 'a SurfaceAst.pat Format.pprinter
    method pat_node : 'a SurfaceAst.pat_node Format.pprinter
    method label : 'p. 'p pprinter -> 'p QmlLoc.label pprinter
    method bindings : ('a * ('a, [< all_directives ]) SurfaceAst.expr) list pprinter
    method code : ('a, [< all_directives ]) SurfaceAst.code pprinter
    method code_elt : ('a, [< all_directives ]) SurfaceAst.code_elt pprinter
    method code_elt_node : ('a, [< all_directives ] as 'h) SurfaceAst.code_elt_node pprinter
    method ident : 'a pprinter
    method ty : 'a SurfaceAst.ty_node QmlLoc.label Format.pprinter
    method ty_node : 'a SurfaceAst.ty_node pprinter
    method variant : ([< all_directives ]) pprinter
    method keyword : string -> bool

    method arrow_row_t : Format.formatter -> 'a SurfaceAst.row_t -> unit
    method sum_t : 'a SurfaceAst.sum_t_node QmlLoc.label Format.pprinter

    method under_colon : 'b
    method under_comma : 'b
    method under_typesum : 'b
  end
end

let makeFamilly syntax =
  let module Printer = struct
    module Generic = (val
                        (match syntax with
                         | OpaSyntax.Classic ->
                             OManager.verbose "Select class printer";
                             (module Classic : SGeneric)
                         | OpaSyntax.Js ->
                             OManager.verbose "Select js printer";
                             (module Js : SGeneric))
                          : SGeneric)
    class string_class =
    object (self)
      inherit [string] Generic.printer as super

      method classify_string  s = classify_string self#keyword s

      method is_operator s = self#classify_string s = `operator
      method to_protected_ident s =
        match classify_string self#keyword s with
        | `operator
        | `backquote -> "`"^s^"`"
        | `ident -> s
      method to_unprotected_ident s =
        assert (self#classify_string s <> (`backquote :> ident_kind ));
        s

      method typeident f (Typeident s) = Format.pp_print_string f s
      method typeident_original_name s = s
      method typevar f (Flatvar s) = pp f "'%s" s

      method directive f = function
        | ((`xml_parser x : [< all_directives ]),_,_) -> self#xml_parser f x
        | (`parser_ x,_,_) -> self#trx_expr f x
        | d -> super#directive f d

      method xml_parser f l =
        pp f "@[<v>xml_parser@ | %a@ end@]" (list "@ | " self#xml_rule) l
      method xml_rule f (pl,e) =
        pp f "@[<2>%a -> @ %a@]" (list "@ " self#xml_named_pattern) pl self#expr e
      method xml_named_pattern f (nameo,p,s) =
        match nameo with
        | None -> pp f "%a%a" self#xml_pattern p self#xml_suffix_option s
        | Some name -> pp f "%s = %a%a" name self#xml_pattern p self#xml_suffix_option s
      method xml_suffix_option f = function
        | None -> ()
        | Some suffix -> self#label self#xml_suffix f suffix
      method xml_suffix f = function
        | Xml_star -> pp f "*"
        | Xml_plus -> pp f "+"
        | Xml_question -> pp f "?"
        | Xml_number e -> pp f "{%a}" self#expr e
        | Xml_range (e1,e2) -> pp f "{%a,%a}" self#expr e1 self#expr e2
      method xml_pattern f = function
        | XmlLetIn (bindings,p) -> pp f "/* encoded let and */@\n@[<v>%a@ %a@]" self#bindings bindings self#xml_pattern p
        | XmlExpr e -> pp f "{%a}" self#expr e
        | XmlNode (ns,attrs,[]) -> pp f "<%a%s%a/>" self#xml_namespace ns (if attrs <> [] then " " else "") (list " " self#xml_attribute) attrs
        | XmlNode (ns,attrs,children) -> pp f "@[<v>@[<v2><%a%s%a>@ %a@]@ </>@]" self#xml_namespace ns (if attrs <> [] then " " else "") (list " " self#xml_attribute) attrs (list "@ " self#xml_named_pattern) children
        | XmlAny -> pp f "_"
        | XmlParser items -> pp f "parser %a" (list "@ " self#trx_item) items
      method xml_attribute f (ns,_bound_opt,xml_attribute_value) =
        match xml_attribute_value with
        | XmlExists -> pp f "%a = _" self#xml_namespace ns
        | XmlName -> self#xml_namespace f ns
        | XmlAttrStringParser e -> pp f "%a = \"{%a}\"" self#xml_namespace ns self#expr e
        | XmlAttrParser e -> pp f "%a = %a" self#xml_namespace ns self#expr e
      method xml_namespace f = function
        | {namespace=ns; name=name} -> pp f "%a:%a" self#expr ns self#string_label name
      method string_label f p = self#label Format.pp_print_string f p

      method trx_expr ?(sub=false) f c = self#label (self#trx_expr_node ~sub) f c
      method trx_expr_node ~sub f (Trx_ast.Expr l) =
        match l with
        | p :: [] when sub -> pp f "%a" self#trx_seq p
        | p :: []          -> pp f "parser@ %a" self#trx_seq p
        | _  -> pp f "@[<v 0>parser@ %a" (list "@]@ @[<2>| " self#trx_seq) l
      method trx_seq f l = self#label self#trx_seq_node f l
      method trx_seq_node f {Trx_ast.seq_items=seq_items; Trx_ast.seq_code=seq_code} =
        match seq_code with
        | None -> list "@ " self#trx_item f seq_items
        | Some e -> pp f "%a ->@ %a" (list "@ " self#trx_item) seq_items self#expr e
      method trx_item f i = self#label self#trx_item_node f i
      method trx_item_node f {Trx_ast.item_name=nameo; Trx_ast.item_prefix=pref; Trx_ast.item_primary=prim; Trx_ast.item_suffix=suff} =
        match nameo with
        | None -> pp f "%a%a%a" self#trx_prefix pref self#trx_primary prim self#trx_suffix suff
        | Some name -> pp f "%s = %a%a%a" name self#trx_prefix pref self#trx_primary prim self#trx_suffix suff
      method trx_primary f p = self#label self#trx_primary_node f p
      method trx_primary_node f = function
        | Trx_ast.Parens e -> pp f "(%a)" (self#trx_expr ~sub:true) e
        | Trx_ast.Literal (name,case) -> pp f "%S%s" name (if case then "" else "~")
        | Trx_ast.DynamicLiteral e -> pp f "\"{%a}\"" self#expr e
        | Trx_ast.Code e -> pp f "{%a}" self#expr e
        | Trx_ast.Rule e -> self#expr f e
        | Trx_ast.Any -> pp f "."
        | Trx_ast.Class ranges -> pp f "[%a]" (list "" self#trx_range) ranges
      method trx_range f = function
        | `ONE i -> (try pp f "%c" (Char.chr i) with Invalid_argument _ -> pp f "\\u%d" i)
        | `RANGE (i1,i2) -> (try pp f "%c-%c" (Char.chr i1) (Char.chr i2) with Invalid_argument _ -> pp f "\\u%d-\\u%d" i1 i2)
      method trx_prefix f = function
        | `AND -> pp f "&"
        | `NOT -> pp f "!"
        | `NORMAL -> ()
      method trx_suffix f = function
        | `QUESTION -> pp f "?"
        | `STAR -> pp f "*"
        | `PLUS -> pp f "+"
        | `NORMAL -> ()

    end

    class string_and_pos_class =
    object
      inherit string_class
      method label p f (v,label) = pp f "%a@ /*%s*/" p v (FilePos.to_string label.QmlLoc.pos)
    end

    class ident_class =
    object
      inherit [Ident.t] Generic.printer
      method is_operator = Ident.is_operator
      method to_protected_ident i = Ident.opa_syntax ~dont_protect_operator:false i
      method to_unprotected_ident i = Ident.opa_syntax ~dont_protect_operator:true i
      method typeident f (Typeident s) = pp f "%s" (Ident.original_name s)
      method typeident_original_name = Ident.original_name
      method typevar f (Flatvar s) = pp f "'%s" (Ident.to_string s)
    end
    let alphanumeric = Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*"

    class readable_ident_class =
    object
      inherit ident_class
      method to_protected_ident i =
        let n = Ident.original_name i in
        if string_complete_match alphanumeric n then n else "`" ^ n ^ "`"
      method to_unprotected_ident = Ident.original_name
      method typeident f (Typeident s) = pp f "%s" (Ident.original_name s)
      method typevar f (Flatvar s) = pp f "'%s" (Ident.original_name s)
    end

    class full_ident_class =
    object
      inherit ident_class
      method typeident f (Typeident s) = pp f "%s" (Ident.to_string s)
    end

  end in (
    module struct
      let string = (new Printer.string_class :> string printer)
      let string_and_pos = (new Printer.string_and_pos_class :> string printer)
      let ident = (new Printer.ident_class :> Ident.t printer)
      let readable_ident = (new Printer.readable_ident_class :> Ident.t printer)
      let full_ident = (new Printer.full_ident_class :> Ident.t printer)
    end : Familly)

let getDefaultFamilly () =
  (module (val (makeFamilly !(OpaSyntax.Args.r).OpaSyntax.Args.printer) : Familly) : Familly)

include (val (makeFamilly OpaSyntax.Js) : Familly)
