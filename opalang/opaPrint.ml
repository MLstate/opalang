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
(* FIXME: rebel open *)
open SurfaceAst

(* depends *)
module Format = Base.Format
module List = Base.List

(* shorthand *)
module S = SurfaceAst

(* refactoring in progress *)

(* -- *)

type 'a pprinter = 'a Format.pprinter
let pp = Format.fprintf
let list = Format.pp_list


let regular_ident_regexp =    Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*" (* same as the parser *)
let regular_typeident_regexp = Str.regexp "[a-zA-Z_][a-zA-Z0-9_.]*"
let keyword = function
  | "match" | "with" | "type" | "do" | "if" | "then" | "else" | "as" | "_" -> true
  | _ -> false
let basic_type = function
  | "int" | "string" | "float" -> true
  | _ -> false
let operator_regexp = Str.regexp "[-.+\\^*/<>=@|&!]+"
let classify_string s =
  if keyword s then `backquote
  else if Str.string_match regular_ident_regexp s 0 then `ident
  else if Str.string_match operator_regexp s 0 then `operator
  else `backquote
let classify_typeident s =
  if keyword s || basic_type s then `backquote
  else if Str.string_match regular_ident_regexp s 0 then `ident
  else `backquote


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
        Some (List.rev acc, None) (* [1,2] *)
    | _ ->
        if acc = [] then None else Some (List.rev acc, Some e) (* [1,2|e] *)

  let sugar_tuple original_name e =
    match extract_coercion e with
    | Some ((Record l,_), (TypeNamed (Typeident name,_),_)) ->
        (try
           let n = Scanf.sscanf (original_name name) "tuple_%d" (fun x -> x) in
           if n = List.length l && n <> 0 then
             let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
             if List.for_alli (fun i (f,_) -> f = "f"^string_of_int (i+1)) l then
               Some (List.map snd l)
             else
               None
           else
             None
         with
         | End_of_file
         | Scanf.Scan_failure _ -> None)
    | _ -> None

  let sugar_if = function
    | Match (e1,[
               ((PatRecord ([("true", _)], `closed), _)  | (PatCoerce ((PatRecord ([("true", _)], `closed), _), _), _)), e2 ;
               ((PatRecord ([("false", _)], `closed), _) | (PatCoerce ((PatRecord ([("false", _)], `closed), _), _), _)), e3 ;
             ]), _ -> Some (e1,e2,e3)
    | _ -> None

  let resugarer original_name e =
    match sugar_list original_name [] e with
    | Some (head,rest) -> `list (head,rest)
    | None ->
        match sugar_tuple original_name e with
        | Some l -> `tuple l
        | None ->
            match sugar_if e with
            | Some (e1,e2,e3) -> `if_ (e1,e2,e3)
            | None -> `no_sugar
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
    match extract_coercion e with
    | Some ((PatRecord (l, `closed), _), (TypeNamed (Typeident name,_),_)) ->
        (try
           let n = Scanf.sscanf (original_name name) "tuple_%d" (fun x -> x) in
           if n = List.length l && n <> 0 then
             let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
             if List.for_alli (fun i (f,_) -> f = "f"^string_of_int (i+1)) l then
               Some (List.map snd l)
             else
               None
           else
             None
         with
         | End_of_file
         | Scanf.Scan_failure _ -> None)
    | _ -> None

  let resugarer original_name p =
    match sugar_list original_name [] p with
    | Some (head,rest) -> `list (head,rest)
    | None ->
        match sugar_tuple original_name p with
        | Some l -> `tuple l
        | None -> `no_sugar
end

type userland_visibility_directive = QmlAst.userland_visibility_directive
type all_directives = SurfaceAst.all_directives

let userland_visibilities_to_whatever ds =
    (ds : userland_visibility_directive list :> [> all_directives] list)

class virtual ['ident] generic_printer =
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
  method under_forall = {< forall = true >}
  method under_arrow = {< arrow = true >}
  method under_colon = {< colon = true >}
  method under_op = {< op = true >}
  method reset = {< typesum = false;
                    comma = false;
                    forall = false;
                    arrow = false;
                    colon = false;
                    op = false >}

  method label : 'a. 'a pprinter -> 'a QmlLoc.label pprinter = fun p f v -> p f (fst v)

  method const_expr f = function
    | CInt i -> Format.pp_print_string f (Big_int.string_of_big_int i)
    | CFloat float -> Format.pp_print_float f float
    | CString s -> pp f "\"%s\"" (QmlPrint.escaped_string s)

  method field f s =
    Format.pp_print_string f (
      match classify_string s with
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

  method const_ty_node f const =
    Format.pp_print_string f (
      match const with
      | TyInt -> "int"
      | TyFloat -> "float"
      | TyString -> "string"
    )
  method rowvar f _ = Format.pp_print_string f "..."
  method colvar f _ = Format.pp_print_string f "..."
  method virtual typeident : 'ident typeident pprinter
  method virtual typevar : 'ident typevar pprinter
  method virtual typeident_original_name : 'ident -> string
  method row_t f x = self#label self#row_t_node f x
  method row_t_node f (TyRow (nodes, rowo)) =
    match nodes, rowo with
    | [], None -> Format.pp_print_string f "{}"
    | [], Some rowvar -> pp f "{%a}" self#rowvar rowvar
    | l, None -> pp f "@[<1>{%a}@]" (list ";@ " self#typerecordbinding) l
    | l, Some rowvar -> pp f "@[<1>{%a;@ %a}@]" (list ";@ " self#typerecordbinding) l self#rowvar rowvar
  method typerecordbinding f (s,ty) =
    match ty with
    | (TypeRecord (TyRow ([],None)),_) -> Format.pp_print_string f s
    | _ -> pp f "%a: %a" self#field s self#reset#ty ty
  method sum_t f v = self#label self#sum_t_node f v
  method sum_t_node f = function
    | SumName typeinstance -> self#typeinstancenode f typeinstance
    | SumRecord r -> self#row_t_node f r
    | SumVar c -> self#colvar f c
  method arrow_t f v = self#label self#arrow_t_node f v
  method arrow_t_node f (row_t,ty) =
    pp f "@[<2>%a ->@ %a@]" self#under_comma#arrow_row_t row_t self#under_arrow#ty ty
  method arrow_row_t f v = self#label self#arrow_row_t_node f v
  method arrow_row_t_node f (TyRow (l,row)) =
    assert (row = None);
    list ",@ " self#arrowbinding f l
  method arrowbinding f (_,ty) = self#ty f ty
  method typeinstance f t = self#label self#typeinstancenode f t
  method typeinstancenode f (ident,params) =
    match params with
    | [] -> self#typeident f ident
    | _ -> pp f "@[@[<2>%a(%a@])@]" self#typeident ident (list ",@ " self#under_comma#ty) params
  method typeforall f (vars,ty) =
    pp f "@[<2>forall(@[<h>%a@]) %a@]" (list ",@ " self#typevar) vars self#under_forall#ty ty
  method typesumsugar f l =
    pp f "@[<v>  %a@]" (list "@ / " self#under_typesum#sum_t) l
  method typemodule f fields =
    pp f "@[@{<v2>{{@ %a@ @]}}@]" (list "@ " self#typerecordbinding) fields

  (*-------------------------*)
  (*---- pattern printer ----*)
  (*-------------------------*)
  method is_tilde_field : 'a. ('a -> 'ident option) -> string * 'a -> bool =
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

  method pat_record_binding f ((s, p) as pat) =
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

  method pat_record f fields rowvar =
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
  method unprotected_ident f i = Format.pp_print_string f (self#to_unprotected_ident i)
  method expr : 'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f e ->
    match ExprSugar.resugarer self#typeident_original_name e with
    | `list (head,None) -> pp f "@[<2>[%a]@]" (list ",@ " self#expr) head
    | `list (head,Some rest) -> pp f "@[<2>[%a@ |@ %a]@]" (list ",@ " self#expr) head self#expr rest
    | `tuple el -> pp f "@[<1>(%a)@]" (list ",@ " self#expr) el
    | `if_ (e1,e2,e3) -> pp f "@[<2>if %a@;<1 -2>then@ %a@;<1 -2>else@ %a@]" self#expr e1 self#expr e2 self#expr e3
    | `no_sugar -> self#label self#expr_node f e
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
  | DBPath (elt,kind) -> pp f "%a%a" self#db_kind kind self#db_elt elt
  | Directive d -> self#directive f d
  method apply_expr : 'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f -> function
  | (Ident _,_)
  | (Apply _,_)
  | (Directive _ ,_)
  | (Dot _, _) as e -> self#expr f e
  | e -> pp f "(%a)" self#reset#expr e
  method db_kind f kind =
    Format.pp_print_string f (
      match kind with
      | QmlAst.Db.Option -> "?"
      | QmlAst.Db.Default -> ""
      | QmlAst.Db.Valpath -> "!"
      | QmlAst.Db.Ref -> "@"
    )
  method db_elt : 'dir. ('ident,[< all_directives ] as 'dir) dbelt pprinter = fun f e ->
    self#label self#db_elt_node f e
  method db_elt_node : 'dir. ('ident,[< all_directives ] as 'dir) dbelt_node pprinter = fun f l ->
    list "" self#db_element f l
  method db_element : 'dir. ('ident,[< all_directives ] as 'dir) preprocessed_db_element pprinter = fun f e ->
    self#label self#db_element_node f e
  method db_element_node : 'dir. ('ident,[< all_directives ] as 'dir) preprocessed_db_element_node pprinter = fun f -> function
  | FldKey s -> pp f "/%a" self#field s
  | ExprKey e -> pp f "[%a]" self#reset#expr e
  | NewKey -> Format.pp_print_string f "[?]"
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
  | `visibility_annotation _ -> Format.pp_print_string f "visibility_annotation"
  | `static_content (s, eval) -> pp f "static_content[%s][%b]" s eval
  | `static_content_directory (s, eval) -> pp f "static_content_directory[%s][%b]" s eval
  | `static_resource s -> pp f "static_resource[%s]" s
  | `static_resource_directory s -> pp f "static_resource_directory[%s]" s
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

  method string_elmt :  'dir. ('ident,[< all_directives ] as 'dir) expr pprinter = fun f (e,_) ->
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


  method pat_directive f (v:SurfaceAst.all_directives) = pp f "@[<2>@@%a@]" self#variant v

  method pat_directives f (vs:SurfaceAst.all_directives list) = pp f "%a" (Format.pp_list "@ " self#pat_directive) vs

  method record_binding  : 'dir. (string * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f ((s, e) as expr) ->
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
          pp f "@[<2>%a =@ %a@]" self#field s self#reset#expr e

  method record :
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


  method lambda_binding : 'a 'dir. 'a pprinter -> ('a * (string * 'ident pat) list * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun p f (s,r,e) ->
    pp f "%a(%a) =@ %a" p s (list ",@ " self#reset#under_comma#pat) (List.map snd r) self#expr e
  method binding : 'dir. ('ident * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (i,e) ->
    match e with
    | (Directive ((`magic_do : [< all_directives ]), [e], _),_) -> pp f "@[<2>do %a@]" self#expr e
    | (Lambda (r,e),_LABEL) -> self#lambda_binding self#ident f (i,r,e)
    | _ -> pp f "%a =@ %a" self#ident i self#expr e
  method bindings : 'dir. ('ident * ('ident, [< all_directives ] as 'dir) expr) list pprinter = fun f iel ->
    let il,el = List.split iel in
    pp f "@[<2>(%a) =@ (%a)@]" (list ",@ " self#ident) il (list ",@ " self#expr) el
  method rule_ : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (p,e) ->
    pp f "@[<2>%a ->@ %a@]" self#under_arrow#pat p self#expr e
  method side f = function
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
  method path_decl_key f = function
  | QmlAst.Db.Decl_fld s -> pp f "/%a" self#field s
  | QmlAst.Db.Decl_int -> pp f "@@fixme<db_decl_int>"
  | QmlAst.Db.Decl_string -> pp f "@@fixme<db_decl_string>"
  | QmlAst.Db.Decl_set [] -> pp f "[_]"
  | QmlAst.Db.Decl_set (_ :: _) -> pp f "@@fixme<db_decl_set>"
  method path_decl f l = list "" self#path_decl_key f l
  method db_def : 'dir. (('ident, [< all_directives] as 'dir) expr, 'ident ty) QmlAst.Db.db_def pprinter = fun f -> function
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
  | Database (ident,[name],[`engine (`db3 (Some s))]) -> pp f "database /* %a */ %s %s" self#ident ident name s
  | Database (ident,[],[`engine (`db3 (Some s))]) -> pp f "database /* %a */ %s" self#ident ident s
  | Database (ident,[name],[`engine (`db3light (Some s))]) -> pp f "database /* %a */ %s %s" self#ident ident name s
  | Database (ident,[],[`engine (`db3light (Some s))]) -> pp f "database /* %a */ %s" self#ident ident s
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
  | NewVal ([bnd],false) -> pp f "@[<2>%a@] "self#pat_binding bnd
  | NewVal (pel,false) ->
      pp f "/* encoding of a let and */@\n%a" self#pat_bindings pel
  | NewVal (pel,true) ->
      pp f "@[<v>@[<2>rec %a@]@]" (list "@]@ and @[" self#pat_binding) pel
  | Package (`import,s) ->
      pp f "import %s" s
  | Package (`import_plugin, s) ->
      pp f "import-plugin %s" s
  | Package (`declaration,s) ->
      pp f "package %s" s
  method pat_binding : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) pprinter = fun f (p,e) ->
    match p, e with
    | (PatVar i,_LABEL1), (Lambda (r,e),_LABEL2) -> self#lambda_binding self#ident f (i.SurfaceAst.ident,r,e)
    | _, (Directive ((`visibility_annotation `public b : [< all_directives ]),[e],_),_) -> (
        match b with
        | `async -> pp f "publish %a" self#pat_binding (p,e)
        | `sync -> pp f "publish_async %a" self#pat_binding (p, e)
        | `funaction -> assert false
      )
    | _, (Directive (`side_annotation side,[e],_),_) -> pp f "%a %a" self#side side self#pat_binding (p,e)
    | _ -> pp f "@[<2>%a =@ %a@]" self#pat p self#expr e
  method pat_bindings : 'dir. ('ident pat * ('ident, [< all_directives ] as 'dir) expr) list pprinter = fun f pel ->
    let pl,el = List.split pel in
    pp f "@[<2>(%a) =@ (%a)@]" (list ",@ " self#pat) pl (list ",@ " self#expr) el

  method typedef_visibility f = function
    | SurfaceAst.TDV_public -> ()
    | SurfaceAst.TDV_abstract _ -> pp f "@@abstract@ "
    | SurfaceAst.TDV_private _ -> pp f "@@private@ "

  method typedef ~print_visibility f ty_def =
    self#label (self#typedef_node ~print_visibility) f ty_def
  method typedef_node ~print_visibility f ty_def =
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



class string_class =
object (self)
  inherit [string] generic_printer as super
  method is_operator s = classify_string s = `operator
  method to_protected_ident s =
    match classify_string s with
    | `operator
    | `backquote -> "`"^s^"`"
    | `ident -> s
  method to_unprotected_ident s =
    assert (classify_string s <> `backquote);
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

  method trx_expr f c = self#label self#trx_expr_node f c
  method trx_expr_node f (Trx_ast.Expr l) =
    pp f "@[<v2>parser@ @[<2>| %a@]@]" (list "@]@ @[<2>| " self#trx_seq) l
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
  | Trx_ast.Parens e -> pp f "(%a)" self#trx_expr e
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
  inherit [Ident.t] generic_printer
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
    if Str.string_match alphanumeric n 0 then n else "`" ^ n ^ "`"
  method to_unprotected_ident = Ident.original_name
  method typeident f (Typeident s) = pp f "%s" (Ident.original_name s)
  method typevar f (Flatvar s) = pp f "'%s" (Ident.original_name s)
end

class full_ident_class =
object
  inherit ident_class
  method typeident f (Typeident s) = pp f "%s" (Ident.to_string s)
end

type 'ident printer =
  <
  code : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code Format.pprinter;
  code_elt : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code_elt Format.pprinter;
  code_elt_node : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code_elt_node Format.pprinter;
  expr : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.expr Format.pprinter;
  ty : 'ident SurfaceAst.ty Format.pprinter;
  directive : 'dir. ('ident,[< SurfaceAst.all_directives ] as 'dir) SurfaceAst.directive Format.pprinter;
  variant : 'dir. ([< SurfaceAst.all_directives ] as 'dir) Format.pprinter;
  typevar : 'ident SurfaceAst.typevar Format.pprinter;
  typeident : 'ident SurfaceAst.typeident Format.pprinter;
  ident : 'ident Format.pprinter;
  >

let string = (new string_class :> string printer)
let string_and_pos = (new string_and_pos_class :> string printer)
let ident = (new ident_class :> Ident.t printer)
let readable_ident = (new readable_ident_class :> Ident.t printer)
let full_ident = (new full_ident_class :> Ident.t printer)
