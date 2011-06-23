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

(**
   Printers of QML AST.
   @author Vincent Benayoun
   @author Mikolaj Konarski
   @author Mathieu Barbin
   @author Valentin Gatien-Baron
   @author Rudy Sicard
   @author Mehdi Bouaziz
   @author David Rajchenbach-Teller
   @author Louis Gesbert
*)

(**
   This module defines some printers for working on Qml AST.

   New version for printer is based on Format, and object inheritence.

   For each type [t] of the AST, there is a method called [t] which is
   of type [Format.formatter -> t -> unit].

   Then, it is easy to inherit from the default object for changing just a few cases.
   By default, at end of the file, there is some alias for hiding the object
   implementation for user who does not need to use several printers.

   {[
   class default =
   object(self)

      method pat ... : Format.formatter -> QmlAst.pat -> unit
      method expr ...: Format.formatter -> QmlAst.expr -> unit
      ....
      method code....: Format.formatter -> QmlAst.code -> unit
   end

   (* exporting default printer to the top level *)
   let pat = default#pat
   let expr = default#expr
   ....

   (* custom printer *)
   inherit, and overwrite any method.
   ]}

   The old printer is deprecated and will be removed (but this means changes in a lot of modules).
*)

(* depends *)
module Format = BaseFormat
module List = BaseList
module String = BaseString

(* refactoring *)

(* alias *)

(* shorthands *)
module Q = QmlAst

(* -- *)

(**
   In opa, string can contains ["an acces to a {variable}"].
   So, any char ['{'] from a row string should be escaped.
*)
let escaped_string s =
  let s = String.escaped s in
  String.replace s "{" "\\{"

(*
  TODO: if possible (no problems of cyclic dependancies,
  put this function in qmlDirectives.ml,
  and remove the duplication of '@' as first char.
*)
let directive (d:QmlAst.qml_directive) =
  match d with
  | `deprecated -> "@deprecated"
  | `todo -> "@todo"
  | `at_init -> "@at_init"
  | `module_ -> "@module"
  | `module_field_lifting -> "@module_field_lifting"
  | `coerce -> "@coerce"
  | `nonexpansive -> "@nonexpansive"
  | `unsafe_cast -> "@unsafe_cast"
  | `opensums -> "@opensums"
  | `openrecord -> "@openrecord"
  | `assert_ -> "@assert"
  | `typeof -> "@typeof"
  | `atomic -> "@atomic"
  | `immovable -> "@immovable"
  | `thread_context -> "@thread_context"
  | `with_thread_context -> "@with_thread_context"
  | `js_ident -> "@js_ident"
  | `throw -> "@throw"
  | `catch -> "@catch"
  | `spawn -> "@spawn"
  | `wait -> "@wait"
  | `callcc -> "@callcc"
  | `restricted_bypass pass -> "@restricted_bypass["^ pass ^ "]"
  | `fail -> "@fail"
  | `create_lazy_record -> "@create_lazy_record"
  | `warncoerce -> "@warncoerce"
  | `apply_ty_arg _ -> "@apply_ty_arg _"
  | `abstract_ty_arg _ -> "@abstract_ty_arg _"
  | `closure_create _ -> "@closure_create"
  | `closure_apply -> "@closure_apply"
  | `closure_create_no_function _ -> "@closure_create_no_function"
  | `closure_define_function _ -> "@closure_define_function"
  | `ajax_publish b -> Printf.sprintf "@ajax_publish(%s)" (match b with `sync -> "`sync" | `async -> "`async")
  | `ajax_call b -> Printf.sprintf "@ajax_call(%s)" (match b with `sync -> "`sync" | `async -> "`async")
  | `comet_publish -> "@comet_publish"
  | `comet_call -> "@comet_call"
  | `insert_server_value i -> Printf.sprintf "@insert_server_value(%s)" (Ident.to_string i)
  | `doctype _ -> "@doctype"
  | `hybrid_value -> "@hybrid_value"
  | `backend_ident s -> Printf.sprintf "@backend_ident[%s]" s
  | `tracker _ -> "@track"
  | `expand _ -> "@expand"
  | `fun_action None -> "@fun_action"
  | `fun_action (Some Q.Client_id) -> "@fun_action[Client_id]"
  | `fun_action (Some Q.Deserialize) -> "@fun_action[Deserialize]"
  | `cps_stack_lambda _ -> "@cps_stack_lambda"
  | `cps_stack_apply _ -> "@cps_stack_apply"
  | `async -> "@async"
  | `sliced_expr -> "@sliced_expr"
  | `may_cps -> "@may_cps"
  | `stringifier -> "@stringifier"
  | `comparator -> "@comparator"
  | `serializer -> "@serializer"
  | `xmlizer -> "@xmlizer"
  | `llarray -> "@llarray"
  | `specialize variant -> Printf.sprintf "@specialize%s" (match variant with `strict -> "_strict" | `polymorphic -> "")
  | `partial_apply (None, ser) -> Printf.sprintf "@partial_apply[ser:%B]" ser
  | `partial_apply (Some i, ser) -> Printf.sprintf "@partial_apply[missing:%d,ser:%B]" i ser
  | `full_apply n -> Printf.sprintf "@full_apply[env %d]" n
  | `lifted_lambda (n,l) ->
      Format.sprintf "@@lifted_lambda[env %d,[%a]]"
        n
        (Format.pp_list "@ " (fun f i -> Format.pp_print_string f (Ident.to_string i))) l
  | `tagged_string (s, kind) ->
      Printf.sprintf "@tagged_string[%S, %s]" s
        (match kind with
         | Q.Rpc_use -> "rpc_use"
         | Q.Rpc_def -> "rpc_def"
         | Q.Type_def -> "type_def"
         | Q.Type_use -> "type_use"
         | Q.Client_closure_use -> "client_closure_use")
  | `apply_cont -> "@apply_cont"
  | `recval -> "@recval"
  | `side_annotation a -> (
      match a with
      | `server -> "@server"
      | `client -> "@client"
      | `both -> "@both"
      | `prefer_server -> "@prefer_server"
      | `prefer_client -> "@prefer_client"
      | `prefer_both -> "@prefer_both"
      | `both_implem -> "@both_implem"
    )
  | `visibility_annotation `private_ -> "@server_private"
  | `visibility_annotation (`public `sync) -> "@publish"
  | `visibility_annotation (`public `async) -> "@publish_async"
  | `visibility_annotation (`public `funaction) -> "@publish_funaction"

(* ************************************************************************** *)
(** {b Descr}: Returns the string corresponding to a type definition
    visibility suitable to be printed *before* the "type" token of a type
    definition pretty print string.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let type_def_visibility = function
  | Q.TDV_public -> ""
  | Q.TDV_abstract _ -> "@abstract "
  | Q.TDV_private _ -> "@private "



let pp = Format.fprintf
let pp_list = Format.pp_list

let regroup_patfield = function
  | Q.PatRecord (_, fields, rowvar) ->
      fields, rowvar = `open_
  | _ -> assert false
let rec regroup_extend_record ?(acc=[]) = function
  | Q.ExtendRecord (_, f, d, r) -> regroup_extend_record ~acc:((f,d) :: acc) r
  | e -> e, List.rev acc
let is_infix s = Ident.is_operator s

class base_printer =
object (self)

  (* handling of priorities
   * when [op] is true, we are under an operator
   * when [arrow] is true, we are on the lhs of an arrow
   * when [amper] is true, we are just under a '&'
   * when [comma] is true, we are just inside a tuple or a lambda binding
   * when [record] is true, we are just under a record binding
   *)
  val op = false
  val arrow = false
  val amper = false
  val comma = false
  val record = false
  val coerce = false

  method reset =
    {<
      op = false;
      arrow = false;
      amper = false;
      comma = false;
      record = false;
      coerce = false;
    >}

  method under_op = {< op = true >}
  method under_arrow = {< arrow = true >}
  method under_amper = {< amper = true >}
  method under_comma = {< comma = true >}
  method under_record = {< record = true >}
  method under_coerce = {< coerce = true >}

  (* annot printer *)
  method expr_node fmt expr =
    self#expr0 fmt expr

  method pat_node fmt pat =
    self#pat0 fmt pat

  method ident_to_string i = Ident.opa_syntax i
  method ident f i = Format.pp_print_string f (self#ident_to_string i)

  (*--------------------*)
  (*--- type printer ---*)
  (*--------------------*)
  method ty f = function
    | Q.TypeArrow _ as t when comma -> pp f "(%a)" self#reset#ty t
    | Q.TypeArrow _ as t when arrow -> pp f "(%a)" self#reset#ty t
    | Q.TypeForall _ as t when arrow || comma -> pp f "(%a)" self#reset#ty t
    | Q.TypeConst const -> Format.pp_print_string f (Q.Const.string_of_ty const)
    | Q.TypeVar typevar -> self#typevar f typevar
    | Q.TypeArrow (lty1, ty2) -> pp f "@[<2>%a ->@ %a@]" (pp_list ",@ " self#under_arrow#ty) lty1 self#under_arrow#ty ty2
    | Q.TypeRecord row -> self#reset#tyrow f row
    | Q.TypeSum ty_col -> self#tysum f ty_col
    | Q.TypeSumSugar tyl -> pp f "@[%a@]" (pp_list "@ /@ " self#ty) tyl
    | Q.TypeName ([],t) -> self#typeident f t
    | Q.TypeName (tyl,t) -> pp f "@[<2>%a(%a)@]" self#typeident t (pp_list ",@ " self#reset#ty) tyl
    | Q.TypeAbstract -> pp f "external"
    | Q.TypeForall (tyvl,rowl,coll,ty) -> self#scheme f tyvl rowl coll ty
  method typeident f t = pp f "%s" (Q.TypeIdent.to_printable_string t)
  method typevar f t = Format.pp_print_string f (QmlTypeVars.TypeVar.to_string t)
  method quant_colvar f t = Format.pp_print_string f (QmlTypeVars.ColVar.to_string t)
  method quant_rowvar f t = Format.pp_print_string f (QmlTypeVars.RowVar.to_string t)
  method colvar = self#quant_colvar
  method rowvar = self#quant_rowvar
  method tyrow f (Q.TyRow (fields,rowvar)) =
    pp f "@[<hv2>{%a%t}@]"
      (pp_list ";@ " self#tyrow_binding) fields
      (fun f ->
         match rowvar with
         | None -> ()
         | Some v -> Format.fprintf f "%s%a" (if fields = [] then "" else "; ") self#rowvar v)

  (*
    Can be overwritten in a class having a gamma, if needed
  *)
  method is_type_void ty =
    match ty with
    | Q.TypeRecord (Q.TyRow ([], None))
    | Q.TypeSum (Q.TyCol ([ [ ] ], None)) ->
        true
    | _ -> false

  method tyrow_binding f (s, ty) =
    if self#is_type_void ty
    then
      Format.pp_print_string f s
    else
      pp f "@[<h>%s :@ %a@]" s self#ty ty

  method tycol = self#tysum
  method tysum f (Q.TyCol (fl, colvar)) =
    (* Attention, if the sum type is closed and contains no row (i.e. a trivial
       sum type with no possible cases), the printed type would be an empty
       string, which would be very confusing ! So, manually take care of this
       case. *)
    if (List.length fl = 0) && colvar = None then
       pp f "<empty sum type>"
    else
      pp f "@[<2>%a%t@]"
        (pp_list "@ /@ " (fun f -> pp f "@[{%a}@]" (pp_list ";@ " self#tyrow_binding))) fl
        (fun f ->
           match colvar with
           | None -> ()
           | Some v -> pp f "@ /@ %a" self#colvar v)

  method typedef f tdef =
    let visibility_str = type_def_visibility tdef.Q.ty_def_visibility in
    match tdef.Q.ty_def_params with
    | [] ->
        pp f "@[<2>%stype %a =@ %a@]"
          visibility_str
          self#typeident tdef.Q.ty_def_name self#ty tdef.Q.ty_def_body
    | _ ->
        pp f "@[<2>%stype %a(%a) =@ %a@]"
          visibility_str
          self#typeident tdef.Q.ty_def_name
          (pp_list ",@ " self#typevar) tdef.Q.ty_def_params
          self#ty tdef.Q.ty_def_body

  method scheme f vars rvars cvars ty =
    if rvars = [] && cvars = [] then
      pp f "@[<2>forall(@[<h>%a@]).@ %a@]"
        (pp_list ",@ " self#typevar) vars
        self#ty ty
    else
      pp f "@[<2>forall(@[<h>%a,@ rows:%a,@ cols:%a@]).@ %a@]"
        (pp_list ",@ " self#typevar) vars
        (pp_list ",@ " self#rowvar) rvars
        (pp_list ",@ " self#colvar) cvars
        self#ty ty

  method tsc f tsc =
    let (quant,ty,()) = QmlGenericScheme.export_unsafe tsc in
    let (vars, rvars, cvars) = QmlTypeVars.FreeVars.export_as_lists quant in
    self#scheme f vars rvars cvars ty

  (*---------------------*)
  (*-- pattern printer --*)
  (*---------------------*)
  method is_tilde_field : 'a. ('a -> Ident.t option) -> string * 'a -> bool =
    (fun getvar (field, pat) ->
      match getvar pat with
      | Some ident ->
          let ident = self#ident_to_string ident in
          String.compare field ident = 0
      | None -> false
    )

  method pat_record_binding f ((s, p) as pat) =
    match p with
    | Q.PatRecord (_, [], `closed)
    | Q.PatCoerce (_, Q.PatRecord (_, [], `closed), _)
        ->
        Format.pp_print_string f s
    | _ ->
        let getvar = function
          | Q.PatVar (_, i) -> Some i
          | _ -> None
        in
        if self#is_tilde_field getvar pat
        then
          pp f "~%s" s
        else
          pp f "@[<h>%s =@ %a@]" s self#pat p

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
            | Q.PatVar (_, i) -> Some i
            | _ -> None
          in
          self#is_tilde_field getvar field
        in
        if List.for_all is_tilde_field fields
        then
          let pp_field f (field, _) = Format.pp_print_string f field in
          pp f "@[<hv2>~{ %a%s }@]"
            (pp_list "@, " pp_field) fields
            rowvar
        else
          pp f "@[<hv2>{ %a%s }@]"
            (pp_list " ;@ " self#pat_record_binding) fields
            rowvar

  method pat0 f = function
    | Q.PatRecord (_, fields, rowvar) -> self#pat_record f fields rowvar
    | Q.PatConst (_, Q.String s) -> Format.fprintf f "\"%s\"" (escaped_string s)
    | Q.PatConst (_, const) -> Format.pp_print_string f (Q.Const.string_of_expr const)
    | Q.PatVar (_, i) -> self#ident f i
    | Q.PatAny _ -> pp f "_"
    | Q.PatCoerce (_, p, ty) -> pp f "(@[<2>%a :@ %a@])" self#pat p self#ty ty
    | Q.PatAs (_, p, i) -> pp f "@[<2>%a as %a@]" self#pat p self#ident i
  method pat f v =
    self#pat_node f v

  method const f = function
  | Q.String s -> Format.fprintf f "\"%s\"" (escaped_string s)
  | c -> Format.pp_print_string f (Q.Const.string_of_expr c)

  method path f (el, knd) =
    pp f "%s%a" (Q.Db.path_kind_to_string knd) (pp_list "" self#path_elt) el

  method path_elts f el =
    pp f "%a" (pp_list "" self#path_elt) el

  (*---------------------*)
  (*---- expr printer ---*)
  (*---------------------*)
  method expr0 f = function
    | (Q.Lambda _ | Q.Coerce _) as e when coerce -> pp f "(%a)" self#reset#expr0 e
    | (Q.Lambda _) as e when comma -> pp f "(%a)" self#reset#expr0 e
    | Q.LetIn _ | Q.LetRecIn _  as e when record -> pp f "(%a)" self#reset#expr0 e
    | Q.Match _ | Q.Lambda _ | Q.LetIn _ | Q.LetRecIn _  as e when op -> pp f "(%a)" self#reset#expr0 e
    | Q.Const (_, c) -> self#const f c
    | Q.Ident (_, i) -> self#ident f i
    | Q.LetIn (_, b, e) ->
        pp f "@[<v>%a@ %a@]" (pp_list "@ " self#binding) b self#expr e
    | Q.LetRecIn (_, iel, e) -> pp f "@[<v>rec %a@ %a@]" (pp_list "@ and " self#binding) iel self#expr e
    | Q.Lambda (_, il, e) ->
        pp f "@[<2>@[<h>%a@] ->@ %a@]" (pp_list ",@ " self#ident) il self#expr e
    | Q.Apply (_, Q.Ident (_, s), [e1; e2]) as e when is_infix s ->
        if op then pp f "(%a)" self#reset#expr0 e else
          let name = Ident.original_name s in
          pp f "%a %s %a" self#under_op#expr e1 name self#under_op#expr e2
    | Q.Apply (_, e, el) ->
        pp f "@[<2>%a(@,%a)@]" self#apply_expr e (pp_list ",@ " self#reset#under_comma#expr) el
    | Q.Match (_, e, pel) ->
        pp f "@[<v>@[<2>match@ %a@ with@]@ | %a@ end@]" self#expr e (pp_list "@ | " self#rule_) pel
    | Q.Record (_, [ s, Q.Coerce (_, Q.Record (_, []), Q.TypeRecord (Q.TyRow ([], None))) ] ) -> pp f "{%s}" s
    | Q.Record (_, sel) -> self#reset#under_record#record f sel
    | Q.Dot (_, e, s) -> pp f "%a.%s" self#apply_expr e s
    | Q.ExtendRecord (_, s, e1, e2) ->
        pp f "@[<2>{%s = %a} ::@ %a@]" s self#expr e1 self#expr e2
    | Q.Bypass (_, s) -> Format.pp_print_string f ("%%" ^ (BslKey.to_string s) ^ "%%")
    | Q.Coerce (_, e,ty) -> pp f "%a : %a" self#under_coerce#expr e self#ty ty
    | Q.Path (_, el, knd) -> self#path f (el, knd)
    | Q.Directive (_, `module_, [e], _) -> pp f "{%a}" self#reset#expr e
    | Q.Directive (_, dir, exprs, tys) -> self#directive f dir exprs tys
  method bind_field fmt (f, d) = pp fmt "%s = %a" f self#under_record#expr d
  method binding f (i, e) =
    pp f "@[<hv2>%a =@ %a@]" self#ident i self#expr e
  method expr f e =
    self#expr_node f e
  method apply_expr f = function
    | Q.Bypass _
    | Q.Directive _
    | Q.Ident _
    | Q.Apply _
    | Q.Dot _ as e -> self#expr f e
    | e -> pp f "(%a)" self#reset#expr e
  method directive f variant exprs tys =
    let variant_aux f var =
      match var with
      | `abstract_ty_arg (tyvars,rowvars,colvars) ->
        pp f "@[<2>@@abstract_ty_arg(%a|%a|%a)@]"
          (pp_list ",@ " self#under_arrow#typevar) tyvars
          (pp_list ",@ " self#under_arrow#rowvar) rowvars
          (pp_list ",@ " self#under_arrow#colvar) colvars
      | `apply_ty_arg (tys,tyrows,tycols) ->
        pp f "@[<2>@@apply_ty_arg(%a|%a|%a)@]"
          (pp_list ",@ " self#under_arrow#ty) tys
          (pp_list ",@ " self#under_arrow#tyrow) tyrows
          (pp_list ",@ " self#under_arrow#tysum) tycols
      | _ -> pp f"@[<2>%s@]" (directive var)
    in
    match exprs, tys with
    | [], [] -> pp f "@[<2>%a@]" variant_aux variant
    | _, [] ->
        pp f "@[<2>%a(@,%a)@]" variant_aux variant (pp_list ",@ " self#reset#under_comma#expr) exprs
    | _ ->
        pp f "@[<2>%a(@,%a ;@ %a)@]" variant_aux variant
          (pp_list ",@ " self#reset#under_comma#expr) exprs
          (pp_list ",@ " self#reset#under_comma#ty) tys
  method record f l =
    match l with
      | [] -> pp f "{}"
      | _ ->
          let is_tilde_field field =
            let getvar = function
              | Q.Ident (_, i) -> Some i
              | _ -> None
            in
            self#is_tilde_field getvar field
          in
          if List.for_all is_tilde_field l
          then
            let pp_field f (field, _) = Format.pp_print_string f field in
            pp f "@[<hv>~{ %a }@]" (pp_list "@, " pp_field) l
          else
            pp f "@[<hv>{ %a }@]" (pp_list " ;@ " self#record_binding) l

  method record_binding f ((s, e) as expr) =
    match e with
    | Q.Record (_, [])
    | Q.Coerce (_, Q.Record (_, []), _)
    | Q.Directive (_, `coerce, [ Q.Record (_, []) ], _) ->
        Format.pp_print_string f s
    | _ ->
        let getvar = function
          | Q.Ident (_, i) -> Some i
          | _ -> None
        in
        if self#is_tilde_field getvar expr
        then
          pp f "~%s" s
        else
          pp f "@[<2>%s =@ %a@]" s self#expr e

  method rule_ f (p,e) =
    pp f "@[<2>%a ->@ %a@]" self#pat p self#expr e
  method path_elt f =
    function
    | Q.FldKey (s) -> pp f "/%s" s
    | Q.ExprKey e -> pp f "[@[<hv>%a@]]" self#reset#expr e
    | Q.NewKey -> pp f "[?]"

  (*---------------------*)
  (*---- code printer ---*)
  (*---------------------*)
  method code_elt f elt =
    let newval rec_ iel =
      pp f "@[<v>%t%s%a%t@]"
        (fun f -> match iel with [_] -> () | _ -> pp f "/* group start */@ ")
        (if rec_ then "rec " else "")
        (if rec_
         then (pp_list "@ and " self#binding)
         else (pp_list "@ " self#binding)
        ) iel
        (fun f -> match iel with [_] -> () | _ -> pp f "@ /* group end */")
    in
    match elt with
    | Q.Database (_, ident, _p, opts) -> pp f "@[<h>database /* %a */@ %s@]" self#ident ident (Q.Db.options_to_string opts)
    | Q.NewDbValue (_, def) -> pp f "@[<hv2>%a@]" (Q.Db.print_def self#expr self#ty) def
    | Q.NewType (_, l) -> pp f "@[<v>%a@]" (pp_list "@ " self#typedef) l
    | Q.NewVal (_, iel) -> newval false iel
    | Q.NewValRec (_, iel) -> newval true iel

  method code f l =
    pp f "@[<v>%a@]" (pp_list "@ @ " self#code_elt) l
end

(** {6 Other mode of printing} *)

class base_printer_with_sugared_types =
object (self)
  inherit base_printer as super

  (* Variables scope for type variables *)
  val typevar_scope = QmlTypeVars.TypeVarPrint.new_scope ()
  val rowvar_scope = QmlTypeVars.RowVarPrint.new_scope ()
  val colvar_scope = QmlTypeVars.ColVarPrint.new_scope ()

  method reset_typevars =
    QmlTypeVars.TypeVarPrint.reset typevar_scope ;
    QmlTypeVars.RowVarPrint.reset rowvar_scope ;
    QmlTypeVars.ColVarPrint.reset colvar_scope ;
    ()

  method! typevar f t = QmlTypeVars.TypeVarPrint.pp typevar_scope f t
  method! quant_rowvar f t = QmlTypeVars.RowVarPrint.pp rowvar_scope f t
  method! quant_colvar f t = QmlTypeVars.ColVarPrint.pp colvar_scope f t
  method! rowvar f _ = Format.pp_print_string f "..."
  method! colvar f _ = Format.pp_print_string f "..."

  method! scheme f vars rvars cvars ty =
    QmlTypeVars.TypeVarPrint.push typevar_scope ;
    QmlTypeVars.RowVarPrint.push rowvar_scope ;
    QmlTypeVars.ColVarPrint.push colvar_scope ;
    super#scheme f vars rvars cvars ty ;
    QmlTypeVars.TypeVarPrint.pop typevar_scope ;
    QmlTypeVars.RowVarPrint.pop rowvar_scope ;
    QmlTypeVars.ColVarPrint.pop colvar_scope ;
    ()

  method ty_new_scope f ty =
    self#reset_typevars;
    self#ty f ty

  method! code_elt f elt =
    self#reset_typevars;
    super#code_elt f elt
end

(**
   The default pretty printer
*)
class opa_printer =
object (self)
  inherit base_printer_with_sugared_types as super

  method expr0 f expr =
    match expr with
    | Q.Match (_, e, pel) -> (
        match QmlAstWatch.uncons_ifthenelse e pel with
        | Some (if_, then_, else_) ->
            pp f "@[<v>@[<2>if@ (%a)@]@ then %a@ else %a@]" self#reset#expr if_ self#expr then_ self#expr else_
        | None ->
            super#expr0 f expr
      )
    | Q.ExtendRecord (_, s, e1, e2) ->
        let e2, fields = regroup_extend_record e2 in
        let fields = (s, e1)::fields in
        pp f "@[<4>{%a with@ %a}@]" self#under_record#expr e2 (pp_list ";@ " self#reset#bind_field) fields
    | _ -> super#expr0 f expr

  method binding f (i,e) =
    pp f "@[<hv2>%a%a" self#ident i self#binding_expr e

  method binding_expr f e =
    match e with
    | Q.Lambda (_, il, e) ->
        pp f "(%a)%a" (pp_list ", " self#ident) il self#binding_expr e
    | Q.Coerce (_, e, ty) ->
        pp f " : %a =@ %a@]" self#ty ty self#expr e
    | _ ->
        pp f " = @ %a@]" self#expr e
end

(**
   A printer for printing only the toplevel declarations.
*)
class declaration_printer =
object(self)
  inherit opa_printer as super (* yeah, opa_printer is really super *)
  method binding f (i, _) = self#ident f i
end

(**
   Same than the standard printer, but with light identifiers.
*)
class light_ident_printer =
object
  inherit opa_printer
  method ident_to_string i = Ident.light_ident i
end

class very_light_ident_printer =
object
  inherit opa_printer
  method ident_to_string i = Ident.original_name i
end

let annotation_node_factory annot pp fmt ast =
      Format.fprintf fmt "(%a : § %d)" pp ast (Annot.to_int (annot ast))

class annotation_printer =
object(self)
  inherit base_printer_with_sugared_types
  method expr_node fmt expr =
    annotation_node_factory QmlAst.QAnnot.expr self#expr0 fmt expr
  method pat_node fmt expr =
    annotation_node_factory QmlAst.QAnnot.pat self#pat0 fmt expr
end



(* ************************************************************************** *)
(** {b Descr}: Prints an AST element and its source code location. Used by the
    position printer below which is made available in opatrack via file
    qmlTracker.ml.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let position_node_factory pos pp fmt ast =
      Format.fprintf fmt "(%a : § %a)" pp ast FilePos.pp (pos ast)



(* ************************************************************************** *)
(** {b Descr}: Printer decorating source code with positions of its elements.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
class position_printer =
object(self)
  inherit base_printer_with_sugared_types
  method expr_node fmt expr =
    position_node_factory QmlAst.Pos.expr self#expr0 fmt expr
  method pat_node fmt expr =
    position_node_factory QmlAst.Pos.pat self#pat0 fmt expr
end



exception Bad_printer

(* you cannot create instances of these two printers
 * because you need an annotmap to do so *)
class printer_with_type annotmap =
object (self)
  inherit base_printer
  method expr_node fmt expr =
    match QmlAnnotMap.find_ty_opt (QmlAst.QAnnot.expr expr) annotmap with
    | None -> raise Bad_printer
    | Some ty -> Format.fprintf fmt "(%a : %a)" self#expr0 expr self#ty ty

  method pat_node fmt pat =
    match QmlAnnotMap.find_ty_opt (QmlAst.QAnnot.pat pat) annotmap with
    | None -> raise Bad_printer
    | Some ty -> Format.fprintf fmt "(%a : %a)" self#pat0 pat self#ty ty

  method code f l =
    try
      pp f "@[<v>%a@]" (pp_list "@ @ " self#code_elt) l
    with Bad_printer ->  pp f "Stupid! printer_with_type does not work on this pass"

end

class printer_for_ei annotmap =
object (self)
  inherit base_printer as super
  method expr_node f expr =
    let annot = QmlAst.QAnnot.expr expr in
    match QmlAnnotMap.find_tsc_opt annot annotmap with
    | None -> (
        match QmlAnnotMap.find_tsc_inst_opt annot annotmap with
        | None -> super#expr_node f expr
        | Some tsc -> pp f "(%a :- %a)" self#expr0 expr self#tsc tsc
      )
    | Some tsc ->
        match QmlAnnotMap.find_tsc_inst_opt annot annotmap with
        | None -> pp f "(%a :+ %a)" self#expr0 expr self#tsc tsc
        | Some tsc_inst ->
            pp f "(%a :- %a :+ %a)" self#expr0 expr self#tsc tsc_inst self#tsc tsc

  method pat_node f pat =
    let annot = QmlAst.QAnnot.pat pat in
    match QmlAnnotMap.find_tsc_opt annot annotmap with
    | None -> (
        match QmlAnnotMap.find_tsc_inst_opt annot annotmap with
        | None -> super#pat_node f pat
        | Some tsc -> pp f "(%a :- %a)" self#pat0 pat self#tsc tsc
      )
    | Some tsc ->
        match QmlAnnotMap.find_tsc_inst_opt annot annotmap with
        | None -> pp f "(%a :+ %a)" self#pat0 pat self#tsc tsc
        | Some tsc_inst ->
            pp f "(%a :- %a :+ %a)" self#pat0 pat self#tsc tsc_inst self#tsc tsc

end

class pp_value_restriction =
object
  inherit opa_printer as super
  val bound_tyvs = QmlTypeVars.TypeVarSet.empty
  val bound_cols = QmlTypeVars.ColVarSet.empty
  val bound_rows = QmlTypeVars.RowVarSet.empty
  method typevar f v =
    if QmlTypeVars.TypeVarSet.mem v bound_tyvs then super#typevar f v
    else pp f "@{<bright>%a@}" super#typevar v
  method colvar f v =
    if QmlTypeVars.ColVarSet.mem v bound_cols then super#colvar f v
    else pp f "@{<bright>%a@}" super#colvar v
  method rowvar f v =
    if QmlTypeVars.RowVarSet.mem v bound_rows then super#rowvar f v
    else pp f "@{<bright>%a@}" super#rowvar v
  method ty f = function
  | Q.TypeForall (tyvs, rows, cols, t) ->
      let self =
        {< bound_tyvs = List.fold_left (fun acc v -> QmlTypeVars.TypeVarSet.add v acc) bound_tyvs tyvs;
           bound_rows = List.fold_left (fun acc v -> QmlTypeVars.RowVarSet.add v acc) bound_rows rows;
           bound_cols = List.fold_left (fun acc v -> QmlTypeVars.ColVarSet.add v acc) bound_cols cols;
        >} in
      self#scheme f tyvs rows cols t
  | ty -> super#ty f ty
end

(** {6 Exporting an instance of each printer} *)

let pp_base = new base_printer
let pp_base_with_sugared_types = new base_printer_with_sugared_types
let pp = new opa_printer
let pp_light_ident = new light_ident_printer
let pp_very_light_ident = new very_light_ident_printer
let pp_declaration = new declaration_printer
let pp_annotation = new annotation_printer
let pp_position = new position_printer
let pp_value_restriction = new pp_value_restriction

(**
   {6 Not pretty printers}
*)

(**
   Sexp printer
*)
let sexp_tyv f t = Format.pp_print_string f (QmlTypeVars.TypeVar.to_string t)
let sexp_rowv f t = Format.pp_print_string f (QmlTypeVars.RowVar.to_string t)
let sexp_colv f t = Format.pp_print_string f (QmlTypeVars.ColVar.to_string t)
let rec sexp_ty f = function
  | Q.TypeConst Q.TyFloat -> Format.fprintf f "F"
  | Q.TypeConst Q.TyInt -> Format.fprintf f "I"
  | Q.TypeConst Q.TyNull -> Format.fprintf f "Null"
  | Q.TypeConst Q.TyString -> Format.fprintf f "S"
  | Q.TypeVar t -> Format.fprintf f "(V %a)" sexp_tyv t
  | Q.TypeArrow (tyl,ty) ->
      Format.fprintf f "(A ";
      List.iter (fun ty -> sexp_ty f ty; Format.fprintf f " ") tyl;
      sexp_ty f ty;
      Format.fprintf f ")"
  | Q.TypeRecord (Q.TyRow (fields,None)) ->
      Format.fprintf f "(R1 ";
      List.iter (fun (s,ty) -> Format.fprintf f "(%s " s; sexp_ty f ty; Format.fprintf f ")") fields;
      Format.fprintf f ")"
  | Q.TypeRecord (Q.TyRow (fields,Some v)) ->
      Format.fprintf f "(R2 ";
      List.iter (fun (s,ty) -> Format.fprintf f "(%s " s; sexp_ty f ty; Format.fprintf f ")") fields;
      Format.fprintf f " %s)" (QmlTypeVars.RowVar.to_string v)
  | Q.TypeSum (Q.TyCol (fieldss,None)) ->
      Format.fprintf f "(S1";
      List.iter
        (fun fields ->
           Format.fprintf f "(";
           List.iter (fun (s,ty) -> Format.fprintf f "(%s " s; sexp_ty f ty; Format.fprintf f ")") fields;
           Format.fprintf f ")"
        ) fieldss;
      Format.fprintf f ")"
  | Q.TypeSum (Q.TyCol (fieldss,Some v)) ->
      Format.fprintf f "(S2";
      List.iter
        (fun fields ->
           Format.fprintf f "(";
           List.iter (fun (s,ty) -> Format.fprintf f "(%s " s; sexp_ty f ty; Format.fprintf f ")") fields;
           Format.fprintf f ")"
        ) fieldss;
      Format.fprintf f " %s)" (QmlTypeVars.ColVar.to_string v)
  | Q.TypeSumSugar _ ->
      assert false
  | Q.TypeName (tyl,ident) ->
      Format.fprintf f "(N %s " (Q.TypeIdent.to_string ident);
      List.iter (sexp_ty f) tyl;
      Format.fprintf f ")"
  | Q.TypeAbstract ->
      Format.fprintf f "Abs"
  | Q.TypeForall (tyvl,rowl,coll,ty) ->
      sexp_scheme f tyvl rowl coll ty

and sexp_scheme ?(tag="Forall") f tyvl rowl coll ty =
  Format.fprintf f "(%s (" tag;
  List.iter (fun tyv -> Format.fprintf f "%s" (QmlTypeVars.TypeVar.to_string tyv)) tyvl;
  Format.fprintf f ") (";
  List.iter (fun tyv -> Format.fprintf f "%s" (QmlTypeVars.RowVar.to_string tyv)) rowl;
  Format.fprintf f ") (";
  List.iter (fun tyv -> Format.fprintf f "%s" (QmlTypeVars.ColVar.to_string tyv)) coll;
  Format.fprintf f ") ";
  sexp_ty f ty;
  Format.fprintf f ")"

let sexp_tsc f tsc =
  let (quant,ty,()) = QmlGenericScheme.export_unsafe tsc in
  let (vars, rvars, cvars) = QmlTypeVars.FreeVars.export_as_lists quant in
  sexp_scheme ~tag:"Tsc" f vars rvars cvars ty

(** {6 Backward Compatibility} *)
(**
   Until we clean this up
*)

let bw_ty = Format.sprintf "%a" pp#ty
let bw_expr = Format.sprintf "%a" pp#expr



(* ************************************************************************** *)
(** {b Descr}: Function to dump the content of a [QmlAst.annotmap]. This is
    mostly for debug purpose and is really very verbose.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let debug_QmlAst_annotmap annotmap =
  QmlAnnotMap.iteri
    ~f_for_key:
      (fun key -> Format.printf "Key: %s@." (Annot.to_string key))
    ~f_for_ty:
       (function
        | None -> Format.printf "  Type: -@."
        | Some t -> Format.printf "@[  Type: %a@]@." pp#ty t)
    ~f_for_tsc:
       (function
        | None -> Format.printf "  Sch gen: -@."
        | Some sch -> Format.printf "@[  Sch gen: %a@]@." pp#tsc sch)
    ~f_for_tsc_inst:
       (function
        | None -> Format.printf "  Sch inst: -@."
        | Some sch -> Format.printf "@[  Sch inst: %a@]@." pp#tsc sch)
    annotmap
