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
(* CF mli *)

(* refactoring *)
module TypeIdent = QmlAst.TypeIdent

(* alias *)
module Q = QmlAst
module List = Base.List

type directive = QmlAst.qml_directive

(*
  Some common types
*)
module Ty =
struct
  let next () = Q.TypeVar (QmlAst.TypeVar.next ())

  let any () =
    let alpha = next () in
    let beta  = next () in
    Q.TypeArrow ([alpha], beta)

  let id () =
    let alpha = next () in
    Q.TypeArrow ([alpha], alpha)

  let id_apply ?(left=Base.identity) ?(right=Base.identity) () =
    let alpha = next () in
    Q.TypeArrow ([left alpha], right alpha)

  let void = Q.TypeRecord (Q.TyRow ([], None))
  let string = Q.TypeConst Q.TyString

  (* Named types *)

  (*
    FIXME: use opacapi for types definition
  *)

  let named_type name args =
    Q.TypeName (args, TypeIdent.of_string name)

  let option t = named_type Opacapi.Types.option [t]
  let continuation t = named_type Opacapi.Types.continuation [t]
  let bool = named_type Opacapi.Types.bool []
  let opaty = named_type Opacapi.Types.OpaType.ty []
  let oparow = named_type Opacapi.Types.OpaType.row []
  let opacol = named_type Opacapi.Types.OpaType.col []
  let future t = named_type Opacapi.Types.Cps.future [t]
  let embedded_obj =
    let name = Opacapi.Types.path_embedded_obj in
    named_type name []

  let thread_context_t = named_type Opacapi.Types.ThreadContext.t []

  let llarray t = named_type Opacapi.Types.llarray [t]



  (* Specific directives *)

  let assertion () =
    Q.TypeArrow ([bool], void)

  let fail ~with_message =
    let args = if with_message then [string] else [] in
    let alpha = next () in
    Q.TypeArrow (args, alpha)

  let typeof () =
    let alpha = next () in
    Q.TypeArrow ([alpha], opaty)

  let callcc () =
    let alpha = next () in
    let f_cont =
      Q.TypeArrow ([continuation alpha], void)
    in
    Q.TypeArrow ([f_cont], alpha)

  let deprecated () =
    let deprecated = named_type Opacapi.Types.Deprecated.argument [] in
    let alpha = next () in
    Q.TypeArrow ([ deprecated ; alpha ], alpha)

  let todo () =
    let alpha = next () in
    Q.TypeArrow ([], alpha)

  (* -> option(ThreadContext.t) *)
  let thread_context () =
    Q.TypeArrow ([], option thread_context_t)

  (* ThreadContext.t, 'a -> 'a *)
  let with_thread_context () =
    let alpha = next () in
    Q.TypeArrow ([thread_context_t ; alpha], alpha)

  let exc =
    let exc_colvar = QmlAst.ColVar.next () in (* Fixed once and for all *)
    Q.TypeSum (Q.TyCol ([], Some exc_colvar))

  (* exc -> 'a *)
  let throw () =
    let alpha = next () in
    Q.TypeArrow ([exc], alpha)

  (* (exc -> 'a), 'a -> 'a *)
  let catch () =
    let alpha = next () in
    let handler = Q.TypeArrow([exc], alpha) in
    Q.TypeArrow ([handler ; alpha], alpha)

  let opensums () =
    let alpha = next () in
    let beta = next () in
    Q.TypeArrow ([alpha], beta)

  let openrecord () =
    let alpha = next () in
    let beta = next () in
    Q.TypeArrow ([alpha], beta)

  let opavalue_make_performer tys build_add build =
    let ty, add_args =
      match tys with
      | [Q.TypeName (args, _) as ty] ->
          let lf = List.map
            (fun param ->
               match param with
               | Q.TypeVar _ -> build_add param
               | _ -> OManager.error "OpaValue directive : parameters of named type can be only a type variable")
            args in
          ty, lf
      | _ -> OManager.error "OpaValue directive should take exclusively named type"
    in
    build add_args ty

end

(*
  Trying to keep an organisation in that file
  we regroup directives by their groups

  We return a arrow corresponding to the type of the directive.
  The typer will check the arity of the exprs list wrt the returned type.
  A directive with no argument should be typed as a apply with no args.
*)
(*
let type_directive directive _exprs tys =
  match directive with

let simple_slicer_directive directive _exprs _tys =
  match directive with

let slicer_directive directive _exprs _tys =
  match directive with

(* TODO: continue to regroup directives semantically *)
let other_directive directive _exprs _tys =
  match directive with
*)
(*
  TODO: continue to split directives with groups of topic,
  and split this function.
*)
let ty directive exprs tys =
  match (directive:directive) with

  (* === *)
  (* Type *)
  | `coerce -> (
      match tys with
      | [ty] -> Q.TypeArrow ([ty], ty)
      | _ -> assert false
    )
  | `module_ -> Ty.id ()
  | `module_field_lifting -> Ty.id ()
  | `opensums -> Ty.opensums ()
  | `openrecord -> Ty.openrecord ()
  | `unsafe_cast -> Q.TypeArrow ([Ty.next()], Ty.next())
  | `nonexpansive -> Ty.id ()
  | `warncoerce -> Ty.id ()

  (* === *)
  (* Simple slicer *)
  | `side_annotation _
  | `visibility_annotation _
  | `ajax_call _
  | `ajax_publish _
  | `comet_call
  | `comet_publish -> Ty.id ()
  | `insert_server_value _ ->
      (match tys with
       | [ty] -> Q.TypeArrow ([], ty)
       | _ -> assert false)
  | `sliced_expr ->
      let ty = Ty.next () in
      Q.TypeArrow ([ty;ty], ty)

  (* === *)
  (* Errors *)
  | `assert_ -> Ty.assertion ()
  | `fail ->
      let with_message = not (List.is_empty exprs) in
      Ty.fail ~with_message

  (* === *)
  (* Coding *)
  | `deprecated -> Ty.deprecated ()
  | `todo -> Ty.todo ()

  (* === *)
  (* Magic *)
  | `typeof -> Ty.typeof ()
  | `specialize _ ->
      let n = List.length exprs in
      assert (n >= 1);
      let ty = Ty.next () in
      Q.TypeArrow (ty :: List.init (n-1) (fun _ -> Ty.next ()), ty)

  (* === *)
  (* Thread context *)
  | `thread_context -> Ty.thread_context ()
  | `with_thread_context -> Ty.with_thread_context ()

  (* === *)
  (* Exceptions *)
  | `throw -> Ty.throw ()
  | `catch -> Ty.catch ()

  (* === *)
  (* CPS and concurrency *)
  | `atomic
  | `immovable -> Ty.id ()
  | `spawn  ->
      let t = Ty.next () in
      Q.TypeArrow ([t], Ty.future t)
  | `wait ->
      let t = Ty.next () in
      Q.TypeArrow ([Ty.future t], t)
  | `callcc -> Ty.callcc ()
  | `cps_stack_lambda _
  | `cps_stack_apply _
  | `async
  | `may_cps
  | `apply_cont
    -> Ty.id ()

  (* === *)
  (* Expansion *)
  | `expand _ -> Ty.id ()

  (* === *)
  (* Closures *)
  | `closure_create _
  | `closure_apply
  | `closure_create_no_function _
  | `closure_define_function _
    -> assert false (* don't care, those directives are used internally by
                     * the closure pass *)
  | `lifted_lambda _
  | `full_apply _
  | `partial_apply _ ->
      Ty.id ()

  (* === *)
  (* FunActions *)
  | `fun_action kind -> (
      match kind with
      | Some Q.Deserialize ->
          let tv = Ty.next () in
          let arg = Q.TypeRecord (Q.TyRow (["arg", tv; "serialized_arg", Ty.string], None)) in
          Q.TypeArrow ([arg], tv)
      | None
      | Some Q.Client_id -> Ty.id ()
    )

  (* === *)
  (* opadoc *)
  | `doctype _ -> Ty.id ()

  (* === *)
  (* Back-end *)
  | `backend_ident _ -> (
      match tys with
      | [ty] -> Q.TypeArrow ([],ty)
      | _ -> assert false
    )

  | `hybrid_value ->
      let res = Ty.next () in
      let t_client = Q.TypeArrow ([Ty.string], res) in
      let t_server = Ty.string in
      let args =
        match exprs with
        | [_client; _server] -> [t_client ; t_server]
        | [_server] -> [t_server]
        | _ -> assert false in
      Q.TypeArrow (args, res)

  | `js_ident -> Q.TypeArrow ([Ty.string], Ty.string)

  | `restricted_bypass _ -> Ty.id ()

  | `llarray ->
      let ty_arg = Ty.next () in
      let args = List.rev_map (fun _ -> ty_arg) exprs in
      Q.TypeArrow (args, Ty.llarray ty_arg)

  (* === *)
  (* Lazyness *)
  | `create_lazy_record -> (
      let t = Ty.next () in
      match exprs with
      | [_] -> Q.TypeArrow ([t],t)
      | [_;_] -> Q.TypeArrow ([t;Ty.embedded_obj],t)
      | _ -> assert false
    )

  (* === *)
  (* Explicit Instantiation *)
  | `apply_ty_arg (lt,lrow,lcol) ->
      let t = Ty.next () in
      let opatys = List.map (fun _ -> Ty.opaty) lt in
      let oparows = List.map (fun _ -> Ty.oparow) lrow in
      let opacols = List.map (fun _ -> Ty.opacol) lcol in
      Q.TypeArrow ([Q.TypeArrow (opatys @ oparows @ opacols, t)], t)
  | `abstract_ty_arg (lt,lrow,lcol) ->
      let t = Ty.next () in
      let opatys = List.map (fun _ -> Ty.opaty) lt in
      let oparows = List.map (fun _ -> Ty.oparow) lrow in
      let opacols = List.map (fun _ -> Ty.opacol) lcol in
      Q.TypeArrow ([t], Q.TypeArrow (opatys @ oparows @ opacols, t))

  (* === *)
  (* Debug *)
  | `tracker _ -> Ty.id ()

  (* === *)
  | `at_init ->
      let alpha = Ty.next() in
      Q.TypeArrow ([], Q.TypeArrow ([Ty.named_type Opacapi.Types.OPA.Init.value [alpha]], alpha))

  | `tagged_string _ ->
      Q.TypeArrow ([], Q.TypeConst Q.TyString)

  (* === *)
  (* Enrich magic *)
  | `stringifier ->
      let stringifier =
        Ty.opavalue_make_performer tys
          (fun param -> Q.TypeArrow ([param], Ty.string))
          (fun add ty -> Q.TypeArrow (add@[ty], Ty.string))
      in Q.TypeArrow ([stringifier], stringifier)

  | `comparator ->
      let comparison = Ty.named_type Opacapi.Types.Order.comparison [] in
      let comparator = Ty.opavalue_make_performer tys
        (fun param -> Q.TypeArrow ([param; param], comparison))
        (fun add ty -> Q.TypeArrow (add@[ty; ty], comparison)) in
      Q.TypeArrow ([comparator], comparator)

  | `serializer ->
      let options = Ty.named_type Opacapi.Types.OpaSerialize.options [] in
      let json = Ty.named_type Opacapi.Types.RPC.Json.json [] in
      let serializer = Ty.opavalue_make_performer tys
        (fun param -> Q.TypeArrow ([param; options], json))
        (fun add ty -> Q.TypeArrow (add@[ty;options], json)) in
      let unserializer = Ty.opavalue_make_performer tys
        (fun param -> Q.TypeArrow ([json], (Ty.named_type Opacapi.Types.option [param])))
        (fun add ty -> Q.TypeArrow (add@[json], (Ty.named_type Opacapi.Types.option [ty]))) in
      let cpl =
        Q.TypeRecord
          (Q.TyRow ([("f1", serializer);
                     ("f2", unserializer)], None)) in
      Q.TypeArrow ([cpl], cpl)

  | `xmlizer ->
      let xml = Ty.named_type Opacapi.Types.xml [] in
      let xmlizer = Ty.opavalue_make_performer tys
        (fun param -> Q.TypeArrow ([param], xml))
        (fun add ty -> Q.TypeArrow (add@[ty], xml)) in
      Q.TypeArrow ([xmlizer], xmlizer)

  | `recval ->
      Ty.id ()

  (* === *)
  (* closure_instrumentation *)
  | `public_env -> Ty.id ()

(* utils *)

let create_lazy_record_arguments = function
  | [ expr ] -> expr, None
  | [ expr ; info ] -> expr, Some info
  | _ -> assert false

let create_lazy_record_exprs record info =
  match info with
  | Some info -> [ record ; info ]
  | None -> [ record ]

module Format = Base.Format

let to_string d =
  match d with
  | `deprecated -> "deprecated"
  | `todo -> "todo"
  | `at_init -> "at_init"
  | `module_ -> "module"
  | `module_field_lifting -> "module_field_lifting"
  | `coerce -> "coerce"
  | `nonexpansive -> "nonexpansive"
  | `unsafe_cast -> "unsafe_cast"
  | `opensums -> "opensums"
  | `openrecord -> "openrecord"
  | `assert_ -> "assert"
  | `typeof -> "typeof"
  | `atomic -> "atomic"
  | `immovable -> "immovable"
  | `thread_context -> "thread_context"
  | `with_thread_context -> "with_thread_context"
  | `js_ident -> "js_ident"
  | `throw -> "throw"
  | `catch -> "catch"
  | `spawn -> "spawn"
  | `wait -> "wait"
  | `callcc -> "callcc"
  | `restricted_bypass pass -> "restricted_bypass["^ pass ^ "]"
  | `fail -> "fail"
  | `create_lazy_record -> "create_lazy_record"
  | `warncoerce -> "warncoerce"
  | `apply_ty_arg _ -> "apply_ty_arg _"
  | `abstract_ty_arg _ -> "abstract_ty_arg _"
  | `closure_create _ -> "closure_create"
  | `closure_apply -> "closure_apply"
  | `closure_create_no_function _ -> "closure_create_no_function"
  | `closure_define_function _ -> "closure_define_function"
  | `ajax_publish b -> Printf.sprintf "ajax_publish(%s)" (match b with `sync -> "`sync" | `async -> "`async")
  | `ajax_call b -> Printf.sprintf "ajax_call(%s)" (match b with `sync -> "`sync" | `async -> "`async")
  | `comet_publish -> "comet_publish"
  | `comet_call -> "comet_call"
  | `insert_server_value i -> Printf.sprintf "insert_server_value(%s)" (Ident.to_string i)
  | `doctype _ -> "doctype"
  | `hybrid_value -> "hybrid_value"
  | `backend_ident s -> Printf.sprintf "backend_ident[%s]" s
  | `tracker _ -> "track"
  | `expand _ -> "expand"
  | `fun_action None -> "fun_action"
  | `fun_action (Some Q.Client_id) -> "fun_action[Client_id]"
  | `fun_action (Some Q.Deserialize) -> "fun_action[Deserialize]"
  | `cps_stack_lambda _ -> "cps_stack_lambda"
  | `cps_stack_apply _ -> "cps_stack_apply"
  | `async -> "async"
  | `sliced_expr -> "sliced_expr"
  | `may_cps -> "may_cps"
  | `stringifier -> "stringifier"
  | `comparator -> "comparator"
  | `serializer -> "serializer"
  | `xmlizer -> "xmlizer"
  | `llarray -> "llarray"
  | `specialize variant -> Printf.sprintf "specialize%s" (match variant with `strict -> "_strict" | `polymorphic -> "")
  | `partial_apply (None, ser) -> Printf.sprintf "partial_apply[ser:%B]" ser
  | `partial_apply (Some i, ser) -> Printf.sprintf "partial_apply[missing:%d,ser:%B]" i ser
  | `full_apply n -> Printf.sprintf "full_apply[env %d]" n
  | `lifted_lambda (n,l) ->
      Format.sprintf "lifted_lambda[env %d,[%a]]"
        n
        (Format.pp_list "@ " (fun f i -> Format.pp_print_string f (Ident.to_string i))) l
  | `tagged_string (s, kind) ->
      Printf.sprintf "tagged_string[%S, %s]" s
        (match kind with
         | Q.Rpc_use -> "rpc_use"
         | Q.Rpc_def -> "rpc_def"
         | Q.Type_def -> "type_def"
         | Q.Type_use -> "type_use"
         | Q.Client_closure_use -> "client_closure_use")
  | `apply_cont -> "apply_cont"
  | `recval -> "recval"
  | `side_annotation a -> (
      match a with
      | `server -> "server"
      | `client -> "client"
      | `both -> "both"
      | `prefer_server -> "prefer_server"
      | `prefer_client -> "prefer_client"
      | `prefer_both -> "prefer_both"
      | `both_implem -> "both_implem"
    )
  | `visibility_annotation `private_ -> "server_private"
  | `visibility_annotation (`public `sync) -> "publish"
  | `visibility_annotation (`public `async) -> "publish_async"
  | `visibility_annotation (`public `funaction) -> "publish_funaction"

  | `public_env -> "public_env"

