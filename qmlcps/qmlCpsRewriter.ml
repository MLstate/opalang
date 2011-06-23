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
(* THIS FILE HAS A DOCUMENTED MLI *)

(*

  TODO:
    -exploit the LetIn rewriting for implementing all multi-expression skipping (like apply case)
    -remove annots were useless fields and add them where usefull match, dot, record construction
*)

(* depends in Base *)
module List = Base.List

(* refactoring in progress *)

(* depends, alias *)
module Factorize = QmlCpsIL.Factorize
module IL = QmlCpsIL.IL
module Q = QmlAst

(* deprecated error managment *)
type error = string
exception Exception of error
external error_message : error -> string = "%identity"

(*
  debug levels : keep it synchro with DebugVariables.mli
*)

module DebugLevel =
struct
  let make_barrier = 2
  let release_barrier = make_barrier

  let cont_tracer = 10

  let il_opt_timer = 1

  let full_backtrace = 100
end

let debug fmt =
  OManager.printf ("@[<2>@{<cyan>[Cps]@}@ "^^fmt^^"@]@.")

(* facilities to generate qmlAst *)
(* TODO: use the statefull constructor, keep annotation, position and types *)
module QC = QmlAstCons.UntypedExpr

(* Bypass helpers *)
let cps_id = "cps"

let il_bycps_call call = IL.Bypass (call, Some cps_id)
let il_other_call call = IL.Bypass (call, None)
let qml_bycps_call call = QC.restricted_bypass ~pass:cps_id call
let qml_other_call call = QC.bypass call
let qml_byobj_magic () = QC.bypass Opacapi.Opabsl.BslPervasives.Magic.id

let il_bypass key = IL.Bypass (key, None)

let qml_group_app expr =
  let fresh = Ident.next "_" in
  QC.letin [fresh, expr] (QC.ident fresh)

(* TODO: replace errors by call to 1) internal or 2) public errors modules *)
let myself = "QmlCpsRewriter"
let error fmt =
  let k s = raise (Exception s) in
  Printf.ksprintf k fmt

type options =
    {
      no_assert : bool ;
      no_server : bool;
      qml_closure : bool ;
      toplevel_concurrency : bool ;
      warn_x_field : unit ;
      server_side : bool ;
    }

(* please, keep default values synchro with the documentation *)
let default_options =
  {
    no_assert = false ;
    no_server = true;
    qml_closure = false ;
    toplevel_concurrency = false ;
    warn_x_field = () ;
    server_side = true ;
  }

type env =
    {
      options : options ;
      bsl_bypass_tags : BslKey.t -> BslTags.t ;
      bsl_bypass_cps : BslKey.t -> BslKey.t option ;
      bsl_bypass_typer : BslKey.t -> BslTypes.t ;
      typing : QmlTyper.env ;
    }

let env_initial ~options ~bsl_bypass_typer ~bsl_bypass_tags ~bsl_bypass_cps ~typing () =
  {
    options = options ;
    bsl_bypass_typer = bsl_bypass_typer ;
    bsl_bypass_tags = bsl_bypass_tags ;
    bsl_bypass_cps = bsl_bypass_cps ;
    typing = typing ;
  }

(* In order to do the transformation, there is a need for type t used to collect
    toplevel barrier introduction.  *)
type private_env =
    {
      (* skipped functions : (arity * fskip_id * fcps_id) Map *)
      skipped_functions : (int * Ident.t * Ident.t) IdentMap.t;
      toplevel_barrier : Ident.t IdentMap.t ;
      warn_x_field : unit ;
      bindings : (Ident.t * QmlAst.expr) list
    }

let private_env_initial : unit -> private_env = fun () ->
  let bindings = [] in
  {
    skipped_functions = IdentMap.empty;
    toplevel_barrier = IdentMap.empty ;
    warn_x_field = () ;
    bindings = bindings
  }

let print_private_env private_env =
  Printf.printf "SKIPPED\n%!";
  IdentMap.iter( fun k (i,i1,i2) ->
    let n i = Ident.stident i in
    Printf.printf "%s %d => %s , %s\n" (n k) i (n i1) (n i2);
  ) private_env.skipped_functions

let private_binding (private_env:private_env) =
  private_env.bindings

let private_env_get_skipped_fun id private_env =
  IdentMap.find_opt id private_env.skipped_functions


module S =
struct
  type t = private_env
  let pass = "qmlCpsRewriter"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module Package = struct
  include ObjectFiles.MakeClientServer(S)

  let debug = false

  let load_dependencies ~side =
    let merge _pack a b =
      if debug then Printf.printf "LOADING %s\n%!\n" (fst _pack);
      { skipped_functions = IdentMap.safe_merge a.skipped_functions b.skipped_functions
      ; toplevel_barrier  = IdentMap.safe_merge a.toplevel_barrier  b.toplevel_barrier
      ; warn_x_field = ()
      ; bindings = [] }
    in
    fold_with_name ~side merge (private_env_initial ())

  let save_current ~side ~private_env_initial ~private_env =
    let (-) ab a =
       { skipped_functions = IdentMap.diff ab.skipped_functions a.skipped_functions
       ; toplevel_barrier  = IdentMap.diff ab.toplevel_barrier  a.toplevel_barrier
       ; warn_x_field = ()
       ; bindings = [] }
    in
    let private_env_increment = private_env - private_env_initial in
    if debug then (
      Printf.printf "SAVING\n%!\n";
      print_private_env private_env_initial;
      print_private_env private_env;
      print_private_env private_env_increment;
    );
    save ~side private_env_increment
end



(* production of embeded location error messages in the server *)
let string_of_pos = FilePos.to_string

(*
   Fetch the definition and the type of a bypass, for use in CPS.

   For the moment, we have no CPS-specific BSL language. Therefore,
   we take the OCaml implementation of the bypass and we transform
   it into a CPS function.

   Note that this form of embedding would fail if a function appears in
   negative position of the type of your bypass (i.e. as argument to a
   function).

   That's why the CTrans has provided a reverse transformation mechanism,
   a way of transforming an OCaml ['a -> 'b continuation -> unit] function
   into a OCaml ['a -> 'b] function using the [uncps-n] primitives.

   In the future, we could also think about having special cps
   types in the bsl, in order to see if we need to do the conversion
   or not.
*)

(* The cps transform makes no assumption about the bypass it receives
 * It will eta expand if needed, but only when necessary (when bypasses
 * are not applied) *)
let expand_bypass (env:env) (expr:QmlAst.expr) =
  let key =
    match expr with
    | Q.Directive (_, `restricted_bypass _, [Q.Bypass (_, key)], _)
    | Q.Bypass (_, key) -> key
    | _ -> assert false in
  let typ = env.bsl_bypass_typer key in
  (* forming the type list corresponding to this type *)
  let inputs, _output = match typ with
    | BslTypes.Fun (_, inputs, output) -> Some inputs, output
    | _ -> None, typ
  in
  match inputs with
  | None ->
      (* it is not a function, do nothing *)
      None
  | Some l ->
      let n = List.length l in
      let args = List.init n (fun i -> Ident.nextf "bypass_arg_%d" i) in
      let apply = QC.apply expr (List.map QC.ident args) in
      Some (QC.lambda args apply)


(* private context to be sure to control what goes out *)
module Context :
sig
  type t
  val make :
    transaction:(IL.vident -> IL.term) option ->
    kappa:(IL.vident -> IL.term) ->
    cont:(IL.cident) option ->
    parent:(IL.cident) option ->
    t
  val start : parent:IL.cident option -> t
  val apply : t -> IL.vident -> IL.term
  val cont : t -> IL.cident -> t
  val kappa : t -> (IL.vident -> IL.term) -> t
  val insertLetCont : t -> (IL.cident -> IL.term) -> IL.term
  val current_cont : t -> IL.cident option
end =
struct
  type t =
      {
        (*The continuation to use in case of transaction failure*)
        transaction: (IL.vident -> IL.term) option;
        kappa: IL.vident -> IL.term ;
        cont : IL.cident option;
        parent : IL.cident option;
      }

  let make ~transaction ~kappa ~cont ~parent =
    {
      transaction = transaction ;
      kappa = kappa ;
      cont = cont;
      parent = parent;

    }

  let start ~parent =
    make
      ~transaction:None
      ~kappa:(fun x -> IL.Done (x, "Top-level expression terminated"))
      ~cont:None
      ~parent

  (* until we manage to remove totally kappa (context.kappa)
      we switch to an hybrid approach with an option of cident *)
  let apply context =
    match context.cont with
    | Some k -> (fun z -> IL.ApplyCont(k, z))
    | None -> context.kappa

  let cont context cont = { context with cont = Some cont }
  let kappa context kappa = { context with cont = None ; kappa = kappa }

  (* dont produce re-binding continuation *)
  (* insertLetCont is never called with a of_cont introducing a ApplyCont,
     so it does not introduced direct applied cont *)
  let insertLetCont context of_cont =
    match context.cont with
    | Some k -> of_cont k
    | None ->
        begin
          let v = IL.fresh_v () in
          let returned = context.kappa v in
          match v, returned with
          | IL.Value v, IL.ApplyCont (cont, IL.Value v')
              when Ident.equal v v' -> of_cont cont
          | _ ->
              let c = IL.fresh_c () in
              IL.LetCont ((c, v, returned, of_cont c), context.parent)
        end

  let current_cont context =
    match context.cont with
    | None -> context.parent
    | c -> c

end

(** skiping utils module *)
module Skip = struct

  (* skipping can be desactivated here *)
  let can = #<If:CPS_NOSKIP> false #<Else> true #<End>

  (* insert a skipped term properly when a context should be used *)
  let remove ilexpr (context:Context.t) =
    match ilexpr with
    | IL.Skip e ->
        begin match e with
        | Q.Ident (_, i) -> Context.apply context (IL.Value i)
        | Q.Const (_, c) -> let v = IL.fresh_v () in IL.LetVal (v, IL.Constant c, Context.apply context v)
        | _ -> let v = IL.fresh_v () in IL.LetVal (v, IL.ValueSkip e, Context.apply context v)
        end
    | _  -> ilexpr

  let remove2 c (a,e) = a, remove e c

  let is1 = function IL.Skip _ -> true | _ -> false
  let is2 = function _, IL.Skip _ -> true | _ -> false
  let is4 = function _, _, _, IL.Skip _ -> true | _ -> false

  let get = function IL.Skip qml -> qml | _ -> assert false

end

(** utily module, essentially to simplify the apply case *)
module U = struct
  (**
     check if an ident need to be changed to something else
     (e.g. wait barrier or another ident)
     note that an ident cannot be a barrier ident and
     at the same time a function with non skipped version
  *)
  let is_stable_ident env = function
    | Q.Ident (_, id)
        when not((IdentMap.mem id env.toplevel_barrier)
        || (IdentMap.mem id env.skipped_functions) )
          -> true
    | _ -> false

  let is_not_barrier_ident_or_internal_bypass penv e = match e with
    | Q.Bypass _ | Q.Directive (_, `restricted_bypass _, _, _) | Q.Directive (_, `may_cps, _, _) -> true
    | Q.Ident (_, id) -> not(IdentMap.mem id penv.toplevel_barrier)
    | _ -> false

  let get_value_il_of_ident = function
      (* cannnot be extended for constant *)
    | Q.Ident (_, id) -> IL.Value id
    | _ -> assert false

  let rec bp_get_key_and_passid bp = match bp with
    | Q.Directive (_, `may_cps, [bypass], _) -> bp_get_key_and_passid bypass
    | Q.Directive (_, `restricted_bypass pass_id, [Q.Bypass (_, key)], _) ->
        key, Some pass_id
    | Q.Bypass (_, key) -> key, None
    | _ ->
        let context = QmlError.Context.expr bp in
        QmlError.i_error None context
          "Unexpected form of bypass"

  let bp_get_key bp = fst (bp_get_key_and_passid bp)

  let bp_cps bp_cps bp =
    let rec aux bp =
      match bp with
      | Q.Directive (label, `restricted_bypass pass_id, [bp] , x) ->
          Q.Directive (label, `restricted_bypass pass_id, [aux bp] , x)
      | Q.Directive (_, `may_cps, [Q.Bypass (label, key) as bp] , _) ->
          (match bp_cps key with
           | Some key -> Q.Bypass (label, key)
           | None -> bp)
      | Q.Bypass _ -> bp
      | _ ->
          Format.printf "@[<2>expr:@ %a@]@." QmlPrint.pp#expr bp;
          assert false
    in aux bp

  let is_second_order_bypass bsltags =
    bsltags.BslTags.second_order && BslTags.do_projection bsltags "cps"

  (** check if a bypass implementation (projection included) is waiting a continuation as last argument *)
  let is_cps_bp bp_tags bp =
    let bsltags = bp_tags (bp_get_key bp) in
    (is_second_order_bypass bsltags)
    || (bsltags.BslTags.cps_bypass)
    || (bsltags.BslTags.raise_)



  (** cps_apply fcps_id f_args context :
      create the IL application of the CPS function fskip_id with IL TERM arguments under the given context *)
  let cps_apply ?stack_info fcps_id f_args context =
    let args = List.map get_value_il_of_ident f_args in
    Context.insertLetCont context (fun k -> IL.ApplyNary (IL.CpsVident (IL.Value fcps_id), args, k, stack_info))

  (** bp_apply ~cont_as_arg bypass (f_args:QmlAst.expr list)
      create the IL application of the bypass with QML arguments,
      WARNING : cps=true  =>
                  bypass take a continuation as last argument
                cps=false =>
                  bypass is applied to standard arguments,
                  then the contination is used on the result of application *)
  let bp_apply ~cps bypass bp_args context =
    let key, pass_id = bp_get_key_and_passid bypass in
    let args = List.map get_value_il_of_ident bp_args in
    Context.insertLetCont context (fun k ->
        if cps
        then IL.ApplyNary(IL.CpsBypass (IL.Bypass (key, pass_id)), args, k, None)
        else IL.ApplyBypass(            IL.Bypass (key, pass_id) , args, k)
    )

  let label () = Annot.nolabel "Cps.nolabel"

  (** skipped_apply fskip_id f_args :
      create the IL application of the SKIPPED function fskip_id with QML IDENT arguments *)
  let skipped_apply ?partial fskip_id f_args =
    let e = QmlAstUtils.App.from_list (Q.Ident (label (), fskip_id) ::  f_args ) in
    let e =
      match partial with
      | None -> e
      | Some (more_args, ser) -> Q.Directive (label (), `partial_apply (None,ser), e :: more_args, []) in
    IL.Skip e

  (** same for bypass *)
  let skipped_bp_apply bypass bp_args = IL.Skip (QmlAstCons.UntypedExpr.apply bypass bp_args)

  (** bad_apply_property  f f_args : check that all args are idents that don't need rewriting
                                     and that f is either an non barrier ident or a bypass *)
  let good_apply_property ?(more_args=[]) penv f f_args =
     List.for_all (is_stable_ident penv) f_args
    && List.for_all (is_stable_ident penv) more_args
    && is_not_barrier_ident_or_internal_bypass penv f

  (** transform the expression so that the apply has the good property
      gives name to all element in need for cps rewriting *)
  let normalize_apply_property ?stack_info ?partial penv f f_args =
    let name_arg (bindings,exprs) e =
      match e with
      | Q.Ident _ when is_stable_ident penv e -> (bindings, e :: exprs)
      | _ ->
          let id = Ident.next "arg" in
          ((id,e) :: bindings, Q.Ident (label (), id) :: exprs) in
    let acc =
      match f with
      | Q.Bypass _ | Q.Directive (_, `restricted_bypass _, _, _) ->
          ([],[f])
      | Q.Ident _ when is_stable_ident penv f || is_not_barrier_ident_or_internal_bypass penv f ->
          ([],[f])
      | _ ->
          name_arg ([],[]) f in
    let bindings, rev_args = List.fold_left name_arg acc f_args in
    let bindings, rev_more_args_opt =
      match partial with
      | None -> bindings, None
      | Some (more_args, ser) ->
          let bindings, rev_more_args = List.fold_left name_arg (bindings,[]) more_args in
          bindings, Some (rev_more_args, ser) in
    let app = QmlAstUtils.App.from_list (List.rev rev_args) in
    let app =
      match stack_info with
      | None -> app
      | Some info -> Q.Directive (label (), `cps_stack_apply info, [app], []) in
    let app =
      match rev_more_args_opt with
      | None -> app
      | Some (rev_more_args, ser) ->
          Q.Directive (label (), `partial_apply (None,ser), (app :: List.rev rev_more_args), []) in
    Q.LetIn (label (), bindings, app)

  let rewrite_apply_partial context f_id f_args =
    let e = IL.Skip (QC.apply (QC.ident f_id) f_args) in
    if Skip.can then e
    else Skip.remove e context

  let rewrite_apply ?stack_info ?partial ~private_env ~expr ~context f_id f_args =
    match private_env_get_skipped_fun f_id private_env with
    | Some(real_arity, fskip_id, fcps_id) -> (
        match partial with
        | Some _ ->
            (* skipped version exists but incomplete call *)
            skipped_apply ?partial fcps_id f_args
        | None ->
            (* skipped version exists, complete call *)
            if List.length f_args <> real_arity then (
              Format.printf "Partial apply (expected %d args, get %d) in CpsRewriter :@\n%a@."
                real_arity (List.length f_args) QmlPrint.pp#expr expr;
              assert false
            );
            skipped_apply ?partial fskip_id f_args
        )
    | None ->
        (* skipped version don t exist *)
        match partial with
        | Some _ -> skipped_apply ?partial f_id f_args
        | None -> cps_apply ?stack_info f_id f_args context

  let is_const e =
    match e with
    | Q.Const _ -> true
    | _ -> false

  let good_llarray_property_elt penv arg =
    is_stable_ident penv arg
    || is_const arg

  let good_llarray_property penv args =
    List.for_all (good_llarray_property_elt penv) args

  let normalize_llarray_property penv args =
    let foldmap bindings expr =
      if good_llarray_property_elt penv expr
      then
        bindings, expr
      else
        (* name all others *)
        let id = Ident.next "arg" in
        let binding = id, expr in
        let bindings = binding :: bindings in
        let expr = Q.Ident (label (), id) in
        bindings, expr
    in
    let bindings, args = List.fold_left_map foldmap [] args in
    let llarray = Q.Directive (label (), `llarray, args, []) in
    assert (bindings <> []) ;
    Q.LetIn (label (), bindings, llarray)
end


(** The code_elt is there only for Error context *)
(* Convert a QML expression to a CPS term.*)
let il_of_qml ?(can_skip_toplvl=false) (env:env) (private_env:private_env) (expr:QmlAst.expr)  =
  (* Records
     <!> beware, this function is partial, it is defined only on complex records, and lazy records.
     the skip option specify if we accept to use and propagate skip nodes and provide the
     constructor for its content *)
  let rec aux_record ?skip fields (context:Context.t) create_record =
    let il_fields, build_fields =
      let fold (il_fields, build) (f, expr) =
        let c = IL.fresh_c () in
        let v = IL.fresh_v () in
        ((IL.Field f, v, (Q.QAnnot.expr expr))::il_fields),
        ((f, c, v, (aux_can_skip expr (Context.cont context c)))::build) in
      List.fold_left fold ([], []) fields in

    (* do you accept Skip to return node ? *)
    let can_skip = Skip.can && skip <> None in

    (* do we accept to propagate skip nodes from fields ? *)
    let all_skip =
      can_skip
      && List.for_all Skip.is4 build_fields
    in

    if all_skip then (* Skipping *) (Option.get skip) fields build_fields
    else
      let il_record =
        let v = IL.fresh_v () in
        IL.LetVal (v, create_record (List.rev il_fields),
                   Context.apply context v)
      in
      let parent = Context.current_cont context in
      let fold il_term (_, c, v, field_term) =
        let field_term = Skip.remove field_term (Context.cont context c) in
        IL.LetCont ((c, v, il_term, field_term), parent)
          in
      List.fold_left fold il_record build_fields

  and aux_lambda ?(can_skip_lambda=false) context label args e =
    let k = IL.fresh_c () in
    let args = List.map (fun id -> IL.Value id) args in
    let cont_e =  (Context.cont context k) in
    let e = aux_can_skip e cont_e in
    (match e with
     | IL.Skip e when can_skip_lambda ->
         (* skip transformations must not be lost -> do not return IL.Skip expr *)
         `skip (QmlAstCons.UntypedExprWithLabel.lambda ~label (List.map (function IL.Value x -> x) args) e)
     | (* include Skipped and non Skipped *)_ ->
         begin
           `letfun (args, k, Skip.remove e cont_e)
         end)


  (* this version of 'aux' is allowed to return 'Skip' node,
     you should only call it if you know how to handle them properly.
     Currently, only const, ident and record case can give 'Skip' node
     Only record case is recursively calling 'aux_can_skip'.
     All other calls are on the standard 'aux' (see after)
  *)
  and aux_can_skip ?(can_skip_lambda=false) expr (context:Context.t) =
    match expr with

    | Q.Const _ when Skip.can -> IL.Skip expr

    | Q.Const (_, c) ->
        let x = IL.fresh_v () in
        IL.LetVal (x, IL.Constant c, Context.apply context x)

    | Q.Ident (label, x) ->
        begin
          match IdentMap.find_opt x private_env.toplevel_barrier with
          | Some barrier ->
            (* if option --cps-toplevel-concurrency is set,
               this ident may be unbound, and we have just a barrier instead *)
              let func k = IL.ApplyNary (IL.CpsBypass (il_bycps_call Opacapi.Opabsl.BslCps.wait), [IL.Value barrier], k, None) in
              Context.insertLetCont context func
          | None when not(Skip.can) ->  Context.apply context (IL.Value x)
          | None ->
              match IdentMap.find_opt x private_env.skipped_functions with
              | Some (_, _, fcps_id) -> IL.Skip (Q.Ident (label, fcps_id))
              | None                 -> IL.Skip expr
        end

    | Q.LetIn (label, l, e) ->
        begin
          let item name value sub_expr =
            let value_il =
              match value with
              | Q.Lambda (label,params,body) -> (
                  match aux_lambda context label params body with
                  | `skip _ -> assert false
                  | `letfun _ as e -> e
                )
              | _ ->
                  let c = IL.fresh_c () in
                  let context_for_value = Context.cont context c in
                  let value_il = aux_can_skip value context_for_value in
                  `letcont (c,context_for_value,value_il) in
            let sub_expr_il  = aux_can_skip sub_expr context  in
            match value_il with
            | `letcont (_, _, value_il) when Skip.is1 sub_expr_il && Skip.is1 value_il ->
              begin
                (* old skip changes (fun_skipped ...) must not be removed *)
                let value = Skip.get value_il in
                let sub_expr =  Skip.get sub_expr_il in
                IL.Skip (QmlAstCons.UntypedExprWithLabel.letin ~label [(name, value)] sub_expr)
              end
            | `letcont (c, context_for_value, value_il) ->
                let parent = Context.current_cont context in
                IL.LetCont ((c, (IL.Value name), Skip.remove sub_expr_il context,
                             Skip.remove value_il context_for_value), parent)
            | `letfun (args,k,body) ->
                IL.LetFun ([IL.Value name,args,k,body], Skip.remove sub_expr_il context)
          in
          match l with
          | [] -> aux_can_skip e context
          | [(name, value)] -> item name value e
          | (name, value)::t -> item name value (Q.LetIn (label, t, e))
        end

    | Q.LetRecIn (_, defs, e) ->
        let items = List.map
          (fun (name, def) ->
             match def with
             | Q.Lambda (_, params, body) ->
                 let params = List.map (fun id -> IL.Value id) params in
                 let k = IL.fresh_c () in
                 (IL.Value name, params, k, aux body (Context.cont context k))
             | _ -> error "Recursive definition of a non-function"
          ) defs
        in
        IL.LetRecFun (items, aux e context)

    | Q.Lambda (label, args, e) -> (
        match aux_lambda ~can_skip_lambda context label args e with
        | `skip e -> IL.Skip e
        | `letfun (args,k,body) ->
            let anonymous = IL.fresh_fun () in
            IL.LetFun([anonymous, args, k, body], Context.apply context anonymous)
      )

    (* Special case for stack traces *)
    | Q.Directive (_, `cps_stack_lambda cont_opt_ref, e_opt,_) ->
        (* this directive does not modifying the rewriting of the expression in any way
         * it just records the current continuation in the reference *)
        (cont_opt_ref : Obj.t option ref) := Obj.magic (Context.current_cont context : IL.cident option);
        let e = List.get_only_element e_opt in
        aux_can_skip e context

    (* Special case of Apply node for stack traces *)
    | Q.Directive (_, `cps_stack_apply ((cont_opt_ref_opt,name,position) as stack_info), [Q.Apply (_, f, f_args)], _) -> (
        if not (U.good_apply_property private_env f f_args) then
          aux_can_skip (U.normalize_apply_property ~stack_info private_env f f_args) context
        else
          (* the only difference with the usual apply case is that the case
           * bypass never occurs here (because `cps_stack_apply is never put
           * on a bypass) *)
          let cont : IL.cident option =
            match cont_opt_ref_opt with
            | None -> None
            | Some x -> Some (Obj.obj (Option.get !x)) in
          match f with
          | Q.Ident (_, f_id) ->
              let stack_info =
                {IL.caller_cont = cont;
                 IL.callee_name = name;
                 IL.position = position} in
              U.rewrite_apply ~stack_info ~private_env ~expr ~context f_id f_args
          | _ ->
              OManager.printf "unexpected expr for cps trace@\n" ;
              OManager.printf "expr: %a@." QmlPrint.pp#expr expr ;
              assert false
      )

    (* BEGIN OF APPLY NODE *)
    (* normalisation of apply node
       to guaranty property : f is a non barrier ident or a bypass, f_args are stable identifiers *)
    | Q.Apply (_, f, f_args) when not(U.good_apply_property private_env f f_args) ->
        aux_can_skip (U.normalize_apply_property private_env f f_args) context
    | Q.Directive (_, `partial_apply (_,ser), Q.Apply (_, f, f_args) :: more_args, _)
        when not (U.good_apply_property ~more_args private_env f f_args) ->
        aux_can_skip (U.normalize_apply_property private_env ~partial:(more_args,ser) f f_args) context

    (* guaranteed property : f is a non barrier ident, f_args are stable identifiers *)
    | Q.Apply (_, Q.Ident (_, f_id), f_args) ->
        U.rewrite_apply ~private_env ~expr ~context f_id f_args
    | Q.Directive (_, `partial_apply (_, ser), Q.Apply (_, Q.Ident (_, f_id), f_args) :: more_args, _) ->
        U.rewrite_apply ~partial:(more_args,ser) ~private_env ~expr ~context f_id f_args

    (* guaranteed property : f is a bypass, f_args are stable identifiers *)
    | Q.Apply (_, bypass, bp_args) ->
        let bypass = U.bp_cps env.bsl_bypass_cps bypass in
        let cps = U.is_cps_bp env.bsl_bypass_tags bypass in
        if cps || not(Skip.can)
        then U.bp_apply ~cps bypass bp_args context
        else U.skipped_bp_apply bypass bp_args

    (* END OF APPLY NODE *)

    | Q.Match (label, e, cases) ->
        let cases =
          let map (pat, epat) = (pat, aux_can_skip epat context) in
          List.map map cases in
        let all_cases_are_skipped = List.for_all Skip.is2 cases in
        let c = IL.fresh_c () in
        let context_for_e = Context.cont context c in
        begin match aux_can_skip e context_for_e with
        | IL.Skip e when all_cases_are_skipped && Skip.can ->
            let map (pat, epat) = (pat, Skip.get epat) in
            IL.Skip (QmlAstCons.UntypedExprWithLabel.match_ ~label e (List.map map cases))
        | e ->
            let cases = List.map (Skip.remove2 context) cases in
            let e = Skip.remove e context_for_e in
            let v = IL.fresh_v () in
            let parent = Context.current_cont context in
            IL.LetCont((c,v,
                        IL.Match (v, cases),
                        e ), parent)
        end

    | Q.Record (label, fields) ->
        let skip _ builded =
          let map (f, _, _, term) = (f, Skip.get term) in
          IL.Skip (QmlAstCons.UntypedExprWithLabel.record ~label (List.map map builded))
        in
        aux_record ~skip:skip fields context (fun fields -> IL.Record fields)

    | Q.Directive (_, `create_lazy_record, exprs, _) -> (
        let expr, info = QmlDirectives.create_lazy_record_arguments exprs in
        (* <!> beware there : the info has type 'a in QmlFlatServerLib *)
        match expr with
        | Q.Record (_, fields) -> (
            match info with
            | Some info ->
                let v = IL.fresh_v () and c = IL.fresh_c () in
                let parent = Context.current_cont context in
                IL.LetCont((c, v,
                            (let create_record fields = IL.LazyRecord (fields, Some v) in
                             aux_record fields context create_record),
                            aux info (Context.cont context c)
                           ), parent)
            | None ->
                let create_record fields = IL.LazyRecord (fields, None) in
                aux_record fields context create_record
          )
        | _ -> assert false
      )

    | Q.Dot (label, e, s) ->
        let c = IL.fresh_c () in
        let context_for_e = Context.cont context c in
        begin match aux_can_skip e context_for_e with
          | IL.Skip e when Skip.can ->
              IL.Skip (QmlAstCons.UntypedExprWithLabel.dot ~label e s)
          | e ->
              let e = Skip.remove e context_for_e in
              let v = IL.fresh_v () in
              let p = IL.fresh_v () in
              let parent = Context.current_cont context in
              IL.LetCont((c, v,
                          IL.LetProj (p, (v, IL.Field s), Context.apply context p),
                          e), parent)
        end

    | Q.ExtendRecord _ ->

        let il_fields, build_fields, rest =
          let rec fold ((il_fields, build, seen) as acc) expr =
            match expr with
            | Q.Coerce (_, e, _) -> fold acc e
            | Q.ExtendRecord (_, f, expr, e) ->
                let acc =
                  if StringSet.mem f seen
                  then
                    (* This field has been added once already.
                       Drop the second extension or it would break [extend_with_array] *)
                    acc
                  else
                    let c = IL.fresh_c () in
                    let v = IL.fresh_v () in
                    (
                      ((IL.Field f, v)::il_fields),
                      ((c, v, (aux expr (Context.cont context c)))::build),
                      (StringSet.add f seen)
                    )
                in
                fold acc e
            | _ -> il_fields, build, expr
          in fold ([], [], StringSet.empty) expr
        in

        let record = IL.fresh_v () in
        let il_extend_record =
          let v = IL.fresh_v () in
          IL.LetVal (v, IL.ExtendRecord ((List.rev il_fields), record), Context.apply context v) in

        let parent = Context.current_cont context in

        let il_term =
          let c = IL.fresh_c () in
          IL.LetCont((c, record, il_extend_record, aux rest (Context.cont context c)), parent) in

        let fold il_term (c, v, term) = IL.LetCont((c, v, il_term, term), parent) in
        List.fold_left fold il_term build_fields

    | Q.Bypass _
    | Q.Directive (_, `restricted_bypass _, [Q.Bypass _], _) -> (
        (* if we end up here, it means QmlBypassHoisting wasn't called, or that someone
         * introduced other bypasses in the meantime
         * In any case, we eta expand them ourselves
         *)
        match expand_bypass env expr with
        | None -> (
            match expr with
            | Q.Directive (_, `restricted_bypass pass, [Q.Bypass (_, key)], _) ->
                (* value bypass *)
                let v = IL.fresh_v () in
                IL.LetVal (v, IL.BasicBypass (IL.Bypass(key, Some pass)), Context.apply context v)
            | Q.Bypass (_, key) ->
                (* value bypass *)
                let v = IL.fresh_v () in
                IL.LetVal (v, IL.BasicBypass (IL.Bypass(key, None)), Context.apply context v)
            | _ -> assert false (* not matched by the outer pattern *)
          )
        | Some e ->
            aux_can_skip ~can_skip_lambda e context
      )

    | Q.Coerce (_, e, _) -> aux_can_skip ~can_skip_lambda e context

    | Q.Path (_, _, _) ->
        failwith "Internal error: At this stage, all first-class paths should have been compiled."

    (* Concurrency-specific directive, and cps specific *)

    | Q.Directive (_, `spawn, expr, _) ->
        begin
          match expr with
          | [expr] ->
              (* TODO there : restriction for Record only *)
              let name = IL.fresh_v () in
              let v = IL.fresh_v () in (* v is of type unit, ignored by the function *)
              let k = IL.fresh_c () in
              let expr = aux expr (Context.cont context k) in
              let defs = [(name, [v], k, expr)] in
              let func c =
                let term = IL.ApplyBypass (il_bycps_call Opacapi.Opabsl.BslCps.spawn, [name], c) in
                IL.LetFun (defs, term)
              in
              Context.insertLetCont context func
          | _ -> assert false (* cannot be parsed *)
        end

    (* TODO: when the basic version passes the [fact_spawn*.qml] tests,
       remove [@wait] and instead process concurrency as we do
       with lazy records: perform @wait implicitely at dot access. *)
    | Q.Directive (_, `wait, expr, _) ->
        begin
          match expr with
          | [expr] ->
              (*We use the result of [future] and [context]
                to build the task which will be set up to*)
              let c1 = IL.fresh_c () in
              let future = IL.fresh_v () in
              let parent = Context.current_cont context in
              IL.LetCont((c1 , future,
                          (let func k = IL.ApplyNary (IL.CpsBypass (il_bycps_call Opacapi.Opabsl.BslCps.wait), [future], k, None) in
                           Context.insertLetCont context func),
                          aux expr (Context.cont context c1)), parent)

          | _ -> assert false (* cannot be parsed *)
        end

    | Q.Directive (_, `atomic, exprs, tys) ->
        let expr = List.get_only_element exprs in
        let expr = aux expr context in
        IL.Directive (`atomic, [expr], tys)

    | Q.Directive (_, `callcc, [expr], _) ->
        let c = IL.fresh_c () and f_callcc = IL.fresh_v () in
        let parent = Context.current_cont context in
        IL.LetCont ((c, f_callcc,
                     (let func k = IL.ApplyNary (IL.CpsBypass (il_bycps_call Opacapi.Opabsl.BslCps.callcc_directive), [f_callcc], k, None) in
                      Context.insertLetCont context func),
                     aux expr (Context.cont context c)
                    ), parent)

    | Q.Directive (_, `immovable, _, _) ->
        failwith "Internal error: CPS Directive @immovable is not yet implemented"
          (* in particular : see if this directive should be removed by this pass,
             or preserved (or transformed?) for a specific back-end directive *)

    | Q.Directive (_, `assert_, _, _) -> assert false (* every assert
        directive has been rewritten in a fail directive by pass_Assertion. *)

    | Q.Directive (_, `fail, args, _) -> (
        (*
          FIXME: skip in case of no message, or if the message is a static string
        *)
        let fail_cps = Opacapi.Opabsl.BslPervasives.fail_cps in
        let position_value =
          let pos = string_of_pos (Q.Pos.expr expr) in
          IL.Constant (Q.String pos)
        in
        let position = IL.fresh_v () in
        let message = IL.fresh_v () in
        let body =
          let term k =
            IL.LetVal (position, position_value,
                       IL.ApplyNary (IL.CpsBypass (il_bypass fail_cps), [message ; position], k, None))
          in
          Context.insertLetCont context term
        in
        match args with
        | [] ->
            let message_value =
              let mes = "" in
              IL.Constant (Q.String mes)
            in
            IL.LetVal (message, message_value, body)
        | e :: _ -> (
            match e with
            | Q.Const (_, ((Q.String _) as literal)) ->
                IL.LetVal (message, IL.Constant literal, body)
            | _ ->
                let c = IL.fresh_c () in
                let parent = Context.current_cont context in
                IL.LetCont((c, message, body, aux e (Context.cont context c)), parent)
          )
      )

    | Q.Directive (_, `module_, [expr], _) -> aux_can_skip ~can_skip_lambda expr context

    | Q.Directive (_, `thread_context, _, _) ->
        let term ((IL.Continuation id) as c) =
          IL.ApplyBypass (il_bycps_call Opacapi.Opabsl.BslCps.thread_context, [ IL.Value id ], c)
        in
        Context.insertLetCont context term

    | Q.Directive (_, `with_thread_context, arguments, _) ->
        let thread_context, alpha =
          match arguments with
          | [ fst ; snd ] -> fst, snd
          | _ -> failwith "Internal error: @with_thread_context is not used 2 arguments"
        in
        let c1 = IL.fresh_c () and c2 = IL.fresh_c () and thread_context_id = IL.fresh_v () in
        let (IL.Continuation thread_context_continuation) as c3 = IL.fresh_c () in
        let v3 = IL.Value thread_context_continuation in
        let term ((IL.Continuation ctop) as parent) =
          let parent = Some parent in
          IL.LetCont((c1, thread_context_id,
                      IL.LetCont((c2, v3, aux alpha (Context.cont context c3),
                                  IL.ApplyBypass ((il_bycps_call Opacapi.Opabsl.BslCps.with_thread_context), [ thread_context_id ; IL.Value ctop ], c2)
                                 ), parent),
                      (aux thread_context (Context.cont context c1))
                     ), parent)
        in
        Context.insertLetCont context term

    | Q.Directive (_, `throw, [exc], _) ->
        let c = IL.fresh_c () and v_exc = IL.fresh_v () in
        let c2 = IL.fresh_c () and v_handler = IL.fresh_v () in
        let cont_of_val (IL.Value id) = IL.Continuation id in
        Context.insertLetCont context
          (fun ((IL.Continuation ctop) as parent) ->
             IL.LetCont
               ((c, v_exc,
                 IL.LetCont
                   ((c2, v_handler, IL.ApplyCont(cont_of_val v_handler, v_exc),
                     IL.ApplyBypass (il_bycps_call Opacapi.Opabsl.BslCps.handler_cont, [ IL.Value ctop ], c2)),
                    Some parent),
                 aux exc (Context.cont context c)),
               Some parent))

    | Q.Directive (_, `catch, [handler ; expr], _) -> ( (* similar to with_thread_context *)
        let (IL.Continuation c1_name) as c1 = IL.fresh_c () in
        let c2 = IL.fresh_c () in
        let (IL.Continuation handler_continuation) as c3 = IL.fresh_c () in
        let v3 = IL.Value handler_continuation in
        let handler = aux handler (Context.cont context c1) in
        let catch_bypass =
          let catch =
            if env.options.server_side && env.options.qml_closure
            then Opacapi.Opabsl.BslCps.catch else Opacapi.Opabsl.BslCps.catch_native in
          il_bycps_call catch
        in
        let catch ((IL.Continuation ctop) as parent) handler_id =
          IL.LetCont
            ((c2, v3, aux expr (Context.cont context c3),
              IL.ApplyBypass (catch_bypass, [ handler_id ; IL.Value ctop ], c2)
             ), Some parent)
        in
        (* code simplification for common cases, handler beeing a lambda *)
        match handler with
        | IL.LetFun ([(IL.Value handler_name) as handler_id, _, _, _] as list,
                     IL.ApplyCont (IL.Continuation if_c1_name, IL.Value if_handler_name))
            when Ident.equal c1_name if_c1_name && Ident.equal handler_name if_handler_name
              ->
            let term parent =
              IL.LetFun (list, catch parent handler_id)
            in
            Context.insertLetCont context term
        | _ ->
            let handler_id = IL.fresh_v () in
            let term parent =
              IL.LetCont((c1, handler_id,
                          catch parent handler_id,
                          handler
                         ), Some parent)
            in
            Context.insertLetCont context term
      )

    | Q.Directive (_, `llarray, args, _) when not (U.good_llarray_property private_env args) ->
        aux_can_skip (U.normalize_llarray_property private_env args) context

    | Q.Directive (_, `llarray, _, _) -> IL.Skip expr

    | Q.Directive (_, `throw, _, _) ->
        failwith "Internal error: directive @throw should have 1 argument"
    | Q.Directive (_, `catch, _, _) ->
        failwith "Internal error: directive @catch should have 2 arguments"

    | Q.Directive (_, `async, _, _) ->
        failwith "Internal error: presence of @asynchronous directive in an expression"

    | Q.Directive (_, `partial_apply _, _, _) -> assert false

    (* other directive : no specific tratement done in the cps *)
    | Q.Directive (_, directive, exprs, tys) ->
        let terms = List.map (fun expr -> aux expr context) exprs in
        IL.Directive (directive, terms, tys)
  (**
     This version of aux is not allowed to return Skip node,
     it is the standard function you should call unless you 100% certain
     of doing mixed cps/noncps code (e.g. Skip nodes)
  *)
  and aux expr (context:Context.t) = Skip.remove (aux_can_skip expr context) context
  in
  let term =
    if can_skip_toplvl then
      aux_can_skip ~can_skip_lambda:true expr (Context.start ~parent:None)
    else
      aux expr (Context.start ~parent:None) in
  private_env, term

let chrono_factorize_letcont = Chrono.make ()

(* OPTIMISATION IN IL *)
let il_simplification (env:env) (private_env:private_env) (term:IL.term) =
  let _ = env in
  let _ = #<If:CPS_VERBOSE $minlevel DebugLevel.il_opt_timer> Chrono.start chrono_factorize_letcont #<End> in
  let term = #<If:CPS_KEEP_LETCONT> term #<Else> Factorize.letcont term #<End> in
  let _ = #<If:CPS_VERBOSE $minlevel DebugLevel.il_opt_timer> Chrono.stop chrono_factorize_letcont #<End> in
  private_env, term

(* Convert an IL value to a qml expression.*)
let qml_of_il_value = function
  | IL.Constant e -> QC.const e

  | IL.Record fields ->
      let fields = List.map (fun (IL.Field s, IL.Value v, _) -> s, (QC.ident v)) fields in
      QC.record fields

  | IL.ExtendRecord (fields, IL.Value v) ->
      let fold (IL.Field s, IL.Value v) acc = QC.extendrecord s (QC.ident v) acc in
      List.fold_right fold fields (QC.ident v)

  | IL.LazyRecord (fields, info) ->
      let info = Option.map (fun (IL.Value info) -> QC.ident info) info in
      let fields = List.map (fun (IL.Field s, IL.Value v, _) -> s, (QC.ident v)) fields in
      let record = QC.record fields in
      let exprs = QmlDirectives.create_lazy_record_exprs record info in
      QC.directive `create_lazy_record exprs []

  | IL.BasicBypass (IL.Bypass (key, restriction)) -> (
      match restriction with
      | None -> QC.bypass key
      | Some pass -> QC.restricted_bypass ~pass key
    )

  | IL.ValueSkip e -> e

(* Convert an IL term to a qml expression.*)

(* TODO in IL :
   + add there a few directives to distinghish from normal QmlAst constructions
   + static type everything possible, dont loose annots (optimal branching with qmlflat...)
*)

let runtime_bt_collection bt_pos _f_string _larg expr =
  let bt_info =
    #<If:CPS_DEBUG $minlevel DebugLevel.full_backtrace>
      let fun_args2string = qml_other_call Opacapi.Opabsl.BslCps.fun_args2string in
      let _larg = List.map (fun arg ->
        QC.apply (qml_byobj_magic ()) [QC.ident arg]) _larg
      in
      let _larg = List.hd _larg in (* TODO: tmp hack; libbsl does not translate lists *)
      QC.apply fun_args2string [QC.string _f_string; (*QC.list*) _larg]
        #<Else>
        QC.string bt_pos
        #<End>
  in
  let bt_add = qml_other_call Opacapi.Opabsl.BslCps.bt_add in
  QC.letin [Ident.next "_", (QC.apply bt_add [bt_info])]
    expr

let runtime_debug minlevel message expr =
  let pr = qml_other_call Opacapi.Opabsl.BslCps.debug in
  QC.letin [ Ident.next "_", (QC.apply pr [ QC.int minlevel ; QC.string message ]) ]
    expr

let apply_cont_tracer =
  let r = ref 0 in
  (fun k expr ->
    incr r;
    let k_string = Ident.stident k in
    let message = Printf.sprintf "+ ap cont %s : #%d#" k_string (!r) in
    runtime_debug DebugLevel.cont_tracer message expr
  )

let apply_fun_tracer f larg expr =
  let f_string = Ident.stident f in
  let backtrace_pos = f_string(*TODO: replace by position*) in
  runtime_bt_collection backtrace_pos f_string larg expr

let qml_bypass_of_il_bypass = function
  | IL.Bypass (key, None) -> QC.bypass key
  | IL.Bypass (key, Some pass) -> QC.restricted_bypass ~pass key

let qml_function_of_cps_function = function
  | IL.CpsVident (IL.Value a) -> QC.ident a
  | IL.CpsBypass il_bypass -> qml_bypass_of_il_bypass il_bypass

let qml_of_il ~toplevel_cont (env:_) (private_env:private_env) (term:IL.term) =
  let atomic = ref false in
  let toplevel_done = ref false in
  let rec aux = function
    | IL.LetVal (IL.Value x, v, term) ->
        let value = qml_of_il_value v in
        QC.letin [x, value] (aux term)

    | IL.LetProj (IL.Value x, (IL.Value record, IL.Field field), term) ->
       QC.letin [x, QC.dot (QC.ident record) field] (aux term)

    | IL.LetCont ((IL.Continuation k, IL.Value id, t, u), parent) ->
        let body =
          let fun_body = aux t in
          let fun_body =
            #<If:CPS_DEBUG>
              apply_cont_tracer k fun_body
            #<Else>
              fun_body
            #<End>
          in
          let lambda = QC.lambda [id] fun_body in
          match parent with
          | None ->
             let make_continuation =
               qml_bycps_call
                 (if env.options.server_side && env.options.qml_closure
                 then Opacapi.Opabsl.BslCps.cont else Opacapi.Opabsl.BslCps.cont_native) in
              QC.apply make_continuation [lambda]
          | Some (IL.Continuation parent) ->
              let make_continuation =
                qml_bycps_call
                  (if env.options.server_side && env.options.qml_closure
                   then Opacapi.Opabsl.BslCps.ccont else Opacapi.Opabsl.BslCps.ccont_native) in
              QC.apply make_continuation [ QC.ident parent ; lambda ]
        in
        QC.letin [k, body] (aux u)

    | IL.LetRecFun (defs, e) ->
        let defs =
          let map (IL.Value id, args, IL.Continuation k, body) =
            let body = aux body in
            let args = List.rev_map (fun (IL.Value v) -> v) args in
            let args = List.rev (k::args) in
            id, QC.lambda args body in
          List.map map defs in
        QC.letrecin defs (aux e)

    | IL.LetFun (defs, e) ->
        let defs =
          let map (IL.Value id, args, IL.Continuation k, body) =
            let body = aux body in
            let args = List.rev_map (fun (IL.Value v) -> v) args in
            let args = List.rev (k::args) in
            id, QC.lambda args body in
          List.map map defs in
        QC.letin defs (aux e)

    | IL.ApplyCont (IL.Continuation k, IL.Value v) ->
        let cpsreturn = qml_bycps_call Opacapi.Opabsl.BslCps.return in
        QC.apply cpsreturn [ QC.ident k ; QC.ident v ]

    | IL.ApplyExpr (cps_function, IL.Value b, IL.Continuation k) ->
        let cps_function =
          match cps_function with
          | IL.CpsVident (IL.Value a) ->
              let magic_a = QC.ident a in
              let magic_a =
                  if env.options.server_side && (not (env.options.qml_closure))
                  then qml_group_app (QC.apply (qml_bycps_call Opacapi.Opabsl.BslCps.magic_func) [magic_a])
                  else magic_a
              in
              #<If:CPS_DEBUG>
                apply_fun_tracer a [b] magic_a
                #<Else>
                magic_a
                #<End>
          | IL.CpsBypass il_bypass -> qml_bypass_of_il_bypass il_bypass
        in
        (* if !atomic *)
        (* then *)
          QC.apply cps_function [ QC.ident b ; QC.ident k ]
        (* else *)
        (*   let cpsapply = qml_bycps_call Opacapi.Opabsl.BslCps.apply in *)
        (*   QC.apply cpsapply [ cps_function ; QC.ident b ; QC.ident k ] *)

    | IL.ApplyNary (cps_function as id, args, IL.Continuation k, stack_infos_opt) ->
        let cps_function = qml_function_of_cps_function cps_function in
        let cps_function =
          if env.options.server_side && (not env.options.qml_closure) then
            (* FIXME: could call specialized magic_funcN to keep
             * some type errors in the backend *)
            #<If:QMLC_NO_MAGIC>
              cps_function
            #<Else>
              let magic =
            (* let arity = List.length args in
               let bypass =
                 if arity <= 5 then "magic_func" ^ string_of_int arity
                 else "magic_func_more" in
               qml_bycps_call bypass *) qml_byobj_magic () in
              QC.apply magic [cps_function]
            #<End>
          else
            cps_function
        in
        let cps_function =
          match id with
          | IL.CpsVident (IL.Value _a) ->
              #<If:CPS_DEBUG>
                let args = List.map (fun (IL.Value id) -> id) args in
                apply_fun_tracer _a args cps_function
                #<Else>
                cps_function
                #<End>
          | _ -> cps_function
        in
        let rev_args = List.rev_map (fun (IL.Value id) -> QC.ident id) args in
        let k =
          match stack_infos_opt with
          | None -> QC.ident k
          | Some {IL.caller_cont = caller_cont_opt; IL.callee_name = name_opt; IL.position = position_opt} ->
              let caller_cont =
                match caller_cont_opt with
                | None -> QC.record ["none", QC.unit ()]
                | Some (IL.Continuation caller_cont) -> QC.record ["some", QC.ident caller_cont] in
              let name = Option.default "anon fun" name_opt in
              let position = Option.default "no pos" position_opt in
              let args = #<If:CPS_STACK_TRACE$contains "arg"> List.rev rev_args #<Else> [] #<End> in
              QC.apply
                (QC.bypass (BslKey.normalize "bslcps_update_cont"))
                [QC.ident k; caller_cont; QC.string name; QC.string position; QC.opa_tuple args] in
        let apply = QC.apply cps_function (List.rev (k :: rev_args)) in
        (* if !atomic *)
        (* then  *)
          apply
        (* else *)
        (*   let fct = QC.lambda (Ident.next "_") apply in *)
        (*   let cpsapply0 = qml_bycps_call "apply0" in *)
        (*   QC.apply cpsapply [ fct ; QC.ident k ] *)

    | IL.ApplyBypass (il_bypass, args, IL.Continuation k) ->
        let qml_bypass = qml_bypass_of_il_bypass il_bypass in
        let bypass_result = QC.apply qml_bypass (List.map (function (IL.Value x) -> QC.ident x) args) in
        let v = Ident.next "bypass_result" in
        let cpsreturn = qml_bycps_call Opacapi.Opabsl.BslCps.return in
        QC.letin [v, bypass_result]
          (QC.apply cpsreturn [ QC.ident k ; QC.ident v ])

    | IL.Match (IL.Value v, pat_terms) ->
        let pat_terms =
          let map (pat, term) = (pat, aux term) in
          List.map map pat_terms in
        QC.match_ (QC.ident v) pat_terms

    | IL.Done (IL.Value v, _) -> toplevel_done := true ; toplevel_cont v

    | IL.Directive (`spawn, _, _)
    | IL.Directive (`wait , _, _)
      -> assert false (* removed by qml -> IL *)

    | IL.Directive (`atomic, terms, _) ->
        let term = List.get_only_element terms in
        let _ = atomic := true in
        aux term

    | IL.Directive (`immovable, _, _) -> assert false (* cf remark in qml -> IL *)

    | IL.Directive (`create_lazy_record, _, _) -> assert false (* expressed as const after qml -> IL *)

    | IL.Directive (`module_, _, _) -> assert false (* removed by qml -> IL *)

    | IL.Directive (`restricted_bypass _, _, _) -> assert false (* rewrited in a expanded_bypass after qml -> IL or removed by hoisting *)

    | IL.Directive (`async, _, _) ->
        (* at toplevel only, checked by qml -> IL *)
        assert false

    (* other directive : no specific treatment done in the cps *)
    | IL.Directive (directive, terms, tys) ->
        let exprs = List.map aux terms in
        QC.directive directive exprs tys

    | IL.Skip qml -> qml

  in
  let qml = aux term in
  (* if not (!toplevel_done) then assert false else  *)
  private_env, qml



(*
val code_elt : t -> QmlAst.code_elt -> t * QmlAst.code
val code : t -> QmlAst.code -> t * QmlAst.code
*)

(*
   Notes about this top-level transformation

   - db stuff + new types are ignored and not returned by this transformation
   - toplevel values becomes barrier

val t = expr

-->

val t_b = make_barrier ()
val _ =
   << expr >>_cont{ v -> release_barrier t_b v}

the access to t in the rest of the code is transformed by :
{ wait cont t_b }

in case of a re-binding :

val y = t

if t is a top level barrier, we replace y by the call to wait t in the rest of the code.
value y is removed from the code.

to avoid nightmare of type error, segfault, etc.. val t is no more defined at top level.
so, if any probleme appear during the construction, we'll get a : unbound value error.

*)

(** facility to manipulate barriers *)
module Barrier :
sig
  val make : Ident.t -> QmlAst.expr
  val release : barrier_id:Ident.t -> value:Ident.t -> QmlAst.expr

  (**
     A bypass for setting by side-effect to QmlCpsServerLib global properties.
     This should by used before a the execution of the continuation releasing a barrier.
  *)
  val before_wait : unit -> QmlAst.expr

  (**
     Special case for synchronous toplevel.
     Make sence only before the lanch of the server.
     The scheduler [loop_until] the barrier was released.
  *)
  val toplevel_wait : Ident.t -> QmlAst.expr
end =
struct

  let make _ident =
    let cpsbarrier =
      qml_bycps_call (#<If:QMLC_NO_MAGIC> Opacapi.Opabsl.BslCps.make_barrier #<Else> Opacapi.Opabsl.BslCps.black_make_barrier #<End>) in
    let barrier = QC.apply cpsbarrier [ QC.string (Ident.to_string _ident) ] in
    let barrier =
      #<If:CPS_DEBUG>
        runtime_debug DebugLevel.make_barrier
        (Printf.sprintf "make_barrier : %s" (Ident.to_string _ident))
        barrier
      #<Else>
        barrier
      #<End>
    in
    barrier

  let release ~barrier_id ~value =
    let cpsrelease =
      qml_bycps_call (#<If:QMLC_NO_MAGIC> Opacapi.Opabsl.BslCps.release_barrier #<Else> Opacapi.Opabsl.BslCps.black_release_barrier #<End>) in
    let release_barrier = QC.apply cpsrelease [ QC.ident barrier_id ; QC.ident value ] in
    let release_barrier =
      #<If:CPS_DEBUG>
        runtime_debug DebugLevel.release_barrier
        (Printf.sprintf "release_barrier : %s" (Ident.to_string barrier_id))
        release_barrier
      #<Else>
        release_barrier
      #<End>
    in
    release_barrier

  let before_wait () =
    QC.apply (qml_bycps_call Opacapi.Opabsl.BslCps.before_wait) [ QC.unit () ]

  let toplevel_wait barrier_id =
    let cpstoplevelwait =
      qml_bycps_call (#<If:QMLC_NO_MAGIC> Opacapi.Opabsl.BslCps.toplevel_wait #<Else> Opacapi.Opabsl.BslCps.black_toplevel_wait #<End>)
    in
    let toplevel_wait = QC.apply cpstoplevelwait [ QC.ident barrier_id ] in
    toplevel_wait
end

let private_env_add id barrier_id private_env =
  { private_env with
      toplevel_barrier = IdentMap.add id barrier_id private_env.toplevel_barrier
  }

let private_env_add_skipped_fun id arity fskip_id fcps_id private_env =
  { private_env with
      skipped_functions = IdentMap.add id (arity, fskip_id, fcps_id) private_env.skipped_functions
  }

let simpl_let_in = function
  | Q.LetIn (_, [(x, expr)], Q.Ident (_, y)) when Ident.equal x y -> expr
  | expr -> expr

let code_elt (env:env) (private_env:private_env) code_elt =
  match code_elt with
  | Q.NewVal (label, defs) ->

      let rec fold_filter_map private_env (id, expr) =
        let immediate_value_or_barrier ?(can_skip_toplvl=false) () =
          let is_asynchronous, expr =
            match expr with
            | Q.Directive (_, `async, [e], _) -> true, e
            | _ -> false, expr
          in
          let private_env, il_term = il_of_qml ~can_skip_toplvl:can_skip_toplvl env private_env expr in
          let private_env, il_term = il_simplification env private_env il_term in
          match il_term with
            (* a barrier won't be needed when an expression is skipable at toplvl *)
          | IL.Skip expr ->
              begin
                (* let toplevel_cont v = QC.ident v in *)
                (* let private_env, expr = qml_of_il ~toplevel_cont env private_env il_term in *)
                (* simplification : if the code is [val f = let x = foo in x], replace it by [val f = foo] *)
                (* much simplier and efficienter to detect after generation *)
                let expr = simpl_let_in expr in
                private_env, [ (id, expr) ]
              end
                (* the expression has not been skiped at toplvl, *)
          | _ ->
              begin
                let asynchronous = env.options.toplevel_concurrency || is_asynchronous in
                let local_barrier_id, barrier_id, make_barrier =
                  let barrier_id = Ident.refreshf ~map:"%s_barrier" id in
                  let make_barrier = Barrier.make barrier_id in
                  let local_barrier_id = if asynchronous then Ident.refresh barrier_id else barrier_id in
                  local_barrier_id, barrier_id, make_barrier
                in
                let toplevel_cont value = Barrier.release ~barrier_id:local_barrier_id ~value in
                let private_env, expr = qml_of_il ~toplevel_cont env private_env il_term in
                let expr = simpl_let_in expr in
                if asynchronous
                then
                  begin
                    private_env_add id barrier_id private_env,
                    [ (barrier_id,
                       QC.letin [local_barrier_id, make_barrier]
                         (QC.letin [Ident.next "barrier_unit", expr]
                            (QC.ident local_barrier_id)
                         )
                      ) ]
                  end
                else
                  begin
                    private_env,
                    [ (id,
                       QC.letin [Ident.nextf "before", Barrier.before_wait ()]
                         (QC.letin [barrier_id, make_barrier]
                            (QC.letin [Ident.next "barrier_unit", expr]
                               (Barrier.toplevel_wait barrier_id)
                            )
                         )
                      ) ]
                  end
              end
        in
        let immediate_lambda arity =
          let private_env, il_term = il_of_qml ~can_skip_toplvl:true env private_env expr in
          let private_env, il_term = il_simplification env private_env il_term in
          let toplevel_cont v = QC.ident v in
          match il_term with
          | IL.Skip e ->
              (* 2 version of the lambda must be created, one is CPS and the other is a SKIP *)
              (* It is mandatory to have both versions, the fun_SKIP will be used when the fun's call
                 is complete and the fun_CPS will be used in all other cases *)
              let fskip = e in
              let fskip_id =  Ident.refreshf ~map:"%s_skip" id in
              let private_env = private_env_add_skipped_fun id arity fskip_id id private_env in
              let fskip_eta_exp = QmlAstUtils.Lambda.eta_expand_ast arity (QC.ident id) in
              let private_env, fcps_il = il_of_qml env private_env fskip_eta_exp in
              let private_env, fcps = qml_of_il ~toplevel_cont env private_env fcps_il in
              let fcps = simpl_let_in fcps in
              private_env, [ (fskip_id, fskip); (id, fcps) ]
          | _ ->
              (* if the lambda is not skipable, only a CPS version is generated *)
              let private_env, expr = qml_of_il ~toplevel_cont env private_env il_term in
              let expr = simpl_let_in expr in
              private_env, [ (id, expr) ]
        in
        match expr with
        | Q.Const _ -> immediate_value_or_barrier ~can_skip_toplvl:true ()
        | Q.Ident (_, x) ->
            (* alias case *)
          begin
            match
              private_env_get_skipped_fun x private_env,(* skipping information *)
              IdentMap.find_opt x private_env.toplevel_barrier(* barrier information *)
            with
            (* on a skipped function *)
            | Some (arity, x_skip_id, x_cps_id),_ ->
                (* we recreate a two version function skip and cps *)
              let skip_id =  Ident.refreshf ~map:"%s_skip" id in
              let private_env = private_env_add_skipped_fun id arity x_skip_id x_cps_id private_env in
              private_env, [ (skip_id,  QC.ident x_skip_id) ; (id, QC.ident x_cps_id )  ]

            (* on a barrier *)
            | _, Some barrier when env.options.toplevel_concurrency ->
              private_env_add id barrier private_env, [] (* removing this re-binding *)

            (* general case *)
            | _ -> immediate_value_or_barrier ~can_skip_toplvl:true ()
          end

        | Q.LetIn _
        | Q.LetRecIn _ -> immediate_value_or_barrier ()

        | Q.Directive (_, `restricted_bypass _, [Q.Lambda (_, l, _)], _)
        | Q.Lambda (_, l, _) -> immediate_lambda (List.length l)

        | Q.Apply _ -> immediate_value_or_barrier ~can_skip_toplvl:true ()

        | Q.Match _ -> immediate_value_or_barrier ~can_skip_toplvl:true ()

        (* optimization for simple record *)
        (* <!> see the corresponding optimization in qml -> IL *)
        | Q.Record _ -> immediate_value_or_barrier ~can_skip_toplvl:true ()

        | Q.Dot _ -> immediate_value_or_barrier ~can_skip_toplvl:true ()

        | Q.ExtendRecord _ -> immediate_value_or_barrier ()

        | Q.Directive (_, `restricted_bypass _, _, _)
        | Q.Bypass _ -> immediate_value_or_barrier ()

        | Q.Coerce (_, e, _) -> fold_filter_map private_env (id, e)

        | Q.Path _ ->
            failwith "Internal error: At this stage, all first-class paths should have been compiled."

        (* control the presence of cps directive at top-level *)

        | Q.Directive (_, `spawn, [_], _) -> immediate_value_or_barrier ()
        | Q.Directive (_, `wait, [_], _) -> immediate_value_or_barrier ()
        | Q.Directive (_, `atomic, [_], _) -> immediate_value_or_barrier ()

        (* TODO: decide what to do with that, probably remove it *)
        | Q.Directive (_, `immovable, [_], _) -> assert false

        (* Some explication on hybrid value :
           - When we encountered a hybrid_value we
           know that we compile a client code. (Indeed hybrid_value can
           be introduce only on client_code)
           - We know also that compiler is before javascript
           compilation (else we would have no client code)
           - And compiler is before cps rewriting on server (because
           it's make after javascript compilation)

           Then we compile only the client value. Javascript compiler
           will compile hybrid_value like a server value and this
           value will be computed by cps after javascript compiler

           CpsClient  :
             hybrid_value(client, server)
               -> hybrid_value(cpsclient, server)
           JsCompiler :
             hybrid_value(cpsclient, server)
               -> "a js cps string using cpsclient" ^ server ^ "..."
           CpsServer :
             "a js cps string using cpsclient" ^ server ^ "..."
               -> "a js cps string using cpsclient" ^ cpsserver ^ "..."
        *)
        | Q.Directive (_, `hybrid_value, l, tys) ->
            begin
              match l with
              | [_server] ->
                  private_env, [(id, expr)]
              | [client; server] ->
                  ignore server;
                  let private_env, e = fold_filter_map private_env (id, client) in
                  (match e with
                   | [] -> assert false
                   | [ (id, e) ] ->
                       private_env,
                       [ id, QC.directive `hybrid_value [e ; server] tys ]
                   | _ -> assert false)
              | _ -> assert false
            end

        | Q.Directive (_, `async, _, _) -> immediate_value_or_barrier ()

        | Q.Directive (_, `llarray, _, _) -> immediate_value_or_barrier ~can_skip_toplvl:true ()

        (* with other directive, there is no way to know if the value can be immediate or not *)
        (* this can be optimized, case by case *)
        | Q.Directive _ -> immediate_value_or_barrier ()

      in
      let private_env, defs = List.fold_left_map fold_filter_map private_env defs in
      private_env,
      begin
        match List.flatten defs with
        | [] -> []
        | defs -> List.map (fun def -> Q.NewVal (label, [def])) defs (*[ NewVal defs ]*)
      end

  | Q.NewValRec (label, defs) ->

      let rec fold_map private_env (id, expr) =
        match expr with

        (* hack case : TODO: remove this when this could be a valid pre-condition *)
        | Q.Coerce (_, expr, _ ) -> fold_map private_env (id, expr)

        (* normal case *)
        | Q.Lambda _ ->
          let private_env, il_term = il_of_qml ~can_skip_toplvl:false env private_env expr in
          let private_env, il_term = il_simplification env private_env il_term in
          let toplevel_cont v = QC.ident v in
          begin match il_term with
          | IL.Skip _ -> assert false
          | _ ->
             (* if the lambda is not skipable, only a CPS version is generated *)
            let private_env, expr = qml_of_il ~toplevel_cont env private_env il_term in
            let expr =
              match expr with
              | Q.LetIn (_, [(x,expr)], Q.Ident (_, y)) when Ident.equal x y -> expr
              | _ -> expr
            in private_env, (id, expr)
          end


        | _ ->
            (* FIXME: use OpaError *)
            OManager.i_error
              "Internal error: there should be no recursive non-functional value at this stage: %a"
              QmlPrint.pp#expr expr
      in
      let private_env, defs = List.fold_left_map fold_map private_env defs in
      private_env, [ Q.NewValRec (label, defs) ]

  (* other top level QmlAst.code_elt are ignored and removed *)
  | _ -> private_env , []


let code (env:env) (private_env:private_env) code =
  List.fold_left_collect (code_elt env) private_env code

(* these two functions instrument the code with directives
 * @cps_stack_lambda and @cps_stack_apply
 * @cps_stack_lambda is put just under all the lambdas
 * @cps_stack_apply is put on the Apply Node when what is being applied is not a bypass
 *)
let instrument_expr e =
  let rec aux tra (env:Obj.t option ref option) e =
    match e with
    | Q.Lambda (label, args, e) ->
        let env = ref None in
        let e = aux tra (Some env) e in
        Q.Lambda (label, args, QC.directive (`cps_stack_lambda env) [e] [])
    | Q.Directive (label,`partial_apply info,[Q.Apply (label2,e,el)],tyl) ->
        let e = aux tra env e in
        let el = List.map (aux tra env) el in
        Q.Directive (label,`partial_apply info,[Q.Apply (label2,e,el)],tyl)
    | Q.Apply (_, Q.Directive (_, `restricted_bypass _, _, _), _)
    | Q.Apply (_, Q.Directive (_, `may_cps, _, _), _)
    | Q.Apply (_, Q.Bypass _,_) -> tra env e
    | Q.Apply (label,f,_) ->
        let name_opt =
          match f with
          | Q.Ident (_,i) -> Some (Ident.stident i)
          | _ -> None in
        let pos_opt =
          let pos = Annot.pos label in
          if FilePos.is_empty pos then None else Some (FilePos.to_string pos) in
        let e = tra env e in
        QC.directive (`cps_stack_apply (env,name_opt,pos_opt)) [e] []
    | _ -> tra env e in
  QmlAstWalk.Expr.traverse_map_context_down aux None e

let instrument code =
  List.map
    (function
     | Q.NewVal (label, iel) ->
         Q.NewVal (label, List.map (fun (i,e) -> (i, instrument_expr e)) iel)
     | Q.NewValRec (label, iel) ->
         Q.NewValRec (label, List.map (fun (i,e) -> (i, instrument_expr e)) iel)
     | _ -> assert false)
    code

(* utils for back'ends *)
let cps_pass ~side env qml_code =
  let qml_code = #<If:CPS_STACK_TRACE>instrument qml_code#<Else>qml_code#<End> in
  let private_env_initial = Package.load_dependencies ~side in
  let private_env, r = code env private_env_initial qml_code in
  Package.save_current ~side ~private_env_initial ~private_env;
  let _ =
    #<If:CPS_VERBOSE $minlevel DebugLevel.il_opt_timer>
      debug "il optimisation global time : %f s." (Chrono.read chrono_factorize_letcont) ;
      debug "%d letcont simplified" (Factorize.count ()) ;
      debug "il substitution global time : %f s." (Factorize.chrono_subst ())
    #<End> in

  match private_binding private_env with
  | [] -> r
  | bindings ->
      let label = Annot.nolabel "QmlCpsRewriter.cps_pass" in
      (Q.NewVal (label, bindings)) :: r

let no_cps_pass env code =
  let private_env = private_env_initial () in
  let bp_uncps = qml_bycps_call Opacapi.Opabsl.BslCps.uncps_native in
  (* can't use opacapi *)
  let bp_cps i = qml_bycps_call (Opacapi.Opabsl.BslCps.Notcps_compatibility.cps_native i) in
  let dummy_cont = qml_bycps_call Opacapi.Opabsl.BslCps.Notcps_compatibility.dummy_cont in
  let uncps expr =
    let ident = QmlAstCons.UntypedExpr.string "InsertedByNoCpsPass" in
    QmlAstCons.UntypedExpr.apply bp_uncps [ident; dummy_cont ;expr] in
  let max_arity_projectable = Opacapi.Opabsl.BslCps.Notcps_compatibility.max_cps_native in (* the maximum defined in ByCps.not_cpscompatiblity *)
  let cps key arity expr =
    if arity > max_arity_projectable then (
      OManager.serror "No cps pass can't project cps bypass %a which take functions with arity(=%d) > %d" BslKey.pp key arity max_arity_projectable;
      QmlAstCons.UntypedExpr.int 0
    )
    else
      QmlAstCons.UntypedExpr.apply (bp_cps arity) [expr] in
  let rec get_bp_tyargs bp =
    let k = U.bp_get_key bp in
    match env.bsl_bypass_typer k with
    | BslTypes.Fun (_, args, _) -> args
    | _ -> assert false
  in
  let rewrite expr =
    match expr with
    | Q.Apply (_, Q.Ident _, _) -> expr
    | Q.Apply (_, f, f_args) when (not(U.good_apply_property private_env f f_args))-> expr
    | Q.Apply (label, bypass, f_args) ->
        let key = U.bp_get_key bypass in
        if (env.bsl_bypass_tags key).BslTags.cps_bypass then (
          let f_args = List.fold_left
            (fun (typ_args, res) arg ->
               match typ_args with
               | (BslTypes.Fun (_,x,_))::q -> q, cps key (List.length x) arg::res
               | _::q -> q, arg::res
               | _ -> assert false)
            (get_bp_tyargs bypass, [])
            f_args in
          let f_args = List.rev (snd f_args) in
          let expr = Q.Apply (label, bypass, f_args) in
          uncps expr
        )
        else expr
    | _ -> expr
  in
  QmlAstWalk.CodeExpr.map
        (QmlAstWalk.Expr.map_up rewrite)
        code

(* keep it consistent with the name of the module *)
let serverlib_module_name = "QmlCpsServerLib"
let bsl = "OpabslgenMLRuntime.BslCps"

(* cf mli *)
let meta_cps_utils n =
  let b = FBuffer.create 1024 in
  let b = FBuffer.printf b "let uncps = %s.uncps_ml\n" bsl in
  let b = FBuffer.printf b "let uncps0 s k f = (); fun () -> uncps s k f\n" in
  let b =
    let b =
      let rec aux i acc b =
        if i > n then b else
          let acc = Printf.sprintf "%s x%d" acc i in
          let b = FBuffer.printf b "let uncps%d s k f%s = uncps s k (f%s)\n" i acc acc in
          aux (succ i) acc b
      in
      aux 1 "" b in
    let b = FBuffer.printf b "let cps f k = %s.return k f\n" serverlib_module_name in
    let b = FBuffer.printf b "let cps0 f k = %s.return k (f ())\n" serverlib_module_name in
    let b =
      let rec aux i acc b =
        if i > n then b else
          let acc = Printf.sprintf "%s x%d" acc i in
          let b = FBuffer.printf b "let cps%d f%s k = %s.return k (f%s)\n" i acc serverlib_module_name acc in
          aux (succ i) acc b
      in aux 1 "" b in
    b
  in
  FBuffer.contents b


(*

  TODO: do not generated unused cont introduction
  as in this example: (_v10_cont)

rec _v0_aux(_v0_i, _v7_cont) =
      match _v0_i with
      | 0 ->
          _v6_val =  {}
          @restricted_bypass[cps](%%bslcps_return%%)(_v7_cont, _v6_val)
      | _v1_i ->
          _v5_val =
            _v2_arg =  "toto"
            _v0__v0_bslpervasives_print_endline_skip(_v2_arg)
          _v4_val =  _v0_`_v0_pred_pointer.opa`_skip(_v1_i)
          _v10_cont =
            @restricted_bypass[cps](%%bslcps_ccont%%)(_v7_cont,
              (_v3_val ->
                 @restricted_bypass[cps](%%bslcps_return%%)(_v7_cont, _v3_val)))
          _v0_aux(_v4_val, _v10_cont)
      end
*)
