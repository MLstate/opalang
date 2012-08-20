(*
    Copyright © 2011 MLstate

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

(* @author Valentin Gatien-Baron
   @author Rudy Sicard *)

(* road map:
   add instrumented function as root on client
   add support of @public_env on function definition (see detect_candidate_def)
   fix TODO in detect_candidate_call
   add static dependencies analysis to detect env that contains informations linked to a server_private
   add option to force having @public_env on all functions *)

module Q = QmlAst
module Cons = QmlAstCons.TypedExpr
module List = BaseList

let public_env_warn =
  WarningClass.create
    ~public:true
    ~name:"public_env"
    ~doc:"All public_env directive related warnings"
    ~err:false
    ~enable:true
    ()

let badarg_warn =
  WarningClass.create
    ~parent:public_env_warn
    ~public:true
    ~name:"badarg"
    ~doc:"Warn if the argument of public_env is suspicious or incorrect"
    ~err:false
    ~enable:true
    ()

let badarg_unknownenv =
  WarningClass.create
    ~parent:badarg_warn
    ~public:true
    ~name:"unknownenv"
    ~doc:"Warn if the argument of public_env is not a local definition or not a partial application or not a local function name"
    ~err:true
    ~enable:true
    ()

let badarg_emptyenv =
  WarningClass.create
    ~parent:badarg_warn
    ~public:true
    ~name:"emptyenv"
    ~doc:"Warn if the argument of public_env is empty"
    ~err:false
    ~enable:true
    ()

let warning_set = WarningClass.Set.create_from_list [
  public_env_warn;
  badarg_warn;
  badarg_unknownenv;
  badarg_emptyenv
]

let warn_unknown annot =
  QmlError.warning ~wclass:badarg_unknownenv (QmlError.Context.pos (Annot.pos annot))
    "The argument of @@public_env is not a local function definition or not a partial application or not a local function name"

let warn_empty annot =
  QmlError.warning ~wclass:badarg_emptyenv (QmlError.Context.pos (Annot.pos annot))
    "The argument of @@public_env has no real environment (toplevel or equivalent)"

type env = Ident.t * (Q.ty,unit) QmlGenericScheme.tsc option IdentMap.t

let empty = IdentMap.empty

type ignored_directive = [
| Q.type_directive
| Q.slicer_directive
]

(* detect function declaration tagged with @public_env *)
let rec is_public_env e = match e with
  | Q.Directive (_, `public_env, _, _) -> true
  | Q.Directive (_, #ignored_directive,[e],_) -> is_public_env e
  | _ -> false

let rec rm_top_public_env e =
  match e with
  | Q.Directive (_, `public_env, [e], _) -> e
  | Q.Directive (a,(#ignored_directive as b),[e],c) ->  Q.Directive (a,b,[rm_top_public_env e],c)
  | e -> e

let detect_candidate_def1 set def = match def with
  | (Q.NewVal (label,iel) | Q.NewValRec (label,iel)) when List.exists (fun (_,e) -> is_public_env e) iel ->
    let set = List.fold_left (fun set (i,e) -> if is_public_env e then IdentSet.add i set else set) set iel in
    set, Q.NewValRec (label,List.map (fun (i,e) -> i,rm_top_public_env e) iel)
  | _ -> set, def

let detect_candidate_def code = List.fold_left_map detect_candidate_def1 IdentSet.empty code

(* detect elligible call site, i.e. tagged with @publish_env or calling @publish_env function (see above)
   also warn for bad use of the directive => not a partial call

   to simplify the usability of the directive, ident on explicit and implicit toplevel construction are considered as partial call (but with a warning class):
    first, some explicit partial application like f(1,_) are translated to toplevel functions because their environement is static (=> no env in the closure)
    second, environement of toplevel construct is empty so the directive would have no effect anyway
*)
let detect_candidate_call always_serialize code =
  let force_rewrite = ref false in
  let rec public_env a local need_instrumentation e =  match e with
    (* partial apply cases *)
    | Q.Directive (_, `partial_apply (_,false), [Q.Apply (_, Q.Ident (_, i), _args)], _) ->
      local,IdentSet.add i need_instrumentation

    (* ident cases *)
    | Q.Ident(_, i) ->
      (if IdentSet.mem i local then warn_unknown else warn_empty) a;
      force_rewrite:=true;
      local,IdentSet.add i need_instrumentation

    (* traverse directives *)
    | Q.Directive (_, #ignored_directive , [e], _) -> public_env a local need_instrumentation e

    (* bad cases *)
    | _ ->
      warn_unknown a;
      force_rewrite:=true;
      local,need_instrumentation

  in
  let _, set = QmlAstWalk.CodeExpr.fold
    (QmlAstWalk.Expr.fold
       (fun (local,need_instrumentation) e ->
         match e with
         | Q.Directive (a, `public_env ,[e], _ ) -> public_env a local need_instrumentation e
         | Q.Directive (a, `public_env , _ , _) -> (* should not parse *)
           QmlError.error (QmlError.Context.pos (Annot.pos a)) "@publish_env with more than one parameter"

         (* partial apply cases *)
         | Q.Directive (_, `partial_apply (_,false),
                        [Q.Apply (_, Q.Ident (_, i), _args)]
                          , _)
             when IdentSet.mem i always_serialize
           -> local,IdentSet.add i need_instrumentation

         (* TODO bind in pattern are missing => bad warning class for some idents *)
         | Q.LetIn(_, decl, _)
         | Q.LetRecIn(_, decl, _) ->
           let add local (id,_) = IdentSet.add id local in
           (List.fold_left add local decl),need_instrumentation
         | Q.Lambda(_ ,param, _ ) ->
           let add local id = IdentSet.add id local in
           (List.fold_left add local param),need_instrumentation

         | _ -> local,need_instrumentation
       )
    ) (IdentSet.empty,IdentSet.empty) code
  in set, !force_rewrite || not(IdentSet.is_empty set)

let extract_env_type env_size gamma ty =
  match QmlTypesUtils.Inspect.get_arrow_through_alias_and_private gamma ty with
  | Some (l1,ret) ->
      assert (List.length l1 >= env_size);
      let l1, l2 = List.split_at env_size l1 in
      l1, Q.TypeArrow (l2, ret), l2, ret
  | None -> assert false

(* generate instrumented version of the function
   a(env,p1,p2) = expr
   =>
   a'(env) = `partial_call(a(env)) with extra ei annotation
*)
let generate_typeofer need_instrumentation gamma annotmap env (i,e) =
  match e with
  | Q.Directive (_, `lifted_lambda (env_size, function_of_origin), [_], _) when IdentSet.mem i need_instrumentation ->
      let new_i = Ident.refreshf ~map:"%s_ser" i in
      let tsc_gen_opt = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap in
      let ty_i = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
      let ty_i =
        (* refreshing or else ei will stupidly propagate type vars to the original def *)
        let tsc = QmlTypes.Scheme.quantify ty_i in
        let tsc = QmlTypes.Scheme.refresh tsc in
        let _quant, ty_i, () = QmlGenericScheme.export_unsafe tsc in
        ty_i in
      let ty_env, ty_remaining, ty_args, _ty_ret = extract_env_type env_size gamma ty_i in
      let annotmap, g = Cons.ident annotmap i (*ty_i*) (Q.TypeArrow (ty_env, ty_remaining)) in
      let annotmap = QmlAnnotMap.add_tsc_inst_opt (Q.QAnnot.expr g) tsc_gen_opt annotmap in
      let new_tsc_gen_opt, gamma =
        let ty = Q.TypeArrow (ty_env,ty_remaining) in
        let tsc = QmlTypes.Scheme.quantify ty in
        let gamma = QmlTypes.Env.Ident.add i tsc gamma in
        let tsc_opt =
          if QmlGenericScheme.is_empty tsc then
            None
          else
            Some tsc in
        tsc_opt, gamma in
      let params = List.init env_size (fun i -> Ident.next ("eta_" ^ string_of_int i)) in
      let annotmap, args = List.fold_left_map2 (fun annotmap i ty -> Cons.ident annotmap i ty) annotmap params ty_env in
      let annotmap, apply_g = Cons.apply_partial gamma annotmap g args in
      let partial_apply = `partial_apply (Some (List.length ty_args), true) in
      let annotmap, typeofs =
        List.fold_left_map2
          (fun annotmap i ty ->
             let annotmap, i = Cons.ident annotmap i ty in
             Cons.directive annotmap `typeof [i] []
          ) annotmap params ty_env in
      let annotmap, body =
        let label = Annot.refresh (Q.Label.expr e) in
        let annotmap = QmlAnnotMap.add_ty_label label ty_remaining annotmap in
        annotmap, Q.Directive (label,partial_apply,apply_g::typeofs,[]) in
      let annotmap, fun_ = Cons.lambda annotmap (List.combine params ty_env) body in
      (* the @lifted_lambda is for the slicer, so that it puts the function on the right side
       * (which is the side of function_of_origin)
       * this probably won't work when we have local annotation, because this function should
       * be on the side of the lambda it is created from instead *)
      let annotmap, fun_ = Cons.directive_id annotmap (`lifted_lambda (0, function_of_origin)) fun_ in
      let annotmap =
        QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr fun_) new_tsc_gen_opt annotmap in
      let env = IdentMap.add i (new_i, new_tsc_gen_opt) env in
      Some (gamma, annotmap, env, new_i, fun_)
  | _ ->
      None

(* generate instrumented version of all declarations *)
let generate_new_binding need_instrumentation (gamma, annotmap, env) iel =
  List.fold_left_filter_map
    (fun (gamma, annotmap, env) (i,e) ->
       match generate_typeofer need_instrumentation gamma annotmap env (i,e) with
       | None -> (gamma, annotmap, env), None
       | Some (gamma, annotmap, env, i, e) -> (gamma, annotmap, env), Some (i,e)
    ) (gamma, annotmap, env) iel

let generate_instrumented_functions need_instrumentation gamma annotmap code =
  List.fold_left_collect
    (fun acc code_elt ->
      match code_elt with
      | Q.NewVal (label,iel) ->
        let acc, new_iel = generate_new_binding need_instrumentation acc iel in
        let code =
          if new_iel = [] then
            [code_elt]
          else
            [code_elt; Q.NewVal (Annot.refresh label,new_iel)] in
        acc, code
      | Q.NewValRec (label,iel) ->
        let acc, new_iel = generate_new_binding need_instrumentation acc iel in
        let code = [Q.NewValRec (label,iel @ new_iel)] in
        acc, code
      | _ ->
        assert false
    ) (gamma, annotmap, empty) code

(* update call elligible site *)
let rewrite_identifiers always_serialize env annotmap code =
  let new_call_site labeli i =
    let new_ident, tsc_opt = IdentMap.find i env in
    let rw_ident e = match e with | Q.Ident (label, _) when label=labeli -> Q.Ident (labeli, new_ident)
                                  | _-> e in
    fun annotmap e ->
    let e = QmlAstWalk.Expr.map rw_ident e in
    let annotmap = QmlAnnotMap.remove_tsc_inst_label labeli annotmap in
    let annotmap = QmlAnnotMap.add_tsc_inst_opt_label labeli tsc_opt annotmap in
    annotmap, e
  in
  let rec get_ident e = match e with
    | Q.Ident (labeli, i) -> Some((labeli,i))
    | Q.Directive (_,#ignored_directive,[e],_) -> get_ident e
    | _ -> None
  in
  let rm_public_env e = match e with
    | Q.Directive (_, `public_env, ([]|_::_::_) , _ ) -> assert false (* see detect_candidate_call *)
    | Q.Directive (_, `public_env, [e], _ ) -> true,e
    | _ -> false,e
  in
  let rec rw_call_site ~has_public_env annotmap e =
    let no_changes = e in
    match e with
    | (Q.Ident _ as id) as e
    | Q.Directive (_, `partial_apply (_,false), [Q.Apply (_, id , _ ) as e], _)
        -> begin match get_ident id with
            | Some((labeli,id)) when has_public_env || IdentSet.mem id always_serialize ->
              new_call_site labeli id annotmap e
            | _ -> (annotmap,no_changes)
        end

    | Q.Directive (a,(#ignored_directive as b),[e],c) ->
      let annotmap,e = rw_call_site ~has_public_env annotmap e in
      annotmap, Q.Directive (a,b,[e],c)

    | _ -> (annotmap,no_changes)
  in
  let rw annotmap e =
    let has_public_env, e = rm_public_env e in
    rw_call_site ~has_public_env annotmap e
  in
  QmlAstWalk.CodeExpr.fold_map
    (QmlAstWalk.Expr.foldmap
       rw
    ) annotmap code


let process_code gamma annotmap code =
  let always_serialize, code = detect_candidate_def code in
  let need_instrumentation, need_rewrite = detect_candidate_call always_serialize code in
  let need_client_code = IdentSet.union need_instrumentation always_serialize in
  let (gamma, annotmap, env), code = if not(IdentSet.is_empty need_instrumentation)
    then generate_instrumented_functions need_instrumentation gamma annotmap code
    else (gamma, annotmap, empty), code in
  let annotmap, code = if need_rewrite then rewrite_identifiers env annotmap code else (annotmap,code) in
  gamma, annotmap, code, need_client_code
