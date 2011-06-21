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

(* depends in base *)
module Format = BaseFormat
module String = BaseString
module List = BaseList

(* refactoring in progress *)
module QmlCons = QmlAstCons

(* alias *)
module TypedExpr = QmlCons.TypedExpr
module TypedPat = QmlCons.TypedPat

(* shorthand *)
module Q = QmlAst

(* -- *)

#<Debugvar:RPC_DEBUG>

(* ******************************************************************)
(* Some utilities functions *****************************************)
(* ******************************************************************)

let full_apply gamma annotmap fun_ tys args =
  let annotmap, e = QmlAstCons.TypedExpr.apply_partial gamma annotmap fun_ (tys @ args) in
  QmlAstCons.TypedExpr.directive annotmap (`full_apply (List.length tys)) [e] []

type publish_directive = [`ajax_publish of [`sync | `async] | `comet_publish ]

type call_directive = [`ajax_call of [`sync | `async] | `comet_call]

type insert_directive = [ `insert_server_value of Ident.t ]

type producted_directive = [ `hybrid_value ]

type all_input_directives = [ publish_directive | call_directive | insert_directive ]

type options = {
  optimize_insert : int;
  optimize_publish : int;
  optimize_call : int;
}

let default_options = {
  optimize_insert = 0;
  optimize_publish = 0;
  optimize_call = 0;
}

let warn_olvl = OManager.warning ~wclass:WarningClass.warn_olevel

let verbose fmt = OManager.printf ("InsertRemote : "^^fmt^^"@.")

let debug_print _f =
  #<If> OManager.printf "InsertRemote : %s@\n" (_f ())
  #<Else> ()
  #<End>
let debug_string str = debug_print (fun _ -> str)

let debug_do _f =
  #<If> _f ()
  #<Else> ()
  #<End>

let debug_code ?(str="") _code =
  ignore(str);
  #<If>
  verbose "DEBUG CODE %s" str;
  debug_do
    (fun _ ->
       List.iter
         (function
            | Q.NewVal (_, l) | Q.NewValRec (_, l) ->
                List.iter (
                  fun (ident, expr) ->
                    if String.is_contained (Option.get DebugVariables.rpc_debug)
                      (Ident.original_name ident)
                    then debug_string
                      (Format.sprintf "%s | %a = %a" str
                         QmlPrint.pp#ident ident
                         QmlPrint.pp#expr expr);
                ) l
            | _ -> ()
         )
         _code
    )
  #<End>

(* Generate optimized code if true *)
(* TODOK1 - Make some other optimization:
   - Do not instantiate at runtime if they aren't type variable
   - ...
*)
let optimized = false

let directive_to_side = function
  | `ajax_publish _ | `comet_call -> `server
  | `comet_publish | `ajax_call _ | `insert_server_value _ -> `client

let call_directive_to_sync = function
  | `ajax_call b -> b
  | _ -> `sync

let publish_directive_to_sync = function
  | `ajax_publish b -> b
  | _ -> `sync

let _call, _stub, _skel, _insert, _reset, get_info =
  #<If>
    let rc = ref 0
    and rs = ref 0
    and rsk = ref 0
    and ri = ref 0
    in
    ((fun _ -> incr rc),
     (fun _ -> incr rs),
     (fun _ -> incr rsk),
     (fun _ -> incr ri),
     (fun _ -> rc := 0; rs := 0; rsk := 0; ri := 0),
     (fun _ -> !rc, !rs, !rsk, !ri)
    )
  #<Else>
    ((fun _ -> ()),
     (fun _ -> ()),
     (fun _ -> ()),
     (fun _ -> ()),
     (fun _ -> ()),
     (fun _ -> -1, -1, -1, -1)
    )
  #<End>

let debug_info _side =
  #<If>
  debug_do
    (fun _ ->
       let c, s, sk, i = get_info () in
       let _side =
         match _side with `server -> "server" | `client -> "client" in
       verbose
         "%i stub(s) generated on %s for %i call(s) resolved. " s _side c;
       verbose
         "%i skeleton(s) generated on %s" sk _side;
       verbose
         "%i insert values resolved on %s" i _side
    )
  #<Else> ()
  #<End>

let check_hybrid_value annotmap e1 e2 =
  let t1 = QmlAnnotMap.find_ty (Q.QAnnot.expr e1) annotmap in
  let t2 = QmlAnnotMap.find_ty (Q.QAnnot.expr e2) annotmap in
  match t1, t2 with
  | Q.TypeArrow ([Q.TypeConst Q.TyString], Q.TypeVar _),
    Q.TypeConst Q.TyString -> true
  | _ -> false

(* ******************************************************************)
(* Retrieve OPA type and functions **********************************)
(* /!\ WARNING : DON'T USE BELOW FUNCTIONS UNTIL YOU READ /!\ *******)
(* /!\ COMMENT of preprocess_types. /!\ *****************************)
(* ******************************************************************)

(** *)
module TyIdent = struct

  let default_pos name = Q.Ident (Annot.nolabel "Opa_InsertRemote", Ident.source name)

  let get name ~side ?(posexpr=default_pos name) annotmap gamma =
    try
      let ident = OpaMapToIdent.val_ ~side name in
      let (ty:QmlAst.ty) = QmlTypes.Scheme.instantiate (QmlTypes.Env.Ident.find ident gamma) in
      ( (* manual check, to verify that explicit instantiation has done its job *)
        match name, ty with
        | "OpaSerialize_serialize", Q.TypeArrow ([_] , Q.TypeArrow([_], Q.TypeConst Q.TyString)) -> ()
        | "OpaSerialize_serialize", _ ->
           OManager.i_error "BAD TYPE FOR %s %a@." name QmlPrint.pp#ty ty;
        | _ -> ()
      );
      TypedExpr.ident annotmap ident ty
    with Not_found ->
      let context = QmlError.Context.annoted_expr annotmap posexpr in
      QmlError.cond_violation QmlAlphaConv.Check.unbound_id context
        "Missing ident"

end


(** This module allows access to some values and types defined on
    module OpaType (written on opa). This expression are well
    typed. For more information see the corresponding module written
    in OPA. *)
module OpaType = struct

  let tsc = ref Pass_ExplicitInstantiation.opatsc_type

  let ty = ref Pass_ExplicitInstantiation.opatype_type

  let row = ref Pass_ExplicitInstantiation.oparow_type

  let col = ref Pass_ExplicitInstantiation.opacol_type

  (** Type of list(OpaType.ty)*)
  let list_of_opaty =
    ref (Q.TypeName ([!ty], Q.TypeIdent.of_string Opacapi.Types.list))

  let tsc_implementation = TyIdent.get Opacapi.OpaTsc.implementation

end

(** This module allows access to some values and types defined on
    module OpaSerialize (written on opa). This expression are well
    typed. For more information see the corresponding module written
    in OPA. *)

module OpaSerialize = struct

  let serialize          = TyIdent.get Opacapi.OpaSerialize.serialize
  let serialize_for_js   = TyIdent.get Opacapi.OpaSerialize.serialize_for_js
  let unserialize        = TyIdent.get Opacapi.OpaSerialize.unserialize
  let unserialize_unsafe = TyIdent.get Opacapi.OpaSerialize.unserialize_unsafe

end

(** This module allows access to some values and types defined on
    module OpaRPC (written on opa). This expression are well
    typed. For more information see the corresponding module written
    in OPA. *)
module OpaRPC = struct

  let request =
    ref (Q.TypeName ([], Q.TypeIdent.of_string Opacapi.Types.OpaRPC.request))

  let taux_extract_values =
    let t =
      QmlAst.TypeName ([], Q.TypeIdent.of_string Opacapi.Types.list) in
    ref (Q.TypeName ([t], Q.TypeIdent.of_string Opacapi.Types.option))

  let serialize      = TyIdent.get Opacapi.OpaRPC.serialize
  let empty_request  = TyIdent.get Opacapi.OpaRPC.empty_request
  let add_var_types  = TyIdent.get Opacapi.OpaRPC.add_var_types
  let add_row_types  = TyIdent.get Opacapi.OpaRPC.add_row_types
  let add_col_types  = TyIdent.get Opacapi.OpaRPC.add_col_types
  let add_args_with_type = TyIdent.get Opacapi.OpaRPC.add_args_with_type
  let unserialize    = TyIdent.get Opacapi.OpaRPC.unserialize
  let extract_types  = TyIdent.get Opacapi.OpaRPC.extract_types
  let extract_values = TyIdent.get Opacapi.OpaRPC.extract_values

  let send_to_other_side ~side ~sync = TyIdent.get
    (if side = `client then
       match sync with
       | `sync -> Opacapi.OpaRPC.client_send_to_server
       | `async -> Opacapi.OpaRPC.client_async_send_to_server
    else
      Opacapi.OpaRPC.server_send_to_client) ~side

  let fake_stub  = TyIdent.get Opacapi.OpaRPC.fake_stub
  let error_stub = TyIdent.get Opacapi.OpaRPC.error_stub

  let try_cache ~side = TyIdent.get
    (match side with
     | `client -> Opacapi.OpaRPC.client_try_cache
     | `server -> Opacapi.OpaRPC.server_try_cache) ~side


  module Dispatcher = struct

    let ty_str = Q.TypeConst Q.TyString

    let ty_opt_str =
      ref (Q.TypeName ([ty_str], Q.TypeIdent.of_string Opacapi.Types.option))

    let register ~side = TyIdent.get
      (if side = `client then Opacapi.OpaRPC.client_dispatcher_register
       else Opacapi.OpaRPC.server_dispatcher_register) ~side
  end

end

module Opa2Js =
struct
  let to_string  = TyIdent.get Opacapi.Opa2Js.to_string
end

(** /!\ WARNING /!\

    This function must be used before all functions above. If this
    function is not called before you use above functions,
    QmlFlatCompiler will make an assert failure.

    Note : All types are refences and are updated by this
    function. I'm accord to you it's not very nice, but I don't want
    had a gamma argument on almost above function.
*)
let preprocess_types gamma =
  let process = QmlTypes.type_of_type gamma in
  OpaType.tsc := process Pass_ExplicitInstantiation.opatsc_type;
  OpaType.ty := process Pass_ExplicitInstantiation.opatype_type;
  OpaType.list_of_opaty :=
    process (Q.TypeName ([!OpaType.ty], Q.TypeIdent.of_string Opacapi.Types.list));
  OpaRPC.request :=
    process (Q.TypeName ([], Q.TypeIdent.of_string Opacapi.Types.OpaRPC.request));
  OpaRPC.taux_extract_values :=
    (let t = process (Q.TypeName ([], Q.TypeIdent.of_string Opacapi.Types.list)) in
     process (Q.TypeName ([t], Q.TypeIdent.of_string Opacapi.Types.option)));
  OpaRPC.Dispatcher.ty_opt_str :=
    process (Q.TypeName ([Q.TypeConst Q.TyString], Q.TypeIdent.of_string Opacapi.Types.option))



(* ******************************************************************)
(* Utils functions for generate some expressions ********************)
(* ******************************************************************)

(** Update gamma *)
let update_gamma gamma id ty =
  QmlTypes.Env.Ident.add id (QmlTypes.Scheme.quantify ty) gamma

(** Generate typed variable.
    @param name Name of variable
    @param ty Type of variable
    @return (annotmap, gamma, id, ty, expr) [id] is the ident of
    variable. [ty] is the type of this variable. [expr] is expression
    of this variable.
*)
let generate_var ~annotmap ~gamma name ty =
  let ident = (Ident.next name) in
  let (annotmap, expr) = TypedExpr.ident annotmap ident ty in
  (annotmap, gamma(* update_gamma gamma ident ty *), ident, ty, expr)

(** Generate typed variables. Name of this variables is
    "prefix[1..n]".
    @param Prefix of variables.
    @param Type of variables.
    @param n Number of variables to generate.
    @return (annotmap, gamma, list of ident, list of expression)
*)
let generate_vars ~annotmap ~gamma prefix ty =
  let rec aux annotmap gamma id_l ex_l = function
    | 0 -> annotmap, gamma, id_l, ex_l
    | n -> let annotmap, gamma, id, _, ex =
        generate_var ~annotmap ~gamma (Printf.sprintf "%s%d" prefix n) ty in
      aux annotmap gamma (id::id_l) (ex::ex_l) (n-1) in
  aux annotmap gamma [] []

(** Generate a match on option, like this in OPA syntax :
    match [expr] with
    | {some = [id_expr]} -> [ok_expr]
    | {none} -> ko_expr

    @param expr Expression to match. This expression must be type of
    option.
    @param ident_list A list of ident for bind elements in matched
    list.
    @param ok_expr Expression when match success. Il this expression
    you can use [id_expr].
    @param ko_expr Expression when match failed.
    @return (annotmap, match expression).

*)
let generate_match_some ~annotmap ~gamma expr id_expr ty_id ok_expr ko_expr =
  let annotmap, patvar =
    TypedPat.var annotmap id_expr ty_id in
  TypedPat.match_option annotmap gamma expr patvar ok_expr ko_expr

(** Generate a pattern on list. *)
let generate_pat_list ~annotmap ident_list ty =
  let annotmap, pat_list =
    let annotmap, e1 = TypedPat.emptyrecord annotmap in
    TypedPat.record annotmap ["nil", e1]
  in
  List.fold_right
    (fun ident (annotmap, acc) ->
       let annotmap, pat = TypedPat.var annotmap ident ty in
       TypedPat.record annotmap [
         "hd", pat ;
         "tl", acc ;
       ]
    ) ident_list (annotmap, pat_list)

(** Generate a match on list(option), like this in OPA syntax :
    match [expr] with
    | {some = [ident_list]} -> [ok_expr]
    | _ -> [ko_expr]

    @param expr Expression to match. This expression must be type of
    option(list).
    @param ident_list A list of ident for bind elements in matched list.
    @param ok_expr Expression when match success. In this expression
    you can use ident of [ident_list].
    @param ko_expr Expression when match failed.
    @return (annotmap, match expression).

*)
let generate_match_some_list ~annotmap expr ident_list ok_expr ko_expr ty_ident =
  let annotmap, pat_list = generate_pat_list ~annotmap ident_list ty_ident in
  let annotmap, pat_some =
    TypedPat.record annotmap ["some", pat_list] in
  let annotmap, any = TypedPat.any annotmap in
  TypedExpr.match_ annotmap expr [(pat_some, ok_expr); (any, ko_expr)]

(**
   Generates the following expression:
   match $expr$ with
   | {field1 = [$ident_list_1$]; field2 = [$ident_list_2$], ...} -> $ok_expr$
   | _ -> $ko_expr$
*)
let generate_match_record_of_list ~annotmap expr ident_list_list ok_expr ko_expr =
  let annotmap, pat_list_list =
    List.fold_left_map
      (fun annotmap (field, ident_list, ty_ident) ->
         let annotmap, l = generate_pat_list ~annotmap ident_list ty_ident in
         annotmap, (field, l)
      ) annotmap ident_list_list in
  let annotmap, pat = TypedPat.record annotmap pat_list_list in
  let annotmap, any = TypedPat.any annotmap in
  TypedExpr.match_ annotmap expr [(pat, ok_expr); (any, ko_expr)]




(* ******************************************************************)
(* Main functions for resolving directives **************************)
(* ******************************************************************)

(** Retrieve a registering string from ident. This string it's the end
    of url where skeleton is published.
    It is NOT related to the closure identifiers
*)
let ident_to_registering renamingmap ident =
  (*try*)
    Ident.to_uniq_string (
        QmlRenamingMap.original_from_new renamingmap ident
    )
  (*with
    Not_found ->
      Printf.printf "Couldn't find original name of %s\n%!" (Ident.to_string ident);
      Ident.to_uniq_string ident*)

(** Check given [expr], and retrieve associated expression, ident, and
    type in explicit map.*)
let check_and_get ?(msg="") ~annotmap ~gamma:_ explicit_map expr =
  let iv expr str =
    invalid_arg
      (Format.sprintf
         "%s %s : unexpected expression => %a"
         msg str QmlPrint.pp#expr expr
      ) in
  match expr with
  | Q.Ident (label,ident) ->
      let o =
        try IdentMap.find ident explicit_map
        with Not_found ->
          OManager.i_error
            "Opa_InsertRemote: cannot find %s in the explicit_map"
            (Ident.to_string ident) in
      begin
        match o with
        | Some (label2, ident, kind) ->
            (* the identifier was rewritten by ei *)
            let expr2 = Q.Ident (label2, ident) in
            #<If>
              if String.is_contained (Option.get DebugVariables.rpc_debug)
                (Ident.original_name ident)
              then
                verbose
                  "@[<v2>Identifier rewritten by ei:@ the one that takes all the type variables:%a@ optimized:%a@]"
                  QmlPrint.pp#ident ident
                  QmlPrint.pp#expr expr
            #<End>;
            (* Get type and scheme of explicit instantiate expression *)
            let ty = QmlAnnotMap.find_ty_label label2 annotmap in
            let tsc = QmlTypes.Scheme.quantify ty in
            (* get nb of ty var *)
            let nb_tyvar,nb_rowvar,nb_colvar = QmlGenericScheme.full_arity tsc in
            (* Get original type and scheme of expression (before exp-inst) *)
            let oty = QmlAstCons.Type.Arrow.drop (nb_tyvar+nb_rowvar+nb_colvar) ty in
            let otsc = QmlTypes.Scheme.quantify oty in
            expr2, ident, ty, tsc, nb_tyvar, nb_rowvar, nb_colvar, oty, otsc, kind

        | None ->
            (* the identifier was not rewritten by ei *)
            #<If>
              if String.is_contained (Option.get DebugVariables.rpc_debug)
                (Ident.original_name ident)
              then
                verbose
                  "Identifier not rewritten by ei: %a = %a"
                  QmlPrint.pp#ident ident
                  QmlPrint.pp#expr expr
            #<End>;
            let ty = QmlAnnotMap.find_ty_label label annotmap in
            let tsc = QmlTypes.Scheme.quantify ty in
            expr, ident, ty, tsc, 0, 0, 0, ty, tsc, `one_lambda
      end
  | _ -> iv expr "on an non ident expression"

(* ***********)
(* SKELETONS *)
(* ***********)

(** Generate a skeleton from an expression. This function is used
    for resolve [@ajax_publish, @comet_publish].
    @param expr Expression to make a skeleton. This expression must be an
    ident else raise Invalid_argument exception.
    @return A skeleton for given [expr].

    This generate skeleton maybe used both on server and client.

    If the call is synchronous, generate a skeleton such as (in OPA style):
      fun str ->
        match OpaRPC.unserialize str with
        | {some = request} ->
          extracted_types = OpaRPC.extract_type request
          (match extracted_types with
           | {types=[v1; v2; ...]; rows=[r1,r2,...]; cols=[c1,c2,...]} ->
             (match OpaTsc.implementation extracted_types tsc with
              | { TyArrow_params = ts; TyArrow_res = tres } ->
                (match  OpaRPC.extract_value ts /* or [] if not a function type */ request  with
                | { some = [a1; a2; a3; ...] } ->
                  { some = OpaSerialize.serialize tres
                        (sliced_fun v1 v2 ... r1 r2 ... c1 c2 ... a1 a2 a3 ...) }
                | _ -> { none = () })
              | _ -> { none = () })
           | _ -> { none = () })
        | _ -> { none = () }
      : string -> string option

    If the call is asynchronous, generate a skeleton such as (in OPA style):
      fun str ->
        match OpaRPC.unserialize str with
        | {some = request} ->
          (match OpaRPC.extract_type request with
           | [i1; i2; ...] ->
             (match OpaTsc.implementation [i1; i2; ...] tsc with
              | { TyArrow_params = [t1; t2; t3; ...]; TyArrow_res = tres } ->
                (match  OpaRPC.extract_value [t1; t2; t3; ...] request  with
                | { some = [a1; a2; a3; ...] } ->
                     do Scheduler.push (-> sliced_fun i1 i2 ... a1 a2 a3 ...)
                     { some = "Async" }
                | _ -> { none = () })
              | _ -> { none = () })
           | _ -> { none = () })
        | _ -> { none = () }
      : string -> string option

    Generated skeleton is good only if explicit instantiation has already taken place.

    Execution of the generated skeleton:
    1 - Unserialize RPC request represented by argument of the
    skeleton.
    2 - Extract instantiate types include in request (if original type
    scheme have type variable).
    3 - Extract value and check with instantiate type of function.
    4 - Call to the function with type arguments and values (either sync or Scheduler.push)
    5 - If sync, serialize the response.
    5 - If async, send standard reply.

    @param sync True if the call is synchronous, false if it is asynchronous i.e. result doesn't matter
    @param expr The body of the function
*)
let generate_skeleton explicit_map ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr =
  (* Check and get *)
  let expr, ident, _ty, _tsc, nb_tyvar, nb_rowvar, nb_colvar, oty, otsc, number_of_lambdas =
    check_and_get ~msg:"generate_skeleton" ~annotmap ~gamma  explicit_map expr
  in

  #<If>
    if String.is_contained (Option.get DebugVariables.rpc_debug)
      (Ident.original_name ident)
    then
      verbose
        "SKELETON %a = @\n Type : %a@\n OriginalType : %a@\nExprType : %a@\n"
        QmlPrint.pp#ident ident
        QmlPrint.pp#ty _ty
        QmlPrint.pp#ty oty
        QmlPrint.pp#ty (QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap)
    else ()
  #<Else>()
  #<End>;

  (* Argument of generated skeleton *)
  let annotmap, gamma, ident_arg, ty_arg, expr_arg =
    generate_var ~annotmap ~gamma "str" (Q.TypeConst Q.TyString) in

  (* Some variables of generated skeleton *)
  let annotmap, gamma, ident_req, ty_req, expr_req =
    generate_var ~annotmap ~gamma "request" !OpaRPC.request in

  (* Opa type scheme expression *)
  let annotmap, opatsc =
    Pass_ExplicitInstantiation.tsc_to_opatsc ~val_:OpaMapToIdent.val_ ~side (annotmap, gamma) otsc in

  (* List of ident for types extracted *)
  let annotmap, gamma, ident_ty_list, list_expr_ty =
    generate_vars ~annotmap ~gamma "v" !OpaType.ty nb_tyvar in
  let annotmap, gamma, ident_row_list, list_expr_row =
    generate_vars ~annotmap ~gamma "r" !OpaType.row nb_rowvar in
  let annotmap, gamma, ident_col_list, list_expr_col =
    generate_vars ~annotmap ~gamma "c" !OpaType.col nb_colvar in

  (* Pattern arrow and associated vars *)
  let annotmap, gamma, ident_tres, ty_tres, expr_tres =
    generate_var ~annotmap ~gamma "tres" !OpaType.ty in
  let annotmap, gamma, pat_arrow, n_args, expr_ins_list, is_a_function =
    let rec aux annotmap gamma = function
      | Q.TypeArrow (lt, _res) ->
          let n_args = List.length lt in
          let ts = Ident.next "ts" in
          (* FIXME: give a real type *)
          let ts_type = QmlAstCons.Type.next_var () in
          let annotmap, ts_expr = TypedExpr.ident annotmap ts ts_type in
          let annotmap, pat_ts = TypedPat.var annotmap ts ts_type in
          let annotmap, pat_res = TypedPat.var annotmap ident_tres ty_tres in
          let annotmap, pat =
            TypedPat.record annotmap [
              "TyArrow_params", pat_ts ;
              "TyArrow_res", pat_res ;
            ] in
          annotmap, gamma, pat, n_args, ts_expr, true
      | Q.TypeName (list, tyident) ->
          aux annotmap gamma
            (QmlTypesUtils.Inspect.find_and_specialize gamma tyident list)
      | _ ->
          let n_args = 0 in
          let annotmap, pat_res = TypedPat.var annotmap ident_tres ty_tres in
          let annotmap, ts_expr = TypedExpr.list (annotmap, gamma) [] in
          annotmap, gamma, pat_res, n_args, ts_expr, false in
    aux annotmap gamma oty in

  (* List of ident for values extracted *)
  let ty_ident_val_list = QmlCons.Type.typevar (Q.TypeVar.next ()) in
  let annotmap, gamma, ident_val_list, list_expr_val =
    generate_vars ~annotmap ~gamma "a" ty_ident_val_list n_args in

  (* Call to encapsulated function *)
  let annotmap, fun_call =
    let annotmap, expr =
      TypedExpr.shallow_copy annotmap expr in
    (* NOT SURE: what if the expression already has no arguments? *)
    (*try*)
      if is_a_function then
        let ((annotmap, typed_fun_call_expr) as typed_fun_call) =
          let args_ty = list_expr_ty @ list_expr_row @ list_expr_col in
          match number_of_lambdas with
          | `one_lambda -> full_apply gamma annotmap expr args_ty list_expr_val
          | `two_lambdas ->
              let annotmap, apply1 = QmlAstCons.TypedExpr.apply gamma annotmap expr args_ty in
              QmlAstCons.TypedExpr.apply gamma annotmap apply1 list_expr_val in
        match sync, side with
        | `sync, _
        | _, `client -> typed_fun_call
        | `async, `server ->
          let (annotmap, typed_async_fun_call)  = TypedExpr.lambda annotmap [] typed_fun_call_expr in
          let (annotmap, typed_oparpc_executor) = TyIdent.get Opacapi.OpaRPC.server_async_execute_without_reply ~side annotmap stdlib_gamma
          in
          TypedExpr.apply gamma annotmap typed_oparpc_executor [typed_async_fun_call]
          (*
            This is <<OpaRPC_Server.async_no_reply(-> $typed_fun_call$)>>
          *)
      else (
        assert (list_expr_val = []);
        TypedExpr.may_apply gamma annotmap expr (list_expr_ty @ list_expr_row @ list_expr_col)
      )
    (*with e -> Format.printf "FAILURE:@\n%a@." QmlPrint.pp#expr fun_call; raise e*)
  in

  (* Call to serialize function *)
  let annotmap, call_ser =
    let annotmap, ser = OpaSerialize.serialize ~side annotmap stdlib_gamma in
    full_apply gamma annotmap ser [expr_tres] [fun_call] in

  (* Call to extract_values *)
  let annotmap, call_ext_val =
    let annotmap, ext = OpaRPC.extract_values ~side annotmap stdlib_gamma in
    TypedExpr.apply gamma annotmap ext [expr_req; expr_ins_list] in

  (* Match values *)
  let annotmap, match_values =
    let annotmap, ok_expr = TypedExpr.some annotmap gamma call_ser in
    let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr call_ser) annotmap in
    let annotmap, ko_expr =
      #<If>
        let annotmap, es =
          OpaRPC.error_stub ~side annotmap stdlib_gamma in
        let annotmap, msg =
          TypedExpr.string annotmap "match_values" in
        TypedExpr.apply gamma annotmap es [msg]
      #<Else>
        TypedExpr.none ~ty annotmap gamma
      #<End>
    in
    generate_match_some_list ~annotmap
      call_ext_val ident_val_list ok_expr ko_expr ty_ident_val_list
  in

  let ident_extracted_types = Ident.next "extracted_types" in
  (* FIXME: give a real type *)
  let type_ident_extracted_types = QmlAstCons.Type.next_var () in

  (* Call to instantiate *)
  let annotmap, call_ins =
    let annotmap, ins = OpaType.tsc_implementation ~side annotmap stdlib_gamma in
    let annotmap, expr_ty_list = TypedExpr.ident annotmap ident_extracted_types type_ident_extracted_types in
    TypedExpr.apply gamma annotmap ins [expr_ty_list ;opatsc] in

  (* Match instantiate types *)
  let annotmap, match_types =
    let annotmap, ko_expr =
      #<If>
        let annotmap, es =
          OpaRPC.error_stub ~side annotmap stdlib_gamma in
        let annotmap, msg =
          TypedExpr.string annotmap "instantiate" in
        TypedExpr.apply gamma annotmap es [msg]
      #<Else>
        TypedExpr.none annotmap gamma
      #<End>
    in
    let annotmap, any = TypedPat.any annotmap in
    TypedExpr.match_ annotmap call_ins [(pat_arrow, match_values); (any, ko_expr)]
  in
  (* Call to extract_types *)
  let annotmap, call_ext_ty =
    let annotmap, ext = OpaRPC.extract_types ~side annotmap stdlib_gamma in
    let annotmap, expr_req = TypedExpr.shallow_copy annotmap expr_req in
    TypedExpr.apply gamma annotmap ext [expr_req] in

  (* Match extracted types *)
  let annotmap, match_types =
    let annotmap, ko_expr =
      #<If>
        let annotmap, es =
          OpaRPC.error_stub ~side annotmap stdlib_gamma in
        let annotmap, msg =
          TypedExpr.string annotmap "match_types" in
        TypedExpr.apply gamma annotmap es [msg]
      #<Else>
        TypedExpr.none annotmap gamma
      #<End> in
    let annotmap, extracted_types = TypedExpr.ident annotmap ident_extracted_types type_ident_extracted_types in
    let annotmap, match_ =
      generate_match_record_of_list
        ~annotmap extracted_types [
          "types", ident_ty_list, !OpaType.ty;
          "rows", ident_row_list, !OpaType.row;
          "cols", ident_col_list, !OpaType.col;
        ] match_types ko_expr in
    TypedExpr.letin annotmap [ident_extracted_types, call_ext_ty] match_
  in

  (* Call to unserialize and match it *)
  let annotmap, call_un =
    let annotmap, un = OpaRPC.unserialize ~side annotmap stdlib_gamma in
    TypedExpr.apply gamma annotmap un [expr_arg] in
  let annotmap, match_un =
    let annotmap, ko_expr =
      #<If>
        let annotmap, es =
          OpaRPC.error_stub ~side annotmap stdlib_gamma in
        let annotmap, msg =
          TypedExpr.string annotmap "unserialize" in
        TypedExpr.apply gamma annotmap es [msg]
      #<Else>
        TypedExpr.none annotmap gamma
      #<End>
    in
    generate_match_some ~annotmap ~gamma call_un ident_req ty_req
      match_types ko_expr in

  (* Make the lambda *)
  let annotmap, fun_skeleton =
    TypedExpr.lambda annotmap [ident_arg, ty_arg] match_un in

  (* Make an ident for skeleton *)
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr fun_skeleton) annotmap in
  let id = Ident.refresh ~map:(fun s -> "skeleton_"^s) ident in
  let annotmap, iskeleton =
    TypedExpr.ident annotmap id ty in
  annotmap, update_gamma gamma id ty, (iskeleton, fun_skeleton)

(** Register a skeleton as an rpc.
    @param iskeleton The skeleton to be register should be identified
    by this parameter.
    @param oexpr Expression that was used for generate skeleton.
    @param side Side where the skeleton should be register.*)
let register_skeleton ~renamingmap ~annotmap ~stdlib_gamma ~gamma ~side oexpr iskeleton =
  match oexpr with
  | Q.Ident (_, oident) ->
      (* Register skeleton *)
      let annotmap, reg =
        OpaRPC.Dispatcher.register ~side annotmap stdlib_gamma in
      let annotmap, str =
        TypedExpr.tagged_string annotmap (ident_to_registering renamingmap oident) Q.Rpc_def in
      let annotmap, call =
        TypedExpr.apply gamma annotmap reg [str; iskeleton] in
      let ident = Ident.refresh ~map:(fun s -> "register_"^s) oident in
      let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr call) annotmap in
      annotmap, update_gamma gamma ident ty , (ident, call)
  | _ -> assert false (* Inexpected expr *)


(* *******)
(* STUBS *)
(** Generate a stub from an expression. This function is used
    for resolve [@ajax_call, @comet_call].
    @param expr Expression to make a skeleton. This expression must be an
    ident else raise Invalid_argument exception.
    @side Side where the stub will be used.
    @return A stub for given [expr].

    If call is synchronous, generate a stub along the lines of:
      v1, v2, .., r1, r2, ..., c1, c2, ... ->
        fun a1 -> fun a2 -> fun a3 ->
          match OpaTsc.implementation [i1; i2; ...] tsc /*type scheme of the function*/ with
          | { TyArrow_params = [t1; t2; t3; ...]; TyArrow_res = tres } } ->
            send_to_<side>
              "<function_id>"
              (OpaRPC.serialize
                (OpaRPC.add_args_with_type t3 a3
                (OpaRPC.add_args_with_type t2 a2
                (OpaRPC.add_args_with_type t1 a1
                  (OpaRPC.add_var_types i2
                  (OpaRPC.add_var_types i1 OpaRPC.empty_request))))))
            tres
          | _ -> /* Make an error */

    If call is asynchronous, generate a stub along the lines of:
      fun i1 -> fun i2 -> ... ->
        fun a1 -> fun a2 -> fun a3 ->
          match OpaTsc.implementation [i1; i2; ...] tsc /*type scheme of the function*/ with
          | { TyArrow_params = [t1; t2; t3; ...]; TyArrow_res = _ } } ->
            async_send_to_<side>
              "<function_id>"
              (OpaRPC.serialize
                (OpaRPC.add_args t3 a3
                (OpaRPC.add_args t2 a2
                (OpaRPC.add_args t1 a1
                  (OpaRPC.add_var_types i2
                  (OpaRPC.add_var_types i1 OpaRPC.empty_request))))))
          | _ -> /* Make an error */

    Generate stub make :
    1 - Instantiate type scheme
    2 - Let ret as the expected type of value returned
     (Used for check type of returned value)
    3 - Add instantiate type variable of the function to an empty
     request. (Type given at runtime, like exp-inst).
    4 - Add argument to request
    5 - Send serialized request, and check returned value with ret.
*)
let generate_stub explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr =
  (* Check and get *)
  let _expr, ident, _ty, _tsc, nb_tyvar, nb_rowvar, nb_colvar, oty, otsc, _number_of_lambdas =
    check_and_get ~msg:"generate_stub" ~annotmap ~gamma explicit_map expr in

  #<If>
    if String.is_contained (Option.get DebugVariables.rpc_debug)
      (Ident.original_name ident)
    then
      verbose
        "%a = %a"
        QmlPrint.pp#ident ident
        QmlPrint.pp#ty _ty
    else ()
  #<Else>()
  #<End>;

  (* Create type args *)
  let annotmap, gamma, ident_ty_list, expr_ty_list =
    generate_vars ~annotmap ~gamma "v" !OpaType.ty nb_tyvar in
  let annotmap, gamma, ident_row_list, expr_row_list =
    generate_vars ~annotmap ~gamma "r" !OpaType.row nb_rowvar in
  let annotmap, gamma, ident_col_list, expr_col_list =
    generate_vars ~annotmap ~gamma "c" !OpaType.col nb_colvar in

  (* Get type of standard args *)
  let is_lambda, typeof_args, typeof_res =
    let rec aux = function
      | Q.TypeArrow (args, res) -> true, args, res
      | Q.TypeName (list, tyident) ->
          aux (QmlTypesUtils.Inspect.find_and_specialize gamma tyident list)
      | r -> false, [], r in
    aux oty
  in
  (* Create standard args [a*] and variable for pattern arrow [t*] *)
  let annotmap, gamma,
    id_ty_std_list, expr_std_list,
    id_ins_list, ex_ins_list =
    let rec aux i annotmap gamma = function
      | [] -> annotmap, gamma, [], [], [], []
      | ty::tys ->
          let annotmap, gamma, id_l, ex_l, id_l2, ex_l2 =
            aux (i+1) annotmap gamma tys in
          let annotmap, gamma, id, _, ex =
            generate_var ~annotmap ~gamma (Printf.sprintf "a%d" i) ty in
          let annotmap, gamma, id2, _, ex2 =
            generate_var ~annotmap ~gamma
              (Printf.sprintf "t%d" i) !OpaType.ty
          in
          annotmap, gamma,
          (id,ty)::id_l, ex::ex_l, id2::id_l2, ex2::ex_l2 in
    aux 0 annotmap gamma typeof_args
  in

  (* Some variables of generated stub *)
  let annotmap, gamma, id_tres, ty_tres, ex_tres =
    generate_var ~annotmap ~gamma "tres" !OpaType.ty in

  (* Type scheme in opa *)
  let annotmap, opatsc =
    Pass_ExplicitInstantiation.tsc_to_opatsc ~val_:OpaMapToIdent.val_ ~side (annotmap, gamma) otsc in

  (* Pattern arrow *)
  let annotmap, pat_arrow =
    let annotmap, pat_res =
      TypedPat.var annotmap id_tres ty_tres in
    if not is_lambda then annotmap, pat_res
    else
      let annotmap, pat_list =
        generate_pat_list ~annotmap id_ins_list !OpaType.ty in
      let annotmap, pat =
        TypedPat.record annotmap [
          "TyArrow_params", pat_list ;
          "TyArrow_res", pat_res ;
        ] in
      annotmap, pat
  in

  (* Create request *)
  let annotmap, req =
    let annotmap, req = OpaRPC.empty_request ~side annotmap stdlib_gamma in
    let annotmap, req =
      let add_variables annotmap req ident variables =
        List.fold_left
          (fun (annotmap, req) t ->
             let annotmap, add_ty = ident ~side ?posexpr:None annotmap stdlib_gamma in
             TypedExpr.apply gamma annotmap add_ty [t;req])
          (annotmap, req) variables in
      (* Insert type var *)
      let annotmap, req = add_variables annotmap req OpaRPC.add_var_types expr_ty_list in
      let annotmap, req = add_variables annotmap req OpaRPC.add_row_types expr_row_list in
      let annotmap, req = add_variables annotmap req OpaRPC.add_col_types expr_col_list in
      annotmap, req in
    (* Insert std args *)
    List.fold_left2
      (fun (annotmap, req) t a ->
         let annotmap, add_args_with_type = OpaRPC.add_args_with_type ~side annotmap stdlib_gamma in
         TypedExpr.apply gamma annotmap add_args_with_type [t; a; req])
      (annotmap, req) ex_ins_list expr_std_list in

  (* String identifier of function *)
  let annotmap, f_id =
    TypedExpr.tagged_string annotmap (ident_to_registering renamingmap ident) Q.Rpc_use in

  (* Match send_to *)
  let annotmap, send_to =
    let annotmap, send =
      OpaRPC.send_to_other_side ~side ~sync annotmap stdlib_gamma in
    TypedExpr.apply gamma annotmap send [f_id; req; ex_tres]
  in

  (* Match instantiate *)
  let annotmap, call_ins =
    let annotmap, ins = OpaType.tsc_implementation ~side annotmap stdlib_gamma in
    let copy_list annotmap expr_ty_list =
      let annotmap, expr_ty_list =
        TypedExpr.shallow_copys annotmap expr_ty_list in
      TypedExpr.list (annotmap, gamma) expr_ty_list in
    let annotmap, ex_list_ty = copy_list annotmap expr_ty_list in
    let annotmap, ex_list_row = copy_list annotmap expr_row_list in
    let annotmap, ex_list_col = copy_list annotmap expr_col_list in
    let annotmap, instantiation = TypedExpr.record annotmap [
      "types", ex_list_ty;
      "rows", ex_list_row;
      "cols", ex_list_col;
    ] in
    TypedExpr.apply gamma annotmap ins [instantiation; opatsc]
  in
  let annotmap, match_instantiate =
    let annotmap, err_stub = OpaRPC.error_stub ~side annotmap stdlib_gamma in
    let annotmap, f_id = TypedExpr.shallow_copy annotmap f_id in
    let annotmap, ko_expr = TypedExpr.apply gamma annotmap err_stub [f_id] in
    let annotmap, any = TypedPat.any annotmap in
    TypedExpr.match_ty annotmap call_ins [(pat_arrow, send_to); (any, ko_expr)] typeof_res
  in

  (* Create lambda *)
  (*
    <!> In case ei has introduce some extra arguments, we must currently generate 2 lambdas.
    but if no argument are added, we should not create an empty lambda.
    That what the function may_lambda does.

    <!> Quentin (Fri Sep 10 17:07:29 CEST 2010) what about the first lambda ?
  *)
  let annotmap, stub =
    if not is_lambda then
      TypedExpr.may_lambda annotmap
        (List.map (fun id -> id, !OpaType.ty) ident_ty_list @
         List.map (fun id -> id, !OpaType.row) ident_row_list @
         List.map (fun id -> id, !OpaType.col) ident_col_list) match_instantiate
    else
      let annotmap, lambda =
        TypedExpr.lambda annotmap
          (List.map (fun id -> id, !OpaType.ty) ident_ty_list @
             List.map (fun id -> id, !OpaType.row) ident_row_list @
             List.map (fun id -> id, !OpaType.col) ident_col_list @
             id_ty_std_list)
          match_instantiate in
      TypedExpr.directive annotmap (`lifted_lambda (nb_tyvar + nb_colvar + nb_rowvar, None)) [lambda] [] in

  (* Expand if necessary to have a lambda with 0 args *)
  let expand = (not is_lambda) && nb_tyvar = 0 && nb_colvar = 0 && nb_rowvar = 0 in
  let annotmap, stub =
    if expand then
      (* Cache checking for non-functional stubs *)
      let annotmap, stub = TypedExpr.lambda annotmap [] stub in
      let annotmap, try_cache = OpaRPC.try_cache ~side annotmap stdlib_gamma in
      let annotmap, try_cache =
        TypedExpr.apply gamma annotmap try_cache [f_id; stub] in
      TypedExpr.lambda annotmap [] try_cache
    else annotmap, stub in
  annotmap, gamma, stub, expand

(** Create a fresh ident for a stub. *)
let make_stub_ident ~annotmap ~gamma ident stub =
  let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr stub) annotmap in
  let id = Ident.refresh ~map:(fun s -> "stub_"^s) ident in
  id, ty, update_gamma gamma id ty, id



(* ******************************************************************)
(* AST transformations **********************************************)
(* ******************************************************************)

(** A generic walker on code for resolving slicer directives :

    - When it meet a [publish_directive] (on top level), it discards
    this directive and calls the [publish_resolver]. This
    [publish_resolver] should be generate couples of (ident,
    expressions). This generated ident, expressions are inserted on
    top level by this walker.

    - When it meet a [call_directive], it replace this directive by
    expression returned by [call_resolver]. [call_resolver] should be
    return also couples of (ident, expressions). This generated ident,
    expressions are inserted on top level by this walker.
*)
let resolving_slicer_directive ~annotmap ~stdlib_gamma ~gamma ~side code
    publish_resolver generate_stub_from_publish call_resolver insert_resolver =

  (* Perform on (ident * expr) list*)
  let vlist_walker =
    List.fold_left
      (fun ((annotmap, gamma, nvals, other_nvals, vlist) as acc) (id, expr) ->
         match expr with
         | Q.Directive (_, (#publish_directive as d), [expr], _) ->
             assert(side = directive_to_side d);
             let sync = publish_directive_to_sync d in
             let annotmap, gamma, nrvals =
               publish_resolver ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr in
             let annotmap, gamma, other_nrvals =
               if ObjectFiles.Arg.is_fully_separated () then
                 generate_stub_from_publish ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr
               else
                 annotmap, gamma, [] in
             (annotmap, gamma, nrvals@nvals, other_nrvals@other_nvals, vlist)
         | Q.Directive (_, #publish_directive, _, _) ->
             (* Make a condition for that *)
             QmlError.i_serror None (QmlError.Context.annoted_expr annotmap expr)
               "Malformed directive";
             acc

         | Q.Directive (label, (#insert_directive as d), _, _) ->
             assert(side = directive_to_side d);
             let ty = QmlAnnotMap.find_ty (Annot.annot label) annotmap in
             let annotmap, expr =
               insert_resolver ~annotmap ~stdlib_gamma ~gamma ~side d id ty in
             annotmap, gamma,  nvals, other_nvals, (id, expr)::vlist

         (*| Directive(#call_directive, _, _) ->
             (* Make a condition for that *)
             QmlError.i_serror None (QmlError.Context.annoted_expr annotmap expr)
               "Call directive at toplevel: %a" QmlPrint.pp#expr expr;
             (annotmap, gamma, nvals, vlist)*)

         | _ ->
             let walker (annotmap, gamma, nvals) expr =
               match expr with (*TODO: If [async], don't use [call_resolver]*)
               | Q.Directive (_, (#call_directive as d), [expr], _) ->
                   assert(side = directive_to_side d);
                   let annotmap, gamma, nrvals, expr =
                     call_resolver ~annotmap ~stdlib_gamma ~gamma ~side ~sync:(call_directive_to_sync d) expr in
                   (annotmap, gamma, nrvals@nvals), expr
               | Q.Directive (_, (#call_directive), _, _) ->
                   (* Make a condition for that *)
                   QmlError.i_serror None (QmlError.Context.annoted_expr annotmap expr)
                     "Malformed directive";
                     (annotmap, gamma, nvals), expr
               | _ -> (annotmap, gamma, nvals), expr
             in
             let (annotmap, gamma, nvals), expr =
               QmlAstWalk.Expr.foldmap_down
                 walker (annotmap, gamma, nvals) expr
             in annotmap, gamma, nvals, other_nvals, ((id, expr)::vlist)
      )
  in

  (* Walk on code *)
  let annotmap, gamma, nvals, other_nvals, code =
    List.fold_left
      (fun (annotmap, gamma, nvals, other_nvals, toplvl) -> function
         | Q.NewValRec (label, vlist) ->
             let annotmap, gamma, nvals, other_nvals, vlist =
               vlist_walker (annotmap, gamma, nvals, other_nvals, []) (List.rev vlist)
             in annotmap, gamma, nvals, other_nvals, (Q.NewValRec (label, vlist)::toplvl)

         | Q.NewVal (label, vlist) ->
             let annotmap, gamma, nvals, other_nvals, vlist =
               vlist_walker (annotmap, gamma, nvals, other_nvals, []) (List.rev vlist)
             in annotmap, gamma, nvals, other_nvals, (Q.NewVal (label, vlist)::toplvl)
         | elt -> annotmap, gamma, nvals, other_nvals, (elt::toplvl)
      ) (annotmap, gamma, [], [], []) code
  in
  let label () = Annot.nolabel "resolving_slicer_directive" in
  annotmap, gamma, List.rev (Q.NewVal (label (), nvals)::code), [Q.NewVal (label (), other_nvals)]

(** Call resolver : [call_resolver]. This call resolver can be work on
    both side if it's reseted by [reset_call_resolver] beetwen two
    walk.  This resolver generate a stub if necessary for given expr.
    You can fold the generated stubs with [fold_stubs folder acc]. Folder
    must be a function like that [folder original_ident stub_ident acc].
*)
let generate_stub_from_publish,
    call_resolver, (* Main resolver *)
  clear_stubs, add_previous_stub, (* Initialize resolver *)
  fold_current_stubs, get_current_stubs (* Get resolver environnment *) =
  (* Stubs generated on the current package *)
  let current_stubs = Hashtbl.create 1024 in
  (* Stubs generated on previous package *)
  let prev_stubs = Hashtbl.create 1024 in
  (* Some functions for manipulates both hastbl *)
  let stub_mem x = Hashtbl.mem current_stubs x || Hashtbl.mem prev_stubs x in
  let stub_find x =
    try Hashtbl.find current_stubs x with Not_found ->
      Hashtbl.find prev_stubs x in
  let stub_add k x = Hashtbl.add current_stubs k x in
  let generate_stub_if_needed explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side ~sync =
    function
    | Q.Ident (_, ident) as expr ->
        if stub_mem ident then
          annotmap, gamma, []
        else (
          _stub ();
          let annotmap, gamma, stub, expanded =
            generate_stub explicit_map renamingmap
              ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr in
          let sident, sty, gamma, id =
            make_stub_ident ~annotmap ~gamma ident stub in
          stub_add ident (sident, sty, expanded);
          annotmap, gamma, [(id, stub)]
        )
    | _ -> assert false in
  let generate_stub_from_publish explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side ~sync =
    generate_stub_if_needed explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side:(OpaMapToIdent.other_side side) ~sync in
  generate_stub_from_publish,
  (* call_resolver*)
  (fun explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side ~sync ->
     function
       | Q.Ident (_, ident) as expr ->
           (*add_rpc_use_declaration side (ident_to_registering renamingmap ident);*)
           _call ();
           let annotmap, gamma, newvals =
             generate_stub_if_needed explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr in
           let sident, sty, expanded = stub_find ident in
           let annotmap, expr = TypedExpr.ident annotmap sident sty in
           let annotmap, expr =
             if expanded then TypedExpr.apply gamma annotmap expr []
             else annotmap, expr in
           annotmap, gamma, newvals, expr
       | _ -> assert false (* Inexpected expression *)
  ),
  (fun _ -> Hashtbl.clear current_stubs; Hashtbl.clear prev_stubs),
  (fun k x -> Hashtbl.add prev_stubs k x),
  (fun folder ->
     Hashtbl.fold
       (fun ident (sident, _, _) acc -> folder ident sident acc)
       current_stubs
  ),
  (fun _ -> current_stubs)

(** Publish resolver. This resolver can be work on both side. This
    resolver generate a skeleton and an expression for register this
    skeleton. *)
let make_publish_resolver genskel explicit_map renamingmap ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr =
  try
    _skel ();
    let annotmap, gamma, (iskeleton, skeleton) =
      genskel explicit_map ~annotmap ~stdlib_gamma ~gamma ~side ~sync expr in
    let annotmap, gamma, cpl_register =
      register_skeleton ~renamingmap ~annotmap ~stdlib_gamma ~gamma ~side expr iskeleton in
    (match iskeleton with
     | Q.Ident (_, iskeleton) ->
         annotmap, gamma, [(iskeleton, skeleton); cpl_register]
     | _ -> assert false)
  with Invalid_argument str ->
    (* TODOK1 - FIXME - This case should be remove... *)
    QmlError.i_error None (QmlError.Context.expr expr)
      "Error on skeleton generation (discard it) : %a@\n%s"
      QmlPrint.pp#expr expr str

(** Select an publish resolver from a level of optimization *)
let select_publish_resolver _n =
  make_publish_resolver generate_skeleton


(** Insert resolver. Produce a directive for qmljs. Its goal is to
    insert server value on client code. *)
let insert_resolver0_unoptimized ~annotmap ~stdlib_gamma ~gamma ~side (`insert_server_value ident) _id ty =
  assert(side = `client);
  _insert ();
  let add_missing_string_arg annotmap func args =
    let string_arg = Ident.next "expand_missing_string_arg" in
    let ty_string = Q.TypeConst Q.TyString in
    let annotmap, string_arg_expr = TypedExpr.ident annotmap string_arg ty_string in
    let annotmap, call = TypedExpr.apply gamma annotmap func (string_arg_expr::args) in
    let annotmap, lamb = TypedExpr.lambda annotmap [string_arg, ty_string] call in
    annotmap,lamb
  in
  (* Make client expr *)
  let annotmap, uns = OpaSerialize.unserialize_unsafe ~side annotmap stdlib_gamma in
  let annotmap, ty_expr_server =
    Pass_ExplicitInstantiation.ty_to_opaty ~val_:OpaMapToIdent.val_ ~side:`server annotmap gamma ty in
  let annotmap, ty_expr_client =
    Pass_ExplicitInstantiation.ty_to_opaty ~val_:OpaMapToIdent.val_ ~side:`client annotmap gamma ty in
  let annotmap, client_expr =
    (* TODO insert an explicit partial call here *)
    add_missing_string_arg annotmap uns [ty_expr_client]
  in

  (* Make server expr *)
  let annotmap, ser_for_js =
    OpaSerialize.serialize_for_js ~side:(OpaMapToIdent.other_side side) annotmap stdlib_gamma in
  let annotmap, serv_expr =
    let annotmap, expr = TypedExpr.ident annotmap ident ty in
    full_apply gamma annotmap ser_for_js [ty_expr_server] [expr]
    (*let annotmap, partial = TypedExpr.apply gamma annotmap ser_for_js [ty_expr_server] in
    TypedExpr.apply gamma annotmap partial [expr]*)
  in
  (* Make directive *)
  let annot = Annot.next () in
  let annotmap = QmlAnnotMap.add_ty annot (Q.TypeConst Q.TyString) annotmap in
  assert(check_hybrid_value annotmap client_expr serv_expr);
  let label = Annot.make_label annot (Q.Pos.expr client_expr) in
  let e = Q.Directive (label, `hybrid_value, [client_expr ; serv_expr], []) in
  annotmap, e

(** Second version for insert resolver. More optimized but not stable
    : back-end and implementation dependent (see opa2js.opa). *)
let insert_resolver1_optimized ~annotmap ~stdlib_gamma ~gamma ~side (`insert_server_value ident) id ty =
  assert(side = `client);
  _insert ();
  (* Make server expr *)
  let annotmap, ty_expr_server =
    Pass_ExplicitInstantiation.ty_to_opaty ~val_:OpaMapToIdent.val_ ~side:`server annotmap gamma ty in
  let annotmap, ser =
    Opa2Js.to_string ~side:(OpaMapToIdent.other_side side) annotmap stdlib_gamma in
  let annotmap, serv_expr =
    let annotmap, toplevel_var = TypedExpr.string annotmap (JsPrint.string_of_ident (JsAst.ExprIdent id)) in
    let annotmap, expr = TypedExpr.ident annotmap ident ty in
    TypedExpr.apply_partial gamma annotmap ser [ty_expr_server; toplevel_var; expr]
  in

  (* Make directive *)
  let annot = Annot.next () in
  let annotmap = QmlAnnotMap.add_ty annot (Q.TypeConst Q.TyString) annotmap in
  (* here creating an hybrid value without client expression
   * (instead of creating it with Magic.id)
   * it generates better code and doesn't break cleaning *)
  (* assert(check_hybrid_value annotmap dir); *)
  let label = Annot.make_label annot (Q.Pos.expr serv_expr) in
  let e = Q.Directive (label, `hybrid_value, [serv_expr], []) in
  annotmap, e

(** Select an insert resolver from a level of optimization *)
let rec select_insert_resolver = function
  | 0 -> insert_resolver0_unoptimized
  | 1 -> insert_resolver1_optimized
  | x when x < 0 ->
      warn_olvl "Optimization level can't be negative";
      select_insert_resolver 0
  | _ ->
      warn_olvl "Optimization level for insert is limited by 1";
      select_insert_resolver 1


(* ******************************************************************)
(* Pass *************************************************************)
(* ******************************************************************)

module S =
struct
  (* Original ident * stub ident, stub type, expanded stub *)
  type t = (QmlAst.ident, QmlAst.ident * QmlAst.ty * bool) Hashtbl.t

  let pass = "ResolveRemoteCalls"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = ObjectFiles.Make(S)

let error_stdlib _gamma annotmap code1 code2 e =
  let package_name = ObjectFiles.get_current_package_name () in
  let aux =
    QmlAstWalk.CodeExpr.iter
      (QmlAstWalk.Expr.iter
         (function
         | Q.Directive (_, #all_input_directives, _, _) as posexpr ->
           let context = QmlError.Context.annoted_expr annotmap posexpr in
           QmlError.serror context "Slicer directive inside %s" package_name
         | _ -> ()))
  in
  aux code1 ; aux code2 ;
  OManager.i_error (
    "InsertRemote raised %s@\n"^^
      "Possible explanation: OpaRPC is not yet defined in %s@\n"^^
      "  And yet you are defining code that need to be sliced@\n"^^
      "  (ie there is one of client only, server only,@\n"^^
      "   alias or insert_server_value in slicer.dump)@\n"^^
      "  If this is the case, some possible problems are:@\n"^^
      "  - you cannot use @@server nor @@client@\n"^^
      "    if you need the code to be client only, then it cannot be defined in this package@\n"^^
      "    if you need the code to be server only, then use @@server_private and publish it later if needed@\n"^^
      "  - you cannot define client only bypasses@\n"^^
      "  - you cannot do side effects at toplevel, except in a declaration that is @@server_private@\n"^^
      "    Note that the slicer cannot guess when bypasses do side effects when applied.@\n"^^
      "    You can update $OPAGENERAL/libqmlcompil/qmlEffects.ml so that the slicer knows when a bypass@\n"^^
      "    application can be computed safely and independently on both sides@."
  ) (Printexc.to_string e) package_name


let prelude_core  ~gamma =
  clear_stubs ();
  preprocess_types gamma;
  R.iter_with_name
    (fun pack previous_stubs ->
      Hashtbl.iter
        (fun k (s, t, e) ->
          let t = QmlRefresh.refresh_typevars_from_ty pack t in
          add_previous_stub k (s, t,e))
        previous_stubs)

let prelude ~gamma ~annotmap code1 code2 =
  try prelude_core  ~gamma
  with e when String.is_prefix "stdlib." (ObjectFiles.get_current_package_name ())
      -> error_stdlib gamma annotmap code1 code2 e

let postlude orig_renaming1 orig_renaming2 =
  (* Finalize pass *)
  (* Update renaming map *)
  let renaming1,renaming2 =
    fold_current_stubs
      (fun org stu (renaming1,renaming2) ->
         match QmlRenamingMap.original_from_new_opt orig_renaming2 org with
         | None ->
             (match QmlRenamingMap.original_from_new_opt orig_renaming1 org with
             | None ->
                 OManager.printf "error when updating renaming map on %a" QmlPrint.pp#ident org
                   (*
                     Mathieu Wed Mar  9 11:53:45 CET 2011
                     FIXME: This is a wild warning
                   *)
                 ;
                 QmlRenamingMap.add renaming1 org stu, QmlRenamingMap.add renaming2 org stu (* no idea what to do *)
             | Some org ->
                 renaming1, QmlRenamingMap.add renaming2 org stu)
         | Some org ->
             assert (Option.is_none (QmlRenamingMap.original_from_new_opt orig_renaming1 org));
             QmlRenamingMap.add renaming1 org stu, renaming2
      ) (orig_renaming1,orig_renaming2) in
  (* Save object files *)
  R.save (get_current_stubs ());
  renaming1, renaming2

(** Perform on an AST (code).
    @param expmap1 Explicit map of the computed side
    @param expmap2 Explicit map of the other side
    @param renaming1 The renaming map of the computed side
    @param renaming2 The renaming map of the other side
*)

let perform_on_code ?(options=default_options) side ~annotmap ~stdlib_gamma ~gamma
    expmap1 expmap2
    renaming1 renaming2
    code =
  #<If>
  debug_print
    (fun _ -> Printf.sprintf "BEGIN on %s code"
       (match side with | `server -> "server" | `client -> "client"));
  #<End>;


  (* Initialize pass *)
  _reset ();


  (* Transform code *)
  let annotmap, gamma, code, other_code =
    resolving_slicer_directive ~annotmap ~stdlib_gamma ~gamma ~side code
      ((select_publish_resolver options.optimize_publish) expmap1 renaming1)
      (generate_stub_from_publish expmap1 renaming1)
      (call_resolver expmap2 renaming2)
      (select_insert_resolver options.optimize_insert) in

  (* let annotmap, gamma, code = *)
  (*   #<If:RPC_ALT_SKELETON> *)
  (*     let (gamma,annotmap),new_code = skel_apply annotmap gamma in *)
  (*     annotmap, gamma, new_code @ code *)
  (*   #<Else> *)
  (*     annotmap, gamma, code *)
  (*   #<End> in *)

  #<If>
  (* Debug printing *)
  debug_info side;
  (* debug_code code; *)
  #<End>;

  annotmap, gamma, code, other_code

let need_to_process_code code1 code2 =
  let need_to_process_one_code code =
    QmlAstWalk.CodeExpr.exists
      (QmlAstWalk.Expr.exists
         (function
          | Q.Directive (_, #all_input_directives, _, _) -> true
          | _ -> false))
      code in
  let need = need_to_process_one_code code1 || need_to_process_one_code code2 in
  if not need then (
    R.save (Hashtbl.create 0); (* we need to create the object files, even if we have nothing to save *)
  );
  need

(* See mli *)
let perform_on_server_code ?options = perform_on_code ?options `server


(* See mli *)
let perform_on_client_code ?options = perform_on_code ?options `client

(* see mli *)
module S2 =
struct
  type t = Pass_ExplicitInstantiation.published_map * QmlAst.annotmap
  let pass = "opa_InsertRemote_explicit_map"
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module R2 =
struct
  include ObjectFiles.MakeClientServer(S2)
  let save ~side annotmap map =
    let small_annotmap =
      IdentMap.fold
        (fun _ o acc ->
           match o with
           | None -> acc
           | Some (label, _, _) ->
               let annot = Annot.annot label in
               let annot_content = QmlAnnotMap.find annot annotmap in
               QmlAnnotMap.add annot annot_content acc)
        map QmlAnnotMap.empty in
    save ~side (map, small_annotmap)

  let load ~side annotmap map =
    fold_with_name ~side
      (fun package (annotmap,map) (old_map,old_annotmap) ->
         let old_annotmap = QmlRefresh.refresh_annotmap package old_annotmap in
         let annotmap, old_map =
           IdentMap.fold_map
             (fun _ o annotmap ->
                match o with
                | None ->
                    annotmap, None
                | Some (label, ident, number_of_lambdas) ->
                    let annot = Annot.annot label in
                    let annot_content = QmlAnnotMap.find annot old_annotmap in
                    let annot = Annot.next () in
                    let pos = Annot.pos label in
                    let annotmap = QmlAnnotMap.add annot annot_content annotmap in
                    let label = Annot.make_label annot pos in
                    annotmap, Some (label, ident, number_of_lambdas))
             old_map annotmap in
         (annotmap, IdentMap.safe_merge map old_map))
      (annotmap,map)
end
