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

(* depends *)
module Array = Base.Array
module Format = Base.Format
module List = Base.List

(* shorthands *)
module B = BslTypes
module Q = QmlAst
module QC = QmlAstCons.UntypedExpr
module V = OpaTopValue
module P = OpaTopProperties

(* debug *)
let debug fmt =
  OManager.printf ("@{<cyan>[Eval]@}@ @[<2>"^^fmt^^"@]@.")

(* Error managment *)
let fail fmt = OManager.error ("@[<2>@{<bright>Eval@}:@\n"^^fmt^^"@]@\n")

(* template *)
(*
  let _ =
    #<If:OPATOP_EVAL $minlevel 1>
      debug "do some %s of level %d@\n" "debug" 1
    #<End>
  in
*)

(* printer *)
let pp =
  #<If:OPATOP_ANNOT>
    QmlPrint.pp_annotation
  #<Else>
    (QmlPrint.pp :> QmlPrint.base_printer_with_sugared_types)
  #<End>

type env = V.t IdentMap.t

type ('a, 'b) ignored_directive = [
| QmlAst.type_directive
| `async
| `atomic
| `fun_action of 'a
| `nonexpansive
| `spawn
| `tracker of 'b
| `unsafe_cast
| `may_cps
| `wait
]

let rec traverse_ignore expr =
  match expr with
  | Q.Directive (_, #ignored_directive, [expr], _)
  | Q.Coerce (_, expr, _) -> traverse_ignore expr
  | _ -> expr

let make_bypass skey = QC.bypass (BslKey.normalize skey)

(* global annotmaps *)
let valueOfAnnot = ref AnnotMap.empty
let getValueOfAnnot x = AnnotMap.find_opt x !valueOfAnnot
let setValueOfAnnot x t = valueOfAnnot := AnnotMap.add x t !valueOfAnnot; ()
let resetAnnot () = valueOfAnnot := AnnotMap.empty

let (!!) pat fmt =
  (* FIXME: get pos from expr *)
  OManager.printf "@[<2>@{<bright>RuntimeError@}:@\nFIXME: citation instead of AST printing@\n";
  OManager.printf "In the pattern %a@]@\n" pp#pat pat;
  OManager.error fmt

(*
  The function [match_pattern] returns an option of a new env, by adding the
  bindings introduced by the pattern

  If the match is unsuccessfull, the function returns [None]
*)
let rec match_pattern env pat value =
  match pat, value with
  | Q.PatCoerce (_, pat, _), _ -> match_pattern env pat value

  | Q.PatConst (_, pc), V.V_const (_, vc) ->
      if pc = vc (* Q.const_expr, Pervasives.compare *)
      then Some env
      else None

  | Q.PatVar (_, ident), _ -> Some (IdentMap.add ident value env)

  | Q.PatAny _, _ -> Some env

  | Q.PatAs (_, alias_pat, ident), _ -> (
      let new_env_opt = match_pattern env alias_pat value in
      match new_env_opt with
      | None ->
          (* The aliased pattern didn't match the value, so the ident alias
             is not bound. *)
          None
      | Some new_env ->
          (* The aliased pattern matched the value. May be this induced some
             bindings we must keep and we also must add a binding for the ident
             alias. *)
          Some (IdentMap.add ident value new_env)
      )
  | Q.PatRecord (_, [], rowvar), V.V_record (_, fields,_) ->
      if rowvar = `open_ || StringMap.is_empty fields
      then Some env
      else None

  | Q.PatRecord (_, pfields, rowvar), V.V_record (_, vfields, _) ->
      let rec check_fields env present = function
        | [] -> (
            match rowvar with
            | `open_ -> Some env

            (* The pattern matching is closed. We must check if we have matched all the fields *)
            | `closed -> (
                let surjective =
                  StringMap.fold (fun key _ bool -> bool && StringSet.mem key present) vfields true
                in
                if surjective then Some env else None
              )
          )

        | (field, p1) :: tl -> (
            match StringMap.find_opt field vfields with
            | None -> None
            | Some value -> (
                match match_pattern env p1 (Lazy.force value) with
                | None -> None
                | Some env -> check_fields env (StringSet.add field present) tl
              )
          )

      in check_fields env StringSet.empty pfields

  | _ -> None

let nopos = FilePos.nopos "OpaTopEval.eval"

let (!!) expr fmt =
  (* FIXME: get pos from expr *)
  OManager.printf "@[<2>@{<bright>RuntimeError@}:@\nFIXME: citation instead of AST printing@\n";
  OManager.printf "In the expression %a@]@\n" pp#expr expr;
  OManager.error fmt

let rec eval env expr =

  let _ =
    #<If:OPATOP_EXPR>
      OManager.printf "eval(expr): %a@." pp#expr expr
    #<End>
  in

  let main_expr = expr in
  let value =
    if P.noeval_get() then V.t_null ~pos:nopos ()
    else match expr with
    | Q.Const (label, e) -> V.V_const (Annot.pos label, e)

    | Q.Ident (_, id) -> (
        try
          (* must be mem by type checking, but typer may be off *)
          IdentMap.find id env
        with
        | Not_found -> !! expr "unbound value %S@\n" (Ident.to_string id)
      )

    | Q.LetIn (_, lets, in_) ->
        let seen = ref IdentSet.empty in
        let fold env (id, expr) =
          if IdentSet.mem id !seen
          then !! main_expr "Variable %S is bound several times in this let and@\n" (Ident.to_string id)
          else (
            seen := IdentSet.add id !seen ;
            let value = eval env expr in
            let env = IdentMap.add id value env in
            env
          )
        in
        let env = List.fold_left fold env lets in
        eval env in_

    | Q.LetRecIn (_, lets, in_) ->
        let env =
          let tmp = ref IdentMap.empty in
          let fold env (id, expr) =
            let value =
              let lambda =
                match traverse_ignore expr with
                | Q.Lambda _ as expr -> expr
                | _ -> !! main_expr "This kind of expression is not allowed as right-hand side of `let rec'@\n"
              in
              V.V_closure (Q.Pos.expr lambda, tmp, lambda)
            in
            setValueOfAnnot (Q.QAnnot.expr expr) value;
            IdentMap.add id value env
          in
          let env = List.fold_left fold env lets in
          tmp := env;
          env
        in
        eval env in_

    | Q.Lambda (label, _, _) -> V.V_closure (Annot.pos label, ref env, expr)

    (* Apply : Be carrefully with this lines, the main idea of this version of interpreter is here *)
    (* The magie is just here in case of specialisation of high functional mixity between qml & ocaml *)
    (* In a simplier version (for example : no functionnal qml value of type 'a -> 'b can be passed
       as an argument of type 'a of a bypass-function) the hack would be not necessary *)
    | Q.Apply (_, f, args) -> (
        let vf = eval env f in
        let fail_arity i j =
          fail "arity mismatch (expected %d, get %d), cannot apply %a in %a." i j
            OpaTopValue.pp vf  pp#expr main_expr in
        match vf with
        | V.V_closure (_, clot_env, Q.Lambda (_, ids, body)) ->
            (* classic_apply : apply beta-redex with closure *)
            let update_env1 clot_env id arg = IdentMap.add id (eval env arg) clot_env in
            let update_env clot_env ids args =
              try List.fold_left2 update_env1 clot_env ids args
              with Invalid_argument "List.fold_left2" -> fail_arity (List.length  ids) (List.length args);
            in
            eval (update_env (!clot_env) ids args) body

        (* The followings line are interresting, that allow partial application of bypass,
           and a shorter code for the initial binding builtins *)
        | V.V_bypass (_, targs , ret, oof) -> (
            let lenargsapp = List.length targs in
            let lenargsty = List.length args in
            if lenargsapp <> lenargsty then fail_arity lenargsapp lenargsty;
            let mls = List.map2 (fun ty qml -> V.Proj.ocaml_of_t ~eval ty (eval env qml)) targs args in
            let ml =
              match mls with
              | [] ->
                  let _ =
                    #<If:OPATOP_HOOK>
                      prerr_endline "eval: HOOK-03";
                    #<End>
                  in
                  (Obj.obj oof) ()
              | _ ->
                  let _ =
                    #<If:OPATOP_HOOK>
                      prerr_endline "eval: HOOK-04";
                    #<End>
                  in
                  List.fold_left (fun func arg -> (Obj.magic func) arg) (Obj.obj oof) mls
            in
            let _ =
              #<If:OPATOP_HOOK>
                prerr_endline "eval: HOOK-05";
              #<End>
            in
            V.Proj.t_of_ocaml ret (Obj.repr ml)
          )

        | _ ->
            !! expr "cannot apply %a on %a. This value is not a function@\n"
              V.pp vf (Format.pp_list "@ " pp#expr) args
      )

    | Q.Match (_, expr, pat_expr_list) ->
        let v_expr = eval env expr in
        let rec aux = function
          | [] ->
              !! main_expr "pattern match failure. the value is %a@\n" V.pp v_expr
          | (pat, expr) :: tl -> (
              match match_pattern env pat v_expr with
              | Some env ->
                  eval env expr
              | None -> aux tl
            )
        in aux pat_expr_list

    | Q.Record (label, fields) ->
        let fold fields (field, expr) =
          let value = eval env expr in
          let fields = StringMap.add field (Lazy.lazy_from_val value) fields in
          fields
        in
        let fields = List.fold_left fold StringMap.empty fields in
        V.V_record (Annot.pos label, fields, ref None)

    | Q.Dot (_, expr, field) -> (
        let v_expr = eval env expr in
        match v_expr with
        | V.V_record (_, fields, _) -> (
            let lazy_value =
              try
                StringMap.find field fields
              with
              | Not_found ->
                  !! expr "this record has no field %S\n(maybe the typer is off)@\n" field
            in
            Lazy.force lazy_value
          )
        | v ->
            !! main_expr "in dot field %S : expected a record@ but got %a@\n" field V.pp v
      )

    (* { field = expr } :: expr *)
    | Q.ExtendRecord (_, field, expr, record) -> (
        let v_expr = eval env expr in
        let v_record = eval env record in
        match v_record with
        | V.V_record (pos, fields, _) ->
            let value = Lazy.lazy_from_val v_expr in
            let fields = StringMap.add field value fields in
            (* FIXME: merge with the pos from the main_expr *)
            V.V_record (pos, fields, ref None)
        | _ ->
            !! main_expr "extend record { %s = %a } :: %a@ expected a record@\n"
              field V.pp v_expr V.pp v_record
      )

    (* FIXME: remove this directive, change the type of the Bypass node *)
    (* TODO: and get back the check of restriction in this case *)
    | Q.Directive (_, `restricted_bypass _, [Q.Bypass (_, key)], _)
    | Q.Bypass (_, key) -> (
        (* get the cached bypass map *)
        let bypass_map = OpaTopBsl.bypass_map () in
        match OpaTopBsl.find_opt key bypass_map with
        | Some bypass -> (
            match OpaTopBsl.eval bypass with
            | Some value -> value
            | None -> (
                !! expr "This external primitive is not available in opatop@\n"
              )
          )

        | None ->
            !! expr "Unknow external primitive. Maybe do you want to use a plugin ?@\n"
      )

    | Q.Path _ ->
        !! expr "Presence of a raw database reading node not resolved by DbGen@\n"

    | Q.Directive (_, `fail, message, _) -> (
        match message with
        | [] ->
            !! expr "@@fail@\n"

        | message :: _ ->
            let message =
              match eval env message with
              | V.V_const (_, Q.String message) -> message
              | v -> !! expr "@@fail expects one argument of type string but got: %a@\n" V.pp v
            in
            !! expr "@@fail: %s@\n" message
      )

    | Q.Directive (_, `assert_, [cond], _) -> (
        let void = V.Proj.t_void () in
        if not (P.assert_get ()) then void else
          let v_cond = eval env cond in
          match v_cond with
          | V.V_record (_, fields, _) ->
              (* Keep in sync with qml semantic *)
              if (StringMap.mem "true" fields) && not (StringMap.mem "false" fields) then void
              else !! expr "assert failure"
          | _ ->
              !! expr "assert condition not a bool value: %a@\n"
                V.pp v_cond
      )

    | Q.Directive (_, `create_lazy_record, exprs, _) -> (
        let expr, o = QmlDirectives.create_lazy_record_arguments exprs in
        match expr with
        | Q.Record (_, fields) ->
            let embed_data = Option.map (eval env) o in
            let fold fields (field, expr) =
              let lazy_value = lazy (eval env expr) in
              StringMap.add field lazy_value fields in
            let fields = List.fold_left fold StringMap.empty fields in
            V.V_record (nopos, fields, ref embed_data)
        | _ -> assert false
      )

    | Q.Directive (_, `callcc, [expr], _) ->
        let fake_bypass = make_bypass "bslcps.notcps_compatibility.callcc_directive" in
        let expr = QC.apply fake_bypass [expr] in
        eval env expr

    | Q.Directive (label, `llarray, exprs, _) ->
        let len = ref 0 in
        let rev_exprs = List.rev_map (fun e -> incr(len); eval env e) exprs in
        let array = Array.unsafe_create !len in
        let pred_len = pred !len in
        let iteri v i = let i = pred_len - i in array.(i) <- v in
        List.iteri iteri rev_exprs ;
        let string = "llarray" in
        let args = [ B.TypeVar (Annot.pos label, B.TypeVar.next()) ] in
        OpaTopValue.Proj.t_extern string args array

    (* ignored nodes *)
    | Q.Directive (_, #ignored_directive, [expr], _)
    | Q.Coerce (_, expr, _) -> eval env expr

    | Q.Directive (_, d, e, t) ->
        !! expr "Directive %a is not available in qmltop"
          (fun fmt () -> pp#directive fmt d e t) ()
  in

  let annot = Q.QAnnot.expr main_expr in

  let _ =
    #<If:OPATOP_EXPR>
      OManager.printf "value: (%a : § %d)@." OpaTopValue.pp value (Annot.to_int annot)
    #<End>
  in

  (* Store the value in the map *)
  setValueOfAnnot annot value;
  value
