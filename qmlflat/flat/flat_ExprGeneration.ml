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
module List = BaseList

(* refactoring in progress *)

(* alias *)
module FCons = Flat_Common.FCons
module ServerLib = Flat_Common.ServerLib
module Opabsl = Flat_Opabsl
module QCons = QmlAstCons.UntypedExpr

(* shorthands *)
module Q = QmlAst
module E = Flat_Env
module P = Qml2ocamlOptions

(* -- *)

(*
  Directive that we assume to traverse without doing anything.
  + type directives
  + most concurrency directives can be ignored if they are still there at this point.
*)
type ('a, 'b, 'c, 'd, 'e) assume_traverse = [
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
| `partial_apply of 'e | `full_apply of 'c | `lifted_lambda of 'd
]

(*
  Structural void only.
  Applications should be computed.
*)
let is_void _env expr =
  let rec aux expr =
    match expr with
    | Q.Record (_, []) -> true
    | Q.Record _ -> false
    | Q.Coerce (_, expr, _) -> aux expr
    | _ -> false
  in
  aux expr

let env_expr_error = Flat_Env.internal_error

let expr env expr =
  let rec aux expr =
    match expr with
    | Q.Const (_, const) ->
        Ocaml.Const (FCons.const const)

    | Q.Ident (_, ident) ->
        FCons.var ident

    | Q.LetIn (_, bindings, expr) ->
        let bindings = List.map (fun (id, expr) -> FCons.param id, aux expr) bindings in
        Ocaml.Letin (bindings, aux expr)

    | Q.LetRecIn (_, bindings, expr) ->
        let bindings = List.map (fun (id, expr) -> FCons.param id, aux expr) bindings in
        Ocaml.Letrecin (bindings, aux expr)

    | Q.Lambda (_, vs, e) -> (
        match vs with
        | [] ->
            let x = Ident.next "empty_lambda" in
            let x = FCons.param x in
            Ocaml.Abs ([ x ], aux e)
        | _ ->
            Ocaml.Abs (List.map FCons.param vs, aux e)
      )

    | Q.Apply (_, f, args) -> (
        let f = aux f in
        match args with
        | [] ->
            Ocaml.App (f, Ocaml.Cons.unit)
        | _ ->
            List.fold_left (fun apply arg -> Ocaml.App (apply, aux arg)) f args
      )

    | Q.Match (_, e, pat_expr) -> (
        let e = aux e in
        let pat_expr = List.map (fun (p, e) -> p, aux e) pat_expr in
        Flat_MatchGeneration.compile e pat_expr
      )

    (* empty record *)
    | Q.Record (_, []) ->
        ServerLib.empty

    (* may be simple record *)
    | Q.Record (_, [ label, expr ]) ->
        if is_void env expr
        then
          Flat_Shared.simple label
        else
          Flat_RecordGeneration.may_be_simple ~info:None label (aux expr)

    (* other cases: static init *)
    | Q.Record (_, fields) ->
        let fields = List.map (fun (f, e) -> f, aux e) fields in
        Flat_RecordGeneration.static_init ~is_lazy:false ~info:None fields

    (* Lazy record *)
    | Q.Directive (_, `create_lazy_record, arguments, _) -> (
        let e, qml_info = QmlDirectives.create_lazy_record_arguments arguments in
        match e with
        | Q.Record (_, fields) -> (
            match fields with
            | [] ->
                ServerLib.empty
            | [ label, expr ] ->
                if is_void env expr
                then
                  Flat_Shared.simple label
                else
                  Flat_RecordGeneration.may_be_simple ~info:None label (aux expr)
            | _ ->
                let info = Option.map aux qml_info in
                let fields = List.map (fun (f, e) -> f, aux e) fields in
                Flat_RecordGeneration.static_init ~is_lazy:true ~info fields
          )
        | _ -> assert false
      )

    | Q.Dot (_, expr, label) ->
        let context = QmlError.Context.expr expr in
        let ty_expr =
          QmlAnnotMap.find_ty_opt (Q.QAnnot.expr expr) env.E.typing.QmlTypes.annotmap
        in
        let expr = aux expr in
        Flat_DotGeneration.compile ~env ~context ~ty_expr expr label

    | Q.ExtendRecord _ ->
        (* Collect record extensions *)
        let rec collect acc rest =
          match rest with
          | Q.ExtendRecord (_, label, expr, rest) -> (
              let expr = Ocaml.make_repr (aux expr) in
              let acc = (label, expr) :: acc in
              collect acc rest
            )
          | Q.Coerce (_, rest, _) ->
              collect acc rest
          | _ ->
              let rest = aux rest in
              List.rev acc, rest
        in
        let extend, record = collect [] expr in
        Flat_RecordGeneration.extend record extend

    | Q.Bypass (_, bslkey) ->
        let context = Flat_Env.env_context env (QmlError.Context.expr expr) in
        let bymap = env.E.bymap in
        Flat_BypassGeneration.compile ~context ~bymap bslkey

    (* FIXME: merge with Bypass of bslkey * restriction option *)
    | Q.Directive (_, `restricted_bypass restriction, [Q.Bypass (_, bslkey)], _) ->
        let context = Flat_Env.env_context env (QmlError.Context.expr expr) in
        let bymap = env.E.bymap in
        Flat_BypassGeneration.compile ~context ~bymap ~restriction bslkey

    | Q.Coerce (_, expr, _) ->
        aux expr

    | Q.Directive (_, `partial_apply (Some 0, _), [e], _) when not env.E.options.P.cps && not env.E.options.P.qml_closure ->
        (* i think that the directive @partial_apply (Some 0) may stay in the code
         * even when closures are activated, but in that case, they have been taken
         * care of already
         * in cps, everybody receives a continuation so no lambda with 0 argument
         * (at least when doing partial application) *)
        Ocaml.Abs ([FCons.param (Ident.next "empty_lambda")], aux e)

    | Q.Path _ ->
        env_expr_error env expr
          "Internal error: At this stage, all first-class paths should have been compiled."

    | Q.Directive (_, `unsafe_cast, [e], _) -> aux e

    | Q.Directive (_, #assume_traverse, [e], _) ->
        aux e

    | Q.Directive (_, `assert_, [_], _) ->
        (* assert directive should have been resolved sooner *)
        aux (QCons.unit ())

    | Q.Directive (_, `fail, message, _) ->
        (* fail is always keeped, though no_assert *)
        let message =
          match message with
          | [] ->
              Ocaml.Cons.string ""
          | message :: _ ->
              aux message
        in
        let message_param, message_var = FCons.param_var (Ident.next "message") in
        let location = QCons.string (Flat_Env.string_of_pos (Q.Pos.expr expr)) in
        Ocaml.Letin (
          [message_param, message],
          Ocaml.Cons.app3 (aux Flat_Opabsl.Bslpervasives.fail) message_var (aux location)
        )

    | Q.Directive (_, `callcc, [e], _) ->
        let callcc = Flat_Opabsl.Bslcps.Notcps_compatibility.callcc_directive in
        let e = QCons.apply callcc [e] in
        aux e

    | Q.Directive (_, `thread_context, [], _) ->
        (* this could be optimized with QCons.none () but this would be to fragile *)
        let thread_context = Flat_Opabsl.Bslcps.Notcps_compatibility.thread_context in
        let e = QCons.apply thread_context [ QCons.unit () ] in
        aux e

    | Q.Directive (_, `with_thread_context, arguments, _) ->
        let thread_context, alpha =
          match arguments with
          | [ fst ; snd ] -> fst, snd
          | _ ->
              env_expr_error env expr
                "Internal error: @@with_thread_context should have 2 arguments"
        in
        (* this could be optimized with (aux alpha) but this would be to fragile *)
        let with_thread_context = Flat_Opabsl.Bslcps.Notcps_compatibility.with_thread_context in
        let e = QCons.apply with_thread_context [ thread_context ; alpha ] in
        aux e

    | Q.Directive (_, `backend_ident ident, _, _) ->
        (*
          backend idents refer to generated primitives by the CTrans.
          For each package, a bsl projection file is generated, and
          contains the definition of backend idents.
        *)
        let bsl_init = Qml2ocaml.bsl_init_module () in
        Ocaml.make_Varl [ bsl_init ; ident ]

    | Q.Directive (_, `llarray, exprs, _) ->
        (*
          We should produce an ocaml array.
          <!> Beware, the list exprs are huge, this should be tail rec.
        *)
        let exprs = List.tail_map aux exprs in
        Ocaml.AnArray exprs

    | Q.Directive (label, (`throw | `catch), _, _) when not env.E.options.Qml2ocamlOptions.cps ->
        aux (Q.Directive (label, `fail, [QCons.string "Call to throw or catch in non cps mode"], []))

    | Q.Directive (_, variant, exprs, tys) ->
        env_expr_error env expr
          "Internal error: At this stage, all directives %a should have been compiled"
          (fun fmt () -> QmlPrint.pp#directive fmt variant exprs tys) ()

  in
  aux expr
