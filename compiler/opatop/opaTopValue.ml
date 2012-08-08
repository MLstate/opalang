(*
    Copyright © 2011, 2012 MLstate

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
(* cf mli *)

(* libbase *)
module List = Base.List

(* Refactoring WIP *)
(* opa compiler *)
module Schema = QmlDbGen.Schema
(* TODO: is there not a default DbGen in the framework ? *)
module DbGen = QmlDbGen.DbGen(QmlDbGen.DbGenByPass.BSLDbGenAlpha)

(* shorthand *)
module Q = QmlAst

(* Error managment *)
let fail fmt = OManager.error ("@[<2>@{<bright>RuntimeError@}:@\n"^^fmt^^"@]@\n")

(* type alias *)
type pos = FilePos.pos
type 't pprinter = 't Base.Format.pprinter

(* Runtime Value Algebra *)
(*
    Although QmlAst fun are Nary, the interpreter can not do a generic apply

    We could also do something like :

    let apply1 =
    let apply2 =
    let apply3 = ...

    and decrete that there is a hard encoded limit...
    This code could be generated, but anyway performance are not the target of qmltop.

    To deal with function of arity 0, the first argument is an option.
    If the type is None, in the implementation the function expects unit
  *)
(*
  TODO: in V_closure change the type so that
  by construction, it must be a lambda node
  (todo that in Nary version, even in master
*)


type t =
  | V_const   of pos * QmlAst.const_expr
  | V_record  of pos * (t Lazy.t) StringMap.t * t option ref
  | V_closure of pos * (t IdentMap.t) ref * QmlAst.expr
  | V_extern  of pos * string * BslTypes.t list * Obj.t
  | V_bypass  of pos * BslTypes.t list * BslTypes.t * Obj.t

let pos = LangAst.pos
let reset_pos = LangAst.reset_pos
let merge_pos = LangAst.merge_pos

(* Printing *)

let pp_value ?ty ?(force=false) fmt value =
  match ty with
  | Some (Q.TypeVar _) -> Format.pp_print_string fmt "<poly>"
  | _ ->
      (* Pretty printing of list *)
      (* Return also Some _ for non-homogene list *)
      let rec to_list value =
        match value with
        | V_record(_, fds, _) ->
            let s = StringMap.size fds in
            if s = 2 then
              Option.bind (fun tl -> Option.map (fun hd -> hd::tl) (StringMap.find_opt "hd" fds))
                (Option.bind
                   (fun x -> if force || Lazy.lazy_is_val x then to_list (Lazy.force x) else None)
                   (StringMap.find_opt "tl" fds))
            else if s = 1 then
              if StringMap.mem "nil" fds then Some [] else None
            else None
        | _ -> None
      in
      let rec aux fmt value =
        let lazy_aux fmt value =
          if force || Lazy.lazy_is_val value
          then aux fmt (Lazy.force value)
          else Format.pp_print_string fmt "<lazy>" in
        match value with
        | V_const  (_, c) -> QmlAst.Const.pp_expr fmt c
        | V_record (_, fields, _) ->
            let size = StringMap.size fields in
            if size = 0 then
              Format.pp_print_string fmt "void"
            else (
              (* this may be a list *)
              match to_list value with
              | Some values ->
                  Format.fprintf fmt "[@ %a@ ]" (Base.Format.pp_list " ;@ " lazy_aux) values
              | None ->
                  (* TODO: use LangPrint.pp_fields *)
                  if size > 3
                  then
                    Format.fprintf fmt "{@\n%a@\n}"
                      (StringMap.pp " ;@\n" (LangPrint.pp_field  " =@ " lazy_aux)) fields
                  else
                    Format.fprintf fmt "{ %a }"
                      (StringMap.pp " ; " (LangPrint.pp_field  " = " lazy_aux)) fields
            )
        | V_closure (_, _, Q.Lambda _)  -> Format.pp_print_string fmt "<fun>"
        | V_closure _ ->
            Format.pp_print_string fmt "<clos>"
        | V_extern  (_, n, p, _) ->
            Format.fprintf fmt "<extern[%a]>" (LangPrint.pp_parameters BslTypes.pp n) p

        | V_bypass  (pos, a, b, _) ->
            Format.fprintf fmt "<bypass[%a]>" BslTypes.pp (BslTypes.Fun (pos, a, b))

      in aux fmt value

let pp fmt x = pp_value fmt x

let pp_type fmt v =
  match v with
  | V_const   (_, c)  -> QmlAst.Const.pp_ty fmt (QmlAst.Const.type_of c)
  | V_record  (_, fields, _) ->
      let pp_field fmt field _ = Format.pp_print_string fmt field in
      Format.fprintf fmt "{ %a }" (StringMap.pp " ; " pp_field) fields
  | V_closure _ -> Format.pp_print_string fmt "<fun>"
  | V_extern  _
  | V_bypass  _ -> pp fmt v

(* comparaison *)

let compare ?(strong=false) a b =
  let rec compare a b =
    match a, b with
    | V_const (_, a), V_const (_, b) ->
        let c = Pervasives.compare (QmlAst.Const.type_of a) (QmlAst.Const.type_of b) in
        if c = 0 && strong then Pervasives.compare a b else c
    | V_const _, _ -> -1
    | _, V_const _ -> 1
    | V_record (_, fields, _), V_record (_, fields', _) ->
        StringMap.compare (fun x y -> compare (Lazy.force x) (Lazy.force y)) fields fields'
    | V_record _, _ -> -1
    | _, V_record _ -> 1
    | V_closure _, V_closure _ -> -1 (* YAGNI *)
    | V_closure _, _ -> -1
    | _, V_closure _ -> 1
    | V_extern (_, a, tl, _), V_extern (_, b, ttl, _) ->
        let r = String.compare a b in
        if r <> 0 then r
        else
          let rec aux = function
            | [], [] -> 0
            | [], _::_ -> -1
            | _::_, [] -> 1
            | t::q, t2::q2 ->
                let r = BslTypes.compare ~normalize:(not strong) t t2 in
                if r <> 0 then r else aux (q, q2)
          in aux (tl, ttl)
    | V_extern _, _ -> -1
    | _, V_extern _ -> 1
    | V_bypass (_, a, b, _), V_bypass (_, c, d, _) ->
        if strong then
          let r = List.make_compare (BslTypes.compare ~normalize:(not strong)) a c in
          if r <> 0 then r
          else BslTypes.compare ~normalize:(not strong) b d
        else 1

  in compare a b

let nopos = FilePos.nopos "opatop:value"
let t_null ?(pos=nopos) () = V_extern (pos, "null", [], Obj.repr 0)

(* value env *)

type env = t IdentMap.t

(*
  Note for hackers :

  This part of the code provides transcription functions between ocaml and qmltop for bsl types
  !!!!! CRITICAL SECTION !!!!!
  be carrefully by hacking this code, there could be seg-faulting consequences

  If you are wondering why [ocaml_of_t] and [t_of_ocaml] are not mutually recursive,
  like in the compiled back-end's, you're a specialist :), congratulations.

  It is because we project function in a currified way, and dynamically. The rest of the projection
  is done argument by argument.
*)
module Proj =
struct
  module B = BslTypes

  let t_int ?(pos=nopos) i    = V_const  (pos, Q.Int (Big_int.big_int_of_int i))
  let t_float ?(pos=nopos) f  = V_const  (pos, Q.Float f)
  let t_string ?(pos=nopos) s = V_const  (pos, Q.String s)
  let t_void ?(pos=nopos) ()  = V_record (pos, StringMap.empty, ref None)
  let t_int64 ?(pos=nopos) i  = V_const  (pos, Q.Int (Big_int.big_int_of_int64 i))

  let shared_void = t_void ()
  let shared_lazy_void = Lazy.lazy_from_val shared_void
  let shared_simple field = StringMap.add field shared_lazy_void StringMap.empty
  let shared_true  = shared_simple "true"
  let shared_false = shared_simple "false"
  let shared_none  = shared_simple "none"

  let t_true ?(pos=nopos) ()  = V_record (pos, shared_true, ref None)
  let t_false ?(pos=nopos) () = V_record (pos, shared_false, ref None)

  let t_bool ?(pos=nopos) b = if b then t_true ~pos () else t_false ~pos ()

  let t_none ?(pos=nopos) () = V_record (pos, shared_none, ref None)
  let t_some ?(pos=nopos) t =
    V_record (pos, StringMap.add "some" (Lazy.lazy_from_val t) StringMap.empty, ref None)

  let t_option ?(pos=nopos) = function
    | None -> t_none ~pos ()
    | Some v -> t_some ~pos v

  let t_extern ?(pos=nopos) name params x = V_extern (pos, name, params, Obj.repr x)

  let rec t_of_ocaml ty x =
    match ty with
    | B.OpaValue _ ->
        (* A value manipluated by the ServerLib in the external primitive library *)
        Obj.magic x

    | B.Const (pos, Q.TyInt)    -> t_int ~pos (Obj.magic x)
    | B.Const (pos, Q.TyFloat)  -> t_float ~pos (Obj.magic x)
    | B.Const (pos, Q.TyString) -> t_string ~pos (Obj.magic x)
    | B.Const (pos, Q.TyNull)   -> t_null ~pos ()

    (* If a type is still an alpha, that means that it is a opa-value represented by itself *)
    | B.TypeVar _ -> Obj.magic x

    | B.Void pos -> t_void ~pos ()
    | B.Bool pos -> t_bool ~pos (Obj.magic x)


    | B.Option (pos, o)  -> (
        match Obj.magic x with
        | None -> t_none ~pos ()
        | Some ocaml -> t_some ~pos (t_of_ocaml o ocaml)
      )

    | B.External (pos, name, params) -> t_extern ~pos name params x

    | B.Fun (pos, u, v) ->
        (* x is a ocaml function *)
        (* we generate dynamically a new ocaml function *)
        (* Note: The projections will be done currified way, application by application *)
        V_bypass (pos, u, v, Obj.repr x)


  let rec ocaml_of_t ~eval:eval ty value =
    let rec aux ty value =
      (* FIXME: see what kind of citation are given *)
      let clash ?(extra="") () =
        fail "Type Citation:%aValue Citation:%aContext requires a value of type %a@\nbut got a value of type %a@\nThe value is %a%s"
          FilePos.pp_citation (BslTypes.pos ty)
          FilePos.pp_citation (pos value)
          BslTypes.pp ty
          pp_type value
          pp value
          extra
      in
      match ty, value with
      | B.TypeVar _, _ ->
          (* Note that in this case, we give direct the opa-value 'as is' *)
          (* there is no probleme, because the ocaml-function which will take it is polymorphic *)
          Obj.magic value

      | B.OpaValue _, _ ->
          (* Value manipulated through the ServerLib API *)
          Obj.magic value

      (* Consts *)
      | B.Const (_, Q.TyInt)     , V_const (_, Q.Int    i)   -> Obj.magic i
      | B.Const (_, Q.TyFloat)   , V_const (_, Q.Float  f)   -> Obj.magic f
      | B.Const (_, Q.TyString)  , V_const (_, Q.String s)   -> Obj.magic s
      | B.Const (_, Q.TyNull)    , _                         -> Obj.magic 0

      | B.Void _, V_record (_, m, r) when StringMap.is_empty m && !r = None -> Obj.magic ()

      (* bool *)
      | B.Bool _, V_record (_, fields, _) ->
          let semantic_bool = (StringMap.mem "true" fields) && not (StringMap.mem "false" fields) in
          Obj.magic semantic_bool

      (* option *)
      | B.Option (_, o), V_record (_, fds, _) ->
          let semantic_option =
            match StringMap.find_opt "some" fds with
            | None -> if not (StringMap.mem "none" fds) then clash () else None
            | Some v -> Some (aux o (Lazy.force v))
          in
          Obj.magic semantic_option

      | B.External (_, name_exp, param_exp), V_extern (_, name, params, obj) ->
          (*
            The typer should have done his work, we could actually just give
            the value as it is : Obj.obj obj, but we add some runtime checks
            because opatop can work in #typer off mode.
            TODO: maybe add an option for desactivating checks, and let
            opatop just segfauls.
          *)
          let _ = (* check *)
            if name_exp <> name
            then clash ()
            else
              (* verification of parametres - specialisation of parametric types in externs types *)
              let rec fold subst = function
                | [], [] -> subst
                | a::u, b::v ->
                    let expected = a in
                    let found = b in
                    let subst = BslTypes.check_inclusion ~static_strict_check:false subst ~expected ~found in
                    fold subst (u, v)
                | _ -> clash ()
              in
              let subst = BslTypes.empty_substitution in
              let _ = fold subst (param_exp, params) in
              ()
          in
          (* the extern must not be projected *)
          Obj.obj obj

      | B.External _, _ ->
          let extra = "@\nHowever, if this opa value is really@ the extern type the bypass is waiting for,@ consider that this bypass is doing @{<bright>illicit things@}...@ The implementation of this bypass should be@ patched to use the type constructor opa[]@\n"
          in
          clash ~extra ()

      (* Here, if the arg-sig or ret_sig are more general that the typ in the real function
         We must add some qml-conversion *)
      | B.Fun _, V_bypass (pos, args, ret, obj) -> (
          (* A value in a V_bypass is an ocaml function. It can be passed 'as is' *)
          (* We perform some checks (cf documentation [Runtime type checking) of this module) *)
          let _ =
            BslTypes.check ~static_strict_check:false ~expected:ty ~found:(B.Fun (pos, args, ret))
          in
          Obj.obj obj
        )

      (* Here is the mixte-functionnal conversion qml --> ocaml : Good Luck to have something like it in C ;) *)
      | B.Fun (_, tys, tyres), V_closure (_, env, Q.Lambda (_, ids, body)) ->
          let rec caml_lambda rtys rids env = match rtys, rids with
            | [], [] ->
                Obj.magic (fun () -> aux tyres (eval env body) )
            | [ty], [id] ->
                Obj.magic (fun v ->
                  let qml = t_of_ocaml ty v in
                  let env = IdentMap.add id qml env in
                  aux tyres (eval env body)
                )
            | ty::rtys, id::rids ->
                Obj.magic (fun v ->
                  let qml = t_of_ocaml ty v in
                  let env = IdentMap.add id qml env in
                  caml_lambda rtys rids env
                )
            | _ -> assert false
          in caml_lambda tys ids (!env)

      | _, _ -> clash ()
    in
    aux ty value

end
