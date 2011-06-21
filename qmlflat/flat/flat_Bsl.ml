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
module Format = Base.Format
module String = Base.String

(* refactoring in progress *)

(* shorthands *)
module B = BslTypes
module BI = BslInterface
module Q = QmlAst

(* alias *)
module ServerLib = Flat_Common.ServerLib

(* -- *)

(*
  Compositional state.
  Should be reset before each compilation unit.
*)

module State =
struct

  let segfault : (BslKey.t * string) list ref = ref []

  let uncps_level = ref 0

  let reset () =
    segfault := [] ;
    uncps_level := 0 ;
    ()
end

let reset = State.reset

(* debug *)

let debug fmt =
  OManager.printf ("@[<2>@{<cyan>[flat]@}@ "^^fmt^^"@]@.")


module ML_CTrans = (* DO NOT COERCE HERE (cf mli) *)
struct

  type env = {
    options : Qml2ocamlOptions.argv_options ;
    coercion : bool ;
    ocaml_type_map : string StringMap.t ;
    typesmap : BslInterface.typesmap ;

    (*
      Collect during projection and compilation the bypass which will segfault.
      Looks scary, but it is only ugly. Used for making error.
      TODO: this is not the final version.
    *)
    segfault : string BslKeyMap.t ;

    (*
      Collect definition of uncps needed to project bypasses.
      Kind of hacky.
    *)
    uncps_level : int
  }

  let empty () = {
    options = Qml2ocamlOptions.ArgvOptions.default "qmlflat" ;
    coercion = true ;
    ocaml_type_map = StringMap.empty ;
    typesmap = BslKeyMap.empty ;
    segfault = BslKeyMap.empty ;
    uncps_level = 0 ;
  }

  (* FIXME: this is ugly *)
  let collect_segfault env =
    let segfault =
      List.fold_left (
        fun seg (key, message) ->
          BslKeyMap.add key message seg
      ) env.segfault !State.segfault in
    State.segfault := [] ;
    { env with segfault = segfault }
  let add_segfault segfault = State.segfault := segfault :: !State.segfault

  let collect_uncps_level env =
    { env with uncps_level = max !State.uncps_level env.uncps_level }

  let add_uncps_level level = State.uncps_level := max !State.uncps_level level

  let bslkey_set, bslkey_get =
    let bslkey = ref (BslKey.normalize "") in
    (fun s -> bslkey := s), (fun () -> !bslkey)

  let bsltags_set, bsltags_get =
    let bsltags = ref BslTags.default in
    (fun s -> bsltags := s), (fun () -> !bsltags)

  (* FIXME: this is ugly, follow guidelines about error reporting *)
  let failure_projection fct bslkey t p =
    match fct p with
    | Some (BI.MetaCode projection) ->
        let m = Format.sprintf (
          "ByPass : %%%% %a %%%%\nProjection failure on a parametric type@\n"^^
          "the instance : %a@\n"^^
          "in the type  : %a@\n"^^
          "is not allowed in the interface of a bypass because there is no@\n"^^
          "way to know how the following projection must be applied to@\n"^^
          "this instance from a value of this type :@\n%s"
        )
          BslKey.pp bslkey
          BslTypes.pp p
          BslTypes.pp t
          projection in
        add_segfault (bslkey, m)
    | Some (BI.MetaComment _)
    | None -> ()


  (* =================== *)

  (* utils *)

  let serverlib = Flat_Common.serverlib
  let (!!) fmt = OcamlPrint.Fmt.expr fmt

  let metacode fmt =
    let k s = Some (BI.MetaCode s) in
    Format.ksprintf k fmt

  let metacomment fmt =
    let k s = Some (BI.MetaComment s) in
    Format.ksprintf k fmt

  let rec aux_qml_of_ocaml env typ (BI.MetaIdent id) = match typ with
    | B.Const _ -> (
        (* match the constant in case we reswitch representation for constants *)
        (* TyNull does not mean anything *)
        None
      )

    | B.TypeVar _ ->
        metacomment "alpha"

    | B.Void _ ->
        metacode "%a" (!!) ServerLib.empty

    | B.Bool _ ->
        metacode "%a %s" (!!) ServerLib.wrap_bool id

    | B.Option (_, o)  -> (
        let x = "ocaml" in
        let conv =
          match aux_qml_of_ocaml env o (BI.MetaIdent x) with
          | Some (BI.MetaCode conv) -> conv
          | Some (BI.MetaComment comment) -> Printf.sprintf "%s (* : %s *)" x comment
          | None -> x in

        metacode "match %s with None -> %a | Some %s -> %a (%s)"
          id
          (!!) ServerLib.none
          x
          (!!) ServerLib.some
          conv
      )

    (* bsl-v2 : manipulation of OpaValue are wrapped for more type safety *)
    (* with LLVM, these magic are not needed *)

    | B.OpaValue (_, bslty) -> (
        match bslty with
        | B.Const (_, const) -> (
            let kwt = QmlAst.Const.ocamlbsl_string_of_ty const in
            metacode "( ( Obj.magic ( %s : ServerLib.ty_%s) ) : %s )" id kwt kwt
          )

        | B.External _ ->
            metacode "( ( Obj.magic %s) : %s.record )" id serverlib

        | typ ->
            metacomment "%a" BslTypesGeneration.Ocaml.pp typ
      )

    | B.External (_, _, p) -> (
        (* TODO : Add coercion ...*)
        List.iter
          (fun ty ->
             match ty with
             | B.OpaValue _ -> ()
             | _ -> (failure_projection
                       (fun ty -> aux_ocaml_of_qml env ty (BI.MetaIdent id))
                       (bslkey_get ()) typ) ty
          )
          p;
        None)


    | B.Fun (_, inputs, output) ->
        let p id =
          BslLib.ml_function_projection
            ~inputs:(fun t -> aux_ocaml_of_qml env t)
            ~outputs:(fun t -> aux_qml_of_ocaml env t)
            inputs output (BI.MetaIdent id) in (* beware of inversion *)
        let conv =
          match p id with
            | None -> id
            | Some (BI.MetaComment comment) -> Printf.sprintf "( %s (* : %s *))" id comment
            | Some (BI.MetaCode conv) -> Printf.sprintf "(%s)" conv in
        let proj_for_cps = BslTags.do_projection (bsltags_get ()) "cps" in
        if not (env.options.Qml2ocamlOptions.cps && proj_for_cps) then
          if not env.options.Qml2ocamlOptions.qml_closure then p id
          else (
            let n = List.length inputs in
            metacode "CR.import (%s) %d" conv n
          )
        else
          if env.options.Qml2ocamlOptions.qml_closure then (
            let n = List.length inputs in
            add_uncps_level n;
            metacode "CR.import (cps%d %s) %d" n conv (n+1)
          )
          else (
            let n = List.length inputs in
            add_uncps_level n;
            metacode "cps%d %s" n conv
          )

  and aux_ocaml_of_qml env typ (BI.MetaIdent id) : _ option = match typ with

    | B.Const _ -> (
        (* match the constant in case we reswitch representation for constants *)
        (* TyNull does not mean anything *)
        None
      )

    | B.TypeVar _ -> (
        metacomment "alpha"
      )

    | B.Void _ -> (
        metacode "Pervasives.ignore %s" id
      )

    | B.Bool _ -> (
        metacode "%a %s" (!!) ServerLib.unwrap_bool id
      )

    | B.Option (_, o) -> (
        (*Note: we assume that, at this stage, anything that has the type ['a option] and that is
          not [{some = ...}] is [{none}]*)
        let x = "qml" in
        let is_conv = ref false in
        let conv =
          match aux_ocaml_of_qml env o (BI.MetaIdent x) with
          | Some (BI.MetaCode conv) -> is_conv := true ; conv
          | Some (BI.MetaComment _) -> x
          | None -> x in
        if !is_conv then
          metacode
            "match %a %s with None -> None | Some %s -> Some (%s)"
            (!!) ServerLib.unwrap_option id
            x conv
        else
          metacode "%a %s" (!!) ServerLib.unwrap_option id
      )

    (* bsl-v2 : manipulation of OpaValue are wrapped for more type safety *)
    (* with LLVM, these magic are not needed *)

    | B.OpaValue (_, bslty) -> (
        match bslty with
        | B.Const (_, const) -> (
            let kwt = QmlAst.Const.ocamlbsl_string_of_ty const in
            metacode "( ( Obj.magic ( %s : %s) ) : ServerLib.ty_%s )" id kwt kwt
          )

        | B.External _ ->
            metacode "(Obj.magic ( %s : %s.record ))" id serverlib

        | typ ->
            metacomment "%a" BslTypes.pp typ
      )

    | B.External (_, _, p) -> (
        (* TODO : Add coercion ...*)
        List.iter
          (fun ty ->
             match ty with
             | B.OpaValue _ -> ()
             | _ -> (failure_projection
                       (fun ty -> aux_ocaml_of_qml env ty (BI.MetaIdent id))
                       (bslkey_get ()) typ) ty
          )
          p;
        None
      )

    | B.Fun(_, inputs, output) ->
        let p id =
          BslLib.ml_function_projection
            ~inputs:(fun t -> aux_qml_of_ocaml env t)
            ~outputs:(fun t -> aux_ocaml_of_qml env t)
            inputs output (BI.MetaIdent id) in  (* beware of inversion *)
        let proj_for_cps = BslTags.do_projection (bsltags_get ()) "cps" in
        if not (env.options.Qml2ocamlOptions.cps && proj_for_cps) then
          if not env.options.Qml2ocamlOptions.qml_closure then p id
          else (
            let fresh = Ident.stident (Ident.next "f") in
            match p fresh with
            | None ->
                metacode "CR.export %s" id

            | Some (BI.MetaComment comment) ->
                metacode "CR.export %s (* : %s *)" id comment

            | Some (BI.MetaCode conv) ->
                metacode "let %s = CR.export (%s) in %s" fresh id conv
          )
        else
          let n = List.length inputs in
          add_uncps_level n;
          let fresh = Ident.stident (Ident.next "f") in
          let uncps = Printf.sprintf "uncps%d bslkey k %s"
            n
            (if env.options.Qml2ocamlOptions.qml_closure
             then Printf.sprintf "(CR.export %s)" id
             else id) in
          let meta_code =
            match p fresh with
            | None ->
                uncps

            | Some (BI.MetaComment comment) ->
                Printf.sprintf "%s (* : %s *)" uncps comment

            | Some (BI.MetaCode conv) ->
                Printf.sprintf "let %s = %s in %s" fresh uncps conv in

          Some (BI.MetaCode meta_code)

  let proj_cps_snd_order bsltags =
    bsltags.BslTags.second_order
    && BslTags.do_projection bsltags "cps"

  let proj_raise bsltags =
    bsltags.BslTags.raise_

  let proj bsltags env =
    if not env.options.Qml2ocamlOptions.cps
    then `none
    else
      if proj_raise bsltags
      then `raise_
      else
        if proj_cps_snd_order bsltags
        then `second_order
        else `none

  let more_args _ bsltags env =
    match proj bsltags env with
    | `none -> None
    | `raise_ | `second_order -> Some "k"

  let more_code bslkey bsltags env =
    match proj bsltags env with
    | `none -> None
    | `second_order -> Some (
        Printf.sprintf "  let bslkey = %S in" (BslKey.to_string bslkey)
      )
    | `raise_ -> Some (
        Printf.sprintf (
          "  let bslkey = %S in\n"^^
          "  let okko = try ("
        ) (BslKey.to_string bslkey)
      )

  let return _ bsltags env (BI.MetaIdent id) =
    match proj bsltags env with
    | `none -> None
    | `second_order -> Some (
        Printf.sprintf "QmlCpsServerLib.return k %s" id
      )
    | `raise_ -> Some (
        Printf.sprintf (
          "`OK %s)\n"^^
          "  with e -> `KO e in\n"^^
          "  match okko with\n"^^
          "  | `OK r -> QmlCpsServerLib.return k r\n"^^
          "  | `KO e -> return_exc bslkey e k"
        )
          id
      )

  let qml_of_ocaml ~bslkey ~bsltags t ~env meta_ident =
    if bsltags.BslTags.cps_bypass then
      env, None
    else
      let _ = bslkey_set bslkey in
      let _ = bsltags_set bsltags in
      let t = aux_qml_of_ocaml env t meta_ident in
      let env = collect_segfault env in
      let env = collect_uncps_level env in
      env, t

  let ocaml_of_qml ~bslkey ~bsltags t ~env meta_ident =
    let _ = bslkey_set bslkey in
    let _ = bsltags_set bsltags in
    let t = aux_ocaml_of_qml env t meta_ident in
    let env = collect_segfault env in
    let env = collect_uncps_level env in
    env, t

  (* =================== *)
  let runtime_ocaml_coercion ~bslkey ~bsltags typ ~env s =
    let _ = bslkey_set bslkey in
    let _ = bsltags_set bsltags in
    (env,
     if env.coercion then (
       #<If:BSL_PROJECTION $equals "flat">
         debug "coercing %a: %a@\n" BslKey.pp bslkey BslTypes.pp typ
       #<End>;
       match typ with
       | B.Const _ ->
           Format.sprintf "( %s : %a )" s BslTypesGeneration.Ocaml.pp typ
       | B.OpaValue (_, opatyp) -> (
           match opatyp with
           | B.Const _ ->
               Format.sprintf "( %s : %a )" s BslTypesGeneration.Ocaml.pp opatyp
           | B.Bool _
           | B.Void _
           | B.Option _ ->
               Format.sprintf "( %s : %s.record )" s serverlib
           | _ ->
               Format.sprintf "( %s (* : %a *) )" s BslTypes.pp typ
         )
       | _ ->
           Format.sprintf "( %s (* : %a *) )" s BslTypes.pp typ
     )
     else s)

  (*Generation of magic fields*)
  let conversion_code env =
    let buf = FBuffer.create 1024 in
    let buf = FBuffer.addln buf "(* Translation of bsl in the flat compiler runtime algebra*)" in
    let buf =
      if env.options.Qml2ocamlOptions.qml_closure then
        FBuffer.addln buf "module CR = QmlClosureRuntime"
      else buf
    in
    let buf = FBuffer.addln buf "(** {6 cps tools} *)" in
    let buf =
      ignore Opacapi.Opabsl.BslPervasives.return_exc ;
      FBuffer.addln buf "let return_exc = OpabslgenMLRuntime.BslPervasives.return_exc"
    in
    let buf =
      FBuffer.addln buf (QmlCpsRewriter.meta_cps_utils env.uncps_level)
    in
    let buf = FBuffer.addln buf "(** {6 closure tools} *)" in
    let buf = FBuffer.addln buf
      (if env.options.Qml2ocamlOptions.qml_closure then
         Pass_Closure.generate_applys ~at_least:env.uncps_level `caml
       else "(* closure not activated *)")
    in
    let buf = FBuffer.addln buf "(** {6 Converters} *)" in
    (env, FBuffer.contents buf)
end

module FlatBSL = BslLib.LibBSLForQml2Ocaml (ML_CTrans)

let build_ctrans_env ?(typesmap=BslKeyMap.empty) options =
  let ocaml_type_map =
    BslLib.record_path_map_of_typesmap ~complete:false ~runtime:true typesmap
  in { ML_CTrans.
    options = options ;
    coercion = true ;
    ocaml_type_map = ocaml_type_map ;
    typesmap = typesmap ;
    segfault = BslKeyMap.empty ;
    uncps_level = 0
  }

let is_segfault key env = BslKeyMap.find_opt key env.ML_CTrans.segfault
