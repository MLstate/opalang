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
(* CF mli *)

(* wait for refactoring *)
module TypeVar = QmlTypeVars

(* dependencies *)
module String = Base.String
module Format = Base.Format

(* shorthand *)
module B = BslTypes
module Q = QmlAst

(* type alias for clarty *)
type 'a pprinter = 'a Format.pprinter

(* === *)

(* Error within a context of type [t] *)
let (!!) t fmt =
  BslTypes.pp_citation OManager.oformatter.contents t ;
  OManager.printf "The type is : %a@\n" B.pp t ;
  OManager.error fmt

type ('env, 'a, 'ast) generator = 'env -> 'a -> 'env * 'ast

module Opa =
struct
  let pp_scope ~scope fmt =
    let rec aux parfun fmt t =
      match t with
      | B.Const _
      | B.TypeVar _
      | B.Void _
      | B.Bool _ ->
          BslTypes.pp_scope ~scope fmt t

      | B.Option (_, t) ->
          Format.fprintf fmt "option(%a)" (aux false) t

      | B.OpaValue (_, t) ->
          aux parfun fmt t

      | B.Fun (_, args, ret) ->
          if parfun then Format.fprintf fmt "(%a)" (aux false) t else
            let paren_out = true in
            Format.fprintf fmt "%a -> %a" (Format.pp_list ",@ " (aux true)) args (aux paren_out) ret

      | B.Callback (pos, args, ret) ->
          Format.fprintf fmt "callback(%a)" (aux false) (B.Fun (pos, args, ret))

      (* all external and record types from bsl should be named *)

      | B.External (_, n, vs) ->
          LangPrint.pp_parameters (aux true) n fmt vs

    in aux false fmt

  let pp fmt =
    let scope = BslTypes.TypeVarPrint.new_scope () in
    pp_scope ~scope fmt

  let pp_definition fmt ty =
    let scope = BslTypes.TypeVarPrint.new_scope () in
    match ty with
    | B.External (_, name, params) ->
        Format.fprintf fmt "type %a = external" (LangPrint.pp_parameters (pp_scope ~scope) name) params

    | t -> !! t "Unexpected type for an external type definition@\n"

end

(*
  Note for hackers:
  For coherence of line errors, the type must fit on one line
*)
module Ocaml =
struct

  let nopos = FilePos.nopos "BslTypesGeneration.Ocaml"

  (* with zero arity fun, in ocaml we need to add a unit argument *)
  let unit_fun args =
    match args with
    | [] ->
        [B.Void nopos]
    | _ ->
        args

  let ty_void   = "unit"
  let ty_bool   = "bool"
  let ty_option = "option"

  (* Support for utilizing values via the server lib *)
  let serverlib = "ServerLib"
  let serverlib fmt s = Format.fprintf fmt "%s.%s" serverlib s

  (* QmlAst.const_ty *)
  let opa_char   = "ty_char"
  let opa_float  = "ty_float"
  let opa_int    = "ty_int"
  let opa_null   = "ty_null"
  let opa_string = "ty_string"

  (* Rest of bsl types *)
  let opa_void    = "ty_void"
  let opa_bool    = "ty_bool"
  let opa_option  = "ty_option"

  (*
    This is more likely a transformation, which can be done in AST(TODO).

    This defines the semantic of register type, since it is used to generated
    the mli of the bsl.

    BslTypesMap is structural, it is exact, and not hacky.
  *)

  let pp fmt =
    let scope = BslTypes.TypeVarPrint.new_scope () in
    let typevar fmt = BslTypes.TypeVarPrint.pp scope fmt in
    (* fresh type variable (string) for any same abstract type *)
    (*
    let map = ref BslTypesMap.empty in
    let index = ref (-1) in
    let memo fmt t =
      let v =
        match BslTypesMap.find_opt t !map with
        | Some v -> v
        | None ->
            incr(index);
            let v = Printf.sprintf "'opa_v%d" !index in
            map := BslTypesMap.add t v !map ;
            v
      in
      Format.pp_print_string fmt v
    in
    *)
    let rec opavalue parfun fmt t =
      match t with
      | B.Const (_, c) -> (
          match c with
          | Q.TyFloat -> serverlib fmt opa_float
          | Q.TyInt -> serverlib fmt opa_int
          | Q.TyNull -> serverlib fmt opa_null
          | Q.TyString -> serverlib fmt opa_string
        )

      | B.TypeVar (_, v) -> typevar fmt v

      | B.Void _ -> serverlib fmt opa_void
      | B.Bool _ ->
          serverlib fmt opa_bool

      | B.Option (_, t) ->
          Format.fprintf fmt "%a %a" (opavalue true) t serverlib opa_option

      | B.OpaValue _ ->
          !! t "Imbrications of opa[] constructors does not make any sense@\n"

      | B.Fun (_, args, ret) ->
          if parfun then Format.fprintf fmt "(%a)" (opavalue false) t else
            let paren_out = true in
            let args = unit_fun args in
            Format.fprintf fmt "%a -> %a" (Format.pp_list " -> " (opavalue true)) args (opavalue paren_out) ret

      | B.Callback (pos, args, ret) ->
          Format.fprintf fmt "callback(%a)" (opavalue false) (B.Fun (pos, args, ret))

      (* External and Record types should be column only *)
      | B.External (_, n, vs) -> (
          OcamlPrint.pp_parameters (opavalue true) n fmt vs
        )

    in
    let rec aux parfun fmt t =
      match t with
      | B.Const (_, c) ->
          Format.pp_print_string fmt (QmlAst.Const.ocamlbsl_string_of_ty c)

      | B.TypeVar (_, v) -> typevar fmt v

      | B.Void _ ->
          Format.pp_print_string fmt ty_void

      | B.Bool _ ->
          Format.pp_print_string fmt ty_bool

      | B.Option (_, t) ->
          Format.fprintf fmt "%a %s" (aux true) t ty_option

      | B.OpaValue (_, t) -> opavalue true fmt t

      | B.Fun (_, args, ret) ->
          if parfun then Format.fprintf fmt "(%a)" (aux false) t else
            let paren_out = true in
            let args = unit_fun args in
            Format.fprintf fmt "%a -> %a" (Format.pp_list " -> " (aux true)) args (aux paren_out) ret

      | B.Callback (pos, args, ret) ->
          Format.fprintf fmt "callback(%a)" (aux false) (B.Fun (pos, args, ret))

      (* all external and record types from bsl should be named *)
      | B.External (_, n, vs) ->
          OcamlPrint.pp_parameters (aux true) n fmt vs

    in aux false fmt

  let pp_definition fmt = function
    | B.External (_, name, params) ->
        Format.fprintf fmt "type %a" (OcamlPrint.pp_parameters pp name) params

    | t -> !! t "Unexpected type for an external type definition@\n"

end

module C =
struct
  (* Prototype use only - Not maintained, not supported *)

  (* <!> Hard link with Pervasives of C *)
  let ty_alphaval = "ty_alphaval"
  let ty_void = "ty_void"
  let ty_bool = "ty_bool"
  let ty_opavalue = "ty_opavalue"

  let pp fmt =
    let rec aux fmt = function
      | B.Const (_, c) ->
          Format.pp_print_string fmt (QmlAst.Const.cbsl_string_of_ty c)

      | B.TypeVar _ ->
          Format.pp_print_string fmt ty_alphaval

      | B.Void _ ->
          Format.pp_print_string fmt ty_void

      | B.Bool _ ->
          Format.pp_print_string fmt ty_bool

      (* In the prototype, option('a) is a [('a)*] *)
      | B.Option (_, o) ->
          Format.fprintf fmt "%a*" aux o

      (* FIXME: we can probably do better *)
      | B.OpaValue _ ->
          Format.pp_print_string fmt ty_opavalue

      | B.Callback (_, args, ret)
      | B.Fun (_, args, ret) -> (
          match args with
          | [] ->
              Format.fprintf fmt "%a(%s)" aux ret ty_void
          | _ ->
              Format.fprintf fmt "%a(%a)" aux ret (Format.pp_list ",@ " aux) args
        )
      | B.External (_, name, []) ->
          Format.pp_print_string fmt name

      (* wait for LLVM to decide what we want to do *)
      | B.External _ -> assert false

    in
    aux fmt

end
