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

(* shorthand *)
module O = Ocaml

(* -- *)
module type Printer =
sig
  type t
  type 'a printer = t -> 'a -> unit
  (* open OcamlAst *) open Ocaml
  val const_expr : const_expr printer
  val const_type_expr : const_type_expr printer
  val type_name : type_name printer
  val type_expr : type_expr printer
  val pattern : pattern printer
  val param_formel : param_formel printer
  val param_effectif : param_effectif printer
  val mlIdent : mlIdent printer
  val code : code printer
  val expr : expr printer
end

module type X =
sig
  type t
  val output_string : t -> string -> unit
end

(**
   <!> The function used for print stident should return the original
   name in case of a source. ident. (stident)
*)
let ident = Ident.stident

module Make ( X : X ) : Printer with type t = X.t =
struct
  open X
  type t = X.t
  type 'a printer = t -> 'a -> unit

  let paren printer oc a =
    output_string oc "(";
    printer oc a;
    output_string oc ")"

  let output_int oc i = output_string oc (string_of_int i)
  let output_float oc f = output_string oc (string_of_float f)
  let output_bool oc b = output_string oc (string_of_bool b)

  let output_ident oc id =
    match ident id with
    | "*" -> output_string oc " * " (* accidental comments... *)
    | idstr -> output_string oc idstr

  let output_concat_map oc sep map =
    let rec aux = function
      | [] -> ()
      | [last] -> map oc last
      | t::q ->
          map oc t;
          output_string oc sep;
          aux q
    in aux

  (* open OcamlAst *) open Ocaml
  let const_expr oc = function
    | String s ->
        output_string oc "\"";
        output_string oc (String.escaped s);
        output_string oc "\""
    | Int i ->
        if i < 0 then (
          output_string oc "(";
          output_int oc i;
          output_string oc ")"
        )
        else
          output_int oc i
    | Float f ->
        if f < 0. then (
          output_string oc "(";
          output_float oc f;
          output_string oc ")"
        )
        else
          output_float oc f
    | Bool b -> output_bool oc b
    | Char c ->
        output_string oc "'";
        output_string oc (Char.escaped c);
        output_string oc "'"
    | Unit ->
        output_string oc "()"

  let const_type_expr oc = function
    | TypeString ->  output_string oc "string"
    | TypeInt -> output_string oc "int"
    | TypeInt64 -> output_string oc "int64"
    | TypeFloat -> output_string oc "float"
    | TypeBool -> output_string oc "bool"
    | TypeChar -> output_string oc "char"
    | TypeUnit -> output_string oc "unit"

  let type_name oc = output_concat_map oc "." output_string

  let rec type_expr oc = function
    | TypeTuple li ->
        output_string oc "( ";
        output_concat_map oc " * " type_expr li;
        output_string oc " )";
    | TypeVar s -> output_string oc s
    | TypeName ([], name) -> type_name oc name
    | TypeName (tl, name) ->
        output_string oc "( ";
        output_concat_map oc ", " type_expr tl;
        output_string oc " ) ";
        type_name oc name
    | TypeConst c -> const_type_expr oc c
    | TypeRef a ->
        output_string oc "( ";
        type_expr oc a;
        output_string oc " ) ref";
    | TypeRecord stl ->
        output_string oc "{ ";
        output_concat_map oc " ; " record stl;
        output_string oc " }";
    | TypeConstructor stl ->
        output_concat_map oc " | " constructor stl
    | TypeArrow (a, b) ->
        (* FIXME, type within an arrow cannot be parenthesized for TypeLabels (for .mli files).
           Is it safe to drop the parens altogether? *)
        begin match a with
        | TypeLabel _ ->
            type_expr oc a;
            output_string oc " -> ";
            type_expr oc b
        | _ ->
            output_string oc "(";
            type_expr oc a;
            output_string oc ") -> ";
            type_expr oc b
        end
    | TypeVerbatim v ->
        output_string oc v
    | TypeLabel (opt, vname, vtype) ->
        if opt then
          output_string oc "?";
        output_string oc vname;
        output_string oc ":";
        type_expr oc vtype
  and record oc = function
    | mutable_, field, type_expr' ->
        (if mutable_ then  output_string oc "mutable ");
        output_string oc field;
        output_string oc " : ";
        type_expr oc type_expr'
  and constructor oc = function
    | name, None ->
        output_string oc name
    | name, Some of_type_expr ->
        output_string oc name;
        output_string oc " of ";
        type_expr oc of_type_expr

  let rec pattern oc = function
    | PatVar v -> output_ident oc v
    | PatList (a, b) ->
        output_string oc "(";
        pattern oc a;
        output_string oc ") :: (";
        pattern oc b;
        output_string oc ")"
    | PatEmptyList -> output_string oc "[]"
    | PatRecord spl ->
        output_string oc "{ ";
        output_concat_map oc " ; " pattern_record spl;
        output_string oc " }"
    | PatConstructor (s, []) | PatVariant (s, []) -> mlIdent oc s
    | PatConstructor (s, pl) | PatVariant (s, pl) ->
        mlIdent oc s;
        output_string oc " ( ";
        output_concat_map oc ", " pattern pl;
        output_string oc " )";
    | PatPVariant (s, []) ->
        output_string oc "`";
        mlIdent oc s
    | PatPVariant (s, pl) ->
        output_string oc "`";
        mlIdent oc s;
        output_string oc " ( ";
        output_concat_map oc ", " pattern pl;
        output_string oc " )";

    | PatConst c -> const_expr oc c
    | PatAny -> output_string oc "_"
    | PatAnnot (p, ty) ->
        output_string oc "( (";
        pattern oc p;
        output_string oc ") : ";
        type_expr oc ty;
        output_string oc " )"
    | PatAs (p, s) ->
        output_string oc "( ";
        pattern oc p;
        output_string oc " ) as ";
        output_ident oc s
    | PatTuple pl ->
        output_string oc "( ";
        output_concat_map oc ", " pattern pl;
        output_string oc " )"
    | PatArray pl ->
        output_string oc "[| ";
        output_concat_map oc " ; " pattern pl;
        output_string oc " |]"
    | O.PatLazy p ->
        output_string oc "lazy " ;
        pattern oc p
    | O.PatOr pl ->
        output_string oc "( ";
        output_concat_map oc " | " pattern pl;
        output_string oc " )"

  and pattern_record oc = function
    | field, pattern' ->
        output_string oc field;
        output_string oc " = ";
        pattern oc pattern'

  and param_formel oc = function
    | Label (s, None, None) ->
        output_string oc "~";
        output_string oc s
    | Label (_, None, Some _) -> assert false (* TODO *)
        (* (type_expr_to_fb (acc ++ "~(" ++ s ++ " : ") ty) ++ ")" *)
    | Label (_, Some _, None) -> assert false (* TODO *)
        (* pattern_to_fb (acc ++ "~" ++ s ++ " : ") n *)
    | Label (_, Some _, Some _) -> assert false (* TODO *)
        (* (type_expr_to_fb ((pattern_to_fb (acc ++ "~" ++ s ++ ":(") n) ++ " : ") ty) ++ ")" *)
    | Opt (s, None, None) ->
        output_string oc "?";
        output_string oc s
    | Opt (_, Some _, None) -> assert false (* TODO *)
         (* (type_expr_to_fb (acc ++ "?(" ++ s ++ " : ") ty) ++ ")" *)
    | Opt (s, None, Some e) ->
        output_string oc "?(";
        output_string oc s;
        output_string oc " = ";
        expr oc e;
        output_string oc ")"
    | Opt (_, Some _, Some _) -> assert false
        (* (expr_to_fb ((type_expr_to_fb (acc ++ "?((" ++ s ++ " : ") ty) ++ ") = ") e) ++ ")" *)
    | Pat p -> pattern oc p

  and param_effectif oc = function
    | Labeled (s, None) ->
        output_string oc "~";
        output_string oc s
    | Labeled (s, Some e) ->
        output_string oc "~";
        output_string oc s;
        output_string oc ":(";
        expr oc e;
        output_string oc ")"
    | Pated (id, false) ->
        mlIdent oc id
    | Pated (id, true) ->
        output_string oc "(";
        mlIdent oc id;
        output_string oc ")"

  and mlIdent oc = function
    | [] -> assert false
    | idents -> output_concat_map oc "." output_ident idents

  and code oc =
    let iter e =
      expr oc e;
      output_string oc "\n" in
    List.iter iter

  and expr_type oc = function
    | [], name, type_expr' ->
        output_string oc name;
        output_string oc " = ";
        type_expr oc type_expr'
    | params, name, type_expr' ->
        output_string oc "(";
        output_concat_map oc ", " output_string params;
        output_string oc ") ";
        output_string oc name;
        output_string oc " = ";
        type_expr oc type_expr'
  and expr_let oc = function
    | param_formel', expr' ->
        param_formel oc param_formel';
        begin
          let expr' =
            match expr' with
            | Abs (params, expr') ->
                output_string oc " ";
                output_concat_map oc " " param_formel params;
                expr'
          | _ -> expr'
          in
          output_string oc " = ";
          expr oc expr'
        end
  and expr_record oc = function
    | field, expr' ->
        output_string oc field;
        output_string oc " = ";
        expr oc expr'
  and expr_pattern oc = function
    | pattern', guard, expr' ->
        pattern oc pattern';
        let () =
          match guard with
          | None -> ()
          | Some guard ->
              output_string oc " when ";
              expr oc guard
        in
        output_string oc " -> ";
        expr oc expr'
  and labelparam oc v =
    begin match v with
    | Var (
        Labeled(_, _)
      ) ->
        expr oc v
    | _ -> paren expr oc v
    end

  and expr oc = function
    | Type [] -> assert false (* TODO: HdList.t in type definition *)
    | Type defs ->
        output_string oc "type ";
        output_concat_map oc " and " expr_type defs
    | Val (name, type_expr') ->
        output_string oc "val ";
        output_ident oc name;
        output_string oc " : ";
        type_expr oc type_expr'
    | Open mlIdent' ->
        output_string oc "open ";
        mlIdent oc mlIdent'

    (*
      | Module of string * expr option * code * expr option
      [Module(name, functor, contents, [Some e])] is a local module definition.
      [Module(name, functor, contents, None)] is a global module definition.*)
    | Module (name, None, code', None) ->
        output_string oc "module ";
        output_string oc name;
        output_string oc " = struct\n";
        code oc code';
        output_string oc "end"

    | Module (_, _, _, _) -> assert false

    | ModuleType (name, code') ->
        output_string oc "module type ";
        output_string oc name;
        output_string oc " = ";
        code oc code' (* code' *should* be an expr (Signature ...) ! *)

    | Structure code' ->
        output_string oc "struct\n";
        code oc code';
        output_string oc "end"
    | Signature (Inlined code') ->
        output_string oc "sig\n";
        code oc code';
        output_string oc "end"
    | Signature (Referenced _) -> assert false (* TODO *)

    (* Used to print functor signatures (for .mli files) *)
    | DeclareFunctor (name, args, (Some sign), (Structure [])) ->
    (* Yeah, that's ugly, we could use an option ^^^^^^^^^^ *)
        output_string oc "module ";
        output_string oc name ;
        output_string oc " :" ;
        List.iter
          (function
            | s, None -> output_string oc ("\nfunctor " ^ s ^ " ->")
            | s, Some si ->
                output_string oc "\nfunctor ("; output_string oc s;
                output_string oc " : "; expr oc si; output_string oc ") ->")
          args;
          output_string oc "\n" ; expr oc sign

    | DeclareFunctor (name, args, sig_opt, content) ->
        (* of string * (string * expr option) list * expr option * expr *)
        output_string oc "module ";
        output_string oc name;
        List.iter
          (function
            | s, None -> output_string oc ("\n" ^ s)
            | s, Some si ->
                output_string oc "\n("; output_string oc s;
                output_string oc " : "; expr oc si; output_string oc ")")
          args;
        (match sig_opt with
          | None -> ()
          | Some si -> output_string oc " :"; expr oc si) ;
        output_string oc " = " ;
        expr oc content

    | Constructor (ml, []) ->
        mlIdent oc ml
    | Constructor (ml, list) ->
        mlIdent oc ml;
        output_string oc "(";
        output_concat_map oc ", " expr list;
        output_string oc ")";
    | ConstructorPV (ml, []) ->
        output_string oc "`";
        mlIdent oc ml
    | ConstructorPV (ml, list) ->
        output_string oc "`";
        mlIdent oc ml;
        output_string oc "(";
        output_concat_map oc ", " expr list;
        output_string oc ")";
    | Const const_expr' ->
        const_expr oc const_expr'
    | Var p ->
        param_effectif oc p
    | MakeRef expr' ->
        output_string oc "ref (";
        expr oc expr';
        output_string oc ")"
    | GetRef expr' ->
        output_string oc "!(";
        expr oc expr';
        output_string oc ")"
    | SetRef (e, f) ->
        expr oc e;
        output_string oc " := ";
        expr oc f
    | SetMutable (e, f) ->
        expr oc e;
        output_string oc " <- ";
        expr oc f
    | Lazy e ->
        output_string oc "lazy (";
        expr oc e;
        output_string oc ")"
    | Tuple pl ->
        output_string oc "(";
        output_concat_map oc ", " expr pl;
        output_string oc ")"
    | Cons (hd, (Cons (_, _) as tl)) ->
        (* This is a kludge, maybe separate cons's for [1,2,3] and (1::(2::(3::[])))??? *)
        let rec build acc = function
          | Cons (a, l) -> build (a::acc) l
          | EmptyList -> List.rev acc
          | _ ->
              (* Too much work to arrange for type sharing with_out_channel *)
              (*output_string stderr "\nocamlPrint.ml Cons error <<<";
              expr stderr e;
              output_string stderr ">>>\n";*)
              assert false
        in
        let list = build [hd] tl in
        output_string oc "[ ";
        output_concat_map oc " ; " expr list;
        output_string oc " ]"
    | Cons (hd, tl) ->
        output_string oc "((";
        expr oc hd;
        output_string oc ")::(";
        expr oc tl;
        output_string oc "))"
    | EmptyList ->
        output_string oc "[]"
    | Cond (if_, then_, else_) ->
        output_string oc "if ( ";
        expr oc if_;
        output_string oc " ) then ( ";
        expr oc then_;
        output_string oc " ) else ( ";
        expr oc else_;
        output_string oc " )";
    | App (e, f) ->
        let rec print_fun e =
          match e with
          | App (e, f) ->
              begin match e with
              | Var (Pated (_, false)) -> expr oc e;
              | _ -> print_fun e
              end;
              output_string oc " ";
              labelparam oc f
          | _ -> paren expr oc e;
        in begin
          output_string oc "(";
          print_fun e;
          output_string oc " ";
          labelparam oc f;
          output_string oc ")";
        end
    | Abs (params, e) ->
        let e, params =
          let rec uncons acc e =
            match e with
            | Abs(params, e) ->
                uncons (params :: acc) e
            | _ -> e, (List.flatten (List.rev acc))
          in uncons [params] e
        in
        begin
          output_string oc "(fun ";
          output_concat_map oc " " param_formel params;
          output_string oc " -> ";
          expr oc e;
          output_string oc ")"
        end
    | Let [] | Letrec [] | Letin ([], _) | Letrecin ([], _) -> assert false (* TODO: HdList.t *)
    | Let binds ->
        output_string oc "let ";
        output_concat_map oc "\nand " expr_let binds;
    | Letrec binds ->
        output_string oc "let rec ";
        output_concat_map oc "\nand " expr_let binds;
    | Letin (binds, e) ->
        output_string oc "let ";
        output_concat_map oc "\nand " expr_let binds;
        output_string oc " in\n";
        expr oc e
    | Letrecin (binds, e) ->
        output_string oc "let rec ";
        output_concat_map oc "\nand " expr_let binds;
        output_string oc " in\n";
        expr oc e
    | Record (rec_opt,fields) ->
        output_string oc "{ ";
        (match rec_opt with Some r -> (output_string oc r; output_string oc " with ") | None -> ());
        output_concat_map oc " ; " expr_record fields;
        output_string oc " }"
    | Dot (((Var _) as a), s) ->
        expr oc a;
        output_string oc ".";
        output_string oc s
    | Dot (a, s) ->
        output_string oc "(";
        expr oc a;
        output_string oc ").";
        output_string oc s
    | Match (e, pel) ->
        output_string oc "(match ";
        expr oc e;
        output_string oc " with\n";
        output_concat_map oc "\n| " expr_pattern pel;
        output_string oc ")";
    | Sequence (e, f) ->
        expr oc e;
        output_string oc " ; ";
        expr oc f
    | Annot (e, ty) ->
        output_string oc "( ( ";
        expr oc e;
        output_string oc " ) : ";
        type_expr oc ty;
        output_string oc " )"
    | Function [] -> assert false
    | Function pel ->
        output_string oc "function ";
        output_concat_map oc "\n| " expr_pattern pel
    | Exception (s, None) ->
        output_string oc "exception ";
        output_string oc s;
    | Exception (s, Some type_expr') ->
        output_string oc "exception ";
        output_string oc s;
        output_string oc " of ";
        type_expr oc type_expr'
    | Raise (ml, None) ->
        output_string oc "raise ";
        mlIdent oc ml
    | Raise (ml, Some e) ->
        output_string oc "raise ";
        output_string oc "(";
        mlIdent oc ml;
        output_string oc "( ";
        expr oc e;
        output_string oc " ))";
    | Try (e, with_) ->
        output_string oc "(try (\n";
        expr oc e;
        output_string oc "\n) with\n";
        output_concat_map oc "\n| " expr_pattern with_;
        output_string oc ")\n" (* We may be in a pat match... *)
    | AnArray el ->
        output_string oc "[| ";
        output_concat_map oc " ; " (paren expr) el;
        output_string oc " |]";
    | Comment comment ->
        output_string oc "(* ";
        output_string oc comment;
        output_string oc " *) "
    | Comments (comment, expr') ->
        output_string oc "( (* ";
        output_string oc comment;
        output_string oc " *) ";
        expr oc expr';
        output_string oc ")"
    | Assert e ->
        output_string oc "(assert ";
        expr oc e;
        output_string oc ")";
    | LineAnnot (n, f, e) ->
        output_string oc "\n#";
        output_int oc n;
        output_string oc " \"";
        output_string oc f;
        output_string oc "\"\n";
        expr oc e
    | Verbatim s -> output_string oc s
end

module X_Output : X with type t = out_channel =
struct
  type t = out_channel
  let output_string = Pervasives.output_string
end

module X_Buf : X with type t = Buffer.t =
struct
  type t = Buffer.t
  let output_string = Buffer.add_string
end

module X_Fmt : X with type t = Format.formatter =
struct
  type t = Format.formatter
  let output_string = Format.pp_print_string
end

module X_FBuf : X with type t = FBuffer.t ref =
struct
  type t = FBuffer.t ref
  let output_string t s =
    let t' = FBuffer.add !t s in
    t := t'
end

module Output = Make ( X_Output )
module Buf = Make ( X_Buf )
module Fmt = Make ( X_Fmt )
module FBuf = Make ( X_FBuf )

module Deprecated =
struct
  type 'a printer = FBuffer.t -> 'a -> FBuffer.t
  let const_expr fb c =
    let r = ref fb in
    FBuf.const_expr r c;
    !r
  let type_expr fb t =
    let r = ref fb in
    FBuf.type_expr r t;
    !r
end

(* {6 Pretty printer} *)
type 'a pprinter = 'a LangPrint.pprinter

let pp_list = Base.Format.pp_list
(* beware for error message, oneline should be set to true *)
(*
  <!> Rather than trying to hack LangPrint with optionnal arguments,
  OcamlPrint defines his own pp_parameters.
*)
let pp_parameters pp name fmt params =
  match params with
  | [] ->  Format.pp_print_string fmt name
  | [p] -> Format.fprintf fmt "%a %s" pp p name
  | _ ->   Format.fprintf fmt "(%a) %s" (pp_list ", " pp) params name
