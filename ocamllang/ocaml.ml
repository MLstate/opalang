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
(*
    @author Geoffroy Chollon
    @author Henri Binsztok
    @author Mathieu Barbin
**)

(* depends *)
module List = BaseList

(**)

(* REFACTOR: replace by Ident.t *)
type ident = Ident.t

type const_expr =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Char of char
  | Unit

type const_type_expr =
  | TypeString
  | TypeInt
  | TypeInt64
  | TypeFloat
  | TypeBool
  | TypeUnit

(* Module.Module.name *)
type type_name = string list

type type_expr =
  | TypeVar of string (* 'a *)
  | TypeName of type_expr list * type_name (* ('a, ...) t *)
  | TypeConst of const_type_expr
  | TypeRef of type_expr
  | TypeTuple of type_expr list
  | TypeRecord of (bool (* mutable *) * string * type_expr) list
  | TypeConstructor of (string * type_expr option) list
  | TypeArrow of type_expr * type_expr
  | TypeLabel of bool (* optional *) * string * type_expr
  | TypeVerbatim of string

type mlIdent = ident list
(* and mlIdent = InModule of (string * mlIdent) | InVector of (expr * mlIdent option) | InRecord of (string * mlIdent option) *)

type pattern =
  | PatVar of ident
  | PatList of pattern * pattern (* hd :: tl *)
  | PatEmptyList
  | PatRecord of (string * pattern) list
  | PatConstructor of mlIdent * pattern list
  | PatVariant of mlIdent * pattern list
  | PatPVariant of mlIdent * pattern list
      (**
         A polymorphic variant.
         Note: The ocaml manual states that the name of the polymorphic variant should start with an uppercase latter.
         This is not enforced, either by this module or by the ocaml compiler.
      *)

  | PatConst of const_expr
  | PatAny
  | PatAnnot of pattern * type_expr
  | PatTuple of pattern list
  | PatAs of pattern * ident
  | PatArray of pattern list
  | PatLazy of pattern
  | PatOr of pattern list

type param_formel = (* FIXME: formal_param *)
    (* ((~label:variable) : t) *)
  | Label of string * pattern option * type_expr option (**A non-optional labelled argument*)
  | Opt of string * type_expr option * expr option      (**An optional labelled argument*)
  | Pat of pattern
and param_effectif = (* FIXME: effective_param *)
  | Labeled of string * expr option
  | Pated of mlIdent * bool (* l'identifiant doit être protégé avec une parenthèse *)

and code = expr list

and signature =
  | Inlined of code
  | Referenced of string list

(* FIXME: manque signatures + signatures inlinees dans les modules *)
(* FIXME: contraindre les string a etres des noms de modules ou d'exceptions (majuscules), de variables de type ('...) *)
and expr =
  | Type of (string list * string * type_expr) list (* type ('...) y = ... and ... *)
  | Val of ident * type_expr (* val x : ... *)

  | Open of mlIdent
  | Module of string * expr option * code * expr option (**[Module(name, functor, contents, [Some e])] is a local module definition.
                                                           [Module(name, functor, contents, None)] is a global module definition.*)
  | ModuleType of string * code
  | Structure of code (* struct ... end *)
  | Signature of signature (* sig ... end *)
  | DeclareFunctor of string * (string * expr option) list * expr option * expr
  | Constructor of mlIdent * expr list
  | ConstructorPV of mlIdent * expr list  (* `X (...): constructor of a polymorphic variant *)
  | Const of const_expr
  | Var of param_effectif
  | MakeRef of expr
  | GetRef of expr
  | SetRef of expr * expr
  | SetMutable of expr * expr
  | Lazy of expr
  | Tuple of expr list
  | Cons of expr * expr (**Addition of an element before a list.
                           [Cons(a,b)] is [a::b]*)
  | EmptyList
  | Cond of expr * expr * expr (* if e then e else e *)
  | App of expr * expr (* e e *)
  | Abs of param_formel list * expr (* \lambda x.e *)
  | Let of (param_formel * expr) list (* let x = e and ... *)
  | Letrec of (param_formel * expr) list (* let rec x = e and ... *)
  | Letin of (param_formel * expr) list * expr (* let x = e and ... in e *)
  | Letrecin of (param_formel * expr) list * expr (* let rec x = e and ... in e *)
  | Record of string option * (string * expr) list (* { f = e ; ... } *)
  | Dot of expr * string (* e.f *)
  | Match of expr * (pattern * expr option (* guard *) * expr) list
  | Sequence of expr * expr
  | Annot of expr * type_expr
  | Function of (pattern * expr option (* guard *) * expr) list
  (* exceptions *)
  | Exception of string * type_expr option
  | Raise of mlIdent * expr option
  | Try of expr * (pattern * expr option (* guard *) * expr) list
  | AnArray of expr list
  | Comment of string (* stand-alone comment *)
  | LineAnnot of int (* line number *) * string (* file name *) * expr
  | Comments of string * expr
  | Assert of expr
  | Verbatim of string

(**
   Special identifiers : need to be protected with parenthesis.
*)
let special_idents =
  let t = Hashtbl.create 50 in
  let spe_i = [ "mod"; "land"; "lor"; "lxor" ;
                "+"; "+."; "="; "<>"; "=="; "!="; "-"; "-."; "*"; "**"; "*."; "/"; "/."; ">"; "<"; "<="; ">="; "<>"; "&&"; "||"; "~-"; "~-."; "^"; "@"; "asr"]
  in
  List.iter (fun s -> Hashtbl.add t s (Pated ([Ident.source s], true))) spe_i;
  t

(**
   {6 Shortcuts}
*)

(**
   Construct a simple variable non protected by parenthesis.
   The name is a source name, not analysed. (Ident.Source)
*)
let make_Var s =
  try
    Var (Hashtbl.find special_idents s)
  with
  | Not_found -> Var (Pated ([Ident.source s], false))

(**
   Construct a complex variable 'List.Make.a_function' non protected by parenthesis
   The names are source names, not analysed. (Ident.Source)
*)
let make_Varl sl = Var (Pated (List.map Ident.source sl, false))

(** Construct a toplevel let declaration for one element,
    [let foo = bar;;]*)
let make_Let id c1 = Let ([id, c1])

(** Construct a toplevel let declaration for several elements,
    [let foo = bar;; let sna = toto;;]*)
let make_Letand idcl = Let (idcl)

(** Construct a toplevel let rec declaration for one element,
    [let rec foo = bar;;]*)
let make_Letrec id c1 = Letrec ([id, c1])

(** Construct a toplevel let rec declaration for several elements,
    [let rec foo = bar;; let rec sna = toto;;]*)
let make_Letrecand idcl = Letrec (idcl)

(** Construct a local let declaration for one element,
    [let foo = bar in e;;]*)
let make_Letin id c1 e = Letin ([id, c1], e)

(** Construct a local let declaration for several elements,
    [let foo = bar and sna = toto in e]*)
let make_Letandin idcl e = Letin (idcl, e)

(** Construct a local let rec declaration for one element,
    [let rec foo = bar in e]*)
let make_Letrecin id c1 e = Letrecin ([id, c1], e)

(** Construct a local let rec declaration for several elements,
    [let rec foo = bar and rec sna = toto in e;;]*)
let make_Letrecandin idcl e = Letrecin (idcl, e)

let make_param_formel s = Pat (PatVar s)
let pf s = Pat (PatVar (Ident.source s))
(** Construct an array using Obj.magic *)
let make_polymorphic_array l = AnArray (List.map (fun e -> App (make_Varl ["Obj"; "magic"] , e)) l)
let make_pair l = Tuple (List.map (fun e -> App (make_Varl ["Obj"; "magic"] , e)) l)
(** Construct successive function applications from a list *)
let make_AppL l =
  let rec aux l = match l with
  | [] -> assert false
  | [x] -> x
  | a::q -> App (aux q, a)
  in aux (List.rev l)
(** *)
let make_magic v = App (make_Varl ["Obj"; "magic"], v)
let make_obj   v = App (make_Varl ["Obj"; "obj"], v)
let make_repr  v = App (make_Varl ["Obj"; "repr"], v)
let make_lazy_force v = App (make_Varl ["Lazy"; "force"], v)

(**
   [make_unsafe_get x i] is [Obj.obj (Obj.field (Obj.repr x) i)]
*)
let make_unsafe_get x p = make_obj ( make_AppL [
  make_Varl ["Obj" ; "field"] ;
  make_repr x ;
  Const (Int p)
] )

(** Build [unsafe_get] / [unsafe_set]*)

let make_array_unsafe_get p t = make_magic (make_AppL [make_Varl ["Array"; "unsafe_get"]; make_magic t; Const (Int p)])
let make_array_unsafe_set p t x = make_magic (make_AppL [make_Varl ["Array"; "unsafe_set"]; make_magic t; Const (Int p); make_magic x])

let make_array_unsafe_get_no_magic p t = make_AppL [make_Varl ["Array"; "unsafe_get"]; t; Const (Int p)]
let make_array_unsafe_set_no_magic p t x = make_AppL [make_Varl ["Array"; "unsafe_set"]; t; Const (Int p); x]

(** Build [assert exp]*)
let make_assert b = Assert b

(** Build [assert false]*)
let make_assert_false = Assert (Const (Bool false))

(** Build [a = b]*)
let make_equals a b = App (App (Var (Pated ([Ident.source "="], true)), a), b)

(** Build [a == b] *)
let physical_equality a b = App (App (Var (Pated ([Ident.source "=="], true)), a), b)

(**
   Abbreviations for commonly used expressions.
*)
module Cons =
struct
  let simple_param_effec s = Pated ([s], false)
  let param_effec sl = Pated (sl, false) (* FIXME : multiple definition *)

  let var id = Var (Pated ([id], false))

  let param s =
    let pat = PatVar s in
    let param = Pat pat in
    param

  let param_var s =
    let pat = PatVar s in
    let param = Pat pat in
    let var = var s in
    param, var

  let pat_var s =
    let pat = PatVar s in
    let var = var s in
    pat, var

  let param_pat_var s =
    let pat = PatVar s in
    let param = Pat pat in
    let var = var s in
    param, pat, var

  let app a b = App (a, b)
  let app2 = app
  let app3 a b c = app (app a b) c
  let app4 a b c d = app (app3 a b c) d
  let app5 a b c d e = app (app4 a b c d) e

  let rec app_list = function
    | [] -> failwith "[ ocaml.ml; #83726 ]"
    | [x] -> x
    | [x; y] -> app x y
    | ls ->
        let xs, x = List.extract_last ls in (*TODO: This looks like an anti-pattern for [List.fold_right]*)
        app (app_list xs) x

  let letin id c1 c2 = Letin ([id, c1], c2)
  let letrec pel = Letrec pel
  let letrecin pfe e = Letrecin (pfe, e)

  let var_of_string s = Var (Pated ([s], false))

  let array l = AnArray l

  let lambda s f = Abs ([make_param_formel s], f)

  let int i = Const (Int i)
  let float f = Const (Float f)
  let char c = Const (Char c)
  let unit = Const (Unit)
  let string s = Const (String s)
  let bool b = Const (Bool b)
  let true_ = bool true
  let false_ = bool false

  let none = Constructor ([Ident.source "None"], [])
  let some e = Constructor ([Ident.source "Some"], [e])
  let tuple e = Tuple e

  let rec list = function
    | x::xs -> Cons (x, list xs)
    | [] -> EmptyList

  let pat_none = PatConstructor ([Ident.source "None"], [])
  let pat_some p = PatConstructor ([Ident.source "Some"], [p])
  let pat_tuple t = PatTuple t
  let pat_unit = PatConst Unit
  let pat_int i = PatConst (Int i)
  let pat_float f = PatConst (Float f)
  let pat_string s = PatConst (String s)
  let pat_char c = PatConst (Char c)

  let make_match e pel = Match (e, pel)

  let comment c s = Comments (c, s)
  let verbatim c = Verbatim c

  let plus a b = app3 (make_Var "+") a b
  let minus a b = app3 (make_Var "-") a b
  let equal a b = app3 (make_Var "=") a b
  let neq a b = app3 (make_Var "<>") a b
  let band a b = app3 (make_Var "&&") a b
  let bor a b = app3 (make_Var "||") a b
  let gt a b = app3 (make_Var ">") a b
  let lt a b = app3 (make_Var "<") a b
  let ge a b = app3 (make_Var ">=") a b
  let le a b = app3 (make_Var "<=") a b

   (* transforms [a; b; c] into [a -> b -> c] *)
  let rec type_arrows = function
    | [] | [_] -> failwith "[ ocaml.ml; #58253 ] impossible case in type_arrows"
    | [x; y] -> TypeArrow (x, y)
    | x::xs -> TypeArrow (x, type_arrows xs)

   (* transforms a list [a; b; c] into a Sequence (a, Sequence (b, ...)) *)
  let rec sequence = function
    | [] -> unit
    | [x] -> x
    | x::xs -> Sequence (x, sequence xs)

  module VarShortCut =
  struct
    let assert_ = make_Var "assert"
    let magic = make_Varl ["Obj"; "magic"]
    let magic_fun = make_Varl ["Base"; "magic_fun"]
    let false_ = Const (Bool false)
  end

  module AppShortCut =
  struct

    let magic a = app VarShortCut.magic a
    let magic_fun a = app VarShortCut.magic_fun a
    let assert_false = app VarShortCut.assert_ VarShortCut.false_

  end

  let magic_array l = array (List.map AppShortCut.magic l)

  module Pattern = (* TODO: remove? too long prefix, "pat_" is better *)
  struct
    let int i = PatConst (Int i)
    let float f = PatConst (Float f)
    let string s = PatConst (String s)
    let char c = PatConst (Char c)
    let unit = PatConst (Unit)
    let array l = PatArray l
    let pvar s = PatVar s
    let any = PatAny
  end
end
