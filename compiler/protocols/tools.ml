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
module B = Base
module O = Ocaml
module G = Grammar
module S = B.String
module L = B.List

exception Operation_after_statecall of string
exception NestedSubPatterns
exception LiteralRequired

let (><) = Option.map

let is_ident = function
  | G.Ident _ -> true
  | _ -> false

let consttype_to_string = function
  | O.TypeBool -> "bool"
  | O.TypeInt64 -> "int64"
  | O.TypeInt -> "int"
  | O.TypeFloat -> "float"
  | O.TypeUnit -> "unit"
  | _ -> "string"

let type_to_str = function
  | O.TypeConst a -> consttype_to_string a
  | O.TypeName (_,["int64"]) -> "int64"
  | O.TypeName (_,["bool"]) -> "bool"
  | t ->
      prerr_string "Tools.type_to_str: Unknown type="; OcamlPrint.Output.type_expr stderr t; prerr_newline ();
      assert false

let prefix p i = p ^ (string_of_int i)

let bruijnise (n,lst) = function
  | G.Ident s | G.SubPattern (s,_,_) -> succ n, (s, succ n) :: lst
  | G.Prevent _ -> n, lst
  | _ -> succ n, lst

let get_patconstr name tuple_list =
  O.PatConstructor([name], L.map (fun str -> O.PatVar (fst str)) tuple_list)

let types_of_tdefs pname lst =
  let rec prefix = function
    | O.TypeVar a when a = "msg" -> O.TypeVar ((S.capitalize pname) ^ ".msg")
    | O.TypeName (lst, n) -> O.TypeName (L.map prefix lst, n)
    | O.TypeTuple lst -> O.TypeTuple (L.map prefix lst)
    (*| O.TypePair (a, b) -> O.TypePair (prefix a, prefix b)*)
    | O.TypeRecord rl -> O.TypeRecord (L.map (fun (b,n,t) -> b, n, prefix t) rl)
    | O.TypeArrow (a, b) -> O.TypeArrow (prefix a, prefix b)
    | O.TypeConstructor lst ->
        O.TypeConstructor (L.map (fun (n, o) -> n, prefix >< o) lst)
    | otherwise -> otherwise in
  let aux = function
    | G.MType (a, b) -> O.Type [[], a, prefix b]
    | G.MVal (a, b) -> O.Val (Ident.source a, prefix b)
    | _ -> assert false
  in L.map aux lst

let tuple_of_var = function
  | G.GVar (a, b) -> a,b
  | _ -> assert false

let val_of_import = function
  | G.Import (G.GVar (n, t)) -> O.Val (Ident.source n, t)
  | _ -> assert false

let let_of_set = function
  | G.Set (G.GVar (n, _), value) -> O.make_Let (O.Pat (O.PatVar (Ident.source n))) (O.Verbatim value)
  | _ -> assert false


let add_suffix ?(force_split=false) name suffix =
  let split(us)=
    if suffix = "" then ""
    else if force_split
    then "_"
    else if us = 0
    then ""
    else "_" in
  match S.fold (fun (uc,lc,us) c ->
                       match c with
                       | 'a'..'z' -> (uc,lc+1,us)
                       | 'A'..'Z' -> (uc+1,lc,us)
                       | '_' -> (uc,lc,us+1)
                       | _ -> (uc,lc,us)) (0,0,0) name with
  | (0,0,0) -> name^suffix
  | (uc,0,us) -> name^(split(us))^(S.uppercase suffix)
  | (0,lc,us) -> name^(split(us))^(S.lowercase suffix)
  | (uc,lc,us) -> name^(split(us))^(S.capitalize suffix)

let add_prefix ?(force_split=false) name prefix =
  let split(us)=
    if prefix = "" then ""
    else if force_split
    then "_"
    else if us = 0
    then ""
    else "_" in
  match S.fold (fun (uc,lc,us) c ->
                       match c with
                       | 'a'..'z' -> (uc,lc+1,us)
                       | 'A'..'Z' -> (uc+1,lc,us)
                       | '_' -> (uc,lc,us+1)
                       | _ -> (uc,lc,us)) (0,0,0) name with
  | (0,0,0) -> prefix^name
  | (uc,0,us) -> (S.uppercase prefix)^(split(us))^name
  | (0,lc,us) -> (S.lowercase prefix)^(split(us))^name
  | (uc,lc,us) -> (S.capitalize prefix)^(split(us))^name

let str_of_type_expr te = let b = Buffer.create 1024 in (OcamlPrint.Buf.type_expr b te; Buffer.contents b)

let str_of_expr e = let b = Buffer.create 1024 in (OcamlPrint.Buf.expr b e; Buffer.contents b)
