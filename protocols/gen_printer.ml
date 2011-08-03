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
module B = Base
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module O = Ocaml
module Cons = O.Cons
module G = Grammar
module T = Tools
module L = B.List
module A = B.Array
module S = B.String

let tuplize n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (pred n) <| ("__" ^ (string_of_int n)) :: acc
  in S.concat "," <| aux n []

let rec rewrite_option n opt next =
  let sub = rewrite_pattern true 1 opt in
  let idents = A.of_list <| L.filter T.is_ident opt in
  let tuple = tuplize <| A.length idents in
    Printf.sprintf "^ (match __%s with None -> \"\" | Some (%s) -> \"\"%s)%s"
      (string_of_int n) tuple sub <| rewrite_pattern false (succ n) next

and rewrite_subpattern n sub next =
  Printf.sprintf " ^ (String.concat \"\" (List.map (fun (%s) -> \"\"%s) __%s))%s"
(*    (tuplize <| A.length (A.of_list <| L.filter is_ident sub)) *)
    (L.fold_left T.bruijnise (0, []) sub
      |> snd
      |> L.map ((T.prefix "__") @* snd)
      |> S.concat ","
      )
    (rewrite_pattern true 1 sub)
    (string_of_int n)
    (rewrite_pattern false (succ n) next)

and rewrite_pattern in_sub n = function
    | [] -> ""
    | G.Prevent _ :: tail -> rewrite_pattern in_sub n tail
    | G.Literal (s,ci) :: tail -> " ^ " ^ s ^ rewrite_pattern in_sub (succ n) tail
    | G.SubPattern (_,_,_) :: _ when in_sub -> raise T.NestedSubPatterns
    | G.SubPattern (_, "?", lst) :: tail -> rewrite_option n lst tail
    | G.SubPattern (_, _, lst) :: tail -> rewrite_subpattern n lst tail
    | G.Ident "string" :: tail ->
        Printf.sprintf " ^ __%s%s" (string_of_int n) <| rewrite_pattern in_sub (succ n) tail
    | G.Ident t :: tail ->
        let stroft =
          match t with
            "int64" -> "Int64.to_string"
          | _ -> Printf.sprintf "string_of_%s" t in
        Printf.sprintf " ^ (%s __%s)%s" stroft
          (string_of_int n) 
          <| rewrite_pattern in_sub (succ n) tail
    | _ -> assert false

let simplify_ident var_lst = function
  | G.Ident n ->
      let get_type = function
        | O.TypeConst t -> T.consttype_to_string t
        | O.TypeName (_,["int64"]) -> "int64"
        | O.TypeName (_,["bool"]) -> "bool"
        | _ -> failwith "Can't convert such a type to string."
      in G.Ident (get_type <| L.assoc n var_lst)
  | otherwise -> otherwise

let pattern_of_define = function
  | G.Define (G.Constr (name, lst), pattern) -> (
      try
        let var_list = L.map T.tuple_of_var lst in
        let simpl = L.map (simplify_ident var_list) pattern in
        let verbatim = rewrite_pattern false 1 simpl in
        let var_lst = snd <| L.fold_left T.bruijnise (0, []) pattern in
        let var_lst = L.map (fun x -> L.assoc (fst x) var_lst) var_list
          |> L.map (T.prefix "__")
          |> L.map (fun s -> O.PatVar (Ident.source s))
        in O.PatConstructor ([Ident.source name], var_lst), O.Verbatim ("\"\"" ^ verbatim)
      with
      | T.NestedSubPatterns ->
          let msg = Printf.sprintf
            "Error in the definition of \"%s\" : sub-patterns cannot be nested"
            name
          in failwith msg
    )
  | _ -> assert false

(* Some conversion functions. *)
let do_it lst =
  let tuple_list = L.map pattern_of_define lst in
  let func = O.Function (L.map (fun (a, b) -> a, None, b) tuple_list) in
    Ocaml.Let [O.Pat (O.PatVar (Ident.source "string_of_msg")), func]
