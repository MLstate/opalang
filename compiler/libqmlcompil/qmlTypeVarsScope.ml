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


(* Typevar *)


(* type typevarsscope_elt = Type of TypeVar.t | Row of RowVar.t | Col of ColVar.t *)

(* module Arg = *)
(* struct *)
(*   type elt = Type of TypeVar.t | Row of RowVar.t | Col of ColVar.t *)

(* (\* TypeVarHash --> Hash basé global stamp (global stamp, local, name, descr) *\) *)

(*   module HashVarstlb = Hashtbl.Make( *)
(*     struct *)
(*       type t = elt *)
(*       let equal x y = *)
(*         match x,y with *)
(*         | Type t, Type t' -> TypeVar.equal t t' *)
(*         | Row r, Row r' -> RowVar.equal r r' *)
(*         | Col c, Col c' -> ColVar.equal c c' *)
(*         | _ -> false *)
(*       let hash x = *)
(*         (\* TODO: a better hash based on stamp *\) *)
(*         match x with *)
(*         | Type _ *)
(*         | Row _ *)
(*         | Col _ -> Hashtbl.hash x *)
(*     end *)
(*   ) *)

(*   type 'a block = 'a HashVarstlb.t *)


(*   let create n = HashVarstlb.create n *)

(*   let fold f = HashVarstlb.fold f *)

(*   let bind b e v = HashVarstlb.add b e v *)
(*   let unbind b e = HashVarstlb.remove b e *)

(*   let find b e = *)
(*     try *)
(*       Some (HashVarstlb.find b e) *)
(*     with *)
(*     | Not_found -> None *)

(* end *)


(* module TypeVarsScope = ImperativeScope.Make(Arg) *)


module TypeVarsScope(Arg : sig type id end) =
struct
  type ident = Arg.id
  type ty_elt = ETy of ident | ERow of ident | ECol of ident
  module IdentScope = ImperativeScope.Default(struct type elt = ty_elt end)
  type ty_vars = VTy of QmlAst.TypeVar.t | VRow of QmlAst.RowVar.t | VCol of QmlAst.ColVar.t

  let create n = IdentScope.create n
  let reset s = IdentScope.reset s

  let bind_typevar s e v = IdentScope.bind s (ETy e) (VTy v)
  let bind_rowvar s e v = IdentScope.bind s (ERow e) (VRow v)
  let bind_colvar s e v = IdentScope.bind s (ECol e) (VCol v)

  let add_local_scope s = IdentScope.push s
  let remove_local_scope s = IdentScope.pop s

  let get_local_vars s =
    let init_acc = ([], [], []) in
    let fold_fun _e v (acc_ty, acc_row, acc_col) =
      match v with
      | VTy v -> (v::acc_ty, acc_row, acc_col)
      | VRow v -> (acc_ty, v::acc_row, acc_col)
      | VCol v -> (acc_ty, acc_row, v::acc_col)
    in
    IdentScope.fold fold_fun s init_acc
  
  let find_typevar_opt s ident =
    match IdentScope.find_opt s (ETy ident) with
    | Some(VTy v) -> Some v
    | _ -> None

  let find_rowvar_opt s ident =
    match IdentScope.find_opt s (ERow ident) with
    | Some(VRow v) -> Some v
    | _ -> None

  let find_colvar_opt s ident =
    match IdentScope.find_opt s (ECol ident) with
    | Some(VCol v) -> Some v
    | _ -> None

end


(* TODO: *)

(* 1) fresh.ml, vérifier l'histoire du quadruplet *)
(* 1.1) créer FreshHash à partir HashMake de caml, spécialisé pour les fresh basé sur global stam *)
(* 2) creer TypeVarHash en utilisant FreshHash (pareil pour Col, Row) *)
(* 3) utilser TypeVarHash ici au lieu de Hash *)
(* 4) inserer le scope dans opa2qml *)
(* 5) tester dasn qmltop *)
