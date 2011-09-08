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
(**
   @deprecated Use OpaWalk instead
*)

(* TODO remove *)
open SurfaceAst
open SurfaceAstHelper

let (|>) = InfixOperator.(|>)
let (@*) = InfixOperator.(@*)

let ident a = a.ident

(* FIXME: add some signatures *)

let unannot sub (v,annot) =
  let unsub, l = sub v in
    (fun l -> (unsub l, annot)), l

(**
   Traversal functions on patterns
*)

module SPat = struct
  open Traverse.Utils
  type 'a t = 'b pat constraint 'a = 'b * 'c * 'd

  let sub_p = sub_current
  let sub_ident x = sub_ignore x
  let sub_pat_record_node pr = sub_list (sub_2 sub_ignore sub_p) pr
  let sub_pat_record pr = unannot sub_pat_record_node pr

  let sub_pat_node = function
    | PatRecord (pr, rowvar) -> (
        match rowvar with
        | `closed ->
            wrap patrecord (sub_pat_record_node pr)
        | `open_ ->
            wrap patextendrecord (sub_pat_record_node pr)
      )
    | (PatAny | PatConst _ | PatVar _) as v -> sub_ignore v
    | PatCoerce (p,ty) -> wrap patcoerce (sub_2 sub_p sub_ignore (p,ty))
    | PatAs (p, i) -> wrap patas (sub_2 sub_p sub_ignore (p,i))

  let sub_pat c = unannot sub_pat_node c
  let subs_cons x = sub_pat x

  (* only goes through opa top level patterns *)
  let sub_code_elt_node = function
    | NewVal (pel,b) -> wrap newval (sub_2 (sub_list (sub_2 sub_p sub_ignore)) sub_ignore (pel,b))
    | v -> sub_ignore v

  let sub_code_elt c = unannot sub_code_elt_node c
  let sub_code c = sub_list sub_code_elt c
end

module PatTraverse =
  Traverse.Make(SPat)

module Pattern =
struct
  let get_vars_gen add empty p =
    PatTraverse.fold (fun acc -> function
                        | (PatVar a, _)
                        | (PatAs (_,a), _) -> add (ident a) acc
                        | _ -> acc
                     ) empty p
  (* TODO: one functor applied to string and exprident? *)
  let get_vars_string p = get_vars_gen StringSet.add StringSet.empty p
  let get_vars_ident p = get_vars_gen IdentSet.add IdentSet.empty p
  let get_vars_ident' p = p |> get_vars_ident |> IdentSet.elements
  let get_vars_code l =
    PatTraverse.lift_fold SPat.sub_code
      (fun acc -> function
         | (PatVar a, _)
         | (PatAs (_,a), _) -> IdentSet.add (ident a) acc
         | _ -> acc
      ) IdentSet.empty l
  (* FIXME: we can have duplicates *)
  (* FIXME: almost the same function is defined several times already! *)
  let get_var_list sub l =
    (* using fold_right to keep the order *)
    PatTraverse.lift_fold_right_down sub
      (fun x acc ->
         match x with
           | (PatVar a, _)
           | (PatAs (_,a), _) -> (ident a) :: acc
           | _ -> acc
      ) l []
  let get_var_list_pattern l = get_var_list SPat.sub_p l
  let get_var_list_code_elt_node l =
    get_var_list SPat.sub_code_elt_node l
  let get_var_list_code l =
    get_var_list SPat.sub_code l
end

(**
    Traversal functions on 'expr'
*)

module SExpr =
struct
  open Traverse.Utils
  type 'a t = ('b, 'c) expr constraint 'a = 'b * 'c * _

  let sub_e e = sub_current e
  let sub_ident x = sub_ignore x
  let sub_ty x = sub_ignore x
  let sub_record_node l = sub_list (sub_2 sub_ignore sub_e) l
  let sub_record r = unannot sub_record_node r
  let sub_pattern x = sub_ignore x
  let sub_db_elt = function
    | FldKey _
    | NewKey as v -> sub_ignore v
    | ExprKey e -> wrap (fun x -> ExprKey x) (sub_e e)

  let sub_db_def x = QmlAst.Db.sub_db_def sub_e sub_ty x


  (* this part does not depend on the type of identifiers, and so can be used by renaming
   * for uninteresting cases *)
  let sub_expr_no_ident = function
    | Const _ as e -> sub_ignore e
    | Apply  (e, r) -> wrap apply (sub_2 sub_e sub_record (e,r))
    | Record r -> wrap record (sub_record_node r)
    | ExtendRecord (r,e) -> wrap extendrecord (sub_2 sub_record_node sub_e (r,e))
    | Dot (e,s) -> wrap dot (sub_2 sub_e sub_ignore (e,s))
    | Bypass b -> wrap bypass (sub_ignore b)
    | DBPath (a,b) -> wrap dbpath (sub_2 (unannot (sub_list (unannot sub_db_elt))) sub_ignore (a,b))
    | _ -> assert false
  let sub_expr_node' fd = function
    | Ident _ as e -> sub_ignore e
    | Lambda (p, e) -> wrap lambda (sub_2 sub_ignore sub_e (p,e))
    | LetIn (b,iel,e) -> wrap letin (sub_3 sub_ignore (sub_list (sub_2 sub_ident sub_e)) sub_e (b,iel,e))
    | Match (e, pel) -> wrap match_ (sub_2 sub_e (sub_list (sub_2 sub_pattern sub_e)) (e,pel))
    | Directive (a,el,t) -> wrap directive (sub_3 sub_ignore (sub_list sub_e) sub_ignore (fd a,el,t))
    | e -> sub_expr_no_ident e

  let sub_expr' fd x = unannot (sub_expr_node' fd) x
  let sub_expr_node e = sub_expr_node' Base.identity e
  let sub_expr x = unannot sub_expr_node x


  let subs_cons x = sub_expr x


  (* unbuild/rebuild code into expressions *)
  let sub_code_elt_node = function
    | (Database _ | NewType _ | Package _) as e ->
        sub_ignore e
    | NewDbDef dbdef ->
        wrap newdbdef (sub_db_def dbdef)
    | NewVal (pel,b) ->
        wrap newval (sub_2 (sub_list (sub_2 sub_ignore sub_e)) sub_ignore (pel,b))

  let sub_code_elt c = unannot sub_code_elt_node c
  let sub_code c = sub_list sub_code_elt c
end

module ExprTraverse =
  struct
    include Traverse.Make(SExpr)

    (*
     * Functions in this module can map on an expression
     * while changing its type
     * it is supposed to be used by passes that remove directives
     * (so that the type is changed accordingly)
     *)
    module Heterogeneous =
      struct
        let map_down fd f e =
          let rec aux e =
            let build, l = SExpr.sub_expr' fd (f e) in
            let l = List.map aux l in
            build l in
          aux e
        let lift_map_down fd f code =
          let build, l = SExpr.sub_code code in
          let l = List.map (fun e -> map_down fd f e) l in
          build l
        let map_down_to_fixpoint fd f e =
          let rec aux e =
            let e' = f e in
            if e' == e then
              let build, l = SExpr.sub_expr' fd e in
              let l = List.map aux l in
              build l
            else
              aux e' in
          aux e
        let lift_map_down_to_fixpoint fd f code =
          let build, l = SExpr.sub_code code in
          let l = List.map (fun e -> map_down_to_fixpoint fd f e) l in
          build l
        let foldmap_down fd f acc e =
          let rec aux acc e =
            let acc, e = f acc e in
            let build, l = SExpr.sub_expr' fd e in
            let acc, l = List.fold_left_map aux acc l in
            acc, build l in
          aux acc e
        let foldmap = foldmap_down

        let traverse_foldmap fd f acc e =
          let rec tra acc e =
            let build, l = SExpr.sub_expr' fd e in
            let acc, l = List.fold_left_map (f tra) acc l in
            acc, build l
          in f tra acc e

      end
    let lift_map_down = lift_map_down
  end

module Expr =
struct
  let appears equal  i e =
    ExprTraverse.exists
      (function
         | (Ident i', _) when equal i i' -> true
         | _ -> false
      ) e
  let get_code_exprs e =
    snd (SExpr.sub_code e)

  (* FIXME: really dirty *)
  let fold_with_env (add_env:'b->Ident.t->'a option->'b) env f acc expr =
    let get_name_and_expr_if_local = function
      | (field, (Directive (`local name, [e], _), _label)) -> Some (field, name, e)
      | (_, (Directive (`local _, _, _), _)) -> assert false
      | _ -> None in
    let get_local_name_and_expr fe =
      Option.get (get_name_and_expr_if_local fe) in
    let add_record r env =
      match r with
        | [] -> None
        | h :: _ ->
            match get_name_and_expr_if_local h with
              | None ->
                  (* case after dependency analysis *)
                  assert (List.for_all (Option.is_none @* get_name_and_expr_if_local) r);
                  None
              | Some _ ->
                  (* before and while dependency analysis *)
                  Some (
                    List.fold_right
                      (fun fe (env,r) ->
                         let (field,name,e) = get_local_name_and_expr fe in
                           (add_env env name (Some e), (field,e) :: r))
                      r
                      (env,[])
                  ) in
    let add_bnd env (n,e) =
      add_env env n (Some e) in
    (* FIXME: use the generic function to find vars in patterns *)
    let add_lambda r env =
      PatTraverse.lift_fold SPat.sub_pat_record_node
        (fun env ->
           function
             | (PatVar a, _)
             | (PatAs (_,a), _) -> add_env env (ident a) None (* could say Some... *)
             | _ -> env) env r in
    let add_pat pat env =
      PatTraverse.fold
        (fun env ->
           function
             | (PatVar a, _)
             | (PatAs (_,a), _) -> add_env env (ident a) None (* could say Some... *)
             | _ -> env) env pat in
    let rec process_pattern_expr tra env acc (pat,expr) =
      let env_bnd = add_pat pat env in
      process_expr tra env_bnd acc expr
    and process_expr tra env acc expr =
      let env, acc = f env acc expr in
        match fst expr with
          | Lambda (r, expr) ->
              process_expr tra (add_lambda r env) acc expr
          | Record r ->
              ( match add_record r env with
                  | Some (env, r) ->
                      process_expr tra env acc (Record r, snd expr)
                  | None ->
                      tra env acc expr )
          | LetIn (rec_, bnd, expr) ->
              let full_env = List.fold_left add_bnd env bnd in
              let local_env = if rec_ then full_env else env in
              let acc =
                List.fold_left (fun acc (_,expr) -> process_expr tra local_env acc expr) acc bnd in
              process_expr tra full_env acc expr
          | Match (expr, pel) ->
              let acc = process_expr tra env acc expr in
              List.fold_left (process_pattern_expr tra env) acc pel
          | _ -> tra env acc expr in
    ExprTraverse.traverse_fold_context_down process_expr env acc expr

  let wrap f =
    (fun env acc expr -> env, f env acc expr)

  let fold_with_expr_map ?(env = IdentMap.empty) f acc expr =
    fold_with_env (fun map id optval -> IdentMap.add id optval map) env (wrap f) acc expr

  let traverse_fold_with_set ?(env = IdentSet.empty) f acc expr =
    fold_with_env (fun map id _optval -> IdentSet.add id map) env f acc expr

  let fold_with_set ?env f acc expr =
    traverse_fold_with_set ?env (wrap f) acc expr

  let get_vars_gen add empty p =
    ExprTraverse.fold (fun acc -> function
                        | (Ident a, _) -> add a acc
                        | _ -> acc
                     ) empty p
  (* TODO: one functor applied to string and exprident? *)
  let get_vars_stringset p = get_vars_gen StringSet.add StringSet.empty p
  let get_vars_identset p = get_vars_gen IdentSet.add IdentSet.empty p
  let get_vars_identlist p = p |> get_vars_identset |> IdentSet.elements
end


module Code =
struct
  let get_pattern_expr code =
    List.concat_map (function
                    | (NewVal (pel,_), _) -> pel
                    | _ -> []
                 ) code

  let map_up f code = ExprTraverse.lift_map_up SExpr.sub_code f code
end

(**
   General purpose traversal functions on types
*)

module SType =
struct
  open Traverse.Utils
  type 'a t = 'b ty constraint 'a = 'b * 'c * 'd
  let sub_t = sub_current
  let sub_fields x = sub_list (sub_2 sub_ignore sub_t) x
  let sub_row_t_node (TyRow (fields,rowvar)) =
    wrap tyrow (sub_2 sub_fields sub_ignore (fields,rowvar))
  let sub_row_t v = unannot sub_row_t_node v
  let sub_arrow_t_node (row_t,ty) =
    sub_2 sub_row_t sub_t (row_t,ty)
  let sub_arrow_t v = unannot sub_arrow_t_node v
  let sub_typeinstance_node (ident,tyl) =
    sub_2 sub_ignore (sub_list sub_t) (ident,tyl)
  let sub_sum_t_node = function
    | SumName ti -> wrap sumname (sub_typeinstance_node ti)
    | SumRecord row_t -> wrap sumrecord (sub_row_t_node row_t)
    | SumVar _ as v -> sub_ignore v
  let sub_sum_t v = unannot sub_sum_t_node v
  let sub_ty_node = function
    | TypeConst _
    | TypeVar _
    | TypeExternal as v -> sub_ignore v
    | TypeArrow r -> wrap typearrow (sub_arrow_t_node r)
    | TypeRecord r -> wrap typerecord (sub_row_t_node r)
    | TypeSumSugar l -> wrap typesumsugar (sub_list sub_sum_t l)
    | TypeNamed ti -> wrap typenamed (sub_typeinstance_node ti)
    | TypeForall (vars, t) -> wrap typeforall (sub_2 sub_ignore sub_t (vars, t))
    | TypeModule fields -> wrap typemodule (sub_fields fields)
  let sub_ty ty = unannot sub_ty_node ty

  let subs_cons = sub_ty
end

module TypeTraverse =
  Traverse.Make(SType)

module Type =
struct
  (* FIXME: we can have duplicates *)
  let get_typename_list sub l =
    TypeTraverse.lift_fold_right_down sub
      (fun x acc ->
         match x with
           | (TypeNamed (Typeident ident, _tyl), _) -> ident :: acc
           | _ -> acc
      ) l []
  let get_typename_list_arrow_t l =
    get_typename_list SType.sub_arrow_t l
  let get_typename_list_type l =
    get_typename_list SType.sub_t l
end
