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
(* depends *)
module Format = BaseFormat
module List = BaseList

(* shorthands *)
module Q = QmlAst

(* -- *)

type info = { strict : bool  ; specialize : (Q.ty * Q.expr) list }
type env = info IdentMap.t


let (@) info1 info2 =
  let strict = info1.strict || info2.strict in
  let specialize = info1.specialize @ info2.specialize in
  {
    strict ;
    specialize ;
  }

let fold_expr f acc env =
  IdentMap.fold (fun _ info acc ->
    List.fold_left (fun acc (_,e) -> f acc e) acc info.specialize
  ) env acc

let fold_map_expr f acc env =
  IdentMap.fold_map (
    fun _ info acc ->
      let acc, specialize =
        List.fold_left_map (
          fun acc (ty,e) ->
            let acc, e = f acc e in
            acc, (ty, e)
        ) acc info.specialize
      in acc, {
        info with
          specialize
      }
  ) env acc

let map_type f env =
  IdentMap.map
    (fun info ->
       let specialize =
         List.map
           (fun (ty,e) ->
              (f ty, e))
           info.specialize
       in
       {
         info with
           specialize ;
       }
    ) env

module S =
struct
  type t = Q.annotmap * env
  let pass = "pass_SimplifyMagic"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R =
struct
  include ObjectFiles.Make(S)
  let load annotmap (env:env) : S.t =
    fold_with_name
      (fun package (annotmap,(env:env)) (annotmap_old,old_env) ->
         let annotmap_old = QmlRefresh.refresh_annotmap package annotmap_old in
         let annotmap, old_env = fold_map_expr (QmlRefresh.refresh_expr package ~annotmap_old) annotmap old_env in
         let old_env = map_type (QmlRefresh.refresh_typevars_from_ty package) old_env in
         let env = IdentMap.merge (@) env old_env in
         annotmap, env
      ) (annotmap,env)
  let save annotmap env =
    let small_annotmap = QmlRefresh.restrict_annotmap_fold_expr fold_expr annotmap env in
    save (small_annotmap,env)
end

let is_monomorphic ty =
  not (
    QmlAstWalk.Type.exists
      (function
       | Q.TypeVar _ -> true (* FIXME: actually, we should check for rowvars and colvars also *)
       | _ -> false) ty
  )

let build_env env gamma annotmap code =
  let _, code as result =
    QmlAstWalk.CodeExpr.fold_map_name_expr
      (fun (env,annotmap) (ident,expr) ->
         match expr with
         | Q.Directive (_, `specialize variant, inner_expr :: l, _) ->
             (* we refuse polymorphic types for now
              * or else we might create troubles with ei *)
             let general_type = QmlAnnotMap.find_ty (Q.QAnnot.expr inner_expr) annotmap in
             let l =
               let specialize = List.map (fun e ->
                                 let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap in
                                 if not (is_monomorphic ty) then
                                   OManager.serror "%a@\n  This expression shouldn't contain type variables@."
                                     FilePos.pp_pos (Q.Pos.expr e)
                                 else (
                                   (* should check that ty is an instance of general_type
                                    * but since ty is monomorphic, checking the unifiability is equivalent *)
                                   if not (QmlMoreTypes.unifiable ~gamma general_type ty) then
                                     OManager.serror "%a@\n  This expression's type should be an instance of the generic expression's type.@."
                                       FilePos.pp_pos (Q.Pos.expr e)
                                 );
                                 ty, e) l in
               let strict = variant = `strict in
               {
                 strict ;
                 specialize ;
               }
             in
             let env = IdentMap.update_default ident ((@) l) l env in
             let tsc = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr expr) annotmap in
             let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr inner_expr) tsc annotmap in
             (env,annotmap), (ident, inner_expr)
         | _ ->
             (env,annotmap), (ident, expr))
      (env,annotmap) code in
  QmlAstWalk.CodeExpr.iter
    (QmlAstWalk.Expr.iter
       (function
        | Q.Directive (label, `specialize _, _, _) ->
          OManager.serror "%a@\n  Illegal @@specialize: it can only be the topmost directive on a toplevel binding.@."
            FilePos.pp_pos (Annot.pos label)
        | _ -> ()
       )
    ) code;
  result

let rewrite_expr env gamma annotmap code =
  let rec aux tra annotmap e =
    match e with
    | Q.Ident (label, i) ->
        let annot = Annot.annot label in
        (try
           let info = IdentMap.find i env in
           let choices = info.specialize in
           let ty = QmlAnnotMap.find_ty annot annotmap in
           try let _, expr = List.find (fun (ty',_) -> QmlMoreTypes.equal_ty ~gamma ty ty') choices in
               let annotmap, expr = QmlAstCons.TypedExpr.copy annotmap expr in
               aux tra annotmap expr
           with Not_found ->
             let fail () =
               QmlPrint.pp#reset_typevars;
               let context = QmlError.Context.label label in
               QmlError.error context (
                 "Failed specialization on %s with type %a@\n"^^
                 "@[<2>@{<bright>Hint@}:@\n"^^
                  "Add a type annotation for a specialization in one of the following types:@\n"^^
                  "%a"^^
                 "@]"
               )
                 (Ident.original_name i) QmlPrint.pp#ty ty
                 (Format.pp_list "@\n" (Format.pp_fst QmlPrint.pp#ty)) choices
             in
             if info.strict
             then
               fail ()
             else (
               #<If:SIMPLIFYMAGIC_FAILURES>
                 fail ()
               #<End> ;
             ) ;
             tra annotmap e
         with Not_found -> tra annotmap e)
     | _ -> tra annotmap e in
  QmlAstWalk.Expr.traverse_foldmap aux annotmap code

let empty_env = IdentMap.empty
let process_code ?(specialized_env=empty_env) gamma annotmap code =
  let (env,annotmap), code = build_env specialized_env gamma annotmap code in
  R.save annotmap env;
  #<If:SIMPLIFYMAGIC_DISABLE>
    annotmap, code
  #<Else>
    let annotmap, env2 = R.load annotmap empty_env in
    let env = IdentMap.merge (@) env2 env in (* the old env has priority over the current one
                                              * so that Date.to_string doesn't override intToString
                                              * in the package date *)
    QmlAstWalk.CodeExpr.fold_map (rewrite_expr env gamma) annotmap code
  #<End>
