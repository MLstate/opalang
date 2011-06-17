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

module Q = QmlAst
module List = BaseList

let process_code_gen ~except annotmap code =
  QmlAstWalk.CodeExpr.fold_map
    (QmlAstWalk.Expr.self_traverse_foldmap
       (fun self tra annotmap e ->
          match e with
          | Q.Directive (_, (#Q.type_directive as variant), _, _) when except variant ->
              tra annotmap e
          | Q.Coerce (label,e,_)
          | Q.Directive (label, #Q.type_directive, [e], _) ->
              assert (QmlAnnotMap.find_tsc_inst_opt_label label annotmap = None);
              let tsc_gen_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
              assert (QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap = None);
              let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr e) tsc_gen_opt annotmap in
              self annotmap e
          | Q.Directive (_, #Q.type_directive, _, _) ->
              assert false
          | Q.Match (label,e2,pel) ->
              let pel' =
                List.map_stable
                  (fun ((p,e) as c) ->
                     let p' =
                       QmlAstWalk.Pattern.self_traverse_map
                         (fun self tra p ->
                            match p with
                            | Q.PatCoerce (_,p,_) -> self p
                            | _ -> tra p
                         ) p in
                     if p == p' then c else (p',e)
                  ) pel in
              let e =
                if pel == pel' then e
                else Q.Match (label,e2,pel') in
              tra annotmap e
          | _ -> tra annotmap e
       )
    ) annotmap code

let process_code_after_typer annotmap code =
  process_code_gen
    ~except:(function
             | `module_ | `warncoerce | `unsafe_cast -> true
             | _ -> false)
    (* not removing these directives, they are used by
     * undot, warncoerce, and codingDirectives
     * for @unsafe_cast, i don't know which type should by kept
     * and i am not sure that keeping the outer type will not break ei *)
    annotmap code

let process_code_after_ei annotmap code =
  process_code_gen
    ~except:(fun _ -> false)
    annotmap code

