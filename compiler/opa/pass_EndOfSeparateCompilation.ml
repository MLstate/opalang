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
module P = Passes
module Q = QmlAst

module S =
struct
  type t = {
    pesc_code : QmlAst.code;
    pesc_doc_types : Ident.t P.doc_types;
    pesc_annotmap : QmlAst.annotmap;
    pesc_gamma : QmlTypes.gamma;
    pesc_schema : QmlDbGen.Schema.t;
  }
  let pass = ObjectFiles.last_pass
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module R = ObjectFiles.Make(S)

let process_code :
    'tmp_env Passes.env_Gen ->
    ('tmp_env Passes.env_Gen -> unit) ->
    unit =
  fun env k ->
  let initial =
    { S.pesc_doc_types = []
    ; S.pesc_code = []
    ; S.pesc_annotmap = QmlAnnotMap.empty
    ; S.pesc_gamma = QmlTypes.Env.empty
    ; S.pesc_schema = QmlDbGen.Schema.initial } in
  let merge_code_annotmap ?package (code1,annotmap1,s1) (code2,annotmap2,s2) =
    let annotmap, code2, s2 =
      match package with
      | Some package ->
          let code2 = QmlRefresh.refresh_typevars_from_code package code2 in
          let annotmap2 = QmlRefresh.refresh_annotmap package annotmap2 in
          let annotmap, code2 =
            QmlAstCons.TypedCode.copy_new_when_possible
              ~annotmap_old:annotmap2 annotmap1 code2 in
          let annotmap, s2 =
            QmlRefresh.refresh_schema2 package ~refreshed_annotmap_old:annotmap2
              annotmap s2 in
          annotmap, code2, s2
      | None ->
          QmlAnnotMap.merge annotmap1 annotmap2, code2, s2 in
    (code1 @ code2, annotmap, QmlDbGen.Schema.merge s1 s2) in
  let merge_gamma ?package gamma1 gamma2 =
    let gamma2 =
      match package with
      | Some package -> QmlRefresh.refresh_gamma package gamma2
      | None -> gamma2 in
    QmlTypes.Env.append gamma1 gamma2 in
  let merge_doc = (@) in
  let merge ?package
      {S.pesc_code=code1; S.pesc_doc_types=doc1;
       S.pesc_annotmap=annotmap1;
       S.pesc_gamma=gamma1; S.pesc_schema = schema1}
      {S.pesc_code=code2; S.pesc_doc_types=doc2;
       S.pesc_annotmap=annotmap2;
       S.pesc_gamma=gamma2; S.pesc_schema = schema2} =
    let code,annotmap,schema = merge_code_annotmap ?package (code1,annotmap1,schema1) (code2,annotmap2,schema2) in
    {S.pesc_code = code;
     S.pesc_doc_types = merge_doc doc1 doc2;
     S.pesc_annotmap = annotmap;
     S.pesc_gamma = merge_gamma ?package gamma1 gamma2;
     S.pesc_schema = schema;
    } in
  if ObjectFiles.Arg.is_fully_separated () then
    k env
  else (
    match ObjectFiles.compilation_mode () with
    | `init ->
        k env
    | `linking | `prelude ->
        QmlRefresh.load ();
        (*Format.printf "show:%t@." M_typ.show;*)
        let acc = R.fold_with_name ~packages:true ~deep:true (fun package -> merge ~package) initial in
        let code,annotmap,schema =
          merge_code_annotmap
            (acc.S.pesc_code,acc.S.pesc_annotmap,acc.S.pesc_schema)
            (env.P.qmlAst,env.P.typerEnv.QmlTypes.annotmap,env.P.typerEnv.QmlTypes.schema) in
        let env = {env with P.
                     doc_types = merge_doc acc.S.pesc_doc_types env.P.doc_types;
                     qmlAst = code;
                     typerEnv = {env.P.typerEnv with QmlTypes.annotmap = annotmap;
                                   QmlTypes.gamma = merge_gamma acc.S.pesc_gamma env.P.typerEnv.QmlTypes.gamma;
                                   QmlTypes.schema = schema;
                                };
                  } in
        ObjectFiles.end_of_separate_compilation ();
        QmlRefresh.clear ();
        k env
    | `compilation ->
        QmlRefresh.save ();
        let t = {S.pesc_code = env.P.qmlAst;
                 S.pesc_doc_types = env.P.doc_types;
                 S.pesc_annotmap = env.P.typerEnv.QmlTypes.annotmap;
                 S.pesc_gamma = env.P.typerEnv.QmlTypes.gamma;
                 S.pesc_schema = env.P.typerEnv.QmlTypes.schema;
                } in
        R.save t;
        ObjectFiles.compilation_is_successfull ()
  )
