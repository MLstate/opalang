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
(* CF mli *)

module BSLDbGen = QmlDbGen.DbGenByPass.BSLDbGenAlphaOpa
module DbGen = QmlDbGen.DbGen ( BSLDbGen )

let process_code gamma annotmap schema code dbinfo alpha_opt =
  (* previous passes should not generate code with db paths at pre_linking *)
  if ObjectFiles.compilation_mode() = `init
  then (gamma, annotmap, alpha_opt, code)
  else
    let nodb = QmlDbGen.Schema.is_empty schema in

    let gamma, annotmap, code =
      if nodb then
        (gamma, annotmap, code)
      else begin
        let annotmap = annotmap in

        (* 3°: expanding database accesses *)
        let gamma, annotmap, code =
          let annotmap_opt, code, gamma =
            DbGen.replace_path_ast
              schema dbinfo gamma ~annotmap:(Some annotmap) ~valinitial_env:alpha_opt
              code in
          let annotmap =
            Option.default_map annotmap (fun am -> QmlAnnotMap.merge (QmlTypes.process_annotmap ~gamma:gamma am) annotmap) annotmap_opt in
          gamma, annotmap, code
        in

        (gamma, annotmap, code)
      end
    in

    (gamma, annotmap, alpha_opt, code)
