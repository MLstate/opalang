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
type filename = string
type contents = string

module ByPassMap = BslLib.BSL.ByPassMap
module D = BslDirectives

module List = Base.List
module String = Base.String

(* FIXME: think about where to put the function now *)
let now = DebugTracer.now

let (!?) pos fmt =
  if WarningClass.is_warn WarningClass.bsl_register
  then (
    OManager.printf "%a@\n" FilePos.pp_citation pos ;
    OManager.warning ~wclass:WarningClass.bsl_register fmt
  )
  else
    Format.ifprintf Format.std_formatter fmt

let (!!) pos fmt =
  OManager.printf "%a@\n" FilePos.pp_citation pos ;
  OManager.error fmt

let preprocess ~final_bymap decorated_file =
  let filename = decorated_file.D.filename in
  let browser = ByPassMap.Browser.init final_bymap in
  let fold_left buf parsed =
    let pos = D.pos parsed in
    let (!?) x = !? pos x in
    let (!!) x = !! pos x in
    match parsed with
      | D.Source (_, s) ->
          let s =
            if String.is_contained ";;" s then (
              !? "This line contains a toplevel separator @{<bright>';;'@}@\nIt will be removed to assure parser-compatibility@\n" ;
              String.replace s ";;" "  "
            ) else s
          in
          FBuffer.addln buf s

      (*
        When a format definition is found, BslRegisterParser stores it in a table,
        and then, the BSL Browser access this table to solve the inclusion.
        In opa syntax, bsl format definition can after preprocessing just be ignored
      *)
      | D.Directive (_, _, D.FormatDefinition _) -> buf

      | D.Directive (_, _, D.IncludeType strreg) ->

          let regexp = Str.regexp strreg in
          let match_any_type = ref false in
          let buf =
            ByPassMap.fold_types final_bymap (
              fun buf t ->
                let name =
                  match t with
                  | BslTypes.External (_, name, _) -> name
                  | _ -> assert false
                in
                if Str.string_match regexp name 0 then (
                  match_any_type := true ;
                  FBuffer.printf buf "%a@\n" BslTypesGeneration.Opa.pp_definition t
                )
                else buf
            ) buf
          in
          if not (!match_any_type) then (
            !? (
              "##include-type, regexpr=%S@\nThis inclusion produces an empty code@\n"^^
              "@[<2>@{<bright>Hint@}:@\n"^^
              "This inclusion may be deprecated, or the types may@\n"^^
              "have been renamed, and do not match the regexp anymore.@]@\n"
            )
              strreg ;
            ()
          );
          buf


      | D.Directive (_, _, D.Include (fmt, link)) ->
          let link = String.lowercase link in (
            match ByPassMap.Browser.Path.of_string link with
            | None ->
                !! "##include, format=<abstr>, path=%S@\nThis is not a valid syntax for a path.@\n" link
            | Some path -> (
                match ByPassMap.Browser.Path.cd browser path with
                | Some elt ->
                    (* TODO: fix whenever BslLib.include_format will uses Format *)
                    let fixme_string_instead_of_format = ByPassMap.Browser.include_format elt fmt in
                    FBuffer.addln buf fixme_string_instead_of_format

                | None ->
                    !! "##include, format=<abstr>, path=%S@\nCannot resolve this path.@\n@[<2>@{<brigh>Hint@}:@\n+ Check if your lib defines such path (ml or js)@\n+ use @{<bright>bslbrowser@} for previous plugins (depends)@]@\n" link
              )
          )

  in
  let buf = FBuffer.create ( 8 * 1024 ) in
  let buf = FBuffer.printf buf "/* File: %S -- auto preprocessing bsl : %S */\n" filename (now()) in
  let buf = List.fold_left fold_left buf decorated_file.D.decorated_source in
  filename, buf


(* Checking *)

(*
  TODO:
  some elts from the old BslRegisterLib

  MLstateLanguages.parse_and_check_type  ~lang  ~interface env (file, contents)

    let mlstate_check_ast lang =
        let fold_map_opt ~interface env (lang2, (file, contents)) =
          if MLstateLanguage.compare lang lang2 = 0
          then MLstateLanguages.parse_and_check_type ~lang ~interface env (file, contents)
          else env, None
        in
        let env as initial_env = BslInitChecker.empty ~bypass_typer () in
        let env, _ = List.fold_left_filter_map (fold_map_opt ~interface:(fun _ -> None)) env reg.reg_bootstrap.bootstrap_mlstateinit in
        let env, ast_list = List.fold_left_filter_map (fold_map_opt ~interface) env finload_mlstateinit in
        let ast_final = List.concat ast_list in

        (* Init-Blend-Checking -- to detect early features used in the libs but not supported by the blender (if any) *)
        let env =
          begin
            verbose "Init-Blend-Checking";
            let options = { (QmlBlender.OfficialDbGenBlender.default_options ()) with
                              QmlBlender.initial_env = initial_env ;
                              alphaconv_opt = None
                          } in
            let fail s =
              werror (sprintf "Init-Blend-Checking failed because of the following error :\n%s" s);
              { env with QmlTypes.had_error = true } in
            try
              let _ = QmlBlender.OfficialDbGenBlender.blend_initial ~options ast_final in
              env
            with
            | QmlTypes.Exception e -> fail (QmlTyper.OfficialTyper.string_of_error e)
            | QmlTyperException.Exception e -> fail (QmlTyperExceptionUtils.to_string e)
            | e -> fail (Printexc.to_string e)
          end
        in
        (* Init-Blend-Checking *)

        (** Will stop if there is some error *)
        (if env.QmlTypes.had_error
         then
           if BslInitChecker.is_unsafe_mode ()
           then werror "building process accomplished with errors ignored because of option unsafe"
           else
             begin
               werror "building process not accomplished due to errors in init-code";
               List.iter (fun (_, (file, contents)) ->
                            let file = Filename.basename file in
                            let log = sprintf "bsl_log_%s" file in
                            werror (sprintf "generating \"%s\" ..." log);
                            (if not (File.output log contents) then error (sprintf "error in generation in debug log %s" log))) (reg.reg_bootstrap.bootstrap_mlstateinit@finload_mlstateinit);
               exit 1
             end
        );
      in


*)

type true_means_error = bool

let checking_fail ~final_bymap:_ _opa_code =
  ( false : true_means_error) , []
