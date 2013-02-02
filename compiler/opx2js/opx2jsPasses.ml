(*
    Copyright © 2012 MLstate

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

module List = BaseList

module PH = PassHandler
module O = Opx2jsOptions

type options = Opx2jsOptions.t

type ('env, 'env2) pass = (options, options, 'env, 'env2) PassHandler.pass

let pass_Welcome =
  PassHandler.make_pass
    (fun {PH.env=()} ->
       let options = Opx2jsOptions.get_options () in
       OManager.verbose "Opa version %s" BuildInfos.opa_version_name ;
       OManager.verbose "(c) 2007-%s MLstate, All Rights Reserved." BuildInfos.year;
       OManager.verbose "Build: %s" BuildInfos.version_id;
       PassHandler.make_env options ())

let pass_CheckOptions =
  PassHandler.make_pass
    (fun e ->
       if List.is_empty e.PH.options.O.packages
       then (
         OManager.printf "@{<bright>No packages are specified@}@.";
         O.print_help ();
         OManager.printf "@[<2>@{<bright>Hint@}:@\nprecise some packages@]@.";
         exit 1;
       ) else e
    )

type env = {
  package : ObjectFiles.package;
  renaming : SurfaceAstRenaming.SExpr.t;
  gamma : QmlTypes.Env.t;
  is_ei : Ident.t -> bool;
  undot : QmlAst.expr StringMap.t IdentMap.t;
  skipped : Ident.t IdentMap.t;
  code : JsAst.code;
}

let pass_LoadEnvironment k =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let module RawRenaming = ObjectFiles.MakeRaw(SurfaceAstRenaming.SExpr) in
       let module RawTyping = ObjectFiles.MakeRaw(Pass_Typing.S) in
       let module RawTypeDefinition = ObjectFiles.MakeRaw(Pass_TypeDefinition.S) in
       let module RawUndot = ObjectFiles.MakeRaw(Pass_Undot.S) in
       let module RawEi = ObjectFiles.MakeRaw(Pass_ExplicitInstantiation.S) in
       let packages =
         List.map (fun package_name ->
                     let package = package_name, FilePos.nopos "commandLine" in
                     ObjectFiles.expand_glob ~mode:`package [] package
                  ) options.O.packages
       in
       let packages = List.flatten packages in
       let _ = List.fold_left
           (fun _acc package ->

              let renaming = RawRenaming.load1 package in
              let gamma = RawTypeDefinition.load1 package in
              let gamma =
                (* Type definition of dependencies are needed *)
                List.fold_left
                  (fun gamma package ->
                     QmlTypes.Env.append gamma (RawTypeDefinition.load1 package))
                  gamma (ObjectFiles.load_deps package)
              in
              let identmap = RawTyping.load1 package in
              let gamma = QmlTypes.Env.Ident.from_map identmap gamma in
              let is_ei =
                let have_typeof = RawEi.load1 package in
                let notei =
                  IdentMap.filter_val
                    (fun tsc ->
                       let vars, _, _ = QmlGenericScheme.export_unsafe tsc in
                       QmlTypeVars.FreeVars.is_empty (QmlTypeVars.FreeVars.inter vars have_typeof)
                    ) identmap
                in
                (fun ident -> not (IdentMap.mem ident notei))
              in
              let srenaming = QmlSimpleSlicer.get_renaming package ~side:`server in
              let undot =
                let {Pass_Undot. modules; aliases} = (fst (RawUndot.load1 package)) in
                IdentMap.fold_rev
                  (fun a i modules ->
                     try
                       IdentMap.add a (IdentMap.find i modules) modules
                     with Not_found ->
                       (* FIXME : Alias through different packages...? *)
                       modules
                  )
                  aliases modules
              in
              let skipped =
                IdentMap.fold
                  (fun cps skip acc ->
                     try
                       IdentMap.add (IdentMap.find cps srenaming) skip acc
                     with Not_found -> acc
                  )
                  (QmlCpsRewriter.get_skipped package)
                  IdentMap.empty
              in
              k (PassHandler.make_env options
                   {renaming; gamma; undot; skipped; package; code=[]; is_ei})
           ) 0 packages
       in
       PassHandler.make_env options 0
    )


let pass_NodeJsPluginCompilation =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let {renaming; gamma; undot; skipped; package; is_ei} = e.PH.env in
       let env = Pass_NodeJsPluginCompilation.build_env
         ~package ~renaming ~gamma ~undot ~skipped ~is_ei
       in
       let code = Pass_NodeJsPluginCompilation.process env in
       PassHandler.make_env options {e.PH.env with code}
    )


let pass_NodeJsPluginGeneration =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let {code; package} = e.PH.env in
       let name = fst package in
       let build_dir = Filename.concat options.O.build_dir name in
       let jsp = JsPackage.default ~name in
       let jsp = JsPackage.set_build_dir jsp build_dir in
       let jsp = JsPackage.add_code jsp code in
       let json = Filename.concat (ObjectFiles.find_dir package) "package.json" in
       let json = File.content json in
       let version =
         match Json_utils.from_string json with
         | JsonTypes.Record l ->
             begin match List.find (fun (name, _) -> name = "version") l with
             | _, JsonTypes.String s -> s
             | _ -> raise Not_found
             end
         | _ -> raise Not_found
       in
       let jsp = JsPackage.set_version jsp version in
       let jsp = JsPackage.add_dependencies jsp [(Printf.sprintf "%s.opx" name), version] in
       let jsp = JsPackage.add_file jsp ("README.md", Printf.sprintf "\
# %s
This is a generated interface from Opa package %s.opx@%s
" name name version)
       in
       JsPackage.write jsp;
       PassHandler.make_env options 0
    )

