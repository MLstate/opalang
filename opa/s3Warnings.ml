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

let warning_set =
  let s = WarningClass.Set.create () in
  let (!+) w = WarningClass.Set.add s w in
  let (?+) w = if BuildInfos.is_release then () else !+ w in
  let (!++) s' = WarningClass.Set.add_set s s' in
  (* let (?++) s' = if BuildInfos.is_release then () else !++ s' in *)
  (* combine there all loaded warning_set you want to be available in opa S3 *)
  (* for organisation layout, please use alphabetic order *)

  (* CONDITIONS *)

  ?+ WarningClass.cond ;

  (* PASSES (alphabetic order) *)

  !++ ObjectFiles.warning_set ;

  (* !++ Pass_BslLoading.warning_set ; *)

  !++ QmlDbGen.warning_set ;

  !++ Imp_Compiler.warning_set ;

  !++ QmlSimpleSlicer.warning_set ;

  !++ QmlTyperWarnings.warning_set ;

  !++ SurfaceAstRenaming.warning_set ;

  !++ I18nAndComputedString.warning_set ;

  !++ Flat_Compiler.warning_set ;

  !++ Pass_CheckPatternMatching.warning_set ;

  !++ Pass_CodingDirectives.warning_set ;

  !++ Pass_MacroExpansion.warning_set ;

  !++ SurfaceAstStaticInclude.warning_set ;

  !++ Pass_InstrumentForClosureSerialization.warning_set;

  (* finally return the global warning_set *)
  s
