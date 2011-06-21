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
   This pass registers all the source code of the application (all the files
   and their content) to a BSL module BslAppSrcCode. This module then
   exposes those sources to the server, which makes a default web-page
   /_internal_/src_code presenting the source code of the application.

   Collecting original sources is accomplished by the earlier pass
   GatherAppSrcCode.

   @author Adam Koprowski
*)

val register_code : special:bool
                 -> SurfaceAst.parsing_directive SurfaceAstPasses.parsed_file list
                 -> Passes.input_file list
                 -> SurfaceAst.parsing_directive SurfaceAstPasses.parsed_file list
