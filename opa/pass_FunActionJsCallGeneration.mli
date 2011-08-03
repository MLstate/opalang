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
   Fun action resolution : Generation of js call in the xhtml

   @author Rudy Sicard
   @author Mathieu Barbin
*)

(**
   This module implements part of the funactions resolution for opa.
   This is the step 3), taking place :
   + after Explicit instantation.
   + before CPS and Global Lambda lifting

   This pass replace the fields [on*events] (onclick, onready, etc....) by the Js Code corresponding to the execution
   of the fun action, which is executed totally on the client side, without any request to the server.
   This pass uses also an optimization (like the hybrid values at toplevel) : whenever it is possible, the Js Code
   generated can be either a call to [unserialize], or directly a back-end dependant Js Code.


   Before the pass, after [FunActionEnvSerialize], the code is :
   {[
   f(arg1,arg2,...)=e
   < onclick=\{EXPR = \@funaction(
                  arg1_ = FunActionServer_serialize_argument(arg1)
                  arg2_ = FunActionServer_serialize_argument(arg2)
                  ...
                  (\@funaction[client_id](f))(
                        \@funaction[Deserialize](arg1_),
                        \@funaction[Deserialize](arg2_),
                        ...
                  )
                  )
                  \} >
   ]}

   After this pass :
   {[
   f(arg1,arg2,...)=e
   < onclick=\{STRING =
                  arg1_ = FunActionServer_serialize_argument(arg1)
                  arg2_ = FunActionServer_serialize_argument(arg2)
                  ...
                  FunActionServer_serialize_call(
                        \@jsident(f),
                        [ arg_1_,
                          arg_2_,
                          ...
                        ]
                  )
                  \}
    >
   ]}

   The pass is after typing, and should preserve types.
*)

val process_server_code:
  QmlTypes.gamma ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
