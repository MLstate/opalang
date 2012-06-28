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
   Fun action resolution : Insertion of environment serialization.

   @author Rudy Sicard
   @author Mathieu Barbin
*)

(**
   This module implements part of the funactions resolution for opa.
   This is the step 2), taking place :
   + after Typing and Slicing
   + before Explicit instantation.


   This pass insert in the code generating the xhml sent to clients containing fun actions
   the serialization of the environment of the fun action, corresponding to the argument
   already applied.

   Initial expressions
   {[
   < onclick=\{EXPR = \@funaction(e)\}>
   ]}

   After [FunActionLifting]
   {[
   f(arg1,arg2,...)=e
   < onclick=\{EXPR = \@funaction( f(arg1,arg2,...) ) \}>
   ]}

   Then, [typing, slicing].

   After [FunActionEnvSerialize]
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

   The pass is after typing, and should preserve types.

   Types of directive:
   + {[\@funaction('a) : 'a]}
   + {[\@funaction[Deserialized]({arg : 'a ; serialized_arg : string}) : 'a]}
   + {[\@funaction[cliend_id]('a) : 'a]}

   So, essentially, the kernel of this pass is the following, applied only
   on the server side code :

   From all directives :
   {[
   \@funaction( f(arg1,arg2,...) )
   ]}

   To
   {[
   \@funaction(
     arg1_ = FunActionServer_serialize_argument(arg1)
     arg2_ = FunActionServer_serialize_argument(arg2)
     ...
     (\@funaction[client_id](f))(
     \@funaction[Deserialize](arg1_),
     \@funaction[Deserialize](arg2_),
   ...
     )
   )
   ]}

*)

val process_server_code:
  stdlib_gamma:QmlTypes.gamma ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
