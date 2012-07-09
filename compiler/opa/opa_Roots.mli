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

(**
   Management of special dynamic pieces of code.

   1) Meta-Informations :

   (added before the parsed code regarding to the dependency).

   A few meta-informations, such as compilation date, or some code depending of
   compilation options (--cps mode, etc..) cannot be known staticly, and so
   cannot be part of the stdlib.

   However, some user may need to access these informations in their opa code.
   If a pass uses some of these ident, they should be tagged as root.

   It is the goal of opa_Roots : building some code dynamically at compile time (opa.exe)
   and insering it in the code, before the stdlib.

   This kind of generated code is added to the parsed code, and so treated as any other code
   by the opa passes.

   The pass inserting these opa value is : *pass_adding_env_var*

   2) Roots :

   list of some identifier needed / or inserted by certain pass, like db, qmlcompiler, etc..
*)

val roots_for_s3 :
  no_server:bool ->
  string list
