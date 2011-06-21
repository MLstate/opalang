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
   Pass to share constants, simple records and records with only constant fields.

   @author Esther Baruk
   @author Mathieu Barbin
*)

(**
   This pass defines for each constant (string, float and records) used in the QML code a toplevel
   definition.
   We define a subset of QML ast to work only on the part of the ast which we are interested in.

   Then it replaces every occurence of the constant by the corresponding
   toplevel identifier just defined.

   Example :

   {[
   _ = %%bslpervasives.print_endline%% "hello"
   _ = %%bslpervasives.print_endline%% (%%bslpervasives.string_of_float%% 2.)
   c = 2.
   _ = %%bslpervasives.print_endline%% "hello"

   is rewritten in

   {[
   v0_const = "hello"
   v1_const = 2.

   _ = %%bslpervasives.print_endline%% v0_const
   _ = %%bslpervasives.print_endline%% (string_of_int v1_const)
   c = v1_const
   _ = %%bslpervasives.print_endline%% v0_const
   ]}
   The constants ["hello"] and [2.] are now shared thanks to the
   declaration of two variables at the beginnning of the code.

   For debuging, and choosing what kind of constant to share,
   cf constant_sharing_* debug variables.
*)

(**
   Beware, part of the env is mutable, which does not allow
   to use this module as purelly functionnal with diverge.
*)
val process_code :
  side:[`client | `server] ->
  typed:bool ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code -> (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
