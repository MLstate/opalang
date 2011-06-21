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
   Eta expand, hoist (lift) and share bypass of the code.
   @author Sebastien Briais
   @author Esther Baruk
   @author Mathieu Barbin
*)

(**
   This pass defines for each bypass used in the code a toplevel
   definition which corresponds to the eta expansion of the bypass.

   It also replaces every occurrence of the bypass by the corresponding
   toplevel identifier just defined.

   Eta-expansion is based on the types provided by the bypass typer function (libbsl)
   (as much lambdas as arrows in the type)

   Some simplification for the generated code :

   + the pass is idempotent, thanks to the directive \@expanded_bypass
   + when the definition at top level of a bypass is the first utilisation of it,
     do the eta-expension in place.
   + remove local alias of bypass, using the top-level identifier instead.

   This pass has 2 mode, typed or not typed.

   Example:

   {[
   plus = %%+%%
   succ x = %%+%% x 1
   toto =
     p = %%+%% in
     p 4 5
   ]}

   is rewritten in

   {[
   plus = x, y -> %%+%% x y
   succ x = plus x 1
   toto = plus 4 5
   ]}

   If the option [just_expand] is set, do not hoist the bypass, just
   do the expand in place, insering the directive \@expanded_bypass.
*)

val process_code :
  just_expand:bool ->
  typed:bool -> (* same remark as in QmlLambdaLifting *)
  QmlTypes.bypass_typer ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
