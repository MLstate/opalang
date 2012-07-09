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
   Checking on Pattern Matching.

   @author Rudy Sicard
   @author Mathieu Barbin
*)

(**
   This pass perform some checks on pattern matching.
   In case of illicit patterns, the pass simply raise some warnings
   with located error messages.

   The code is typed, so that we can perform more clever checks.

   Currently, the code is not transformed.
   We can activate a toggle for applying the pattern normalization,
   until we decide in what cases we do want to keep the normalized
   version (if e.g. size explosion)
*)

val process_code :
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  QmlAst.annotmap * QmlAst.code

val warning_set : WarningClass.Set.t
