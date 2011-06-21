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
(* this pass doesn't really depend on the slicer but it does depend on the bsl
 * it could be moved elsewhere, but not to libqmlcompil *)
val discard_remote_bypasses :
  bymap:BslLib.BSL.ByPassMap.t ->
  lang:BslLanguage.t ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
(**
   - Job:
     This pass discards bypasses that are not defined in the current language
     If they are really called at runtime, an exception will be thrown

   - Precond:
     It assumes bypass hoisting has occured

   - Goal:
     It is intended for tests, because when the code is not sliced you end up with client bypasses
     in your server code or vice versa.

   - More precise description of the job:
     More precisely, non functional values are treated this way:
     [x = \@expanded_bypass(%%max_int%%)
      f() = x] becomes
     [x(_) = error("max_int was called")
      f() = x()]
     And functional values are treated this way:
     [bypass(x,y) = \@expanded_bypass(%%something%%(x,y))]
     becomes
     [bypass(_,_) = error("bypass something was called")]
*)
