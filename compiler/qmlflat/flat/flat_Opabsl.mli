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
   Flat Compiler : Bypass generation
   @author Mathieu Barbin
*)

(**
   This modules reproduce the bypass hierarchy of the opabsl introduced by the flat compiler.
*)

(**
   The type of an expression accessing a function of the bsl.
   This is a bypass node.
*)
type primitive = QmlAst.expr

module Bslpervasives :
sig
  val assertion : primitive
  val fail : primitive
end

module Bslcps :
sig
  module Notcps_compatibility :
  sig
    val callcc_directive : primitive
    val thread_context : primitive
    val with_thread_context : primitive
  end
end
