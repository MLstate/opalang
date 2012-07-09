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
(* CF mli *)

(* depends *)

(* shorthands *)
module Q = QmlAst
module QC = QmlAstCons.UntypedExpr

(* -- *)

type primitive = QmlAst.expr

let pass = "flat"
let make skey = QC.restricted_bypass ~pass (BslKey.normalize skey)

module Bslpervasives =
struct
  let make f = make ("bslpervasives."^f)
  let assertion = make "assertion"
  let fail = make "fail"
end

module Bslcps =
struct
  let make f = make ("bslcps."^f)
  module Notcps_compatibility =
  struct
    let make f = make ("notcps_compatibility."^f)
    let callcc_directive = make "callcc_directive"
    let thread_context = make "thread_context"
    let with_thread_context = make "with_thread_context"
  end
end
