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

(**************************************************************)
(* Before adding a function here please READ MLI INSTRUCTIONS *)
(**************************************************************)

type ('env,'a) checker = ('env -> 'a) -> 'env PassHandler.cond

module Ident =
struct

  let ident_unbound _code = assert false   (* TODO *)

  let unbound_id = QmlAlphaConv.Check.unbound_id

  let unbound extract =
    PassHandler.make_condition unbound_id
      (fun env -> ident_unbound (extract env))
end
