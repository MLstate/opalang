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
module Make (Ord: OrderedTypeDebugableSig.S) : (BaseMapSig.S with type key = Ord.t) =
struct
  module M = BaseMap.Make (Ord)
  include M
  let add x data map =
    let _ = fold (fun index _value (c1, old) ->
                    let c2 = Ord.compare index x in
                    if c2 < c1 then
                      begin
                        prerr_endline (Ord.to_string old);
                        prerr_endline (Ord.to_string x);
                        prerr_endline (Ord.to_string index);
                        assert false
                      end
                    else (c2, index)
                 ) map ((-42), Obj.magic 42) (* the first case is unused *)
    in add x data map

end
