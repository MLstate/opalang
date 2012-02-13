(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
