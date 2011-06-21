/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
    @author AUDOUIN Maxime 
**/

type multimap('a, 'b, 'order) = ordered_map('a, list('b), 'order)

Make_multimap(order) =
{{
   @private multimap = Map_make(order)
   empty = multimap.empty
   add(key, val, map) = set(key, (val +> get(key, map)), map)
   get(key, map) = default( [], multimap.get(key, map))
   rem(key, val, map) =
     rec f(li) =
       match li with
       | [] -> []
       | [hd | tl] -> if Order.equal(hd,val,order) then tl else hd +> f(tl)
     v = f(get(key, map))
     set(key, v, map)
   rem_last(key, map) =
     set(key, List.tail(get(key, map)), map)
   fold(f, map, acc) =
     multimap.fold(
       ( key, val, acc -> List.fold_left(
         (acc, val -> f(key, val, acc)),
         acc,
         val)),
       map, acc)
   rem_all_val(key, val, map) =
     set(key, List.filter((Order.not_equal(val,_,order)), get(key, map) ) , map)
   rem_all(key, map) = multimap.remove(key, map)
   set(key, val_list, map) = multimap.add(key, val_list, map)
}}

Multimap = @nonexpansive(Make_multimap(Order.default))
