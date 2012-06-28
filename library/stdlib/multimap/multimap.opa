/*
    Copyright © 2011, 2012 MLstate

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

/** Multimap operations with custom order */
Make_multimap(order) =
{{
   @private multimap = Map_make(order)
   empty:multimap = multimap.empty
   /** add a value [val] for the key [key] in the map [map], preserving old previous binding for [key] */
   add(key, val, map:multimap):multimap = set(key, [val|get(key, map)], map)

   /** get all value associated to key [key] */
   get(key, map:multimap) = multimap.get(key, map) ? []

   /** remove, if possible, the more recent value equals to [val] associated with key [key] in [map] */
   rem(key, val, map:multimap):multimap =
     rec f(li) =
       match li with
       | [] -> []
       | [hd | tl] -> if Order.equal(hd,val,order) then tl else [hd|f(tl)]
     v = f(get(key, map))
     set(key, v, map)
   /** remove the more recent value associated to [key], fails if no such value exists */
   rem_last(key, map:multimap):multimap =
     set(key, List.tail(get(key, map)), map)
   /** fold all key value association */
   fold(f, map:multimap, acc) =
     multimap.fold(
       ( key, val, acc -> List.fold_left(
         (acc, val -> f(key, val, acc)),
         acc,
         val)),
       map, acc)

   /** remove, if possible, all values equals to [val] associated with key [key] in [map] */   rem_all_val(key, val, map:multimap):multimap =
     set(key, List.filter((Order.not_equal(val,_,order)), get(key, map) ) , map)
   /** remove all values associated to [key] */
   rem_all(key, map:multimap):multimap = multimap.remove(key, map)

   /** set the complete list [list] of val associated to the [key],
       the list is ordered from more recent to less recent */
   set(key, val_list, map:multimap) = multimap.add(key, val_list, map)

}}

/** Multimap operations with default order */
Multimap = @nonexpansive(Make_multimap(Order.default))
