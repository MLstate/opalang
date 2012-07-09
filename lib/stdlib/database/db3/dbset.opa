/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
    @authors ?
**/

/**
 * Utility functions on dbset
 *
 * @category DATABASE
 * @author Quentin Bourgerie
 * @destination PUBLIC
 * @stability Not stable
 */

/**
 * {1 About this module}
 *
 * This module defines the primitive type of dbset and functions for
 * manipulate this kind of data.
 *
 * {1 Where do I start?}
 *
 * {2 Path declaration}
 *
 * On the first step define a multiple keys path like that :
 * [db /myset[a;b] : {a : int; b : string; c : int}]
 *
 * This declaration define a path to a dbset that contains data of
 * type [{a : int; b : string; c : int}] indexed by the keys [a] and
 * [b].
 *
 * {2 Data(s) access}
 *
 * Actually you can access to path by 2 ways, in the two case you
 * access with a virtual path.
 *
 * On both ways the write access take a record that contains all
 * fields of the stored data type unless the keys which are already
 * binded.
 *
 *
 * The read access differs if you give a full key, or just a partial
 * key. Indeed if you give a full key the read returns a single value,
 * else the read returns a dbset value.
 *
 * {3 Examples }
 *
 * {[
 *   /* Access with a full key */
 *   @/myset[a=1; b="opa"] : virtual_ref_path({a:int; b:string; c:int},
 *                                            {b:string; c : int})
 *   /* Access with a partial key */
 *   @/myset[a=1] : virtual_ref_path(dbset({a:int; b:string; c:int}),
 *                                   {b:string; c : int})
 *
 *   /* And of course, all syntactic sugar on path works */
 *   !/myset[a=1; b="opa"] : virtual_val_path({a:int; b:string; c:int})
 *
 *   !/myset[a=1] : virtual_val_path(dbset({a:int; b:string; c:int}))
 *
 *   /myset[a=1; b="opa"] : {a:int; b:string; c:int}
 *
 *   /myset[b="opa"] : dbset({a:int; b:string; c:int})
 * }
 *
 * {1 What if I need more?}
 *
 * TODO (Access with range, etc.)
**/

/**
 * {1 Types defined in this module}
 */

/**
 * A database set that contains ['a] values
**/

@abstract
@opacapi
type Db3Set.engine('a) = external

@opacapi
type Db3Set.t('a) = dbset('a, Db3Set.engine('a))

/**
 * {1 Interface}
 */

Db3Set = {{

  iterator(d:Db3Set.engine('a)) =
    Iter.of_list(List.rev(%%Badoplink.fold_dbset%%([], d, (l, a -> List.add(a, l)))))


}}

@opacapi Db3Set_iterator = Db3Set.iterator
