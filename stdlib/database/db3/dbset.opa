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
@opacapi
type dbset('a) = external

/**
 * {1 Interface}
 */

DbSet = {{

  /**
   * [DbSet.fold(dbset, acc, folder)] Fold on a [dbset].
   */
  fold = %%badoplink_fold_dbset%% : dbset('a), 'acc, ('acc, 'a -> 'acc) -> 'acc

}}
