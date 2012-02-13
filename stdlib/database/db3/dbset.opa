/*
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
  fold = %%badoplink_fold_dbset%% : 'acc, dbset('a), ('acc, 'a -> 'acc) -> 'acc

}}
