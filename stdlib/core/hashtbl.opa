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

package stdlib.core

/**
 * The type of hash tables that contain ['key] to ['value] bindings.
 */
type Hashtbl.t('key, 'value) = external

Hashtbl = {{

  /**
   * Creates an empty hash table, with an initial [size].
   * @param size The initial size
   */
  create(size) =
    %%BslHashtbl.create%%(size)

  /**
   * Creates an empty hash table, with an initial [size].
   * @param size The initial size
   */
  make(hash, equals, size) =
    %%BslHashtbl.make%%(hash, equals, size)

  /**
   * Empty the hash [table].
   * @param table The table to clear
   */
  clear(table) = %%BslHashtbl.clear%%(table)

  /**
   * Adds a binding of [key] to [value] in the hash [table].
   * @param table The table where the binding is added
   * @param key The key of the binding
   * @param value The value associated to the [key]
   */
  add(table, key, value) = %%BslHashtbl.add%%(table, key, value)

  /**
   * Replace or add the binding of [key] to [value] in the hash [table].
   * @param table The table where the binding is added
   * @param key The key of the binding
   * @param value The value associated to the [key]
   */
  replace(table, key, value) = %%BslHashtbl.replace%%(table, key, value)

  /**
   * Returns the current value associated to the [key] in the [table].
   * @param table The hash table
   * @param key The key of wanted binding
   */
  try_find(table, key) = %%BslHashtbl.try_find%%(table, key)

  /**
   * Removes the current biding of key [key] in [table].
   * @param table The hash table
   * @param key The key of removed binding
   */
  remove(table, key) = %%BslHashtbl.remove%%(table, key)

  /**
   * Returns the number of bindings in [table]
   */
  size(table) = %%BslHashtbl.size%%(table)

  /**
   * Test the emptiness of the [table]
   */
  is_empty(table) = size(table) == 0

  /**
   * Returns [true] if [key] is bound in the [table].
   */
  mem(table, key) = %%BslHashtbl.mem%%(table, key)

}}
