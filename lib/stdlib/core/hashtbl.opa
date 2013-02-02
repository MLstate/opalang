/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package stdlib.core

/**
 * The type of hash tables that contain ['key] to ['value] bindings.
 */
@abstract
type Hashtbl.t('key, 'value) = external

type Hashtbl.binding('key, 'value) = {
  key : 'key
  value : 'value
}

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

  /**
   * Returns the list of all bindings of the [table]
   */
  bindings(table) = %%BslHashtbl.bindings%%(table)

}}
