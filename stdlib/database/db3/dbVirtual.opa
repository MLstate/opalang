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

/**
 * Utility functions on virtual path
 *
 * @category DATABASE
 * @author Quentin Bourgerie
 * @destination PUBLIC
 * @stability Not stable
 */

/**
 * {1 About this module}
 *
 * A virtual reference path parametrized by the type of value which
 * can be read, and type of value that take for write.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

@opacapi
@abstract type virtual_ref_path('read, 'write) = {
  read : -> 'read;
  write : 'write -> void;
}

/**
 * A virtual value path parametrized by the type of value which
 * can be read.
 */
@opacapi
@abstract type virtual_val_path('read) = {
  read : -> 'read;
}

/**
 * {1 Interface}
 */

DbVirtual = {{

  /**
   * Read value on the given path.
   */
  read(path : virtual_ref_path) = path.read()

  /**
   * Write the value to the virtual reference path.
   */
  write(path : virtual_ref_path, d) = path.write(d)

  /**
   * [DbVirtual.make_ref(ref, v_read, v_write)] Make a virtual reference path from
   * a [ref] reference path, a function that handle read access and
   * one for write access
   */
  make_ref(ref_path : ref_path('a),
           v_read : 'a -> 'read, v_write : 'write -> 'a) = {
    read = (-> v_read(Db.read(ref_path)))
    write = (w -> Db.write(ref_path, v_write(w)))
  } : virtual_ref_path('read, 'write)

  /**
   * [DbVirtual.make_ref(val, v_read)] Make a virtual value path from
   * a [val] value path and a function that handle read access.
   */
  make_val(val_path : val_path('a),
           v_read : 'a -> 'read) = {
    read = (-> v_read(Db.read(val_path)))
  } : virtual_val_path('read)

}}


////////////////////////////
// Needed by the compiler //
////////////////////////////
@opacapi DbVirtual_make_val = DbVirtual.make_val
@opacapi DbVirtual_make_ref = DbVirtual.make_ref
@opacapi DbVirtual_hack_coerce_default(_handler : {read : 'd -> 'vr; write : 'wr -> 'd}, x : 'vr) = x
@opacapi DbVirtual_hack_coerce_option(_handler : {read : 'd -> 'vr; write : 'wr -> 'd}, x : option('vr)) = x
@opacapi DbVirtual_hack_coerce_vvpath(_handler : {read : 'd -> 'vr; write : 'wr -> 'd}, x : virtual_val_path('vr)) = x
@opacapi DbVirtual_hack_coerce_vrpath(_handler : {read : 'd -> 'vr; write : 'wr -> 'd}, x : virtual_ref_path('vr, 'wr)) = x
////////////////////////////
// Needed by the compiler //
////////////////////////////
