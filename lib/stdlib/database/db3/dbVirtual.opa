/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
