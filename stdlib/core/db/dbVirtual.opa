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
/**
 * Utility functions on virtual path
 *
 * @category DATABASE
 * @author Quentin Bourgerie
 * @destination PUBLIC
 * @stability Not stable
 */

/**
 * A virtual reference path parameterized by the type of value which
 * can be readed, and type of value that take for write.
 */
@opacapi
@abstract type virtual_ref_path('read, 'write) = {
  read : -> 'read;
  write : 'write -> void;
}

/**
 * A virtual value path parameterized by the type of value which
 * can be readed.
 */
@opacapi
@abstract type virtual_val_path('read) = {
  read : -> 'read;
}

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
