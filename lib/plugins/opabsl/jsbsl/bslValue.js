/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

//////////////////////////////////////////////////////////////////////
// BEWARE THIS FILE IS SHARING BEETWEEN THE JAVASCRIPT AND NODE BSL //
//////////////////////////////////////////////////////////////////////

/**
 * Back-end instrospection
 * @side{both}
 */

/** @externType Record.field */

/** @externType Record.constructor */

/** @externType Record.patterns_indexes */

/** @externType Record.fields_indexes */

/** @externType Record.field_index */

/** @module record */

/**
 * @register {'a, Record.field -> opa[option('b)]} dot dot
 * @pure
 */

/**
 * @register {'a, Record.field -> 'b} unsafe_dot unsafe_dot
 * @pure
 */

/**
 * @register {(Record.field, 'a, 'b -> 'b), 'c, 'b -> 'b} fold_record fold_record
 * @pure
 */

/**
 * @register {(Record.field, 'a, 'b, continuation('b) -> void), 'c, 'b, continuation('b) -> void} fold_record_cps fold_record_cps
 * @pure
 * @cpsBypass
 */

/**
 * @register {(Record.field, 'a, 'a, 'b -> 'b), 'c, 'c, 'b -> 'b} fold_2_record fold_2_record
 * @pure
 */

/**
 * @register {(Record.field, 'a, 'a, 'b, continuation('b) -> void), 'c, 'c, 'b, continuation('b) -> void} fold_2_record_cps fold_2_record_cps
 * @pure
 * @cpsBypass
 */

/** @register {Record.field -> opa[option(string)]} name_of_field name_of_field */

/** @register {string -> opa[option(Record.field)]} field_of_name field_of_name */

/**
 * @register {-> Record.constructor} empty_constructor empty_constructor
 * @pure
 */

/**
 * @register {Record.constructor, Record.field, _ -> Record.constructor} add_field add_field
 * @pure
 */

/**
 * @register {Record.constructor -> _} make_record make_record
 * @pure
 */

/**
 * @register {Record.field -> _} make_simple_record make_simple_record
 * @pure
 */

/**
 * @register {llarray(Record.field) -> Record.fields_indexes} fields_indexes fields_indexes
 * @pure
 */

/**
 * @register {Record.fields_indexes,Record.field -> Record.field_index} field_index field_index
 * @pure
 */

/**
 * @register {'record,Record.field_index -> 'field_content} dot_with_field_index dot_with_field_index
 * @pure
 */

/**
 * @register {llarray(Record.fields_indexes) -> Record.patterns_indexes} patterns_indexes patterns_indexes
 * @pure
 */

/**
 * @register {Record.patterns_indexes,'record,'record -> int} compare_structure compare_structure
 * @pure
 */


/** @endModule */

var bsl_tsc_tsctbl = {}

/** @module tsc */

  /**
   * @opacapi
   * @register {string, _ -> opa[void]}
   */
  function add(str, f) {
    // FIXME: If we project this function it gets cleaned.
    bsl_tsc_tsctbl[str] = f;
    return js_void;
  }

  /**
   * @register {string -> opa[option(_)]} get
   */
  function value_tsc_get(str) {
    var r = bsl_tsc_tsctbl[str];
    if(bsl_tsc_tsctbl[str]){
      return js_some(r);
    }
    return js_none;
  }

/** @endModule */

var to_string_tbl = {}
var compare_tbl = {}
var serializer_tbl = {}
var xmlizer_tbl = {}

/** Used for register and get some specialized function for magic
    function. */
/** @module MagicContainer */
  /**
   * @register { string, 'a -> void}
   */
  function to_string_add(k, o) {
    to_string_tbl[k] = o;
  }

  /**
   * @register { string -> opa[option('a)]}
   */
  function to_string_get(k) {
    var r = to_string_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

  /**
   * @register { string, 'a -> void}
   */
  function compare_add(k, o) {
    compare_tbl[k] = o;
  }

  /**
   * @register { string -> opa[option('a)]}
   */
  function compare_get(k) {
    var r = compare_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

  /**
   * @register { string, 'a -> void}
   */
  function serializer_add(k, o) {
    serializer_tbl[k] = o;
  }

  /**
   * @register { string -> opa[option('a)]}
   */
  function serializer_get(k) {
    var r = serializer_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

  /**
   * @register { string, 'a -> void}
   */
  function xmlizer_add(k, o) {
    xmlizer_tbl[k] = o;
  }

  /**
   * @register { string -> opa[option('a)]}
   */
  function xmlizer_get(k) {
    var r = xmlizer_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

/** @endModule */
