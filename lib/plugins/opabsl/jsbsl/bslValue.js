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

/** For more information see the ml implementation. */

##extern-type Record.field

##extern-type Record.constructor

##extern-type Record.patterns_indexes

##extern-type Record.fields_indexes

##extern-type Record.field_index

##module record \ bsl_record

##register [pure] dot \ `dot` : 'a, Record.field -> opa[option('b)]

##register [pure] unsafe_dot \ `unsafe_dot` : 'a, Record.field -> 'b

##register [pure] fold_record \ `fold_record` : (Record.field, 'a, 'b -> 'b), 'c, 'b -> 'b

##register [pure] fold_2_record \ `fold_2_record` : (Record.field, 'a, 'a, 'b -> 'b), 'c, 'c, 'b -> 'b

##register name_of_field \ `name_of_field` : Record.field -> opa[option(string)]

##register field_of_name \ `field_of_name` : string -> opa[option(Record.field)]

##register [pure] empty_constructor \ `empty_constructor` : -> Record.constructor

##register [pure] add_field \ `add_field` : Record.constructor, Record.field, _ -> Record.constructor

##register [pure] make_record \ `make_record` : Record.constructor -> _

##register [pure] make_simple_record \ `make_simple_record` : Record.field -> _

##register fields_indexes \ `fields_indexes`  : llarray(Record.field) -> Record.fields_indexes

##register field_index \ `field_index`: Record.fields_indexes,Record.field -> Record.field_index

##register [pure] dot_with_field_index \ `dot_with_field_index`: 'record,Record.field_index -> 'field_content

##register patterns_indexes \ `patterns_indexes`: llarray(Record.fields_indexes) -> Record.patterns_indexes

##register [pure] compare_structure \ `compare_structure`: Record.patterns_indexes,'record,'record -> int


##endmodule

var bsl_tsc_tsctbl = {}

##module tsc \ bsl_tsc

##register add : string, _ -> opa[void]
  ##args(str, f)
  {
    // FIXME: If we project this function it gets cleaned.
    bsl_tsc_tsctbl[str] = f;
    return js_void;
  }

##register get : string -> opa[option(_)]
  ##args(str)
  {
    var r = bsl_tsc_tsctbl[str];
    if(bsl_tsc_tsctbl[str]){
      return js_some(r);
    }
    return js_none;
  }

##endmodule

var to_string_tbl = {}
var compare_tbl = {}
var serializer_tbl = {}
var xmlizer_tbl = {}

/** Used for register and get some specialized function for magic
    function. */
##module MagicContainer

  ##register to_string_add : string, 'a -> void
  ##args(k, o)
  {
    to_string_tbl[k] = o;
  }

  ##register to_string_get : string -> opa[option('a)]
  ##args(k)
  {
    var r = to_string_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

  ##register compare_add : string, 'a -> void
  ##args(k, o)
  {
    compare_tbl[k] = o;
  }

  ##register compare_get : string -> opa[option('a)]
  ##args(k)
  {
    var r = compare_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

  ##register serializer_add : string, 'a -> void
  ##args(k, o)
  {
    serializer_tbl[k] = o;
  }

  ##register serializer_get : string -> opa[option('a)]
  ##args(k)
  {
    var r = serializer_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

  ##register xmlizer_add : string, 'a -> void
  ##args(k, o)
  {
    xmlizer_tbl[k] = o;
  }

  ##register xmlizer_get : string -> opa[option('a)]
  ##args(k)
  {
    var r = xmlizer_tbl[k];
    if(typeof r == 'undefined') return js_none;
    else return js_some(r);
  }

##endmodule
