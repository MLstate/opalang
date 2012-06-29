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

##register dot \ `dot` : 'a, Record.field -> option('b)

##register unsafe_dot \ `unsafe_dot` : 'a, Record.field -> 'b

##register fold_record \ `fold_record` : (Record.field, 'a, 'b -> 'b), 'c, 'b -> 'b

##register fold_2_record \ `fold_2_record` : (Record.field, 'a, 'a, 'b -> 'b), 'c, 'c, 'b -> 'b

##register name_of_field \ `name_of_field` : Record.field -> option(string)

##register field_of_name \ `field_of_name` : string -> option(Record.field)

##register empty_constructor \ `empty_constructor` : -> Record.constructor

##register add_field \ `add_field` : Record.constructor, Record.field, _ -> Record.constructor

##register make_record \ `make_record` : Record.constructor -> _

##register make_simple_record \ `make_simple_record` : Record.field -> _

##register fields_indexes \ `fields_indexes`  : llarray(Record.field) -> Record.fields_indexes

##register field_index \ `field_index`: Record.fields_indexes,Record.field -> Record.field_index

##register dot_with_field_index \ `dot_with_field_index`: 'record,Record.field_index -> 'field_content

##register patterns_indexes \ `patterns_indexes`: llarray(Record.fields_indexes) -> Record.patterns_indexes

##register compare_structure \ `compare_structure`: Record.patterns_indexes,'record,'record -> int


##endmodule

var bsl_tsc_tsctbl = {}

##module tsc \ bsl_tsc

##register add : string, _ -> void
  ##args(str, f)
  {
    bsl_tsc_tsctbl[str] = f;
  }

##register get : string -> option(_)
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

  ##register to_string_get : string -> option('a)
  ##args(k)
  {
    var r = to_string_tbl[k];
    if(r==undefined) return js_none;
    else return js_some(r);
  }

  ##register compare_add : string, 'a -> void
  ##args(k, o)
  {
    compare_tbl[k] = o;
  }

  ##register compare_get : string -> option('a)
  ##args(k)
  {
    var r = compare_tbl[k];
    if(r==undefined) return js_none;
    else return js_some(r);
  }

  ##register serializer_add : string, 'a -> void
  ##args(k, o)
  {
    serializer_tbl[k] = o;
  }

  ##register serializer_get : string -> option('a)
  ##args(k)
  {
    var r = serializer_tbl[k];
    if(r==undefined) return js_none;
    else return js_some(r);
  }

  ##register xmlizer_add : string, 'a -> void
  ##args(k, o)
  {
    xmlizer_tbl[k] = o;
  }

  ##register xmlizer_get : string -> option('a)
  ##args(k)
  {
    var r = xmlizer_tbl[k];
    if(r==undefined) return js_none;
    else return js_some(r);
  }

##endmodule
