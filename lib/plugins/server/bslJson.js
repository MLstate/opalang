/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

##extern-type RPC.Json.json

/** This part concerns translation of a OPA Json object to/from a native javascript object */
##extern-type RPC.Json.private.native
##extern-type ll_json_list_repr
##extern-type ll_json_record_repr


function is_float(n){
    return /[e\.]/.test(n);
}


##module Json \ bsl_json

/** Get a "special json string" and return corresponding javascript
 * object. This string is a json cut to be not too deep. For more
 * information on this "special json special" see json.opa.
 *
 * @param {!string} str A string that may be parsed into a {e cut} JSON object.
 * @return {Object} JS object represented by the given string
 * @raise SyntaxError if given string doesn't respect "special" JSON
 * format
 */
##register string_to_native : string -> 'a
##args(str)
{
    /*This auxiliar function route the tree of given object. And get
      all object with at least one field equals null.  It's used by
      string_to_native.
      @param obj Object to route
      @return An array of object with field(s) equals null
    */
    var get_all_pre_null = function(obj){
        var tab = new Array();
        var aux = function(obj){
            if (typeof obj != 'object') return;
            for (var field in obj){
                if (obj[field] == null){
                    tab.push(obj);
                    break;
                }
            }
            for (var field in obj){
                if (obj[field] != null){
                    aux(obj[field])
                }
            }
        }
        aux(obj);
        return tab;
    }
    /* Main string_to_native */
    var tab = new Array();
    if (str.charAt(0) == '$'){
        str = str.substr(1);
        while (str.length > 0){
            var start_json = str.search(/[^0-9]/);
            // Numbers are parsed as decimal strings.
            var json_length = parseInt(str.substr(0, start_json), 10);
            var json = str.substr(start_json, json_length);
            tab.push(JSON.parse(json));
            str = str.substr(start_json + json_length);
        }
    } else {
        tab.push(JSON.parse(str));
    }
    var iroot = 0;
    for (var i = 1; i < tab.length;i=i){
        var pre_null = get_all_pre_null(tab[iroot]);
        for (var j = 0; j < pre_null.length; j++){
            for (var field in pre_null[j]){
                if(pre_null[j][field] == null){
                    pre_null[j][field] = tab[i];
                    i++;
                }
            }
        }
        iroot++;
    }
    return tab[0];
}

var json_void_result = make_onefield_record(static_field_of_name("Record"), make_simple_record(static_field_of_name("nil")));


/** This function create a Json object from a native javascript object.
 *
 * Note : order of record and/or list is preserved
 *
 * @param {!Object} obj A native JSON object (i.e. an object without methods, recursively)
 * @return {!Object} A json value, if possible, or none
 */
##register native_to_json:'a -> option(RPC.Json.json)
##args(obj)
{
    function aux(obj){
        switch (typeof obj){
        case "number":
            return is_float(obj) ? make_onefield_record("Float", obj) : make_onefield_record("Int", obj);
        case "string":
            return make_onefield_record("String", obj);
        case "boolean":
            return make_onefield_record("Bool", obj);
        case "object":
            if (obj instanceof Array) {
                var i;
                var len = obj.length;
                var tab = new Array(len);
                for(i = 0; i < len; ++i)
                    tab[i] = aux(obj[i])
                return make_onefield_record("List", js2list(tab));
            } else {
                var i;
                var tab = [];
                var empty = true;
                for(i in obj)
                {
                    //Construct the pair
                    var pair_cons = empty_constructor();
                    pair_cons = add_field(pair_cons, "f1", i);
                    pair_cons = add_field(pair_cons, "f2", aux(obj[i]));
                    var pair  = make_record(pair_cons);
                    tab.push(pair);
                    empty = false;
                }
                if (empty) return json_void_result;
                return make_onefield_record("Record", js2list(tab));
            }
        default:
            throw ({message:"This is not a valid object to transform to json", obj:obj });
        }
    }
    try {
        return js_some(aux(obj));
    } catch(e) {
        jlog(e);
        return js_none;
    }
}




/** This function create a Json object from a string.
 * @param str
 * @return A json value
 */
##register of_string : string -> option(RPC.Json.json)
##args(str)
{
    /* Construct a json value, from a native js object */
    try {
        var obj = %%Bsljson.Json.string_to_native%%(str);
        if (obj == null) return js_none;
        return %%Bsljson.Json.native_to_json%%(obj);
    } catch(e) {
        return js_none;
    }
}

##register of_json_repr :RPC.Json.private.native -> option(RPC.Json.json)
##args(obj)
{
        return %%BslJson.Json.native_to_json%%(obj);

}



##register json_list_empty : -> ll_json_list_repr
##args()
{ return new Array(); }

##register json_list_cons : RPC.Json.private.native, ll_json_list_repr -> ll_json_list_repr
##args(a,l)
{
    l.unshift(a);
    return l;
}

##register json_record_empty : -> ll_json_record_repr
##args()
{ return new Object(); }

##register json_record_cons : string, RPC.Json.private.native, ll_json_record_repr -> ll_json_record_repr
##args(a,b,r)
{
    r[a] = b;
    return r;
}




##register json_repr_int : int -> RPC.Json.private.native
##args(x)
{ return x; }

##register json_repr_float : float -> RPC.Json.private.native
##args(x)
{ return x; }

##register json_repr_string : string -> RPC.Json.private.native
##args(x)
{ return x; }

##register json_repr_bool : bool -> RPC.Json.private.native
##args(x)
{ return x; }

##register json_repr_void : -> RPC.Json.private.native
##args()
{ return {}; }

##register json_repr_array : ll_json_list_repr -> RPC.Json.private.native
##args(x)
{ return x; }

##register json_repr_record : ll_json_record_repr -> RPC.Json.private.native
##args(x)
{ return x; }


##endmodule
