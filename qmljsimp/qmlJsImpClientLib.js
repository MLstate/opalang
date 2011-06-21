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

var js_void = {}
var _true = true
var _false = false
var js_none = {none:js_void}
function js_some(x) { return {some:x} }
function bool2obj(x) {
    // WONT BE IMPLEMENTED or change the name
    return x
}
function unsafe_dot(r,f) {
    switch (f) {
    case "true": r = dot_true(r); break
    case "false": r = dot_false(r); break
    case _size: r = undefined; break;
    default: r = r[f];
    }
    if (r === undefined) error("unsafe_dot failed on " + f);
    return r
}
function dot(r,f) {
    switch (f) {
    case "true": r = dot_true(r); break
    case "false": r = dot_false(r); break
    case _size: r = undefined; break;
    default: r = r[f];
    }
    if (r === undefined) return js_none
    return {some:r}
}
function udot(r,f) {
    switch (f) {
    case "true": return dot_true(r)
    case "false": return dot_false(r)
    case _size: return undefined
    default: return r[f];
    }
}
function fold_record(f, record, acc) {
    // FIXME? should try to use the enumeration of object properties
    // to avoid resorting later
    // but if we decide to do it, beware with the implementation of extendrecord
    switch (record) {
    case true: return f("true",js_void,acc)
    case false: return f("false",js_void,acc)
    default:
        var tab = new Array(), field, i = 0, n;
        for (field in record) _size === field || tab.push(field)
        tab.sort()
        for (n = tab.length; i < n; i++) {
            field = tab[i]
            acc = f(field, record[field], acc)
        }
        return acc
    }
}
function fold_2_record(f, record1, record2, acc) {
    switch (record1) {
    case true: return f("true", js_void, js_void, acc)
    case false: return f("false", js_void, js_void, acc)
    default:
        var tab = new Array(), field, i = 0, n;
        for (field in record1) _size === field || tab.push(field)
        tab.sort()
        for (n = tab.length; i < n; i++) {
            field = tab[i]
            acc = f(field, record1[field], record2[field], acc)
        }
        return acc
    }
}
function name_of_field(field) {
    return {some : field}
}
function field_of_name(name) {
    return {some : name}
}
function static_field_of_name(name) {
    return name
}
function empty_constructor() {
    var c = {} /* this object will be modified, do not share it with js_void */
    c[_size] = 0
    return c
}
function add_field(constructor, field, value) {
    if (field in constructor) error("add_field: trying to add the field " +field+" to the constructor "+constructor.toSource())
    constructor[field] = value;
    constructor[_size]++;
    return constructor
}
function make_record(constructor) {
    switch (size(constructor)) {
    case 0: return js_void;
    case 1: return un_uniformize_bool(constructor)
    default: return constructor;
    }
}
function make_simple_record(field) {
    switch (field) {
    case "true": return true
    case "false": return false
    default:
        var r = {}
        r[field] = js_void
        return r
    }
}
function make_onefield_record(field,value) {
    switch (field) {
    case "true": return build_true(value)
    case "false": return build_false(value)
    default:
        var r = {}
        r[field] = value;
        return r;
    }
}
function obj2bool() {
    // WONT BE IMPLEMENTED or change the name
    error("obj2bool not implemented")
}
function normalize_obj(x) { return x;
    // WONT BE IMPLEMENTED
    // error("normalize_obj not implemented")
}
function normalize_num() {
    // WONT BE IMPLEMENTED or change the name
    error("normalize_num not implemented")
}
function extendrecord() {
    // WONT BE IMPLEMENTED or change the name
    error("extendrecord not implemented")
}
function list2js(opalist) {
    var result = [], tl;
    while ((tl = opalist.tl) !== undefined) {
        result.push(opalist.hd);
        opalist = tl;
    }
    return result;
}
function js2list(array) {
    var i = array.length - 1, result = {nil : js_void}
    for (; i >= 0; i--) {
        result = {hd:array[i], tl:result}
    }
    return result
}
function option2js(obj) {
    return 'some' in obj ? obj.some : null
}
function option2jsu(obj) {
    return obj.some
}
function js2option(obj) {
    return obj == null ? js_none : {some : obj}
}

// copy pasted from qmlfunclientlib
function compare_raw(a,b) {
    switch (typeof a){
    case "number":
    case "string":
    case "boolean":
        return a > b ? 1 : a < b ? -1 : 0;
    case "object":
        if (size(a) < size(b)) return -1
        if (size(a) > size(b)) return 1

        var all_fields = {};//The set of fields appearing in [a] and [b]
        var field_name;
        // since size was called on a and b, _size is defined for both records
        // so the comparison cannot fail because _size is defined only on one record
        for(field_name in a) all_fields[field_name] = 0;
        for(field_name in b) all_fields[field_name] = 0;

        var sorted_fields = [];//The set of fields appearing in [a] and [b], sorted in consistent order
        for(field_name in all_fields) sorted_fields.push(field_name);

        //Now compare recursively
        var sorted_fields_length;
        var sorted_fields_index;
        for(sorted_fields_index = 0,
            sorted_fields_length = sorted_fields.length;
            sorted_fields_index < sorted_fields_length;
            sorted_fields_index++)
        {
            field_name  = sorted_fields[sorted_fields_index];
            var field_a = a[field_name];
            var field_b = b[field_name];
            if(field_a === undefined) return -1;
            if(field_b === undefined) return  1;
            var sorting = compare_raw(field_a, field_b);
            if(sorting != 0) return sorting;
        }

        return 0;
    default :
        error("[compare_raw] Cannot compare objects "+a+" and "+b);
    }

    error("compare_raw not implemented")
}

function tailcall_manager_call(fun, arg) {
    return fun.call(null, arg) ;
}

function tailcall_manager_apply(fun, args) {
    return fun.apply(null, args) ;
}

/* call to these functions are generated by the back end */
function build_true(e) {
    return e === js_void ? true : {'true':e}
}
function build_false(e) {
    return e === js_void ? false : {'false':e}
}

function dot_true(e) {
    return e === true ? js_void : e["true"]
}
function dot_false(e) {
    return e === false ? js_void : e["false"]
}

// we don't use _size but instead we use size`
// because in opa, we cannot write field names containing
// backquotes, so we cannot have collision between size` and
// user defined fields
var _size = "size`"
function size(x) {
    if (x === true || x === false) return 1;
    var i, s ;
    if (s = x[_size], s != undefined) return s;
    s = 0;
    for (i in x) s++;
    x[_size] = s;
    return(s)
}

// returns {true:js_void} or {false:js_void} instead of js booleans
// or else returns the given object
function uniformize_bool(o) {
    return o === true ? {'true':js_void} : o === false ? {'false':js_void} : o;
}
function un_uniformize_bool(o) {
    return o["true"] === js_void && size(o) === 1 ? true : o["false"] === js_void && size(o) === 1 ? false : o;
}

function extend_record(original_record,new_fields) {
    original_record = uniformize_bool(original_record)
    for (var i in original_record) {
        // beware: we must not copy the _size field!
        i === _size || i in new_fields || (new_fields[i] = original_record[i])
    }
    return un_uniformize_bool(new_fields)
}

/*
 * dynamic type checking
 */
function typeerror(key,msg) {
    error("TypeError on the bsl key %"+"%"+key+"%"+"% : "+msg)
}
function type_var(key,value) {
    value != null || typeerror(key,"type var vs value "+value)
}
function type_bool(key,value) {
    typeof value == "boolean" || typeerror(key,"type bool vs value "+value)
}
function type_void(key,value) {
    value === js_void || typeerror(key,"type void vs value "+value)
}
function type_option(key,value) {
    size(value) == 1 && ('none' in value || 'some' in value) || typeerror(key,"type option vs value "+value)
}
// special case: we accept the js_void, in case of a double projection
function type_native_void(key,value) {
    value == null || value === js_void || typeerror(key, "type native void vs value "+value)
}
function type_native_option(key,value) {
    value != null || value === null || typeerror(key, "type native option vs value "+value)
}
function type_int(key,value) {
    typeof value == "number" && Math.round(value) == value && !isNaN(value) || typeerror(key,"type int vs value "+value)
}
function type_float(key,value) {
    typeof value == "number" || typeerror(key,"type float vs value "+value)
}
function type_string(key,value) {
    typeof value == "string" || typeerror(key,"type string vs value "+value)
}
function type_char(key,value) {
    typeof value == "string" && value.length == 1 || typeerror(key,"type char vs value "+value)
}
function type_extern(key,value) {
    value != null || typeerror(key,"type extern vs value "+value)
}
function type_fun(key,value) {
    typeof value == "function" || typeerror(key,"type fun vs value "+value)
}
function type_fun_arity(key,args,arity) {
    args.length == arity || typeerror(key,"type fun of arity="+arity+" vs arguments received: "+args.length)
}
function type_opavalue(key,value) {
    value != null || typeerror(key,"type opavalue vs value "+value)
}

/*
 * other dynamic check
 */
function assert_length(args) {
    var n = args.length;
    while (--n >= 0) {
        args[n] === undefined && error("arity error, "+n+"th argument is undefined")
    }
}

/*
 * stuff for dynamic pattern matching
 */
function fields_indexes(fs) {
    return fs;
}

function field_index(fi,f) {
   return f;
}

function dot_with_field_index(r,fi) {
   return  unsafe_dot(r,fi);
}

function patterns_indexes(patterns) {
   return patterns;
}

function compare_structure_same(fields,r) {
   for (var i=0;i<fields.length;i++){
     var field = fields[i];
     if (unsafe_dot(r,field)==undefined) return false;
   };
   return true;
}

function compare_structure(patterns,r1,r2) {
   // TODO in case r1 and r2 has same structure reduce calls remove one compare_structure_same per loop, see serverLib.ml
   for(var ifs=0;ifs<patterns.length;ifs++){
     var fields = patterns[ifs];
     if( compare_structure_same(fields,r1) ){
       if( compare_structure_same(fields,r2) ) return ifs;
       else return -1; /* gt */
     };
     if( compare_structure_same(fields,r2) ) return -2; /* lt */
   };
   return error("compare_structure"+patterns+"\n"+r1);
}
