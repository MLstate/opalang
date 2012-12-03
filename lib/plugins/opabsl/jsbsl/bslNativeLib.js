/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Really needed?
 * @side{server}
 * @externType caml_list('a)
 * @opaType list('a)
 * TODO : Rename caml -> native ?
 */

/**
 * @register {('a -> 'b), opa[list('a)] -> caml_list('b)}
 */
function opa_list_to_ocaml_list(f, l) {
  var x = new Array();
  var cl = l;
  var hd = cl.hd;
  while(hd){
      x.push(hd);
      cl = cl.tl;
      hd = cl.hd;
  }
  return x;
}

/**
 * @opacapi
 * @register {caml_list('a)} empty_list
 */
var empty_list = [];

/**
 * @opacapi
 * @register {'a, caml_list('a) -> caml_list('a)} cons
 */
function native_lib_cons(e, l) {
  return [e].concat(l);
}


/** @externType caml_tuple_2('a,'b) */
/** @opaType tuple_2('a, 'b) */
/**
 * @register {opa[tuple_2('a,'b)] -> caml_tuple_2('a,'b)}
 */
function ocaml_tuple_2(t) {
    return t;
}

/** @externType caml_tuple_4('a, 'b, 'c, 'd) */
/** @opaType tuple_4('a, 'b, 'c, 'd) */
/**
 * @register { opa[tuple_4('a,'b,'c,'d)] -> caml_tuple_4('a,'b,'c,'d)}
 */
function ocaml_tuple_4(t) {
    return t;
}

/**
 * @register {'a -> opa[list(string)]} record_fields
 */
function record_fields(x) {
  if (typeof(x) == 'object') {
    flds = [];
    for (fld in x) { flds.push(fld); };
    return js2list(flds);
  } else return {nil:js_void};
}

