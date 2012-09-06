/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/** @externType JsWindow */
/** @externType list('a) */
/** @externType client_window_element */

/** @module client */

  /**
   * @register {string -> void}
   */
  function setTitle(str) {
    document.title = str;
  }

  /**
   * @register {bool -> void}
   */
  function reload(forceget) {
    location.reload(forceget);
  }

  /**
   * @register {string -> void}
   */
  function jump(str) {
    location.replace(str);
  }

  /**
   * @register {string -> void}
   */
  function alert(str) {
    window.alert(str);
  }

  /**
   * @register {string -> bool}
   */
  function confirm(str) {
    return window.confirm(str);
  }

  /**
   * @register {string, string -> opa[option(string)]}
   */
  function prompt(str, dft) {
    var res = window.prompt(str, dft);
    if (res == null) return js_none // cancel
    return js_some(res) // ok
  }

  /**
   * @register {string -> void}
   */
  function goto(str) {
    location.href = str;
  }

  /**
   * @register { -> void} stop
   */
  function client_stop() {
    window.stop();
  }

  /**
   * @register { -> int}
   */
  function width() {
    return window.innerWidth;
  }

  /**
   * @register { -> int}
   */
  function height() {
    return window.innerHeight;
  }

  /**
   * @register {int -> void}
   */
  function historyGoto(nbr) {
    window.history.go(nbr);
  }

  /**
   * @register {-> int}
   */
  function historyLength() {
    return window.history.length;
  }

  /**
   * @register {-> client_window_element}
   */
  function the_window() {
    return window;
  }

  /**
   * @register {string, client_window_element, list(string), bool, string -> client_window_element}
   * @backend qmljs
   */
  function winopen2(url, win, spec, repl, uid) {
    return win.open(url, uid, list2js(spec).join(', '), repl);
  }

  //closes the *current* window
  /**
   * @register {client_window_element -> void}
   */
  function winclose(win) {
    win.close();
  }

/** @endModule */
