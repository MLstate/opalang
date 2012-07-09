/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

##extern-type JsWindow
//extern-type position
//extern-type jquery
##extern-type list('a)
##extern-type client_window_element

##module client
##register setTitle : string -> void
  ##args(str)
  {
    document.title = str;
  }

##register reload : bool -> void
  ##args(forceget)
  {
    location.reload(forceget);
  }

##register jump : string -> void
  ##args(str)
  {
    location.replace(str);
  }

##register alert : string -> void
  ##args(str)
  {
    alert(str);
  }

##register confirm : string -> bool
  ##args(str)
  {
    return confirm(str);
  }

##register prompt : string, string -> option(string)
  ##args(str, dft)
  {
    var res = window.prompt(str, dft);
    if (res == null) return js_none // cancel
    return js_some(res) // ok
  }

##register goto : string -> void
  ##args(str)
  {
    location.href = str;
  }

##register stop : -> void
  ##args()
  {
    window.stop();
  }

##register width : -> int
  ##args()
  {
    return window.innerWidth;
  }

##register height : -> int
  ##args()
  {
    return window.innerHeight;
  }

##register historyGoto : int -> void
  ##args(nbr)
  {
    window.history.go(nbr);
  }

##register historyLength : -> int
  ##args()
  {
    return window.history.length;
  }

##register the_window: -> client_window_element
##args()
{
  return window;
}

##register [backend : qmljs] winopen2 : string, client_window_element, list(string), bool, string -> client_window_element
##args(url, win, spec, repl, uid)
{
  return win.open(url, uid, list2js(spec).join(', '), repl);
}

//closes the *current* window
##register winclose : client_window_element -> void
##args(win)
{
  win.close();
}

##endmodule
