/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
    if (res == null || res === "") return js_none
    return js_some(res)
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
