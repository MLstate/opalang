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

##register reload : -> void
  ##args()
  {
    location.reload();
  }

##register jump : string -> void
  ##args(str)
  {
    location.replace(str);
  }

##register confirm : string -> bool
  ##args(str)
  {
    return confirm(str);
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
