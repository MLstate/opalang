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
/**
 * Toggle element management
 * link with the javascript
**/

OpaDocToggle = {{

  class = "toggle_elt"

  /**
   * The call to javascript to insert
  **/
  @private onclick(i:int) = "toggle_elt({i});return false"

  /**
   * Make a new fresh maker for elements.
   * Needed to be different for each new html page
  **/
  fresh()=
    Fresh.mutable(i->("toggle_elt_{i}", onclick(i)))

  onclick_hide_all = "toggle_all_elts(false); return false"
  onclick_show_all = "toggle_all_elts(true); return false"
}}
