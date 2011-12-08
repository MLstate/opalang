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

/*
    @author David Rajchenbach-Teller
**/



/**
 * Dimensions or coordinates. By convention, they're always given in pixels.
 */
type Dom.dimensions = {x_px: int; y_px: int}

type Dom.key_code  = int//See module [Dom.Key] for common constants

/**
 * The type of an event sent by the browser
 *
 * Note: this type will be extended in type. Don't depend on it having a fixed set of fields.
 */
type Dom.event =
{
  kind:           Dom.event.kind/**The kind of event, e.g. "click"*/
  mouse_position_on_page: Dom.dimensions/**The position of the mouse respective to the page*/
  key_code:       option(Dom.key_code)
  key_modifiers:  list({alt}/{ctrl}/{meta}/{shift})
  mouse_button:   option({left}/{middle}/{right}/{wheel:int})
  value_change:   option({from:string; to:string})
}

type Dom.propagation = {
  stop_propagation: bool
  prevent_default: bool
}

type Dom.event_option = {stop_propagation}/{prevent_default}

@opacapi
type Dom.event.kind =
    { click }
  / { dblclick }
  / { mouseup }
  / { mousedown }
  / { mouseover }
  / { mouseout }
  / { mousemove }
  / { mouseenter }
  / { mousewheel }
  / { mouseleave }
  / { keypress }
  / { keydown }
  / { keyup }
  / { load }
 /  { unload }
  / { ready }
  / { error }
  / { submit }
  / { focus }
  / { focusin }
  / { focusout }
  / { blur }
  / { change }
  / { scroll }
  / { select }
  / { newline }
  / { keyesc }
  / { resize }
  / { input } //HTML5
  / { paste } //HTML5
  / { custom : string}
