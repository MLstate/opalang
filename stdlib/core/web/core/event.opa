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

type Dom.event_propagation = {
  stop_propagation: bool
  prevent_default: bool
}

type Dom.event_option =
  {propagation_handler:(Dom.event -> Dom.event_propagation)}
/ {stop_propagation}
/ {prevent_default}

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
