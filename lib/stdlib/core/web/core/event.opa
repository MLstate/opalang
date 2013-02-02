/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
