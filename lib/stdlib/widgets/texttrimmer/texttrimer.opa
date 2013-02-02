/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A simple text trimmer.
 * Could be optionally plugged into stringvalue (this widget is not meant for editing).
 *
 * The text will be trimmed until either it fits in the given width
 * (with dots and suffix if relevant) or it is trimmed to the given
  * minimal number of characters
 */

import stdlib.widgets.core

type WTextTrimmer.config = {
  width_px: int; // the text will be trimmed until its width is less than this
  dots: string;  // if the text has to be trimmed, it will have this suffix
  suffix: string;  // the text will always have this suffix (but after the dots in case of trimming)
  min_chars : int;
}

WTextTrimmer =
{{

  /*
   * {1 Configuration}
   */

  default_config : WTextTrimmer.config =
    {
      width_px = 100;
      dots = "...";
      suffix = "";
      min_chars = 1;
    }

  @client @private
  insert_loop(id : string, value : string, config : WTextTrimmer.config) =
    dom = #{id}
    ~{ width_px; dots; suffix; min_chars } = config
    insert_and_get_width(s) =
      dom = Dom.put_inside(dom, Dom.of_xhtml(<>{ s }</>))
      Dom.get_width(dom)
    do WStyler.set_dom({ style = css { visibility: hidden; white-space: nowrap; } }, id); // hide while we compute the right length in the DOM
    do if insert_and_get_width(value ^ suffix) > width_px
       then /* we have to trim */
         full_suffix = dots ^ suffix
         _ = for(value, String.drop_right(1, _), (s -> insert_and_get_width(s ^ full_suffix) > width_px && String.length(s) >= min_chars))
         void ;
    do WStyler.clear_dom(id); // it is safe to do this, because the id is fresh
    void;

  html(config : WTextTrimmer.config, value : string) : xhtml =
    id = "trimmed_{Dom.fresh_id()}" // not a user given id, because we have to overwrite both the element and its style
    <span id={id} onready={_ -> insert_loop(id,value,config)}></span>

}}
