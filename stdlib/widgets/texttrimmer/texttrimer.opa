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
