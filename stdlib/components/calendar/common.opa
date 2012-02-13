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
 * @category COMPONENT
 * @author Adam Koprowski, 2011
 */


import stdlib.widgets.core

@package style_css(css_style) =
  WStyler.add(WStyler.make_style(css_style), _)

@package style_stl(styler) =
  WStyler.add(styler, _)

@package style_stl_css(styler, css_style) =
  s = WStyler.merge([styler, WStyler.make_style(css_style)])
  WStyler.add(s, _)
