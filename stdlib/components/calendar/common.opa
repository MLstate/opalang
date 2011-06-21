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
