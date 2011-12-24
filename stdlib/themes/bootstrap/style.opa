/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/

/*Author: Ida Swarczewskaja, MLstate */

/**
 * {1 About this module}
 * This module allows you to use Twitter's bootstrap style (http://twitter.github.com/bootstrap/) directly in your application.
 * It also includes several sets of icons.
 *
 * {2 Where should I start}
 * If you want to use the current version (1.3.0), just import this package.
 * Otherwise, import stdlib.themes.bootstrap.core and call: Bootstrap.import(VERSION)
 * before launching your server (note: no check is performed), or import  stdlib.themes.bootstrap-VERSION.
 *
 * {3 How to use icons}
 * There are different sizes:
 * - to use 16x16 pixels icon, set class "icon"
 * - to use 32x32 pixels icon, set class "icon32"
 * There are different colors:
 * - gray is the default
 * - to use black color, set class "icon-white"
 * - to use white color, set class "icon-black"
 * There are different icons:
 * - triangle (icon-triangle-n,icon-triangle-e, ...)
 * - arrow (icon-arrow-n, icon-arrowthick-n, icon-arrowreturn-se, ...)
 * - icon-plus, icon-minus, icon-close, icon-check, icon-help, icon-notice ...
 */

import stdlib.themes.bootstrap.core

current_bootstrap_version = "1.4.0"
current_bootstrap_url = "http://twitter.github.com/bootstrap/{current_bootstrap_version}/bootstrap.min.css"

do Resource.register_external_css(current_bootstrap_url)
