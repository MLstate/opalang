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
import stdlib.themes

do
name = "default"
files_css = @static_include_directory("stdlib/themes/default/css")
// files_img = @static_include_directory("stdlib/themes/default/images")
// _ = Themes.load_theme(name, StringMap.union(files_css,files_img))
Themes.load_theme(name, files_css)
