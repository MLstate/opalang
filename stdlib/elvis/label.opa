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
 * Elvis Labels
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Simple display}
 *
 * Display some xhtml content as an elvis.
 */

type ELabel.sources = {}
@abstract type ELabel.implementation = {}
type ELabel.elvis = Elvis.elvis(ELabel.sources, ELabel.implementation)

ELabel =
{{
   simple(content:xhtml): ELabel.elvis =
      make(_ -> content)

   make(content: Elvis.theme -> xhtml): ELabel.elvis =
      display(theme) =
         xhtml = content(theme)
         dom   = Dom.of_xhtml(xhtml)
         {~xhtml ~dom}
      Elvis.make(void, (void), display)

}}
