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
