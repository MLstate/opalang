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

type environment_interface_engine =
    { X11       }/**The browser runs under Linux, FreeBSD, OpenBSD, etc.*/
  / { Windows   }/**The browser runs under Windows.*/
  / { Macintosh }/**The browser runs on a Macintosh (either MacOS X or Classic but not iPhone OS).*/
  / { iPhone    }/**The browser runs under iPhone OS, i.e. an iPhone or an iPod touch.*/
  / { Symbian   }/**The browser runs under a Symbian OS*/
  / { Unidentified }

type webkit_variant =
    { Safari: list(int) }
  / { Chrome: list(int) }
  / { Unidentified }

type bot_engine =
    { Googlebot: list(int) } /**Google's indexing engine.    */
  / { Msnbot:    list(int) } /**Microsoft's indexing engine. */
  / { Yahoobot: void       } /**Yahoo!'s indexing engine.    */

type renderer_engine =
    { Bot: bot_engine }      /**This is not a browser but a bot. */
  / { Gecko:   list(int) }   /**Gecko-based browser, e.g. Firefox.  */
  / { Trident: list(int) }   /**Trident-based browser, e.g. Internet Explorer.*/
  / { Webkit:  list(int); variant: webkit_variant }   /**WebKit-based browser, e.g. Safari or Google Chrome.*/
  / { Nokia:   list(int) }
  / { Presto:  list(int) }   /**Presto-based browser, e.g. Opera. */
  / { Unidentified }

type user_compat =
{
  environment: environment_interface_engine
  renderer: renderer_engine
}
