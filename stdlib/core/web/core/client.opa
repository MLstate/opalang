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
