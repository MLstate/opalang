/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
