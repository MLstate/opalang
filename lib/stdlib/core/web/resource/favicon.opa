/*
    Copyright © 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Format of a favicon, mixes type and
 */
type Favicon.format =
  { ico }
/ { png }
/ { gif }
/ { `apple-touch-icon` }
/ { `apple-touch-icon-precomposed` }

/**
 * Type of a favicon meant to be registered in a web page
 */
type Favicon.t = {
  format : Favicon.format
  path : string
}

/**
 * Module allowing to create and generate favicon links.
 * Meant to be used as a Server.handler,
 * or to be registered with Resource.register_favicon.
 */
Favicon = {{

  /**
   * Creates a favicon
   */
  make(format, path) = {
    ~format
    ~path
  }

  /**
   * Converts a favicon into an html link
   */
  to_html(f) =
    match f.format
    { ico } -> <link rel="icon" type="image/x-icon" href="{f.path}"/>
    { png } -> <link rel="icon" type="image/png" href="{f.path}"/>
    { gif } -> <link rel="icon" type="image/gif" href="{f.path}"/>
    { `apple-touch-icon` } -> <link rel="apple-touch-icon" href="{f.path}"/>
    { `apple-touch-icon-precomposed` } -> <link rel="apple-touch-icon-precomposed" href="{f.path}"/>

}}
