/*
    Copyright © 2012 MLstate

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
