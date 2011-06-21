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
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

/**
 * GitHub generic API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github

/* Types returned by API */

type gdate = string

type GitHub.repository = {
  name          : string
  owner         : string
  homepage      : string /* Not always returned */
  url           : string
  description   : string
  language      : string
  created_at    : gdate
  pushed_at     : gdate
  size          : int
  private       : bool
  fork          : bool
  forks         : int
  watchers      : int
  has_downloads : bool
  has_wiki      : bool
  has_issues    : bool
  open_issues   : int
}

type GitHub.public_key = {
  title : string
  id    : int
  key   : string
}
