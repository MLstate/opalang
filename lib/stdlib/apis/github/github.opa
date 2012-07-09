/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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


type GitHub.plan = {
  name          : string
  collaborators : int
  space         : int
  private_repos : int
}

type GitHub.user_more = {
  total_private_repo_count : int
  collaborators            : int
  disk_usage               : int
  owned_private_repo_count : int
  private_gist_count       : int
  plan                     : GitHub.plan
}

type GitHub.user = {
  id                : int
  login             : string
  name              : string
  company           : string
  gravatar_id       : string
  created_at        : Date.date
  location          : string
  blog              : string
  public_repo_count : int
  public_gist_count : int
  followers_count   : int
  following_count   : int
  user_type         : string
  more              : option(GitHub.user_more)
}

type GitHub.repository = {
  name          : string
  owner         : string
  homepage      : string /* Not always returned */
  url           : string
  description   : string
  language      : string
  created_at    : Date.date
  pushed_at     : Date.date
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

type GitHub.commit_user = {
  name  : string
  login : string
  email : string
}
