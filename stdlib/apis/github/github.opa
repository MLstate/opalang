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
