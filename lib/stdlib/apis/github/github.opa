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

package stdlib.apis.github

/* Types returned by API */

type GitHub.encoding = {utf8} / {base64}

type GitHub.state = {open} / {closed}
type GitHub.direction = {asc} / {desc}

type GitHub.plan = {
  name          : string
  space         : int
  private_repos : int
}

/**
 * Type of a GitHub user provided to various functions
 */
type GitHub.user_id =
    { self  : string } /** Token of user - provides more information */
  / { login : string } /** Login of user */

type GitHub.user_more = {
  total_private_repos : int
  collaborators       : int
  disk_usage          : int
  owned_private_repos : int
  private_gists       : int
  plan                : GitHub.plan
}

type GitHub.short_user = {
  id            : int
  login         : string
  url           : string
  gravatar_id   : string
  avatar_url    : string
  `type`        : string // Not always present
  contributions : int // Only if as contributor
}

type GitHub.user = {
  id           : int
  login        : string
  name         : string
  company      : string
  gravatar_id  : string
  created_at   : Date.date
  location     : string
  blog         : string
  public_repos : int
  public_gists : int
  followers    : int
  following    : int
  user_type    : string
  avatar_url   : string
  url          : string
  html_url     : string
  more         : option(GitHub.user_more)
}

type GitHub.repository = {
  url           : string
  html_url      : string
  clone_url     : string
  git_url       : string
  ssh_url       : string
  svn_url       : string
  mirror_url    : string
  id            : int
  owner         : GitHub.short_user
  name          : string
  full_name     : string
  description   : string
  homepage      : string /* Not always returned */
  language      : string
  `private`     : bool
  fork          : bool
  forks         : int
  watchers      : int
  size          : int
  master_branch : string
  open_issues   : int
  pushed_at     : Date.date
  created_at    : Date.date
  updated_at    : Date.date
  organization  : option(GitHub.short_user)
  parent        : option(GitHub.repository)
  source        : option(GitHub.repository)
  has_issues    : option(bool)
  has_wiki      : option(bool)
  has_downloads : option(bool)
}

type GitHub.repo_comment = {
  html_url   : string
  url        : string
  id         : int
  body       : string
  path       : string
  position   : int
  line       : int
  commit_id  : string
  user       : GitHub.short_user
  created_at : Date.date
  updated_at : Date.date
}

type GitHub.download = {
  url            : string
  html_url       : string
  id             : int
  name           : string
  description    : string
  size           : int
  download_count : int
  content_type   : string
}

type GitHub.gist_file =
  (string,{
     size     : int
     filename : string
     raw_url  : string
     content  : string
   })

type GitHub.gist_forks = {
  user       : GitHub.short_user
  url        : string
  created_at : Date.date
}

type GitHub.gist_history = {
  user          : GitHub.short_user
  version       : string
  url           : string
  change_status : option({ deletions : int additions : int total : int })
  committed_at  : Date.date
}

type GitHub.gist = {
  id           : int
  public       : bool
  description  : string
  user         : GitHub.short_user
  url          : string
  html_url     : string
  git_push_url : string
  git_pull_url : string
  comments     : int
  created_at   : Date.date
  updated_at   : Date.date
  files        : list(GitHub.gist_file)
  forks        : list(GitHub.gist_forks)
  history      : list(GitHub.gist_history)
}

type GitHub.label = {
  url   : string
  name  : string
  color : string
}

type GitHub.milestone = {
  url           : string
  number        : int
  state         : {open}/{closed}
  title         : string
  description   : string
  creator       : GitHub.short_user
  open_issues   : int
  closed_issues : int
  created_at    : Date.date
  due_on        : Date.date
}

type GitHub.pull_request = {
  html_url  : string
  diff_url  : string
  patch_url : string
}

type GitHub.issue = {
  url          : string
  html_url     : string
  number       : int
  state        : {open}/{closed}
  title        : string
  body         : string
  user         : GitHub.short_user
  labels       : list(GitHub.label)
  assignee     : GitHub.short_user
  milestone    : option(GitHub.milestone)
  comments     : int
  pull_request : option(GitHub.pull_request)
  closed_at    : Date.date
  created_at   : Date.date
  updated_at   : Date.date
}

type GitHub.issue_comment = {
  id          : int
  url         : string
  body        : string
  user        : GitHub.short_user
  created_at  : Date.date
  updated_at  : Date.date
}

type GitHub.ref = {
  label      : string
  ref        : string
  sha        : string
  user       : GitHub.short_user
  repository : GitHub.repository
}

type GitHub.pull_req = {
  url           : string
  html_url      : string
  diff_url      : string
  patch_url     : string
  issue_url     : string
  number        : int
  state         : {open}/{closed:Date.date}/{other:string}
  title         : string
  body          : string
  created_at    : Date.date
  updated_at    : Date.date
  closed_at     : Date.date
  merged_at     : Date.date
  head          : GitHub.ref
  base          : GitHub.ref
  _links        : GitHub.links
  user          : GitHub.short_user
  merged        : bool
  mergeable     : bool
  merged_by     : GitHub.short_user
  comments      : int
  commits       : int
  additions     : int
  deletions     : int
  changed_files : int
}

type GitHub.public_key = {
  id       : int
  key      : string
  title    : string
  url      : string
  verified : bool
}

type GitHub.commit_user = {
  name  : string
  email : string
  date  : Date.date
}

type GitHub.url_sha = {
  url : string
  sha : string
}

type GitHub.commit = {
  author        : GitHub.commit_user
  url           : string
  sha           : string
  message       : string
  tree          : GitHub.url_sha
  committer     : GitHub.commit_user
}

type GitHub.stats = {
  additions : int
  deletions : int
  total     : int
}

type GitHub.file = {
  sha       : string
  filename  : string
  status    : string
  additions : int
  deletions : int
  changes   : int
  blob_url  : string
  raw_url   : string
  patch     : string
}

type GitHub.full_commit = {
  sha           : string
  commit        : GitHub.commit
  author        : GitHub.short_user
  parents       : list(GitHub.url_sha)
  url           : string
  committer     : GitHub.short_user
  stats         : option(GitHub.stats)
  files         : list(GitHub.file)
}

type GitHub.links = {
  self            : { href : string }
  html            : { href : string }
  comments        : { href : string }
  review_comments : { href : string }
  pull_request    : { href : string }
  git             : { href : string }
}

type GitHub.links_no_href = {
  self            : string
  html            : string
  comments        : string
  review_comments : string
  pull_request    : string
  git             : string
}

type GitHub.id_name_url = {
  url  : string
  name : string
  id   : int
}

