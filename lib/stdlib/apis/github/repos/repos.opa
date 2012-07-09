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
 * GitHub repository API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.repos
import stdlib.apis.github
import stdlib.apis.github.lib

type GHRepos.value =
    { description   : string }
  / { homepage      : string }
  / { has_wiki      : bool }
  / { has_issues    : bool }
  / { has_downloads : bool }

/**
 * Type of repos search parameters
 * Pages start at 1
 */
type GHRepos.search_params = {
  request  : string
  language : string
  page     : int
}

type GHRepos.new = {
  name        : string
  description : string
  homepage    : string
  public      : bool
}

/* Types returned by API */

type GitHub.full_watcher = {
  login        : string
  email        : string
  name         : string
  blog         : string
  location     : string
  gravatar_id  : string
  watcher_type : string
}

type GitHub.contributor = {
  login            : string
  name             : string
  email            : string
  company          : string
  gravatar_id      : string
  location         : string
  blog             : string
  contributor_type : string
  contributions    : int
}

type GitHub.language = {
  name  : string
  lines : string
}

type GitHub.tag = {
  name : string
  tag  : string
}

type GitHub.branch = {
  name : string
  head : string
}



@private GHRp = {{

  explode_repos_value(v) =
    ( match v : GHRepos.value with
      | ~{description}   -> ("description",description)
      | ~{homepage}      -> ("homepage",homepage)
      | ~{has_wiki}      -> ("has_wiki","{has_wiki}")
      | ~{has_issues}    -> ("has_issues","{has_issues}")
      | ~{has_downloads} -> ("has_downloads","{has_downloads}")
    ) |> (a,b) -> ("values[{a}]",b)

  @private GP = GHParse

  get_watcher(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("login") then
      res = {
        login        = m.str("login")
        email        = m.str("email")
        name         = m.str("name")
        blog         = m.str("blog")
        location     = m.str("location")
        gravatar_id  = m.str("gravatar_id")
        watcher_type = m.str("watcher_type")
      } : GitHub.full_watcher
      {some=res}
    else {none}

  get_contributor(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("contributions") then
      res = {
        login            = m.str("login")
        name             = m.str("name")
        email            = m.str("email")
        company          = m.str("company")
        gravatar_id      = m.str("gravatar-id")
        location         = m.str("location")
        blog             = m.str("blog")
        contributor_type = m.str("contributor_type")
        contributions    = m.int("contributions")
      } : GitHub.contributor
      {some=res}
    else {none}

  get_languages(srcmap) =
    aux(name,v,acc) = match v with
      | {Int=lines} -> List.add(~{name lines},acc)
      | _ -> acc
    GP.obj_list(srcmap, aux)

  get_tags(srcmap) =
    aux(name,v,acc) = match v with
      | {String=tag} -> List.add(~{name tag},acc)
      | _ -> acc
    GP.obj_list(srcmap, aux)

  get_branches(srcmap) =
    aux(name,v,acc) = match v with
      | {String=head} -> List.add(~{name head},acc)
      | _ -> acc
    GP.obj_list(srcmap, aux)

  one_repo(res) =
    GP.dewrap_obj(res, "repository", GP.get_repo(false))

  multiple_repos(res) =
    GP.dewrap_list(res, "repositories", GP.get_repo(false))

  collaborators(res) =
    GP.multiple_strings(res, "collaborators")

  watchers(res) =
    GP.multiple_strings(res, "watchers")

  fullwatchers(res) =
    GP.dewrap_list(res, "watchers", get_watcher)

  network(res) =
    GP.dewrap_list(res, "network", GP.get_repo(false))

  multiple_public_keys(res) =
    GP.dewrap_list(res, "public_keys", GP.get_public_key)

  contributors(res) =
    GP.dewrap_list(res, "network", get_contributor)

  languages(res) =
    GP.dewrap_obj(res, "languages", get_languages)

  tags(res) =
    GP.dewrap_obj(res, "tags", get_tags)

  branches(res) =
    GP.dewrap_obj(res, "branched", get_branches)

}}

GHRepos = {{

  @private GP = GHParse

  @private p(o:string,r:string) = "{o}/{r}"

  /* Search and display */

  default_search_params(req) = {
    request  = req
    language = ""
    page     = 0
  } : GHRepos.search_params

  full_search(params:GHRepos.search_params) =
    path = "/repos/search/{params.request}"
    data =
      (if params.page > 0 then
        [("start_page", "{params.page}")]
       else [] )
      |> GHLib.add_if("language", params.language, _)
    GHLib.api_get(path, data, GHRp.multiple_repos)

  search(req) = full_search(default_search_params(req))

  show_user_repos(owner:string, page) =
    path = "/repos/show/{owner}"
    data = if page > 0 then [("page", "{page}")] else []
    GHLib.api_get(path, data, GHRp.multiple_repos)

  @private generic_show(what:string, owner:string, repo:string, token, parse_fun) =
    path = "/repos/show/{p(owner,repo)}/{what}"
    if token == "" then
      GHLib.api_get(path, [], parse_fun)
    else GHLib.api_get_logged(path, token, parse_fun)

  show(owner, repo) = generic_show("", owner, repo, "", GHRp.one_repo)

  /* Fork, create and delete */

  @private generic_post(act:string, owner:string, repo:string, token, parse_fun) =
    path = "/repos/{act}/{p(owner,repo)}"
    GHLib.api_post_logged(path, token, parse_fun)

  fork(owner, repo, token) =
    generic_post("fork", owner, repo, token, GHRp.one_repo)

  default_new_repo(name) = {
    name        = name
    description = ""
    homepage    = ""
    public      = true
  } : GHRepos.new

  create(new_repos:GHRepos.new, token) =
    path = "/repos/create"
    data =
      [("access_token", token), ("name", new_repos.name),
       ("description", new_repos.description),
       ("homepage", new_repos.homepage),
       ("public", (if new_repos.public then "1" else "0"))]
    GHLib.api_post(path, data, GHRp.one_repo)

  create_simple(name, token) =
    create(default_new_repo(name), token)

  delete(owner, repo, token) =
    generic_post("delete", owner, repo, token, GP.one_string(_,"delete_token"))

  confirm_delete(owner, repo, delete_token, token) =
    path = "/repos/delete/{p(owner,repo)}"
    data = [("access_token", token),
            ("delete_token", delete_token)]
    GHLib.api_post(path, data, GP.one_string(_,"status"))

  /* Update */

  update(owner, repo, updates, token) =
    path = "/repos/show/{p(owner,repo)}"
    data = List.map(GHRp.explode_repos_value, updates)
      |> List.add(("login", owner), _)
      |> List.add(("access_token", token), _)
    GHLib.api_post(path, data, GHRp.one_repo)

  set_private(owner, repo, token) =
    generic_post("set/private", owner, repo, token, GHRp.one_repo)

  set_public(owner, repo, token) =
    generic_post("set/public", owner, repo, token, GHRp.one_repo)

  /* Deploy keys */

  get_keys(owner, repo, token) =
    path = "/repos/keys/{p(owner,repo)}"
    GHLib.api_get_logged(path, token, GHRp.multiple_public_keys)

  add_key(title, key, owner, repo, token) =
    path = "/repos/key/{p(owner,repo)}/add"
    data = [("title", title), ("key", key),
            ("access_token", token)]
    GHLib.api_post(path, data, GHRp.multiple_public_keys)

  remove_key(id, owner, repo, token) =
    path = "/repos/key/{p(owner,repo)}/remove"
    data = [("id", id), ("access_token", token)]
    GHLib.api_post(path, data, GHRp.multiple_public_keys)

  /* Collaborators */

  get_collaborators(owner, repo, token) =
     generic_show("collaborators", owner, repo, token, GHRp.collaborators)

  pushable(token) =
    path = "/repos/pushable"
    GHLib.api_get_logged(path, token, GHRp.multiple_repos)

  get_teams(owner, repo) =
    generic_show("teams", owner, repo, "", some)

  add_collaborator(collaborator, owner, repo, token) =
    path = "/repos/collaborators/{p(owner,repo)}/add/{collaborator}"
    GHLib.api_post_logged(path, token, GHRp.collaborators)

  remove_collaborator(collaborator, owner, repo, token) =
    path = "/repos/collaborators/{p(owner,repo)}/remove/{collaborator}"
    GHLib.api_post_logged(path, token, GHRp.collaborators)

  /* Watch */

  get_watched(user) =
    path = "/repos/watched/{user}"
    GHLib.api_get(path, [], GHRp.multiple_repos)

  get_watchers(user, repo) =
    generic_show("watchers", user, repo, "", GHRp.watchers)

  get_full_watchers(user, repo) =
    generic_show("watchers?full=1", user, repo, "", GHRp.fullwatchers)

  watch(owner, repo, token) =
    generic_post("watch", owner, repo, token, GHRp.one_repo)

  unwatch(owner, repo, token) =
    generic_post("unwatch", owner, repo, token, GHRp.one_repo)

  /* Other */

  /**
   * Get a list of collaboratos on a project,
   * sorted by number of contributions.
   * - with_anon : Include non-users of GitHub
   */
  get_contributors(owner, repo, with_anon) =
    what =
      if with_anon then "contributors/anon"
      else "contributors"
    generic_show(what, owner, repo, "", GHRp.contributors)

  get_network(owner, repo) =
    generic_show("network", owner, repo, "", GHRp.network)

  get_languages(owner, repo) =
    generic_show("languages", owner, repo, "", GHRp.languages)

  get_tags(owner, repo) =
    generic_show("tags", owner, repo, "", GHRp.tags)

  get_branches(owner, repo) =
    generic_show("branches", owner, repo, "", GHRp.branches)

}}
