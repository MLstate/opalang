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
 * GitHub organization API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.orgs
import stdlib.apis.github
import stdlib.apis.github.lib

type GHOrgs.value =
    { name          : string }
  / { email         : string }
  / { blog          : string }
  / { company       : string }
  / { location      : string }
  / { billing_email : string }

/* Types returned by API */

type GitHub.organization = {
  name              : string
  login             : string
  company           : string
  email             : string
  gravatar_id       : string
  location          : string
  created_at        : Date.date
  blog              : string
  public_gist_count : int
  public_repo_count : int
  id                : int
  org_type          : string
  followers_count   : string
// permission : ??
}

type GitHub.permission = {admin} / {push} / {pull}

type GitHub.team = {
  name       : string
  id         : int
  permission : GitHub.permission
}

@private GHOp = {{

  explode_orgs_values(v) =
    ( match v : GHOrgs.value with
      | ~{name}          -> ("name",name)
      | ~{email}         -> ("email",email)
      | ~{blog}          -> ("blog",blog)
      | ~{company}       -> ("company",company)
      | ~{location}      -> ("location",location)
      | ~{billing_email} -> ("billing_email",billing_email)
    ) |> (a:string,b:string) -> ("organization[{a}]",b)

  perm_to_str(p) =
    match p : GitHub.permission with
    | {admin} -> "admin"
    | {push}  -> "push"
    | {pull}  -> "pull"

  perm_from_str(p) : GitHub.permission =
    match p : string with
    | "admin" -> {admin}
    | "push"  -> {push}
    | "pull"  -> {pull}
    | _       -> {pull}

  @private GP = GHParse

  get_org(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("gravatar_id") then
      res = {
        name              = m.str("name")
        login             = m.str("login")
        company           = m.str("company")
        email             = m.str("email")
        gravatar_id       = m.str("gravatar_id")
        location          = m.str("location")
        created_at        = m.date("created_at")
        blog              = m.str("blog")
        public_gist_count = m.int("public_gist_count")
        public_repo_count = m.int("public_repo_count")
        id                = m.int("id")
        org_type          = m.str("org_type")
        followers_count   = m.str("followers_count")
      } : GitHub.organization
      some(res)
    else none

  get_team(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("name") then
      res = {
        name       = m.str("name")
        id         = m.int("id")
        permission = m.str("permission") |> perm_from_str
      } : GitHub.team
      some(res)
    else none

  one_org(res) =
    GP.dewrap_obj(res, "organizations", get_org)

  multiple_orgs(res) =
    GP.dewrap_list(res, "organizations", get_org)

  multiple_repos(res) =
    GP.dewrap_list(res, "repositories", GP.get_repo(false))

  multiple_users(res) =
    GP.dewrap_list(res, "users", GP.get_user)

  one_team(res) =
    GP.dewrap_obj(res, "team", get_team)

  multiple_teams(res) =
    GP.dewrap_list(res, "teams", get_team)

}}

GHOrgs = {{

  show(org:string) =
    path = "/organizations/{org}"
    GHLib.api_get(path, [], GHOp.one_org)

  update(org:string, updates, token) =
    path = "/organizations/{org}"
    data = List.map(GHOp.explode_orgs_values, updates)
      |> List.add(("access_token", token), _)
    GHLib.api_post(path, data, GHOp.one_org)

  get_user_organizations(user:string) =
    path = "/user/show/{user}/organizations"
    GHLib.api_get(path, [], GHOp.multiple_orgs)

  get_current_user_organizations(token) =
    path = "/organizations"
    GHLib.api_get_logged(path, token, GHOp.multiple_orgs)

 /**
  * List all repositories across all the organizations
  * that you can access.
  *
  * @param inc_all_owned If [false], returns all repos that current user can explicitly access through teams. If [true], adds all repos from all organizations owned by user.
  * @param inc_public If [true], adds public repos from organizations that current user has no explicit access to.
  * @param token User token
  */
  get_current_user_repos(inc_all_owned, inc_public, token) =
    path = "/organizations/repositories"
    data = [("access_token",token)]
      |> (if inc_all_owned then List.add(("owned","1"), _)
          else identity)
      |> (if inc_public then List.add(("public","1"), _)
          else identity)
    GHLib.api_get(path, data, GHOp.multiple_repos)

  get_org_owners(org:string, token) =
    path = "/organizations/{org}/owners"
    GHLib.api_get_logged(path, token, GHOp.multiple_users)

  get_org_public_members(org:string) =
    path = "/organizations/{org}/public_members"
    GHLib.api_get(path, [], GHOp.multiple_users)

  get_org_public_repos(org:string) =
    path = "/organizations/{org}/public_repositories"
    GHLib.api_get(path, [], GHOp.multiple_repos)


  get_teams(org:string, token) =
    path = "/organizations/{org}/teams"
    GHLib.api_get_logged(path, token, GHOp.multiple_teams)

  create_team(org:string, name, permission, repos, token) =
    path = "/organizations/{org}/teams"
    repos = List.map(
      ((a:string,b:string)-> ("team[repo_names][]","{a}/{b}")),
      repos)
    data =
      [("access_token",token), ("team[name]",name),
       ("team[permission]",GHOp.perm_to_str(permission))]
      |> List.append(repos,_)
    GHLib.api_post(path, data, GHOp.one_team)

  get_team(id:int, token) =
    path = "/teams/{id}"
    GHLib.api_get_logged(path, token, GHOp.one_team)

  /* Uncertain: TESTME */
  del_team(id:int, token) =
    path = "/teams/{id}/delete"
    data = [("access_token",token)]
    GHLib.api_post(path, data, some)

  get_team_members(id:int, token) =
    path = "/teams/{id}/members"
    GHLib.api_get_logged(path, token, some)

  add_team_member(id:int, name, token) =
    path = "/teams/{id}/members"
    data = [("access_token",token),("name",name)]
    GHLib.api_post(path, data, some)

  /* Uncertain: TESTME */
  remove_team_member(id:int, name, token) =
    path = "/teams/{id}/members/delete"
    data = [("access_token",token),("name",name)]
    GHLib.api_post(path, data, some)


  get_team_repos(id:int, token) =
    path = "/teams/{id}/repositories"
    GHLib.api_get_logged(path, token, GHOp.multiple_repos)

  add_team_repo(id:int, owner:string, repo:string, token) =
    path = "/teams/{id}/repositories"
    data = [("access_token",token),("name","{owner}/{repo}")]
    GHLib.api_post(path, data, some)

  /* Uncertain: TESTME */
  remove_team_repo(id:int, owner:string, repo:string, token) =
    path = "/teams/{id}/repositories/delete"
    data = [("access_token",token),("name","{owner}/{repo}")]
    GHLib.api_post(path, data, some)

}}
