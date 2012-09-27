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
 * GitHub organization API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

package stdlib.apis.github.orgs
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
  login        : string
  id           : int
  url          : string
  avatar_url   : string
  name         : string
  company      : string
  blog         : string
  location     : string
  email        : string
  public_repos : int
  public_gists : int
  followers    : int
  following    : int
  html_url     : string
  created_at   : Date.date
  org_type     : string
  total_private_repos : int
  owned_private_repos : int
  private_gists : int
  disk_usage : int
  collaborators : int
  billing_email : string
  plan: option(GitHub.plan)
}

type GitHub.permission = {admin} / {push} / {pull}

type GitHub.team = {
  url           : string
  name          : string
  id            : int
  members_count : int
  permission    : GitHub.permission
  repos_count   : int
}

@private GHORp = {{

  @private GP = GHParse

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

  get_org(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      res = {
        login               = m.str("login")
        id                  = GP.get_id(m)
        url                 = m.str("url")
        avatar_url          = m.str("avatar_url")
        name                = m.str("name")
        company             = m.str("company")
        blog                = m.str("blog")
        location            = m.str("location")
        email               = m.str("email")
        public_repos        = m.int("public_repos")
        public_gists        = m.int("public_gists")
        followers           = m.int("followers")
        following           = m.int("following")
        html_url            = m.str("html_url")
        created_at          = m.date("created_at")
        org_type            = m.str("org_type")
        total_private_repos = m.int("total_private_repos")
        owned_private_repos = m.int("owned_private_repos")
        private_gists       = m.int("private_gists")
        disk_usage          = m.int("disk_usage")
        collaborators       = m.int("collaborators")
        billing_email       = m.str("billing_email")
        plan                = GP.get_rec(m, "plan", GP.get_plan_opt)
      } : GitHub.organization
      some(res)
    else none

  get_team(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      res = {
        id                  = GP.get_id(m)
        url                 = m.str("url")
        name                = m.str("name")
        members_count       = m.int("members_count")
        permission          = perm_from_str(m.str("permission"))
        repos_count         = m.int("repos_count")
      } : GitHub.team
      some(res)
    else none

  one_org(res) = GP.dewrap_whole_obj(res, get_org)
  multiple_orgs(res) = GP.dewrap_whole_list(res, get_org)

  multiple_repos(res) = GP.dewrap_whole_list(res, GP.get_repo(false))

  multiple_users(res) = GP.dewrap_list(res, "users", GP.get_user)

  one_team(res) = GP.dewrap_whole_obj(res, get_team)
  multiple_teams(res) = GP.dewrap_whole_list(res, get_team)

}}

GHOrgs = {{

  @private GP = GHParse

  list_organizations(user:GitHub.user_id) =
    (path, data) = GHLib.userpath(user,"orgs")
    GHLib.api_get(path, data, GP.multiple_short_users)

  get_organization(token:string, org:string) =
    GHLib.api_get_full("/orgs/{org}", token, [], GHORp.one_org)

  edit_organization(token:string, org:string,
                    billing_email:option(string), company:option(string), email:option(string),
                    location:option(string), name:option(string)) =
    json = GHLib.mkopts([{sopt=("billing_email",billing_email)},{sopt=("company",company)},{sopt=("email",email)},
                         {sopt=("location",location)},{sopt=("name",name)}])
    GHLib.api_patch_string("/org/{org}", token, json, GHORp.one_org)

  list_members(token:string, org:string) =
    GHLib.api_get_full("/orgs/{org}/members", token, [], GP.multiple_short_users)

  get_member(token:string, org:string, user:string) =
    GHLib.api_get_full("/orgs/{org}/members/{user}", token, [], GP.expect_204_404)

  remove_member(token:string, org:string, user:string) =
    GHLib.api_delete_string("/orgs/{org}/members/{user}", token, "", GP.expect_204)

  list_public_members(token:string, org:string) =
    GHLib.api_get_full("/orgs/{org}/public_members", token, [], GP.multiple_short_users)

  is_public_member(token:string, org:string, user:string) =
    GHLib.api_get_full("/orgs/{org}/public_members/{user}", token, [], GP.expect_204_404)

  publicize_membership(token:string, org:string, user:string) =
    GHLib.api_put_string("/orgs/{org}/public_members/{user}", token, "", GP.expect_204)

  conceal_membership(token:string, org:string, user:string) =
    GHLib.api_delete_string("/orgs/{org}/public_members/{user}", token, "", GP.expect_204)

  list_teams(token:string, org:string) =
    GHLib.api_get_full("/orgs/{org}/teams", token, [], GHORp.multiple_teams)

  get_team(token:string, id:int) =
    GHLib.api_get_full("/teams/{id}", token, [], GHORp.one_team)

  create_team(token:string, org:string,
              name:string, repo_names:list(string), permission:option(GitHub.permission)) =
    json = GHLib.mkopts([{sreq=("name",name)},{slst=("repo_names",repo_names)},
                         {ocst=("permission",GHORp.perm_to_str,permission)}])
    GHLib.api_post_string("/orgs/{org}/teams", token, json, GHORp.one_team)

  edit_team(token:string, id:int, name:string, permission:option(GitHub.permission)) =
    json = GHLib.mkopts([{sreq=("name",name)},{ocst=("permission",GHORp.perm_to_str,permission)}])
    GHLib.api_patch_string("/teams/{id}", token, json, GHORp.one_team)

  delete_team(token:string, id:int) =
    GHLib.api_delete_string("/teams/{id}", token, "", GP.expect_204)

  list_team_members(token:string, id:int) =
    GHLib.api_get_full("/teams/{id}/members", token, [], GP.multiple_short_users)

  get_team_member(token:string, id:int, user:string) =
    GHLib.api_get_full("/teams/{id}/members/{user}", token, [], GP.expect_204_404)

  add_team_member(token:string, id:int, user:string) =
    GHLib.api_put_string("/teams/{id}/members/{user}", token, "", GP.expect_204)

  remove_team_member(token:string, id:int, user:string) =
    GHLib.api_delete_string("/teams/{id}/members/{user}", token, "", GP.expect_204)

  list_team_repos(token:string, id:int) =
    GHLib.api_get_full("/teams/{id}/repos", token, [], GHORp.multiple_repos)

  get_team_repo(token:string, id:int, user:string, repo:string) =
    GHLib.api_get_full("/teams/{id}/repos/{user}/{repo}", token, [], GP.expect_204_404)

  add_team_repo(token:string, id:int, user:string, repo:string) =
    GHLib.api_put_string("/teams/{id}/repos/{user}/{repo}", token, "", GP.expect_204)

  remove_team_repo(token:string, id:int, user:string, repo:string) =
    GHLib.api_delete_string("/teams/{id}/repos/{user}/{repo}", token, "", GP.expect_204)

}}
