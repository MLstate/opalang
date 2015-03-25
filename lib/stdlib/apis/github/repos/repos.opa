/*
    Copyright © 2011, 2012 MLstate

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

package stdlib.apis.github.repos
import stdlib.apis.github
import stdlib.apis.github.lib
import stdlib.io.file
import stdlib.crypto

type GitHub.archive_format = {tarball} / {zipball}

/* Types returned by API */

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
  name        : string
  commit      : GitHub.url_sha
  zipball_url : string
  tarball_url : string
}

type GitHub.short_branch = {
  name   : string
  commit : GitHub.url_sha
}

type GitHub.full_branch = {
  name   : string
  commit : GitHub.full_commit
  _links : GitHub.links_no_href
}

type GitHub.commit_diff = {
  url           : string
  html_url      : string
  permalink_url : string
  diff_url      : string
  patch_url     : string
  base_commit   : GitHub.full_commit
  status        : string
  ahead_by      : int
  behind_by     : int
  total_commits : int
  commits       : list(GitHub.full_commit)
  files         : list(GitHub.file)
}

type GitHub.file_content = {
  `type`   : string
  encoding : GitHub.encoding
  size     : int
  _links   : GitHub.links_no_href
  name     : string
  path     : string
  content  : string
  sha      : string
}

type GitHub.download_create_reply = {
  url            : string
  html_url       : string
  id             : int
  name           : string
  description    : string
  size           : int
  download_count : int
  content_type   : string
  policy         : string
  signature      : string
  bucket         : string
  accesskeyid    : string
  path           : string
  acl            : string
  expirationdate : string
  prefix         : string
  mime_type      : string
  redirect       : bool
  s3_url         : string
}

type GitHub.event_name =
   {push}           // Any git push to a Repository.
 / {issues}         // Any time an Issue is opened or closed.
 / {issue_comment}  // Any time an Issue is commented on.
 / {commit_comment} // Any time a Commit is commented on.
 / {pull_request}   // Any time a Pull Request is opened, closed, or synchronized
                    // (updated due to a new push in the branch that the pull request is tracking).
 / {gollum}         // Any time a Wiki page is updated.
 / {watch}          // Any time a User watches the Repository.
 / {download}       // Any time a Download is added to the Repository.
 / {fork}           // Any time a Repository is forked.
 / {fork_apply}     // Any time a patch is applied to the Repository from the Fork Queue.
 / {member}         // Any time a User is added as a collaborator to a non-Organization Repository.
 / {`public`}       // Any time a Repository changes from private to public.
 / {status}         // Any time a Repository has a status update from the API

type GitHub.hook = {
  url        : string
  updated_at : Date.date
  created_at : Date.date
  name       : string
  events     : list(GitHub.event_name)
  active     : bool
  config     : { url : string content_type : string }
  id         : int
}

type GitHub.status_state = {pending} / {success} / {error} / {failure}

type GitHub.status = {
  created_at  : Date.date
  updated_at  : Date.date
  state       : string
  target_url  : string
  description : string
  id          : int
  url         : string
  creator     : GitHub.short_user
}

type GitHub.self_repo_type = {all} / {owner} / {`public`} / {`private`} / {member}
type GitHub.user_repo_type = {all} / {owner} / {member}
type GitHub.org_repo_type = {all} / {`public`} / {`private`} / {member}
type GitHub.org_repo_sort = {created} / {updated} / {pushed} / {full_name}
type GitHub.fork_sort = {newest} / {oldest} / {watchers}

@private GHRp = {{

  @private GP = GHParse

  string_of_self_repo_type(rt:GitHub.self_repo_type) =
    match rt with | {all} -> "all" | {owner} -> "owner" | {`public`} -> "public" | {`private`} -> "private" | {member} -> "member"
  string_of_user_repo_type(rt:GitHub.user_repo_type) =
    match rt with | {all} -> "all" | {owner} -> "owner" | {member} -> "member"
  string_of_org_repo_type(rt:GitHub.org_repo_type) =
    match rt with | {all} -> "all" | {`public`} -> "public" | {`private`} -> "private" | {member} -> "member"

  string_of_repo_sort(rs:GitHub.org_repo_sort) =
    match rs with
    | {created} -> "created"
    | {updated} -> "updated"
    | {pushed} -> "pushed"
    | {full_name} -> "full_name"

  string_of_event(ev:GitHub.event_name) =
    match ev with
    | {push} -> "push"
    | {issues} -> "issues"
    | {issue_comment} -> "issue_comment"
    | {commit_comment} -> "commit_comment"
    | {pull_request} -> "pull_request"
    | {gollum} -> "gollum"
    | {watch} -> "watch"
    | {download} -> "download"
    | {fork} -> "fork"
    | {fork_apply} -> "fork_apply"
    | {member} -> "member"
    | {`public`} -> "public"
    | {status} -> "status"

  event_of_string(s:string) : GitHub.event_name =
    match s with
    | "push" -> {push}
    | "issues" -> {issues}
    | "issue_comment" -> {issue_comment}
    | "commit_comment" -> {commit_comment}
    | "pull_request" -> {pull_request}
    | "gollum" -> {gollum}
    | "watch" -> {watch}
    | "download" -> {download}
    | "fork" -> {fork}
    | "fork_apply" -> {fork_apply}
    | "member" -> {member}
    | "public" -> {`public`}
    | "status" -> {status}
    | _ -> {push}

  string_of_status_state(st) =
    match st with
    | {pending} -> "pending"
    | {success} -> "success"
    | {error} -> "error"
    | {failure} -> "failure"

  status_state_of_string(st) =
    match st with
    | "pending" -> {pending}
    | "success" -> {success}
    | "error" -> {error}
    | "failure" -> {failure}
    | _ -> {failure}

  opt_sort = GHLib.opt_generic(string_of_repo_sort,"sort",_)

  get_languages(srcmap) =
    aux(name,v,acc) = match v with
      | {Int=lines} -> List.add(~{name lines},acc)
      | _ -> acc
    GP.obj_list(srcmap, aux)

  get_tag(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("name")
    then
      res = {
        name        = m.str("name")
        commit      = GP.get_rec(m, "commit", GP.get_url_sha)
        zipball_url = m.str("zipball_url")
        tarball_url = m.str("tarball_url")
      } : GitHub.tag
      {some=res}
    else {none}

  get_short_branch(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("name")
    then
      res = {
        name   = m.str("name")
        commit = GP.get_rec(m, "commit", GP.get_url_sha)
      } : GitHub.short_branch
      {some=res}
    else {none}

  get_commit(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("author")
    then
      res = {
        author    = GP.get_rec(m, "author", GP.get_commit_user)
        sha       = m.str("sha")
        url       = m.str("url")
        message   = m.str("message")
        tree      = GP.get_rec(m, "tree", GP.get_url_sha)
        committer = GP.get_rec(m, "author", GP.get_commit_user)
      } : GitHub.commit
      {some=res}
    else {none}

  get_commit_diff(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("base_commit")
    then
      res = {
        url           = m.str("url")
        html_url      = m.str("html_url")
        permalink_url = m.str("permalink_url")
        diff_url      = m.str("diff_url")
        patch_url     = m.str("patch_url")
        base_commit   = GP.get_rec(m, "base_commit", GP.get_full_commit)
        status        = m.str("status")
        ahead_by      = m.int("ahead_by")
        behind_by     = m.int("behind_by")
        total_commits = m.int("total_commits")
        commits       = GP.get_list(m, "commits", GP.get_full_commit_opt)
        files         = GP.get_list(m, "files", GP.get_file_opt)
      } : GitHub.commit_diff
      {some=res}
    else {none}

  get_file_content(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("content")
    then
      res = {
        `type`   = m.str("type")
        encoding = GHLib.encoding_of_string(m.str("encoding"))
        size     = m.int("size")
        _links   = GP.get_rec(m, "_links", GP.get__links_no_href)
        name     = m.str("name")
        path     = m.str("path")
        content  = m.str("content")
        sha      = m.str("sha")
      } : GitHub.file_content
      {some=res}
    else {none}

  get_download_create_reply(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("download_count")
    then
      res = {
        url            = m.str("url")
        html_url       = m.str("html_url")
        id             = GP.get_id(m)
        name           = m.str("name")
        description    = m.str("description")
        size           = m.int("size")
        download_count = m.int("download_count")
        content_type   = m.str("content_type")
        policy         = m.str("policy")
        signature      = m.str("signature")
        bucket         = m.str("bucket")
        accesskeyid    = m.str("accesskeyid")
        path           = m.str("path")
        acl            = m.str("acl")
        expirationdate = m.str("expirationdate")
        prefix         = m.str("prefix")
        mime_type      = m.str("mime_type")
        redirect       = m.bool("redirect")
        s3_url         = m.str("s3_url")
      } : GitHub.download_create_reply
      {some=res}
    else {none}

  get_full_branch(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("name")
    then
      res = {
        name   = m.str("name")
        commit = GP.get_rec(m, "commit", GP.get_full_commit)
        _links = GP.get_rec(m, "_links", GP.get__links_no_href)
      } : GitHub.full_branch
      {some=res}
    else {none}

  get_event(e) =
    match e with
    | {String=ev} -> {some=event_of_string(ev)}
    | _ -> none

  get_url_content_type(srcmap) =
    m = GP.map_funs(srcmap)
    {
      url          = m.str("url")
      content_type = m.str("content_type")
    } : {url:string content_type:string}

  get_hook(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id")
    then
      res = {
        url        = m.str("url")
        updated_at = m.date("updated_at")
        created_at = m.date("created_at")
        name       = m.str("name")
        events     = GP.get_list(m, "events", get_event)
        active     = m.bool("active")
        config     = GP.get_rec(m, "user", get_url_content_type)
        id         = GP.get_id(m)
      } : GitHub.hook
      {some=res}
    else none

  get_status(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id")
    then
      status = {
        created_at  = m.date("created_at")
        updated_at  = m.date("updated_at")
        state       = m.str("state")
        target_url  = m.str("target_url")
        description = m.str("description")
        id          = GP.get_id(m)
        url         = m.str("url")
        creator     = GP.get_rec(m, "user", GP.get_short_user)
      } : GitHub.status
      {some=status}
    else none

  one_repo(res) = GP.dewrap_whole_obj(res, GP.get_repo(false))
  multiple_repos(res) = GP.dewrap_whole_list(res, GP.get_repo(false))

  languages(res) = GP.dewrap_whole_obj(res, get_languages)

  tags(res) = GP.dewrap_whole_obj(res, get_tag)

  short_branches(res) = GP.dewrap_whole_list(res, get_short_branch)

  one_full_branch(res) = GP.dewrap_whole_obj(res, get_full_branch)

  one_repo_comment(res) = GP.dewrap_whole_obj(res, GP.get_repo_comment)
  multiple_repo_comments(res) = GP.dewrap_whole_list(res, GP.get_repo_comment)

  one_full_commit(res) = GP.dewrap_whole_obj(res, GP.get_full_commit_opt)
  multiple_full_commits(res) = GP.dewrap_whole_list(res, GP.get_full_commit_opt)

  one_commit_diff(res) = GP.dewrap_whole_obj(res, get_commit_diff)

  one_file_content(res) = GP.dewrap_whole_obj(res, get_file_content)

  one_download(res) = GP.dewrap_whole_obj(res, GP.get_download)
  multiple_downloads(res) = GP.dewrap_whole_list(res, GP.get_download)

  one_download_create_reply(res) = GP.dewrap_whole_obj(res, get_download_create_reply)

  one_key(res) = GP.dewrap_whole_obj(res, GP.get_public_key)
  multiple_keys(res) = GP.dewrap_whole_list(res, GP.get_public_key)

  one_hook(res) = GP.dewrap_whole_obj(res, get_hook)
  multiple_hooks(res) = GP.dewrap_whole_list(res, get_hook)

  one_status(res) = GP.dewrap_whole_obj(res, get_status)
  multiple_statuses(res) = GP.dewrap_whole_list(res, get_status)

}}

type GitHub.user_org =
   {self}
 / {org:string}

GHRepos = {{

  @private GP = GHParse

  list_self_repositories(token:string,
                         repo_type:option(GitHub.self_repo_type), sort:option(GitHub.org_repo_sort),
                         direction:option(GitHub.direction)) =
    options = List.flatten([GHLib.opt_generic(GHRp.string_of_self_repo_type,"type",repo_type),
                            GHRp.opt_sort(sort),GHLib.opt_direction(direction)])
    GHLib.api_get_full("/user/repos", token, options, GHRp.multiple_repos)

  list_user_repositories(token:string, user:string,
                         repo_type:option(GitHub.user_repo_type), sort:option(GitHub.org_repo_sort),
                         direction:option(GitHub.direction)) =
    options = List.flatten([GHLib.opt_generic(GHRp.string_of_user_repo_type,"type",repo_type),
                            GHRp.opt_sort(sort),GHLib.opt_direction(direction)])
    GHLib.api_get_full("/users/{user}/repos", token, options, GHRp.multiple_repos)

  list_org_repositories(token:string, org:string,
                        repo_type:option(GitHub.org_repo_type), sort:option(GitHub.org_repo_sort),
                        direction:option(GitHub.direction)) =
    options = List.flatten([GHLib.opt_generic(GHRp.string_of_org_repo_type,"type",repo_type),
                            GHRp.opt_sort(sort),GHLib.opt_direction(direction)])
    GHLib.api_get_full("/orgs/{org}/repos", token, options, GHRp.multiple_repos)

  create_repository(token:string, user_org:GitHub.user_org,
                    name:string, description:option(string), homepage:option(string), private:option(bool),
                    has_issues:option(bool), has_wiki:option(bool), has_downloads:option(bool), team_id:option(int)) =
    path =
      match user_org with
      | {self} -> "/user/repos"
      | {~org} -> "/orgs/{org}/repos"
    json = GHLib.mkopts([{sreq=("name",name)},{sopt=("description",description)},{sopt=("homepage",homepage)},
                         {bopt=("private",private)},{bopt=("has_issues",has_issues)},{bopt=("has_wiki",has_wiki)},
                         {bopt=("has_downloads",has_downloads)},{iopt=("team_id",team_id)}])
    GHLib.api_post_string(path, token, json, GHRp.one_repo)

  get_repository(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}", token, [], GHRp.one_repo)

  edit_repository(token:string, user:string, repo:string,
                  name:string, description:option(string), homepage:option(string), private:option(bool),
                  has_issues:option(bool), has_wiki:option(bool), has_downloads:option(bool)) =
    json = GHLib.mkopts([{sreq=("name",name)},{sopt=("description",description)},{sopt=("homepage",homepage)},
                         {bopt=("private",private)},{bopt=("has_issues",has_issues)},{bopt=("has_wiki",has_wiki)},
                         {bopt=("has_downloads",has_downloads)}])
    GHLib.api_patch_string("/repos/{user}/{repo}", token, json, GHRp.one_repo)

  list_contributors(token:string, user:string, repo:string, anon:option(bool)) =
    GHLib.api_get_full("/repos/{user}/{repo}/contributors", token, GHLib.opt_bool("anon",anon), GP.multiple_short_users)

  list_languages(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/languages", token, [], GHRp.languages)

  list_teams(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/teams", token, [], GP.multiple_id_name_urls)

  list_tags(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/tags", token, [], GHRp.tags)

  list_branches(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/branches", token, [], GHRp.short_branches)

  get_branch(token:string, user:string, repo:string, branch:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/branches/{branch}", token, [], GHRp.one_full_branch)

  delete_repository(token:string, user:string, repo:string) =
    GHLib.api_delete_string("/repos/{user}/{repo}", token, "", GP.expect_204)

  list_collaborators(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/collaborators", token, [], GP.multiple_short_users)

  is_collaborator(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/collaborators/{user}", token, [], GP.expect_204_404)

  add_collaborator(token:string, user:string, repo:string, user_to_add:string) =
    GHLib.api_put_string("/repos/{user}/{repo}/collaborators/{user_to_add}", token, "", GP.expect_204)

  remove_collaborator(token:string, user:string, repo:string, user_to_remove:string) =
    GHLib.api_delete_string("/repos/{user}/{repo}/collaborators/{user_to_remove}", token, "", GP.expect_204)

  list_repo_comments(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/comments", token, [], GHRp.multiple_repo_comments)

  list_commit_comments(token:string, user:string, repo:string, sha:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/commits/{sha}/comments", token, [], GHRp.multiple_repo_comments)

  create_commit_comment(token:string, user:string, repo:string, sha:string,
                        body:string, path:string, position:int, line:option(int)) =
    json = GHLib.mkopts([{sreq=("body",body)},{sreq=("path",path)},{ireq=("position",position)},{iopt=("line",line)}])
    GHLib.api_post_string("/repos/{user}/{repo}/commits/{sha}/comments", token, json, GHRp.one_repo_comment)

  get_commit_comment(token:string, user:string, repo:string, id:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/comments/{id}", token, [], GHRp.one_repo_comment)

  update_commit_comment(token:string, user:string, repo:string, id:int,
                        body:string) =
    json = GHLib.mkopts([{sreq=("body",body)}])
    GHLib.api_patch_string("/repos/{user}/{repo}/comments/{id}", token, json, GHRp.one_repo_comment)

  delete_commit_comment(token:string, user:string, repo:string, id:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/comments/{id}", token, "", GP.expect_204)

  list_repo_commits(token:string, user:string, repo:string,
                    sha:option(string),path:option(string),author:option(string)) =
    options = List.flatten([GHLib.opt_string("sha",sha),GHLib.opt_string("path",path),GHLib.opt_string("author",author)])
    GHLib.api_get_full("/repos/{user}/{repo}/commits", token, options, GHRp.multiple_full_commits)

  get_commit(token:string, user:string, repo:string, sha:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/commits/{sha}", token, [], GHRp.one_full_commit)

  compare_commits(token:string, user:string, repo:string, base:string, head:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/compare/{base}...{head}", token, [], GHRp.one_commit_diff)

  get_readme(token:string, user:string, repo:string, ref:option(string)) =
    GHLib.api_get_full("/repos/{user}/{repo}/readme", token, GHLib.opt_string("ref",ref), GHRp.one_file_content)

  get_contents(token:string, user:string, repo:string, path:option(string), ref:option(string)) =
    options = List.flatten([GHLib.opt_string("path",path),GHLib.opt_string("ref",ref)])
    GHLib.api_get_full("/repos/{user}/{repo}/contents", token, options, GHRp.one_file_content)

  @private string_of_archive_format(af) =
    match af with
    | {tarball} -> "tarball"
    | {zipball} -> "zipball"

  // TODO: this doesn't work, problem with header_get?
  get_location(res) =
    //do jlog("get_location: res={res}")
    if res.code == 302
    then
      loc = res.header_get("location")
      //do jlog("loc:{loc}")
      loc
    else none

  get_archive_link(token:string, user:string, repo:string, archive_format:GitHub.archive_format, ref:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/{string_of_archive_format(archive_format)}/{ref}", token, [], get_location)

  list_downloads(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/downloads", token, [], GHRp.multiple_downloads)

  get_download(token:string, user:string, repo:string, id:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/downloads/{id}", token, [], GHRp.one_download)

  // We wrap the two calls into a single function, create download point and then upload the given file
  create_download(token:string, user:string, repo:string,
                  name:string, description:option(string), content_type:option(string),
                  file:string) =
    // TODO: mimetype doesn't work on node, otherwise we could use it instead of content_type
    //do jlog("mimetype: {File.mimetype(file)}")
    match File.read_opt(file) with
    | {some=content} ->
      json = GHLib.mkopts([{sreq=("name",name)},{ireq=("size",Binary.length(content))},
                           {sopt=("description",description)},{sopt=("content_type",content_type)}])
      match GHLib.api_post_string("/repos/{user}/{repo}/downloads", token, json, GHRp.one_download_create_reply) with
      | {some=dcr} ->
         forms = [{form=("key",dcr.path)},{form=("acl",dcr.acl)},
                  {form=("success_action_status","201")},{form=("Filename",dcr.name)},
                  {form=("AWSAccessKeyId",dcr.accesskeyid)},{form=("Policy",dcr.policy)},
                  {form=("Signature",dcr.signature)},{form=("Content-Type",dcr.mime_type)},
                  {file=("file",File.basename(file, none),dcr.mime_type,string_of_binary(content))}]
         ({some=dcr},GHLib.full_post_forms(dcr.s3_url, "", "", forms, GP.expect_201))
      | _ -> (none,none)
      end
    | _ -> (none,none)

  delete_download(token:string, user:string, repo:string, id:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/downloads/{id}", token, "", GP.expect_204)

  @private string_of_fork_sort(fs) =
    match fs with
    | {newest} -> "newest"
    | {oldest} -> "oldest"
    | {watchers} -> "watchers"

  list_forks(token:string, user:string, repo:string, sort:option(GitHub.fork_sort)) =
    options = GHLib.opt_generic(string_of_fork_sort,"sort",sort)
    GHLib.api_get_full("/repos/{user}/{repo}/forks", token, options, GHRp.multiple_repos)

  create_fork(token:string, user:string, repo:string, org:option(string)) =
    json = GHLib.mkopts([{sopt=("org",org)}])
    GHLib.api_post_string("/repos/{user}/{repo}/forks", token, json, GHRp.one_repo)

  list_keys(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/keys", token, [], GHRp.multiple_keys)

  get_key(token:string, user:string, repo:string, id:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/keys/{id}", token, [], GHRp.one_key)

  create_key(token:string, user:string, repo:string, title:string, key:string) =
    json = GHLib.mkopts([{sreq=("title",title)},{sreq=("key",key)}])
    GHLib.api_post_string("/repos/{user}/{repo}/keys", token, json, GHRp.one_key)

  edit_key(token:string, user:string, repo:string, id:int, title:string, key:string) =
    json = GHLib.mkopts([{sreq=("title",title)},{sreq=("key",key)}])
    GHLib.api_patch_string("/repos/{user}/{repo}/keys/{id}", token, json, GHRp.one_key)

  delete_key(token:string, user:string, repo:string, id:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/keys/{id}", token, "", GP.expect_204)

  list_hooks(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/hooks", token, [], GHRp.multiple_hooks)

  get_hook(token:string, user:string, repo:string, id:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/hooks/{id}", token, [], GHRp.one_hook)

  create_hook(token:string, user:string, repo:string,
              name:string, config:list((string,string)), events:list(string), active:option(bool)) =
    config = List.map(((n,v) -> (n,"\"{v}\"")),config)
    json = GHLib.mkopts([{sreq=("name",name)},{obj=("config",config)},
                         {slst=("events",events)},{bopt=("active",active)}])
    GHLib.api_post_string("/repos/{user}/{repo}/hooks", token, json, GHRp.one_hook)

  edit_hook(token:string, user:string, repo:string, id:int,
            name:string, config:list((string,string)),
            events:list(string), add_events:list(string), remove_events:list(string), active:option(bool)) =
    config = List.map(((n,v) -> (n,"\"{v}\"")),config)
    json = GHLib.mkopts([{sreq=("name",name)},{obj=("config",config)},
                         {slst=("events",events)},{slst=("add_events",add_events)},{slst=("remove_events",remove_events)},
                         {bopt=("active",active)}])
    GHLib.api_post_string("/repos/{user}/{repo}/hooks/{id}", token, json, GHRp.one_hook)

  test_hook(token:string, user:string, repo:string, id:int) =
    GHLib.api_post_string("/repos/{user}/{repo}/hooks/{id}/test", token, "", GP.expect_204)

  delete_hook(token:string, user:string, repo:string, id:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/hooks/{id}", token, "", GP.expect_204)

  // TODO: PubSubHubbub

  @private merge_result(res:WebClient.success(string)) =
    some(match res.code with
         | 201 ->
           match Json.of_string(res.content) with
           | {some={Record=r}} -> {success={some=GP.get_full_commit({Record=r})}}
           | _ -> {failure="merge_result: Bad JSON"}
           end
         | 204 -> {success={none}} // Is this what we want?
         | 404 | 409 ->
           match Json.of_string(res.content) with
           | {some={Record=[("message",{String=message})]}} -> {failure=message}
           | _ -> {failure="merge_result: Bad message"}
           end
         | _ -> {failure="merge_result: Bad HTTP code {res.code}"})

  perform_merge(token:string, user:string, repo:string, base:string, head:string, commit_message:option(string)) =
    json = GHLib.mkopts([{sreq=("base",base)},{sreq=("head",head)},{sopt=("commit_message",commit_message)}])
    GHLib.api_post_string("/repos/{user}/{repo}/merges", token, json, merge_result)

  list_stargazers(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/stargazers", token, [], GP.multiple_short_users)

  list_starred_repositories(user:GitHub.user_id) =
    (path, data) = GHLib.userpath(user,"starred")
    GHLib.api_get(path, data, GHRp.multiple_repos)

  is_starred(token:string, user:string, repo:string) =
    GHLib.api_get_full("/user/starred/{user}/{repo}", token, [], GP.expect_204_404)

  star_repository(token:string, user:string, repo:string) =
    GHLib.api_put_string("/user/starred/{user}/{repo}", token, "", GP.expect_204)

  unstar_repository(token:string, user:string, repo:string) =
    GHLib.api_delete_string("/user/starred/{user}/{repo}", token, "", GP.expect_204)

  list_statuses(token:string, user:string, repo:string, sha:string) =
    options = GHLib.opt_string("sha",{some=sha})
    GHLib.api_get_full("/repos/{user}/{repo}/statuses/{sha}", token, options, GHRp.multiple_statuses)

  create_status(token:string, user:string, repo:string, sha:string,
                state:GitHub.status_state, target_url:option(string), description:option(string)) =
    json = GHLib.mkopts([{rcst=("state",GHRp.string_of_status_state,state)},{sopt=("target_url",target_url)},
                         {sopt=("description",description)}])
    GHLib.api_post_string("/repos/{user}/{repo}/statuses/{sha}", token, json, GHRp.one_status)

  list_watchers(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/subscribers", token, [], GP.multiple_short_users)

  list_watched_repositories(user:GitHub.user_id) =
    (path, data) = GHLib.userpath(user,"subscriptions")
    GHLib.api_get(path, data, GHRp.multiple_repos)

  is_watched(token:string, user:string, repo:string) =
    GHLib.api_get_full("/user/subscriptions/{user}/{repo}", token, [], GP.expect_204_404)

  watch_repository(token:string, user:string, repo:string) =
    GHLib.api_put_string("/user/subscriptions/{user}/{repo}", token, "", GP.expect_204)

  unwatch_repository(token:string, user:string, repo:string) =
    GHLib.api_delete_string("/user/subscriptions/{user}/{repo}", token, "", GP.expect_204)

}}
