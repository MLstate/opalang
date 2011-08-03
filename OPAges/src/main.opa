/*
    Copyright Â© 2011 MLstate

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

import opages
import stdlib.web.template
import stdlib.components.login
import stdlib.widgets.loginbox
import stdlib.widgets.core
import stdlib.web.client

/**
 * {1 Main}
 * This main file define a command line parser, the services to run and
 * provides css.

 * - command line parser accepts a uniq option without arguments
 *   "--init-admin" used for initialize "admin" password. Then password
 *   is printed on stderr.
 *
 * - The first service on HTTP just serves page in database.
 *

 * - The second service on HTTPS is same of HTTP service but provide
 *   additionaly an access to the administration pages.
 */

/* DOES NOT COMPILE ANYMORE,
   if you need a demo, please see repo opalang.org
   @TODO: remove CApplicationFrame !! */

/** A  path to store template content. */
db /opages/pages : stringmap(Page.stored)
db /opages/pages[_] = Page.empty
db /opages/pages_rev : stringmap(Page.published)
db /opages/pages_rev[_] = Page.not_published

database opages = @meta

id=Dom.fresh_id()
idappframe = "{id}appframe"
idclogin = "{id}clogin"

/** */
demo_engine  = Template.combine(TemplateDemo.engine, Template.default)

Access = {{

  rec val cache = Cache.make(
    Cache.Negociator.always_necessary(cached),
    Cache.default_options
  )
  cached(key) = select(key)
  select =
    | {last = key}      ->
      do Log.warning("OpalangPage","Database access : last {key}")
      Option.map(page -> {rev = nb_rev(key) ~page}, ?/opages/pages[key])
    | ~{key rev}        ->
      do Log.warning("OpalangPage","Database access : {rev} {key}")
      match Db.history(@/opages/pages[key], rev, 1)
      | []  -> do Log.error("OpalangPage", "Revision {rev} at {key} not found") none
      | [page] -> some(~{page rev})
      | _ -> do Log.error("OpalangPage", "Inconsistent result : Revision {rev} at {key} not found") none
      end
    | {published = key} ->
      do Log.warning("OpalangPage","Database access : published {key}")
      match ?/opages/pages_rev[key]
      | {none} -> cache.get({last = key})
      | {some=p} ->
        rev = Page.rev_of_published(p)
        cache.get(~{key rev})
    nb_rev(key) = List.length(Db.history(@/opages/pages[key], 1, 0))

  reset_cache() =
    lst = access.ls()
    List.iter((key, _) -> do cache.invalidate({last = key}) do cache.invalidate({published = key}) void, lst)
  fill_cache() =
    lst = access.ls()
    List.iter((key, _) -> ignore(access.select({published = key})), lst)

  access = {

    select = cache.get(_)

    save =
      | ~{key page} ->
        rec aux() = match Db.transaction(
             -> do /opages/pages[key] <- page
             nb_rev(key)
          ) with
          | {none} ->
            do Log.error("OpalangPage", "Save transaction failed, retry") aux()
          | {some = rev} ->
            do cache.invalidate({last = key})
            rev
          end
        aux()
      | ~{key publish} ->
        do /opages/pages_rev[key] <- publish
        do cache.invalidate({published = key})
        Page.rev_of_published(publish)

    published(key) = ?/opages/pages_rev[key]

    rm(key) =
      do Db.remove(@/opages/pages[key])
      do Db.remove(@/opages/pages_rev[key])
      do cache.invalidate({last = key})
      do cache.invalidate({published = key})
      void
    ls() = StringMap.rev_fold(key, _, acc -> (key, ?/opages/pages_rev[key]) +> acc,
                              /opages/pages,
                              [])
    history(key) = Db.history(@/opages/pages[key], 1, 0)

  }
}}

page_demo = Page.make(
  {
    access = Access.access
    engine(_env)= demo_engine
  } : Page.config)

/** Init admin is does not exists or if needed on command line. */
admin_init =
  cmdline:CommandLine.family(bool) = {
    init = false
    parsers = [{ CommandLine.default_parser with
      names = ["--init-admin"]
      description = "Reinitialize administrator password and print it."
      on_encounter(_) = {no_params = true}
    }]
    anonymous = []
    title = "OPAges"
  }
  if not(User.has_admin()) || CommandLine.filter(cmdline) then
    User.init_admin()

/** Non secure server - just serve page in database */
server =
  dispatcher = parser
    | url=(.*) ->
      match page_demo.resource(Text.to_string(url))
      | ~{embedded} -> Resource.full_page("Opalang",embedded.body, embedded.head,
                                      {success}, [])
      | ~{resource} -> resource
  { Server.simple_server(dispatcher) with server_name = "http" }
/** authentication */

logout_xhtml(name : string, dochange : User.tokken -> void) =
  <>
    {name} - <a onclick={_->dochange(none)}>logout</a>
  </>

authenticate(token,_cred) =
  match token with
    | {some=(n,p)} ->
    if User.is_admin(n,p)
      then
        do Log.debug("Build html", "User iz admin")
        some({admin=n})
      else
        do Log.debug("Build html", "User iz not admin n=={n} p=={p}")
        none
    | _ -> {none}

login_config : CLogin.config(User.tokken,User.credential,User.credential) = {
  ~authenticate
  get_credential = identity
  loginbox =
    dochange, cred ->
      box(xhtml) =
        WLoginbox.html({style=WStyler.empty},
                       idclogin,(s1,s2->dochange(some((s1,s2)))),xhtml)
    match cred with
    | {anon} -> box(none)
    | {~admin} -> box(some(logout_xhtml(admin,dochange)))

  on_change = dochange, cred ->
    match cred with
      | {anon} ->
        do WLoginbox.set_logged_out(idclogin,<></>)
        do Client.reload()
        void
      | {~admin} ->
        do WLoginbox.set_logged_in(idclogin,logout_xhtml(admin,dochange))
        do Client.reload()
        void
    end
  prelude=none
}

@publish
server_state = CLogin.make({anon}, login_config)

default_embedded =
{ head = <></>
  body = <>Administration Page</>
  }

/** Secure server - page in database + administration page access */
server =
  build_main(x)=
    <div>{CLogin.html(server_state)}</div>
    <div id="totololz">{x}</div>
  build_html(url, embedded) =
    match CLogin.get_credential(server_state) with
      | {anon} ->
        do Log.debug("Build html", "Anon waz here")
        Resource.full_page("", build_main(embedded.body), embedded.head, {success}, [])
      | {~admin} ->
        do Log.debug("Build html", "Admin waz here")
        Resource.full_page("",build_main(page_demo.admin(url, admin)), <link rel="stylesheet" type="text/css" href="/admin/style.css"/>, {success}, [] )
  /* OPAges url dispatcher it's just a coating of Page.resource */
  url_dispatcher = parser
  | "/admin" ->
    resource=build_html("/admin", default_embedded )
    Server.public(_ -> resource)
  | "/admin/style.css" ->
    resource=Resource.source(@static_source_content("static-include/admin/style.css"),
    "text/css")
    Server.public(_ -> resource)
  | url=(.*) ->
    url = Text.to_string(url)
    Server.public(_ ->
      Page.to_resource(page_demo.resource(url))
    )

  /* Secured service */
  ssl_params = { Server.ssl_default_params with
                   certificate="opages.crt"
                   private_key="opages.key"
               } <: Server.encryption
  { Server.secure(ssl_params, url_dispatcher) with server_name = "https" }


/** CSS used for administration pages for moment all services share
    css. */
css = css
  #admin_files {
    position:absolute;
    top:10%;
    left:0%;
    height:80%;
    width:19%;
    overflow: scroll;
  }
  #admin_editor_container {
    position:absolute;
    top:10%;
    left:20%;
    height:80%;
    width:80%;
    float:left;
  }
  .admin_editor {
    position:absolute;
    height:100%;
    width:100%;
    float:left;
  }
  #admin_buttons {
    position:absolute;
    top:90%;
    width:100%;
    height:10%;
  }
  .ace_editor {
    position: relative;
    width:99%;
    float:left;
  }
  #admin_editor_container .off {
    display: none;
  }
  #admin_files_table .on {
    font-weight:bold;
  }
  .admin_editor_html_header {
    height:10%;
  }
  .admin_editor_html_body {
    height:80%;
  }
  .admin_editor_css {
    height:90%;
  }
  .admin_editor_source {
    height:90%;
  }
