import opages
import stdlib.web.template

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
        rec aux() = match Db.transaction(opages,
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

authenticate(token,cred) =
  match token with
    | {some=(n,p)} -> if User.is_admin(n,p) then some({admin=n}) else none
    | _ -> {none}

login_config : CLogin.config(User.tokken,User.credential,User.credential) = {
  ~authenticate
  get_credential = identity
  loginbox =
    dochange, cred ->
      box(xhtml) =
        WLoginbox.html({style=WStyler.empty},
                       idclogin,(s1,s2->dochange(some((s1,s2)))),xhtml)
    match cred.cred with
    | {anon} -> box(none)
    | _ -> box(some(logout_xhtml(cred.name,dochange)))
  on_change = dochange, cred ->
    match cred.cred with
      | {anon} ->
             do WLoginbox.set_logged_out(idclogin,<></>)
             WAppFrame.do_set_content(idappframe, public_page(""))
      | {user} ->
             do WLoginbox.set_logged_in(idclogin,logout_xhtml(cred.name,dochange))
             WAppFrame.do_set_content(idappframe, private_page("",cred.name))
      | {admin} -> do WLoginbox.set_logged_in(idclogin,logout_xhtml(cred.name,dochange))
             WAppFrame.do_set_content(idappframe, admin_private_page("",cred.name))
    end
  prelude=none
}

@publish
server_state = CLogin.make({anon}, login_config)


/** Secure server - page in database + administration page access */
server =
  build_main(x)=
    <div>{CLogin.html(server_state)}</div>
    <div>{x}</div>
  build_html(url, embedded) =
    match CLogin.get_credential(server_state) with
      | {anon} -> Resource.full_page("", build_main(embedded.body), embedded.head, {success}, [])
      | {~admin} -> Resource.page("",build_main(page_demo.admin(url, user)))
  /* OPAges url dispatcher it's just a coating of Page.resource */
  url_dispatcher = parser
  | url=(.*) ->
    url = Text.to_string(url)
    match page_demo.resource(url) with
    | ~{embedded} ->
      /* Build application frame resource that embedded the returned
         embedded html.*/
      build_html(url, embedded)
    | ~{resource} ->
      /* Page return directly a resource. */
      Server.public(_ -> resource)
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
