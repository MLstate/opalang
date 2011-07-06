import opages
import stdlib.components.applicationframe

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
db /opages : stringmap(Page.stored)

db /opages[_] full
db /opages[_] = Page.empty

/** */
demo_engine = Template.combine(TemplateDemo.engine, Template.default)

demo_access = {
  set = key, page -> /opages[key] <- page
  get = key -> ?/opages[key]
  rm  = key -> Db.remove(@/opages[key])
  ls  = -> StringMap.rev_fold(key, _, acc -> key +> acc, /opages, [] )
  date = key -> Db.modification_time(@/opages[key])
}

demo_not_found =
  Resource.error_page("Page not found", <h1>Page not found</h1>, {wrong_address})

page_demo = Page.make({access = demo_access engine = demo_engine not_found = demo_not_found} : Page.config)

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

/** Secure server - page in database + administration page access */
server =
  build_html(url, embedded) =
  /* Application frame configuration. /!\ @TODO: Remove me !! /!\ */
  app_frame_config = { CApplicationFrame.default_config with
    authenticate(name, pass) =
      if User.is_admin(name, pass) then none
      else some("Invalid password")
    private_page(_title, user) = page_demo.admin(url, user)
    public_page(_title) = embedded.body
    public_head(_title) = embedded.head
  }
  /* Application frame initialization. */
  init = CApplicationFrame.init(app_frame_config)
  CApplicationFrame.make(init, app_frame_config, "OPAges")
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
      Resource.secure(resource)
  /* Secured service */
  secure = Server.secure(Server.ssl_default_params, url_dispatcher)
  { secure with
    /* Record extension will give us a record, i.e. a closed sum so, open it
       to make it compatible with type [Server.encryption]. */
      encryption = {Server.ssl_default_params with
                      certificate="./main.crt"
                      private_key="./main.key"} <: Server.encryption
      server_name = "https"
  }

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
