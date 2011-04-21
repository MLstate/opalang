import opages
import components.applicationframe

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

/** Init admin is does not exists or if needed on command line. */
admin_init =
  cmdline:CommandLine.family(bool) = {
    init = false
    parsers = [{ CommandLine.default_parser with
      names = ["--init-admin"]
      description = "Reinitialize administrator password and print it."
      on_encounter(_) = {no_params = true}
    }]
  }
  if not(User.has_admin()) || CommandLine.filter("OPAges", cmdline) then
    User.init_admin()

/** Non secure server - just serve page in database */
server =
  dispatcher = parser
    | url=(.*) ->
      match Page.resource(Text.to_string(url))
      | ~{embedded} -> Resource.full_page("Opalang",embedded.body, embedded.header,
                                      {success}, [])
      | ~{resource} -> resource
  { Server.simple_server(dispatcher) with server_name = "http" }

/** Secure server - page in database + administration page access */
server =
  build_html(url, embedded) =
    /* Application frame configuration. */
    app_frame_config = { CApplicationFrame.default_config with
      authenticate(name, pass) =
        if User.is_admin(name, pass) then none
        else some("Invalid password")
      private_page(_title, user) = Page.admin(url, user)
      public_page(_title) = embedded.body
      public_head(_title) = embedded.header
    }
    /* Application frame initialization. */
    init = CApplicationFrame.init(app_frame_config)
    CApplicationFrame.make(init, app_frame_config, "OPAges")
  /* OPAges url dispatcher it's just a coating of Page.resource */
  url_dispatcher = parser
    | url=(.*) ->
      url = Text.to_string(url)
      match Page.resource(url) with
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
