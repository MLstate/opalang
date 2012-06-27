module Controller {

  // URL dispatcher of your application; add URL handling as needed
  dispatcher = {
    parser {
    case (.*) : View.default_page()
    }
  }

}

resources = @static_resource_directory("resources")

Server.start(Server.http, [
  { register:
    [ { doctype: { html5 } },
      { js: [ ] },
      { css: [ "/resources/css/style.css"] }
    ]
  },
  { ~resources },
  { custom: Controller.dispatcher }
])
