resources = @static_resource_directory("resources")

custom = {
  parser {
  case r={Server.resource_map(resources)} : r
  case "/" : Resource.default_redirection_page("/index.html")
  case "/statistics" : Resource.page("Display", View.statistics())
  case p=(.*) : path = Text.to_string(p); Resource.page(path, View.page(path))
  }
}

Server.start(Server.http, [
  { register:
    [ { doctype: { html5 } },
      { js: [ ] },
      { css: [ "/resources/css/style.css"] }
    ]
  },
  { ~custom }
])
