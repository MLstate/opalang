// This file demonstrate the @i18n directives on strings
// comments are assuming a server with env var LANG="en_US.UTF-8" and a client with "fr" in the Accept-Language header


package hello_i18n // the default translation package will be  hello_i18n.translation

@server
server_log() =
 println(@i18n("{@i18n("server")} say hello")) // This is server eventually with a user context, can be in english or french

// onready use Dom.transform so it can only be called on client side
onready(_) =
 do Dom.transform([#h1 +<- @i18n("client say _hello")]) // user context => french
 server_log()   // call server_log with   a user context => French

do server_log() // call  server_log without user context => English

page() =
  do println("serving page") // no i18n
  <h1 id="h1" onready={onready} onclick={onready}>{@i18n("Hello World")}</h1>

server = one_page_server(
 @i18n("Hello"), // server context => english, here we see one_page_server is not designed for i18n
 page // page will be called in user context
)
