// This file demonstrate the @i18n directives on strings
// comments are assuming a server with env var LANG="en_US.UTF-8" and a client with "fr" in the Accept-Language header

package custom // the default translation package will be custom.translation


// we declare a value that requires the lang as parameter
`Am I english?` =
| {en} -> true
| _    -> false

// when we want to resolve `Am I english?` with the current context langage, pass it to @i18n
`Answer to "Am I english?"`() = @i18n(`Am I english?`) // the context langage will be the context at call site of `Answer to "Am I english?"`

message() = "Am I english ? {`Answer to "Am I english?"`()}" // the context langage will be the context at call site of `message`

onready(_) = Dom.transform( [#h1 <- message() ] ) // client context french => "Am I english ? false"

page() =
  do println(message()) //  client context, french => "Am I english ? true"
  <h1 id="h1" onready={onready}> WAIT ONREADY </h1>


do println(message()) // server context, english => "Am I english ? true"
server = one_page_server(message(), // server context, english => "Am I english ? true"
  page // page will be called in user context)
)
