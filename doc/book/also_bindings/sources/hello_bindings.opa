plugin_stammer = %%plugin.stammer%%

@server stammer_server = plugin_stammer
@client stammer_client = plugin_stammer

stammer(a, side) =
  match side with
  | {client} -> stammer_client(a)
  | {server} -> stammer_server(a)

action(side) =
  a = Dom.get_value(#input_a)
  Dom.transform([ #result <- stammer(a, side) ])

button(id, message, side) =
  <a id={id:string}
     class="button"
     ref="#"
     onclick={_->action(side)}>{message:string}
  </a>

page() =
  <>
  <h1>Hello Bindings</h1>
  <h2>Argument</h2>
  <input id="input_a"/>
  <h2>Result</h2>
  <div id="result" />
  <br/>
  <h2>Calling functions (using argument input)</h2>
  {button("stammer_client", "Compute stammer on the client", {client})}<br/>
  {button("stammer_server", "Compute stammer on the server", {server})}<br/>
  </>

server = one_page_server("Hello Bindings", page)
