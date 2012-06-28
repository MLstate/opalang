plugin_stammer = %%plugin.stammer%%

server stammer_server = plugin_stammer
client stammer_client = plugin_stammer

function stammer(a, side) {
    match (side) {
    case { on_client }: stammer_client(a);
    case { on_server }: stammer_server(a);
    }

}

function action(side) {
    a = Dom.get_value(#input_a);
    #result = stammer(a, side);
}

function button(string id, string message, side) {
  <a id={id}
     class="button"
     ref="#"
     onclick={function(_) { action(side) }}>{message}
  </a>
}

function page() {
  <>
  <h1>Hello Bindings</h1>
  <h2>Argument</h2>
  <input id="input_a"/>
  <h2>Result</h2>
  <div id="result" />
  <br/>
  <h2>Calling functions (using argument input)</h2>
  {button("stammer_client", "Compute stammer on the client", {on_client})}<br/>
  {button("stammer_server", "Compute stammer on the server", {on_server})}<br/>
  </>
}

Server.start(Server.http, {title: "Hello Bindings", ~page});
