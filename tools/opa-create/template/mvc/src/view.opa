module View {
  
  function template(content) {
    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="brand" href="./index.html">application_name</a>
          <div class="nav-collapse collapse">
            <ul class="nav">
              <li><a href="/statistics">Statistics</a></li>
            </ul>
          </div>
        </div>
      </div>
    </div>
    <div id=#main class="container-fluid">
      <div class="row-fluid">     
        {content} 
      </div>
      <hr>
      <footer>
        <p>application_name</p>
      </footer>
    </div>
  
  }

  function statistics() {
    tbody = Iter.fold(
      function(page, acc) { <>{acc}<tr><td>{page.path}</td><td>{page.counter}</td></tr></> },
      Model.statistics(), <></>
    )
    content =
      <h3>Page statistics</h3>
      <table class="table table-bordered">
        <thead><tr><td>Path</td><td>Counter</td></tr></thead>
        <tbody>{tbody}</tbody>
      </table>
    template(content)
  }

  function page(path) {
    content =
      <div class="hero-unit">
        <h3>{path}</h3>
        <textarea id=#content rows="15">{ Model.get_content(path) }</textarea>
        <button class="btn" onclick={ function(_) Model.set_content(path, Dom.get_value(#content)) }>
          Save
        </button>
      </div>;
    template(content)
  }

}

