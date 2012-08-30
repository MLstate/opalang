module View {

   // View code goes here

  function page_template(title, content) {
    html =
      <div class="navbar navbar-fixed-top">
        <div class=navbar-inner>
          <div class=container>
            <a class=brand href="./index.html">application_name</>
          </div>
        </div>
      </div>
      <div id=#main class=container-fluid>
        {content}
      </div>
    Resource.page(title, html)
  }

  function default_page() {
    content =
      <div class="hero-unit">
        Page content goes here...
      </div>
    page_template("Default page", content)
  }

}
