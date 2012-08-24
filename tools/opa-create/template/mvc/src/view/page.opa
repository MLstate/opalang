module Page {

   // View code goes here

  function page_template(title, content) {
    html =
      <div class="navbar navbar-fixed-top">
        <div class=navbar-inner>
          <div class=container>
            <a class=brand href="./index.html">application_name</>
          </>
        </>
      </>
      <div id=#main class=container-fluid>
        {content}
      </>
    Resource.page(title, html)
  }

  function default_page() {
    content =
      <div class="hero-unit">
        <>Page content goes here...</>
      </>
    page_template("Default page", content)
  }

}
