module View {

   // View code goes here

  function page_template(content) {
    <div class="navbar navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="brand" href="./index.html">hello_chat</>
        </>
      </>
    </>
    <div id=#main class="container-fluid">
      <div class="hero-unit">
        {content}
      </>
      <footer>
        <p>hello_chat</>
      </>
    </>
  }

  function default_page() {
    content =
      // page content goes here
      <>Page content goes here...</>
    html = page_template(content)
    Resource.page("Default page", html)
  }

}
