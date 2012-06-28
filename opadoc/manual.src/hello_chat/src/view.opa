module View {

  function page_template(content) {
    <>
      <div class="navbar navbar-fixed-top">
        <div class=navbar-inner>
          <div class=container>
            <div id=#logo />
          </>
        </>
      </>
      {content}
    </>
  }

  chat_html =
    <div id=#conversation class=container-fluid />
    <div id=#footer class="navbar navbar-fixed-bottom">
      <div class=container>
        <div class=input-append>
          <input id=#entry class=input-xlarge type=text>
          <button class="btn btn-primary" type=button>Post</>
        </>
      </>
    </>

  function default_page() {
    Resource.page("Opa chat", page_template(chat_html))
  }

}
