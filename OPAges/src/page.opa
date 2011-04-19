import opace

/**
 * {1 About this module}
 * This module provides a function for build an xhtml administration
 * interface, with this interface OPAges administrator can add, edit,
 * remove... OPAges resources. [Page] module provides also a resources
 * function that be able to returns resources added by administrator
 * or it build default error resource.
 */

/**
 * {1 Types defined in this module}
 */
/**
 * Html page that should be embedded by the applications.
 */
/* @private */type Page.embedded = {
  header : string
  body   : string
}

/**
 * Database embeddable resource
 */
/* @private */type Page.resource =
  {image : image} /
  {css : string} /
  {source : string; mime : string}

/**
 * Define what is a page.
 * - embedded means that the page should be embedded in the applications.
 * - resource is a subset of [resource] that be able to store in database.
 */
/* @private */type Page.t =
  {embedded : Page.embedded} /
  {resource : Page.resource}

/**
 * Define different class of file.
 */
/* @private */type Page.class =
  {embedded} / {image} / {octet} / {source}

/**
 * A record used for give some primitive to the admin GUI unless
 * publish critical primitives.
 */
/* @private */type Page.access = {
  get_class : (string -> Page.class)
  get  : (string -> Page.t)
  save : (string, Page.t -> void)
  remove : (string -> void)
}

/*
 * A database stringmap that contains binding beetween url and html
 * page.
 */
db /pages/public : stringmap(Page.t)

/*
 * A database path which contains the 404 page. The default value is
 * [Pages.default_404]
 */
db /pages/not_found : Page.embedded = {
  header = "<title>OPAges Not found</title>"
  body   = "<h1>404 - Not found</h1>"
}

/* Some default needed default values. */
db /pages/public[_] = { embedded = /pages/not_found }
db /pages/public[_]/resource/image = {png=binary_of_string("")}
db /pages/public[_]/resource = {css = ""}

Page = {{

  /**
   * {2 Privates utils}
   */
  /**
   * Transform an "string" embedded to "xhtml" embedded
   */
  @private embedded_to_xembedded(embedded) = {embedded = {
        header = Xhtml.of_string_unsafe(embedded.header)
        body = Xhtml.of_string_unsafe(embedded.body)
      }
    }

  /**
   * Get mode corresponding to the [file].
   */
  @private get_suffix(file) =
    match file with
    | "/" -> some("html")
    | _ -> x = Parser.try_parse(Rule.has_suffix(Rule.alphanum_string), file)
           Option.map((x -> x.f2), x)


  /**
   * Return default value corresponding to a file mode.
   */
  @private default =
    | {some = "html"} ->
        { embedded = { header = "<!-- Enter your headers here -->"
                       body="<!-- Enter your body here -->" } }
    | {some = "css"}  ->
        { resource = { css = "/* Enter your css here */"} }
    | _               ->
        { resource = { source = "Default ressource" mime="text/plain" } }


  /**
   * Save a public [page] at [url].
   */
  @private save(url, page) = /pages/public[url] <- page

  /**
   * Delete page at [url].
   */
  @private remove(url) = /pages/public <- StringMap.remove(url, /pages/public)

  /**
   * Get a page at [url].
   */
  @private get(url) = match ?/pages/public[url]
    | ~{some} -> some
    | {none} -> default(get_suffix(url))

  @private get_class(url) = match ?/pages/public[url]
    | {some = {embedded = _}} -> {embedded}
    | {some = {resource = {image = _}}} -> {image}
    | {some = {resource = {mime = "application/octet-stream" ...}}} -> {octet}
    | _ -> {source}

  /**
   * {2 Graphical user interface (admin)}
   */
  /**
   * This modules provides a build function for administration
   * interface.
   */
  AdminGui = {{

    /** Just a simple string transformation for xhtml insert. */
    @private file_for_xhtml =
      | "/" -> "*index*"
      | x -> x

    /** A translation that allows to use file on dom identifiers. */
    @private file_id = md5

    /** Create a file line for table insert. */
    @private file_line(access:Page.access, opened, file) =
      class = if opened then "on" else "off"
      <tr class="{class}" id="admin_files_table_{file_id(file)}">
        <td onclick={Action.open_file(access, file)}>
          {file_for_xhtml(file)}
        </td>
      </tr>

    /** Insert if necessary a file line into files table. */
    @private file_line_insert(access, opened, file) =
      line = Dom.select_id("admin_files_table_{file_id(file)}")
      if Dom.is_empty(line) then
        /* Insert a line on files table. */
        table = Dom.select_id("admin_files_table")
        _ = Dom.put_at_end(table, Dom.of_xhtml(file_line(access, true, file)))
        void
      else Dom.add_class(line, if opened then "on" else "off")

    /**
     * Build a buffer and insert it on dom node with identifier
     * [id]. The buffer can be specialized according to file
     * extension.
     *
     * Note : Client side function because need dom access.
     */
    @client build_buffer(access, file, id) =
      make_buttons(get_page) =
        buttons =
          remove = <button type="button"
                    onclick={Action.remove_file(access, file)}>Remove</button>
          match get_page with
          | {some = get_page} ->
            <><button type="button"
                    onclick={_ -> access.save(file, get_page())}>Save</button>
            {remove}</>
          | {none} -> remove
        _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(buttons))
        void
      match access.get_class(file) with
      | {image} ->
        dom = Dom.of_xhtml(<img src="{file}"/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        make_buttons(none)
      | {octet} ->
        dom = Dom.of_xhtml(<span>Uneditable binary file</span>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        make_buttons(none)
      | _ ->
        match access.get(file) with
        | {embedded = ~{body header}} ->
          /* Html embedded editor is divided in two ace editor, one for
          headers another for body */
          head_id = "{id}_headers" body_id = "{id}_body"
          dom = Dom.of_xhtml(<div id="{head_id}" class="admin_editor_html_header"/>
                             <div id="{body_id}" class="admin_editor_html_body"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          /* Head ace */
          ace_head = Ace.edit(head_id)
          do Ace.set_content(ace_head, header)
          _ = Ace.set_mode(ace_head, "html")
          /* Body ace */
          ace_body = Ace.edit(body_id)
          do Ace.set_content(ace_body, body)
          _ = Ace.set_mode(ace_body, "html")
          /* Buffer buttons */
          make_buttons(some( -> {embedded = {header=Ace.get_content(ace_head)
                                             body=Ace.get_content(ace_body)}}))


        | {resource = ~{css}} ->
          /* Css editor is just a simple ace editor */
          id_css = "{id}_css"
          dom = Dom.of_xhtml(<div id="{id_css}" class="admin_editor_css"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          ace_css = Ace.edit(id_css)
          _ = Ace.set_content(ace_css, css)
          _ = Ace.set_mode(ace_css, "css")
          make_buttons(some( -> {resource = {css=Ace.get_content(ace_css)}}))

        | {resource = ~{source mime}} ->
          /* A raw resource editor is an ace editor + an input for mime */
          id_src = "{id}_src"
          dom = Dom.of_xhtml(<div id="{id_src}" class="admin_editor_source"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          ace_src = Ace.edit(id_src)
          _ = Ace.set_content(ace_src, source)
          /* Buffer buttons */
          get_mime() = Dom.get_value(Dom.select_id("admin_editor_mime"))
          do make_buttons(some( -> {resource = {source=Ace.get_content(ace_src)
                                                mime=get_mime()}}))
          dom = Dom.of_xhtml(<input type="text" value="{mime}" id="admin_editor_mime"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          void
        | _ ->
          dom = Dom.of_xhtml(<span>Unexpected kind of file</span>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          void


    /**
     * Create a buffer for the given [file].
     */
    @private file_buffer(access:Page.access, opened, file) =
      id = "admin_buffer_{file_id(file)}"
      class = if opened then "on" else "off"
      <div class="{class} admin_editor" id="{id}"
       onready={_ -> _ = build_buffer(access, file, id) void}>
      </div>

    /**
     * Create and insert a buffer for given [file] on dom.
     */
    @client insert_buffer(access:Page.access, opened, file) =
      edito = Dom.select_id("admin_editor_container")
      buffer = Dom.of_xhtml(file_buffer(access, opened, file))
      _ = Dom.put_at_end(edito, buffer)
      void

    /**
     * This module provides fun action for admin interface.
     */
    @client @private Action = {{

      /**
       * Switch all "on" classes to "off" classes
       */
      @private all_off() =
        dom_on = Dom.select_class("on")
        do Dom.add_class(dom_on, "off")
        do Dom.remove_class(dom_on, "on")
        void

      /**
       * Open file
       */
      open_file(access:Page.access, file) : FunAction.t = _event ->
        do all_off()
        do file_line_insert(access, true, file)
        buf = Dom.select_id("admin_buffer_{file_id(file)}")
        if Dom.is_empty(buf) then
          insert_buffer(access, true, file)
        else
          do Dom.add_class(buf, "on")
          do Dom.remove_class(buf, "off")
          void

      /**
       * Create a new file.
       */
      new_file(access:Page.access) : FunAction.t = event ->
        /* Select file to open */
        file = Dom.get_value(Dom.select_id("admin_new_file"))
        do file_line_insert(access, true, file)
        open_file(access, file)(event)

      remove_file(access:Page.access, file) : FunAction.t = _event ->
        do access.remove(file)
        fl  = Dom.select_id("admin_files_table_{file_id(file)}")
        buf = Dom.select_id("admin_buffer_{file_id(file)}")
        do Dom.remove(fl)
        do Dom.remove(buf)
        void

      save_file(access:Page.access, get, file) : FunAction.t = _event ->
        // TODO - Something
        access.save(file, get())


    }}

    /**
     * Build the xhtml administration interface.
     */
    build(access:Page.access, url) =
      /* Add default page if url does not already exists on page map */
      page_map =
        map = /pages/public
        if StringMap.mem(url, map) then map
        else StringMap.add(url, default(get_suffix(url)), map)

      /* Perform an uploaded file */
      perform_file(file) =
        /* Save page resource for the uploaded file using file
           suffix. */
        save(filename, content) =
          content = match get_suffix(filename) with
          | {some = "html"} | {some = "xhtml"} ->
            {embedded = {header="" body=content}}
          | {some = "png"} -> {resource= {image={png=content}}}
          | {some = "jpg"} -> {resource= {image={jpg=content}}}
          | {some = "ico"} -> {resource= {image={ico=content}}}
          | {some = "gif"} -> {resource= {image={gif=content}}}
          | _ -> {resource = {source = content mime="application/octet-stream"}}
          save(filename, content)
        /* Waiting full file content */
        rec aux() =
          match file.content() with
          | {partial = _} ->
            Scheduler.sleep(1000, aux)
          | ~{content} ->
            filename = "/{file.filename}"
            do save(filename, content)
            do Log.notice("Uploader", "Uploading of {file.filename} was done")
            do Action.open_file(access, filename)(Dom.Event.default_event)
            void
        aux()
      /* Build admin xhtml body page */
      <div id="admin_files">
        <table id="admin_files_table">
          <caption> Published files </caption>
          {
           /* Insert xhtml list of <tr><td> */
           StringMap.rev_fold(
            (file, _, lxhtml -> file_line(access, file == url, file) +> lxhtml),
            page_map, []
           )
          }
        </table>
      </div>
      <div id="admin_editor_container">
        {file_buffer(access, true, url)}
      </div>
      <div id="admin_buttons">
        <input  id="admin_new_file" type="text"/>
        <button type="button" onclick={Action.new_file(access)}>New file</button>
        {Upload.make({Upload.default_config with ~perform_file})}
      </div>

  }}

  /**
   * Provides an interface which allows to create and modify pages.
   * This iterface is composed :
   *   1 - A file viewer
   *   2 - A code editor
   *   3 - An action zone
   * This admin interface open by default an editor for [url] file.
   */
  admin(url, _user) =
    AdminGui.build(~{get_class get save remove}, url)

  /**
   * Return a record corresponding to the resource at [url].
   * - [{embedded = ...}] Means that page should be embedded into
   *   applications, that provides a header and body field.
   * - [{resource = ...}] Is a raw resource that be able to return
   *   directly to the client.
   */
  resource(url) =
    match ?/pages/public[url] with
    | {some = ~{embedded}} -> embedded_to_xembedded(embedded)
    | {some = {resource = ~{image}}} -> {resource = Resource.image(image)}
    | {some = {resource = ~{css}}} -> {resource = Resource.build_css(css)}
    | {some = {resource = ~{source mime}}} -> {resource = Resource.source(source, mime)}
    | {none} -> match get_suffix(url) with
    | {some="html"} -> embedded_to_xembedded(/pages/not_found)
    | _ -> {resource = Resource.full_page("",
               Xhtml.of_string_unsafe(/pages/not_found/body),
               Xhtml.of_string_unsafe(/pages/not_found/header),
               {wrong_address}, []
             )}


}}
