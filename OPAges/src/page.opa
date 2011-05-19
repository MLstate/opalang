profile(_name:string, f:-> 'a) = f()


package opages
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
 * A configuration for initialize a page entity
 */
type Page.config('a, 'b) = {
  access : Page.config.access
  engine : Template.engine('a, 'b)
  not_found : resource
}

type Page.config.access = {
  set : string, Page.stored -> void
  get : string -> option(Page.stored)
  rm  : string -> void
  ls  : -> list(string)
  date : string -> Date.date
}

/**
 * Define what is a page.
 * - embedded means that the page should be embedded in the applications.
 * - resource is a subset of [resource] that be able to store in database.
 */
type Page.t =
  {embedded : {head : xhtml body : xhtml}} /
  {resource : resource}

type Page.manager = {
  admin: string, string -> xhtml
  resource: string -> Page.t
  try_resource: string -> option(Page.t)
  date : string -> Date.date
}

/**
 * Type of data stored in database
 */
@abstract type Page.stored =
  {image : image} /
  {css   : string} /
  {source : string mime : string} /
  {binary : binary mime : string} /
  /* Contains sourced template */
  {hcontent : string
   bcontent : string}

/**
 * A record used for give some primitive to the admin GUI unless
 * publish critical primitives.
 */
@private type Page.access('a, 'b) = {
  save : string, Page.stored -> void
  get : string -> option(Page.stored)
  remove : string -> void
  get_edit : string -> Page.edit
  list : -> list(string)
  date : string -> Date.date
}

@private type Page.edit =
  {image} /
  {binary mime : string save : {mime : string} -> void} /
  {source : string; mime : string} /
  {css : string} /
  {hsource : string; bsource : string
   save : {hsource : string; bsource : string} -> option(Template.failure)}

Page = {{

  /**
   * {2 Privates utils}
   */
  /**
   * Get mode corresponding to the [file].
   */
  @private get_suffix(file) =
    match file with
    | "" | "/" -> some("html")
    | _ -> x = Parser.try_parse(Rule.has_suffix(Rule.alphanum_string), file)
           Option.map((x -> x.f2), x)

  /**
   * {2 Database access}
   */
  @server @private Access(~{engine access=caccess ...}:Page.config('a, 'b)) = {{
    get(key) = caccess.get(key)

    save(key, page) = caccess.set(key, page)

    remove(key) = caccess.rm(key)

    date(key) = caccess.date(key)

    list() = caccess.ls()

    save_as_template(key, ~{hsource bsource}) =
      match Template.try_parse(engine, hsource)
      | ~{failure} -> some(failure)
      | {success = hcontent} ->
        match Template.try_parse(engine, bsource)
        | ~{failure} -> some(failure)
        | {success = bcontent} ->
          do save(key, ~{hcontent=Template.to_source(engine, hcontent)
                         bcontent=Template.to_source(engine, bcontent)})
          none

    get_edit(key):Page.edit =
      match get(key) with
      | {some={image=_}} -> {image}
      | {some={css=_} as k}
      | {some={source=_ mime=_} as k} -> k
      | {some=~{binary mime}} ->
        save(~{mime}) = save(key, ~{binary mime})
        {binary ~mime ~save}
      | {some=~{hcontent bcontent}} -> {
          hsource = hcontent
          bsource = bcontent
          save = save_as_template(key, _)
        }
      | {none} ->
        match get_suffix(key)
        | {some="html"} | {some = "xmlt"}-> {
            hsource = "<!-- Enter your headers here -->"
            bsource = "<!-- Enter your body here --> "
            save = save_as_template(key, _)
          }
        | {some="css"} -> {css = "/* Enter your css here */"}
        | _ -> {source = "A custom resource" mime="text/plain"}

    access : Page.access    =
      ~{save get get_edit remove list date}

  }}

  /**
   * {2 Graphical user interface (admin)}
   */
  /**
   * This modules provides a build function for administration
   * interface.
   */
  @private AdminGui = {{
    @private save_id_button = "save_button"
    @private message_placeholder_id = "msg_placeholder"

    /** Just a simple string transformation for xhtml insert. */
    @private file_for_xhtml =
      | "/" -> "*index*"
      | x -> x

    /** A translation that allows to use file on dom identifiers. */
    @private file_id = Crypto.Hash.md5

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
      failure_msg_placeholder = Dom.of_xhtml(<div id={message_placeholder_id}></div>)
      _ = Dom.put_at_start(Dom.select_id(id), failure_msg_placeholder)
      make_buttons(save) =
        buttons =
          remove = <button type="button"
                    onclick={Action.remove_file(access, file)}>Remove</button>
          match save with
          | {some = save} ->
            <><button type="button"
                    onclick={_ -> save()}>Save</button>
            {remove}</>
          | {none} -> remove
        _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(buttons))
        void
      match access.get_edit(file) with
      | {image} ->
        dom = Dom.of_xhtml(<img src="{file}"/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        make_buttons(none)
      | {binary ~mime ~save} ->
        mime_id = "mime_{id}"
        x = <>Uneditable binary file, Mimetype
              <input type="text" id="{mime_id}" value="{mime}"/></>
        _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(x))
        save() = save({mime = Dom.get_value(Dom.select_id(mime_id))})
        make_buttons(some(save))
      | ~{source mime} ->
        /* A raw resource editor is an ace editor + an input for mime */
        id_src = "{id}_src"
        id_mime = "{id}_mime"
        dom = Dom.of_xhtml(<div id="{id_src}" class="admin_editor_source"/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        ace_src = Ace.edit(id_src)
        _ = Ace.set_content(ace_src, source)
        /* Buffer buttons */
        get_mime() =
          x = Dom.get_value(Dom.select_id(id_mime))
          x
        do make_buttons(some(->
             access.save(file, {source=Ace.get_content(ace_src) mime=get_mime()})
          ))
        dom = Dom.of_xhtml(<input type="text" value="{mime}" id="{id_mime}"/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        void
      | ~{css} ->
        /* Css editor is just a simple ace editor */
        id_css = "{id}_css"
        dom = Dom.of_xhtml(<div id="{id_css}" class="admin_editor_css"/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        ace_css = Ace.edit(id_css)
        _ = Ace.set_content(ace_css, css)
        _ = Ace.set_mode(ace_css, "css")
        make_buttons(some(->
            access.save(file, {css=Ace.get_content(ace_css)})
          ))
      | ~{hsource bsource save} ->
        /* Html embedded editor is divided in two ace editor, one for
        headers another for body */
        head_id = "{id}_headers" body_id = "{id}_body" error_id = "{id}_error"
        dom = Dom.of_xhtml(<div id="{head_id}" class="admin_editor_html_header"/>
                           <div id="{body_id}" class="admin_editor_html_body"/>
                           <span id="{error_id}"></span>
                          )
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        /* Head ace */
        ace_head = Ace.edit(head_id)
        do Ace.set_content(ace_head, hsource)
        _ = Ace.set_mode(ace_head, "html")
        /* Body ace */
        ace_body = Ace.edit(body_id)
        do Ace.set_content(ace_body, bsource)
        _ = Ace.set_mode(ace_body, "html")
        /* Buffer buttons */
        make_buttons(
          some(->
            fail = save({
              hsource = Ace.get_content(ace_head)
              bsource = Ace.get_content(ace_body)
            })
            msg(msg) =
              _ = Dom.put_inside(Dom.select_id(error_id), Dom.of_xhtml(msg))
              void
            match fail
            | {none} -> msg(<>Save success</>)
            | {some=fail} -> msg(<>{"{fail}"}</>)

          )
        )



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
    }}

    get_filename_mime() =
      (Dom.get_value(Dom.select_id("admin_new_file")),
       Dom.get_value(Dom.select_id("upload_mime_type")))

    /**
     * Build the xhtml administration interface.
     */
    build(access:Page.access, url) =
      /* Add default page if url does not already exists on page map */
      page_list = List.unique_list_of(url +> access.list())

      /* Perform an uploaded file */
      perform_file(file) =
        /* Save page resource for the uploaded file using file
           suffix. */
        // save(filename, content, mimetype) =
        //   content = match get_suffix(filename) with
        //   | {some = "png"} -> {image={png=content}}
        //   | {some = "jpg"} -> {image={jpg=content}}
        //   | {some = "ico"} -> {image={ico=content}}
        //   | {some = "gif"} -> {image={gif=content}}
        //   | _ -> {source = content mime=mimetype}
        //   access.save(filename, content)
        /* Waiting full file content */
        rec aux() =
          match file.content() with
          | {partial = _} ->
            Scheduler.sleep(1000, aux)
          | ~{content} ->
            (filename, mime) = get_filename_mime()
            filename = match String.trim(filename)
              | "" -> "/{file.filename}"
              | x -> x
            mime = match String.trim(mime)
              | "" -> "application/octet-stream"
              | x -> x
            do access.save(filename, {binary = content ~mime})
            do Log.notice("Uploader", "Uploading of {file.filename} ({mime}) was done")
            do Action.open_file(access, filename)(Dom.Event.default_event)
            void
        aux()
      /* Build admin xhtml body page */
      <div id="admin_files">
        <table id="admin_files_table">
          <caption> Published files </caption>
          {
           /* Insert xhtml list of <tr><td> */
           List.fold(file, lxhtml -> file_line(access, file == url, file) +> lxhtml, page_list, [])
          }
        </table>
      </div>
      <div id="admin_editor_container">
        {file_buffer(access, true, url)}
      </div>
      <div id="admin_buttons">
        <input  id="admin_new_file" type="text"/>
        <button type="button" onclick={Action.new_file(access)}>New file</button>
        { config = {Upload.default_config with ~perform_file
                    body_form = (<>
                      {Upload.default_config.body_form}
                      Mime-type <input id="upload_mime_type" type="text" value="application/octet-stream"/>
                    </>)
                    }
          Upload.make(config)}
      </div>

  }}

  @private build_resource(engine, stored) =
    match stored
    | ~{image}             -> {resource = Resource.image(image)}
    | ~{css}               -> {resource = Resource.build_css(css)}
    | ~{source mime}       -> {resource = Resource.source(source, mime)}
    | ~{binary mime}       -> {resource = Resource.source(binary, mime)}
    | ~{hcontent bcontent} ->
      {embedded = {
         head = Template.to_xhtml(engine, Template.parse(engine, hcontent))
         body = Template.to_xhtml(engine, Template.parse(engine, bcontent))
      } }

  /**
   * Provides an interface which allows to create and modify pages.
   * This iterface is composed :
   *   1 - A file viewer
   *   2 - A code editor
   *   3 - An action zone
   * This admin interface open by default an editor for [url] file.
   */
  make(config) : Page.manager =
    access = Access(config).access
    admin(url, _user:string) = AdminGui.build(access, url)
    resource(url) =
      match profile("Access.get({url})", -> access.get(url))
      | {none} -> {resource = config.not_found}
      | {some=resource} -> build_resource(config.engine, resource)
    try_resource(url) =
      Option.map((resource -> build_resource(config.engine, resource)),
                 access.get(url))
    date = access.date

    ~{admin resource try_resource date}

  to_resource:Page.t -> resource =
    | ~{resource} -> resource
    | ~{embedded} -> Resource.full_page("", embedded.body, embedded.head,
                       {success}, [])

  empty: Page.stored =
    { source = "" mime = "text/plain" }
}}
