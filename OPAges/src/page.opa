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
}

type Page.config.access = {
  set : string, Page.stored -> void
  set_rev : string, int -> void
  get : string -> option(Page.stored)
  get_rev : string -> option(Page.stored)
  get_rev_number : string -> int
  rm  : string -> void
  ls  : -> list(string)
  date : string -> Date.date
  history : string, int, int -> list(Page.stored)
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
  source : string -> option(string)
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
  set_rev : string, int -> void
  get : string -> option(Page.stored)
  get_edit : string -> Page.edit
  get_rev_number : string -> int
  remove : string -> void
  list : -> list(string)
  date : string -> Date.date
  release : string -> Page.Lock.return
  try_lock : string -> Page.Lock.return
  force_release : string -> Page.Lock.return
  history_size : string -> int
  history_edit : string, int -> option(Page.edit)
}

@private type Page.edit =
  {image} /
  {binary mime : string save : {mime : string} -> void} /
  {source : string; mime : string} /
  {css : string} /
  {hsource : string; bsource : string
   save : {hsource : string; bsource : string} -> option(Template.failure)}

type Page.Lock.return = outcome(
  {lock_at:Date.date},

  {internal_error}
/ {locked : {lock_at : Date.date lock_by : ThreadContext.client}}
/ {unlocked}
)

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

  @private template_config = { Template.default_conf with fallback_to_text_node = false }

  @private lock_message(r:Page.Lock.return) = match r
    | {success = ~{lock_at}} -> "Success : lock at {Date.to_string(lock_at)}"
    | {failure = {internal_error}} -> "Failure : lock internal error"
    | {failure = {unlocked}} -> "Failure : seems already unlocked"
    | {failure = {locked = ~{lock_at lock_by}}} ->
      "Failure : already locked at {Date.to_string(lock_at)} by {lock_by}"

  /**
   * {2 Database access}
   */
  @server @private Access(~{engine access=caccess ...}:Page.config('a, 'b)) = {{

    /**
     * {2 Lock system}
     */
    Lock = {{
      /**
       * Private cell that manage lock.
       */
      @private lock_cell = Cell.make(StringMap.empty,
        (map, message -> match message
          | ~{lock} ->
            match StringMap.get(lock, map)
            | ~{some} -> match ThreadContext.get({current}).key
              | ~{client} ->
                if client == some.lock_by then
                  {return = {success={lock_at = some.lock_at}} instruction = {unchanged}}
                else
                  {return = {failure={locked = some}} instruction = {unchanged}}
              | _ -> {return = {failure={locked = some}} instruction = {unchanged}}
              end
            | {none} -> match ThreadContext.get({current}).key
              | {client=lock_by} ->
                lock_at = Date.now()
                {return = {success=~{lock_at}}
                 instruction = {set = StringMap.add(lock, ~{lock_at lock_by}, map)}}
              | _ -> {return = {failure={internal_error}} instruction = {unchanged}}
              end
            end
          | ~{unlock force} ->
            match StringMap.get(unlock, map)
            | {none}  -> {return = {failure={unlocked}} instruction = {unchanged}}
            | ~{some} -> match ThreadContext.get({current}).key
              | ~{client} ->
                if force || client == some.lock_by then
                  {return = {success={lock_at = some.lock_at}}
                   instruction = {set = StringMap.remove(unlock, map)}}
                else
                  {return = {failure={locked = some}}
                   instruction = {unchanged}}
              | _ -> {return = {failure={internal_error}} instruction = {unchanged}}
              end
            end
        )
      )

      try(url) = Cell.call(lock_cell, {lock=url})

      release(url) = Cell.call(lock_cell, {unlock=url force=false})

      force_release(url) = Cell.call(lock_cell, {unlock=url force=true})

    }}

    get(key) = caccess.get_rev(key)

    save(key, page) = caccess.set(key, page)

    remove(key) = caccess.rm(key)

    date(key) = caccess.date(key)

    list() = caccess.ls()

    save_as_template(key, ~{hsource bsource}) =
      match Template.try_parse_with_conf(template_config, engine, hsource)
      | ~{failure} -> some(failure)
      | {success = hcontent} ->
        match Template.try_parse_with_conf(template_config, engine, bsource)
        | ~{failure} -> some(failure)
        | {success = bcontent} ->
          hcontent = Template.to_source(engine, hcontent)
          do Log.notice("PageDebug", "Saving headers {hcontent}")
          do save(key, ~{hcontent=hcontent
                         bcontent=Template.to_source(engine, bcontent)})
          none

    make_edit(key, stored) = match stored
      | {image=_} -> {image}
      | {css=_} as k
      | {source=_ mime=_} as k -> k
      | ~{binary mime} ->
        save(~{mime}) = save(key, ~{binary mime})
        {binary ~mime ~save}
      | ~{hcontent bcontent} -> {
          hsource = hcontent
          bsource = bcontent
          save = save_as_template(key, _)
        }


    get_edit(key):Page.edit =
      match caccess.get(key) with
      | {some=stored} -> make_edit(key, stored)
      | {none} ->
        match get_suffix(key)
        | {some="html"} | {some = "xmlt"}-> {
            hsource = "<!-- Enter your headers here -->"
            bsource = "<!-- Enter your body here --> "
            save = save_as_template(key, _)
          }
        | {some="css"} -> {css = "/* Enter your css here */"}
        | _ -> {source = "A custom resource" mime="text/plain"}


    history_size(key) = List.length(caccess.history(key, 1, 0))

    history_edit(key, rev) = match caccess.history(key, rev, 1)
      | [] -> none
      | [stored] -> some(make_edit(key, stored))
      | _ -> do Log.error("Opages", "Unexpected multiple revisions") none

    access : Page.access    =
      ~{save get get_edit remove list date history_edit history_size
        try_lock=Lock.try release=Lock.release force_release=Lock.force_release
        set_rev=caccess.set_rev
        get_rev_number=caccess.get_rev_number
       }

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
      error_id = "{id}_error"
      /* Error reporting */
      make_reporting() =
        do Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(<div id={error_id}/>))
        void
      reporting(msg:xhtml) =
        ignore(Dom.put_inside(Dom.select_id(error_id), Dom.of_xhtml(msg)))

      /* Generate some buttons */
      make_buttons(action) =
        buttons =
          common = <>
            <button type="button"
                    onclick={Action.remove_file(access, file)}>Remove</button>
          </>
          match action with
          | {some = action} ->
              save_button_id = "{id}_button_save"
              lock_button_id = "{id}_button_lock"
              make_lock_button(lock, x) =
                <button type="button" id={lock_button_id}
                        onclick={lock}>{x}</button>
              rec val lock_button = make_lock_button(action_try_lock, "Lock")
              and val unlock_button = make_lock_button(action_release, "Release")
              and action_try_lock(_:Dom.event) =
                result = access.try_lock(file)
                do reporting(<>{lock_message(result)}</>)
                match result
                | {success=_} ->
                  do ignore(Dom.put_replace(Dom.select_id(lock_button_id),
                                            Dom.of_xhtml(unlock_button)))
                  do Dom.set_enabled(Dom.select_id(save_button_id), true)
                  action.read_only(false)
                | _ ->
                  do Dom.set_enabled(Dom.select_id(save_button_id), false)
                  action.read_only(true)
              and action_release(_:Dom.event) =
                result = access.release(file)
                do reporting(<>Release {lock_message(result)}</>)
                do Dom.put_replace(Dom.select_id(lock_button_id),
                                   Dom.of_xhtml(lock_button))
                do Dom.set_enabled(Dom.select_id(save_button_id), false)
                do action.read_only(true)
                void
              and action_force_release(_:Dom.event) =
                result = access.force_release(file)
                do reporting(<>{lock_message(result)}</>)
                do Dom.set_enabled(Dom.select_id(save_button_id), false)
                do action.read_only(true)
                void
            <>
              <button type="button"
                      onclick={action_force_release}>ForceRelease</button>
              {lock_button}
              <button type="button"
                      id={save_button_id}
                      disabled="disabled"
                      onclick={_ -> action.save()}>Save</button>
              {common}
            </>
          | {none} -> common
        _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(buttons))
        void

      /* Generate ace buffer */
      common_ace_init(id_buffer, class, content, mode) =
        dom = Dom.of_xhtml(<div id={id_buffer} class={class}/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        ace = Ace.edit(id_buffer)
        do Option.iter(x -> ignore(Ace.set_mode(ace, x)), mode)
        _ = Ace.set_content(ace, content)
        do Ace.read_only(ace, true)
        (dom, ace)

      /* Generate history selector */
      rec build_history_selector(rev) =
        history_id = "{id}_history"
        select_id = "{history_id}_select"
        get_selected_rev() =
          Int.of_string(Dom.get_value(Dom.select_id(select_id)))
        action_history_show(_:Dom.event) =
          rev = some(get_selected_rev())
          do build(rev)
          void
        action_history_current(_:Dom.event) = build(none)
        action_set_rev(_:Dom.event) = access.set_rev(file, get_selected_rev())
        build_select() =
          size = access.history_size(file)
          make_option =
            default(i) = <option value="{i}">{i}</option>
            selected(i) = <option value="{i}" selected="selected">{i}</option>
            match rev
            | {none} -> i -> if i == size then selected(i) else default(i)
            | {some=rev} -> i -> if i == rev then selected(i) else default(i)
          <select id={select_id}>{
            for((1, []),
                ((i, acc) -> (i+1, make_option(i) +> acc)),
                ((i, _) -> i <= size)).f2
          }</select>
        xhtml =
          <div id={history_id} class="opages_history_browser">
            <span>Current revision : {access.get_rev_number(file)}</span>
            <button onclick={action_history_current}>Show last</button>
            <button onclick={action_history_show}>Show selected revision</button>
            <button onclick={action_set_rev}>Set public page as selected revision</button>
            {build_select()}
          </div>
        Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(xhtml))

      and build_buffers(rev) =
        edit = match rev
          | {none} -> access.get_edit(file)
          | {some=rev} -> Option.get(access.history_edit(file, rev))
        make_buttons = match rev | {some=_} -> _ -> void | _ -> make_buttons
        match edit with
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
          make_buttons(some({
              ~save
              read_only(_) = void
            }))
        | ~{source mime} ->
          /* A raw resource editor is an ace editor + an input for mime */
          id_src = "{id}_src"
          id_mime = "{id}_mime"
          (_, ace) = common_ace_init(id_src, ["admin_editor_source"], source, none)
          /* Buffer buttons */
          get_mime() =
            x = Dom.get_value(Dom.select_id(id_mime))
            x
          do make_buttons(some({
               save() = access.save(file, {source=Ace.get_content(ace) mime=get_mime()})
               read_only(b) = Ace.read_only(ace, b)
            }))
          dom = Dom.of_xhtml(<input type="text" value="{mime}" id="{id_mime}"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          void
        | ~{css} ->
          /* Css editor is just a simple ace editor */
          id_css = "{id}_css"
          (_, ace) = common_ace_init(id_css, ["admin_editor_css"], css, some("css"))
          make_buttons(some({
              save = -> access.save(file, {css=Ace.get_content(ace)})
              read_only(b) = Ace.read_only(ace, b)
            }))
        | ~{hsource bsource save} ->
          /* Html embedded editor is divided in two ace editor, one for
          headers another for body */
          head_id = "{id}_headers" body_id = "{id}_body"
          /* Head ace */
          (_, ace_head) = common_ace_init(head_id, ["admin_editor_html_header"],
                                          hsource, some("html"))
          /* Body ace */
          (_, ace_body) = common_ace_init(body_id, ["admin_editor_html_body"],
                                          bsource, some("html"))
          /* Buffer buttons */
          make_buttons(
            some({
              save() =
                fail = save({
                  hsource = Ace.get_content(ace_head)
                  bsource = Ace.get_content(ace_body)
                })
                match fail
                | {none} -> reporting(<>Save success</>)
                | {some=fail} -> reporting(<>{"{fail}"}</>)

             read_only(b) =
               do Ace.read_only(ace_head, b)
               do Ace.read_only(ace_body, b)
               void

            })
          )

        and build(rev) =
          do Dom.remove_content(Dom.select_id(id))
          /* Create view */
          do make_reporting()
          do build_buffers(rev)
          do build_history_selector(rev)
          void

        build(none)



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
      init_result = {mime=""
                     filename=""
                     content=none}
      fold_datas(data, result) = match data
        | ~{name="upload_file_content" filename content fold_headers} ->
          if filename=="" then result
          else
            mime=
              if result.mime=="" then
                @unsafe_cast(fold_headers)("",
                  (head, (value:string), mime -> match head | "Content_Type" -> value | _ -> mime)
                  )
              else result.mime
            content =
              rec aux() = match content()
                | {partial=_} -> do Scheduler.wait(500) aux()
                | ~{content} -> content
              aux()
            filename = if result.filename=="" then "/{filename}" else result.filename
            {~filename ~mime content=some(content)}
        | {name = "upload_mime_type" value=""} -> result
        | {name = "upload_mime_type" ~value}   ->
          value = String.trim(value)
          if value=="" then result else
          { result with mime=value }

        | {name = "upload_file_name" value=""} -> result
        | {name = "upload_file_name" ~value}   ->
          value = String.trim(value)
          if value=="" then result else
          { result with filename=value }
        | {~name filename=_ content=_ fold_headers=_}
        | {~name value=_}
         -> do Log.error("PageUploader", "Unknown field {name}") result
      perform_result(result) =
        match result.content
        | {none} -> Log.error("PageUploader", "No file to perform")
        | {some = binary} ->
          mime = if result.mime=="" then "application/octet-some" else result.mime
          do Log.notice("PageUploader", "Uploading of {result.filename} ({mime}) was done")
          do access.save(result.filename, {~binary ~mime})
          do Action.open_file(access, result.filename)(Dom.Event.default_event)
          void
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
        { config = Upload.default_config(init_result)
          config = { config with ~fold_datas ~perform_result
                       body_form = <>
                         Filename : <input  id="admin_new_file" name="upload_file_name" type="text"/>
                         <button type="button" onclick={Action.new_file(access)}>New file</button>
                         Mime-type <input name="upload_mime_type" type="text" value="application/octet-stream"/>
                         <input type="file" name="upload_file_content"/>
                         <button type="submit">Upload</button>
                       </> }
          Upload.make(config)}
      </div>

  }}

  @private build_resource(engine, stored, modified_on) =
    add_web_cache(r) =
      Option.switch(modified_on -> Resource.cache_control(r, ~{modified_on}), r, modified_on)
    match stored
    | ~{image}             -> {resource = add_web_cache(Resource.image(image))}
    | ~{css}               -> {resource = add_web_cache(Resource.build_css(css))}
    | ~{source mime}       -> {resource = add_web_cache(Resource.source(source, mime))}
    | ~{binary mime}       -> {resource = add_web_cache(Resource.source(binary, mime))}
    | ~{hcontent bcontent} ->
      {embedded = {
         head = Template.to_xhtml(engine, Template.parse(engine, hcontent))
         body = Template.to_xhtml(engine, Template.parse(engine, bcontent))
      } } : Page.t

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
    source(url) = Option.bind(
      (page:Page.stored -> match page
        | ~{css} -> some(css)
        | ~{source ...} -> some(source)
        | ~{bcontent ...} -> some(bcontent)
        | _ -> none),
        access.get(url))
    resource(url) =
      match access.get(url)
      | {none} -> {resource =
          match access.get("404.xmlt")
          | {none} -> Resource.full_page("Not found",<h1>404 - Not found</h1>, <></>, {found}, [])
          | {some=r} ->
            match build_resource(config.engine, r, {some = access.date(url)})
            | {embedded = ~{body head}} -> Resource.full_page("", body, head, {found}, [])
            | x -> to_resource(x)
        }
      | {some=resource} -> build_resource(config.engine, resource, {some = access.date(url)})
    try_resource(url) =
      Option.map((resource -> build_resource(config.engine, resource, {some = access.date(url)})),
                 access.get(url))
    date = access.date

    ~{admin resource try_resource date source}

  to_resource:Page.t -> resource =
    | ~{resource} -> resource
    | ~{embedded} -> Resource.full_page("", embedded.body, embedded.head,
                       {success}, [])

  empty: Page.stored =
    { source = "" mime = "text/plain" }
}}
