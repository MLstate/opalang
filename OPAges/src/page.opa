package opages
import opace
import stdlib.crypto
import stdlib.web.template
import stdlib.web.client
import stdlib.upload

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

type Page.env = {
  current_url : string
}

/**
 * A configuration for initialize a page entity
 */
type Page.config('a, 'b) = {
  access : Page.config.access
  engine : Page.env -> Template.engine('a, 'b)
}

type Page.config.access = {
  set : string, Page.stored -> void
  set_rev : string, int -> void
  get : string -> option(Page.stored)
  get_rev : string -> option(Page.stored)
  get_rev_number : string -> int
  rm  : string -> void
  ls  : -> list((string,int))
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
@abstract type Page.stored_content =
  {image : image} /
  {css   : string} /
  {source : string mime : string} /
  {binary : binary mime : string} /
  /* Contains sourced template */
  {hcontent : string
   bcontent : string}

@abstract type Page.stored =
  {author : string
   content : Page.stored_content
   date : Date.date}

/**
 * A record used for give some primitive to the admin GUI unless
 * publish critical primitives.
 */
@private type Page.access = {
  save : string, Page.stored_content -> void
  set_rev : string, int -> void
  get : string -> option(Page.stored)
  get_edit : string -> Page.edit
  get_rev_number : string -> int
  remove : string -> void
  list : -> list((string,int))
  date : string -> Date.date
  history_size : string -> int
  history_list : string -> list((string,Date.date))
  history_edit : string, int -> option(Page.edit)
}

@private type Page.notify = {
  subscribe : ({event : Page.Notification.event by : string} -> void) -> void
  send    :  Page.Notification.event -> void
}

@private type Page.locker = {
  release : string -> Page.Lock.return
  try : string -> Page.Lock.return
  check : string -> Page.Lock.return
  force_release : string -> Page.Lock.return
}

@private type Page.full_access = {
  access : Page.access
  notify : Page.notify
  locker : Page.locker
}

@private type Page.edit =
  {image}
/ {binary
   mime:string
   save:{mime:string} -> void
   set_preview:{mime:string} -> void}
/ {source : string; mime : string}
/ {css : string}
/ {hsource : string
   bsource : string
   save_template : {hsource : string; bsource : string} -> option(Template.failure)
   preview_template : {hsource : string; bsource : string} -> option(Template.failure)}

type Page.Notification.event =
  {connect}
/ {publish : (string,int)}
/ {open : string}
/ {save : string}
/ {remove : string}
/ {lock : string}
/ {release : string}
/ {talk : string}

type Page.Lock.return = outcome(
  {lock_at:Date.date by : string},

  {internal_error}
/ {locked : {lock_at : Date.date by : string lock_by : ThreadContext.client}}
/ {unlocked}
)

Page = {{

  /**
   * {2 Privates preview utils}
   */

  @private preview_context = UserContext.make(StringMap.empty:stringmap(Page.stored_content))

  @private set_preview(key,page) =
    UserContext.change(StringMap.add(key,page,_), preview_context)

  @private has_preview(key) =
    UserContext.execute(StringMap.mem(key,_), preview_context)

  @private get_preview(key) =
    UserContext.execute(StringMap.get(key,_), preview_context)

  @private del_preview(key) =
    UserContext.change(StringMap.remove(key,_), preview_context)

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
    | {success = ~{lock_at by}} -> "Success : lock at {Date.to_string(lock_at)} by {by}"
    | {failure = {internal_error}} -> "Failure : lock internal error"
    | {failure = {unlocked}} -> "Failure : seems already unlocked"
    | {failure = {locked = ~{lock_at lock_by by}}} ->
      "Failure : already locked at {Date.to_string(lock_at)} by {by}    (with context {lock_by})"

  @private
  search_dead_link_for_resource(access, resource_id):option(list(string)) = (
    inside_href_parser = (parser | t=((!"\"" .)+) -> Text.to_string(t))
    href_parser = (parser | "href=\"" url=inside_href_parser "\"" -> url)
    sep = (parser | (!"href=\"" .)+ -> void)
    link_parser = (parser | lst={Rule.parse_list_sep(true, href_parser, sep)} sep  -> lst)
    Option.switch( {content=stored author=_ date=_ } ->
      match stored with
      | { ~bcontent; hcontent=_ } -> Parser.try_parse(link_parser, bcontent)
      | _ -> none
      , none
      , access.access.get(resource_id))
    )

  @private
  fetch_dead_links(access) : stringmap(list(string)) = (
    List.foldl((el,_), acc ->
      do Log.debug("Admin", "Search for dead links in {el}")
      match search_dead_link_for_resource(access, el) with
      | { ~some } -> StringMap.add(el,some, acc)
      | {none } -> acc
    , access.access.list()
    , StringMap_empty )
  )

  /**
   * {2 Database access}
   */

  //Hack because we no slicer annot on functor
  @client @private get_listener() =
    handler(state, msg) = match msg
      | ~{handler} -> {set = handler +> state}
      | ~{event by} -> do Scheduler.push(-> List.iter(handler -> handler(~{event by}), state)) {unchanged}
    Session.make([], handler)

  @server @private ServerAccess(~{engine access=caccess}:Page.config('a, 'b)) = {{

    /**
     * Network used for the notification systemp
     */
    network = Network.empty()
      : Network.network(
          { by: string; event: Page.Notification.event }
        / { handler: { by: string; event: Page.Notification.event } -> void }
      )

    /**
     * {2 Notification system + Lock system for one user}
     */
    Notification_Lock =
      lock_cell = Cell.make(StringMap.empty,
        (map, message ->
          by = message.by
          match message.action
          | ~{lock} ->
            match StringMap.get(lock, map)
            | ~{some} -> match ThreadContext.get({current}).key
              | ~{client} ->
                if client == some.lock_by then
                  {return = {success={lock_at = some.lock_at ~by}} instruction = {unchanged}}
                else
                  {return = {failure={locked = some}} instruction = {unchanged}}
              | _ -> {return = {failure={locked = some}} instruction = {unchanged}}
              end
            | {none} -> match ThreadContext.get({current}).key
              | {client=lock_by} ->
                lock_at = Date.now()
                {return = {success=~{lock_at by}}
                 instruction = {set = StringMap.add(lock, ~{by lock_by lock_at}, map)}}
              | _ -> {return = {failure={internal_error}} instruction = {unchanged}}
              end
            end
          | ~{unlock force} ->
            match StringMap.get(unlock, map)
            | {none}  -> {return = {failure={unlocked}} instruction = {unchanged}}
            | ~{some} -> match ThreadContext.get({current}).key
              | ~{client} ->
                if force || client == some.lock_by then
                  {return = {success={lock_at = some.lock_at ~by}}
                   instruction = {set = StringMap.remove(unlock, map)}}
                else
                  {return = {failure={locked = some}}
                   instruction = {unchanged}}
              | _ -> {return = {failure={internal_error}} instruction = {unchanged}}
              end
            end
          | ~{check} ->
            match StringMap.get(check, map)
            | ~{some} -> match ThreadContext.get({current}).key
              | ~{client} ->
                if client == some.lock_by then
                  {return = {success={lock_at = some.lock_at ~by}} instruction = {unchanged}}
                else
                  {return = {failure={locked = some}} instruction = {unchanged}}
              | _ -> {return = {failure={internal_error}} instruction = {unchanged}}
              end
            | {none} -> {return = {success={lock_at = Date.now() ~by}} instruction = {unchanged}}
        )
      )
      by ->
        Notification = {{

          @private client_listener =
            r = Mutable.make(none)
            -> match r.get()
               | {none} ->
                 c = get_listener()
                 do r.set({some = c})
                 do Network.add(c, network)
                 c
               | ~{some} -> some



          send(event:Page.Notification.event) =
            Network.broadcast(~{event by}, network)

          subscribe(handler) = Session.send(client_listener(), ~{handler})
        }}
        Lock = {{
          @private wrap(x) = {action=x ~by}

          @private wrap_result(event, result) = match result
            | {success=_} -> do Notification.send(event) result
            | _ -> result

          try(url) = wrap_result({lock=url}, Cell.call(lock_cell, wrap({lock=url})))

          release(url) = wrap_result({release=url}, Cell.call(lock_cell, wrap({unlock=url force=false})))

          force_release(url) = wrap_result({release=url}, Cell.call(lock_cell, wrap({unlock=url force=true})))

          check(url) = Cell.call(lock_cell, wrap({check=url}))
        }}
        (Notification, Lock)


    access(user) : Page.access    =
      get(key) : option(Page.stored) = caccess.get_rev(key)

      save(key, page : Page.stored_content) : void= caccess.set(key, {author=user content=page date=Date.now()})

      remove(key) = caccess.rm(key)

      date(key) = caccess.date(key)

      list() = caccess.ls()

      save_as_template(key, ~{hsource bsource}) =
        engine = engine({current_url=""})
        match Template.try_parse_with_conf(template_config, engine, hsource)
        | ~{failure} -> some(failure)
        | {success = hcontent} ->
          match Template.try_parse_with_conf(template_config, engine, bsource)
          | ~{failure} -> some(failure)
          | {success = bcontent} ->
            hcontent = Template.to_source(engine, hcontent)
            bcontent = Template.to_source(engine, bcontent)
            do Log.notice("PageDebug", "Saving headers {hcontent}")
            do save(key, ~{hcontent bcontent})
            none

    preview_template(key, ~{hsource bsource}) =
      engine = engine({current_url=""})
      match Template.try_parse_with_conf(template_config, engine, hsource)
      | ~{failure} -> some(failure)
      | {success = h} ->
        match Template.try_parse_with_conf(template_config, engine, bsource)
        | ~{failure} -> some(failure)
        | {success = b} ->
          page = {hcontent=Template.to_source(engine, h)
                  bcontent=Template.to_source(engine, b)
                 }:Page.stored_content
          do set_preview(key, page)
          none

      make_edit(key, {content=stored date=_ author=_} : Page.stored) = match stored
        | {image=_} -> {image}
        | {css=_} as k
        | {source=_ mime=_} as k -> k
        | ~{binary mime} ->
          save(~{mime}) = save(key, ~{binary mime})
          set_preview(~{mime}) = set_preview(key, ~{binary mime})
          {binary ~mime ~save ~set_preview}
        | ~{hcontent bcontent} -> {
            hsource = hcontent
            bsource = bcontent
            save_template = save_as_template(key, _)
            preview_template = preview_template(key, _)
          } : Page.edit


      get_edit(key):Page.edit =
        stored_page = match get_preview(key) with
          | {some=s} ->
            some({content=s date=Date.now() author=""})
          | {none} -> caccess.get(key)
        match stored_page with
        | {some=stored} -> make_edit(key, stored)
        | {none} ->
          match get_suffix(key)
          | {some="html"} | {some = "xmlt"}-> {
              hsource = "<!-- Enter your headers here -->"
              bsource = "<!-- Enter your body here --> "
              save_template = save_as_template(key, _)
              preview_template = preview_template(key, _)
            }
          | {some="css"} -> {css = "/* Enter your css here */"}
          | _ -> {source = "A custom resource" mime="text/plain"}


      history_size(key) = List.length(caccess.history(key, 1, 0))

      history_list(key) = List.map(x -> (x.author,x.date) ,caccess.history(key, 1, 0))

      history_edit(key, rev) = match caccess.history(key, rev, 1)
        | [] -> none
        | [stored] -> some(make_edit(key, stored))
        | _ -> do Log.error("Opages", "Unexpected multiple revisions") none

      ~{save get get_edit remove list date history_edit history_size history_list
        set_rev=caccess.set_rev
        get_rev_number=caccess.get_rev_number
       }

    full(user) =
      (Notification, Lock) = Notification_Lock(user)
      { access = access(user)
        locker = {
          try=Lock.try
          release=Lock.release
          force_release=Lock.force_release
          check=Lock.check
        } : Page.locker
        notify = {
          subscribe = Notification.subscribe
          send = Notification.send
        } : Page.notify
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

    @private file_line_content(access:Page.full_access, file, edit_rev, published_rev) =
      <td onclick={Action.open_file(access, file, published_rev)}>
        {file_for_xhtml(file)}
        {match published_rev with | {~some} -> " [pub #{some}]" | {none} -> ""}
        {match edit_rev with | {~some} -> " [editing #{some}]" | {none} -> ""}
      </td>

    /** Create a file line for table insert. */
    @private file_line(access:Page.full_access, opened, file, edit_rev, published_rev) =
      class = if opened then "on" else "off"
      <tr class="{class}" id="admin_files_table_{file_id(file)}">
        {file_line_content(access, file, edit_rev, published_rev)}
      </tr>

    /** Insert if necessary a file line into files table. */
    @private file_line_insert(access, opened, file, edit_rev, published_rev) =
      line = Dom.select_id("admin_files_table_{file_id(file)}")
      if Dom.is_empty(line) then
        /* Insert a line on files table. */
        table = Dom.select_id("admin_files_table")
        _ = Dom.put_at_end(table, Dom.of_xhtml(file_line(access, opened, file, edit_rev, published_rev)))
        void
      else
        do Dom.add_class(line, if opened then "on" else "off")
        _ = Dom.put_inside(line,Dom.of_xhtml(file_line_content(access, file, edit_rev, published_rev)))
        void
    /** 
     * Build a buffer and insert it on dom node with identifier
     * [id]. The buffer can be specialized according to file
     * extension.
     *
     * Note : Client side function because need dom access.
     */
    @client build_buffer(access, file, id) =
      do access.notify.send({open = file})
      history_id = "{id}_history"
      select_id = "{history_id}_select"
      update_id = "{history_id}_report"
      error_id = "{id}_error"
      
      /* Error reporting */
      make_reporting() =
        do Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(<div id={error_id}/>))
        void

      reporting(msg:xhtml) =
        ignore(Dom.put_inside(Dom.select_id(error_id), Dom.of_xhtml(msg)))

      /* Generate some buttons */

      /* Generate ace buffer */
      common_ace_init(id_buffer, class, content, mode) =
        dom = Dom.of_xhtml(<div id={id_buffer} class={class}/>)
        _ = Dom.put_at_end(Dom.select_id(id), dom)
        ace = Ace.edit(id_buffer)
        do Option.iter(x -> ignore(Ace.set_mode(ace, x)), mode)
        _ = Ace.set_content(ace, content)
        do Ace.move_cursor(ace, 0, 0)
        do Ace.read_only(ace, false)
        (dom, ace)

      on_ok_preview() =
        do reporting(<></>) 
        unpreview_id = "{id}_unpreview"
        _ = Client.winopen(file, {_blank},[], false)
        do Dom.set_style(#{unpreview_id},
                         [{display={inline}}])
        void

      /* Generate history selector */
      rec build_history_selector(rev : option(int)) =
        get_selected_rev() =
          Int.of_string(Dom.get_value(Dom.select_id(select_id)))
        action_history_current(_:Dom.event) = build(none)
        xhtml =
          // <div id={history_id} class="opages_history_browser">
          //   <button onclick={action_history_show}>Show selected revision</button>
          // </div>
          <div id={history_id}> Published revision : {access.access.get_rev_number(file)}</div>
        do access.notify.subscribe(
             | {event={~save} ~by} ->
               if save != file then void else
               build_history_selector(rev)
             | _ -> void
           )
        _ = Dom.remove(#{history_id})
        _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(xhtml))
        void

      and make_buttons(rev : option(int),action) =
        get_selected_rev() =
            Int.of_string(Dom.get_value(Dom.select_id(select_id)))
        action_change_rev(_:Dom.event)=
          build(some(get_selected_rev()))
        action_set_rev(_:Dom.event) =
          rev = get_selected_rev()
          do access.notify.send({publish=(file,rev)})
          do access.access.set_rev(file, get_selected_rev())
          build(some(rev))
        build_select(rev) =
          hist = access.access.history_list(file)
          size = List.length(hist)
          make_option(i : int ,(user,date)  : (string,Date.date)) : xhtml =(
            i = size - i
            value = "#{i} {Date.to_formatted_string(Date.debug_printer,date)} by {user}"
            default(i : int) : xhtml = (<option value="{i}">{value}</option>)
            selected(i : int) : xhtml = (<option value="{i}" selected="selected">{value}</option>)
            match rev
            | {none} ->    if i == size then selected(i) else default(i)
            | {some=rev} ->if i == rev  then selected(i) else default(i))

          <select id={select_id} onchange={action_change_rev}>
            {List.mapi(make_option,hist)}
          </select>
        buttons =
          common = <>
            <button type="button"
                    onclick={Action.remove_file(access, file)}>Remove</button>
          </>
          match action with
          | {some = action} ->
              unpreview_id = "{id}_unpreview"
              lock_button_id = "{id}_button_lock"
              save_button_id = "{id}_button_save"
              do access.notify.subscribe(
                   | {event={~lock} ~by} ->
                     if lock != file || Outcome.is_success(access.locker.check(file)) //so bad
                     then void else
                     b = Dom.select_id(save_button_id)
                     do Dom.set_enabled(b, false)
                     do action.read_only(true)
                     do Dom.put_inside(b, Dom.of_xhtml(<>Save : (locked by {by})</>))
                     void
                   | {event={~release} ~by} -> if release != file then void else
                     b = Dom.select_id(save_button_id)
                     do Dom.set_enabled(b, true)
                     do action.read_only(false)
                     do Dom.put_inside(b, Dom.of_xhtml(<>Save</>))
                     void
                   | {event={publish=(mfile,rev)} ~by } -> if mfile != file then void else
                     build_history_selector(some(rev))
                   | {event={~save} by=_} -> if save != file then void else
                     build_history_selector(rev)
                   | _ -> void
                 )
              make_lock_button(lock, x) =
                <button type="button" id={lock_button_id}
                        onclick={lock}>{x}</button>
              rec val lock_button = make_lock_button(action_try_lock, "Lock")
              and val unlock_button = make_lock_button(action_release, "Release")
              and action_try_lock(_:Dom.event) =
                result = access.locker.try(file)
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
                result = access.locker.release(file)
                do reporting(<>Release {lock_message(result)}</>)
                do Dom.put_replace(Dom.select_id(lock_button_id),
                                   Dom.of_xhtml(lock_button))
                do Dom.set_enabled(Dom.select_id(save_button_id), false)
                do action.read_only(true)
                void
              and action_force_release(_:Dom.event) =
                result = access.locker.force_release(file)
                do reporting(<>{lock_message(result)}</>)
                do Dom.set_enabled(Dom.select_id(save_button_id), false)
                do action.read_only(true)
                void
            <>
              {unpreview =
                 act(_) =
                   do del_preview(file)
                   do Dom.set_style(#{unpreview_id}, [{display={css_none}}])
                   void
                 display =
                   if has_preview(file) then {inline}
                   else {css_none}
                 <span style="display:{display};" id="{unpreview_id}">
                   <button type="button" onclick={act}>
                      Disable preview
                   </button>
                 </span>;
               <>
                 <button type="button" onclick={_->action.preview()}>
                   Preview
                 </button>
                 {unpreview}
               </>}
              <div>Current Revision : {build_select(rev)}</div>
              <div class="button_group" style="display:inline-block; margin-right:40px" >
                {match access.locker.check(file)
                 | {success = _} ->
                   <button type="button"
                           id={save_button_id}
                           onclick={_ -> action.save()}>Save</button>
                 | ~{failure} ->
                   by = match failure | {locked = {~by ...}} -> by | _ -> "501??!!??"
                   <button type="button"
                     disabled="disabled"
                     id={save_button_id}
                     onclick={_ -> action.save()}>Save (locked by {by})</button>
                }
                <button type="button" onclick={action_change_rev}> Discard change </button>
                <button type="button"
                        onclick={action_set_rev}>Publish</button>
              </div>
              <div class="button_group" style="display:inline-block; margin-right:40px">
                {lock_button}
                <button type="button"
                        onclick={action_force_release}>ForceRelease</button>
              </div>
              <div class="button_group" style="display:inline-block; margin-right:40px">
                {common}
              </div>
            </>
          | {none} -> common
        _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(buttons))
        void


      and build_buffers(rev : option(int)) =
        do jlog("revision #{rev}")
        edit = match rev
          | {none} -> access.access.get_edit(file)
          | {some=rev} -> Option.get(access.access.history_edit(file, rev))
        match edit with
        | {image} ->
          dom = Dom.of_xhtml(<img src="{file}"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          make_buttons(rev,none)
        | {binary ~mime ~save ~set_preview} ->
          mime_id = "mime_{id}"
          x = <>Uneditable binary file, Mimetype
                <input type="text" id="{mime_id}" value="{mime}"/></>
          _ = Dom.put_at_end(Dom.select_id(id), Dom.of_xhtml(x))
          save() =
            do save({mime = Dom.get_value(Dom.select_id(mime_id))})
            do access.notify.send({save = file})
            void
          make_buttons(rev,some({
              ~save
              read_only(_) = void
              preview() =
                page = {mime = Dom.get_value(Dom.select_id(mime_id))}
                do set_preview(page)
                on_ok_preview()
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
          do make_buttons(rev,some({
               save() =
                 do access.access.save(file, {source=Ace.get_content(ace) mime=get_mime()})
                 do access.notify.send({save = file})
                 void
               read_only(b) = Ace.read_only(ace, b)
               preview() =
                 page = {source=Ace.get_content(ace)
                         mime=get_mime()}
                do set_preview(file, page)
                on_ok_preview()
            }))
          dom = Dom.of_xhtml(<input type="text" value="{mime}" id="{id_mime}"/>)
          _ = Dom.put_at_end(Dom.select_id(id), dom)
          void
        | ~{css} ->
          /* Css editor is just a simple ace editor */
          id_css = "{id}_css"
          (_, ace) = common_ace_init(id_css, ["admin_editor_css"], css, some("css"))
          make_buttons(rev,some({
              save() =
                do access.access.save(file, {css=Ace.get_content(ace)})
                do access.notify.send({save = file})
                void
              read_only(b) = Ace.read_only(ace, b)
              preview() =
                page = {css=Ace.get_content(ace)}
                do set_preview(file, page)
                on_ok_preview()
            }))
        | ~{hsource bsource save_template preview_template} ->
          /* Html embedded editor is divided in two ace editor, one for
          headers another for body */
          head_id = "{id}_headers"
          body_id = "{id}_body"
          /* Head ace */
          (_, ace_head) = common_ace_init(head_id, ["admin_editor_html_header"],
                                          hsource, some("html"))
          /* Body ace */
          (_, ace_body) = common_ace_init(body_id, ["admin_editor_html_body"],
                                          bsource, some("html"))
          /* Buffer buttons */
          make_buttons(rev,
            some({
              save() =
                fail = save_template({
                  hsource = Ace.get_content(ace_head)
                  bsource = Ace.get_content(ace_body)
                })
                match fail with
                | {none} ->
                  do access.notify.send({save = file})
                  reporting(<>Save success</>)
                | {some=fail} -> reporting(<>{"{fail}"}</>)

             read_only(b) =
               do Ace.read_only(ace_head, b)
               do Ace.read_only(ace_body, b)
               void

             preview() =
               fail = preview_template({
                  hsource = Ace.get_content(ace_head)
                  bsource = Ace.get_content(ace_body)
               })
               match fail with
               | {none} -> on_ok_preview()
               | {some=fail} -> reporting(<>{"{fail}"}</>)

            })
          )

        and build(rev : option(int)) =
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
    @private file_buffer(access:Page.full_access, opened, file) =
      id = "admin_buffer_{file_id(file)}"
      class = if opened then "on" else "off"
      <div class="{class} admin_editor" id="{id}"
       onready={_ -> _ = build_buffer(access, file, id) void}>
      </div>

    /**
     * Create and insert a buffer for given [file] on dom.
     */
    @client insert_buffer(access:Page.full_access, opened, file) =
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
      open_file(access:Page.full_access, file,pub) : FunAction.t = _event ->
        do all_off()
        do file_line_insert(access, true, file, none, pub)
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
      new_file(access:Page.full_access) : FunAction.t = event ->
        /* Select file to open */
        file = Dom.get_value(Dom.select_id("admin_new_file"))
        do file_line_insert(access, true, file, none, none)
        open_file(access, file,none)(event)

      /**
       * Remove a file
       */
      remove_file(access:Page.full_access, file) : FunAction.t = _event ->
        do access.access.remove(file)
        do access.notify.send({remove = file})
        fl  = Dom.select_id("admin_files_table_{file_id(file)}")
        buf = Dom.select_id("admin_buffer_{file_id(file)}")
        do Dom.remove(fl)
        do Dom.remove(buf)
        void

      send_message(access:Page.full_access) : FunAction.t = _event ->
        input = Dom.select_id("admin_notifications_action_msg")
        talk = Dom.get_value(input)
        do access.notify.send(~{talk})
        Dom.set_value(input, "")

      search_dead_links(access:Page.full_access) : FunAction.t = _event ->
        do all_off()
        dead_links = fetch_dead_links(access)
        buf = Dom.select_id("admin_buffer_dead_links")
        edito = Dom.select_id("admin_editor_container")
        buffer = Dom.of_xhtml(
          <div id=#admin_buffer_dead_links class="on" >
            <ul>
              {StringMap.fold(key, val, acc ->
                <>{acc}
                  <li>{key} : {List.to_string_using("","",", ", val)}</li>
                </>
              , dead_links
              , <></>)}
            </ul>
          </div>
        )
        if Dom.is_empty(buf) then
          _ = Dom.put_at_end(edito, buffer)
          void
        else
          do Dom.add_class(buf, "on")
          do Dom.remove_class(buf, "off")
          void

    }}

    get_filename_mime() =
      (Dom.get_value(Dom.select_id("admin_new_file")),
       Dom.get_value(Dom.select_id("upload_mime_type")))

    @client @private hack(access:Page.full_access)(_:Dom.event) =
      d = Dom.select_id("admin_files_table")
      page_list = List.unique_list_of(access.access.list())
      do List.iter(
        (file,pub) ->
          file_line_insert(access, false, file, none,
            (if pub==0 then none else some(pub))),
        page_list)
      access.notify.subscribe(
        | {event={publish=(file,rev)} ~by} ->
          file_line_insert(access, false, file, none,some(rev))
        | _ -> void
      )

    /**
     * Build the xhtml administration interface.
     */
    build(access:Page.full_access, url) =
      /* Add default page if url does not already exists on page map */
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
          do access.access.save(result.filename, {~binary ~mime})
          do Action.open_file(access, result.filename,none)(Dom.Event.default_event)
          void
      /* Build admin xhtml body page */
      <div id="admin_files">
        <table id="admin_files_table" onready={hack(access)}>
          <caption> Published files </caption>
        </table>
      </div>
      <div id="admin_notifications">
        <div id="admin_notifications_box" onready={_ ->
          handler(~{event by}) =
            message = match event
              | {connect} -> "{by} is now connected"
              | ~{save} -> "{by} save {save}"
              | ~{open} -> "{by} open {open}"
              | ~{lock} -> "{by} lock {lock}"
              | ~{release} -> "{by} release {release}"
              | ~{remove} -> "{by} remove {remove}"
              | ~{talk} -> "{by} says {talk}"
              | ~{publish} -> "{by} publish {publish.f1} rev {publish.f2}"
            xhtml = <div>{message}</div>
            dom = Dom.select_id("admin_notifications_box")
            _ = Dom.put_at_end(dom, Dom.of_xhtml(xhtml))
            do Dom.scroll_to_bottom(dom)
            void
          do access.notify.subscribe(handler)
          do access.notify.send({connect})
          void
        }>
        </div>
        <div id="admin_notifications_action">
          <input type="text" onnewline={Action.send_message(access)} id="admin_notifications_action_msg"/>
          <button onclick={Action.send_message(access)}>Send a message</button>
        </div>
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
        <div id="search_dead_link_button">
          <button type="button" onclick={Action.search_dead_links(access)}>Search for dead links</button>
        </div>
      </div>

  }}

  @private build_resource(engine, content, modified_on) =
    add_web_cache(r) =
      Option.switch(modified_on -> Resource.cache_control(r, ~{modified_on}), r, modified_on)
    match content with
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
  make(config:Page.config('a, 'b)) : Page.manager =
    srvacc = ServerAccess(config)
    access = srvacc.access("server")
    admin(url, user) =
      AdminGui.build(srvacc.full(user), url)
    source(url) = Option.bind(
      (page:Page.stored -> match page.content
        | ~{css} -> some(css)
        | ~{source ...} -> some(source)
        | ~{bcontent ...} -> some(bcontent)
        | _ -> none),
        access.get(url))
    resource(url) =
      engine = config.engine({ current_url = url })
      res = match get_preview(url) with
        | {some=r} -> some(r)
        | {none} -> Option.map(x->x.content,access.get(url))
      match res with
      | {none} -> {resource =
          match access.get("404.xmlt")
          | {none} -> Resource.full_page("Not found",<h1>404 - Not found</h1>, <></>, {found}, [])
          | {some=r} ->
            match build_resource(engine, r.content, {some = access.date(url)})
            | {embedded = ~{body head}} -> Resource.full_page("", body, head, {found}, [])
            | x -> to_resource(x)
        }
      | {some=resource} ->
        build_resource(engine, resource, {some = access.date(url)})
    try_resource(url) =
      engine = config.engine({ current_url = url })
      Option.map((resource -> build_resource(engine, resource.content, {some = access.date(url)})),
                 access.get(url))
    date = access.date

    ~{admin resource try_resource date source}

  to_resource:Page.t -> resource =
    | ~{resource} -> resource
    | ~{embedded} -> Resource.full_page("", embedded.body, embedded.head,
                       {success}, [])

  empty: Page.stored =
    {author="server" content={source = "" mime = "text/plain" } date=Date.now()}
}}
