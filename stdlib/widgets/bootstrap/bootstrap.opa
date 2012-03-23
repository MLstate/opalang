/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * Bootstrapped widgets library.
 *
 * @author Frederic Ye 2011-2012
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 2.0
 */

/**
 * {1 TODO}
 *
 * - Forms
 * - Hero-unit
 * - Navbar ...
 * - Pager
 * - Progress bars
 * - JS
 */

/**
 * {1 Types definition}
 */

// Grid

type WBootstrap.Grid.column =
  { span: int // 1-12
    offset: option(int) // 1-12
    content: xhtml }

// Responsive

type WBootstrap.Responsive.visibility =
  {visible_phone}
/ {visible_tablet}
/ {visible_desktop}
/ {hidden_phone}
/ {hidden_tablet}
/ {hidden_desktop}

// List

type WBootstrap.List.description =
  {title: xhtml} /
  {description: xhtml}

// Thumbnail

type WBootstrap.Thumbnail.img = {
  href: option(string)
  onclick: Dom.event -> void
  src: string
  alt: string
}

// Navigation

type WBootstrap.Navigation.elt =
  {active: xhtml onclick: (Dom.event -> void) href: option(string)} /
  {inactive: xhtml onclick: (Dom.event -> void) href: option(string)} /
  {disabled: xhtml onclick: (Dom.event -> void) href: option(string)} /
  {custom_li: xhtml} /
  {divider}

type WBootstrap.Navigation.page_nav_elt = {
  href: option(string)
  onclick: Dom.event -> void
  content: xhtml
}


// Table

type WBootstrap.Table.elt = xhtml

type WBootstrap.Table.decoration =
  {bordered}
/ {striped}
/ {condensed}

// Button

type WBootstrap.Button.type =
  {button:xhtml callback:(Dom.event -> void)}
/ {link:xhtml href:option(string) callback:(Dom.event -> void)}
/ {input:string callback:(Dom.event -> void)}

type WBootstrap.Button.size =
  {normal} / {small} / {large} / {mini}

type WBootstrap.Button.importance =
  {default}
/ {primary}
/ {info}
/ {success}
/ {warning}
/ {danger}
/ {inverse}

type WBootstrap.Button.status = {enabled} / {disabled}

type WBootstrap.Button.option =
  WBootstrap.Button.size
/ WBootstrap.Button.importance
/ WBootstrap.Button.status

type WBootstrap.Button.button = {
  bt_type: WBootstrap.Button.type
  bt_options: list(WBootstrap.Button.option)
}

type WBootstrap.Button.group = list(WBootstrap.Button.button)

// Label

type WBootstrap.Label.importance =
  {default}
/ {success}
/ {warning}
/ {important}
/ {info}
/ {inverse}

// Badge

type WBootstrap.Badge.importance =
  {default}
/ {success}
/ {warning}
/ {error}
/ {info}
/ {inverse}

// Alert

type WBootstrap.Alert.type =
  {alert:WBootstrap.Alert.content closable:bool}
/ {block:WBootstrap.Alert.content actions:option(xhtml) closable:bool}

type WBootstrap.Alert.importance =
  {default}
/ {warning}
/ {error}
/ {success}
/ {info}

type WBootstrap.Alert.content = {
  title: string
  description: xhtml
}

/**
 * {1 About this module}
 *
 * This module is for generating Bootstrapped HTML chunks.
 *
 * {1 Where should I start?}
 *
 * Your pages needs the HTML5 doctype.
 *
 * {1 Compatibility?}
 *
 * Compatible with Bootstrap 2.0
 */
WBootstrap = {{

  /**
   * Add a pull-left class to an xhtml chunk
   */
  pull_left(x:xhtml) = Xhtml.update_class("pull-left", x)

  /**
   * Add a pull-right class to an xhtml chunk
   */
  pull_right(x:xhtml) = Xhtml.update_class("pull-right", x)

  /**
   * Add an optional href attribute to an xhtml
   */
  @private add_href_opt(href:option(string), x:xhtml) =
    match href
    {some=h} -> Xhtml.add_attribute_unsafe("href", h, x)
    {none} -> x

  Grid = {{

    @private
    row_private(columns:list(WBootstrap.Grid.column)) =
      <div>{
        @toplevel.List.map(~{span offset content} ->
          offset = match offset
                   {some=ofs} -> " offset{ofs}"
                   {none} -> ""
          <div class="span{span}{offset}">{content}</div>
        , columns)
      }</div>

    /**
     * Create a row
     *
     * @param columns a list of WBootstrap.Grid.column
     * @see {!WBootstrap.Grid.column} for column restrictions
     */
    row(cols) = row_private(cols) |> Xhtml.update_class("row", _)

    /**
     * Create a fluid row
     *
     * @param columns a list of WBootstrap.Grid.column
     * @see {!WBootstrap.Grid.column} for column restrictions
     */
    row_fluid(cols) = row_private(cols) |> Xhtml.update_class("row-fluid", _)

  }}

  Layout = {{

    /**
     * Create a fixed Layout
     */
    fixed(content:xhtml) =
      Div.container(content)

    /**
     * Create a fluid Layout
     */
    fluid(sidebar:xhtml, content:xhtml) =
      Div.container_fluid(
        <div class="sidebar">{sidebar}</div>
        <div class="content">{content}</div>
      )

  }}

  Responsive = {{

    /**
     * Decorate an xhtml chunk with some responsiveness visibilities
     */
    decorate(visibilities:list(WBootstrap.Responsive.visibility), xhtml:xhtml) =
      @toplevel.List.fold(visibility, acc ->
        cl = match visibility
             | {visible_phone} -> "visible-phone"
             | {visible_tablet} -> "visible-tablet"
             | {visible_desktop} -> "visible-desktop"
             | {hidden_phone} -> "hidden-phone"
             | {hidden_tablet} -> "hidden-tablet"
             | {hidden_desktop} -> "hidden-desktop"
        Xhtml.update_class(cl, acc)
      , visibilities, xhtml)

  }}

  Typography = {{

    /**
     * Create a header, with an optional sub-header
     * @param level 1 <= level <= 6
     */
    header(level:int, sub_content:option(xhtml), content:xhtml) =
      sub = match sub_content
            {some=s} -> <>{" "}</><small>{s}</small>
            {none} -> <></>
            end
      match level
      1 -> <h1>{content}{sub}</h1>
      2 -> <h2>{content}{sub}</h2>
      3 -> <h3>{content}{sub}</h3>
      4 -> <h4>{content}{sub}</h4>
      5 -> <h5>{content}{sub}</h5>
      6 -> <h6>{content}{sub}</h6>
      _ -> <></>

    /**
     * Create an HTML5 address
     */
    address(name:xhtml, address:xhtml) =
      <address>
        <strong>{name}</strong>
        <br/>
        {address}
      </address>

    /**
     * Create an abbreviation or acronym
     */
    abbrev(text:string, expanded_text:string, uppercase:bool) =
      <abbr title="{expanded_text}">{text}</abbr>
      |> (if uppercase then Xhtml.update_class("initialism", _)
          else identity)

    /**
     * Create a blockquote
     */
    blockquote(content:xhtml, author:xhtml) =
      <blockquote>
        <p>{content}</p>
        <small>{author}</small>
      </blockquote>

  // TODO: hero-unit

  }}

  Code = {{

    /**
     * Create an inline code snippet
     */
    inline(code:string) = <code>{code}</code>

    /**
     * Create a block of code, with multiple lines
     */
    block(code:string, limit_height:bool) =
      <pre>{code}</pre>
      |> (if limit_height then Xhtml.update_class("pre-scrollable", _)
          else identity)

    /**
     * Create a preformatted text, compatible with google-code-prettify
     */
    prettyprint(s:string, linenums:bool, lang:option(string)) =
      lang = match lang
             {some=l} -> " lang-{l}"
             {none} -> ""
      <pre>{Xhtml.of_string(s)}</pre>
      |> Xhtml.update_class("prettyprint{lang}", _)
      |> (if linenums then Xhtml.update_class("linenums", _)
          else identity)

  }}

  List = {{

    /**
     * Create an unordered list
     */
    unordered(list:list(xhtml)) =
      <ul>{
        @toplevel.List.map(e -> <li>{e}</li>, list)
      }</ul>

    /**
     * Create an unordered and unstyled list
     */
    unstyled(list:list(xhtml)) =
      unordered(list) |> Xhtml.update_class("unstyled", _)

    /**
     * Create an ordered list
     */
    ordered(list:list(xhtml)) =
      <ol>{
        @toplevel.List.map(e -> <li>{e}</li>, list)
      }</ol>

    /**
     * Create an description list
     */
    description(list:list(WBootstrap.List.description), horizontal:bool) =
      <dl>{
        @toplevel.List.map(e -> match e
          {title=t} -> <dt>{t}</dt>
          {description=d} -> <dd>{d}</dd>
        , list)
      }</dl>
      |> (if horizontal then Xhtml.update_class("dl-horizontal", _)
          else identity)

  }}

  Label = {{

    make_label(content:xhtml) =
      <span>{content}</span> |> Xhtml.update_class("label", _)

    success = Xhtml.update_class("label-success", _)
    warning = Xhtml.update_class("label-warning", _)
    important = Xhtml.update_class("label-important", _)
    info = Xhtml.update_class("label-info", _)
    inverse = Xhtml.update_class("label-inverse", _)

    /**
     * Create a label
     */
    make(text:xhtml, importance:WBootstrap.Label.importance) =
      lb = make_label(text)
      lb = match importance
          {default} -> lb
          {success} -> lb |> success(_)
          {warning} -> lb |> warning(_)
          {important} -> lb |> important(_)
          {info} -> lb |> info(_)
          {inverse} -> lb |> inverse(_)
      lb

  }}

  // FIXME: merge with label
  Badge = {{

    make_badge(content:string) =
      <span>{content}</span> |> Xhtml.update_class("badge", _)

    success = Xhtml.update_class("badge-success", _)
    warning = Xhtml.update_class("badge-warning", _)
    error = Xhtml.update_class("badge-error", _)
    info = Xhtml.update_class("badge-info", _)
    inverse = Xhtml.update_class("badge-inverse", _)

    /**
     * Create a label
     */
    make(text:string, importance:WBootstrap.Badge.importance) =
      lb = make_badge(text)
      lb = match importance
          {default} -> lb
          {success} -> lb |> success(_)
          {warning} -> lb |> warning(_)
          {error} -> lb |> error(_)
          {info} -> lb |> info(_)
          {inverse} -> lb |> inverse(_)
      lb

  }}

  Thumbnail = {{

    /**
     * Create a thumbnails grid
     *
     * FIXME: li span size, limit to img
     */
    grid(imgs:list(WBootstrap.Thumbnail.img)) =
      list =
        @toplevel.List.map(
          img ->
            <a onclick={img.onclick}
               class="thumbnail">
              <img src="{img.src}" alt="{img.alt}"/>
            </a> |> add_href_opt(img.href, _)
        , imgs)
      List.unordered(list) |> Xhtml.update_class("thumbnails", _)

  }}

  Table = {{

    @private gen_head(elts:list(WBootstrap.Table.elt)) =
      <thead><tr>{
        @toplevel.List.map(e -> <th>{e}</th>, elts)
      }</tr></thead>

    @private gen_body(lines:list(list(WBootstrap.Table.elt))) =
      <tbody>{
        @toplevel.List.map(
          line ->
            <tr>{
              @toplevel.List.map(
                e -> <td>{e}</td>
              , line)
            }</tr>
        , lines)
      }</tbody>

    /**
     * Create a table
     */
    table(head:list(WBootstrap.Table.elt), body:list(list(WBootstrap.Table.elt)), decorations:list(WBootstrap.Table.decoration)) =
      xhtml = <table class="table">
                {gen_head(head)}
                {gen_body(body)}
              </table>
      @toplevel.List.fold(decoration, acc ->
        cl = match decoration
             {bordered} -> "table-bordered"
             {striped} -> "table-striped"
             {condensed} -> "table-condensed"
        Xhtml.update_class(cl, acc)
      , decorations, xhtml)

    /**
     * Create a zebra stripped table
     */
    bordered(h, b) = table(h, b, [{bordered}])

    /**
     * Create a zebra striped table
     */
    zebra_stripped(h, b) = table(h, b, [{striped}])

    /**
     * Create a condensed table
     */
    condensed(h, b) = table(h, b, [{condensed}])

  }}

  Form = {{

    // TODO

    classic(content:xhtml) =
      <form action="javascript:void(0);">{content}</form>

    vertical(content:xhtml) =
      <form action="javascript:void(0);" class="form-vertical">{content}</form>

    horizontal(content:xhtml) =
      <form action="javascript:void(0);" class="form-horizontal">{content}</form>

    inline(content:xhtml) =
      <form action="javascript:void(0);" class="form-inline">{content}</form>

    search(content:xhtml) =
      <form action="javascript:void(0);" class="form-search">{content}</form>

    Fieldset = {{

      make(legend:option(xhtml), elements:list(xhtml)) =
        <>
          {match legend {some=l} -> <legend>{l}</legend> {none} -> <></>}
          {@toplevel.List.map(
            e -> <div class="control-group">{e}</div>
           , elements)}
        </>

    }}

  }}

  Button = {{

    make_button(content:xhtml, callback:(Dom.event -> void)) =
      <button onclick={callback}>{content}</button>
      |> Xhtml.update_class("btn", _) // CAUTION: cannot use class="btn" (see on top)

    make_no_propagation_button(content:xhtml, callback:(Dom.event -> void)) =
      <button onclick={callback} options:onclick={[{stop_propagation}]}>{content}</button>
      |> Xhtml.update_class("btn", _) // CAUTION: cannot use class="btn" (see on top)

    make_link(content:xhtml, href:option(string), callback:(Dom.event -> void)) =
      <a onclick={callback}>{content}</a>
      |> Xhtml.update_class("btn", _)
      |> add_href_opt(href, _)

    make_input(text:string, callback:(Dom.event -> void)) =
      <input type="button" value="{text}" onclick={callback}/>
      |> Xhtml.update_class("btn", _)

    primary = Xhtml.update_class("btn-primary", _)
    info = Xhtml.update_class("btn-info", _)
    success = Xhtml.update_class("btn-success", _)
    danger = Xhtml.update_class("btn-danger",_)
    warning = Xhtml.update_class("btn-warning",_)
    inverse = Xhtml.update_class("btn-inverse",_)
    large = Xhtml.update_class("btn-large", _)
    small = Xhtml.update_class("btn-small", _)
    mini = Xhtml.update_class("btn-mini", _)
    disabled = Xhtml.update_class("disabled", _) // FIXME: incomplete on buttons and inputs (see below)

    /**
     * Create a button
     */
    make(bt_type:WBootstrap.Button.type, bt_options_list:list(WBootstrap.Button.option)) =

      bt = match bt_type
           ~{button callback} ->
             make_button(button, callback)
           ~{link href callback} ->
             make_link(link, href, callback)
           ~{input callback} ->
             make_input(input, callback)

      bt_options = @toplevel.List.fold_right(
                     opts, opt ->
                       match opt
                       {primary} -> { opts with class={primary} }
                       {info} -> { opts with class={info} }
                       {success} -> { opts with class={success} }
                       {danger} -> { opts with class={danger} }
                       {warning} -> { opts with class={warning} }
                       {inverse} -> { opts with class={inverse} }
                       {small} -> { opts with size={small} }
                       {large} -> { opts with size={large} }
                       {mini} -> { opts with size={mini} }
                       {disabled} -> { opts with disabled=true }
                       _ -> opts
                   , bt_options_list, {class={default} size={normal} disabled=false})

      bt = match bt_options.class
          {default} -> bt
          {primary} -> bt |> primary(_)
          {info} -> bt |> info(_)
          {success} -> bt |> success(_)
          {danger} -> bt |> danger(_)
          {warning} -> bt |> warning(_)
          {inverse} -> bt |> inverse(_)

      bt = match bt_options.size
           {normal} -> bt
           {small} -> bt |> small(_)
           {large} -> bt |> large(_)
           {mini} -> bt |> mini(_)

      bt = match bt_options.disabled
           {false} -> bt
           {true} ->
             match bt_type
             {link=_ ...} -> bt |> disabled(_)
             _ -> bt |> disabled(_) |> Xhtml.update_attribute_unsafe("disabled", "disabled", _)
             end

      bt

    /**
     * Create a group of buttons
     */
    group(group:WBootstrap.Button.group) =
      <div class="btn-group">{
        @toplevel.List.map(button ->
          make(button.bt_type, button.bt_options)
        , group)
      }</div>

    /**
     * Create a toolbar of buttons group
     */
    toolbar(groups:list(WBootstrap.Button.group)) =
      <div class="btn-toolbar">{
        @toplevel.List.map(bt_group ->
          group(bt_group)
        , groups)
      }</div>

    /**
     * Create a dropdown button
     * Needs the dropdown plugin
     */
    dropdown(bt_type:WBootstrap.Button.type,
             bt_options_list:list(WBootstrap.Button.option),
             list:list(WBootstrap.Navigation.elt),
             up:bool) =
    bt_type = match bt_type
              ~{button callback} -> {button=<>{button}<span class="caret"/></> ~callback}
              ~{link href callback} -> {link=<>{link}<span class="caret"/></> ~href ~callback}
              _ -> bt_type
    bt = make(bt_type, bt_options_list)
         |> Xhtml.update_class("dropdown-toggle", _)
         |> Xhtml.add_attribute_unsafe("data-toggle", "dropdown", _)
    <div class="btn-group">
      {bt}
      <ul class="dropdown-menu">{
        @toplevel.List.map(Navigation.nav_elt_to_xhtml(false, identity), list)
      }</ul>
    </div>
    |> (if up then Xhtml.update_class("dropup", _)
        else identity)

    /**
     * Create a aplit button
     * Needs the dropdown plugin
     */
    split(bt_type:WBootstrap.Button.type,
          bt_options_list:list(WBootstrap.Button.option),
          list:list(WBootstrap.Navigation.elt),
          up:bool) =
    bt = make({button=<span class="caret"></span> callback=ignore}, bt_options_list)
         |> Xhtml.update_class("dropdown-toggle", _)
         |> Xhtml.add_attribute_unsafe("data-toggle", "dropdown", _)
    <div class="btn-group">
      {make(bt_type, bt_options_list)}
      {bt}
      <ul class="dropdown-menu">{
        @toplevel.List.map(Navigation.nav_elt_to_xhtml(false, identity), list)
      }</ul>
    </div>
    |> (if up then Xhtml.update_class("dropup", _)
        else identity)

  }}

  Navigation = {{

    @package nav_elt_to_xhtml(vertical_sep, f) =
      | {active=e ~onclick ~href} ->
        <li class="active">
          {<a onclick={onclick}>{e}</a>
          |> add_href_opt(href, _)
          |> f(_)}
        </li>
      | {inactive=e ~onclick ~href} ->
        <li>
          {<a onclick={onclick}>{e}</a>
          |> add_href_opt(href, _)
          |> f(_)}
        </li>
      | {disabled=e ~onclick ~href} ->
        <li class="disabled">
          {<a onclick={onclick}>{e}</a>
          |> add_href_opt(href, _)
          |> f(_)}
        </li>
      | {divider} -> <li class="divider{if vertical_sep then "-vertical" else ""}"></li>
      | {~custom_li} -> custom_li

    /**
     * Create a navbar
     */
    navbar(content:xhtml) =
      <div data-spy="scroll">
        <div class="navbar-inner">{content}</div>
      </div> |> Xhtml.update_class("navbar", _)

    /**
     * Create a fixed navbar (top or bottom)
     */
    fixed_navbar(content:xhtml, location:{top}/{bottom}) =
      xhtml = navbar(content)
      cl = if location == {top} then "top" else "bottom"
      Xhtml.update_class("navbar-fixed-{cl}", xhtml)

    // TODO: navbar forms

    /**
     * Create a brand link
     */
    brand(brand:xhtml, href:option(string), callback:(Dom.event -> void)) =
      html = match href
             {none} -> <span class="brand" onclick={callback}>{brand}</span>
             {some=_} -> <a class="brand" onclick={callback}>{brand}</a>
      add_href_opt(href, html)

    /**
     * Create a dropdown li (for use with WBootstrap.List)
     */
    dropdown_li(toggle:xhtml, href:option(string), list:list(WBootstrap.Navigation.elt)) =
      <li class="dropdown">
        {<a class="dropdown-toggle"
             data-toggle="dropdown">{toggle}</a>
         |> add_href_opt(href, _)}
        <ul class="dropdown-menu">{
          @toplevel.List.map(nav_elt_to_xhtml(false, identity), list)
        }</ul>
      </li>

    @private make_tabs(cl:string, tabs:list(WBootstrap.Navigation.elt), stacked:bool, f) =
      <ul class="nav">{
        @toplevel.List.map(nav_elt_to_xhtml(true, f), tabs)
      }</ul>
      |> Xhtml.update_class(cl, _)
      |> (if stacked then Xhtml.update_class("nav-stacked", _)
          else identity)

    nav(l, stacked) = make_tabs("", l, stacked, identity)
    tabs(l, stacked) = make_tabs("nav-tabs", l, stacked, Xhtml.add_attribute_unsafe("data-toggle", "tab", _))
    pills(l, stacked) = make_tabs("nav-pills", l, stacked, Xhtml.add_attribute_unsafe("data-toggle", "pill", _))

    // TODO: nav-list, tabbable nav

    /**
     * Create a breadcrumb
     */
    breadcrumb(path:list(WBootstrap.Navigation.elt), sep:xhtml) =
      <ul class="breadcrumb">{
        list = @toplevel.List.map(nav_elt_to_xhtml(false, identity), path)
        XmlConvert.of_list_using(<></>, <></>, Span.divider(sep), list)
      }</ul>

    /**
     * Create a pagination
     */
    pagination(pages:list(WBootstrap.Navigation.elt),
               prev:WBootstrap.Navigation.page_nav_elt,
               next:WBootstrap.Navigation.page_nav_elt,
               alignment:{left}/{centered}/{right}) =
      list = @toplevel.List.map(nav_elt_to_xhtml(true, identity), pages)
      is_disabled(l) = if @toplevel.List.is_empty(l)
                          || (match @toplevel.List.head(l) {active=_ href=_ onclick=_} -> true _ -> false)
                       then "disabled" else ""
      prev_disabled = is_disabled(pages)
      next_disabled = is_disabled(@toplevel.List.rev(pages))
      <div class="pagination">
        <ul>
          <li class="previous {prev_disabled}">
            {<a onclick={prev.onclick}>{prev.content}</a>
             |> add_href_opt(prev.href, _)}
          </li>
          {list}
          <li class="next {next_disabled}">
            {<a onclick={next.onclick}>{next.content}</a>
             |> add_href_opt(next.href, _)}
          </li>
        </ul>
      </div>
      |> (match alignment
          {centered} -> Xhtml.update_class("pagination-centered", _)
          {left} -> identity
          {right} -> Xhtml.update_class("pagination-right", _))

    // TODO: pager

  }}

  Alert = {{

    @private gen_make(closable:bool, content:WBootstrap.Alert.content, block:option(int), more:xhtml) =
      id = Dom.fresh_id()
      <div id=#{id} class="alert">
        {if not(closable) then <></>
         else <a class="close" data-dismiss="alert"
                 onclick={_->Dom.remove(#{id})}>&times;</a>}
        {if content.title == "" then <></>
         else match block
              {some=i} -> Typography.header(i, none, <>{content.title}</>)
                          |> Xhtml.update_class("alert-heading", _)
              {none} -> <strong>{content.title}</strong>}
        {content.description}
        {more}
      </div>

    make_alert(closable:bool, content:WBootstrap.Alert.content) =
      gen_make(closable, content, none, <></>)

    make_block(closable:bool, actions:option(xhtml), content:WBootstrap.Alert.content) =
      more = match actions
             {some=a} -> <div class="alert-actions">{a}</div>
             {none} -> <></>
      gen_make(closable, content, some(4), more) |> Xhtml.update_class("alert-block", _)

    warning = Xhtml.update_class("alert-warning", _)
    error = Xhtml.update_class("alert-error", _)
    success = Xhtml.update_class("alert-success", _)
    info = Xhtml.update_class("alert-info", _)

    /**
     * Create a message (alert or block)
     */
    make(msg_type:WBootstrap.Alert.type, msg_class:WBootstrap.Alert.importance) =

      msg = match msg_type
            ~{alert closable} -> make_alert(closable, alert)
            ~{block actions closable} -> make_block(closable, actions, block)

      msg = match msg_class
            {default} -> msg
            {warning} -> msg |> warning(_)
            {error} -> msg |> error(_)
            {success} -> msg |> success(_)
            {info} -> msg |> info(_)

      msg

  }}

  // TODO: progress bars

  Div = {{

    container(content:xhtml) =
      <div class="container">{content}</div>

    container_fluid(content:xhtml) =
      <div class="container-fluid">{content}</div>

    content(content:xhtml) =
      <div class="content">{content}</div>

    page_header(level:int, title:string, subtitle:option(string)) =
      sub = match subtitle
            {some=s} -> some(<>{s}</>)
            {none} -> none
      <div class="page-header">{Typography.header(level, sub, <>{title}</>)}</div>

    inner(content:xhtml) =
      <div class="inner">{content}</div>

    well(content:xhtml) =
      <div class="well">{content}</div>

  }}

  Span = {{

    divider(content:xhtml) =
      <span class="divider">{content}</span>

  }}

}}
