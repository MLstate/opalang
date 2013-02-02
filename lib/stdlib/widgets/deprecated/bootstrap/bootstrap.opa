/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Bootstrapped widgets library.
 *
 * @author Frederic Ye 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.9
 */

/**
 * {1 TODO}
 *
 * - Forms
 * - JS
 */

package stdlib.widgets.deprecated.bootstrap

/**
 * {1 Types definition}
 */

// Grid

type WBootstrap.Grid.column =
  { span: int // 1-16
    offset: option(int) // 1-16
    content: xhtml } /
  { third: int // 1-2
    offset: option(int) // 1-2
    content: xhtml }

// List

type WBootstrap.List.description =
  {title: xhtml} /
  {description: xhtml}

// Media

type WBootstrap.Media.media = {
  href: option(string)
  onclick: Dom.event -> void
  content: xhtml
}

// Navigation

type WBootstrap.Navigation.elt =
  {active: xhtml onclick: (Dom.event -> void) href: option(string)} /
  {inactive: xhtml onclick: (Dom.event -> void) href: option(string)} /
  {disabled: xhtml onclick: (Dom.event -> void) href: option(string)} /
  {custom_li: xhtml} /
  {divider}

type WBootstrap.Navigation.page_nav_elt = WBootstrap.Media.media

// Table

type WBootstrap.Table.elt = xhtml

// Button

type WBootstrap.Button.type =
  {button:xhtml callback:(Dom.event -> void)}
/ {link:xhtml href:option(string) callback:(Dom.event -> void)}
/ {input:string callback:(Dom.event -> void)}

type WBootstrap.Button.size =
  {normal} / {small} / {large}

type WBootstrap.Button.importance =
  {default}
/ {primary}
/ {info}
/ {success}
/ {danger}

type WBootstrap.Button.status = {enabled} / {disabled}

type WBootstrap.Button.option =
  WBootstrap.Button.size
/ WBootstrap.Button.importance
/ WBootstrap.Button.status

// Message

type WBootstrap.Message.type =
  {alert:WBootstrap.Message.content closable:bool}
/ {block:WBootstrap.Message.content actions:option(xhtml) closable:bool}

type WBootstrap.Message.importance =
  {default}
/ {warning}
/ {error}
/ {success}
/ {info}

type WBootstrap.Message.content = {
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
 * You can find documentation and examples at http://bootstrap.opalang.org.
 *
 * {1 Compatibility?}
 *
 * It should be compatible with Bootstrap <= 1.4.0
 */
WBootstrap = {{

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

    /**
     * Create a row
     *
     * @param columns a list of WBootstrap.Grid.column
     * @see {!WBootstrap.Grid.column} for column restrictions
     */
    row(columns:list(WBootstrap.Grid.column)) =
      <div>{
        @toplevel.List.map(column ->
          match column
          ~{span offset content} ->
            offset = match offset
                     {some=ofs} -> " offset{ofs}"
                     {none} -> ""
            <div class="span{span}{offset}">{content}</div>
          ~{third offset content} ->
            offset = match offset
                     {some=1} -> " offset-one-third"
                     {some=2} -> " offset-two-thirds"
                     _ -> ""
            match third with
            1 -> <div class="span-one-third{offset}">{content}</div>
            2 -> <div class="span-two-thirds{offset}">{content}</div>
            _ -> <></>
        , columns)
      }</div> |> Xhtml.update_class("row", _)

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
     * Create a blockquote
     */
    blockquote(content:xhtml, author:xhtml) =
      <blockquote>
        <p>{content}</p>
        <small>{author}</small>
      </blockquote>

    /**
     * Create a preformatted text, compatible with google-code-prettify
     */
    prettyprint(s:string, linenums:bool, lang:option(string)) =
      lang = match lang
             {some=l} -> " lang-{l}"
             {none} -> ""
      <pre>{Xhtml.of_string(s)}</pre>
      |> Xhtml.update_class("prettyprint{lang}", _)
      |> (match linenums
          {false} -> identity
          {true} -> Xhtml.update_class("linenums", _))

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
    description(list:list(WBootstrap.List.description)) =
      <dl>{
        @toplevel.List.map(e -> match e
          {title=t} -> <dt>{t}</dt>
          {description=d} -> <dd>{d}</dd>
        , list)
      }</dl>

  }}

  Label = {{

    make_label(content:string) =
      <span>{content}</span> |> Xhtml.update_class("label", _)

    success = Xhtml.update_class("success", _)
    warning = Xhtml.update_class("warning", _)
    important = Xhtml.update_class("important", _)
    notice = Xhtml.update_class("notice",_)

    /**
     * Create a label
     */
    make(lb_text, lb_class) =
      lb = make_label(lb_text)
      lb = match lb_class
          {default} -> lb
          {success} -> lb |> success(_)
          {warning} -> lb |> warning(_)
          {important} -> lb |> important(_)
          {notice} -> lb |> notice(_)
      lb

  }}

  Media = {{

    /**
     * Create a media grid
     */
    grid(imgs:list(WBootstrap.Media.media)) =
      list =
        @toplevel.List.map(
          media ->
            content = media.content
            <a onclick={media.onclick}>
              {content}
            </a> |> add_href_opt(media.href, _)
        , imgs)
      List.unordered(list) |> Xhtml.update_class("media-grid", _)

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
    table(head:list(WBootstrap.Table.elt), body:list(list(WBootstrap.Table.elt))) =
      <table>
        {gen_head(head)}
        {gen_body(body)}
      </table>

    /**
     * Create a zebra stripped table
     */
    zebra_stripped(h, b) = table(h, b) |> Xhtml.update_class("zebra-striped", _)

  }}

  Form = {{

    // TODO

    classic(content:xhtml) =
      <form action="javascript:void(0);">{content}</form>

    stacked(content:xhtml) =
      <form action="javascript:void(0);" class="form-stacked">{content}</form>

    Fieldset = {{

      make(legend:option(xhtml), elements:list(xhtml)) =
        <>
          {match legend {some=l} -> <legend>{l}</legend> {none} -> <></>}
          {@toplevel.List.map(
            e -> <div class="clearfix">{e}</div>
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

    primary = Xhtml.update_class("primary", _)
    info = Xhtml.update_class("info", _)
    success = Xhtml.update_class("success", _)
    danger = Xhtml.update_class("danger",_)
    large = Xhtml.update_class("large", _)
    small = Xhtml.update_class("small", _)
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
                       {small} -> { opts with size={small} }
                       {large} -> { opts with size={large} }
                       {disabled} -> { opts with disabled=true }
                       _ -> opts
                   , bt_options_list, {class={default} size={normal} disabled=false})

      bt = match bt_options.class
          {default} -> bt
          {primary} -> bt |> primary(_)
          {info} -> bt |> info(_)
          {success} -> bt |> success(_)
          {danger} -> bt |> danger(_)

      bt = match bt_options.size
           {normal} -> bt
           {small} -> bt |> small(_)
           {large} -> bt |> large(_)

      bt = match bt_options.disabled
           {false} -> bt
           {true} ->
             match bt_type
             {link=_ ...} -> bt |> disabled(_)
             _ -> bt |> disabled(_) |> Xhtml.update_attribute_unsafe("disabled", "disabled", _)
             end

      bt


  }}

  Navigation = {{

    @private nav_elt_to_xhtml =
      | {active=e ~onclick ~href} ->
        <li class="active">
          {<a onclick={onclick}>{e}</a>
          |> add_href_opt(href, _)}
        </li>
      | {inactive=e ~onclick ~href} ->
        <li>
          {<a onclick={onclick}>{e}</a>
          |> add_href_opt(href, _)}
        </li>
      | {disabled=e ~onclick ~href} ->
        <li class="disabled">
          {<a onclick={onclick}>{e}</a>
          |> add_href_opt(href, _)}
        </li>
      | {divider} -> <li class="divider"></li>
      | {~custom_li} -> custom_li

    /**
     * Create a topbar
     */
    topbar(content:xhtml) =
      <div data-scrollspy="scrollspy">
        <div class="topbar-inner">{content}</div>
      </div> |> Xhtml.update_class("topbar", _)

    /**
     * Create a brand link
     */
    brand(brand:xhtml, href:option(string), callback:(Dom.event -> void)) =
      <a class="brand" onclick={callback}>{brand}</a>
      |> add_href_opt(href, _)

    /**
     * Create a dropdown li (for use with WBootstrap.List)
     */
    dropdown_li(toggle:xhtml, href:option(string), list:list(WBootstrap.Navigation.elt)) =
      a = <a class="dropdown-toggle">{toggle}</a>
          |> add_href_opt(href, _)
      <li class="dropdown" data-dropdown="dropdown">
        {a}
        <ul class="dropdown-menu">{
          @toplevel.List.map(nav_elt_to_xhtml, list)
        }</ul>
      </li>

    @private make_tabs(cl:string, tabs:list(WBootstrap.Navigation.elt)) =
      <ul>{
        @toplevel.List.map(nav_elt_to_xhtml, tabs)
      }</ul> |> Xhtml.update_class(cl, _)

    nav(l) = make_tabs("nav", l)
    tabs(l) = make_tabs("tabs", l) |> Xhtml.add_attribute_unsafe("data-tabs", "tabs", _)
    pills(l) = make_tabs("pills", l) |> Xhtml.add_attribute_unsafe("data-pills", "pills", _)

    /**
     * Create a breadcrumb
     */
    breadcrumb(path:list(WBootstrap.Navigation.elt), sep:xhtml) =
      <ul class="breadcrumb">{
        list = @toplevel.List.map(nav_elt_to_xhtml, path)
        XmlConvert.of_list_using(<></>, <></>, Span.divider(sep), list)
      }</ul>

    /**
     * Create a pagination
     */
    pagination(pages:list(WBootstrap.Navigation.elt), prev:WBootstrap.Navigation.page_nav_elt, next:WBootstrap.Navigation.page_nav_elt) =
      list = @toplevel.List.map(nav_elt_to_xhtml, pages)
      is_disabled(l) = if @toplevel.List.is_empty(l) || (match @toplevel.List.head(l) {active=_ href=_ onclick=_} -> true _ -> false) then "disabled" else ""
      prev_disabled = is_disabled(pages)
      next_disabled = is_disabled(@toplevel.List.rev(pages))
      <div class="pagination">
        <ul>
          <li class="prev {prev_disabled}">
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

  }}

  Message = {{

    @private gen_make_alert(closable:bool, content:WBootstrap.Message.content, more:xhtml) =
      id = Dom.fresh_id()
      <div id=#{id} class="alert-message">
        {if not(closable) then <></>
         else <a class="close" onclick={_->Dom.remove(#{id})}>&times;</a>}
        <p>
          {if content.title == "" then <></> else <strong>{content.title}</strong>}
          {content.description}
        </p>
        {more}
      </div>

    make_alert(closable:bool, content:WBootstrap.Message.content) =
      gen_make_alert(closable, content, <></>)

    make_block(closable:bool, actions:option(xhtml), content:WBootstrap.Message.content) =
      more = match actions
             {some=a} -> <div class="alert-actions">{a}</div>
             {none} -> <></>
      gen_make_alert(closable, content, more) |> Xhtml.update_class("block-message", _)

    warning = Xhtml.update_class("warning", _)
    error = Xhtml.update_class("error", _)
    success = Xhtml.update_class("success", _)
    info = Xhtml.update_class("info", _)

    /**
     * Create a message (alert or block)
     */
    make(msg_type:WBootstrap.Message.type, msg_class:WBootstrap.Message.importance) =

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
