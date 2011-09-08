/*
    Copyright © 2011 MLstate

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
 * A Markdown implementation
 *
 * @author Nicolad Glondu, 2011
 * @category TOOL
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

package stdlib.tools.markdown

/**
 * {1 About this module}
 *
 * Simple Markdwon implementation
 * See http://daringfireball.net/projects/markdown/
 * to learn more about Markdown.
 *
 * What you can do:
 * - Create titles (both styles)
 * - Insert links (both styles)
 * - Insert images (both styles) 
 * - Make an simple or a strong emphasis (both ways: _ or *)
 * - Make simple emphasis inside a strong one
 * - Make strong emphasis inside a simple one
 * - Write lists (ul or ol) with elements on one or more lines
 * - Insert code (both ways: block or inside ``)
 * - Insert quotes containing anything supported (including other quotes)
 * - Insert linebreaks
 * - Insert linefeeds
 *
 * What you cannot do:
 * - Put any effect inside a title
 * - Put any effect inside a link
 * - Put any effect inside a emphasis
 * - Put lists inside of lists
 * - Put a title inside a lists
 * - Put a block of code inside a list
 *
 */

type Markdown.private.image =
    { alt    : string
      title  : option(string)
      uri    : string }
  / { alt    : string
      img_id : string }

type Markdown.private.ref =
    { uri   : string
      title : option(string) }

type Markdown.private.link =
    { text    : string
      title   : option(string)
      uri     : string }
  / { text    : string
      link_id : string }

type Markdown.private.sub_emph = {e_strong:string}/{e_text:string}
type Markdown.private.sub_strong = {s_emph:string}/{s_text:string}

type Markdown.private.content_part =
    { text   : string }
  / { emph   : list(Markdown.private.sub_emph) }
  / { strong : list(Markdown.private.sub_strong) }
  / { code   : string }
  / { link   : Markdown.private.link } // [example](http://url.com/ "Title")
  / { image  : Markdown.private.image } // ![alt text](/path/img.jpg "Title")
  / { br }

type Markdown.private.content = list(Markdown.private.content_part)

type Markdown.private.list_type = { bullet } / { number }

type Markdown.private.block =
    { header     : int /** Level (h1-h6) */
      content    : Markdown.private.content }
  / { paragraph  : Markdown.private.content }
  / { blockquote : list(Markdown.private.block) }
  / { list       : Markdown.private.list_type
      elements   : list(Markdown.private.content) }
  / { code       : string }
  / { hr }

type @abstract Markdown.t = {
  blocks : list(Markdown.private.block)
  refs   : stringmap(Markdown.private.ref)
}

type Markdown.options = {
  detect_text_links : bool
}

Markdown = {{

  /**
   * Export functions
   */

  /* A simple linebreak in output file */
  @private break =
    <>
    </>

  @private aux_link(refs:stringmap(Markdown.private.ref))(l:Markdown.private.link) =
    match l with
    | ~{text title={some=title} uri} ->
      <a href="{uri}" title="{title}">{text}</a>
    | ~{text title={none} uri} ->
      <a href="{uri}">{text}</a>
    | ~{text link_id} ->
      match StringMap.get(link_id, refs) with
      | {some=~{uri title={some=title}}} ->
        <a href="{uri}" title="{title}">{text}</a>
      | {some=~{uri title={none}}} ->
        <a href="{uri}">{text}</a>
      | {none} ->
        <>[{text}][{link_id}]</>

  @private aux_image(refs:stringmap(Markdown.private.ref))(i:Markdown.private.image) =
    match i with
    | ~{alt title={some=title} uri} ->
      <img alt="{alt}" src="{uri}" title="{title}"/>
    | ~{alt title={none} uri} ->
      <img alt="{alt}" src="{uri}"/>
    | ~{alt img_id} ->
      match StringMap.get(img_id, refs) with
      | {some=~{uri title={some=title}}} ->
        <img alt="{alt}" src="{uri}" title="{title}"/>
      | {some=~{uri title={none}}} ->
        <img alt="{alt}" src="{uri}"/>
      | {none} ->
        <>![{alt}][{img_id}]</>

  @private auto_link(src:string):xhtml =
    blanck = parser c=[ \t\n] -> c
    chr = parser c=(!blanck .) -> c
    create_link(text:text) =
      link = Text.to_string(text)
      <a target="_blank" href="{link}">{link}</a>
    link = parser
      | lnk=("http://" chr*)  -> create_link(lnk)
      | lnk=("https://" chr*) -> create_link(lnk)
      | lnk=("ftp://" chr*)   -> create_link(lnk) 
    text = parser t=((!link .)*) -> <>{t}</>
    link_text = parser l=link t=text -> <>{l}{t}</>
    main_parse = parser
      | t=text lt=link_text* -> <>{t}{lt}</>
      | lt=link_text*        -> <>{lt}</>
      | lt=link_text* l=link -> <>{lt}{l}</>
    Parser.parse(main_parse, src)

  @private aux_content(opt:Markdown.options, refs:stringmap(Markdown.private.ref))(c:Markdown.private.content) =
    e_sub(pp) =
      match pp:Markdown.private.sub_emph with
      | ~{e_text}   -> <>{e_text}</>
      | ~{e_strong} -> <strong>{e_strong}</strong>
    s_sub(pp) =
      match pp:Markdown.private.sub_strong with
      | ~{s_text} -> <>{s_text}</>
      | ~{s_emph} -> <em>{s_emph}</em>

    sub(p:Markdown.private.content_part) =
      match p with
      | ~{text}   -> if opt.detect_text_links then <>{auto_link(text)}</>
                     else <>{text}</>
      | ~{emph}   ->
        <em>{List.fold((elt, acc -> acc <+> e_sub(elt)), emph, <></>)}</em>
      | ~{strong} ->
        <strong>{List.fold((elt, acc -> acc <+> s_sub(elt)), strong, <></>)}</strong>
      | ~{code}   -> <code>{code}</code>
      | ~{link}   -> aux_link(refs)(link)
      | ~{image}  -> aux_image(refs)(image)
      | {br}      -> <br/>;
    <>{List.fold((elt,acc -> acc <+> sub(elt)), c, <></>)}</>

  @private rec aux_block(opt:Markdown.options, refs:stringmap(Markdown.private.ref))(b:Markdown.private.block) =
    aux_cont = aux_content(opt, refs)
    match b with
    | ~{header content} ->
      c = aux_cont(content)
      match header with
      | 1 -> <h1>{c}</h1>
      | 2 -> <h2>{c}</h2>
      | 3 -> <h3>{c}</h3>
      | 4 -> <h4>{c}</h4>
      | 5 -> <h5>{c}</h5>
      | 6 -> <h6>{c}</h6>
      | x -> if x<1 then <h1>{c}</h1> else <h6>{c}</h6>
      end
    | ~{paragraph} ->
      <p>{aux_cont(paragraph)}</p>
    | ~{blockquote} ->
      <blockquote>
        {List.fold(
          elt, acc ->
            acc <+> break <+> aux_block(opt, refs)(elt),
          blockquote,<></>)}
      </blockquote>
    | ~{list elements} ->
      elts = List.fold(
        el, acc ->
          acc <+> break <+> <li>{aux_cont(el)}</li>,
        elements, <></>)
      match list with
      | {bullet} -> <ul>{elts}</ul>
      | {number} -> <ol>{elts}</ol>
      end
    | ~{code} ->
      <pre><code>{code}</code></pre>
    | {hr} -> <hr/>

  @private rec check_title(pos,max,str,res:option(int)):option(int) =
    if pos < max then
      c = String.substring_unsafe(pos,1,str)
      if pos == 0 then
        if c == "=" then check_title(pos+1,max,str,some(1))
        else if c == "-" then check_title(pos+1,max,str,some(2))
        else none
      else match (c,res) with
      | ("=",{some=1}) -> check_title(pos+1,max,str,res)
      | ("-",{some=2}) -> check_title(pos+1,max,str,res)
      | _ -> none
    else res

  /**
   * Import functions
   */

  @private prepare_parts(parts) =
    add_acc(acc,res) =
      if acc == [] then res
      else List.add({tmp_block=List.rev(acc)},res)
    is_special_blank(line) =
      test = parser bq_white eol=eol -> eol
      Parser.try_parse(test, line) |> Option.is_some
    List.fold(
      line, (acc,pline,res) ->
        ll = String.strip_right(line)
        len = String.length(ll)
        match check_title(0,len,ll,none) with
        | {some=1} ->
          pline = String.strip_right(pline)
          if pline == "" then (acc,line,res)
          else
            res = add_acc(acc,res)
            res = List.add({tmp_header=(1,pline)}, res)
            ([],"",res)
        | {some=2} ->
          pline = String.strip_right(pline)
          if pline == "" && len < 3 then (acc,line,res)
          else
            res = add_acc(acc,res)
            res =
              if pline == "" then List.add({tmp_hr=ll}, res)
              else List.add({tmp_header=(2,pline)}, res)
          ([],"",res)
        | _ ->
          acc =
            if pline == "" then acc
            else List.add(pline,acc)
          if ll == "" && not(is_special_blank(line)) then
            ([],"",add_acc(acc,res))
          else (acc,line,res),
      parts, ([],"",[]))
    |> (acc, pline, res) ->
      acc = if pline == "" then acc else List.add(pline,acc)
      add_acc(acc,res)
    |> List.rev
    |> List.fold(
         elt, (prev,res) ->
           match prev with
           | {none} -> (some(elt), res)
           | {some=pr} ->
             match (pr,elt) with
             | ({tmp_hr=h},{tmp_hr=_}) ->
               (none, List.add({tmp_header=(2,h)}, res))
             | _ ->
               (some(elt), List.add(pr,res)),
         _, (none,[]))
    |> (prev,res) ->
      match prev with
      | {none} -> res
      | {some=pr} -> List.add(pr,res)
    |> List.rev

  @private tts = Text.to_string
  @private sp = parser c=[ \t] -> c
  @private not_sp = parser c=(!sp .) -> c
  @private not_cbracket = parser c=(!"]" .) -> c
  @private not_dquote = parser c=(![\"] .) -> c

  @private title = parser "\"" ttl=(not_dquote*) "\"" -> tts(ttl)

  @private bq_white = parser
    | "    " -> {}
    | "\t"   -> {}
  @private eol = parser endline=(.*) -> tts(endline)

  @private detect_specials(parts) =
    start = parser
      | "   " -> {}
      | "  "  -> {}
      | " "   -> {}
      | ""    -> {}
    header = parser
      | "######" -> 6
      | "#####"  -> 5
      | "####"   -> 4
      | "###"    -> 3
      | "##"     -> 2
      | "#"      -> 1
    special = parser
      | bq_white eol=eol -> {tmp_code=eol}
      | start ">" " "? eol=eol -> {tmp_blockquote=[eol]}
      | start lvl=header eol=eol -> {tmp_header=(lvl,eol)}
      | start "[" ref=(not_cbracket*) "]:" sp* lnk=(not_sp*) sp* ttl=title? sp* ->
          {tmp_ref=(tts(ref),tts(lnk),ttl)}
      | start [-*+] sp* eol=eol -> {tmp_list=({bullet},[[eol]])}
      | start [0-9]+ "." sp* eol=eol -> {tmp_list=({number},[[eol]])}
      | eol=eol -> {tmp_block=[eol]}
    add_cur((cur,acc)) =
      if cur == {tmp_block=[]} then acc
      else List.add(cur,acc)
    List.map(
      part ->
        match part with
        | {tmp_block=l} ->
          List.fold(
            line, (cur,acc) ->
              sp = Parser.parse(special, line)
              match (sp,cur) with
              | ({tmp_block=b},{tmp_block=bb}) ->
                  ({tmp_block=List.append(bb,b)}, acc)
              | ({tmp_code=c}, {tmp_code=cc}) ->
                  ({tmp_code="{cc}\n{c}"}, acc)
              | ({tmp_blockquote=l}, {tmp_blockquote=ll}) ->
                  ({tmp_blockquote=List.append(ll,l)}, acc)
              | ({tmp_list=(t,l)}, {tmp_list=(tt,ll)}) ->
                  if t == tt then ({tmp_list=(t, List.append(l,ll))}, acc)
                  else (sp, add_cur((cur,acc)))
              | ({tmp_code=c}, {tmp_list=(t,l)}) ->
                  if l == [] then (sp, add_cur((cur,acc)))
                  else
                    last = List.add(c, List.head(l))
                    ({tmp_list=(t, List.add(last,List.tail(l)))}, acc)
              | _ -> (sp, add_cur((cur,acc))),
            l, ({tmp_block=[]},[]) )
          |> add_cur |> List.rev
        | _ -> [part],
      parts) |> List.flatten

  @private process_parts(p) =
    prepare_parts(p)
    |> detect_specials

  @private generic_multiple(unit:Parser.general_parser('a), not_elt_fun:string->'a):Parser.general_parser(list('a)) =
    not_elt = parser res=((!unit .)*) -> not_elt_fun(tts(res))
    elts = parser e1=unit e2=not_elt -> [e1,e2]
    parser start=not_elt? body=elts* stop=unit? ->
      start = match start with {some=e} -> [[e]] | {none} -> []
      stop = match stop with {some=e} -> [[e]] | {none} -> []
      List.append(start, List.append(body, stop)) |> List.flatten

  @private allowed = parser c=[ \t\n.,] -> c

  @private star_end = parser c=(![ \t*] . "*" allowed) -> c
  @private not_star = parser c=(!star_end .) -> c
  @private not_dstar = parser c=(!(not_sp "**") .) -> c

  @private under_end = parser c=(![ \t_] . "_" allowed) -> c
  @private not_under = parser c=(!under_end .) -> c
  @private not_dunder = parser c=(!(not_sp "__") .) -> c

  @private strong(fun) = parser
    | "**" c=(not_sp) "**" -> fun(tts(c))
    | "**" c=(not_sp not_dstar* not_sp) "**" -> fun(tts(c))
    | "__" c=(not_sp) "__" -> fun(tts(c))
    | "__" c=(not_sp not_dunder* not_sp) "__" -> fun(tts(c))

  @private emph(fun) = parser
    | "*" c=(not_sp) "*" -> fun(tts(c))
    | "*" c=(not_sp not_star* not_sp) "*" -> fun(tts(c))
    | "_" c=(not_sp) "_" -> fun(tts(c))
    | "_" c=(not_sp not_under* not_sp) "_" -> fun(tts(c))

  @private addr(f1,f2) =
    not_sp_par = parser c=(![ \t)] .) -> c
    parser
    | "[" txt=(not_cbracket*) "]" sp* "(" sp* lnk=(not_sp_par*) sp* ttl=title? ")" ->
      f1(tts(txt), tts(lnk), ttl)
    | "[" txt=(not_cbracket*) "]" sp* "[" ref=(not_cbracket*) "]" ->
      f2(tts(txt), tts(ref))

  @private prepare_content(src:string) =
    not_bquot = parser
      | "\\`" -> "`"
      | c=(![`] .) -> tts(c)

    sub_strong:Parser.general_parser(list(Markdown.private.sub_strong)) =
      strong_elt = emph(t->{s_emph=t})
      generic_multiple(strong_elt, (t->{s_text=t}))

    sub_emph:Parser.general_parser(list(Markdown.private.sub_emph)) =
      emph_elt = strong(t->{e_strong=t})
      generic_multiple(emph_elt, (t->{e_text=t}))

    elt =
      strong_parse = strong(t -> {strong=Parser.parse(sub_strong,t)})
      emph_parse = emph(t -> {emph=Parser.parse(sub_emph,t)})
      img_parse = addr((alt,uri,title -> ~{alt uri title}),
                        (alt, img_id -> ~{alt img_id}))
      link_parse = addr((text,uri,title -> ~{text uri title}),
                        (text, link_id -> ~{text link_id}))
      parser
      | "  \n" -> {br}
      | "`" code=not_bquot* "`" -> {code=String.concat("",code)}
      | strong=strong_parse -> strong
      | emph=emph_parse -> emph
      | "!" img=img_parse -> {image=img}
      | lnk=link_parse -> {link=lnk}

    main = generic_multiple(elt, (t->{text=t}))
    Parser.parse(main,src):Markdown.private.content

  @private help_string = "
You can enter a message using a Markdown-like syntax.
Here is a list of all elements recognized.

## Paragraphe

A group of text isolated by two or more linebreaks will be put
in a HTML paragraph<p>.

## Titles

You can enter titles using various syntaxes :

    First level title
    =================

will create:

First level title
=================

and

    Second level title
    ------------------

will create:

Second level title
-----------------

You can also create titles by preceding som text with as many `#` as the
level of the title, so

    #### Title 4

will create:

#### Title 4

## Emphasis

You may want to put emphasis on some words in your texts.

You can do a *simple emphasis* by surrounding some text with
`*` or `_`: `*text*` will give in result *text*.

To put a **strong emphasis** on something, you can surround it with
`**` or `__`: `__text__` will give in result __text__.

You can put *a **strong emphasis** inside a simple one* or
__a _simple emphasis_ inside a strong one__.

## Lists

You can insert simple bullet lists simply by putting each element in
a line starting by a `-`, a `+` or a `*`. Mixing symbols will not create
different lists unless there is a blanck line in the middle:

    - one
    - two

    * three
    + four

will result in:

- one
- two

* three
+ four

If you want to put a linebreak in your source to avoid very long lines, you
can put four spaces or a tabulation at the beginning of your line to keep the
content as one list element:

    -   one
        two
    -   three
        four

will result in:

-   one
    two
-   three
    four

To insert numbered lists, you can follow the same rules and start your lines
with a number followed by a period. The numbers do not have to follow a logical
suit to be recognized:

    1.  one
        two
    3.  three
    42. four
    444. five

will result in:

1.  one
    two
3.  three
42. four
444. five


## Links

You can insert links inside your text in two ways:

    some text [Inline-style](https://mlstate.com \"MLstate\") more text
    or
    some text [Reference-style][mlstate] some random text
    [mlstate]: https://mlstate.com  \"MLstate\"

In both cases, the title `(\"MLstate\")` is optional. The result of above is:

some text [Inline-style](https://mlstate.com \"MLstate\") more text
or
some text [Reference-style][mlstate] some random text
[mlstate]: https://mlstate.com  \"MLstate\"

Note that references are case-sensitive and can be used for several links.

## Images

Image insertion uses the same syntax than inserting links preceded by a `!`:

    ![HTML5](http://www.w3.org/html/logo/badge/html5-badge-h-solo.png \"HTML5\")
    ![HTML5][html5]
    [html5]: http://www.w3.org/html/logo/badge/html5-badge-h-solo.png \"HTML5\"

both result in:

![HTML5](http://www.w3.org/html/logo/badge/html5-badge-h-solo.png \"HTML5\")
![HTML5][html5]
[html5]: http://www.w3.org/html/logo/badge/html5-badge-h-solo.png \"HTML5\"

You can use a same reference for a link and an image (if it links to one).

## Code

There are two ways of inserting code inside your text.

You can first use surround it with `\\``, so `\\`some code\\`` will
result in `some code`.

You can also indent a paragraph it with at least four spaces or a tabulation:

        rec fact(x) =
          if x < 1 then 1
          else x * fact(x-1)

will result in:

    rec fact(x) =
      if x < 1 then 1
      else x * fact(x-1)

## Quote

You can insert a quote in you texts by preceding it with `>`. You
can put a quote inside a quote:

    > > I don't eat filthy animals.
    > Shit just got real.

will result in:

> > I don't eat filthy animals.
> Shit just got real.

## Various

You can insert an html linebreack (`<br/>`) by putting two spaces at
the end of a line.
You can insert an horizontal line by writing a line containing only
the character `-` preceded by an empty line (else it would put a `<h2>` on
the content of that line).

    --------------
    Some text
    with a linebreak


will result in:

--------------
Some text
with a linebreak

"

  @private help_xhtml = xhtml_of_string(default_options, help_string)

  @private @client toggle_help(src_id, res_id)(_) =
    do Dom.toggle(#{src_id})
    do Dom.toggle(#{res_id})
    void

  /**
   * End user functions
   */

 /**
  * Default options for Markdown parser
  */
  default_options = {
    detect_text_links = false
  } : Markdown.options

 /**
  * Transforms a string into a Markdown element
  *
  * @param opt Markdown configuration
  * @param src Source string
  */
  @publish @server of_string(_opt:Markdown.options, src:string):Markdown.t =
    src = "{src} "
    parts = String.replace("\r", "", src)
      |> String.explode_with("\n", _, false)
      |> process_parts
    process_tmp_block(b):Markdown.private.content =
      String.concat("\n",b)
      |> prepare_content
    se = StringMap.empty
    rec endprocess(parts) : Markdown.t =
      (blocks, refs) = List.map(
        tmp_elt ->
          match tmp_elt with
          | {tmp_block=b} -> ([{paragraph=process_tmp_block(b)}], se)
          | {tmp_code=c} -> ([{code=c}], se)
          | {tmp_header=(n,t)} -> ([{header=n content=[{text=t}]}], se)
          | {tmp_hr=_} -> ([{hr}], se)
          | {tmp_ref=(r,l,t)} -> ([], StringMap.singleton(r, {uri=l title=t}))
          | {tmp_list=(list,elements)} ->
            elements =
              List.map(
                elts -> process_tmp_block(List.rev(elts)),
                List.rev(elements)
              )
            ([~{list elements}],se)
          | {tmp_blockquote=b} ->
            subparts = process_parts(b)
            ~{blocks refs} = endprocess(subparts)
            ([{blockquote=blocks}], refs),
        parts) |> List.unzip
      blocks = List.flatten(blocks)
      refs = List.fold(StringMap.union, refs, se)
      ~{blocks refs}
    endprocess(parts)

 /**
  * Transforms a Markdown element into xhtml
  *
  * @param opt Markdown configuration
  * @param src Markdown source element
  */
  @publish @server to_xhtml(opt:Markdown.options, src:Markdown.t) =
    List.fold(
      elt, acc ->
        acc <+> break <+> aux_block(opt, src.refs)(elt),
      src.blocks, <></>)

 /**
  * Directly transforms a Markdown source text into xhtml
  * It is in fact nothing more than a [of_string] followed by a
  * [to_xhtml].
  *
  * @param opt Markdown configuration
  * @param src Source string
  */
  @publish @server xhtml_of_string(opt:Markdown.options, src:string):xhtml =
    of_string(opt, src) |> to_xhtml(opt, _)

  // maybe use WNotification widget?
  // Nico G says : NO
  //   WNotification (implementation based on default one):
  //   - Open dialog: 16 requests
  //   - Close dialog: 42 request
  //   Manually: 0 request to open or close the dialog \o/

 /**
  * Main element of the Markdown help display
  * Note that this does not show anything (you can add a CSS entry
  * to hide markdown_help to be sure of this). Once placed in a page,
  * use a [help_button] to add a button that will allow the user to
  * see this help.
  *
  * @param id Identifier of the help element
  */
  help_div(id) =
    src_id = "{id}_source"
    res_id = "{id}_result"
    bg_style = css {
      position: fixed;
      z-index:2000;
      top:0; left:0;
      width:100%; height:100%;
      background: #888;
      opacity:0.7;
    }
    help_style = css {
      position: fixed;
      width: 560px;
      z-index: 2001;
      top: 50px;
      background: white;
      padding: 0;
      border-radius: 6px;
    }
    header_style = css {
      border-bottom:1px solid #eee;
      padding: 5px 20px;
      height:80px;
    }
    body_style = css {
      padding: 20px;
      height:450px;
      overflow: auto;
    }

    <div style="display:none;" id=#{id} class="markdown_help">
      <div class="markdown-bg" style={bg_style} onclick={_->Dom.hide(#{id})}></div>
      <div style={help_style} class="markdown">
        <div class="markdown-header" style={header_style}>
             <h3>Markdown Syntax Help</h3> 
             <p>This help is written using this syntax. 
             To toggle between source and result <a onclick={toggle_help(src_id, res_id)}>click here</a>. 
             <br/>To close this help click outside.</p>
        </div>
        <div class="markdown-body" style={body_style}> 
             <div id=#{res_id}>{help_xhtml}</div>
             <pre style="display:none;" id=#{src_id}>{help_string}</pre>
        </div>
      </div>
    </div>

  @private @client do_expand(id) =
    do Dom.show(#{id})
    bg_sel = "{id} > .markdown-bg"
    content_sel = "{id} > .markdown"
    body_sel = "{id} > .markdown-body"
    win_width = Dom.get_outer_width(#{bg_sel})
    win_height = Dom.get_outer_height(#{bg_sel})
    left = (win_width - 560)/2
    height = win_height - 100
    style = [ {height={px=height}}, {left={px=left}} ]
    do Dom.set_style(#{content_sel}, style)
    do Dom.set_style(#{body_sel}, [{height={px=(height-80)}}])
    void

 /**
  * Displays a button to show Markdown help
  * Note that there MUST be a [help_div] somewhere in the page for this
  * to actually show something.
  *
  * @param id Identifier of the help element
  * @param content Content of the link
  */
  help_button(id, content:xhtml) =
    <a class="markdown_help_button" onclick={_->do_expand(id)}>{content}</a>

}}
