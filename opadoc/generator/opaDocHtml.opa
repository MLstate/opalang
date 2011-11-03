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
 * opadoc HTML generator
 **/

OpaDocHtml = {{

  @private
  write = %%BslFile.of_string%%

  /**
   * Static css resource
   **/
  @private
  css_style = @static_content("opadoc.css")

  /**
   * Generated css file
   **/
  gen_css(path) =
    write("{path}/style.css", css_style())

  /**
   * Static javascript resource
   **/
  @private
  ext_libs = [
    @static_content("js/jquery.min.js"),
    @static_content("js/jquery.jstree.min.js"),
    @static_content("js/jquery.cookie.min.js"),
    @static_content("js/jquery.scrollTo-1.4.2-min.js"),
    @static_content("js/jquery.hotkeys.min.js"),
    @static_content("js/jquery.history.min.js"),
  ]

  resources = @static_content_directory("resources/")

  js_str_of_xhtml(html: xhtml): string =
    Xhtml.to_string(html)
      |> String.replace("\n", "'\n+ '", _)

  js_fun_of_string(fname: string, str: string): string =
"function {fname}() \{
  return '{str}';
}
"

  js_fun_of_json(fname: string, str: string): string =
"function {fname}() \{
  return {str};
}
"

  js_of_xhtml(fname: string, html: xhtml): string =
    js_str_of_xhtml(html)
      |> js_fun_of_string(fname, _)

  js_of_json(fname: string, json: RPC.Json.json): string =
    Json.serialize(json)
      |> js_fun_of_json(fname, _)

  /**
   * Generated javascript file
   **/
  gen_javascript(path, morejs: string): void =
    do write("{path}/opadoc.js", @static_content("opadoc.js")())
    do write("{path}/trees.js", morejs)
    List.fold((js, acc -> acc ^ js()), ext_libs, "")
      |> write("{path}/ext_libs.js", _)

  gen_resources(path): void =
    StringMap.iter((name, content ->
        write("{path}/{name}", content())), resources)

  /**
   * The common header for all pages
   **/
  @private
  header_index =
    // TODO: simplify OpaDocTree interface
    // DO NOT SEPARATE </li><li> (end+begin), because put space in HTML otherwise !
    <div class="sidebar">
      <ul class="menu">
        <li class="packages_tab">
          <a href="javascript:void(0)" events_unsafe={[{name={click} value={value="switchTab('packages')"}}]}>Packages</a>
        </li><li class="values_tab">
          <a href="javascript:void(0)" events_unsafe={[{name={click} value={value="switchTab('values')"}}]}>Values</a>
        </li><li class="types_tab">
          <a href="javascript:void(0)" events_unsafe={[{name={click} value={value="switchTab('types')"}}]}>Types</a>
        </li><li class="files_tab">
          <a href="javascript:void(0)" events_unsafe={[{name={click} value={value="switchTab('files')"}}]}>Files</a>
        </li>
      </ul>
      <div id="navigator"></div>
    </div>

  /**
   * Take a header and body code, and build the page, adding the common body
   **/
  @private
  xhtml_encaps(header, code, sidebar) =
    tree = OpaDocTree.make(
          {OpaDocTree.default_config with animation = 0},
          "navigator", OpaDocTree.default_plugins, "packages"
        )
    js_init = "javascript:init()"
    <html>
      {header}
      <body events_unsafe={[{name={load} value={value="{if sidebar then tree else js_init}"}}]}>
        {if sidebar then
          <>
          {header_index}
          <div id="toggler"></div>
          </>
        else <></>}
        { if sidebar then <div id="container"><iframe events_unsafe={[{name={load} value={value="javascript:initMessage()"}}]} src="welcome.html" id="doc"></iframe></div>
          else
            <div class="main">
              <div class="content">
                {code}
              </div>
            </div> }
      </body>
    </html>

  /**
   * Build a header from the title of the page, adding link to css and javascript files
   **/
  @private
  xhtml_header(title) =
    <head>
      <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
      <link rel="stylesheet" href="style.css" type="text/css" />
      <script src="opadoc.js" type="text/javascript"/>
      <script src="trees.js" type="text/javascript"/>
      <script src="ext_libs.js" type="text/javascript"/>
      <title>{title}</title>
    </head>

  /**
   * The hyperlink is a map for adding link from utilisation of types to their definition.
   * FIXME: move somewhere else (other module, not specific to html production, shared with dynamic version)
   **/
  @private build_hyperlink(mj: Join.final) : Join.Html.link.map =
    fold_entry(page_name, entry : Api.entry, map : Join.Html.link.map) =
      // Do not use entry.fname, this is not the filename used for page generation
      fname = page_name
      path_name = String.concat(".", entry.path)
      path_html = OpaDocUtils.sanitize_path("{OpaDocXhtml.string_of_code_elt(entry.code_elt)}_{entry.pkg}.{path_name}")
      link_elt = ~{ fname path_html }
      map = StringMap.add(path_name, link_elt, map)
      map
    fold_group(page_name)(group : Join.group, map) =
      match group.support with
      | { none } -> map
      | { some = entry } -> fold_entry(page_name, entry, map)
    fold_joined(page_name, joined, map) = List.fold(fold_group(page_name), joined, map)
    fold_final(final, map) =
      StringMap.fold(fold_joined, final, map)
    map = OpaDocXhtml.default_link_map
    map = fold_final(mj, map)
    map

  /**
   * Generate all files
   **/
  gen_xhtml_final(output_path, mj:Join.final) =

    legacy_map =
      StringMap.fold(
        f, _e, acc ->
          match List.rev(String.explode(".", f))
          [sfx, file | tl] ->
            basefile = "{file}.{sfx}"
            //do jlog(basefile)
            tl = List.rev(tl)
            match StringMap.get(basefile, acc)
            {some=l} -> StringMap.add(basefile, [(f, tl)|l], acc)
            _ -> StringMap.add(basefile, [(f, tl)], acc)
            end
          _ -> acc
      , mj, StringMap.empty)

    hyperlink = build_hyperlink(mj)

       aux_group(res, lg : Join.group) =
         match lg with
         | {support={none} comment={some=c}} -> <> {res} {OpaDocXhtml.print_comment(hyperlink, c)} </>

         // FIXME: when real pat as are available replace by :
         /*
         | {support={some= { ~path ~code_elt ; ... } as entry } ~comment} ->
         */

         | {support={some= { ~path ~code_elt ; ~pkg ; fname=_ ; pos=_ } as entry } ~comment} ->
         // End of FIXME
           if OpaDocUtils.is_private(entry)
           then
             res
           else

           // if the code_elt is private, nothing should be displayed at all
           // unless we would be in private mode of opadoc (parameters)

           path_name = String.concat(".", path)
           path_str = OpaDocUtils.sanitize_path("{pkg}.{path_name}")
           (path, api, path_str) =
             match code_elt with
             | { value = { ~ty ; ... } } ->
               (
                 (<> <code class="val_def">{path_name}</code> : </>),
                 (<> <code class="type"> {OpaDocXhtml.opaty_to_xhtml_in_path(hyperlink, path_name, ty)} </code> </>),
                 "value_{path_str}"
               )
             | { ~type_def } ->
               (
                 (<></>),
                 (<>{OpaDocXhtml.type_def_to_xhtml(hyperlink, type_def)}</>),
                 "type_{path_str}"
               )
             end
           com = match comment with
                 | {none} -> <></>
                 | {some=c} -> <div class="description">{OpaDocXhtml.print_comment(hyperlink, c)}</div>
                 end
           <>
             {res}
             <div class="wrapper">
             <div class="declaration">
               <a name="{path_str}"/>{path}{api}
             </div>
             {com}
             </div>
           </>

         | { support = {none} ; comment = {none} } -> @fail
         end

      aux_final(fname, l_jj:Join.joined) =
        do jlog("generating doc for : {fname}")
        xhtml = xhtml_encaps(
                  xhtml_header(fname),
                  (List.fold_left(aux_group, <></>, (List.rev(l_jj)))),
                  false
                )
        x = Xhtml.to_string(xhtml)
        do write("{output_path}/{fname}.html", x)

        // legacy html file
        match List.rev(String.explode(".", fname))
        [sfx, file | _tl] ->
          o_file = "{output_path}/{file}.{sfx}.html"
          match StringMap.get("{file}.{sfx}", legacy_map)
          {some=l} ->
            content =
              match l
              | [(e, f)] ->
                // only one file, no ambiguity, automatic redirection
                f = String.concat(".", f)
                <p>You will be automatically redirected to <a href="{e}.html#value_{f}">{e}</a>...</p>
                <script>{Xhtml.of_string_unsafe(
"window.addEventListener('message', function(e) \{
  setTimeout('global_source.postMessage(\"{e}.html\"+window.location.hash, \"*\");', 100);
\}, false);")}</script>
              | _ ->
                // many files, let the user choose
                // FIXME: we lose the anchor the user wanted
                <h2>Sorry this page does not exist anymore. Do you mean one of the following pages ?</h2>
                <ul>{
                  List.map((e, f) ->
                    f = String.concat(".", f)
                    <li><a href="{e}.html#value_{f}">{e}</a></li>
                  , l)
                }</ul>
            xhtml = xhtml_encaps(
                      xhtml_header(fname),
                      content,
                      false
                    )
            write(o_file, Xhtml.to_string(xhtml))
          {none} -> void
          end
        _ -> void
        end

      StringMap.iter(aux_final, mj)

  /* Generate the home page (index.html) */
  gen_index(output_path) =
    welcome =
      <p>Welcome to OPA documentation home page.</p>
      <p>You can browse the documentation using the sidebar on the left.</p>
      <table>
        <caption>Legend</caption>
        <thead>
          <tr>
            <th>Icon</th>
            <th>Element kind</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td><img src="resources/package_icon.png" alt="Package icon" /></td>
            <td>Package</td>
          </tr>
          <tr>
            <td><img src="resources/module_icon.png" alt="Module icon" /></td>
            <td>Module</td>
          </tr>
          <tr>
            <td><img src="resources/type_icon.png" alt="Type icon" /></td>
            <td>Type</td>
          </tr>
          <tr>
            <td><img src="resources/value_icon.png" alt="Value icon" /></td>
            <td>Value</td>
          </tr>
          <tr>
            <td><img src="resources/file_icon.png" alt="File icon" /></td>
            <td>File</td>
          </tr>
        </tbody>
      </table>
    do write("{output_path}/welcome.html", Xhtml.to_string(welcome))
    xhtml = xhtml_encaps(xhtml_header("OPA Documentation"), <></>, true)
    write("{output_path}/index.html", Xhtml.to_string(xhtml))

}}
