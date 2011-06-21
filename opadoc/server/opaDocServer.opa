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
 * Opadoc launcher
 *
 * @author Adrien Jonquet
 */

type cl_state =
{ cl_pages : stringmap(xhtml) }

type sv_state =
{ sv_pages : stringmap(xhtml) }

type cl_msg =
{ GetPage : string }

type sv_msg =
{ GetPage : string }

// Loading the serialized (file -> (api*comment)) map
apix = JsonFile.open_apix("opadoc.apix"):Join.final

@publish files = StringMap.To.key_list(apix)
get_api(f) = StringMap.get(f, apix) ? error("file {f} not in opadoc.apix")

gen_xhtml(fname) =
  hyperlink = OpaDocXhtml.default_link_map
  rec aux_group(lg, res) =
    match lg with
    | {support={~none} comment={some=c}} -> <> {res} {OpaDocXhtml.print_comment(hyperlink, c)} </>
    | {support={some={~pkg ~path ~code_elt ~fname ~pos}} ~comment} ->
       path_name = String.concat(".", path)
       path_html = OpaDocUtils.sanitize_path(path_name)
       (path, api) =
         match code_elt with
         | { value = { ~ty ; ... } } ->
           (
             (<> <code class="val_def">{path_name}</code> : </>),
             (<> <code class="type"> {OpaDocXhtml.opaty_to_xhtml(hyperlink, ty)} </code> </>)
           )
         | { ~type_def } ->
           (
             <></>,
             <>{OpaDocXhtml.type_def_to_xhtml(hyperlink, type_def)}</>
           )
         end
       com = match comment with
              | {~none} -> <></>
              | {some=c} -> <tr><td>{OpaDocXhtml.print_comment(hyperlink, c)}</td></tr>
              end
        <>
          {res}
          <table border="1" width="100%" cellpadding="3" cellspacing="0">
          <tr>
            <td><a name="{path_html}"/>{path}{api}</td>
          </tr>
          {com}
          </table><br />
        </>
    end
    jj = get_api(fname)
    Xhtml.precompile(List.foldr(aux_group, jj, <></>))

client_on_msg(state:cl_state, msg:cl_msg) =
  match msg with
  | { GetPage=f } ->
    match StringMap.get(f, state.cl_pages) with
    | {none} ->
      page = sv_ask_page(f)
      do Dom.transform([#cur_doc <- page])
      {set = {state with cl_pages = StringMap.add(f, page, state.cl_pages)}}
    | {~some} -> do Dom.transform([#cur_doc <- some])
      {unchanged}
    end
  | _ -> error("client_on_msg")
  end

server_on_msg(state:sv_state, msg:sv_msg) =
  match msg with
  | { GetPage=f } ->
    match StringMap.get(f, state.sv_pages) with
    | {none} ->
      page = gen_xhtml(f)
      { instruction = {set = {state with sv_pages = StringMap.add(f, page, state.sv_pages)}};
        return = page }
    | {~some} ->
      { instruction = {unchanged};
        return = some }
    end
  | _ -> error("server_on_msg")
  end

@publish sv_ask_page =
  cell = Cell.make({sv_pages=StringMap.empty}, server_on_msg)
  fname -> Cell.call(cell, {GetPage = fname})


menu(files) =
  menu_files() =
    xhtml_ =
      List.fold((f, acc ->
        <>{acc}
          <li><a href="#{f}">{f}</a></li>
        </>), files, <></>)
    Dom.transform([#sub_menu <- <ul>{xhtml_}</ul>])
  <>
  <ul>
  <li><a onclick={ _ -> menu_files() }>Files</a></li>
  <li>packages</li>
  <li>modules</li>
  </ul>
  </>

opadoc_onready(files) =
  empty = {cl_pages=StringMap.empty}
  client_session = Session.make(empty, client_on_msg)
  // associate links with page display fun action
  List.iter((f -> Client.register_anchor(f, ( -> _ = client_on_msg(empty, {GetPage=f}) void))), files)
   // load the pages in the cache in background
   // rec aux(l) =
   //   match l with
   //   | [f|tl] ->
   //     do Session.send(client_session, {GetPage=f})
   //     sleep(10000, -> aux(tl))
   //   | [] -> void
   // end
   // aux(files)

opadoc(name) =
  <>
    <div id="opadoc" onready={ _ -> opadoc_onready(files)}>
      <div id="header"><h1>{name}</h1></div>
      <div id="glob_menu">
        <div id="menu">{menu(files)}</div>
        <div id="sub_menu"></div>
      </div>
      <div id="cur_doc">Opadoc</div>
    </div>
  </>

main(name) = html("Opadoc in OPA", opadoc(name))

//TODO: Can't find the favicon!
//favicon = @static_source_content("./favicon.png")
//png(png) = Resource.image({png=png})

urls = parser
//      | "/favicon." (.*) -> png(favicon)
       | "/" -> main("Opadoc")

server = simple_server(urls)

css = css

body {
  padding : 0px;
  margin : 0px;
  background : #808080;
  color : #333333;
}
#header{
}
#opadoc{
  margin-left: auto;
  margin-right: auto;
}
#glob_menu{
  float:left;
  width : 200px;
}
#menu{
  background: #c0c0c0;
  float:left;
  width : 200px;
  height : 100px;
  overflow: auto;
}
#menu a {
  font-size: .9em;
  text-decoration: none;
}
#sub_menu{
  background: #c0c0c0;
  float:left;
  width : 200px;
  height: 800px;
  overflow: auto;
  margin-top: 2px
}
#sub_menu a {
  font-size: .9em;
  text-decoration: none;
}
#cur_doc{
  padding: 20px;
  background: white;
  margin-left: 202px;
  overflow: auto;
  height:900px
}
.val_def { font-weight : bold ; color : Red }
.type_def { font-weight : bold ; color : Green }
.type { color : #465F91 }
.module { font-size : 9pt; }
.value { font-size : 9pt; }
.comment { background-color : white;
           color: black;
           margin : 10px 10px 10px 10px }
h1 { font-size : 20pt;
     text-align: left;
     margin-top: 2px;
     margin-bottom: 2px;
     background-color: #90BDFF;
     padding: 2px; }
h2 { font-size : 15pt;
      margin-top: 5px;
      margin-bottom: 2px;
      text-align: left;
      background-color: #90BDFF;
      padding: 2px; }
h3 { font-size : 13pt;
     margin-top: 5px;
     margin-bottom: 2px;
     text-align: left;
     background-color: #90DDFF;
     padding: 2px; }
 h4 { font-size : 10pt;
      margin-top: 5px;
      margin-bottom: 2px;
      text-align: left;
      background-color: #90EDFF;
      padding: 2px; }
h5 { font-size : 10pt;
     margin-top: 5px;
     margin-bottom: 2px;
     text-align: left;
     background-color: #90FDFF;
     padding: 2px; }
h6 { font-size : 10pt ;
     margin-top: 5px;
     margin-bottom: 2px;
     text-align: left;
     background-color: #90BDFF;
     padding: 2px; }
