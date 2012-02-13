/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

import stdlib.core.{color, funaction, map}

/**
 * @author Adam Koprowski
 *
 * {1 About this module}
 *
 * This module presents the source code of the application.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 Interface}
 */

AppSources =
{{

  @private
  apply_style(style, dom : xhtml) =
    match dom with
    | { specific_attributes=attrs_opt ~namespace ~tag ~args ~content } ->
        attrs = default(Xhtml.default_attributes, attrs_opt)
  // FIXME, why the following does not work?
  //        { dom with specific_attributes = some({ attrs with ~style}) }
        { specific_attributes = some({ attrs with ~style})
          ~namespace ~tag ~args ~content }
    | _ -> dom


  show_file(filename) =
    content = %%BslAppSrcCode.get_file_content%%(filename)
    Dom.transform([#content <- <>{content}</>])

  page() =
    app_files = %%BslAppSrcCode.get_file_list%%()
    init() =
      Dom.set_style(
        Dom.select_raw_unsafe("html, body"),
        css {height: 100%; margin: 0px}
      )
    files =
      new_file(filename) =
        <a onclick={_ -> show_file(filename)}>{filename}</>
        |> apply_style(css { display: block }, _)
      <span id=#files onready={_ -> init()}>
        {List.map(new_file, app_files)}
      </>
      |> apply_style(css {
           width: 20%;
           height: 95%;
           overflow: auto;
           border: 1px solid black;
           display: inline-block;
         }, _)
    file_content =
       // FIXME move vertical-align to the css below
      <span id=#content style="vertical-align: top;" />
      |> apply_style(
        css {
          border: 1px solid black;
          font-family: Monospace;
          padding: 5px;
          width: 75%;
          height: 95%;
          overflow: auto;
          display: inline-block;
          white-space: pre;
        }, _)
    xhtml =
      <>
        {files}
        {file_content}
      </>
    Resource.page("Application sources", xhtml)

}}
