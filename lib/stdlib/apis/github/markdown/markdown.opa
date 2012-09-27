/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

package stdlib.apis.github.markdown
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub user API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

type GitHub.markdown_mode = {markdown} / {gfm}

@private GHMp = {{

  string_of_markdown_mode(mm) =
    match mm with
    | {markdown} -> "markdown"
    | {gfm} -> "gfm"

}}

GHMarkdown = {{

  @private GP = GHParse

  markdown(token:string, text:string, mode:option(GitHub.markdown_mode), context:option(string)) =
    json = GHLib.mkopts([{sreq=("text",text)},{ocst=("mode",GHMp.string_of_markdown_mode,mode)},{sopt=("context",context)}])
    GHLib.api_post_string("/markdown", token, json, GP.xhtml)

  // TODO: markdown_raw

}}
