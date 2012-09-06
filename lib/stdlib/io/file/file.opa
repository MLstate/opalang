/*
    Copyright Â© 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin unix
/**
 * {1 About this module}
 *
 * Be aware that this package access local file
 * and could be inaccessible or not working with some cloud configuration
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

/**
  * Type of file event.
  * For :
  *  - directory, 'rename' can be a sub-file/directory creation/deletion, 'change' is a sub-file change
  *  - file, 'rename' is file deletion and creation, 'change' is a file modification
  *  The string indicates which files has been modified, changed ...
  */
type File.raw_event = {rename:string} / {change:string}

/** Equivalent to File.raw_event */
type File.event = File.raw_event

/** A file watcher handler */
type File.watcher = external

/** Options for File.onchange function */
type File.onchange = {persistent:bool}

/**
  * A module for very basic file access
  */
File = {{
  exists = %% BslFile.exists %% : string -> bool
  content = %% BslFile.content %% : string -> binary
  content_opt = %% BslFile.content_opt %% : string -> option(binary)
  is_directory = %% BslFile.is_directory %% : string -> bool
  mimetype =
    #<Ifstatic:OPA_BACKEND_QMLJS>
    _ -> none
    #<Else>
    %% BslFile.mimetype_opt %% : string -> option(string)
    #<End>
  basename = %% BslFile.basename %% : string -> string
  dirname = %% BslFile.dirname %% : string -> string

  readdir(path):outcome(llarray(string),string) =
    (err,r) = Raw.readdir(path)
    if err==""
    then {success=r}
    else {failure=err}

  onchange_default = {persistent=false} : File.onchange

  /** if the path of the file is changed, the behaviour is undefined */
  onchange(path1,conf_opt:option(File.onchange))(f) =
    conf = conf_opt ? onchange_default
    g(event,path2) = f(path1,Raw.conv(event,path2))
    watcher = Raw.onchange(path1,conf.persistent,g)
    {stop() = Raw.watcher_stop(watcher)}

  @private
  Raw = {{
    conv(event,path2) = match event
      "rename" -> {rename=path2}
      "change" -> {change=path2}
      _ ->  do Log.warning("File.onchange","Unknown event {Debug.dump(event)}")
            conv("change",path2)
      end
    onchange = %% BslFile.onchange %% : string, bool, (string,string->void) -> File.watcher
    watcher_stop = %% BslFile.filewatcher_stop %% : File.watcher -> void
    readdir = %% BslFile.readdir %% : string -> (string,llarray(string))
  }}

}}
