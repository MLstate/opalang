/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin unix

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
  *
  * Be aware that this package access local file
  * and could be inaccessible or not working with some cloud configuration
  */
File = {{

  /**
   * Read the content of a file
   *
   * @param path The path of the file
   * @return The binary content
   */
  read(path) = %% BslFile.content %%(path)
  @deprecated({use="read"})
  content = read

  /**
   * Read the content of a file
   *
   * @param path The path of the file
   * @return The optional binary content
   */
  read_opt(path) = %% BslFile.content_opt %%(path)
  @deprecated({use="read_opt"})
  content_opt = read_opt

  /**
   * Write a binary value to a file
   *
   * @param path The path of the file
   * @param content The binary content to put in the file
   */
  write(path, content) = %% BslFile.write %%(path, content)

  /**
   * Create a directory
   *
   * @param path The path of the directory
   * @return true if creation successful
   */
  mkdir(path) = %% BslFile.make_dir %%(path)

  /**
   * Check the existence of a file
   *
   * @param path The path of the file to check
   * @return true if the file exists
   */
  exists(path) = %% BslFile.exists %%(path)

  /**
   * Check if the given path is a directory
   *
   * @param path The path to test
   * @return true if the path is a directory
   */
  is_directory(path) = %% BslFile.is_directory %%(path)

  /** Warning: not working on node backend */
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
