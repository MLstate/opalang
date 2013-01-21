/*
    Copyright © 2011-2013 MLstate

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
 * The type of file descriptors
 */
type File.descriptor = external

/**
 * Unix.error
 */
type Unix.error = external

/**
  * A module for very basic file access
  *
  * Be aware that this package access local file
  * and could be inaccessible or not working with some cloud configuration
  */
File = {{

  /**
   * As [File.open] but the result is returned to the callback.
   */
  open_async(path, flags, mode, callback) =
    %%BslFileDesc.open%%(path, flags, mode, callback)

  /**
   * Open the file at [path] with [flags]. The [mode] is used to create the file
   * if [flags] indicates that the file should be created.
   * @param path The path to file.
   * @param flags the openning flags
   * @param mode An optional mode that be used to create the file.
   * @return A file descriptor.
   */
  open(path, flags, mode) =
    match @callcc(k ->
      open_async(path, flags, mode,
        (err, fd -> Continuation.return(k, (err, fd)))
      )
    ) with
    | (_, {some = fd}) -> fd
    | ({some = er}, _) -> @fail("{er}")
    | _ -> @fail("File.open")

  /**
   * As [File.close] but the result is returned to the callback.
   */
  close_async(fd, callback) =
    %%BslFileDesc.close%%(fd, callback)

  /**
   * Close a file descriptor
   * @param fd The file descriptor to close.
   * @return An error if it occurs.
   */
  close(fd) =
    @callcc(k -> close_async(fd, Continuation.return(k, _)))

  /**
   * As [File.d_read] but the result is returned to the callback.
   */
  d_read_async(fd, length, pos, callback) =
    %%BslFileDesc.read%%(fd, length, pos, callback)

  /**
   * Read [length] bytes along the file descriptor [fd] at position [pos] (or
   * the current position if it is not specified).
   * @param fd The file descriptor to read.
   * @param length The number of bytes to read.
   * @param pos An optional position on the file descriptor.
   * @return A binary which contains read bytes.
   */
  d_read(fd, length, pos) =
    match @callcc(k ->
      d_read_async(fd, length, pos,
        (err, b -> Continuation.return(k, (err, b)))
      )
    ) with
    | (_, {some = fd}) -> fd
    | ({some = er}, _) -> @fail("{er}")
    | _ -> @fail("File.read")

  /**
   * As [File.d_write] but the result is returned to the callback.
   */
  d_write_async(fd, binary, pos, callback) =
    %%BslFileDesc.write%%(fd, binary, pos, callback)

  /**
   * Read [length] bytes along the file descriptor [fd] at position [pos] (or
   * the current position if it is not specified).
   * @param fd The file descriptor to write.
   * @param binary The binary data to write.
   * @param pos An optional position on the file descriptor.
   * @return The number of written bytes.
   */
  d_write(fd, binary, pos) =
    match @callcc(k ->
      d_write_async(fd, binary, pos,
        (err, br -> Continuation.return(k, (err, br)))
      )
    ) with
    | (_, {some = fd}) -> fd
    | ({some = er}, _) -> @fail("{er}")
    | _ -> @fail("File.d_write")

  /**
   * As [File.d_write] but the result is returned to the callback.
   */
  unlink_async(path, callback) =
    %%BslFile.unlink%%(path, callback)

  /**
   * Unlink file at [path].
   * @param path The path to file to unlink.
   * @return An Unix error if occurs
   */
  unlink(path) =
    @callcc(k -> unlink_async(path, Continuation.return(k, _)))

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

  /**
   * Recursively fold on a directory, and apply a function
   * to each file in this directory (or one of its sub-directory)
   *
   * @param f(acc, filename, path) The function to apply
   * @param acc The accumultor to give to f
   * @param dir The directory to fold
   */
  fold_dir_rec(f, acc, dir) =
    rec aux(rpath, name, acc) =
      path = List.rev([name|rpath])
      pathstr = String.concat("/", path)
      if File.is_directory(pathstr) then
        v = File.readdir(pathstr)
        match v
        {success=array} ->
          LowLevelArray.fold((v, acc ->
            aux([name|rpath], v, acc)
          ), array, acc)
        {failure=_} -> acc
      else f(acc, name, pathstr)
    aux([], dir, acc)

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
