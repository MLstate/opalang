/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin socket

type Socket.t = {mbox:Mailbox.t; conn:Socket.connection}

@abstract
type Socket.connection = external

type Mailbox.t = {
  buf : binary;
  start : int;
  len : int;
  min : int;
  hint : int;
}

type Socket.host = (string, int)

/**
    {1 About this module}

    ``Low-level'' network module.

    This module helps you to do some low-level networking. You can
    connect to a server, and write and read raw data to/from it.

    The err_cont functions are exactly the same as the non-err_cont
    functions except you get an outcome of either the success result or
    a failure with the exception turned into a string.  Timeouts are
    ints as milliseconds.

    The binary_ functions are intended to work directly with the
    binary type but note that a socket opened in binary_mode should
    only be read/written with the binary functions.  Reading or
    writing the other type will result in the data being copied.

    The Mailbox module is intended to allow reading of data in multiple
    calls.  See [read_fixed] for an example of its use.

    {b Warning:} This module is still experimental.
*/

@private bindump = (%% BslPervasives.bindump %%: binary -> string)

Mailbox = {{

  // Currently, buffers are allocated by create and returned by reset
  // at which time the mailbox record can be loosed into the GC.
  // There is a primitive attempt to reclaim long-term allocated
  // memory in the check function, but this involves copying.
  // TODO: get_buf, free_buf

  /**
   * Create a mailbox, ths initial size is given by the hint parameter.
   */
  create(hint:int) : Mailbox.t = {buf=Binary.create(hint); start=0; len=0; min=128; ~hint}

  /**
   * Reset the internal buffer.
   */
  reset(mb:Mailbox.t) : Mailbox.t =
    do Binary.reset(mb.buf)
    {mb with start=0; len=0}

  /**
   * Add a string to the end of the buffer.
   */
  add_string(mb:Mailbox.t, str:string) : Mailbox.t =
    do Binary.add_string(mb.buf, str)
    {mb with len=mb.len+String.length(str)}

  /**
   * Add binary data to the end of the buffer.
   */
  add_binary(mb:Mailbox.t, bin:binary) : Mailbox.t =
    do Binary.add_binary(mb.buf, bin)
    {mb with len=mb.len+Binary.length(bin)}

  /**
   * Return the contents of the mailbox as a string.
   */
  string_contents(mb:Mailbox.t) : string = Binary.get_string(mb.buf, mb.start, mb.len)

  /**
   * Return the contents of the mailbox as binary data.
   */
  binary_contents(mb:Mailbox.t) : binary = Binary.get_binary(mb.buf, mb.start, mb.len)

  /**
   * Check a mailbox for size, if start>0 and the live data
   * is less than min then the buffer is reset to contain just that data.
   */
  check(mb:Mailbox.t) : Mailbox.t =
    if mb.start > 0 && mb.len < mb.min
    then
      if mb.len > 0
      then
        data = binary_contents(mb)
        mb = reset(mb)
        add_binary(mb, data)
      else
        reset(mb)
    else mb

  /**
   * Return  the next [len] bytes from the mailbox, the mailbox
   * internal parameters are updated.
   * If there is not enough data in the mailbox then [failure] is returned.
   */
  sub(mb:Mailbox.t, len:int) : outcome((Mailbox.t,binary),string) =
    if len <= mb.len
    then
      data = Binary.get_binary(mb.buf, mb.start, len)
      {success=(check({mb with start=mb.start+len; len=mb.len-len}),data)}
    else
      {failure="Mailbox.sub: Not enough data for {len} bytes (currently {mb.len})"}

  /**
   * Skip the next [len] bytes in the mailbox.
   */
  skip(mb:Mailbox.t, len:int) : outcome(Mailbox.t,string) =
    if len <= mb.len
    then {success=(check({mb with start=mb.start+len; len=mb.len-len}))}
    else {failure="Mailbox.skip: Not enough data for {len} bytes (currently {mb.len})"}

  /**
   * Debug function, print the contents of the mailbox.
   */
  print(name:string, mb:Mailbox.t) : void = jlog("{name}: {bindump(binary_contents(mb))}")

}}

Socket = {{
    /**
     * Connect to a server without any encryption.
     *
     * {b Usage:} [connect(hostname, portnumber)] will connect to
     * [hostname] on [portnumber].
     *
     * @return A connection.
     */
    connect: string, int -> Socket.connection =
      %%BslSocket.connect%%

    connect_with_err_cont: string, int -> outcome(Socket.connection,string) =
      %%BslSocket.connect_with_err_cont%%

    binary_connect_with_err_cont: string, int -> outcome(Socket.connection,string) =
      %%BslSocket.binary_connect_with_err_cont%%

    /**
     * Connect to a server over an SSL encryption.
     *
     * {b Usage:} [secure_connect(hostname, portnumber, secure_type)]
     * will connect to [hostname] on [portnumber] using [secure_type].
     *
     * @return A connection.
     */
    secure_connect: string, int, SSL.secure_type -> Socket.connection =
      %%BslSocket.secure_connect%%

    secure_connect_with_err_cont: string, int, SSL.secure_type -> outcome(Socket.connection,string) =
      %%BslSocket.secure_connect_with_err_cont%%

    binary_secure_connect_with_err_cont: string, int, SSL.secure_type -> outcome(Socket.connection,string) =
      %%BslSocket.binary_secure_connect_with_err_cont%%

    /** Close the connection. */
    close: Socket.connection -> void = %%BslSocket.close%%

    /**
     * Write data to a connection.
     *
     * @return The amount of bytes written.
     */
    write: Socket.connection, string -> int =
      %%BslSocket.write%%

    write_with_err_cont: Socket.connection, int, string -> outcome(int,string) =
      %%BslSocket.write_with_err_cont%%

    binary_write_with_err_cont: Socket.connection, int, binary -> outcome(int,string) =
      %%BslSocket.binary_write_with_err_cont%%

    /**
     * Write partial data to a connection.
     *
     * @return The amount of bytes written.
     */
    write_len: Socket.connection, string, int -> int =
      %%BslSocket.write_len%%

    write_len_with_err_cont: Socket.connection, int, string, int -> outcome(int,string) =
      %%BslSocket.write_len_with_err_cont%%

    binary_write_len_with_err_cont: Socket.connection, int, binary, int -> outcome(int,string) =
      %%BslSocket.binary_write_len_with_err_cont%%

    /**
     * Read data from a connection.
     *
     * @return The data read.
     */
    read: Socket.connection -> string =
      %%BslSocket.read%%

    read_with_err_cont: Socket.connection, int -> outcome(string,string) =
      %%BslSocket.read_with_err_cont%%

    binary_read_with_err_cont: Socket.connection, int -> outcome(binary,string) =
      %%BslSocket.binary_read_with_err_cont%%

    conn_id: Socket.connection -> int = %%BslSocket.conn_id%%

    /**
     * Ensure there is a minimum number of bytes available in a mailbox.
     * If there is not enough data in the mailbox then addition
     * reads are performed on the socket until there is enough data.
     *
     * @return An outcome of an updated mailbox.
     */
    read_fixed(conn:Socket.connection, timeout:int, len:int, mb:Mailbox.t) : outcome(Mailbox.t,string) =
      if mb.len >= len
      then {success=mb}
      else
        match binary_read_with_err_cont(conn, timeout) with
        | {success=data} -> read_fixed(conn,timeout,len,Mailbox.add_binary(mb, data))
        | {~failure} -> {~failure}

}}
