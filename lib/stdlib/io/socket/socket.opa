/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin socket

type Socket.connection = external

/**
    {1 About this module}

    ``Low-level'' network module.

    This module helps you to do some low-level networking. You can
    connect to a server, and write and read raw data to/from it.

    The err_cont functions are exactly the same as the non-err_cont
    functions except you get an outcome of either the success result or
    a failure with the exception turned into a string.  Timeouts are
    ints as milliseconds.

    {b Warning:} This module is still experimental.
*/
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

    /**
     * Write partial data to a connection.
     *
     * @return The amount of bytes written.
     */
    write_len: Socket.connection, string, int -> int =
      %%BslSocket.write_len%%

    write_len_with_err_cont: Socket.connection, int, string, int -> outcome(int,string) =
      %%BslSocket.write_len_with_err_cont%%

    /**
     * Read data from a connection.
     *
     * @return The data read.
     */
    read: Socket.connection -> string =
      %%BslSocket.read%%

    read_with_err_cont: Socket.connection, int -> outcome(string,string) =
      %%BslSocket.read_with_err_cont%%

    conn_id: Socket.connection -> int = %%BslSocket.conn_id%%

}}
