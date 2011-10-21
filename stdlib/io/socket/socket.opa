/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under
    the terms of the GNU Affero General Public License, version 3, as
    published by the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
    Public License for more details.

    You should have received a copy of the GNU Affero General Public
    License along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
import-plugin socket

type Socket.connection = external

/**
    {1 About this module}

    ``Low-level'' network module.

    This module helps you to do some low-level networking. You can
    connect to a server, and write and read raw data to/from it.

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
    connect: string, int -> Socket.connection = %%BslSocket.connect%%

    connect_with_err_cont: string, int, continuation(string) -> Socket.connection = %%BslSocket.connect_with_err_cont%%
    connect_with_err_cont2: string, int -> outcome(Socket.connection,string) = %%BslSocket.connect_with_err_cont2%%

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

    /** Close the connection. */
    close: Socket.connection -> void = %%BslSocket.close%%

    /**
     * Write data to a connection.
     *
     * @return The amount of bytes written.
     */
    write: Socket.connection, string -> int = %%BslSocket.write%%

    /**
     * Write partial data to a connection.
     *
     * @return The amount of bytes written.
     */
    write_len: Socket.connection, string, int -> int = %%BslSocket.write_len%%

    /**
     * Read data from a connection.
     *
     * @return The data read.
     */
    read: Socket.connection -> string = %%BslSocket.read%%
}}
