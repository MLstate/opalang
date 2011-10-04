module IBC = IrcBotCore

(**
 * Connects a newly created bot to a server.
 *
 * @param irc The parameters of the bot.
 * @param sched The scheduler to use during the connection.
 * @param server The server to which the bot will connect.
 * @param port The port on which to connect.
 *)
let insert_bot (irc: IBC.irc) sched server port =
    let t = {
        IBC.runtime = {
            IBC.rt_proto = {
                IBC.rt_backtrace = true;
                rt_block_size = 4096;
                rt_payload = ();
                rt_server_write_timeout = Time.seconds 60
            }
        };
        err_cont = None;
        extra_params = irc
    } in
    IBC.connect t sched server port

(**
 * Creates and runs a new bot.
 *
 * @param username The username of the bot.
 * @param realname The realname of the bot.
 * @param nickname The nickname of the bot.
 * @param password The password to the server.
 * @param server The server to which the bot will connect.
 * @param port The port on which to connect.
 *)
let create_bot username realname nickname password server channel port callback onreceive =
    let parameters = {
        IBC.username = username;
        realname = realname;
        nickname = nickname;
        password = password;
        server = server;
        channel = channel;
        callback = callback;
        onreceive = onreceive
    } in
    let sched = Scheduler.default in
    insert_bot parameters sched server port
