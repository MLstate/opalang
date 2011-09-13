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

/* LowLevel modules exported on the DOM */
var LowLevelSession = {};
var LowLevelRPC = {};
var LowLevelPingLoop = {};

/*
 * This block hide some functions that we want keep privates and
 * allows to no poluate the DOM. At the end of this block we add field
 * of above modules.
 */
{
    /* ************************************************** */
    /* Variables definitions **************************** */
    /**
     * The table of channels which have been sent to the server and
     * could be received back.
     *
     * A mapping from local channel id (as generated during
     * the first serialization of [LocalChannel]) to [LocalChannel]
     */
    var client_stored = new Object();

    /**
     * Thrown when a session was killed.
     *
     * @constructor
     */
    function Killed(){}

    /**
     * Channel to register on next request with [internal_ajax]
     */
    var to_register = new Array();

    /**
     * Channel to export
     */
    var to_export = new Array();

    function random() {
        return Math.floor(Math.random() * 1073741824);
    }
    /**
     * Set the page number of this client ([page_server] is inserted by
     * the server on S3)
     */
    var page_index = -1;
    if (typeof(page_server) != 'undefined') {
        page_index = page_server;
    } else {
        if(!command_line_execution) jlog("Warning: the server hasn't set the page number");
        page_index = random();
    }

    var base_url_client = "";
    if (typeof(base_url) != 'undefined') {
        base_url_client = base_url;
    } else {
        base_url_client = "";
    }

    /**
     * All internal request needs a page number
     */
    var internal_url = base_url_client+"/_internal_/"+page_index;

    var async_rpc_return = true;
    /* If client is dumb, we must force rpc to be synchronous */
    if (!command_line_execution && 'safari' in jQuery.browser){
        async_rpc_return = !jQuery.browser.safari;
    }

    var serialize_uu = function(_){error("Unregistered serialization function")}

    var unserialize_uu = function(_){error("Unregistered unserialization function")}

    /* DEBUG FUNCTIONS*/
    #<Ifstatic:PING_DEBUG>
    function ping_debug (msg) {
        syslog("notice", "PING", msg);
    }

    function sess_debug(msg){
        syslog("notice", "SESSION", msg);
    }
    #<End>

    /* ************************************************** */
    /* Utilities functions ****************************** */

    function generate_cl_id() {
        var rand = random();
        var c = getStableCookie();
        if ( 'none' in c )
            throw new Error("no cookie no session");
        else {
            return ("" + rand + c.some);
        }
    }

    function generate_lchan_id() {
        return random();
    }

    function internal_ajax(settings){
        settings.url = internal_url + settings.url;
        if(to_register.length != 0){
            var body = settings.data;
            if (body == undefined) body="";
            var data = {
                to_register:to_register,
                uri:settings.url,
                body:body
            };
            settings.data = JSON.stringify(data);
            settings.url = internal_url + "/chan/register";
            to_register = new Array();
        }
        return jQuery.ajax(settings);
    }



    /* ************************************************** */
    /* Local Channel ************************************ */

    /**
     * A channel which may be used to send messages locally
     *
     * @constructor
     */
    function LocalChannel(st, unserialize, fun_session, ctx, dfun, more,
                          cps_mode, concurrent) {
        this.lchan_id = generate_lchan_id();
        this.state = st;
        this.action = fun_session;
        this.unserialize = unserialize;
        this.messages = new Array() ;
        this.on_delete = dfun;
        this.more = more;
        this.ctx = ctx;
        this.concurrent = concurrent;
        this.serialized = null;
        this.killed = false;
        this.is_client = true;
    }

    LocalChannel.prototype = {

        /* Sending functions **************************** */

        #<Ifstatic:OPA_CPS_CLIENT>
        #<Else>
        /**
         * An auxiliar function for send a message to a session.
         *
         * @param herror is optionnal, it's a function that called if
         * session is Killed.
         * @param hsuccess is optionnal, it's a function that called if
         * message is treated.
         *
         * Note : USE UNIQUELY ON NO CPS MODE (like its name indicates)
         */
        send_no_cps_aux: function(msg, context, herror, hsuccess){
            // Session has been stopped
            if(this.state == null) {
                #<Ifstatic:PING_DEBUG>
                sess_debug("Call a killed session : "+this.lchan_id);
                #<End>
                if (herror != undefined) herror();
                #<Ifstatic:PING_DEBUG>
                    sess_debug("[LocalChannel.send] Killed :"+er);
                #<End>
                return;
            }
            // Get the good context (owner if setted, sender else)
            var ctx;
            if ('some' in this.ctx) ctx = this.ctx;
            else ctx = context;
            // Perform action
            #<Ifstatic:PING_DEBUG> ping_debug("Start handler"); #<End>
            if (hsuccess !=  undefined) hsuccess();
            var new_st = null ;
            try {
                new_st = this.action(this.state, msg, ctx);
                #<Ifstatic:PING_DEBUG> ping_debug("End handler"); #<End>
                if ('none' in new_st){
                    // Stop session
                    this.state = null;
                    this.kill();
                    return;
                } else {
                    // Update state
                    this.state = new_st.some;
                    return;
                }
            } catch (er) {
                %%bslsyslog_error%%("[LocalChannel.send] Catch :", er);
                return;
            }
        },
        #<End>

        /**
         * Send a message [msg] along this channel.
         *
         * @param {*} serialize Ignored
         * @param {!Object} msg The message, in a status fit for delivery
         * (e.g. not serialized)
         * @param {*} ctx The Thread context in which to send the message
         * @param {Function=} herror a function called if
         * session is Killed.
         * @param {Function=} hsuccess a function called if
         * message is treated properly.
         */
        send: function(serialize, msg, ctx, herror, hsuccess){
        #<Ifstatic:OPA_CPS_CLIENT>
        this.messages.unshift({msg: msg, ctx: ctx, herror : herror, hsuccess : hsuccess});
            //If [state] is [null], then we're currently handling a message
            // [msg] will be handled later
            if (this.state == null) return;
            else {
                var st = this.state;
                var lchan = this;
                // lock the state
                this.state = null;
                //Recursive wait loop
                function aux(stt) {
                    var cpl = lchan.messages.pop();
                    if (cpl == null) {
                        lchan.state = stt;
                    } else {
                        var ctx;
                        if ('some' in lchan.ctx){
                            ctx = lchan.ctx;
                        } else {
                            ctx = cpl.ctx;
                        }
                        if (hsuccess !=  undefined) hsuccess();
                        var new_st = lchan.action(stt, cpl.msg, ctx);
                        if ('none' in new_st) {
                            //If the action returns [none], we should kill the session
                            lchan.state = null;
                            this.kill();
                        } else {
                            setTimeout(function() {aux(new_st.some)}, 0);
                        }
                    }
                }
                setTimeout(function() {aux(st);}, 0);
            }
        #<Else>
            var lchan = this;
            function aux(){
                lchan.send_no_cps_aux(msg, ctx, herror, hsuccess);
            }
            if(this.concurrent){
                aux();
            } else {
                setTimeout(aux, 0);
            }
        #<End>
        },

        #<Ifstatic:OPA_CPS_CLIENT>
        #<Else>
        /**
         * Call a local cell represented by [lchan] with the message
         * [msg]. Same remarks that [llsend_lchan_no_cps].
         * This implementation of [Cell.call] is not very fair. Indeed if a
         * message waiting for the session, this call will be execute before
         * the waiting message. We can have also an unlikely starvation with
         * this implementation. But that respect the semantic of session.
         */
        call_no_cps: function(msg, _1, _2, onmsg){
            /* That is a very dirty hack, stdlib
             * implementation dependent (see also cell.opa) */
            var res = null;
            try {
                var f = function(state, msg){
                    /* Make cell message, with a continuation which set [res] */
                    var cmsg = empty_constructor()
                    cmsg = add_field(cmsg,static_field_of_name('f1'),new Continuation(function(r){res = r}))
                    cmsg = add_field(cmsg,static_field_of_name('f2'),msg)
                    cmsg = make_record(cmsg)
                    /* [onmsg] returns the session instruction */
                    return onmsg(state, cmsg);
                }
                this.action = f;
                this.send_no_cps_aux(msg, js_none);
            } catch (er) {
                %%bslsyslog_error%%("[LocalChannel.call] Cell :", er);
            }
            if(res === null) error("Call failed, result was [null]");
            return res;
        },
        #<End>



        /* Utils **************************************** */
        /**
         * Return the serialized form of the channel.
         */
        serialize: function(){
            var serialized = this.serialized;
            if(serialized == null) {
                serialized = {cl_id : generate_cl_id()};
                client_stored[serialized.cl_id] = this;
                to_register.push(serialized);
                this.serialized = serialized;
            }
            return serialized;
        },

        /**
         * Kill a local channel
         *
         * Trigger any [on_delete] callbacks
         */
        kill: function(){
            this.killed = true;
            var on_delete = this.on_delete;
            if ('some' in on_delete){
                on_delete.some();
            }
            while(this.messages.length > 0){
                var herror = this.messages.pop().herror;
                if (herror != undefined) herror();
            }
            var serialized = this.serialized;
            if(serialized != null){
                delete client_stored[serialized.cl_id];
                internal_ajax({ type : 'POST',
                            url : "/chan/remove",
                            data : JSON.stringify(serialized)});
            }
        },

        /**
         * Compare against another channel
         *
         * @return -1, 0, 1 or null if the channels are not comparable
         */
        compare: function(to) {
            if (to instanceof LocalChannel)
                return compare_native(this.lchan_id, to.lchan_id);
            else
                return 1;
        },

        owner: function(){
            return null;
        }

    }

    /* ************************************************** */
    /* Distant Channel ********************************** */
    /**
     * A common ancestor for [ServerChannel] and [CousinChannel]
     *
     * @constructor
     */
    function DistantChannel()
    {
    }
    DistantChannel.prototype = {
        message_to_post: function(message){
            var ser = this.serialize();
            return {to:ser,
                    message:message};
        },

        /**
         * Send a message along this channel
         *
         * @param {!Function} serialize A function (generally provided by OPA)
         * used to serialize the message
         * @param {!Object} msg The message, unserialized
         * @param {*} ctx Ignored
         * @param {Function=} herror
         * @param {Function=} hsuccess
         */
        send: function(serialize, msg, ctx, herror, hsuccess){
            var msg_to_post = this.message_to_post(serialize(msg));
            if (herror != undefined) msg_to_post.herror = serialize_uu(herror);
            if (hsuccess != undefined) msg_to_post.hsuccess = serialize_uu(hsuccess);
            var serialized_msg = JSON.stringify(msg_to_post);
            internal_ajax({ type : 'POST',
                          url : "/chan/send",
                          data : serialized_msg
                        });
        },

        #<Ifstatic:OPA_CPS_CLIENT>
        #<Else>
        /**
         * Call a distant cell
         *
         * @param message The message, unserialized
         * @param serialize A function for serialize the message
         * @param unserialize A function for unserialized the result
         */
        call_no_cps: function(message, serialize, unserialize, _){
            var str = the_pang_loop(internal_url + "/cell/CallThatPlease",
                                    this.message_to_post(serialize(message)),
                                    true);
            var json = %%BslJson.Json.string_to_native%%(str);
            return unserialize(json);
        }
        #<End>
    }


    /* ************************************************** */
    /* Server Channel *********************************** */
    /**
     * A channel which may be used to send a message to the server
     *
     * @constructor
     *
     * <!> The name of this constructor is used by opa, and inserted
     * as a constant string by opa2js.opa
     */
    function ServerChannel(id) {
        this.srv_id = id;
        this.is_client = false;
    }
    ServerChannel.prototype = {

        /**
         * Return the serialized form of the channel.
         *
         * @return {!Object}
         */
        serialize: function(){
            var r = {srv_id:this.srv_id};
            if (this.addr != undefined){
                r.addr = this.addr;
                r.port = this.port;
            }
            return r;
        },

        /**
         * Compare against another channel
         *
         * @return -1, 0, 1 or null if the channels are not comparable
         */
        compare: function(to){
            if(to instanceof ServerChannel)
                return compare_native(this.srv_id, to.srv_id);
            else return -1;
        },

        owner: function(){
            return this;
        },

       /**
         * Send a message along this channel
         *
         * @param serialize A function (generally provided by OPA)
         * used to serialize the message to a json
         * @param msg The message, unserialized
         * @param ctx Ignored
         */
        send: DistantChannel.prototype.send,

        call_no_cps: DistantChannel.prototype.call_no_cps,

        message_to_post: DistantChannel.prototype.message_to_post

    }


    /* ************************************************** */
    /* Client Channel *********************************** */
    /**
     * A channel which may be used to send a message to another client
     *
     * @constructor
     */
    function CousinClientChannel(id) {
        this.cl_id = id;
        this.is_client = true;
    }
    CousinClientChannel.prototype = {

        /**
         * Return the serialized form of the channel.
         *
         * @return {!Object}
         */
        serialize: function(){
            var r = {cl_id:this.cl_id};
            if (this.addr != undefined){
                r.addr = this.addr;
                r.port = this.port;
            }
            return r;
        },


        compare: function(to) {
            if(to instanceof CousinClientChannel)
                return compare_native(this.cl_id == to.cl_id);
            else
                if(to instanceof LocalChannel)
                    return -1;
                else // ServerChannel
                    return 1;
        },

        owner: function(){
            return this;
        },

        /**
         * Send a message along this channel
         *
         * @param serialize A function (generally provided by OPA)
         * used to serialize the message to json
         * @param The message, unserialized
         * @param ctx Ignored
         */
        send: DistantChannel.prototype.send,

        call_no_cps: function(){
            throw Error("Not yet implemented");
        },

        message_to_post: DistantChannel.prototype.message_to_post

    }


    /* ************************************************** */
    /* RPC support (server -> client) ******************* */
    /** Contains skeletons. */
    var RPC_comet_table = new Object();

    /* the id is null when the call is async */
    function RPC_call(id, name, argument) {
        var funct = RPC_comet_table[name];
        if ( funct == null ) throw new Error("Rpc client "+name+" doesn't exists");
        // hook for try catch there, and return a special case, maybe e.g. rpc_return_exc
        var data = funct(argument);
        if ('none' in data) {
            if(window.console && window.console.error) window.console.error("RPC comet call ", id, " failed, no data in ", argument);
        } else if (id != null) {
            internal_ajax({
                    type : 'POST',
                        url : "/rpc_return/"+id,
                        data : data.some,
                        async : async_rpc_return
                });
        }
    }

    /* ************************************************** */
    /* Ping loop **************************************** */
    /** @param {{ id, msg, herror, hsuccess, name, args }} srvmsg */
    function recovers_from_server(srvmsg, ctx) {
        var id = srvmsg.id;
        var message = srvmsg.msg;
        var herror = srvmsg.herror;
        var hsuccess = srvmsg.hsuccess;
        #<Ifstatic:PING_DEBUG>
              ping_debug("COMM", "Received message "+message+" for channel "+id);
        #<End>
        var lchan = client_stored[id];
        if (herror != undefined)
            herror = function(){unserialize_uu(srvmsg.herror)()};
        if (hsuccess != undefined)
            hsuccess = function(){unserialize_uu(srvmsg.hsuccess)()};
        if (lchan != null){
            var unser_msg = lchan.unserialize(message);
            lchan.send(null, unser_msg, ctx, herror, hsuccess);
        } else if (herror !=  undefined){
            // Session doesn't exists execute handler of error
            herror();
            #<Ifstatic:PING_DEBUG>
            ping_debug("COMM", "Local channel does not exist: "+id);
            #<End>
        }
    }

    /** Process messages according to here type */
    /** @param {{ id, msg, herror, hsuccess, name, args }} mess */
    function process_msg (mess){
        switch(mess.type){
        case "rpc" :
            RPC_call(mess.id, mess.name, mess.args);
            break;
        case "asyncrpc" :
            RPC_call(null, mess.name, mess.args);
            break;
        case "chan" :
            recovers_from_server(mess, js_none);
            break;
        default :
            error("Messages type "+mess.type+" is unknown");
        }
    }

    var the_ping_loop;
    var the_pang_loop;
    {
        /* Private variables **************************** */
        var cpt = 0;
        var launched = false;
        var panged = { waiting_pang : 0 };

        var max_failure_delay = 90000;
        var min_failure_delay = 15000;
        var failure_delay = min_failure_delay;

        /* Request handlers ***************************** */
        /** Make somethings with the loop response */
        function success_ping_response(response, nb){
            var native_response = %%BslJson.json.string_to_native%%(response);
            switch(native_response.type){
            case "pong" : break;
            case "break" : return;
            case "msgs" :
                var messages = native_response.body;
                for(var i = 0; i < messages.length; i++){
                    process_msg(messages[i]);
                }
                break;
            case "result" :
                return native_response;
            default :
                error("Ping loop type "+native_response.type+" is unknown");
            }
            /* Relaunch loop */
            if (!async_rpc_return){
                internal_loop(false);
            }
            else if ((nb != undefined && nb >= cpt))
                internal_loop(false);
        }

        /** Make somethings when an error occurs with the ping loop */
        function error_ping_response(xhr, str, exn){
            if (failure_delay == max_failure_delay){
                jlog("Error: the connexion with the server seems to be lost");
                return;
            } else {
                //jlog("Warning: could not reach the server. Retrying in " + failure_delay);
            }
            setTimeout(internal_loop, failure_delay);
            failure_delay = Math.min (failure_delay * 2, max_failure_delay);
        }

        /* Main loop function *************************** */
        function internal_loop(force){
            var nb = ++cpt;
            var f = function(){
                if (nb < cpt) {
                    // jlog("Don't launch ping loop :"+ nb +" vs "+cpt);
                } else {
                    internal_ajax({
                            type : 'POST',
                            url : "/ping",
                            data : JSON.stringify(cpt),
                            success : function(r){success_ping_response(r, nb)},
                            error : error_ping_response
                        });
                }
            };
            if(force==true){f();}else{setTimeout(f, 0);}
        }

        function internal_pang_loop(url, req, is_json){
            var request, response, result, id;
            cpt++;
            id = cpt;
            panged[id] = null;
            panged.waiting_pang++;
            request = {
                ping : cpt,
                uri : url,
                body : is_json ? JSON.stringify(req) : req
            };
            while(panged[id] === null){
                response =
                    internal_ajax({
                            type : 'POST',
                            async : false,
                            url : "/pang",
                            data : JSON.stringify(request)
                        });
                result = success_ping_response(response.responseText, -1);
                if (result != undefined) panged[result.id]=result.body;
                request = ++cpt;
            }
            result = panged[id];
            delete(panged[id]);
            panged.waiting_pang--;
            if(panged.waiting_pang == 0) internal_loop(false);
            return result;
        }

        the_pang_loop = internal_pang_loop;

        the_ping_loop = function(){
            if(!launched){
                launched = true;
                internal_loop(true)
            }
        }
    }

    /* ************************************************** */
    /* LowLevelSession - Exported functions ************* */
    var shared = null;
    function set_shared(){
        if (shared == null){
            var rep = internal_ajax({
                    type : 'POST',
                    async : false,
                    url : "/chan/sharedaddr"
                });
            shared = JSON.parse(rep.responseText);
        }
    }

    LowLevelSession.llmake = function(st, unserialize, fun_session,
                                      ctx, dfun, more, cps_mode, concurrent) {
        the_ping_loop();
        var local =
        new LocalChannel(st, unserialize, fun_session,
                         ctx, dfun, more, cps_mode, concurrent);
        #<Ifstatic:PING_DEBUG>
        ping_debug("SESSION", "Creating local session");
        #<End>
        return local;
    }

    LowLevelSession.unserialize= function(chan) {
        var rtchan;
        if ("srv_id" in chan) {
            rtchan = new ServerChannel(chan["srv_id"]);
        } else if ("cl_id" in chan) {
            var lchan = client_stored[chan.cl_id];
            if (lchan != null) {
                return lchan;
            } else {
                rtchan = new CousinClientChannel(chan.cl_id);
            }
        } else {
            throw new Error("Bad formatted channel");
        }
        if ("addr" in chan && "port" in chan){
            set_shared();
            if (shared.addr != chan.addr || shared.port != chan.port){
                rtchan.addr = chan.addr;
                rtchan.port = chan.port;
            }
        }
        return rtchan;
    }


    LowLevelSession.get_more = function(chan){
        var more = chan.more;
        if (chan.more === undefined){
            //TODO -> throw new Error("Internal Error");
            return js_none;
        }
        return more;
    }

    LowLevelSession.set_uu = function(s, u){
        serialize_uu = s;
        unserialize_uu = u;
    }

    LowLevelSession.serialize_and_share = function(chan){
        var shared_serialize = chan.shared_serialize;
        if (shared_serialize == null){
            var ser = chan.serialize();
            shared_serialize = {};
            for (var i in ser) shared_serialize[i] = ser[i];
            if (shared_serialize.addr == null){
                set_shared();
                shared_serialize.addr = shared.addr;
                shared_serialize.port = shared.port;
            }
            chan.shared_serialize = shared_serialize;
        }
        return shared_serialize;
    }

    LowLevelSession.exportt = function(chan, entity){
        if (entity == null) return chan.serialize();
        var e = entity.serialize();
        var c;
        if (e.addr != null){
            c = LowLevelSession.serialize_and_share(chan)
        } else {
            c = chan.serialize();
        }
        if (! (entity.srv_id != null && entity.addr ==null)){
            var m = {entity : e, channel : c};
            to_register.push(m);
        }
        return c;
    }

    /* ************************************************** */
    /* LowLevelRPC - Exported functions ***************** */
    /** Adding a skeletons with the key [name].*/
    LowLevelRPC.comet_table_add = function(name, skeleton) {
        #<Ifstatic:PING_DEBUG>
        var is_here = RPC_comet_table[name];
        if (is_here != null) {
            throw Error("Rpc client "+name+" is already registered");
        }
        #<End>
        RPC_comet_table[name] = skeleton;
    }

    /* ************************************************** */
    /* LowLevelPingLoop - Exported functions ************ */
    /**
     * Start the PING loop, this loop allows to send request to the
     * client (RPC, Session, ...)
     */
    LowLevelPingLoop.start = the_ping_loop;

    /** Setting the page identifier */
    LowLevelPingLoop.set_page = function(page){
        page_index = page;
        internal_url = base_url_client+"/_internal_/"+page_index;
    }

    /**
     * Like [jQuery.ajax] but for internal url of OPA server
     */
    LowLevelPingLoop.ajax = internal_ajax;

    /**
     * Make an internal synchronous request using the pang system. Can
     * froze the client GUI. But the client can always interact with
     * the server unlike the simple synchronous request.
     * @param url Url where to send the request
     * @param request Request to send on url
     * @param is_json Indicates if the request is not already
     * stringify (Is an javascript object), or is already stringify
     * (Is a javascript string to send without traitment)
     * @return the result of request
     */
    LowLevelPingLoop.pang_request = function(url, request, is_json){
        return the_pang_loop(internal_url + url, request, is_json);
    }

    LowLevelPingLoop.async_call = function(url, request){
        internal_ajax({ type : 'POST',
                        url : url,
                        data : request
                      });
    }}


/* ****************************************************** */
/* BSL REGISTERING ************************************** */

/* Session ********************************************** */
##extern-type Session.private.native('a, 'b)

##extern-type Session.entity

##opa-type ThreadContext.client

##register set_uu \ `LowLevelSession.set_uu` : \
  ((-> void) -> RPC.Json.private.native), \
  (RPC.Json.private.native -> (-> void)) -> void

##register [cps-bypass] llmake_cps : \
   'st, \
   (option('ctx), RPC.Json.private.native, continuation(opa[option('msg)]) -> void), \
   ('st, 'msg, option('ctx), continuation(opa[option('st)]) -> void), \
   option(continuation(opa[void]) -> void), \
   option('ctx), \
   option('more), \
   bool, \
   continuation(Session.private.native('msg, 'ctx)) -> \
   void
##args(state, unser, fun, dfun, ctx, more, concurrent, k)
{
    throw new Error("Not yet implemented, TODO for cps client.");
}

##register llmake : \
    'st, \
    (option('ctx), RPC.Json.private.native -> option('msg)), \
    ('st, 'msg, option('ctx) -> option('st)), \
    option(-> void), \
    option('ctx), \
    option('more), \
    bool -> \
    Session.private.native('msg, 'ctx)
//'
##args(state, unser, fun, dfun, ctx, more, concurrent)
{
  var unserbis = function (x) {
    /* js_none because when we unserialize on client we don't care of
     * thread context owner... for moment*/
    var result = unser(js_none, x);
    if ('none' in result) {
      throw new Error("Unserialize fail");
    } else {
      return result.some;
    }
  };
  /* cps_mode is falseelse opa use llmake_cps*/
  return LowLevelSession.llmake(state, unserbis, fun, ctx, dfun, more,
                                false, concurrent);
}

/*
 * Important note: We only export equality check as the order is not stable from client to server -- and not even stable in time inside the client
 */
##register equal_channel : Session.private.native('b, 'c), Session.private.native('b, 'c) -> bool
##args(ch1, ch2)
{
    if(ch1.compare(ch2) == 0)
        return true
    else
        return false
}

##register compare_channels : Session.private.native('msg, 'ctx), Session.private.native('msg, 'ctx) -> int
##args(ch1, ch2)
{
    return ch1.compare(ch2);
}

##register llsend : Session.private.native('b, 'c), ('b -> RPC.Json.private.native), 'b, option('c) -> void
##args(ch, ser, msg, ctx)
{
    ch.send(ser, msg, ctx);
}

##register llsend_then : \
  Session.private.native('msg, 'ctx), \
  ('msg -> RPC.Json.private.native), \
  'msg, option('ctx), (-> void), (-> void) -> void
##args(ch, ser, msg, ctx, herror, hsuccess)
{
    ch.send(ser, msg, ctx, herror, hsuccess);
}

##register export : Session.private.native('msg, 'ctx), opa[ThreadContext.client] -> RPC.Json.private.native
##args(chan, _)
{
    return chan.serialize();
}

##register serialize_for_entity : Session.private.native('b, 'c), Session.entity -> RPC.Json.private.native
##args(chan, entity)
{
    return LowLevelSession.exportt(chan, entity);
}

##register unserialize : option('c), RPC.Json.private.native -> option(Session.private.native('b, 'c))
##args(_, str_chan)
{
    try {
      return js_some (LowLevelSession.unserialize(str_chan));
    } catch(e) {
      return js_none;
    }
}

##register on_remove : Session.private.native('msg, 'ctx), (-> void) -> void
##args(chan, callback)
{
    %%bslsyslog_warning%%("[Session.on_remove]", "client-side version not implemented");
    return;
}

##register is_remote : Session.private.native('msg, 'ctx) -> bool
##args(chan)
{
    return ('addr' in chan);
}

##register is_local : Session.private.native('msg, 'ctx) -> bool
##args(_)
{
    // This is incorrect, but unused yet.
    // If we implement a feature about timeout for cell calls
    // on the client side, this will probably need an update.
    return false;
}

##register owner : Session.private.native('msg, 'ctx) -> option(Session.entity)
##args(chan)
{
    var r = chan.owner();
    if(r==null){
        return js_none;
    } else {
        return js_some(r);
    }
}

##register is_client : option(Session.entity) -> bool
##args(chan)
{
    if (chan.some){
        return chan.some.is_client;
    } else {
        return true;
    }
}

##register get_more : Session.private.native('msg, 'ctx) -> option('more)
##args(chan)
{
    return LowLevelSession.get_more(chan)
}

##register comet_table_add : string, (string -> option(string)) -> void
##args(str,f)
{
    LowLevelRPC.comet_table_add(str,f);
}


/* Ping loop system ************************************* */
##module PingRegister \ bsl_pingregister

    ##register pang_request : string, string -> string
    ##args(url, request)
    {
        return LowLevelPingLoop.pang_request(url, request, false);
    }

    ##register pang_json_request : string, RPC.Json.private.native -> string
    ##args(url, request)
    {
        return LowLevelPingLoop.pang_request(url, request, true);
    }

    ##register ping_async_call: string, string -> void
    ##args(url, request)
    {
        LowLevelPingLoop.async_call(url, request);
    }

##endmodule

/* Hack for non cps client ****************************** */
##module SynchronousCell
    /**
     * Synchronous call a cell used for non cps mode.
     */
    ##register llcall : 'a, 'message, ('message -> RPC.Json.private.native), (RPC.Json.private.native -> 'result), ('c, 'message -> 'b) -> 'result
    ##args(chan, message, serialize, unserialize, onmsg)
    {
        return chan.call_no_cps(message, serialize, unserialize, onmsg);
    }

##endmodule

//Start ping loop
if (!command_line_execution) LowLevelPingLoop.start();
