/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * 1 - The client implementation for Opa actor : Wrapper of LocalChannel +
 * ServerChannel + CousinClientChannel.
 * 2 - Client RPC
 * 3 - Ping mescanism
 *
 * @author Quentin Bourgerie
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
(function () {
    /* ************************************************** */
    /* Variables definitions **************************** */
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
     * the server)
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

    // All internal request needs a page number
    var internal_url = base_url_client+"/_internal_/"+page_index;

    var async_rpc_return = true;
    /* If client is safari, we must force rpc to be synchronous */
    var hack = typeof(desactivate_safari_hack) == 'undefined' || !desactivate_safari_hack
    if (hack && !command_line_execution && 'userAgent' in navigator){
        var ua = navigator.userAgent;
        async_rpc_return = (!/Safari/.test(ua)) || /Chromium/.test(ua);
    }

    var serialize_uu = function(_){error("Unregistered serialization function")}

    var unserialize_uu = function(_){error("Unregistered unserialization function")}

    var max_pang_attempt = 3;

    /* DEBUG FUNCTIONS*/
    #<Ifstatic:PING_DEBUG>
        function ping_debug (x, y) {
        console.log("PING", x, y);
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

    var linked_with_server = true;

    function break_ping_loop() {
        linked_with_server = false;
        jlog("Error: the connexion with the server seems to be lost. Please reload");
        throw("Error: the connexion with the server seems to be lost. Please reload");
    }

    // Domain used for AJAX request
    var domain_url = "";

    function internal_ajax(settings){
        if(linked_with_server){
            settings.url = domain_url + internal_url + settings.url;
            if(to_register.length != 0){
                var body = settings.data;
                if (typeof body == 'undefined') body="";
                var data = {
                    to_register:to_register,
                    uri:settings.url,
                    body:body
                };
                settings.data = JSON.stringify(data);
                settings.url = internal_url + "/chan/register";
                to_register = new Array();
                var success
                if(settings.success){
                    success = settings.success
                    settings.success = function () {}
                } else {
                    success = function () {}
                }
                settings.statusCode = { 205: break_ping_loop,
                                        200: success};
            }
            return jQuery.ajax(settings);
        }
        else {
            break_ping_loop();
        }
    }





    /* ************************************************** */
    /* Distant Channel ********************************** */
    /**
     * A common prototype for [ServerChannel] and [ClientChannel]
     */
    var distantChannelPrototype = {

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
            if (typeof herror != 'undefined') msg_to_post.herror = serialize_uu(herror);
            if (typeof hsuccess != 'undefined') msg_to_post.hsuccess = serialize_uu(hsuccess);
            var serialized_msg = JSON.stringify(msg_to_post);
            internal_ajax({ type : 'POST',
                            url : "/chan/send",
                            data : serialized_msg,
                            async: async_rpc_return
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
            var json = string_to_native(str);
            return unserialize(json);
        },
        #<End>

        message_to_post: function(message) {
            return { to: this.serialize(),
                     message: message };
        }
    };

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
            if (typeof this.addr != 'undefined'){
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

        on_remove: function(callback) {
            return null;
        },

       /**
         * Send a message along this channel
         *
         * @param serialize A function (generally provided by OPA)
         * used to serialize the message to a json
         * @param msg The message, unserialized
         * @param ctx Ignored
         */
        send: distantChannelPrototype.send,

        call_no_cps: distantChannelPrototype.call_no_cps,

        message_to_post: distantChannelPrototype.message_to_post

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
            if (typeof this.addr != 'undefined'){
                r.addr = this.addr;
                r.port = this.port;
            }
            return r;
        },


        compare: function(to) {
            if(to instanceof CousinClientChannel)
                return compare_native(this.cl_id, to.cl_id);
            else
                if(to instanceof LocalChannel)
                    return -1;
                else // ServerChannel
                    return 1;
        },

        owner: function(){
            return this;
        },

        on_remove: function(callback) {
            return null;
        },

        /**
         * Send a message along this channel
         *
         * @param serialize A function (generally provided by OPA)
         * used to serialize the message to json
         * @param The message, unserialized
         * @param ctx Ignored
         */
        send: distantChannelPrototype.send,

        call_no_cps: function(){
            throw Error("Not yet implemented");
        },

        message_to_post: distantChannelPrototype.message_to_post

    }

    /**
     * @param {...*} var_args
     */
    function log_error(var_args) {
        if(window.console && window.console.error) window.console.error.apply(null, arguments);
        else jlog.apply(null, arguments);
    }
    /* ************************************************** */
    /* RPC support (server -> client) ******************* */
    /** Contains skeletons. */
    var RPC_comet_table = new Object();

    /* the id is null when the call is async */
    function RPC_call(id, name, argument) {
        var funct = RPC_comet_table[name];
        if ( funct == null ) throw new Error("Rpc client "+name+" doesn't exist");
        // hook for try catch there, and return a special case, maybe e.g. rpc_return_exc
        var data = funct(argument);
        if ('none' in data) {
            log_error("RPC comet call ", id, " failed, no data in ", argument);
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
        var lchan = LocalChannelStore.get(id);
        if (typeof herror != 'undefined')
            herror = function(){unserialize_uu(srvmsg.herror)()};
        if (typeof hsuccess != 'undefined')
            hsuccess = function(){unserialize_uu(srvmsg.hsuccess)()};
        if (lchan != null){
            var unser_msg = lchan.unserialize(message);
            lchan.send(null, unser_msg, ctx, herror, hsuccess);
        } else if (typeof herror != 'undefined'){
            // Session doesn't exist execute handler of error
            herror();
            #<Ifstatic:PING_DEBUG>
            ping_debug("COMM", "Local channel does not exist: "+id);
            #<End>
        }
    }

    /** Process messages according to here type */
    /** @param {{ id, msg, herror, hsuccess, name, args }} mess */
    function native_process (mess){
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

    var process_msg = function(_){return false};

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
            var native_response = string_to_native(response);
            switch(native_response.type){
            case "pong" : break;
            case "break" : return;
            case "msgs" :
                var messages = native_response.body;
                for(var i = 0; i < messages.length; i++){
                    var n = messages[i];
                    var m = native_to_json(n);
                    m = option2js(m);
                    if (m!=null){
                        if(!process_msg(m)){
                            native_process(n);
                        }
                    } else {
                        console.error("LLPing", "Bad json", messages[i]);
                    }
                }
                if(nb == -1) return null; else break;
            case "result" :
                return native_response;
            default :
                error("Ping loop type "+native_response.type+" is unknown");
            }
            /* Relaunch loop */
            if (!async_rpc_return){
                internal_loop(false);
            }
            else if ((typeof nb != 'undefined' && nb >= cpt))
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
            var attempt = 0;
            while(panged[id] === null){
                response =
                    internal_ajax({
                            type : 'POST',
                            async : false,
                            url : "/pang",
                            data : JSON.stringify(request)
                        });
                result = success_ping_response(response.responseText, -1);
                if (result !== null && typeof result != 'undefined') {
                    if(typeof panged[result.id] == 'undefined') {
                        log_error("Receive the pang("+result.id+") result too late", result);
                    } else {
                        panged[result.id]=result.body;
                    }
                } else if (typeof result == 'undefined'){
                    if(attempt >= max_pang_attempt) {
                        delete(panged[id]);
                        panged.waiting_pang--;
                        if(panged.waiting_pang == 0) internal_loop(false);
                        throw new Error("Maximum pang attempt reached");
                    }
                    attempt++;
                }
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
                    url : "/chan/sharedaddr",
                    async: async_rpc_return
                });
            shared = JSON.parse(rep.responseText);
        }
    }

    LowLevelSession.set_domain_url = function(d) {
	domain_url = d;
    }

    LowLevelSession.llmake = function(st, unserialize, fun_session,
                                      ctx, dfun, more, concurrent) {
        the_ping_loop();
        var local =
        new LocalChannel(st, unserialize, fun_session,
                         ctx, dfun, more, concurrent);
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
            var lchan = LocalChannelStore.get(chan.cl_id);
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
        if (typeof chan.more == 'undefined') {
            //TODO -> throw new Error("Internal Error");
            return js_none;
        }
        return more;
    }

    LowLevelSession.set_uu = function(s, u){
        serialize_uu = s;
        unserialize_uu = u;
    }

    LowLevelSession.serialize = function(chan){
        var serialized = chan.serialized;
        if(serialized == null){
            serialized = chan.serialize();
            if(chan.on_remove != null){
                chan.on_remove(function(){
                        internal_ajax({ type : 'POST',
                                    url : "/chan/remove",
                                    data : JSON.stringify(serialized),
                                    async: async_rpc_return});
                    });
            }
            if (chan instanceof LocalChannel)
                to_register.push(serialized);
        }
        return serialized;
    }

    LowLevelSession.serialize_and_share = function(chan){
        var shared_serialize = chan.shared_serialize;
        if (shared_serialize == null){
            var ser = LowLevelSession.serialize(chan);
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
        if (entity == null) return LowLevelSession.serialize(chan);
        var e = entity.serialize();
        var c;
        if (e.addr != null){
            c = LowLevelSession.serialize_and_share(chan)
        } else {
            c =  LowLevelSession.serialize(chan);
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

    LowLevelPingLoop.internal_prefix =
        base_url_client+"/_internal_/"+page_index;

    /**
     * Like [jQuery.ajax] but for internal url of OPA server
     */
    LowLevelPingLoop.ajax = internal_ajax;

    LowLevelPingLoop.set_max_pang_attempt = function(i){
        max_pang_attempt = i;
    }

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
                        data : request,
                        async : async_rpc_return
                      });
    }

    LowLevelPingLoop.process_msg = function(f){
        process_msg = f;
    }

})();


/* ****************************************************** */
/* BSL REGISTERING ************************************** */

/* Session ********************************************** */
/** @externType Session.private.native('a, 'b) */

/** @externType OpaNetwork.entity */

/** @opaType ThreadContext.client */

/**
 * @register {((-> void) -> RPC.Json.private.native), \
              (RPC.Json.private.native -> (-> void)) -> void}
 */
function set_uu(x0, x1) {
    LowLevelSession.set_uu(x0, x1);
    return js_void;
}

/**
 * @register {string -> void}
 */
function set_domain_url(d) {
    LowLevelSession.set_domain_url(d);
    return js_void;
}

/**
 * @register {'st, \
              (opa[option('ctx)], RPC.Json.private.native, \
                 continuation(opa[option('msg)]) -> void), \
              ('st, 'msg, opa[option('ctx)], \
                 continuation(opa[option('st)]) -> void), \
              option(continuation(opa[void]) -> void), opa[option('ctx)], \
              opa[option('more)], bool, \
              continuation(Session.private.native('msg, 'ctx)) -> void}
 * @cpsBypass
 */
function llmake_cps(state, unser, fun, dfun, ctx, more, concurrent, k) {
    return_(k, LowLevelSession.llmake(state, unser, fun, ctx, dfun, more, concurrent))
}

/**
 * @register {'st, (opa[option('ctx)], RPC.Json.private.native -> opa[option('msg)]), \
              ('st, 'msg, opa[option('ctx)] -> opa[option('st)]), opa[option(-> void)], \
              opa[option('ctx)], opa[option('more)], bool -> Session.private.native('msg, 'ctx)}
 */
function llmake(state, unser, fun, dfun, ctx, more, concurrent) {
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
  return LowLevelSession.llmake(state, unserbis, fun, ctx, dfun, more, concurrent);
}

/*
 * Important note: We only export equality check as the order is not stable from client to server -- and not even stable in time inside the client
 */
/**
 * @register {Session.private.native('b, 'c), Session.private.native('b, 'c) -> bool}
 */
function equal_channel(ch1, ch2) {
    if(ch1.compare(ch2) == 0)
        return true
    else
        return false
}

/**
 * @register {Session.private.native('msg, 'ctx), Session.private.native('msg, 'ctx) -> int}
 */
function compare_channels(ch1, ch2) {
    return ch1.compare(ch2);
}

/**
 * @register {Session.private.native('b, 'c), ('b -> RPC.Json.private.native), 'b, opa[option('c)] -> void}
 */
function llsend(ch, ser, msg, ctx) {
    ch.send(ser, msg, ctx);
}

/**
 * @register {Session.private.native('msg, 'ctx), \
              ('msg -> RPC.Json.private.native), 'msg, opa[option('ctx)], \
              (-> void), (-> void) -> void}
 */
function llsend_then(ch, ser, msg, ctx, herror, hsuccess) {
    ch.send(ser, msg, ctx, herror, hsuccess);
}

/**
 * @register {Session.private.native('msg, 'ctx), \
              opa[ThreadContext.client] -> RPC.Json.private.native} export
 */
function export_(chan, _) {
    return LowLevelSession.serialize(chan);
}

/**
 * @register {Session.private.native('b, 'c), OpaNetwork.entity -> RPC.Json.private.native}
 */
function serialize_for_entity(chan, entity) {
    return LowLevelSession.exportt(chan, entity);
}

/**
 * @register {opa[option('c)], RPC.Json.private.native -> opa[option(Session.private.native('b, 'c))]}
 */
function unserialize(_, str_chan) {
    try {
      return js_some (LowLevelSession.unserialize(str_chan));
    } catch(e) {
      return js_none;
    }
}

/**
 * @register {Session.private.native('msg, 'ctx), (-> void) -> void}
 */
function on_remove(chan, callback) {
    chan.on_remove(callback);
}

/**
 * @register {Session.private.native('msg, 'ctx) -> bool}
 */
function is_remote(chan) {
    return ('addr' in chan);
}

/**
 * @register {Session.private.native('msg, 'ctx) -> bool}
 */
function is_local(chan) {
    return (chan instanceof LocalChannel);
}

/**
 * @register {Session.private.native('msg, 'ctx) -> opa[option(OpaNetwork.entity)]}
 */
function owner(chan) {
    var r = chan.owner();
    if(r==null){
        return js_none;
    } else {
        return js_some(r);
    }
}

/**
 * @register {OpaNetwork.entity -> bool}
 */
function is_client(chan) {
    return chan.is_client;
}

/**
 * @register {Session.private.native('msg, 'ctx) -> opa[option('more)]}
 */
function get_more(chan) {
    return LowLevelSession.get_more(chan)
}

/**
 * @register {string, (string -> opa[option(string)]) -> void}
 */
function comet_table_add(str,f) {
    LowLevelRPC.comet_table_add(str,f);
}

/**
 * @register {string, 'st, \
              (opa[option('ctx)], RPC.Json.private.native, \
                 continuation(opa[option('msg)]) -> void), \
              ('st, 'msg, opa[option('ctx)], \
              continuation(opa[option('st)]) -> void), \
              option(continuation(opa[void]) -> void), \
              opa[option('ctx)], opa[option('more)], bool, \
              continuation(Session.private.native('msg, 'ctx)) -> void}
 * @cpsBypass
 */
function make_shared(_key, _state, _unserialize, _handler,
                     _ondelete, _ctx, _more, _concurrent, _k) {
   error("Session.make_shared");
}

/**
 * @register {Session.private.native('msg, 'ctx), \
              opa[option(ThreadContext.client)] -> opa[option(string)]}
 */
function get_server_id(chan, _) {
    return js_some("TODO");
}

/* Ping loop system ************************************* */
/** @module PingRegister */

    /**
     * @register {string, string -> string}
     */
    function pang_request(url, request) {
        return LowLevelPingLoop.pang_request(url, request, false);
    }

    /**
     * @register {string, RPC.Json.private.native -> string}
     */
    function pang_json_request(url, request) {
        return LowLevelPingLoop.pang_request(url, request, true);
    }

    /**
     * @register {string, string -> void}
     */
    function ping_async_call(url, request) {
        LowLevelPingLoop.async_call(url, request);
    }

    /**
     * @register {int -> void}
     */
    function set_max_pang_attempt(i) {
        LowLevelPingLoop.set_max_pang_attempt(i);
        return js_void;
    }

    /**
     * @register {(RPC.Json.json -> bool) -> void}
     */
    function process_msg(processor) {
        LowLevelPingLoop.process_msg(processor);
        return js_void;
    }

    /** @register {string} internal_prefix LowLevelPingLoop.internal_prefix */

/** @endModule */

/* Hack for non cps client ****************************** */
/** @module SynchronousCell */
    /**
     * Synchronous call a cell used for non cps mode.
     */
    /**
     * @register {'a, 'message, ('message -> RPC.Json.private.native), \
                  (RPC.Json.private.native -> 'result), ('c, 'message -> 'b) \
                  -> 'result} llcall
     */
    function sync_cell_non_cps_llcall(chan, message, serialize, unserialize, onmsg) {
        return chan.call_no_cps(message, serialize, unserialize, onmsg);
    }

/** @endModule */

//Start ping loop
if (!command_line_execution) LowLevelPingLoop.start();
