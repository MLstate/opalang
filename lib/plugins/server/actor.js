/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Defines the LocalChannel prototype and the local storage. A LocalChannel is a
 * local and distribuable Opa actor.
 * The local storage is used for serializing and retrieving local channels.
 *
 * Note : Depending on debug variables, we provide two different
 * implementations. One that supports CPS and another one doesn't.
 *
 * @author Quentin Bourgerie
 */


var LocalChannelStore = function(){
    /**
     * The table of channels which have been sent to the server and
     * could be received back.
     *
     * A mapping from local channel id (as generated during
     * the first serialization of [LocalChannel]) to [LocalChannel]
     */
    var stored = new Object();
    function random_id() {
        return Math.floor(Math.random() * 1073741824);
    }
    var generate_id = function(){
        var rand = random_id();
        #<Ifstatic:OPABSL_NODE>
        return rand;
        #<Else>
        var c = getStableCookie();
        if ( 'none' in c )
            throw new Error("no cookie no session");
        else {
            return ("" + rand + c.some);
        }
        #<End>
    }
    return {
        get : function(id){
            return stored[id];
        },

        remove : function(id){
            delete stored[id]
        },

        serialize : function(chan){
            var serialized = chan.serialized;
            if (serialized == null){
                #<Ifstatic:OPABSL_NODE>
                serialized = {srv_id : generate_id()};
                chan.serialized = serialized;
                stored[serialized.srv_id] = serialized;
                #<Else>
                serialized = {cl_id : generate_id()};
                chan.serialized = serialized;
                stored[serialized.cl_id] = chan;
                #<End>
            }
            return serialized;
        },
        random_id : random_id
    };

}();

/**
 * A channel which may be used to send messages locally
 *
 * @constructor
 */
function LocalChannel(st, unserialize, fun_session, ctx, dfun, more, concurrent) {
    this.lchan_id = LocalChannelStore.random_id();
    this.state = st;
    this.action = fun_session;
    this.unserialize = unserialize;
    this.messages = new Array() ;
    this.on_delete = dfun === null ? [] : [dfun];
    this.more = more;
    this.ctx = ctx;
    this.concurrent = concurrent;
    this.killed = false;
    this.is_client = true;
}

LocalChannel.prototype = {

    get_context:function(context){
        if ('some' in this.ctx) return this.ctx;
        else return context;
    },

    /* ************************************************** */
    /* Sending functions ******************************** */

    /**
     * An auxiliar function for send a message to a session.
     *
     * @param {*} msg
     * @param {*} context
     * @param {?Function} herror A function that is called if
     * session is Killed.
     * @param {?Function} hsuccess A function that called if
     * message is treated.
     *
     * Note : USE UNIQUELY ON NO CPS MODE (like its name indicates)
     *
     * By putting the function header inside the PP block, we were losing
     * the annotation.
     */
    #<Ifstatic:OPA_CPS_CLIENT>
    #<Else>
    send_no_cps_aux: function(msg, context, herror, hsuccess){
        // Session has been stopped
        if(this.state == null) {
            #<Ifstatic:SESSION_DEBUG>>
            sess_debug("Call a killed session : "+this.lchan_id);
            #<End>
            if (herror) herror();
            #<Ifstatic:SESSION_DEBUG>>
            sess_debug("[LocalChannel.send] Killed :"+er);
            #<End>
            return;
        }
        // Get the good context (owner if setted, sender else)
        var ctx = this.get_context(ctx);

        // Perform action
        #<Ifstatic:SESSION_DEBUG>> ping_debug("Start handler"); #<End>
        if (hsuccess) hsuccess();
        var new_st = null ;
        try {
            new_st = this.action(this.state, msg, ctx);
            #<Ifstatic:SESSION_DEBUG>> ping_debug("End handler"); #<End>
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
            // TODO - plugins dependencies
            // bslsyslog_error("[LocalChannel.send] Catch :", er);
            console.error("[LocalChannel.send] Catch :", er);
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
                    var ctx = lchan.get_context(cpl.ctx);
                    if (typeof hsuccess != 'undefined') hsuccess();
                    lchan.action(stt, cpl.msg, ctx, function(new_st){
                      if ('none' in new_st) {
                          //If the action returns [none], we should kill the session
                          lchan.state = null;
                          lchan.kill();
                      } else {
                          push(function() {aux(new_st.some); return 0});
                      }
                    });
                }
            }
            push(function() {aux(st); return 0});
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

    /**
     * Call a local cell represented by [lchan] with the message
     * [msg]. Same remarks that [llsend_lchan_no_cps].
     * This implementation of [Cell.call] is not very fair. Indeed if a
     * message waiting for the session, this call will be execute before
     * the waiting message. We can have also an unlikely starvation with
     * this implementation. But that respect the semantic of session.
     */
    #<Ifstatic:OPA_CPS_CLIENT>
    #<Else>
    call_no_cps: function(msg, _1, _2, onmsg){
        // That is a very dirty hack, stdlib
        // implementation dependent (see also cell.opa)
        var res = null;
        try {
            var f = function(state, msg){
                // Make cell message, with a continuation which set [res]
                var cmsg = empty_constructor()
                cmsg = add_field(cmsg,static_field_of_name('f1'),cont(function(r){res = r}))
                cmsg = add_field(cmsg,static_field_of_name('f2'),msg)
                cmsg = make_record(cmsg)
                // [onmsg] returns the session instruction
                return onmsg(state, cmsg);
            }
            this.action = f;
            this.send_no_cps_aux(msg, js_none, null, null);
        } catch (er) {
            // TODO - plugins dependencies
            // bslsyslog_error("[LocalChannel.call] Cell :", er);
            console.error("[LocalChannel.call] Cell :", er);
        }
        if(res === null) error("Call failed, result was [null]");
        return res;
    },
    #<End>


    /* ************************************************** */
    /* Utils **************************************** */
    /**
     * Return the serialized form of the channel.
     */
    serialize: function(){
        return LocalChannelStore.serialize(this);
    },

    /**
     * Kill a local channel
     *
     * Trigger any [on_delete] callbacks
     */
    kill: function(){
        this.killed = true;
        var on_delete = this.on_delete;
        var n = on_delete.length;
        for (var i = 0; i < n; i ++) {
            on_delete[i]();
        }
        while(this.messages.length > 0){
            var herror = this.messages.pop().herror;
            if (typeof herror != 'undefined') herror();
        }
        var serialized = LocalChannelStore.get(this.lchan_id);
        if(serialized != null){
            #<Ifstatic:OPABSL_NODE>
            LocalChannelStore.remove(serialized.srv_id);
            #<Else>
            LocalChannelStore.remove(serialized.cl_id);
            #<End>
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
    },

    /**
     * Adds a callback which will be called, when the actor stops.
     */
    on_remove: function(callback) {
        if(this.killed){
            callback();
        } else {
            this.on_delete.push(callback);
        }
    }

}
