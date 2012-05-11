/*
    Copyright © 2011, 2012 MLstate

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

/**
 * Defines the LocalChannel prototype and the local storage. A LocalChannel is a
 * local and distribuable Opa actor.
 * The local storage is used for serialize and retreive local channels.
 *
 * Note : Dependings to debug variables, we provides two different
 * implementation. One that supports CPS the other one don't.
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
    return {
        get : function(id){
            return stored[id];
        },

        remove : function(id){
            delete stored[id]
        },

        serialize : function(chan){
            var serialized = stored[chan.lchan_id];
            if (serialized == null){
                serialized = {cl_id : generate_cl_id()};
                chan.serialized = serialized;
                stored[serialized.cl_id] = serialized;
            }
            return serialized;
        }
    };

}();

/**
 * A channel which may be used to send messages locally
 *
 * @constructor
 */
function LocalChannel(st, unserialize, fun_session, ctx, dfun, more, concurrent) {
    var tmp;
    this.lchan_id = Math.floor(Math.random() * 1073741824);
    this.state = st;
    this.action = fun_session;
    this.unserialize = unserialize;
    this.messages = new Array() ;
    this.on_delete = (tmp = dfun.some)?[tmp]:[];
    this.more = more;
    this.ctx = ctx;
    this.concurrent = concurrent;
    this.killed = false;
    this.is_client = true;
}

LocalChannel.prototype = {

    /* ************************************************** */
    /* Sending functions ******************************** */

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
            #<Ifstatic:SESSION_DEBUG>>
            sess_debug("Call a killed session : "+this.lchan_id);
            #<End>
            if (herror != undefined) herror();
            #<Ifstatic:SESSION_DEBUG>>
            sess_debug("[LocalChannel.send] Killed :"+er);
            #<End>
            return;
        }
        // Get the good context (owner if setted, sender else)
        var ctx;
        if ('some' in this.ctx) ctx = this.ctx;
        else ctx = context;
        // Perform action
        #<Ifstatic:SESSION_DEBUG>> ping_debug("Start handler"); #<End>
        if (hsuccess !=  undefined) hsuccess();
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
            // bslsyslog_error%%("[LocalChannel.send] Catch :", er);
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
                    var ctx;
                    if ('some' in lchan.ctx){
                        ctx = lchan.ctx;
                    } else {
                        ctx = cpl.ctx;
                    }
                    if (hsuccess !=  undefined) hsuccess();
                    lchan.action(stt, cpl.msg, ctx, function(new_st){
                      if ('none' in new_st) {
                          //If the action returns [none], we should kill the session
                          lchan.state = null;
                          lchan.kill();
                      } else {
                          setTimeout(function() {aux(new_st.some)}, 0);
                      }
                    );
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
            // TODO - plugins dependencies
            // bslsyslog_error%%("[LocalChannel.call] Cell :", er);
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
            if (herror != undefined) herror();
        }
        var serialized = LocalChannelStore.get(this.lchan_id);
        if(serialized != null){
            LocalChannelStore.remove(serialized.cl_id);
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
