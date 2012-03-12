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

(function($) {
    $.extend({
        /**
         * {1 The general system for managing anchors}
         */
        opa_anchors_initialized: false,
        opa_history_handlers_keys: [],
        opa_history_handlers_implem: {},

        /**
         * Add a history handler
         *
         * @param {function(string): *} callback A callback to trigger whenever history changes
         */
        add_history_handler: function(callback)
        {
            var implems = $.opa_history_handlers_implem;
            var keys    = $.opa_history_handlers_keys;

            if(!$.opa_anchors_initialized)
            {
                //Perform (lazy) initialization of our anchor handling system
                //Note: we do this lazily, as the system involves a [setTimeout] which may be costly
                function onchange(anchor)
                {
                    //console.log("[onchange] started");
                    //console.log(window.location.hash);
                    //console.log("[add_history_handler]: anchor is "+anchor);

                    var listener;
                    var i;
                    var len = keys.length;
                    for(i = 0; i < len; ++i)
                    {
                        listener = implems[keys[i]];
                        listener.call(listener, anchor);
                    }
                }

                //console.log("[add_history_handler] registering hash listener")
                //console.log(window.history.pushState)
                if(window.history.pushState)
                {
                    window.addEventListener("hashchange", function(event) {
                        onchange((window.location.hash || location.hash)
                                     .replace(/^[^#]*#/, '')	/* strip anything before the first anchor */
                                     .replace(/^#+|#+$/, ''));
                    }, false);
                } else {
                    //console.log("Browser offers no support for history");
                    var history = { hash: null };
                    function poll_changes()
                    {
                        var old_hash     = history.hash;
                        var current_hash = window.location.hash || location.hash;
                        if(old_hash != current_hash)
                        {
                            //console.log("This is a movement");
                            history.hash = current_hash;
                            onchange(current_hash
                                     .replace(/^[^#]*#/, '')	/* strip anything before the first anchor */
                                     .replace(/^#+|#+$/, ''));
                        }
                    }
                    window.setInterval(poll_changes, 200);
                }
                $.opa_anchors_initialized = true;
            }

            var name = "history/"+ (%%BslNumber.Random.string%%)(32);
            keys.push(name);
            implems[name] = callback;
            return function() {
                var index = keys.indexOf(name);
                if(index == -1) return;//We have already removed the handler
                keys.splice(index, 1);
                delete implems[name];
            };
        },

        /**
         * @param handle A function returned by [add_history_handler]
         */
        remove_history_handler: function(handle)
        {
            handle();
        },

        /**
         * {1 The subsystem for pushing states}
         */
        opa_states_implem:      null,

        push_state: function(callback)
        {
            var implems = $.opa_states_implem;
            if(!implems) //Perform lazy initialization
            {
                implems = $.opa_states_implem = {};
                $.add_history_handler(function(anchor) {
                    var handler = implems[anchor];
                    if(handler == null) return;
                    handler(anchor);
                });
            }

            var name = "position_"+ (%%BslNumber.Random.string%%)(32);
            //window.history.go(name);
            window.location.hash = name;

            $(window).trigger("hashchange");
            implems[name] = callback;
            return name;
        }
    });

})(jQuery);




##extern-type Client.Anchor.handler
##register push_state\ `jQuery.push_state`: (string -> void) -> string
##register add_history_handler\ `jQuery.add_history_handler`: (string -> void) -> (->void)
##register remove_history_handler\ `jQuery.remove_history_handler`: (->void) -> void

##register get_anchor : -> string
  ##args()
  {
    return window.location.hash;
  }

##register set_anchor : string -> void
  ##args(str)
  {
    window.location.hash = str;
  }
