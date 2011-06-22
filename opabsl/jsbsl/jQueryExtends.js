
/**
 * Initialize CSS hooks to patch broken -ms-transform, -ms-transform-origin properties
 */
(function($){
    if ( !$.cssHooks ){
        alert("jQuery 1.4.3+ is needed for this plugin to work");
        return;
    }
var div = document.createElement( "div" );

//-moz-transform-origin / -webkit-transform-origin / -ms-transform-origin
    $.support.transformOrigin =
    div.style.MozTransformOrigin     === '' ? 'MozTransformOrigin' :
    (div.style.WebkitTransformOrigin === '' ? 'WebkitTransformOrigin' :
    (div.style.msTransformOrigin     === '' ? 'msTransformOrigin' :
    (div.style.transformOrigin       === '' ? 'TransformOrigin' : false)));

    if ( $.support.transformOrigin && $.support.transformOrigin !== "TransformOrigin" ){
        $.cssHooks.transformOrigin = {
            get: function( elem, computed, extra ) {
                return $.css(elem, $.support.transformOrigin);
            },
            set: function( elem, value ) {
                elem.style[$.support.transformOrigin] = value;
            }
        };
    }

//-moz-transform / -webkit-transform / -ms-transform
    $.support.transform =
    div.style.MozTransform     === '' ? 'MozTransform' :
    (div.style.WebkitTransform === '' ? 'WebkitTransform' :
    (div.style.msTransform     === '' ? 'msTransform' :
    (div.style.transform       === '' ? 'Transform' : false)));

    if ( $.support.transform && $.support.transform !== "Transform" ){
        $.cssHooks.transform = {
            get: function( elem, computed, extra ) {
                return $.css(elem, $.support.transform);
            },
            set: function( elem, value ) {
                elem.style[$.support.transform] = value;
            }
        };
    }

    div = null;
})(jQuery);


// Opa overload replacement
(function($){
    var jq_bind   = $.fn.bind;
    var jq_unbind = $.fn.unbind;
    var jq_keydown= $.fn.keydown;
    var jq_keypress=$.fn.keypress;

$.fn.extend({

    /**
     * Behaves as jQuery [keydown] but attempts to normalize key repeat for exotic keys (e.g. arrow keys)
     *
     * Standard FF behavior: pressing an arrow key and letting it down first triggers a [keydown], then any number of *empty* [keypress], then a [keyup]
     * Standard Safari/Chrome/Opera behavior: pressing an arrow key and letting it down triggers a [keydown], then additional [keydown], then a [keyup]
     * This function (almost) convinces FF to have the same behavior as other navigators: note that we cannot completely suppress the empty [keypress]
     */
    keydown:function(fn)
    {
        if(fn)
        {
            var me = this;

            /**
             * A function triggered on [keydown].
             *
             * Remember the latest [keydown] as [this._latest_keydown], then call [fn].
             */
            var onkeydown = function(event)
            {
                me._latest_keydown = event.which;
                me._trigger_keydown = false; // Flag to avoid triggering a second 'keydown' on the first keypress
                return fn(event);
            }

            /**
             * A function triggered on [keypress].
             *
             * If this [keypress] is empty and if we're between a [keydown] and a [keyup], then stop event and rather trigger a [keydown],
             * with the key pressed during the latest [keydown].
             */
            var onkeypress = function(event)
            {
                if((!event.which) && me._latest_keydown) //If there is no key, but we're between a keydown and a keyup, this is a FF bug/oddity
                {
                    if(me._trigger_keydown) // Check if it's the first keypress in order to avoid multiple keydown triggering at once
                    {
                        event.type  = "keydown";
                        event.which = me._latest_keydown;      //...we're actually repeating a keydown as a keypress (it's a FF oddity)
                        event.stopPropagation();
                        return me.trigger(event);
                    } else {
                        me._trigger_keydown = true; // From now on, trigger the last keydown on each keypress
                    }
                }
            }

            /**
             * A function triggered on [keyup]
             *
             * Forget the latest [keydown]
             */
            var onkeyup = function(event)
            {
                me._latest_keydown = null;
            }

            jq_bind.apply(this, ["keyup",          onkeyup]);
            jq_bind.apply(this, ["keypress",       onkeypress]);
            return jq_bind.apply(this, ["keydown", onkeydown]);
        } else {
            return this.trigger("keydown");
        }
    },


    //On new line
    newline:function(fn)
    {
        if(fn)
        {
            var f = function(event)
            {
                if(event.keyCode == 13)
                {
                    return fn(event)
                }
            }
            jq_bind.apply(this, ["keypress.newline", f]);
            jq_bind.apply(this, ["newline", f]);
            return this;
        } else {
            return this.trigger("newline");
        }
    },

    //On "esc"
    keyesc:function(fn)
    {
        if(fn)
        {
            var f = function(event)
            {
                if(event.keyCode == 27)
                {
                    return fn(event)
                }
            }
            jq_bind.apply(this, ["keypress.keyesc", f]);
            jq_bind.apply(this, ["keyup.keyesc",    f]);
            jq_bind.apply(this, ["keydown.keyesc",  f]);
            jq_bind.apply(this, ["keyesc",          f]);
            return this;
        } else {
            return this.trigger("keyesc");
        }
    },

    /**
     * Chainable version of [opabind]
     */
    opachbind:function(name, fn)
    {
        function f(e)
        {
            return fn(dom_event_to_opa_event(e))
        }
        return this.bind(name, f)
    },

    /**
     * Bind an event handler to an event
     *
     * @param {string} name The name of the event (e.g. "click")
     * @param {function(Object)} fn The event handler
     * @return {string} An abstract value which can be passed to [opaunbind] for unbinding at a later stage.
     */
    opabind:function(name, fn)
    {
        var ns = name + "." + Math.random()
        function f(e)
        {
            return fn(dom_event_to_opa_event(e))
        }
        this.special_bind(name, ns, f)
        return ns
    },

    opaunbind:function(name, fn)
    {
        return this.unbind(name, fn)
    },

    special_bind:function(name, ns, fn, data)
    {
       switch(name)
       {
           case "keydown":            return this.keydown(fn);
           case "keydown.keyesc":     return this.keyesc(fn);
           case "keydown.newline":    return this.newline(fn);
           case "mousewheel":         return this.mousewheel(fn);
           default:                   return jq_bind.apply(this, [ns, data, fn]);
       }
    },

    bind:function(name, data, fn)
    {
       if ( arguments.length === 2 || data === false ) {
         fn = data;
         data = undefined;
       }
       return this.special_bind(name, name, fn, data);
    },

    unbind:function(name, fn)
    {
       switch(name)
       {
           case "keyesc":
           jq_unbind.apply(this, [name, fn]);
           jq_unbind.apply(this, ["keypress.keyesc", fn]);
           jq_unbind.apply(this, ["keyup.keyesc",    fn]);
           jq_unbind.apply(this, ["keydown.keyesc",  fn]);
           return this;
           break;
           case "newline":
           jq_unbind.apply(this, [name, fn]);
           jq_unbind.apply(this, ["keypress.newline", fn]);
           return this;
           break;
           default:
           return jq_unbind.apply(this, [name, fn]);
       }
    },

    opa_do_not_normalize: true//[jQuery] is a system object and should not be normalized by [normalize_obj]
}) })(jQuery);

/**
 * {2 Conversion between JS events and OPA events}
 */

/**
 * The representation of keypress [alt] in an event
 */
var shortcut_key_alt   = make_simple_record("alt");

/**
 * The representation of keypress [ctrl] in an event
 */
var shortcut_key_ctrl  = make_simple_record("ctrl");

/**
 * The representation of keypress [meta] in an event
 */
var shortcut_key_meta  = make_simple_record("meta");

/**
 * The representation of keypress [shift] in an event
 */
var shortcut_key_shift = make_simple_record("shift");

/**
 * The representation of keypress [left] in an event
 */
var shortcut_button_left   = make_simple_record("left");

/**
 * The representation of keypress [middle] in an event
 */
var shortcut_button_middle   = make_simple_record("middle");

/**
 * The representation of keypress [right] in an event
 */
var shortcut_button_right   = make_simple_record("right");

/**
 * A hash-table for converting names of well-known events into the corresponding OPA data structure DOM.Event.kind
 */
var event_kind_table = {
      click      : make_simple_record("click"),
      mouseup    : make_simple_record("mouseup"),
      mousedown  : make_simple_record("mousedown"),
      mouseover  : make_simple_record("mouseover"),
      mouseout   : make_simple_record("mouseout"),
      mousemove  : make_simple_record("mousemove"),
      mouseenter : make_simple_record("mouseenter"),
      mouseleave : make_simple_record("mouseleave"),
      dblclick   : make_simple_record("dblclick"),
      keypress   : make_simple_record("keypress"),
      keydown    : make_simple_record("keydown"),
      keyup      : make_simple_record("keyup"),
      load       : make_simple_record("load"),
      unload     : make_simple_record("unload"),
      error      : make_simple_record("error"),
      select     : make_simple_record("select"),
      submit     : make_simple_record("submit"),
      focus      : make_simple_record("focus"),
      blur       : make_simple_record("blur"),
      mousewheel : make_simple_record("mousewheel"),
      scroll     : make_simple_record("scroll"),
      change     : make_simple_record("change"),
      resize     : make_simple_record("resize"),
      newline    : make_simple_record("newline"),
      keyesc     : make_simple_record("keyesc")
}
event_kind_table["keydown.newline"] = event_kind_table.newline
event_kind_table["keydown.keyesc"] = event_kind_table.keyesc

/**
 * A default event, substituted when receiving empty DOM events
 * Beware, it is part of the JsInterface, which means that
 * you cannot change its name.
 * @jsinterface
 */
var default_opa_event =
    normalize_obj(
{
    kind: {custom : "none"},
    mouse_position_on_page: {x_px: 0, y_px:0},
    key_code: js_none,
    mouse_button: js_none,
    key_modifiers: {nil: js_void},
    value_change: js_none
})

/**
 * Convert a JS-level DOM event to a OPA representation.
 *
 * @param e A jQuery event
 */
function dom_event_to_opa_event(e)
{
    var cons = empty_constructor();
    if(e==null)//Special case, upon [onload]/[onready]
        return default_opa_event;

    //1. Handle mouse position
    var page_x = e.pageX
    var page_y = e.pageY
    if(page_x==null) page_x = 0;
    if(page_y==null) page_y = 0;
    var mouse_position_on_page = normalize_obj({x_px: page_x, y_px: page_y});
    add_field(cons, "mouse_position_on_page", mouse_position_on_page);


    //2. Handle key and button pressed
    var js_which               = e.which;
    //Here, we need to guess. We'll assume that keys 1-3 are mouse buttons, while other ones are keyboard events
    //Yes, it's ugly.

    var key_code;
    var mouse_button;

    if(isNaN(Number(js_which)) || js_which == null || js_which <= 0)
    {
        key_code     = js_none;
        mouse_button = null;   //Note: We'll normalize this when we have checked if a wheelDelta is present
    } else {
        switch(js_which)
        {
            case 1:
            key_code     = js_none;
            mouse_button = js_some(shortcut_button_left);
            break;
            case 2:
            key_code     = js_none;
            mouse_button = js_some(shortcut_button_right);
            break;
            case 3:
            key_code     = js_none;
            mouse_button = js_some(shortcut_button_middle);
            break;
            default:
            key_code     = js_some(js_which);
            mouse_button = null;//Note: We'll normalize this when we have checked if a wheelDelta is present
        }
    }
    if(mouse_button == null)
    {
        var delta = e.wheelDelta
        var detail= Number(e.detail)
        if(delta)
            mouse_button = js_some(make_onefield_record("wheel", delta / 120))
        else if (!isNaN(detail))
            mouse_button = js_some(make_onefield_record("wheel", - detail / 3))
        else
            mouse_button = js_none;
    }

    add_field(cons, "key_code",               key_code);
    add_field(cons, "mouse_button",           mouse_button);

    //3. Handle key modifiers
    var js_key_modifiers       = [];
    if(e.altKey) js_key_modifiers.push(shortcut_key_alt);
    if(e.ctrlKey)js_key_modifiers.push(shortcut_key_ctrl);
    if(e.metaKey)js_key_modifiers.push(shortcut_key_meta);
    if(e.shiftKey)js_key_modifiers.push(shortcut_key_shift);
    var key_modifiers          = js2list(js_key_modifiers);
    add_field(cons, "key_modifiers",          key_modifiers);

    //4. Handle value change
    var js_old_value = e.prevValue;
    var js_new_value = e.newValue;
    if(js_old_value == null && js_new_value == null)
    {
        add_field(cons, "value_change", js_none);
    } else {
        js_old_value = ""+js_old_value;//Normalize, ensuring we never deal with a null string
        js_new_value = ""+js_new_value;
        var cons2 = empty_constructor();
        add_field(cons2, "from", js_old_value);
        add_field(cons2, "to",   js_new_value);

        add_field(cons, "value_change", js_some(make_record(cons2)));
    }

    //5. Handle event kind
    var type = e.type;
    var possible_kind = event_kind_table[type]
    add_field(cons, "kind", possible_kind?possible_kind:make_onefield_record("custom", "none"));

    //Return result
    return make_record(cons);
}

/**
 * Convert a OPA-level DOM event to a JS representation.
 *
 * @param {Object} event A OPA event
 * @param {string} name The jQuery name of the event, as obtained from OPA module Dom.Event
 * @return {jQuery.Event}
 */
function opa_event_to_dom_event(event, name)
{
    var record = record2obj(event);
    var result = new jQuery.Event(name) ;

    //1. Handle mouse position
    var mouse_position_on_page = record2obj(record.mouse_position_on_page);
    result.pageX = mouse_position_on_page.x_px;
    result.pageY = mouse_position_on_page.y_px;

    //2. Handle key and button pressed
    var key_code     = record.key_code;
    var mouse_button = record.mouse_button;
    var some_keycode = udot(key_code, "some");
    if(some_keycode == null)
    {
        var maybe_mb = record2obj(mouse_button);
        if(maybe_mb.none)
            result.which = null;
        else
        {
            var some_mb = record2obj(maybe_mb.some);
            //Only 4 possibilities
            if(some_mb.left) result.which = 1;
            else if(some_mb.middle) result.which = 3;
            else if(some_mb.right)  result.which = 2;
            else
            {
                var delta = some_mb.delta || 0;
                result.wheelDelta = delta * 120;
                result.detail     = - delta * 3;
            }
        }
    } else {
        result.which = record2obj(key_code).some;
    }

    //3. Handle key modifiers
    var js_key_modifiers = list2js(record.key_modifiers);
    for(var i = 0, len = js_key_modifiers.length;
            i < len;
            ++i)
    {
        switch(js_key_modifiers[i])
        {
        case "alt":   result.altKey   = true; break;
        case "ctrl":  result.ctrlKey  = true; break;
        case "meta":  result.metaKey  = true; break;
        case "shift": result.shiftKey = true; break;
        }
    }


    //4. Handle value change
    var value_change = record2obj(record.value_change);
    if(value_change.none != null)
    {
        var change = record2obj(value_change.some);
        result.prevVal = change.from;
        result.newVal  = change.to;
    }

    return result;
}

/* Looks like it is not used anymore.
function tn_encaps(tn, fun){
    var a = (new $(document.createElement('span')));
  a.append(tn[0].data);
  tn.myrplce(a);
  var ret = fun(a);
  a.myrplce(tn);
  return ret;
}
*/

// jQuery Plugins

/*! Copyright (c) 2009 Brandon Aaron (http://brandonaaron.net)
 * Dual licensed under the MIT (http://www.opensource.org/licenses/mit-license.php)
 * and GPL (http://www.opensource.org/licenses/gpl-license.php) licenses.
 * Thanks to: http://adomas.org/javascript-mouse-wheel/ for some pointers.
 * Thanks to: Mathias Bank(http://www.mathias-bank.de) for a scope bug fix.
 *
 * Version: 3.0.2
 *
 * Requires: 1.2.2+
 */

(function ($) {
  var types = ['DOMMouseScroll', 'mousewheel'];
  function handler (event) {
    var args = [].slice.call(arguments, 1), delta = 0, returnValue = true;
    event = $.event.fix(event || window.event);
    event.type = "mousewheel";
    if (event.wheelDelta) delta = event.wheelDelta / 120;
    if (event.detail) delta = - event.detail / 3;
    args.unshift(event, delta);
    event.wheelDelta = delta; // hack
    return $.event.handle.apply(this, args); }
  $.event.special.mousewheel = {
    setup: function () {
      if (this.addEventListener) for (var i = types.length; i; ) this.addEventListener(types[--i], handler, false);
      else this.onmousewheel = handler; },
    teardown: function () {
      if (this.removeEventListener) for (var i = types.length; i; ) this.removeEventListener(types[--i], handler, false);
      else this.onmousewheel = null; }};
  $.fn.extend({
    mousewheel: function (fn) { return fn ? this.bind("mousewheel", fn) : this.trigger("mousewheel"); },
    unmousewheel: function (fn) { return this.unbind("mousewheel", fn); }});
})(jQuery);
