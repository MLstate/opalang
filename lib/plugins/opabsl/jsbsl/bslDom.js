/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

//jQuery-based implementation of the functions behind dom.opa

/** @externType Dom.private.element */
/** @externType Dom.dimensions */
/** @externType Dom.event_handler */
   //Implemented as a [string], containing a random name for the handler

/** @externType Css.compiled_property */
   //Implemented as a record {name: string; value:string}, in css.opa

/** @externType Dom.private.selected */

/**
 * Import a low-level Dom node to a jQuery value.
 *
 * @register {Dom.private.element -> Dom.private.element} import
 */
function dom_import(value) {
    return (new $(value))
}

/**
 * @register { -> Dom.private.element}
 */
function select_nothing() {
    return (new $([]))
}

/**
 * @register { -> Dom.private.element}
 */
function select_all() {
    return (new $("*"))
}

/**
 * @register { -> Dom.private.element}
 */
function select_document() {
    return (new $(document))
}

/**
 * @register { -> Dom.private.element}
 */
function select_body() {
    return (new $(document.body))
}

/**
 * @register { -> Dom.private.element}
 */
function select_window() {
    return(new  $(window))
}

/**
 * @register {string -> Dom.private.element}
 */
function select_id(name) {
    return (new $(document.getElementById(name)))
}

/**
 * @register {string -> Dom.private.element}
 */
function select_class(name) {
    return (new $("."+name));
}

/**
 * @register {string -> Dom.private.element}
 */
function select_css(name) {
    return (new $(name));
}

/**
 * @register {string -> Dom.private.element}
 */
function select_tag(name) {
    return (new $(document.getElementsByTagName(name)));
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_children(dom) {
    return dom.children()
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_contents(dom) {
    return dom.contents()
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_parent(dom) {
    return dom.parent()
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_all_in(dom) {
    return dom.find("*");
}

/**
 * @register {string,Dom.private.element -> Dom.private.element}
 */
function select_id_in(name, dom) {
    return dom.find("#"+name);
}

/**
 * @register {string,Dom.private.element -> Dom.private.element}
 */
function select_class_in(name, dom) {
    return dom.find("."+name);
}

/**
 * @register {string,Dom.private.element -> Dom.private.element}
 */
function select_tag_in(name, dom) {
    return dom.find(name);
}

/**
 * @register {string,Dom.private.element -> Dom.private.element}
 */
function select_css_in(selector, dom) {
    return dom.find(selector);
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_previous_several(dom) {
    return dom.prevAll();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_next_several(dom) {
    return dom.nextAll();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_previous_one(dom) {
    return dom.prev();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_next_one(dom) {
    return dom.next();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_last_one(dom) {
    return dom.last();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_first_one(dom) {
    return dom.first();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_parent_one(dom) {
    return dom.parent();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_siblings(dom) {
    return dom.siblings();
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function select_parent_several(dom) {
    return dom.parents();
}

/**
 * @register {Dom.private.element -> void}
 */
function select(dom) {
    dom.select();
    return js_void;
}

/**
 * @register {opa[list(Dom.private.element)] -> Dom.private.element}
 */
function compose(dom) {
    var jq_list      = list2js(dom);
    var item_list    = [];
    var i;
    var len = jq_list.length;
    for(i = 0; i < len; ++i)
    {
        var current = jq_list[i].get();
        var curlen  = current.length;
        for(var j = 0; j < curlen; ++j)
            item_list.push(current[j]);
    }
    return (new $(item_list));
}

/**
 * @register {Dom.private.element, int -> Dom.private.element}
 */
function at(dom, i) {
    return dom.eq(i)
}

/**
 * @register {Dom.private.element, int, int -> Dom.private.element} sub
 */
function dom_sub(dom, start, stop) {
    return dom.slice(start, stop)
}

/**
 * @register {Dom.private.element -> opa[list(Dom.private.element)]}
 */
function decompose(dom) {
    var raw_array = dom.toArray();
    var len       = raw_array.length;
    var dst_array = new Array(len);
    var i;
    for(i = 0; i < len; ++i)
        dst_array[i] = (new $(raw_array[i]));//Ensure that this is a well-formed jQuery
    return js2list(dst_array);
}

/**
 * @register {Dom.private.element -> int} length
 */
function dom_length(dom) {
    return dom.size()
}

/**
 * @register {Dom.private.element -> bool} is_empty
 */
function dom_is_empty(dom) {
    return (dom.size() == 0);
}

/**
 * @register {Dom.private.element, string -> bool}
 */
function contains_selector(dom, selector) {
    return dom.is(selector)
}

/**
 * @register {Dom.private.element -> string}
 */
function get_id(dom) {

    var id = dom.prop("id");
    if(id == null)
    {
        id = new String(Math.floor(Math.random()*0xFFFFFFFF))
        dom.prop("id", id)
    }
    return id
}

/**
 * @register {(Dom.private.element -> 'void), Dom.private.element -> void}
 */
function iter(f, dom) {
    var g = function(index, element) { f((new $(element))) }
    dom.each(g);
}


/**
 * @register {(Dom.private.element,'a -> 'a), 'a, Dom.private.element -> 'a}
 */
function fold(f, init, dom) {
    var acc = {value: init}//Create a reference
    var g = function(index, element) { acc.value = f((new $(element)), acc.value) }
    dom.each(g)
    return acc.value
}

/**
 * @register {(Dom.private.element -> 'void), Dom.private.element -> void}
 */
function iter_deep(f, dom) {
    iter(f, select_all_in(dom));
}

/**
 * @register {(Dom.private.element,'a -> 'a), 'a, Dom.private.element -> 'a}
 */
function fold_deep(f, init, dom) {
    return fold(f, init, select_all_in(dom))
}

/**
 * @register {Dom.private.element, Dom.private.element -> opa[option(int)]}
 */
function find(item, collection) {
    var result = collection.index(item)
    if(result == -1) return js_none
    return js_some(result)
}

/**
 * @register {Dom.private.element -> opa[option(int)]} index
 */
function dom_index(item) {
    var result = item.index()
    if(result == -1) return js_none
    return js_some(result)
}

/**
 * @register {Dom.private.element, Dom.private.element -> Dom.private.element} append
 */
function dom_append(to, item) {
    var result = to.append(item);
    BslClientOnly_Dom_flush_all(to);
    BslClientOnly_Dom_flush_all(item);
    return result;
}

/**
 * @register {Dom.private.element, Dom.private.element -> Dom.private.element}
 */
function prepend(to, item) {
    var result = to.prepend(item);
    BslClientOnly_Dom_flush_all(to);
    BslClientOnly_Dom_flush_all(item);
    return result;
}

/**
 * @register {Dom.private.element, Dom.private.element -> Dom.private.element}
 */
function before(to, item) {
    var result = to.before(item);
    BslClientOnly_Dom_flush_all(to);
    BslClientOnly_Dom_flush_all(item);
    return result;
}

/**
 * @register {Dom.private.element, Dom.private.element -> Dom.private.element}
 */
function after(to, item) {
    var result = to.after(item);
    BslClientOnly_Dom_flush_all(to);
    BslClientOnly_Dom_flush_all(item);
    return result;
}

/**
 * @register {Dom.private.element, Dom.private.element -> Dom.private.element} replace
 */
function dom_replace(to, item) {
    var result = to.replaceWith(item);
    BslClientOnly_Dom_flush_all(to);
    BslClientOnly_Dom_flush_all(item);
    return result;
}

/**
 * @register {Dom.private.element, Dom.private.element -> Dom.private.element}
 */
function replace_contents(to, item) {
    to.empty();
    var result = to.append(item);
    BslClientOnly_Dom_flush_all(to);
    BslClientOnly_Dom_flush_all(item);
    return result;
}

/**
 * @register {Dom.private.element -> void}
 */
function remove(dom) {
    dom.empty();//Implementation note: faster than calling directly [dom.remove()]
    dom.remove();
}

/**
 * @register {Dom.private.element -> void}
 */
function detach(dom) {
    return dom.detach()
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function give_focus(dom) {
    dom.focus();
    return dom;
}

/**
 * @register {Dom.private.element -> Dom.private.element}
 */
function give_blur(dom) {
    dom.blur();
    return dom;
}

/**
 * @register {Dom.private.element -> string}
 */
function get_value(dom) {
    var result = { value: "" }
    var f = function(node) {
        var value = dom.val()
        if(value == "" || value == null) return;
        result.value = result.value + value;
    }
    dom.each(f);
    return result.value
}

/**
 * @register {Dom.private.element, string -> void}
 */
function set_value(dom, value) {
    dom.val(value);
}

/**
 * @register {Dom.private.element -> string}
 */
function get_text(dom) {
    return dom.text()
}

/**
 * @register {Dom.private.element, string -> void}
 */
function set_text(dom, text) {
    dom.text(text)
}

/**
 * @register {Dom.private.element,bool -> void}
 */
function set_enabled(dom, v) {
    dom.prop("disabled", !v);
}

/**
 * @register {Dom.private.element -> bool}
 */
function get_enabled(dom) {
    if(dom.prop("disabled")) // if undefined, return true
        return false
    else return true;
}

/**
 * @register {Dom.private.element -> string}
 */
function get_content(dom) {
    var value = dom.val()
    if(value == "") return dom.text()
    return value;
}

/**
 * @register {Dom.private.element, string -> void}
 */
function set_html(dom, content) {
    dom.html(content)
}

/**
 * @register {Dom.private.element -> void}
 */
function remove_content(dom) {
    dom.empty()
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_offset(dom) {
    var offset = dom.offset();
    return normalize_obj({x_px: Math.round(offset.left || 0), y_px: Math.round(offset.top || 0)})
}

/**
 * @register {Dom.private.element, Dom.dimensions -> void}
 */
function set_offset(dom, offset) {
    dom.offset({top: offset.y_px, left:offset.x_px})
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_position(dom) {
    var position = dom.position();
    return normalize_obj({x_px: Math.round(position.left || 0), y_px: Math.round(position.top || 0)})
}

/**
 * @register {Dom.private.element, Dom.dimensions -> void}
 */
function set_position(dom, position) {
    dom.position({top: position.y_px, left:position.x_px})
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_size(dom) {
    return normalize_obj({x_px: Math.round(dom.width() || 0), y_px: Math.round(dom.height() || 0)})
}

/**
 * @register {Dom.private.element, int -> void}
 */
function set_width(dom, x) {
    dom.width(x);
}

/**
 * @register {Dom.private.element, int -> void}
 */
function set_height(dom, x) {
    dom.height(x);
}

/**
 * @register {Dom.private.element, Dom.dimensions -> void}
 */
function set_size(dom, size) {
    dom.width(size.x_px);
    dom.height(size.y_px);
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_inner_size(dom) {
    return {x_px: dom.innerWidth() || 0, y_px: dom.innerHeight() || 0}
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_outer_size(dom) {
    return {x_px: dom.outerWidth() ||0, y_px: dom.outerHeight() || 0}
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_scrollable_size(dom) {
    var x_px  = 0;
    var y_px  = 0;
    var items = dom.get();
    var len = items.length;
    for(var i = 0; i < len; ++i)
    {
        var current = items[i];
        x_px = Math.max(x_px, current.scrollWidth);
        y_px = Math.max(y_px, current.scrollHeight);
    }
    return {x_px: x_px, y_px: y_px};
}

/**
 * @register {Dom.private.element, Dom.dimensions -> void}
 */
function set_scroll(dom, dimensions) {
    dom.scrollLeft(dimensions.x_px)
    dom.scrollTop(dimensions.y_px)
}

/**
 * @register {Dom.private.element -> Dom.dimensions}
 */
function get_scroll(dom) {
    return {x_px: dom.scrollLeft() || 0, y_px: dom.scrollTop() || 0}
}


/**
 * @register {Dom.private.element, string, (Dom.event -> void) -> Dom.event_handler} bind
 */
function bom_bind(dom, event, f) {
    return dom.opabind(event, f, null, false, false);
}

/**
 * @register {Dom.private.element, string, (Dom.event -> void), opa[option(Dom.event -> Dom.event_propagation)], bool, bool -> Dom.event_handler}
 */
function bind_with_options(dom, event, f, p_opt, stop_propagation, prevent_default) {
    var p = option2js(p_opt);
    return dom.opabind(event, f, p, stop_propagation, prevent_default);
}

/** @register {Dom.private.element, string -> void} unbind_event bsldom_unbind */
/** @register {Dom.private.element,Dom.event_handler -> void} unbind bsldom_unbind */
function bsldom_unbind(dom, handler)
{
    dom.unbind(handler);
}

/**
 * @register {Dom.private.element, string -> void}
 */
function trigger(dom, event_kind) {
    dom.trigger(event_kind);
}

/**
 * @register {Dom.private.element, Dom.event, string -> void}
 */
function trigger_event(dom, event, name) {
    var dom_event = opa_event_to_dom_event(event, name);
    dom.trigger(dom_event);
}

/**
 * @register {Dom.private.element, string, string -> void}
 */
function set_property_unsafe(dom, name, value) {
    dom.prop(name, value);
}

/**
 * @register {Dom.private.element, string, string -> void}
 */
function set_attribute_unsafe(dom, name, value) {
    dom.attr(name, value);
}

/**
 * @register {Dom.private.element, string -> void}
 */
function remove_attribute(dom, name) {
    dom.removeAttr(name);
}

/**
 * @register {Dom.private.element, string, string -> void}
 */
function set_style_property_unsafe(dom, name, value) {
    dom.css(name, value);
}

/**
 * @register {Dom.private.element -> void}
 */
function void_style(dom) {
  var items = dom.get();
  var length = items.length;
  var style;
  var i;
  for(i = 0; i < length; ++i)
    if (style=items[i].style) style.cssText = "";
}

/**
 * @return {boolean} [true] if at least one element has attribute 'checked', [false] otherwise
 */
/**
 * @register {Dom.private.element -> bool}
 */
function is_checked(dom) {
   var items  = dom.get();
   var length = items.length;
   var i;
   for(i = 0; i < length; ++i)
      if (items[i].checked) return true;
   return false;
}

/**
 * @register {Dom.private.element, bool -> void}
 */
function set_checked(dom, v) {
   var items  = dom.get();
   var length = items.length;
   var i;
   for(i = 0; i < length; ++i)
       items[i].checked = v;
}

/**
 * @register {Dom.private.element, string -> string}
 */
function get_style_property_unsafe(dom, name) {
    return dom.css(name) || "";
}

/**
 * @register {Dom.private.element, string -> opa[option(string)]}
 */
function get_property(dom, name) {
    return js2option(dom.prop(name))
}

/**
 * @register {Dom.private.element, string -> string}
 */
function get_property_unsafe(dom, name) {
    return dom.prop(name) || ""
}

/**
 * @register {Dom.private.element, string -> opa[option(string)]}
 */
function get_attribute(dom, name) {
    return js2option(dom.attr(name))
}

/**
 * @register {Dom.private.element, string -> string}
 */
function get_attribute_unsafe(dom, name) {
    return dom.attr(name) || ""
}

/**
 * @register {Dom.private.element, string -> void}
 */
function add_class(dom, name) {
    dom.addClass(name);
}

/**
 * @register {Dom.private.element, string -> void}
 */
function remove_class(dom, name) {
    dom.removeClass(name);
}

/**
 * @register {Dom.private.element, string -> bool}
 */
function has_class(dom, name) {
    return dom.hasClass(name)
}

/**
 * @register {Dom.private.element, string -> void}
 */
function set_class(dom, name) {
    dom.removeClass();
    dom.addClass(name);
}

/**
 * @register {Dom.private.element -> void}
 */
function void_class(dom) {
    dom.removeClass();
}


/**
 * @register {Dom.private.element,string -> void}
 */
function toggle_class(dom, name) {
    dom.toggleClass(name);
}

//Export a callback
function export_cb(a){
  var undefined;
  return a ? function () { a(this) } : undefined;
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_fade_in(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.fadeIn.apply(dom, args);
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_fade_out(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.fadeOut.apply(dom, args);
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_fade_toggle(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.fadeToggle.apply(dom, args);
}

// Mathieu Tue Feb  1 19:41:34 CET 2011
// FIXME:Why do we need 2 bypass there ?
/** @register { Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void} do_show bsldom_do_slide_in */
/** @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void} do_slide_in bsldom_do_slide_in */
function bsldom_do_slide_in(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.slideDown.apply(dom, args);
}

// Mathieu Tue Feb  1 19:41:44 CET 2011
// FIXME:Why do we need 2 bypass there ?
/** @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void} do_hide bsldom_do_slide_out */
/** @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void} do_slide_out bsldom_do_slide_out */
function bsldom_do_slide_out(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.slideUp.apply(dom, args);
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_slide_toggle(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.slideToggle.apply(dom, args);
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_toggle(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.toggle.apply(dom, args);
}

/**
 * @register {opa[list(Css.compiled_property)] -> (Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void)}
 */
function do_change_to(properties) {
    var list = list2js(properties);
    var map  = {}
    var i;
    while(i = list.pop()) map[i.name] = i.value;
    var f = function(dom, maybe_duration, maybe_easing, maybe_cb){
      var easing= option2jsu(maybe_easing);
      var dur   = option2jsu(maybe_duration);
      var cb    = export_cb(option2jsu(maybe_cb));
      var args = [map, dur, easing, cb]
      dom.animate.apply(dom, args);
    }
    return f
}

/**
 * @register {opa[list(Css.compiled_property)] -> (Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void)}
 */
function do_fade_to(properties) {
    var list = list2js(properties);
    var map  = {}
    var i;
    while(i = list.pop()) map[i.name] = i.value;
    var f = function(dom, maybe_duration, maybe_easing, maybe_cb){
      var easing= option2jsu(maybe_easing);
      var dur   = option2jsu(maybe_duration);
      var cb    = export_cb(option2jsu(maybe_cb));
      var args = [map, dur, easing, cb]
      dom.fadeTo.apply(dom, args);
    }
    return f
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_scroll_to_bottom(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [{scrollTop: get_scrollable_size(dom).y_px }, dur, easing, cb]
    dom.animate.apply(dom, args);
}

/**
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_scroll_to_right(dom, maybe_duration, maybe_easing, maybe_cb) {
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [{scrollLeft: get_scrollable_size(dom).x_px }, dur, easing, cb]
    dom.animate.apply(dom, args);
}

/**
 * @param {?Function} maybe_cb
 *
 * @register {Dom.private.element, opa[option(int)], opa[option(string)], opa[option(Dom.private.element -> void)] -> void}
 */
function do_wait(_, maybe_duration, _2, maybe_cb) {
    /**@type {?Function}*/
    var cb   = option2js(maybe_cb);
    if(cb == null) return;//No callback? Do nothing!
    var x    = option2js(maybe_duration);
    /**@type {number}*/
    var dur  = x||400
    setTimeout(cb, dur);
}

/**
 * @register {Dom.private.element -> void} stop
 */
function dom_stop(dom) {
   dom.stop(true, false);
}

/**
 * @register {-> Dom.private.selected}
 */
function get_selection_range() {
   //Under everything but IE
   if(window.getSelection)
   {
       var selection = window.getSelection();
       return normalize_obj({
          start_in:   (new $(selection.anchorNode)),
          start_at:     selection.anchorOffset,
          finish_in:  (new $(selection.focusNode)),
          finish_at:    selection.focusOffset,
          is_collapsed: selection.isCollapsed
       })
   }
   //Under IE, nothing works
   var ds = document.selection;
   if(ds.type == 'None' || ds.type == 'Control')
   {
       return normalize_obj({
           start_in:     (new $(document)),
           start_at:     0,
           finish_in:    (new $(document)),
           finish_at:    0,
           is_collapsed: true
       })
   } else {//Need to rebuild the information -- thanks, IE.
       var range          = ds.createRange();
       var parent_range   = ds.parentElement().createTextRange();
       var offset         = 0;
       var start_left     = range.offsetLeft;
       var start_top      = range.offsetTop;
       while((parent_range.offsetLeft != start_left)
          && (parent_range.offsetTop  != start_top)) {
         ++offset
         parent_range.move('character', 1)
       }
       var length = range.text.length;
       return normalize_obj({
           start_in:     document.elementFromPoint(range.boundingLeft, range.boundingTop),
           finish_in:    document.elementFromPoint(range.boundingLeft + range.boundingWidth, range.boundingTop + range.boundingHeight),
           start_at:     offset,
           finish_at:    offset + length,
           is_collapsed: length == 0
       })
   }
}

/**
 * @register {(Dom.event -> opa[option(string)]) -> void}
 */
function bind_unload_confirmation(cb) {
   function f(e)
   {
      var ev = e || window.event; //Everybody but IE / IE
      var opa_event = dom_event_to_opa_event(ev);
      var prompt = option2js(cb(opa_event));
      if(prompt == null) return;  //Don't display a prompt
      alert(prompt);
   }
   (new $(window).unload(f));
}

/**
 * @register {(Dom.event -> opa[option(string)]) -> void}
 */
function bind_beforeunload_confirmation(cb) {
   function f(e)
   {
      var ev = e || window.event; //Everybody but IE / IE
      var opa_event = dom_event_to_opa_event(ev);
      var prompt = option2js(cb(opa_event));
      if(prompt == null) return;  //Don't display a prompt
      ev.returnValue = prompt;    //For IE, FF
      return prompt;              //For Safari
   }
    (new $(window)).bind('beforeunload', f);
}

/**
 * @register {string,string,string -> void}
 */
function notification(img_url,title,body) {
    if(window.webkitNotifications){
        function RequestPermission (callback) { window.webkitNotifications.requestPermission(callback); }
        function showNotification(){
            if (window.webkitNotifications.checkPermission() > 0) {
                RequestPermission(showNotification);
            } else {
                window.webkitNotifications.createNotification(img_url,title,body).show();
            }
        }
        showNotification()
            };
}
