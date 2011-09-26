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

//jQuery-based implementation of the functions behind dom.opa

##extern-type Dom.private.element
##extern-type Dom.dimensions
##extern-type Dom.event_handler
   //Implemented as a [string], containing a random name for the handler

##extern-type Css.compiled_property
   //Implemented as a record {name: string; value:string}, in css.opa

##extern-type Dom.private.selected

/**
 * Import a low-level Dom node to a jQuery value.
 */
##register import: Dom.private.element -> Dom.private.element
##args(value)
{
    return (new $(value))
}

##register select_nothing: -> Dom.private.element
##args()
{
    return (new $([]))
}

##register select_all: -> Dom.private.element
##args()
{
    return (new $("*"))
}

##register select_document: -> Dom.private.element
##args()
{
    return (new $(document))
}

##register select_body: -> Dom.private.element
##args()
{
    return (new $(document.body))
}

##register select_window: -> Dom.private.element
##args()
{
    return(new  $(window))
}

##register select_id: string -> Dom.private.element
##args(name)
{
    return (new $(document.getElementById(name)))
}

##register select_class: string -> Dom.private.element
##args(name)
{
    return (new $("."+name));
}

##register select_css: string -> Dom.private.element
##args(name)
{
    return (new $(name));
}

##register select_tag: string -> Dom.private.element
##args(name)
{
    return (new $(document.getElementsByTagName(name)));
}

##register select_children: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.children()
}

##register select_parent: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.parent()
}

##register select_all_in: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.find("*");
}

##register select_id_in: string,Dom.private.element -> Dom.private.element
##args(name, dom)
{
    return dom.find("#"+name);
}

##register select_class_in: string,Dom.private.element -> Dom.private.element
##args(name, dom)
{
    return dom.find("."+name);
}

##register select_tag_in: string,Dom.private.element -> Dom.private.element
##args(name, dom)
{
    return dom.find(name);
}

##register select_css_in: string,Dom.private.element -> Dom.private.element
##args(selector, dom)
{
    return dom.find(selector);
}

##register select_previous_several: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.prevAll();
}

##register select_next_several: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.nextAll();
}

##register select_previous_one: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.prev();
}

##register select_next_one: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.next();
}

##register select_last_one: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.last();
}

##register select_first_one: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.first();
}

##register select_parent_one: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.parent();
}

##register select_siblings: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.siblings();
}

##register select_parent_several: Dom.private.element -> Dom.private.element
##args(dom)
{
    return dom.parents();
}

##register compose: opa[list(Dom.private.element)] -> Dom.private.element
##args(dom)
{
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

##register at: Dom.private.element, int -> Dom.private.element
##args(dom, i)
{
    return dom.eq(i)
}

##register sub: Dom.private.element, int, int -> Dom.private.element
##args(dom, start, stop)
{
    return dom.slice(start, stop)
}

##register decompose: Dom.private.element -> opa[list(Dom.private.element)]
##args(dom)
{
    var raw_array = dom.toArray();
    var len       = raw_array.length;
    var dst_array = new Array(len);
    var i;
    for(i = 0; i < len; ++i)
        dst_array[i] = (new $(raw_array[i]));//Ensure that this is a well-formed jQuery
    return js2list(dst_array);
}

##register length: Dom.private.element -> int
##args(dom)
{
    return dom.size()
}

##register is_empty: Dom.private.element -> bool
##args(dom)
{
    return (dom.size() == 0);
}

##register contains_selector: Dom.private.element, string -> bool
##args(dom, selector)
{
    return dom.is(selector)
}

##register get_id: Dom.private.element -> string
##args(dom)
{

    var id = dom.prop("id");
    if(id == null)
    {
        id = new String(Math.floor(Math.random()*0xFFFFFFFF))
        dom.prop("id", id)
    }
    return id
}

##register iter: (Dom.private.element -> 'void), Dom.private.element -> void
##args(f, dom)
{
    var g = function(index, element) { f((new $(element))) }
    dom.each(g);
}


##register fold: (Dom.private.element,'a -> 'a), 'a, Dom.private.element -> 'a
##args(f, init, dom)
{
    var acc = {value: init}//Create a reference
    var g = function(index, element) { acc.value = f((new $(element)), acc.value) }
    dom.each(g)
    return acc.value
}

##register iter_deep: (Dom.private.element -> 'void), Dom.private.element -> void
##args(f, dom)
{
    %%BslDom.iter%%(f, %%BslDom.select_all_in%%(dom));
}

##register fold_deep: (Dom.private.element,'a -> 'a), 'a, Dom.private.element -> 'a
##args(f, init, dom)
{
    return %%BslDom.fold%%(f, init, %%BslDom.select_all_in%%(dom))
}

##register find: Dom.private.element, Dom.private.element -> opa[option(int)]
##args(item, collection)
{
    var result = collection.index(item)
    if(result == -1) return js_none
    return js_some(result)
}

##register index: Dom.private.element -> opa[option(int)]
##args(item)
{
    var result = item.index()
    if(result == -1) return js_none
    return js_some(result)
}

##register append: Dom.private.element, Dom.private.element -> Dom.private.element
##args(to, item)
{
    var result = to.append(item);
    (%%BslClientOnly.Dom.flush_all%%)(to);
    (%%BslClientOnly.Dom.flush_all%%)(item);
    return result;
}

##register prepend: Dom.private.element, Dom.private.element -> Dom.private.element
##args(to, item)
{
    var result = to.prepend(item);
    (%%BslClientOnly.Dom.flush_all%%)(to);
    (%%BslClientOnly.Dom.flush_all%%)(item);
    return result;
}

##register before: Dom.private.element, Dom.private.element -> Dom.private.element
##args(to, item)
{
    var result = to.before(item);
    (%%BslClientOnly.Dom.flush_all%%)(to);
    (%%BslClientOnly.Dom.flush_all%%)(item);
    return result;
}

##register after: Dom.private.element, Dom.private.element -> Dom.private.element
##args(to, item)
{
    var result = to.after(item);
    (%%BslClientOnly.Dom.flush_all%%)(to);
    (%%BslClientOnly.Dom.flush_all%%)(item);
    return result;
}

##register replace: Dom.private.element, Dom.private.element -> Dom.private.element
##args(to, item)
{
    var result = to.replaceWith(item);
    (%%BslClientOnly.Dom.flush_all%%)(to);
    (%%BslClientOnly.Dom.flush_all%%)(item);
    return result;
}

##register replace_contents: Dom.private.element, Dom.private.element -> Dom.private.element
##args(to, item)
{
    to.empty();
    var result = to.append(item);
    (%%BslClientOnly.Dom.flush_all%%)(to);
    (%%BslClientOnly.Dom.flush_all%%)(item);
    return result;
}

##register remove: Dom.private.element -> void
##args(dom)
{
    dom.empty();//Implementation note: faster than calling directly [dom.remove()]
    dom.remove();
}

##register detach: Dom.private.element -> void
##args(dom)
{
    return dom.detach()
}

##register give_focus: Dom.private.element -> Dom.private.element
##args(dom)
{
    dom.focus();
    return dom;
}

##register get_value: Dom.private.element -> string
##args(dom)
{
    var result = { value: "" }
    var f = function(node) {
        var value = dom.val()
        if(value == "" || value == null) return;
        result.value = result.value + value;
    }
    dom.each(f);
    return result.value
}

##register set_value: Dom.private.element, string -> void
##args(dom, value)
{
    dom.val(value);
}

##register get_text: Dom.private.element -> string
##args(dom)
{
    return dom.text()
}

##register set_text: Dom.private.element, string -> void
##args(dom, text)
{
    dom.text(text)
}

##register set_enabled: Dom.private.element,bool -> void
##args(dom, v)
{
    dom.prop("disabled", !v);
}

##register get_enabled: Dom.private.element -> bool
##args(dom)
{
    if(dom.prop("disabled")) // if undefined, return true
        return false
    else return true;
}

##register get_content: Dom.private.element -> string
##args(dom)
{
    var value = dom.val()
    if(value == "") return dom.text()
    return value;
}

##register set_html: Dom.private.element, string -> void
##args(dom, content)
{
    dom.html(content)
}

##register remove_content: Dom.private.element -> void
##args(dom)
{
    dom.empty()
}

##register get_offset: Dom.private.element -> Dom.dimensions
##args(dom)
{
    var offset = dom.offset();
    return normalize_obj({x_px: Math.round(offset.left || 0), y_px: Math.round(offset.top || 0)})
}

##register set_offset: Dom.private.element, Dom.dimensions -> void
##args(dom, offset)
{
    dom.offset({top: offset.y_px, left:offset.x_px})
}

##register get_size: Dom.private.element -> Dom.dimensions
##args(dom)
{
    return normalize_obj({x_px: Math.round(dom.width() || 0), y_px: Math.round(dom.height() || 0)})
}

##register set_width: Dom.private.element, int -> void
##args(dom, x)
{
    dom.width(x);
}

##register set_height: Dom.private.element, int -> void
##args(dom, x)
{
    dom.height(x);
}

##register set_size: Dom.private.element, Dom.dimensions -> void
##args(dom, size)
{
    dom.width(size.x_px);
    dom.height(size.y_px);
}

##register get_inner_size: Dom.private.element -> Dom.dimensions
##args(dom)
{
    return {x_px: dom.innerWidth() || 0, y_px: dom.innerHeight() || 0}
}

##register get_outer_size: Dom.private.element -> Dom.dimensions
##args(dom)
{
    return {x_px: dom.outerWidth() ||0, y_px: dom.outerHeight() || 0}
}

##register get_scrollable_size: Dom.private.element -> Dom.dimensions
##args(dom)
{
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

##register set_scroll: Dom.private.element, Dom.dimensions -> void
##args(dom, dimensions)
{
    dom.scrollLeft(dimensions.x_px)
    dom.scrollTop(dimensions.y_px)
}

##register get_scroll: Dom.private.element -> Dom.dimensions
##args(dom)
{
    return {x_px: dom.scrollLeft() || 0, y_px: dom.scrollTop() || 0}
}


##register bind: Dom.private.element, string, (Dom.event -> void) -> Dom.event_handler
##args(dom, event, f)
{
    return dom.opabind(event, f);
}

##register bind_with_options: Dom.private.element, string, (Dom.event -> void), bool, bool -> Dom.event_handler
##args(dom, event, f, stop_propagation, prevent_default)
{
    var g = function(e)
    {
        return f(dom_event_to_opa_event(e))
    }
    var h = stop_propagation?function(event) { event.stopPropagation(); g(event)}:g;
    var i = prevent_default ?function(event) { event.preventDefault();  h(event)}:h;
    return dom.opabind(event, i);
}

##register unbind_event\ bsldom_unbind: Dom.private.element, string -> void
##register unbind \ bsldom_unbind : Dom.private.element,Dom.event_handler -> void
function bsldom_unbind(dom, handler)
{
    dom.unbind(handler);
}

##register trigger: Dom.private.element, string -> void
##args(dom, event_kind)
{
    dom.trigger(event_kind);
}

##register trigger_event: Dom.private.element, Dom.event, string -> void
##args(dom, event, name)
{
    var dom_event = opa_event_to_dom_event(event, name);
    dom.trigger(dom_event);
}

##register set_property_unsafe: Dom.private.element, string, string -> void
##args(dom, name, value)
{
    dom.prop(name, value);
}

##register set_style_property_unsafe: Dom.private.element, string, string -> void
##args(dom, name, value)
{
    dom.css(name, value);
}

##register void_style: Dom.private.element -> void
##args(dom)
{
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
##register is_checked: Dom.private.element -> bool
##args(dom)
{
   var items  = dom.get();
   var length = items.length;
   var i;
   for(i = 0; i < length; ++i)
      if (items[i].checked) return true;
   return false;
}

##register set_checked: Dom.private.element, bool -> void
##args(dom, v)
{
   var items  = dom.get();
   var length = items.length;
   var i;
   for(i = 0; i < length; ++i)
       items[i].checked = v;
}

##register get_style_property_unsafe: Dom.private.element, string -> string
##args(dom, name)
{
    return dom.css(name) || "";
}

##register get_property: Dom.private.element, string -> opa[option(string)]
##args(dom, name)
{
    return js2option(dom.prop(name))
}

##register get_property_unsafe: Dom.private.element, string -> string
##args(dom, name)
{
    return dom.prop(name) || ""
}

##register add_class: Dom.private.element, string -> void
##args(dom, name)
{
    dom.addClass(name);
}

##register remove_class: Dom.private.element, string -> void
##args(dom, name)
{
    dom.removeClass(name);
}

##register has_class: Dom.private.element, string -> bool
##args(dom, name)
{
    return dom.hasClass(name)
}

##register set_class: Dom.private.element, string -> void
##args(dom, name)
{
    dom.removeClass();
    dom.addClass(name);
}

##register void_class: Dom.private.element -> void
##args(dom)
{
    dom.removeClass();
}


##register toggle_class: Dom.private.element,string -> void
##args(dom, name)
{
    dom.toggleClass(name);
}

//Export a callback
function export_cb(a){
  return a ? function () { a(this) } : undefined;
}

##register do_fade_in: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.fadeIn.apply(dom, args);
}

##register do_fade_out: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.fadeOut.apply(dom, args);
}

##register do_fade_toggle: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.fadeToggle.apply(dom, args);
}

// Mathieu Tue Feb  1 19:41:34 CET 2011
// FIXME:Why do we need 2 bypass there ?
##register do_show     \ bsldom_do_slide_in:  Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##register do_slide_in \ bsldom_do_slide_in: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
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
##register do_hide      \ bsldom_do_slide_out: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##register do_slide_out \ bsldom_do_slide_out: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
function bsldom_do_slide_out(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing = option2jsu(maybe_easing)
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.slideUp.apply(dom, args);
}

##register do_slide_toggle: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.slideToggle.apply(dom, args);
}
##register do_toggle: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [dur, easing, cb]
    dom.toggle.apply(dom, args);
}

##register do_change_to: opa[list(Css.compiled_property)] -> (Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void)
##args(properties)
{
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

##register do_fade_to: opa[list(Css.compiled_property)] -> (Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void)
##args(properties)
{
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

##register do_scroll_to_bottom: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [{scrollTop: %%BslDom.get_scrollable_size%%(dom).y_px }, dur, easing, cb]
    dom.animate.apply(dom, args);
}

##register do_scroll_to_right: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(dom, maybe_duration, maybe_easing, maybe_cb)
{
    var easing= option2jsu(maybe_easing);
    var dur   = option2jsu(maybe_duration);
    var cb    = export_cb(option2jsu(maybe_cb));
    var args = [{scrollLeft: %%BslDom.get_scrollable_size%%(dom).x_px }, dur, easing, cb]
    dom.animate.apply(dom, args);
}

/**
 * @param {?Function} maybe_cb
 */
##register do_wait: Dom.private.element, option(int), option(string), option(Dom.private.element -> void) -> void
##args(_, maybe_duration, _2, maybe_cb)
{
    /**@type {?Function}*/
    var cb   = option2js(maybe_cb);
    if(cb == null) return;//No callback? Do nothing!
    var x    = option2js(maybe_duration);
    /**@type {number}*/
    var dur  = x||400
    setTimeout(cb, dur);
}

##register stop: Dom.private.element -> void
##args(dom)
{
   dom.stop(true, false);
}

##register get_selection_range: -> Dom.private.selected
##args()
{
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
           finish_in:    document.elementFromPoint(range.boundingLeft + range.boundingWidth, range.boundingTop + range.offsetHeight),
           start_at:     offset,
           finish_at:    offset + length,
           is_collapsed: length == 0
       })
   }
}

##register bind_unload_confirmation: (Dom.event -> option(string)) -> void
##args(cb)
{
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

##register bind_beforeunload_confirmation: (Dom.event -> option(string)) -> void
##args(cb)
{
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

##register notification: string,string,string -> void
##args(img_url,title,body)
{
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
