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

##extern-type xhtml
##extern-type style_constructor
##extern-type insertion_handlers
##extern-type dom_element
##extern-type Client.location
##extern-type Dom.event
##extern-type Dom.propagation
##extern-type dom
##extern-type Xhtml.private_dom_element_list
   //equal to [private(list(dom_element))]

//Intercept all "esc" events before they can kill the ping loop
(function($){
    var intercept_escape = function(event)
    {
       if(event.keyCode == 27) {
          event.preventDefault();
          event.stopPropagation();
        }
    }
    $(window).keypress(intercept_escape);
    $(window).keyup(intercept_escape);
    $(window).keydown(intercept_escape);
    $(document).keypress(intercept_escape);
    $(document).keyup(intercept_escape);
    $(document).keydown(intercept_escape);
    $(document.body).bind("ready",function(event){$(document.body).focus()});
})(jQuery);

##register default_opa_event: -> Dom.event
##args()
{
  return default_opa_event
}


/**
 * {1 Dynamic loading of JavaScript}
 */

/**
 * A set of files that have been loaded dynamically during the execution of this client.
 */
var load_js_files = {}
##register load_js_then: string, (-> void) -> void
##args(uri, f)
{
    var key = encodeURI(uri);
    var cb = function() {
        load_js_files[key] = true;
        f();
    }
    if(load_js_files[key] == null)
        $.getScript(uri, cb);
    else
        f();
}



##register bad_cookie : -> 'a
##args()
{
  alert("Browser configuration error. Please clean your cookies and reload this page.")
}

##module Dom
  /* Dom node constructors **********************/
  ##register create_text_node: string -> dom_element
  ##args(str)
  {
    return document.createTextNode(str);
  }

  ##register create_element: string -> dom_element
  ##args(tag)
  {
    return document.createElement(tag);
  }

  ##register create_inner_html_unsafe : string -> dom_element
  ##args(inner)
  {
    var div = document.createElement("div");
    var fragment = document.createDocumentFragment();
    (new $(div)).html(inner);
    var child;
    while (child = div.firstChild){ fragment.appendChild(child)} //This will remove [child] from [div] transparently.
    return fragment;
  }

  ##register create_element_ns: string,string -> dom_element
  ##args(ns_uri, tag)
  {
    return ns_uri ? document.createElementNS(ns_uri, tag) : document.createElement(tag)
  }

  ##register create_fragment: -> dom_element
  ##args()
  {
    return document.createDocumentFragment();
  }



  /* Dom node manipulations *********************/
  ##register set_attribute: dom_element, string, string -> void
  ##args(element, name, value)
  {
    element.setAttribute(name, value);
  }

  ##register set_attribute_ns: dom_element, string, string, string -> void
  ##args(element, ns_uri, name, value)
  {
    ns_uri ? element.setAttributeNS(ns_uri, name, value) : element.setAttribute(name, value);
 }

  function apply_options_to_handler(f, stop_propagation, prevent_default)
  {
    if(stop_propagation)
    {
        var g = f;
        f = function(event) { event.stopPropagation(); g(event)}
    }
    if(prevent_default)
    {
        var h = f;
        f = function(event) { event.preventDefault(); h(event)}
    }
    return f;
  }

  /**
   * Bind a event handler to a UI element.
   *
   * @param name A event name (e.g. "click", "mouseover", etc. -- not "onclick" or "onmouseover").
   */
  ##register bind : dom_element, string, ('a -> 'b) -> void
  ##args(element, name, f)
  {
    (new $(element)).opabind(name, f, false, false);
  }


  /**
   * Bind a string to an event.
   *
   * @warning This function uses [eval] to deserialize strings to functions, albeit (normally) in a controlled way.
   */
  ##register opabind_value_unsafe : dom_element, string, string -> void
  ##args(element, name, value)
  {
    //TODO: If you find a way to do this without [eval], you're welcome to replace this technique
    //It may be tempting to manually set attribute ["on"+name] (e.g. "onload") to [value], but
    //                 1) It's not faster
    //                 2) It's not safer
    //                 3) It would break other event handlers on the same event and the same tag
    //See proposals in xhtml.opa for an alternative way f compiling [value].

    //It would be nicer to do [var f = eval("function ...")]. Unfortunately, tests on FF 3.6 show that it doesn't work.
    eval("var f = function(event) { " + value + " }");
    (new $(element)).opabind(name, f, false, false);
  }

  ##register bind_stop_propagation : dom_element, string -> void
  ##args(element, name)
  {
      (new $(element)).bind(name, function(event){event.stopPropagation()});
  }
  ##register bind_prevent_default : dom_element, string -> void
  ##args(element, name)
  {
      (new $(element)).bind(name, function(event){event.preventDefault()});
  }

  ##register set_style_attribute : dom_element, string, string -> void
  ##args(element, name, value)
  {
      (new $(element)).css(name, value);
  }

  ##register append_child: dom_element, dom_element -> void
  ##args(parent, child)
  {
    parent.appendChild(child);
  }

  ##register add_class_name : dom_element, string -> void
  ##args(element, className)
  {
      (new $(element)).addClass(className);
  }



  /* Style constructor **************************/
  /** see dom.opa */
  ##register create_style_constructor : -> style_constructor
  ##args()
  {
      return new Array();
  }

  /** see dom.opa */
  ##register add_style_application : style_constructor, dom_element, string, string -> void
  ##args(cons, element, name, value)
  {
      cons.push(function(){(new $(element)).css(name, value);});
  }

  /** see dom.opa */
  ##register flush_style_constructor : style_constructor -> void
  ##args(cons)
  {
      while(cons.length > 0) cons.pop()();
  }

  /**
   * Store a style information as a OPA-specific attribute of a Dom element.
   *
   * Note: Uses custom attribute name "opa_style".
   *
   * See also [apply_stored_css]
   */
  ##register store_style_constructor: dom_element, style_constructor -> void
  ##args(element, style)
  {
     if(style.length != 0)//Note: This is not just an optimization. It also ensures that we never attempt to (wrongly) store a (useless) constructor in a text node.
          element.opa_style = style;//Do not use [jQuery.attr] for this. According to experiments, this flattens everything into a string.
  }


  /**
   * Use information stored by [store_style_constructor] to apply CSS
   *
   * Note: Uses custom attribute name "opa_style".
   *
   * @param element The node containing the style previously stored
   *
   * See also [Xhtml.to_dom]
   */
  ##register apply_stored_css: dom_element -> void
  ##args(element)
  {
      var style = element.opa_style;
      if(style != null)
          while(style.length > 0) style.pop()();
  }


  /**
   * Use information stored by [store_style_constructor] to apply CSS
   *
   * Note: Uses custom attribute name "opa_style".
   *
   * @param jquery A set of nodes, possibly containing style previously stored
   *
   * See also [Xhtml.to_dom]
   */
  ##register apply_stored_css_to_jquery: dom -> void
  ##args(jquery)
  {
      var f = function(element) { %% BslClientOnly.Dom.apply_stored_css %%(element) }
      jquery.each(f)
  }



  /* Insertion handlers *************************/
  /**
   * Create an empty [insertion_handlers].
   */
  ##register create_insertion_handlers : -> insertion_handlers
  ##args()
  {
    return new Array();
  }

  /**
   * Add a handler to an [insertion_handlers].
   */
  ##register add_insertion_handler : insertion_handlers, (-> void) -> void
  ##args(cons, f)
  {
    cons.push(f);
  }

  /**
   * Add a handler to an [insertion_handlers] from a unsafe (JavaScript) string.
   */
  ##register add_insertion_handler_unsafe : insertion_handlers, string -> void
  ##args(cons, str)
  {
    var f = function(){eval(str)};
    cons.push(f);
  }


  /**
   * Store an [insertion_handlers] to a OPA-specific attribute of a Dom element.
   *
   * Note: Uses custom attribute name "opa_insertion".
   */
  ##register store_insertion_handlers : dom_element, insertion_handlers -> void
  ##args(element, cons)
  {
    if (cons.length != 0)
      element.opa_insertion = cons;
  }

  /**
   * Execute and remove from a [dom_element] all attached [insertion_handlers].
   */
  ##register apply_insertion_handlers : dom_element -> void
  ##args(element)
  {
    var handler = element.opa_insertion;
    if (handler != undefined){
      delete(element.opa_insertion);
      while(handler.length > 0) handler.pop()();
    }
  }

    /**
     * Flush any pending CSS or binding handler
     */
  ##register flush_all: dom -> void
  ##args(jq)
  {
      var elements = jq.get();
      var i;
      var len      = elements.length;
      for(i = 0; i < len; ++i)
      {
          var current = elements[i];
          (%% BslClientOnly.Dom.apply_stored_css %%)(current);
          (%% BslClientOnly.Dom.apply_insertion_handlers %%)(current);
      }
  }

##endmodule



##register scrollby : int, int -> void
##args(x, y)
{
    window.scrollBy(x, y);
}



##register get_cookie : -> string
##args()
{
    return document.cookie;
}
var event;


// Patch for Mozilla about contains method
if (window.Node && Node.prototype && !Node.prototype.contains)
{
  Node.prototype.contains = function (arg) { return !!(this.compareDocumentPosition(arg) & 16) }
}

/**
 * {1 Timeout and Intervals}
 *
 * In Javascript, there is no type [jstimerobjecti] nor [jstimerobjectt],
 * the functions set* return a simple ID of type int.
 * We define there 2 abtract types so that, thanks to the typer of OPA,
 * we cannot mix other ID with those.
 *
 * For a more detailed documentation of this :
 * @see http://www.w3schools.com/jsref/met_win_setinterval.asp
**/

##extern-type jstimerobjecti
##extern-type jstimerobjectt

##register setTimeout     \ `setTimeout`    : (-> void), int ->  jstimerobjectt
##register setInterval    \ `setInterval`   : (-> void), int ->  jstimerobjecti

##register clearTimeout   \ `clearTimeout`  :  jstimerobjectt -> void
##register clearInterval  \ `clearInterval` :  jstimerobjecti -> void

##register getCookie : -> option(string)
##args()
{
  return document.cookie.length ? js_some(document.cookie) : js_none;
}

##register getStableCookie \ getStableCookie : -> option(string)
function getStableCookie()
{
    var full_cookie = document.cookie;
    if(full_cookie.length == 0) return js_none;
    var reg = /ic=([0-9a-zA-Z]{32})/;
    var tmp = full_cookie.match(reg);
    if (tmp != null) return js_some(tmp[1]);
    return js_none;//What does this case cover, exactly?
}

##register get_location : -> Client.location
##args()
{
    var host = "";
    var hostname = "";
    var href = "";
    var origin = "";
    var pathname = "";
    var protocol = "";
    var port = "";
    if(window && window.location){
	var l = window.location;
	if(l.host) host=l.host;
	if(l.hostname) hostname=l.hostname;
	if(l.href) href=l.href;
	if(l.origin) origin=l.origin;
	if(l.pathname) pathname=l.pathname;
	if(l.port) port = l.port;
	if(l.protocol) protocol = l.protocol;
    }
    var fhost = static_field_of_name("host");
    var fhostname = static_field_of_name("hostname");
    var fhref = static_field_of_name("href");
    var forigin = static_field_of_name("origin");
    var fpathname = static_field_of_name("pathname");
    var fprotocol = static_field_of_name("protocol");
    var fport = static_field_of_name("port");
    var r = empty_constructor();
    r = add_field(r,fhost,host);
    r = add_field(r,fhostname,hostname);
    r = add_field(r,fhref,href);
    r = add_field(r,forigin,origin);
    r = add_field(r,fpathname,pathname);
    r = add_field(r,fprotocol,protocol);
    r = add_field(r,fport,port);
    return r;
}
