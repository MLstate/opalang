/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/** @externType xhtml */
/** @externType style_constructor */
/** @externType insertion_handlers */
/** @externType dom_element */
/** @externType Client.location */
/** @externType Dom.event */
/** @externType native_event */
/** @externType Dom.event_propagation */
/** @externType dom */
/** @externType Xhtml.private_dom_element_list */
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

/**
 * @register { -> Dom.event} default_opa_event
 */
function get_default_opa_event() {
  return default_opa_event
}

/** @register { Dom.event -> native_event} dom_event_to_opa_event dom_event_to_opa_event */

/**
 * {1 Dynamic loading of JavaScript}
 */

/**
 * A set of files that have been loaded dynamically during the execution of this client.
 */
var load_js_files = {}

/**
 * @register {string, (-> void) -> void}
 */
function load_js_then(uri, f) {
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



/**
 * @register { -> 'a}
 */
function bad_cookie() {
  alert("Browser configuration error. Please clean your cookies and reload this page.")
}

/** @module Dom */
  /* Dom node constructors **********************/

  /**
   * @register {string -> dom_element}
   */
  function create_text_node(str) {
    return document.createTextNode(str);
  }

  /**
   * @register {string -> dom_element}
   */
  function create_element(tag) {
    return document.createElement(tag);
  }

  /**
   * @register {string -> dom_element}
   */
  function create_inner_html_unsafe(inner) {
    var div = document.createElement("div");
    var fragment = document.createDocumentFragment();
    (new $(div)).html(inner);
    var child;
    while (child = div.firstChild){ fragment.appendChild(child)} //This will remove [child] from [div] transparently.
    return fragment;
  }

  /**
   * @register {string,string -> dom_element}
   */
  function create_element_ns(ns_uri, tag) {
    return ns_uri ? document.createElementNS(ns_uri, tag) : document.createElement(tag)
  }

  /**
   * @register { -> dom_element}
   */
  function create_fragment() {
    return document.createDocumentFragment();
  }



  /* Dom node manipulations *********************/
  /**
   * @register {dom_element, string, string -> void}
   */
  function set_attribute(element, name, value) {
    element.setAttribute(name, value);
  }

  /**
   * @register {dom_element, string, string, string -> void}
   */
  function set_attribute_ns(element, ns_uri, name, value) {
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
   *
   * @register {dom_element, string, ('a -> 'b) -> void} bind
   */
  function client_only_bind(element, name, f) {
    (new $(element)).opabind(name, f, null, false, false);
  }


  /**
   * Bind a string to an event.
   *
   * @warning This function uses [eval] to deserialize strings to functions, albeit (normally) in a controlled way.
   *
   * @register {dom_element, string, string -> void}
   */
  function opabind_value_unsafe(element, name, value) {
    //TODO: If you find a way to do this without [eval], you're welcome to replace this technique
    //It may be tempting to manually set attribute ["on"+name] (e.g. "onload") to [value], but
    //                 1) It's not faster
    //                 2) It's not safer
    //                 3) It would break other event handlers on the same event and the same tag
    //See proposals in xhtml.opa for an alternative way f compiling [value].

    //It would be nicer to do [var f = eval("function ...")]. Unfortunately, tests on FF 3.6 show that it doesn't work.
    eval("var f = function(event) { " + value + " }");
    (new $(element)).opabind(name, f, null, false, false);
  }

  /**
   * @register {dom_element, string -> void}
   */
  function bind_stop_propagation(element, name) {
      (new $(element)).bind(name, function(event){event.stopPropagation()});
  }
  /**
   * @register {dom_element, string -> void}
   */
  function bind_prevent_default(element, name) {
      (new $(element)).bind(name, function(event){event.preventDefault()});
  }

  /**
   * @register {dom_element, string, string -> void}
   */
  function set_style_attribute(element, name, value) {
      (new $(element)).css(name, value);
  }

  /**
   * @register {dom_element, dom_element -> void}
   */
  function append_child(parent, child) {
      if (parent.appendChild) parent.appendChild(child);
      else (new $(parent)).append($(child));
  }

  /**
   * @register {dom_element, string -> void}
   */
  function add_class_name(element, className) {
      (new $(element)).addClass(className);
  }



  /* Style constructor **************************/
  /**
   * See dom.opa
   *
   * @register { -> style_constructor}
   */
  function create_style_constructor() {
      return new Array();
  }

  /**
   * See dom.opa
   *
   * @register {style_constructor, dom_element, string, string -> void}
   */
  function add_style_application(cons, element, name, value) {
      cons.push(function(){(new $(element)).css(name, value);});
  }

  /**
   * See dom.opa
   *
   * @register {style_constructor -> void}
   */
  function flush_style_constructor(cons) {
      while(cons.length > 0) cons.pop()();
  }

  /**
   * Store a style information as a OPA-specific attribute of a Dom element.
   *
   * Note: Uses custom attribute name "opa_style".
   *
   * See also [apply_stored_css]
   *
   * @register {dom_element, style_constructor -> void}
   */
  function store_style_constructor(element, style) {
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
   *
   * @register {dom_element -> void}
   */
  function apply_stored_css(element) {
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
   *
   * @register {dom -> void}
   */
  function apply_stored_css_to_jquery(jquery) {
      var f = function(element) { BslClientOnly_Dom_apply_stored_css(element) }
      jquery.each(f)
  }



  /* Insertion handlers *************************/
  /**
   * Create an empty [insertion_handlers].
   *
   * @register { -> insertion_handlers}
   */
  function create_insertion_handlers() {
    return new Array();
  }

  /**
   * Add a handler to an [insertion_handlers].
   *
   * @register {insertion_handlers, (-> void) -> void}
   */
  function add_insertion_handler(cons, f) {
    cons.push(f);
  }

  /**
   * Add a handler to an [insertion_handlers] from a unsafe (JavaScript) string.
   *
   * @register {insertion_handlers, string -> void}
   */
  function add_insertion_handler_unsafe(cons, str) {
    var f = function(){eval(str)};
    cons.push(f);
  }


  /**
   * Store an [insertion_handlers] to a OPA-specific attribute of a Dom element.
   *
   * Note: Uses custom attribute name "opa_insertion".
   *
   * @register {dom_element, insertion_handlers -> void}
   */
  function store_insertion_handlers(element, cons) {
    if (cons.length != 0)
      element.opa_insertion = cons;
  }

  /**
   * Execute and remove from a [dom_element] all attached [insertion_handlers].
   *
   * @register {dom_element -> void}
   */
  function apply_insertion_handlers(element) {
    var handler = element.opa_insertion;
    if (typeof handler != 'undefined'){
      delete(element.opa_insertion);
      while(handler.length > 0) handler.pop()();
    }
  }

  /**
   * Flush any pending CSS or binding handler
   *
   * @register {dom -> void}
   */
  function flush_all(jq) {
      var elements = jq.get();
      var i;
      var len      = elements.length;
      for(i = 0; i < len; ++i)
      {
          var current = elements[i];
          BslClientOnly_Dom_apply_stored_css(current);
          BslClientOnly_Dom_apply_insertion_handlers(current);
      }
  }

/** @endModule */



/**
 * @register {int, int -> void}
 */
function scrollby(x, y) {
    window.scrollBy(x, y);
}



/**
 * @register { -> string}
 */
function get_cookie() {
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

/** @externType jstimerobjecti */
/** @externType jstimerobjectt */

/** @register {(-> void), int ->  jstimerobjectt} setTimeout setTimeout */
/** @register {(-> void), int ->  jstimerobjecti} setInterval setInterval */

/** @register {jstimerobjectt -> void} clearTimeout clearTimeout */
/** @register {jstimerobjecti -> void} clearInterval clearInterval */

/**
 * @register { -> opa[option(string)]}
 */
function getCookie() {
  return document.cookie.length ? js_some(document.cookie) : js_none;
}

/** @register { -> opa[option(string)]} getStableCookie getStableCookie */
function getStableCookie()
{
    var full_cookie = document.cookie;
    if(full_cookie.length == 0) return js_none;
    var reg = /ic=([0-9a-zA-Z]{32})/;
    var tmp = full_cookie.match(reg);
    if (tmp != null) return js_some(tmp[1]);
    return js_none;//What does this case cover, exactly?
}

/**
 * @register { -> Client.location}
 */
function get_location() {
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
