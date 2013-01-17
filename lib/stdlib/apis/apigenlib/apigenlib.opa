/*
    Copyright © 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * Support code for apigen.
 *
 * @category web
 * @author Norman Scaife, 2012
 * @destination public
 * @stability Work in progress
 */
import stdlib.core.wbxml
import stdlib.apis.common
import stdlib.apis.oauth
import stdlib.io.socket
import stdlib.crypto

type Apigen.expected_type =
    {BOOL}
 or {INT}
 or {OBJECT}
 or {LIST}

type Apigen.content =
    {xml_document xmldoc}
 or {xmlns xmlns}
 or {xhtml xhtml}
 or {string plain}
 or {binary binary}
 or {(string,string) unknown}
 or {(string,binary) unknown_binary}
 or {none}

type Apigen.failure =
    {string bad_path}
 or {WebClient.failure network}
 or {RPC.Json.json error_json}
 or {RPC.Json.json bad_json}
 or {string post_process}
 or {string socket}
 or {string pack}
 or {string bad_xml}
 or {string bad_wbxml}
 or {Apigen.content bad_content}
 or {string error}
 or {xhtml error_html}

type Apigen.outcome('a) = outcome('a,Apigen.failure)

type ApigenAuth.credentials = {
  string access_token,
  string access_secret
}

type ApigenLib.auth =
    {string user, string password}
 or {none}

type ApigenLib.general_value =
    {string String}
 or {int Int}
 or {bool Bool}
 or {float Float}
 or {binary Binary}
 or {Date.date Datetime}
 or {xmlns Verbatim}
 or {Empty}

type ApigenLib.simple_seq = list((string,ApigenLib.general_value))

private (binary -> string) bindump = %% BslPervasives.bindump %%
private (string -> string) memdump = %% BslPervasives.memdump %%

module ApigenOauth(OAuth.parameters params) {

  function alOAuth(http_method) { OAuth({params with ~http_method}) }

  private function generic(string base, string path, wget_fun, parse_fun) {
    API_libs_private.apijlog("ApigenOauth.generic: uri={base}{path}")
    (t, res) = Duration.execution_time(function () { wget_fun("{base}{path}") })
    match (res) {
    case {success:res}:
      API_libs_private.apijlog("ApigenOauth.generic: got={res}")
      API_libs_private.apijlog("Download: {Duration.in_seconds(t)} seconds")
      (t, res) = Duration.execution_time(function () { parse_fun(res.content) })
      API_libs_private.apijlog("ApigenOauth.generic: parsed={res}")
      API_libs_private.apijlog("Parsing:  {Duration.in_seconds(t)} seconds")
      res
    case {failure:f}:
      API_libs_private.apijlog("ApigenOauth.generic: Network failure {f}")
      {failure:{network:f}}
    }
  }

  function GET(base, path, params, ApigenAuth.credentials credentials, parse_fun) {
    function f(uri) { alOAuth({GET}).get_protected_resource_2(uri,params,credentials.access_token,credentials.access_secret) }
    generic(base, path, f, parse_fun)
  }

  function POST(base, path, params, content, ApigenAuth.credentials credentials, parse_fun) {
    _ = content
    function f(uri) { alOAuth({POST}).get_protected_resource_2(uri,params,credentials.access_token,credentials.access_secret) }
    generic(base, path, f, parse_fun)
  }

}

module ApigenLib {

  private tzfmt = Date.generate_printer("%z")
  private fmt = Date.generate_printer("%FT%T")

  function date_to_string(Date.date d) {
    tz = Date.to_formatted_string(tzfmt, d)
    tz =
      match (String.length(tz)) {
      case 4: String.sub(0,2,tz)^":"^String.sub(2,2,tz)
      case 5: String.sub(0,3,tz)^":"^String.sub(3,2,tz)
      default: tz
      }
    "{Date.to_formatted_string(fmt, d)}{tz}"
  }

  function date_to_string2(Date.date d) {
    "{Date.to_formatted_string(fmt, d)}{Date.to_formatted_string(tzfmt, d)}"
  }

  private function generic_build_path(path, options) {
    if (options == []) path else "{path}?{API_libs.form_urlencode(options)}"
  }

  function authentication(http_options, ApigenLib.auth auth) {
    match (auth) {
    case ~{user, password}:
      auth = Crypto.Base64.encode(binary_of_string("{user}:{password}"))
      {http_options with custom_headers:List.append(http_options.custom_headers,["Authorization: Basic {auth}"])};
      //{http_options with headers:List.append(http_options.headers,[("Authorization","Basic {auth}")])};
    default: http_options;
    }
  }

  /**
   * Make a HTTP GET on [path] at [base] with [options]
   */
  function GET(base, path, options, parse_fun) {
    final_path = generic_build_path("{base}{path}", options)
    //API_libs_private.apijlog("GET {final_path}")
    match (Uri.of_string(final_path)) {
    case {none}: {failure:{bad_path:final_path}}
    case {some:uri}:
      match (WebClient.Get.try_get(uri)) {
      case {success:res}: parse_fun(res)
      case {failure:f}: {failure:{network:f}}
      }
    }
  }

  function content_only(res) {
    match (res) {
    case {failure:f}: {failure:{network:f}}
    case {success:s}: /*API_libs_private.apijlog("HTTP {s.code}\n{s.content}");*/ {success:s.content}
    }
  }

  // header_get doesn't work on node.
  private function find_header(name,headers) {
    List.fold(function (h, acc) {
                if (String.has_prefix(name,h)) {
                  len = String.length(name)
                  {some:String.sub(len+2,String.length(h)-(len+2),h)}
                } else acc
              },headers,none)
  }

  function content_with_type(res) {
    match (res) {
    case {failure:f}: {failure:{network:f}}
    case {success:s}:
       //API_libs_private.apijlog("hdrs=\n{String.concat("\n",s.headers)}")
       content_type = Option.default("unknown/unknown",find_header("content-type",s.headers))
       location = Option.default("",find_header("location",s.headers))
       //API_libs_private.apijlog("HTTP {s.code} type={content_type} location={location}")
       {success:(content_type,location,s.content)}
    }
  }

  function redirect(res) {
    match (res) {
    case {failure:f}: {failure:{network:f}}
    case {success:s}:
       //API_libs_private.apijlog("HTTP {s.code}")
       if (s.code == 302) {
         match (find_header("location",s.headers)) {
         case {some:hdr}: {success:hdr}
         default: {failure:{post_process:"header location not found"}}
         }
       } else {failure:{bad_code:s.code}}
    }
  }

  /**
   * Make a HTTP POST on [path] at [base] with string [data]
   */
  function POST_GENERIC(string base, string path, list((string,string)) options,
                        ApigenLib.auth auth, WebClient.Post.options(string) http_options, parse_fun) {
                        //ApigenLib.auth auth, WebClient.options(binary) http_options, parse_fun) {
    //API_libs_private.apijlog("POST {base}{path}\n{http_options.content}\n")
    final_path = generic_build_path("{base}{path}", options)
    match (Uri.of_string(final_path)) {
    case {none}: {failure:{bad_path:final_path}};
    case {some:uri}:
      //http_options = {http_options with method:"POST"}
      http_options = authentication(http_options, auth)
jlog("uri:{uri}\nhttp_options:{{http_options with content:none}}")
      match (WebClient.Post.try_post_with_options(uri,http_options)) {
      //match (WebClient.request(uri,http_options)) {
      case {success:res}: jlog("res:{{res with content:"Binary"}}"); parse_fun(res)
      case {failure:f}: {failure:{network:f}}
      }
    }
  }

  /**
   * Make a HTTP POST on [path] at [base] with string [content]
   */
  function POST(base, path, options, content, auth, parse_fun) {
    POST_GENERIC(base, path, options, auth, {WebClient.Post.default_options with content:{some:content}}, parse_fun)
    //POST_GENERIC(base, path, options, auth, {WebClient.default_options with content:{some:content}}, parse_fun)
  }

  /**
   * Make a HTTP POST on [path] at [base] with form [data]
   */
  function POST_FORM(base, path, options, data, auth, parse_fun) {
    content = API_libs.form_urlencode(data)
    POST_GENERIC(base, path, options, auth, {WebClient.Post.default_options with content:{some:content}}, parse_fun)
    //content = binary_of_string(API_libs.form_urlencode(data))
    //POST_GENERIC(base, path, options, auth, {WebClient.default_options with content:{some:content}}, parse_fun)
  }

  /**
   * Make a HTTP POST on [path] at [base] with string [xmlns]
   */
  function POST_XML(base, path, options, auth, custom_headers, xmlns, parse_fun) {
    http_options = {WebClient.Post.default_options with mimetype:"text/xml", ~custom_headers, content:{some:xmlns}}
    POST_GENERIC(base, path, options, auth, http_options, parse_fun)
    //headers = [("Content-Type","text/xml")|custom_headers]
    //http_options = {WebClient.default_options with ~headers, content:{some:xmlns}}
    //POST_GENERIC(base, path, options, auth, http_options, parse_fun)
  }

  /**
   * Make a HTTP POST on [path] at [base] with string [xmlns]
   */
  function POST_WBXML(base, path, options, auth, custom_headers, wbxml, parse_fun) {
    http_options = {WebClient.Post.default_options with mimetype:"application/vnd.ms-sync.wbxml",
                                                        ~custom_headers,
                                                        content:{some:wbxml}}
    POST_GENERIC(base, path, options, auth, http_options, parse_fun)
    //headers = [("Content-Type","application/vnd.ms-sync.wbxml")|custom_headers]
    //http_options = {WebClient.default_options with ~headers, content:{some:wbxml}}
    //POST_GENERIC(base, path, options, auth, http_options, parse_fun)
  }

  /** Generic http operation */
  private function WebClient.Generic.options default_options(op) {
    { operation       : op,
      auth            : {none},
      custom_headers  : [],
      custom_agent    : {none},
      redirect        : {none},
      timeout_sec     : {some:36.0},
      ssl_key         : {none},
      ssl_policy      : {none}
    }
  }

  private function void try_generic_with_options_async(string op, Uri.uri location, string content,
                                                       WebClient.Generic.options options,
                                                       (WebClient.result(string) -> void) on_result) {
    generic_options = { options with
                          operation      : op,
                          custom_headers : ["Content-Length: {String.length(content)}"]++options.custom_headers
                      }
    function on_success(x) { on_result({success:x}) }
    function on_failure(x) { on_result({failure:x}) }
    WebClient.Generic.try_request_with_options_async(location, op, generic_options, {some:content}, on_success, on_failure)
  }

  private function WebClient.result(string) try_generic_with_options(string op, Uri.uri location,
                                                                     string content, WebClient.Generic.options options) {
    @callcc(function (k) {
              function on_result(x) { Continuation.return(k, x) }
              try_generic_with_options_async(op, location, content, options, on_result)
            })
  }

  private function WebClient.result(string) try_generic(string op, Uri.uri location, string content) {
    try_generic_with_options(op, location, content, default_options(op))
  }

  /**
   * Make a HTTP OPTIONS on [path] at [base] with form [data]
   */
  function try_options(location, content) { try_generic("OPTIONS", location, content) }
  function try_options_with_options(location, content, options) {
    try_generic_with_options("OPTIONS", location, content, options)
  }

  /**
   * Make a HTTP OPTIONS on [path] at [base] with string [data]
   */
  function OPTIONS(base, path, options, http_options, content, parse_fun) {
    //API_libs_private.apijlog("OPTIONS {base}{path}\n{content}\n")
    final_path = generic_build_path("{base}{path}", options)
    match (Uri.of_string(final_path)) {
    case {none}: {failure:{bad_path:final_path}};
    case {some:uri}:
      match (try_options_with_options(uri,content, http_options)) {
      case {success:res}: parse_fun(res)
      case {failure:f}: {failure:{network:f}}
      }
    }
  }

  function OPTIONS_XML(base, path, options, content, parse_fun) {
    http_options = {default_options("OPTIONS") with custom_headers:["Content-Type: text/xml"]}
    //content = "<?xml version=\"1.0\"?>\n"^Xmlns.to_string(xmlns)
    OPTIONS(base, path, options, http_options, content, parse_fun)
  }

  /** Generic HTTP parameters, sreq=required string, bopt=optional bool etc. */

  function params(parameters) {
    List.fold(function (p, data) {
      match (p) {
      case {nes:(name,v)}: if (v == "") data else List.add((name,v),data)
      case {sreq:(name,v)}: List.add((name,v),data)
      case {sopt:(name,v)}: if (Option.is_none(v)) data else List.add((name, Option.get(v)), data)
      case {ireq:(name,v)}: List.add((name,Int.to_string(v)),data)
      case {iopt:(name,v)}: if (Option.is_none(v)) data else List.add((name, Int.to_string(Option.get(v))),data)
      case {breq:(name,v)}: List.add((name,Bool.to_string(v)),data)
      case {bopt:(name,v)}: if (Option.is_none(v)) data else List.add((name, Bool.to_string(Option.get(v))),data)
      case {dreq:(name,v)}: List.add((name,date_to_string(v)),data)
      case {dopt:(name,v)}: if (Option.is_none(v)) data else List.add((name, date_to_string(Option.get(v))),data)
      case {jreq:(name,toj,v)}: List.add((name, Json.serialize(toj(v))), data)
      case {jopt:(name,toj,v)}: if (Option.is_none(v)) data else List.add((name, Json.serialize(toj(Option.get(v)))), data)
      case {creq:(name,tos,v)}: List.add((name,tos(v)),data)
      case {copt:(name,tos,v)}: if (Option.is_none(v)) data else List.add((name, tos(Option.get(v))),data)
      case {aopt:(name,toj,v)}: if (v == []) data else List.add((name, Json.serialize({List:List.map(toj,v)})), data)
      case {lopt:(name,tos,v)}: if (v == []) data else List.add((name, String.concat(",",List.map(tos,v))), data)
      }
    },parameters,[])
  }

  /** Generic HTTP forms, file=binary file definition, bopt=optional bool etc. */

  function forms(forms) {
    List.fold(function (f, forms) {
      match (f) {
      case {file:(name,filename,content_type,content)}: List.add(~{name, filename, content_type, content},forms)
      case {nes:(name,v)}: if (v == "") forms else List.add({~name, content:v},forms)
      case {sreq:(name,v)}: List.add({~name, content:v},forms)
      case {sopt:(name,v)}: if (Option.is_none(v)) forms else List.add({~name, content:Option.get(v)},forms)
      case {breq:(name,v)}: List.add({~name, content:Bool.to_string(v)},forms)
      case {bopt:(name,v)}: if (Option.is_none(v)) forms else List.add({~name, content:Bool.to_string(Option.get(v))},forms)
      case {dreq:(name,v)}: List.add({~name, content:date_to_string(v)},forms)
      case {dopt:(name,v)}: if (Option.is_none(v)) forms else List.add({~name, content:date_to_string(Option.get(v))},forms)
      case {ireq:(name,v)}: List.add({~name, content:Int.to_string(v)},forms)
      case {iopt:(name,v)}: if (Option.is_none(v)) forms else List.add({~name, content:Int.to_string(Option.get(v))},forms)
      case {jreq:(name,toj,v)}: List.add({~name, content:Json.serialize(toj(v))}, forms)
      case {jopt:(name,toj,v)}:
        if (Option.is_none(v)) forms else List.add({~name, content:Json.serialize(toj(Option.get(v)))},forms)
      case {creq:(name,tos,v)}: List.add({~name, content:tos(v)},forms)
      case {copt:(name,tos,v)}: if (Option.is_none(v)) forms else List.add({~name, content:tos(Option.get(v))},forms)
      case {aopt:(name,toj,v)}: if (v == []) forms else List.add({~name, content:Json.serialize({List:List.map(toj,v)})}, forms)
      }
    },forms,[])
  }

  function get_json_int(json) { match (json) { case {Int:i}: i default: -1 } }
  function get_json_string(json) { match (json) { case {String:s}: s default: "" } }
  function get_unknown(name, map) { Map.get(name, map) ? {String:"missing unknown type"} }
  get_int = API_libs_private.map_get_int
  get_float = API_libs_private.map_get_float
  get_string = API_libs_private.map_get_string
  get_bool = API_libs_private.map_get_bool
  function get_date(name, map) { Json.to_string(Map.get(name, map) ? {String:"Error in date" }) }
  function get_raw_obj(json, get_elt) { get_elt(json) }
  function get_obj(name, map, get_elt) { get_raw_obj((Map.get(name, map)) ? {Record:[]}, get_elt) }
  function get_raw_list(json, get_elt) {
    match (json) {
    case {List:_}: {success:List.map(get_elt, JsonOpa.to_list(json) ? [])}
    default: {failure:{bad_json:json}}
    }
  }
  function get_list(name, map, get_elt) { get_raw_list(Map.get(name, map) ? {List:[]}, get_elt) }

  function Apigen.outcome('a) check_errors(string s, string error_field, Apigen.expected_type expected_type,
                                           (RPC.Json.json -> Apigen.outcome('a)) on_ok) {
    json = API_libs_private.parse_json(s)
    match ((expected_type,json)) {
    case (_,{Record:r}):
       if (error_field != "") {
         match (List.assoc(error_field,r)) {
         case {some:error_json}: {failure:{~error_json}}
         case {none}: if (expected_type == {OBJECT}) on_ok(json) else {failure:{bad_json:json}}
         }
       } else if (expected_type == {OBJECT}) on_ok(json) else {failure:{bad_json:json}}
    case ({LIST},{List:_}): on_ok(json)
    case ({BOOL},{Bool:_}): on_ok(json)
    case ({INT},{Int:_}): on_ok(json)
    default: {failure:{bad_json:json}}
    }
  }

  function Apigen.outcome(bool) expect_bool(RPC.Json.json json) {
    match (json) {
    case {Bool:b}: {success:b}
    default: {failure:{bad_json:json}}
    }
  }

  function build_bool(WebClient.success res, string error_field) {
    if (error_field != "")
      check_errors(res.content, error_field, {BOOL}, expect_bool)
    else
      expect_bool(API_libs_private.parse_json(res.content))
  }

  function Apigen.outcome(int) expect_int(RPC.Json.json json) {
    match (json) {
    case {Int:i}: {success:i}
    default: {failure:{bad_json:json}}
    }
  }

  function build_int(WebClient.success res, string error_field) {
    if (error_field != "")
      check_errors(res.content, error_field, {INT}, expect_int)
    else
      expect_int(API_libs_private.parse_json(res.content))
  }

  function build_xml(WebClient.success res) {
    //jlog(Ansi.print({yellow},"res:{res}"))
    if (String.has_prefix("<!DOCTYPE html",res.content))
      {success:{xhtml:Xhtml.of_string(res.content)}}
    else
      match (Xmlns.try_parse_document(res.content)) {
      case {some:xmldoc}: {success:{xmldoc:xmldoc}};
      case {none}: {failure:{bad_xml:res.content}};
      }
  }

  function build_wbxml(WebClient.success res, WBXml.context context) {
    match (WBXml.to_xmlns(context, %%bslBinary.of_encoding%%(res.content,"binary"))) {
    case {success:(_ctxt,xmlns)}: {success:xmlns};
    case {~failure}: {failure:{bad_wbxml:failure}};
    }
  }

  function outcome(Apigen.content,Apigen.failure) build_from_content_type(WebClient.success(string) res,
                                                                          option(WBXml.context) context) {
    //jlog(Ansi.print({yellow},"res:{res}"))
    content_type = Option.default("unknown/unknown",find_header("content-type",res.headers))
    match (List.map(String.trim,String.explode(";",content_type))) {
    case ["text/plain"|_]:
      {success:{plain:res.content}};
      //{success:{plain:%%bslBinary.to_encoding%%(res.content,"binary")}};
    case ["text/html"|_]:
      {success:{xhtml:Xhtml.of_string(res.content)}};
      //{success:{xhtml:Xhtml.of_string(%%bslBinary.to_encoding%%(res.content,"binary"))}};
    case ["text/xml"|_]:
      match (Xmlns.try_parse_document(res.content)) {
      //match (Xmlns.try_parse_document(%%bslBinary.to_encoding%%(res.content,"binary"))) {
      case {some:xmldoc}: {success:{xmldoc:xmldoc}};
      case {none}: {failure:{bad_xml:res.content}};
      //case {none}: {failure:{bad_xml:%%bslBinary.to_encoding%%(res.content,"binary")}};
      }
    case ["application/vnd.ms-sync.wbxml"|_]:
      match (context) {
      case {some:context}:
//jlog("res.content:\n{memdump(res.content)}")
        match (WBXml.to_xmlns({context with debug:1}, %%bslBinary.of_encoding%%(res.content,"binary"))) {
        //match (WBXml.to_xmlns({context with debug:2}, res.content)) {
        case {success:(_ctxt,xmlns)}:
          List.iter(function (header) {
                      //jlog("header: \"{header}\"")
                      if (String.has_prefix("x-ms-aserror:",header))
                        jlog("x-ms-aserror: \"{Ansi.print({red},String.sub(13,String.length(header)-13,header))}\"")
                    },res.headers)
          {success:xmlns};
        case {~failure}: {failure:{bad_wbxml:failure}};
        }
      case {none}: {failure:{error:"No WBXml context"}};
      }
    default:
      //jlog("content_type:\"{content_type}\"")
      {failure:{bad_content:{unknown:(content_type,res.content)}}};
      //{failure:{bad_content:{unknown_binary:(content_type,res.content)}}};
    }
  }

}

/**
 * Generalized configuration for API and protocol code.
 * Only contains the basics, in future this may be customizable.
 */
type ApigenLib.conf = {
  Socket.host default_host,
  string user,
  string password,
  int bufsize,
  int poolmax,
  int timeout,
  bool verbose,
}

/**
 * Length definition, describes where in a packet to find the length information.
 */
type ApigenLib.length = {
  int offset,
  bool le,
  bool signed,
  Pack.s size
}

/**
 * Wrapper type for configuration, will be used to host customized parameters later.
 */
type ApigenLib.params = {
  ApigenLib.conf conf
}

protected @package
module ApigenLibConf(Socket.host default_host) {

  private
  ApigenLib.conf default_conf_ = {
    ~default_host,
    user          : "user",
    password      : "password",
    bufsize       : 50*1024,
    poolmax       : 10,
    timeout       : 60 * 60 * 1000,
    verbose       : false,
  }

  private Hashtbl.t(string, ApigenLib.params) params = Hashtbl.create(3)

  private
  auth_parser = parser {
    case user=((![:] .)+)[:] password=((![@] .)+):
      {user:Text.to_string(user), password:Text.to_string(password)}
  }

  private
  function (string,option(int)) host_of_string(string s) {
    match (String.explode(":",s)) {
    case [host|[port|[]]]: (host,{some:Int.of_string(port)})
    default: (s,none)
    }
  }

  private
  function family(family_name, name) {
    family_name = if (family_name == "") "apigenlib" else family_name
    name = if (name == "") "" else ":{name}"
    {
      title:"Options for {family_name} connection",
      init:{conf:default_conf_},
      anonymous:[],
      parsers:[ // TODO: filter these out, not all possible clients will need all options. (Also allow custom options).
        {CommandLine.default_parser with
           names:["--{family_name}-bufsize{name}"],
           description:"Hint for initial {name} connection buffer size",
           param_doc:"<int>",
           on_param:function (p) {
             parser {
               case n={Rule.natural}:
                 {no_params:{ p with conf.bufsize:n }}
             }
           }
        },
        {CommandLine.default_parser with
           names:["--{family_name}-pool{name}"],
           description:"Number of sockets in socket pool (>=2 enables socket pool)",
           param_doc:"<int>",
           on_param:function (p) {
             parser {
               case n={Rule.natural}:
                 {no_params:{ p with conf.poolmax:Int.max(n,1) }}
             }
           }
        },
        {CommandLine.default_parser with
           names:["--{family_name}-timeout{name}"],
           description:"Timeout for read/write operations",
           param_doc:"<int>",
           on_param:function (p) {
             parser {
               case n={Rule.natural}:
                 {no_params:{ p with conf.timeout:Int.max(n,1) }}
             }
           }
        },
        {CommandLine.default_parser with
           names:["--{family_name}-log{name}"],
           description:"Enable logging",
           param_doc:"<bool>",
           on_param:function (p) {
             parser {
               case b={Rule.bool}:
                 {no_params:{ p with conf.verbose:b }}
             }
           }
        },
        {CommandLine.default_parser with
           names:["--{family_name}-default-host{name}"],
           description:"Set the host for connection {name}",
           param_doc:"<host>[:<port>]",
           on_param:function (p) {
             parser {
               case s={Rule.consume}:
                 {no_params:
                   match (host_of_string(s)) {
                   case (host,{some:port}): { p with conf.default_host:(host,port) }
                   case (host,{none}): { p with conf.default_host:(host,p.conf.default_host.f2) }
                   }
                 }
             }
           }
        },
        {CommandLine.default_parser with
           names:["--{family_name}-auth{name}"],
           description:"Generic user authentication for connection {name}",
           param_doc:"user:password",
           on_param:function (p) {
             parser {
               case auth={auth_parser}:
                 {no_params:{p with conf.user:auth.user, conf.password:auth.password}}
             }
           }
        },
      ]
    }
  }

  private
  function command_line_conf(family_name, name) {
    match (Hashtbl.try_find(params, "{family_name}{name}")) {
    case {some:conf}: conf
    case {none}:
      conf = CommandLine.filter(family(family_name, name))
      Hashtbl.add(params, "{family_name}{name}", conf)
      conf
    }
  }

  private
  function ApigenLib.params overload(c0, c1) {
    c1 = if (c1.conf.default_host == default_conf_.default_host) {c1 with conf.default_host:c0.conf.default_host} else c1
    c1 = if (c1.conf.user == default_conf_.user) {c1 with conf.user:c0.conf.user} else c1
    c1 = if (c1.conf.password == default_conf_.password) {c1 with conf.password:c0.conf.password} else c1
    c1 = if (c1.conf.bufsize == default_conf_.bufsize) {c1 with conf.bufsize:c0.conf.bufsize} else c1
    c1 = if (c1.conf.poolmax == default_conf_.poolmax) {c1 with conf.poolmax:c0.conf.poolmax} else c1
    c1 = if (c1.conf.timeout == default_conf_.timeout) {c1 with conf.timeout:c0.conf.timeout} else c1
    c1 = if (c1.conf.verbose == default_conf_.verbose) {c1 with conf.verbose:c0.conf.verbose} else c1
    c1
  }

  /**
   * Returns configuration for the database [name], overloaded by the command
   * line options.
   * @param conf The default configuration.
   * @param name The name of the configuration.
   * @return The same configuration of [conf] but overloaded by the command line.
   */
  function get(conf, family_name, name) {
    overload(~{conf}, command_line_conf(family_name, name))
  }

  /**
   * As get but with the default configuration.
   */
  function get_default(family_name, name) { get(default_conf_, family_name, name).conf }

}

type ApigenLib.read_packet = Socket.connection, int, Mailbox.t -> outcome((Mailbox.t,binary),string)

/**
 * Basic connection information.
 * Has the configuration data plus the temporary data to manage the socket pool, etc.
 */
type ApigenLib.connection = {
  string name,
  ApigenLib.conf conf,
  bool retain, // total mass
  option(Socket.t) conn,
  SocketPool.t pool,
  ApigenLib.read_packet read_packet
}

private
type ApigenLib.sr =
     {recv} // Expect reply
  or {int recvraw} // Expect raw reply
  or {binary send} // Send and forget
  or {binary sendrecv} // Send and expect reply
  or {stop} // Stop the cell

private
type ApigenLib.srr =
     {ApigenLib.connection sendresult}
  or {(ApigenLib.connection,binary) sndrcvresult}
  or {(ApigenLib.connection,binary) rcvresult}
  or {(ApigenLib.connection,binary) rcvrawresult}
  or {stopresult}
  or {Apigen.failure failure}

module ApilibConnection(Socket.host default_host) {

  Conf = ApigenLibConf(default_host)

  private module Log {
    private function gen(f, m, string fn, msg) {
      if (m.conf.verbose) f("ApilibConnection({m.name}).{fn}", msg) else void
    }
    function info(m, fn, msg) { gen(@toplevel.Log.info, m, fn, msg) }
    function debug(m, fn, msg) { gen(@toplevel.Log.debug, m, fn, msg) }
    function error(m, fn, msg) { gen(@toplevel.Log.error, m, fn, msg) }
  }

  /** Default length object, no offset, big endian, signed and 32-bit length. */
  ApigenLib.length default_length = { offset : 0, le : false, signed : false, size : {L} }

  /**
   * Initialize a generalized connection.
   * @param name The name of the connection to open.
   * @param conf The configuration of the connection to open.
   * @param secure Optional SSL secure_type value.
   * @param preamble Optional function called on socket before SSL handshake.
   * @param retain Retain allocated sockets between calls.
   * @return A connection object (not connected).
   */
  function ApigenLib.connection init(family_name, name, secure, preamble, retain) {
    conf = Conf.get_default(family_name, name)
    ~{ name, conf, retain, conn:{none},
       pool:SocketPool.make_secure(conf.default_host,{hint:conf.bufsize, max:conf.poolmax, verbose:conf.verbose},secure,preamble),
       read_packet:read_packet_prefixed(default_length)
     }
  }

  /**
   * Install a custom packet reader.  Needed to ensure a complete packet is present
   * in the buffer when message decode takes place.
   * @param conn The connection object.
   * @param read_packet A function to perform the read (see read_packet_prefixed for an example).
   * @returns Updated connection object.
   */
  function ApigenLib.connection custom_read_packet(ApigenLib.connection conn, ApigenLib.read_packet read_packet) {
    {conn with ~read_packet}
  }

  /**
   * Ensure that a socket is allocated to the connection.
   * @param conn The connection object.
   */
  function Apigen.outcome(ApigenLib.connection) allocate(ApigenLib.connection conn) {
    match (conn.conn) {
    case {some:_}: {success:conn};
    case {none}:
      match (SocketPool.get(conn.pool)) {
      case ~{failure}: {failure:{socket:failure}};
      case {success:c}: {success:{conn with conn:{some:c}}};
      }
    }
  }

  /**
   * Release an allocated socket.
   * @param conn The connection object.
   */
  function ApigenLib.connection release(ApigenLib.connection conn) {
    match (conn.conn) {
    case {some:c}: SocketPool.release(conn.pool,c); {conn with conn:none};
    case {none}: conn;
    }
  }

  /**
   * Connect to the server (actually makes a physical connection).
   * @param conn The connection object.
   * @param host The socket host definition (host, port).
   * @returns Updated connection object with the physical connection installed.
   */
  function Apigen.outcome(ApigenLib.connection) connect(ApigenLib.connection conn, Socket.host host) {
    conn = release(conn);
    Log.info(conn, "connect","addr={host.f1} port={host.f2}")
    SocketPool.reconnect(conn.pool,(host.f1,host.f2))
    {success:conn}
  }

  /**
   * Check if the connection is ready.
   * Note that this will break the connection if it is already allocated.
   * @param m The connection to check
   * @returns connection object with the same allocated state as on call.
   */
  function Apigen.outcome(ApigenLib.connection) check(ApigenLib.connection conn) {
    allocated = Option.is_some(conn.conn)
    conn = release(conn)
    match (SocketPool.get(conn.pool)) {
    case ~{failure}: {failure:{socket:failure}}
    case {success:c}:
      conn =
        if (allocated)
          {conn with conn:{some:c}}
        else {
          SocketPool.release(conn.pool, c)
          conn
        }
      {success:conn}
    }
  }

  /**
   * Close the physical connection.
   */
  function ApigenLib.connection close(ApigenLib.connection conn) {
    conn = release(conn)
    Log.info(conn, "close","{SocketPool.gethost(conn.pool)}")
    SocketPool.stop(conn.pool)
    conn
  }

  private function set_mbox(c, mbox) {
    // We MUST update the mailbox in the connection before release.
    c =
      match ((mbox,c.conn)) {
      case ({some:mbox},{some:conn}): {c with conn:{some:{conn with ~mbox}}};
      default: c;
      }
    if (c.retain) c else release(c)
  }

  private function ApigenLib.srr send_no_reply(ApigenLib.connection c, binary msg) {
    match (c.conn) {
    case {some:conn}:
      len = Binary.length(msg)
      Log.debug(c, "send", "\n{bindump(msg)}")
      match (Socket.binary_write_len_with_err_cont(conn.conn,c.conf.timeout,msg,len)) {
      case {success:cnt}: if (cnt==len) {sendresult:set_mbox(c,none)} else {failure:{socket:"Write failure"}}
      case {~failure}: {failure:{socket:failure}};
      }
    case {none}:
      Log.error(c, "send", "No socket")
      {failure:{socket:"ApigenLib.send: No socket"}}
    }
  }

  private function ApigenLib.srr send_with_reply(ApigenLib.connection c, binary msg) {
    match (c.conn) {
    case {some:conn}:
       match (send_no_reply(c,msg)) {
       case {sendresult:c}:
         match (c.read_packet(conn.conn, c.conf.timeout, conn.mbox)) {
         case {success:(mailbox,reply)}:
           Log.debug(c, "receive", "\n{bindump(reply)}")
           {sndrcvresult:(set_mbox(c,{some:mailbox}),reply)}
         case {~failure}:
           Log.info(c, "receive","failure={failure}")
           _ = Mailbox.reset(conn.mbox)
           {failure:{socket:failure}}
         }
       case {~failure}:
         Log.info(c, "receive","failure={failure}")
         _ = Mailbox.reset(conn.mbox)
         {~failure};
       default: @fail("bad sendresult");
       }
    case {none}:
      Log.error(c, "receive","No socket")
      {failure:{socket:"No socket"}}
    }
  }

  private function ApigenLib.srr get_reply(ApigenLib.connection c) {
    match (c.conn) {
    case {some:conn}:
    match (c.read_packet(conn.conn, c.conf.timeout, conn.mbox)) {
    case {success:(mailbox,reply)}:
      Log.debug(c, "receive", "\n{bindump(reply)}")
      {rcvresult:(set_mbox(c,{some:mailbox}),reply)}
    case {~failure}:
      Log.info(c, "receive","failure={failure}")
      _ = Mailbox.reset(conn.mbox)
      {failure:{socket:failure}}
    }
    case {none}:
      Log.error(c, "receive","No socket")
      {failure:{socket:"No socket"}}
    }
  }

  private function ApigenLib.srr get_raw(ApigenLib.connection c, int no_bytes) {
    match (c.conn) {
    case {some:conn}:
    match (read_raw(conn.conn, c.conf.timeout, conn.mbox, no_bytes)) {
    case {success:(mailbox,reply)}:
      Log.debug(c, "receive", "\n{bindump(reply)}")
      {rcvrawresult:(set_mbox(c,{some:mailbox}),reply)}
    case {~failure}:
      Log.info(c, "receive","failure={failure}")
      _ = Mailbox.reset(conn.mbox)
      {failure:{socket:failure}}
    }
    case {none}:
      Log.error(c, "receive","No socket")
      {failure:{socket:"No socket"}}
    }
  }

  private function ApigenLib.srr srpool(ApigenLib.connection c, ApigenLib.sr msg) {
    match (allocate(c)) {
    case {success:c}:
      match (msg) {
      case {recv}: get_reply(c)
      case {recvraw:no_bytes}: get_raw(c,no_bytes)
      case {send:msg}: send_no_reply(c,msg)
      case {sendrecv:msg}: send_with_reply(c,msg)
      case {stop}: Log.debug(c, "srpool","stop"); @fail
      }
    case {~failure}:
      Log.error(c, "srpool","Can't get pool {failure}")
      {~failure}
    }
  }

  /**
   * Send a binary message to a connection, no reply expected.
   *
   * Note that we accept an outcome as a parameter so that we can chain these
   * function calls together, the first failure in the chain will terminate.
   *
   * @param conn The connection object outcome.
   * @param msg The binary message.
   * @returns Success void or a failure code.
   */
  function Apigen.outcome(ApigenLib.connection) snd(Apigen.outcome(ApigenLib.connection) conn, binary msg) {
    match (conn) {
    case {success:c}:
      match (srpool(c,{send:msg})) {
      case {~sendresult}: {success:sendresult};
      case {~failure}: {~failure};
      default: @fail
      }
    case {~failure}: {~failure};
    }
  }

  /**
   * Send a binary message to a connection and wait for a reply.
   *
   * Note that we accept an outcome as a parameter so that we can chain these
   * function calls together, the first failure in the chain will terminate.
   *
   * @param conn The connection object outcome.
   * @param msg The binary message.
   * @returns Success of a reply message or a failure code.
   */
  function Apigen.outcome((ApigenLib.connection,binary)) sndrcv(Apigen.outcome(ApigenLib.connection) conn, binary msg) {
    match (conn) {
    case {success:c}:
      match (srpool(c,{sendrecv:msg})) {
      case {~sndrcvresult}: {success:sndrcvresult};
      case {~failure}: {~failure};
      default: @fail;
      }
    case {~failure}: {~failure};
    }
  }

  /**
   * Wait for a reply from a connection.
   *
   * Note that we accept an outcome as a parameter so that we can chain these
   * function calls together, the first failure in the chain will terminate.
   *
   * @param conn The connection object outcome.
   * @returns Success of a reply message or a failure code.
   */
  function Apigen.outcome((ApigenLib.connection,binary)) rcv(Apigen.outcome(ApigenLib.connection) conn) {
    match (conn) {
    case {success:c}:
      match (srpool(c,{recv})) {
      case {~rcvresult}: {success:rcvresult};
      case {~failure}: {~failure};
      default: @fail;
      }
    case {~failure}: {~failure};
    }
  }

  /**
   * Wait for a raw reply from a connection.
   *
   * Note that we accept an outcome as a parameter so that we can chain these
   * function calls together, the first failure in the chain will terminate.
   *
   * @param conn The connection object outcome.
   * @param no_bytes The number of bytes to read.
   * @returns Success of a reply message or a failure code.
   */
  function Apigen.outcome((ApigenLib.connection,binary)) rcvraw(Apigen.outcome(ApigenLib.connection) conn, int no_bytes) {
    match (conn) {
    case {success:c}:
      match (srpool(c,{recvraw:no_bytes})) {
      case {~rcvrawresult}: {success:rcvrawresult};
      case {~failure}: {~failure};
      default: @fail;
      }
    case {~failure}: {~failure};
    }
  }

  /**
   * Read in a packet to a mailbox.
   *
   * This is a generic routine to ensure that there is a complete packet
   * present in the mailbox.  The assumption is that somewhere in the packet
   * is a value which can be read out to give the actual packet length.
   *
   * @param (first block) length The [ApigenLib.length] object describing the position of the packet length.
   * @param (second block) conn The connection object.
   * @param (second block) timeout The timeout for read operations.
   * @param (second block) mailbox The mailbox to read the data into (may have data on entry).
   * @returns The mailbox is guaranteed to contain at least one packet.
   */
  function outcome((Mailbox.t,binary),string) read_packet_prefixed(ApigenLib.length length
                                                                  )(Socket.connection conn, int timeout, Mailbox.t mailbox) {
    bound = length.offset + Pack.sizesize(length.size)
    //jlog("read: offset={length.offset} bound={bound}")
    match (Socket.read_fixed(conn, timeout, bound, mailbox)) {
    case {success:mailbox}:
      match (Pack.Decode.int(length.le, length.signed, length.size, mailbox.buf, mailbox.start+length.offset)) {
      case {success:len}:
        //jlog("read: len={len}")
        if (len+length.offset == bound) // the size was at the end
          Mailbox.sub(mailbox, length.offset+len)
        else if (len < bound)
          {failure:"Inconsistent packet size: len={len} bound={bound}"}
        else {
          //jlog("read: reading {len-bound}")
          match (Socket.read_fixed(conn, timeout, len-bound, mailbox)) {
          case {success:mailbox}: Mailbox.sub(mailbox, length.offset+len)
          case {~failure}: {~failure}
          }
        }
      case {~failure}: {~failure}
      }
    case {~failure}: {~failure}
    }
  }

  function outcome((Mailbox.t,binary),string) read_raw(Socket.connection conn, int timeout, Mailbox.t mailbox, int no_bytes) {
    //jlog("read_raw: no_bytes={no_bytes}")
    match (Socket.read_fixed(conn, timeout, no_bytes, mailbox)) {
    case {success:mailbox}: Mailbox.sub(mailbox, no_bytes);
    case {~failure}: {~failure};
    }
  }

}

type ApigenLib.parsed_xml_rule = { list(string) pp, string value }

type ApigenLib.parsed_xml_path_element = { string tag, list(ApigenLib.parsed_xml_rule) rules }

type ApigenLib.parsed_xml_path = list(ApigenLib.parsed_xml_path_element)

type ApigenLib.xml_extractor('a) = ApigenLib.parsed_xml_path, xmlns -> outcome(list('a),string)

module ApigenLibXml {

function dbg(where) {
  parser {
    Rule.debug_parse_string(function (s) {
      dots = if (String.length(s) > 20) { "..." } else ""
      s = String.sub(0,Int.min(20,String.length(s)),s)
      s = String.escape_non_utf8_special(s)
      jlog("{where}: {s}{dots}")
    }): void
  }
}

  path_string = parser { case /*dbg("pstr")*/ "\"" str=((!"\"" .)*) "\"": Text.to_string(str) }

  path_id = parser { case /*dbg("id")*/ id=([A-Za-z0-9_\-]+): Text.to_string(id); }

  dot = parser { case ".": void; }

  Parser.general_parser(list(string)) plain_tag = Rule.parse_list_sep(false, path_id, dot)

  Parser.general_parser(ApigenLib.parsed_xml_rule) path_rule = parser {
    //case pp=path_parser Rule.ws "=" Rule.ws value=path_string Rule.ws: ~{pp, value};
    case /*dbg("pr")*/ pp=plain_tag Rule.ws "=" Rule.ws value=path_string Rule.ws: ~{pp, value};
  }

  comma = parser { case ",": void; }

  Parser.general_parser(list(ApigenLib.parsed_xml_rule)) rules = Rule.parse_list_sep(false, path_rule, comma)

  Parser.general_parser(list(ApigenLib.parsed_xml_rule)) path_filter = parser {
    case /*dbg("pf")*/ "[" Rule.ws ~rules Rule.ws "]": rules;
  }

  Parser.general_parser(ApigenLib.parsed_xml_path_element) path_tag = parser {
    //case tag=path_id Rule.ws: ~{tag};
    case /*dbg("pt")*/ tag=path_id Rule.ws rules=path_filter?:
      match (option(list(ApigenLib.parsed_xml_rule)) rules) {
      case {some:rules}: ~{tag, rules};
      case {none}: ~{tag, rules:[]};
      };
  }

  Parser.general_parser(ApigenLib.parsed_xml_path_element) pp0 = parser { case /*dbg("pp0")*/ "." Rule.ws ~path_tag: path_tag }

  Parser.general_parser(list(ApigenLib.parsed_xml_path_element)) tag = parser { t=path_tag Rule.ws ~pp0*: [t|pp0]; }

  Parser.general_parser(ApigenLib.parsed_xml_path) path_parser = parser {
    case /*dbg("pp")*/ Rule.ws ~tag: tag;
  }

  function parse_path(string path) {
    match (Parser.try_parse(path_parser,path)) {
    case {some:pathdef}: {some:pathdef};
    case {none}: {none};
    }
  }

  function matches_rules(rules, xmlns) {
//jlog("{i}) {Ansi.print({cyan},"matches_rules")}: rules={Ansi.print({red},"{rules}")}")
//jlog("{i}) {Ansi.print({cyan},"matches_rules")}: xmlns={Ansi.print({blue},Xmlns.to_string(xmlns))}")
//res =
    List.exists(function (~{pp, value}) {
                  pp = List.map(function (tag) { ~{tag, rules:[]} },pp)
                  match (get_xml_string(pp, xmlns)) {
                  case {success:strs}: List.mem(value,strs);
                  default: false;
                  }
                },rules)
//jlog("{i}) {Ansi.print({cyan},"matches_rules")}: res={Ansi.print({yellow},"{res}")}")
//res
  }

  function matches_content(rules, list(xmlns) content) {
    rules == [] || List.exists(matches_rules(rules, _),content)
  }

//  cnt = Mutable.make(0)

  function outcome(list(xmlns),string) get_xml_elements(ApigenLib.parsed_xml_path pp, xmlns xmlns) {
//i = cnt.get(); cnt.set(i+1);
//jlog("{i}) get_xml_elements: pp={Ansi.print({magenta},"{pp}")} xmlns={Ansi.print({blue},Xmlns.to_string(xmlns))}")
    match (pp) {
      case []: {success:[xmlns]};
      case [{tag:pptag, ~rules}|next_pp]:
//jlog("{i}) rules:{Ansi.print({red},"{rules}")}")
        match (xmlns) {
        case {~text}: {failure:text};
        case {args:_, ~content, namespace:_, specific_attributes:_, xmlns:_, ~tag}:
//jlog("{i}) tag={Ansi.print({yellow},tag)} pptag={Ansi.print({cyan},pptag)}")
          if (pptag == tag) {
            recursive function list(xmlns) aux(list(xmlns) l) {
              match (l) {
              case []: list(xmlns) [];
              case [xmlns xmlns|l]:
//jlog("{i}) next_pp={Ansi.print({magenta},"{next_pp}")}")
//jlog("{i}) xmlns={Ansi.print({blue},Xmlns.to_string(xmlns))}")
                match (get_xml_elements(ApigenLib.parsed_xml_path next_pp, xmlns xmlns)) {
                case {success:list(xmlns) res}:
//jlog("{i}) res={Ansi.print({green},String.concat("\n",List.map(Xmlns.to_string,res)))}")
                      List.append(res,aux(l))
                default: aux(l);
                }
              }
            }
            if (matches_content(rules, content))
              {success:aux(content)}
            else
              {success:[]}
         } else
           {failure:"expected {pptag} got {tag}"};
        case {content_unsafe:_}: {failure:"content_unsafe"};
        case {fragment:_}: {failure:"fragment"};
        case {xml_dialect:_}: {failure:"xml_dialect"};
        }
    }
  }

  function find_xml_element(ApigenLib.parsed_xml_path pp, list(xmlns) xmlnss) {
    recursive function aux(xmlnss) {
      match (xmlnss) {
      case []: {failure:"not found"};
      case [xmlns|xmlnss]:
        match (get_xml_elements(pp,xmlns)) {
        case {~success}: {~success};
        default: aux(xmlnss);
        }
      }
    }
    aux(xmlnss)
  }

  function string_of_xml_element(xmlns) {
    match (xmlns) {
    case {~text}
    case {args:_, content:[{~text}], namespace:_, specific_attributes:_, xmlns:_, tag:_}: {success:text};
    default: {failure:"element {xmlns} is not text"};
    }
  }

  function outcome(list('b),'e) Outcome_fold_backwards(('a, outcome(list('b),'e) -> outcome(list('b),'e)) f,
                                                       outcome(list('a),'e) l,
                                                       outcome(list('b),'e) acc) {
    match (l) {
    case {success:l}: List.fold_backwards(f,l,acc);
    case {~failure}: {~failure};
    }
  }

  function outcome(list('b),'e) Outcome_map(('a -> outcome('b,'e)) f, outcome(list('a),'e) l) {
    Outcome_fold_backwards(function (a, l) {
                             match (l) {
                             case {success:l}:
                               match (f(a)) {
                               case {success:b}: {success:[b|l]};
                               case {~failure}: {~failure};
                               }
                             case {~failure}: {~failure};
                             }
                           }, l, {success:[]})
  }

  function outcome(list(string), string) get_xml_string(ApigenLib.parsed_xml_path pp, xmlns xmlns) {
    Outcome_map(string_of_xml_element,get_xml_elements(pp,xmlns))
  }

  function outcome(list(string), string) find_xml_string(ApigenLib.parsed_xml_path pp, list(xmlns) xmlnss) {
    Outcome_map(string_of_xml_element,find_xml_element(pp,xmlnss))
  }

  function get_xml_bool(pp, xmlns) {
    Outcome_map(function (str) {
      match (String.to_lower(str)) {
      case "true": {success:true};
      case "false": {success:false};
      default: {failure:"{str} is not bool"};
      }
    },get_xml_string(pp, xmlns))
  }

  function get_xml_int(pp, xmlns) {
    Outcome_map(function (str) {
      match (Int.of_string_opt(str)) {
      case {some:i}: {success:i};
      default: {failure:"{str} is not int"};
      }
    },get_xml_string(pp,xmlns))
  }

  function list(xmlns) make_simple_sequence(string namespace, ApigenLib.simple_seq seq) {
    List.map(function ((tag,gv)) {
               list(xmlns) content =
                 match (gv) {
                 case {~String}: [{text:String}];
                 case {Int:i}: [{text:Int.to_string(i)}];
                 case {Bool:b}: [{text:Bool.to_string(b)}];
                 case {Float:f}: [{text:Float.to_string(f)}];
                 case {Binary:bin}:
                   [{args:[], content:[{text:%%bslBinary.to_encoding%%(bin,"binary")}],
                     ~namespace, specific_attributes:none, xmlns:[], tag:"Opaque"}];
                 case {Datetime:d}: [{text:ApigenLib.date_to_string2(d)}];
                 case {~Verbatim}: [Verbatim];
                 case {Empty}: [];
                 }
               {args:[], ~content, ~namespace, specific_attributes:none, xmlns:[], ~tag}
             },seq)
  }

  function list(xmlns) xmlnsl0() { [] }

  function list(xmlns) xmlnsl(xmlns xmlns) { [xmlns] }

  function gettag_unknown(list(xmlns) content) { {some:content} }

  function gettag_label(list(xmlns) _content) { {some:{}} }
  function gettag_empty(list(xmlns) content) { {some:true} }

  function gettag_value(get, list(xmlns) content) {
    match (content) {
    case [{~text}|_]: get(text);
    default: none;
    }
  }

  gettag_string = gettag_value(some,_)
  gettag_binary = gettag_value(function (s) { {some:%%bslBinary.of_encoding%%(s,"binary")} },_)
  gettag_int = gettag_value(Int.of_string_opt,_)
  gettag_bool = gettag_value(Bool.of_string,_)
  gettag_float = gettag_value(Float.of_string_opt,_)

  function is_tag(tag, xmlns) {
    match (xmlns) {
    case {args:_, content:_, namespace:_, specific_attributes:_, tag:xtag, xmlns:_}: tag == xtag;
    default: false;
    }
  }

  function get_xmlns_tag(tag, list(xmlns) content, strictness) {
    match (strictness) {
    case {first}: match (List.nth(0,content)) { case {some:e}: {some:[e]}; default: none; };
    case {first_match}: match (List.find(is_tag(tag,_),content)) { case {some:e}: {some:[e]}; default: none; };
    case {all_matches}: {some:List.filter(is_tag(tag,_),content)};
    case {all}: {some:content};
    }
  }

  function get_rec(list(xmlns) content, dflt, set) {
    //jlog("get_rec: content={String.concat("\n",List.map(Xmlns.to_string,content))}")
    List.fold(function (xmlns, record) {
                match (record) {
                case {some:record}:
                  match (xmlns) {
                  case {args:_, ~content, namespace:_, specific_attributes:_, ~tag, xmlns:_}:
                    match (set(tag, record, content)) {
                    case {some:record}: {some:record};
                    case {none}: {some:record};
                    };
                  default: {some:record};
                  }
                case {none}: none;
                }
              },content,{some:dflt})
  }

  function option('a) get_alt(list(xmlns) content, (string, list(xmlns) -> option('a)) set) {
    //jlog("get_alt: content={String.concat("\n",List.map(Xmlns.to_string,content))}")
    recursive function aux(content) {
      match (content) {
      case [xmlns|content]:
        match (xmlns) {
        case {args:_, content:tcontent, namespace:_, specific_attributes:_, ~tag, xmlns:_}:
          //jlog("get_alt: tag={tag}")
          match (set(tag, tcontent)) {
          case {some:res}: {some:res};
          case {none}: aux(content);
          }
        default: aux(content);
        }
      case []: none;
      }
    }
    aux(content)
  }

  function get_option(list(xmlns) content, get) {
    //jlog("get_option: content={String.concat("\n",List.map(Xmlns.to_string,content))}")
    match (get(content)) {
    case {some:v}: {some:{some:v}};
    case {none}: {some:{none}};
    }
  }

  function get_list(list(xmlns) content, get) {
    //jlog("get_list: content={String.concat("\n",List.map(Xmlns.to_string,content))}")
    {some:List.filter_map(function (xmlns) { get([xmlns]) },content)}
  }

  function dorec(r, get, set, content) {
    //jlog("dorec: content={String.concat("\n",List.map(Xmlns.to_string,content))}")
    match (get(content)) {
    case {some:value}: {some:set(r, value)};
    case {none}: none;
    }
  }

  function doalt(get, mk, content) {
    //jlog("doalt: content={String.concat("\n",List.map(Xmlns.to_string,content))}")
    match (get(content)) {
    case {some:value}: {some:mk(value)};
    case {none}: none;
    }
  }
    
}
