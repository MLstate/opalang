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
import stdlib.apis.common
import stdlib.apis.oauth
import stdlib.io.socket

type Apigen.expected_type =
    {BOOL}
 or {INT}
 or {OBJECT}
 or {LIST}

type Apigen.failure =
    {string bad_path}
 or {WebClient.failure network}
 or {RPC.Json.json error_json}
 or {RPC.Json.json bad_json}
 or {string post_process}
 or {string socket}
 or {string pack}

type Apigen.outcome('a) = outcome('a,Apigen.failure)

type ApigenAuth.credentials = {
  string access_token,
  string access_secret
}

private (binary -> string) bindump = %% BslPervasives.bindump %%

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

  private function generic_build_path(path, options) {
    if (options == []) path else "{path}?{API_libs.form_urlencode(options)}"
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
  function POST(base, path, options, content, parse_fun) {
    //API_libs_private.apijlog("POST {base}{path}\n{content}\n")
    final_path = generic_build_path("{base}{path}", options)
    match (Uri.of_string(final_path)) {
    case {none}: {failure:{bad_path:final_path}};
    case {some:uri}:
      match (WebClient.Post.try_post(uri,content)) {
      case {success:res}: parse_fun(res)
      case {failure:f}: {failure:{network:f}}
      }
    }
  }

  /**
   * Make a HTTP POST on [path] at [base] with form [data]
   */
  function POST_FORM(base, path, options, data, parse_fun) {
    content = API_libs.form_urlencode(data)
    POST(base, path, options, content, parse_fun)
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

}

type ApigenLib.read_packet =
  Socket.connection, int, Mailbox.t, int, bool, bool, Pack.s -> outcome((Mailbox.t,binary),string)

/**
 *
 */
abstract
type ApigenLib.connection = {
  string name,
  ApigenLib.conf conf,
  option(Socket.t) conn,
  SocketPool.t pool,
  ApigenLib.read_packet read_packet
}

/**
 *
 */
type ApigenLib.conf = {
  Socket.host default_host,
  int bufsize,
  int poolmax,
  int timeout,
  bool verbose,
  int length_offset,
  bool length_le,
  bool length_signed,
  Pack.s length_size
}

type ApigenLib.sr =
     {(ApigenLib.connection) recv} // Expect reply
  or {(ApigenLib.connection,binary) send} // Send and forget
  or {(ApigenLib.connection,binary) sendrecv} // Send and expect reply
  or {stop} // Stop the cell

type ApigenLib.srr =
     {Apigen.outcome(void) sendresult}
  or {Apigen.outcome(binary) sndrcvresult}
  or {Apigen.outcome(binary) rcvresult}
  or {stopresult}
  or {Apigen.failure failure}

module ApilibConnection(Socket.host default_host) {

  private module Log {
    private function gen(f, m, string fn, msg) {
      if (m.conf.verbose) f("ApilibConnection({m.name}).{fn}", msg) else void
    }
    function info(m, fn, msg) { gen(@toplevel.Log.info, m, fn, msg) }
    function debug(m, fn, msg) { gen(@toplevel.Log.debug, m, fn, msg) }
    function error(m, fn, msg) { gen(@toplevel.Log.error, m, fn, msg) }
  }

  ApigenLib.conf default_conf = ~{
    default_host,
    bufsize : 50 * 1024,
    poolmax : 10,
    timeout : 60 * 60 * 1000,
    verbose : false,
    length_offset : 1,
    length_le : false,
    length_signed : false,
    length_size : {L}
  }

  /**
   * Initialize a generalized connection.
   * @param name The name of the connection to open.
   * @param conf The configuration of the connection to open. See [default_conf].
   * @return A connection object (not connected).
   **/
  function ApigenLib.connection init(name, ApigenLib.conf _conf) {
    conf = default_conf //get_conf(conf, name)
    ~{ name,
       conf,
       conn:{none},
       pool:SocketPool.make(conf.default_host,{hint:conf.bufsize, max:conf.poolmax, verbose:conf.verbose}),
       read_packet:read_packet_prefixed
     }
  }

  function ApigenLib.connection custom_read_packet(ApigenLib.connection c, ApigenLib.read_packet read_packet) {
    {c with ~read_packet}
  }

  function Apigen.outcome(ApigenLib.connection) connect(ApigenLib.connection conn, Socket.host host) {
    Log.info(conn, "connect","addr={host.f1} port={host.f2}")
    SocketPool.reconnect(conn.pool,(host.f1,host.f2))
    {success:conn}
  }

  /**
   * Check if the connection is ready
   * @param m The connection to check
   * @return
   */
  function Apigen.outcome(ApigenLib.connection) check(ApigenLib.connection conn) {
    match (SocketPool.get(conn.pool)) {
    case ~{failure}: {failure:{socket:failure}}
    case {success:c}:
      SocketPool.release(conn.pool, c)
      {success:conn}
    }
  }

  function ApigenLib.connection close(ApigenLib.connection conn) {
    Log.info(conn, "close","{SocketPool.gethost(conn.pool)}")
    SocketPool.stop(conn.pool)
    conn
  }

  private function Apigen.outcome(void) send_no_reply_(ApigenLib.connection c, binary msg, bool _reply_expected) {
    match (c.conn) {
    case {some:conn}:
      len = Binary.length(msg)
      Log.debug(c, "send", "\n{bindump(msg)}")
      match (Socket.binary_write_len_with_err_cont(conn.conn,c.conf.timeout,msg,len)) {
      case {success:cnt}: if (cnt==len) {success:void} else {failure:{socket:"Write failure"}}
      case {~failure}: {failure:{socket:failure}};
      }
    case {none}:
      Log.error(c, "send", "No socket")
      {failure:{socket:"ApigenLib.send: No socket"}}
    }
  }

  private function Apigen.outcome(void) send_no_reply(ApigenLib.connection c, binary msg) { send_no_reply_(c,msg,false) }

  private function (option(Mailbox.t),Apigen.outcome(binary)) send_with_reply(ApigenLib.connection c, binary msg) {
    match (c.conn) {
    case {some:conn}:
       match (send_no_reply_(c,msg,true)) {
       case {success:_}:
         match (c.read_packet(conn.conn, c.conf.timeout, conn.mbox,
                              c.conf.length_offset, c.conf.length_le, c.conf.length_signed, c.conf.length_size)) {
         case {success:(mailbox,reply)}:
           Log.debug(c, "receive", "\n{bindump(reply)}")
           ({some:mailbox},{success:reply})
         case {~failure}:
           Log.info(c, "receive","failure={failure}")
           _ = Mailbox.reset(conn.mbox)
           ({none},{failure:{socket:failure}})
         }
       case {~failure}:
         Log.info(c, "receive","failure={failure}")
         _ = Mailbox.reset(conn.mbox)
         ({none},{~failure});
       }
    case {none}:
      Log.error(c, "receive","No socket")
      ({none},{failure:{socket:"No socket"}})
    }
  }

  private function (option(Mailbox.t),Apigen.outcome(binary)) get_reply(ApigenLib.connection c) {
    match (c.conn) {
    case {some:conn}:
    match (c.read_packet(conn.conn, c.conf.timeout, conn.mbox,
                         c.conf.length_offset, c.conf.length_le, c.conf.length_signed, c.conf.length_size)) {
    case {success:(mailbox,reply)}:
      Log.debug(c, "receive", "\n{bindump(reply)}")
      ({some:mailbox},{success:reply})
    case {~failure}:
      Log.info(c, "receive","failure={failure}")
      _ = Mailbox.reset(conn.mbox)
      ({none},{failure:{socket:failure}})
    }
    case {none}:
      Log.error(c, "receive","No socket")
      ({none},{failure:{socket:"No socket"}})
    }
  }

  private function sr_snr(ApigenLib.connection c, binary msg) {
    sr = send_no_reply(c,msg)
    ({none},{sendresult:sr})
  }

  private function sr_swr(ApigenLib.connection c, binary msg) {
    (mbox,swr) = send_with_reply(c,msg)
    (mbox,{sndrcvresult:swr})
  }

  private function sr_gr(ApigenLib.connection c) {
    (mbox,gr) = get_reply(c)
    (mbox,{rcvresult:gr})
  }

  private function ApigenLib.srr srpool(ApigenLib.connection c, ApigenLib.sr msg) {
    match (SocketPool.get(c.pool)) {
    case {success:connection}:
      conn = {some:connection}
      (mbox,result) =
        match (msg) {
        case {recv:c}: sr_gr({c with ~conn})
        case {send:(c,msg)}: sr_snr({c with ~conn},msg)
        case {sendrecv:(c,msg)}: sr_swr({c with ~conn},msg)
        case {stop}: Log.debug(c, "srpool","stop"); @fail
        }
      connection =
        match (mbox) {
        case {some:mbox}: {connection with ~mbox}
        case {none}: connection
        }
      SocketPool.release(c.pool,connection)
      result
    case {~failure}:
      Log.error(c, "srpool","Can't get pool {failure}")
      {failure:{socket:failure}}
    }
  }

  function Apigen.outcome(void) snd(Apigen.outcome(ApigenLib.connection) c, binary msg) {
    match (c) {
    case {success:c}:
      match (srpool(c,{send:((c,msg))})) {
      case {~sendresult}: sendresult;
      case {~failure}: {~failure};
      default: @fail
      }
    case {~failure}: {~failure};
    }
  }

  function Apigen.outcome(binary) sndrcv(Apigen.outcome(ApigenLib.connection) c, binary msg) {
    match (c) {
    case {success:c}:
      match (srpool(c,{sendrecv:((c,msg))})) {
      case {~sndrcvresult}: sndrcvresult;
      case {~failure}: {~failure};
      default: @fail;
      }
    case {~failure}: {~failure};
    }
  }

  function Apigen.outcome(binary) rcv(Apigen.outcome(ApigenLib.connection) c) {
    match (c) {
    case {success:c}:
      match (srpool(c,{recv:c})) {
      case {~rcvresult}: rcvresult;
      case {~failure}: {~failure};
      default: @fail;
      }
    case {~failure}: {~failure};
    }
  }

  function outcome((Mailbox.t,binary),string) read_packet_prefixed(Socket.connection conn, int timeout, Mailbox.t mailbox,
                                                                   int offset, bool le, bool signed, Pack.s size) {
    bound = offset + Pack.sizesize(size)
    //jlog("read: offset={offset} bound={bound}")
    match (Socket.read_fixed(conn, timeout, bound, mailbox)) {
    case {success:mailbox}:
      match (Pack.Decode.int(le, signed, size, mailbox.buf, mailbox.start+offset)) {
      case {success:len}:
        //jlog("read: len={len}")
        if (len+offset == bound) // the size was at the end
          Mailbox.sub(mailbox, len)
        else if (len < bound)
          {failure:"Inconsistent packet size: len={len} bound={bound}"}
        else {
          //jlog("read: reading {len-bound}")
          match (Socket.read_fixed(conn, timeout, len-bound, mailbox)) {
          case {success:mailbox}: Mailbox.sub(mailbox, offset+len)
          case {~failure}: {~failure}
          }
        }
      case {~failure}: {~failure}
      }
    case {~failure}: {~failure}
    }
  }

}
