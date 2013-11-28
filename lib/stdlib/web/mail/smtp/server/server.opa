/*
    Copyright © 2011, 2012, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin mail

import stdlib.web.mail
import stdlib.web.utils
import stdlib.crypto

(binary -> string) bindump = %% BslPervasives.bindump %%

type SmtpServer.email = {
  string from,
  list(string) to,
  iter(binary) body
}

abstract type SmtpServer.smtp_server = reference({
  RAIServer.raiserver raiserver,
  string server_domain,
  string hello_message,
  int max_size,
  list(string) auth_methods,
  (SmtpServer.email -> (int, string)) email_callback,
  (string -> (int, string)) verify_callback,
  (string -> list((int, string))) expand_callback,
  (-> void) tls_callback,
  (string, string -> bool) auth_callback,
  (V8.Error -> void) error_callback
})

private type SmtpServer.mode =
    {message}
 or {data}
 or {plain}
 or {loginuser}
 or {loginpass}
 or {quit}
 or {closed}

private type SmtpServer.state = reference({
  RAIServer.raiclient raiclient,
  SmtpServer.mode mode,
  SmtpServer.email email,
  string client_domain,
  bool extended,
  string username,
  bool authenticated
})

module SmtpServer {

  bool verbose = true

  private module Log {
    @expand function void gen(f, string msg) { if (verbose) f("SmtpServer", Ansi.printc(msg)) else void }
    @expand function void info(msg) { gen(@toplevel.Log.info, msg) }
    @expand function void debug(msg) { gen(@toplevel.Log.debug, msg) }
    @expand function void error(msg) { gen(@toplevel.Log.error, msg) }
  }

  private module SmtpServer_private {

    function send_single(RAIServer.raiclient raiclient, (int,string) (code,msg)) {
      RAIServer.send(raiclient, "{code} {msg}")
    }

    function send_multi(RAIServer.raiclient raiclient, list((int,string)) msgs) {
      len = List.length(msgs)
      List.iteri(function (i,(c,m)) {
                   s = if (i == len-1) " " else "-"
                   RAIServer.send(raiclient, "{c}{s}{m}")
                 },msgs)
    }

    function send_multi_code(RAIServer.raiclient raiclient, int code, list(string) messages) {
      send_multi(raiclient, List.map(function (msg) { (code,msg) },messages))
    }

    function void command_common(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, string command, string payload) {
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (command) {
      case "vrfy":
        srvr = ServerReference.get(srvrref)
        send_single(raiclient, srvr.verify_callback(payload));
      case "expn":
        srvr = ServerReference.get(srvrref)
        send_multi(raiclient, srvr.expand_callback(payload));
      case "noop":
        RAIServer.send(raiclient, "250 Ok")
      case "rset":
        ServerReference.set(stateref, ~{state with mode:{message}});
        RAIServer.send(raiclient, "250 Ok")
      case "quit":
        RAIServer.send(raiclient, "221 Bye")
        RAIServer.client_end(raiclient);
        ServerReference.set(stateref, ~{state with mode:{closed}});
      default:
        Log.error("Bad command {command}")
        RAIServer.send(raiclient, "500 Command not recognised")
      }
    }

    function void mode_common(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      srvr = ServerReference.get(srvrref)
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (cb) {
      case ~{command, payload}:
        command_common(srvrref, stateref, String.lowercase(command), payload);
      case ~{error}:
        ServerReference.get(srvrref).error_callback(error);
        RAIServer.send(raiclient, "451 Server error")
        RAIServer.client_end(raiclient);
      case {timeout}:
        Log.error("Timeout")
        RAIServer.send(raiclient, "421 4.4.2 {srvr.server_domain} Timeout")
      case {tls}:
        srvr.tls_callback();
      default:
        Log.error("Unhandled {cb}")
        RAIServer.send(raiclient, "503 Bad sequence of commands")
      }
    }

    function SmtpServer.email init_email() { {from:"", to:[], body:Iter.empty} }

    function void send_greeting(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, string client_domain, bool extended) {
      srvr = ServerReference.get(srvrref)
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      if (client_domain == "")
        RAIServer.send(raiclient, "501 Syntax: {if (extended) "EHLO" else "HELO"} hostname")
      else {
        msgs = ["Hello {client_domain}"]
        msgs =
         if (extended) {
           List.flatten([msgs,
                         ["8BITMIME","ENHANCEDSTATUSCODES"],
                         if (srvr.max_size > 0) { ["SIZE {srvr.max_size}"] } else [],
                         if (List.length(srvr.auth_methods) > 0) {
                           ["AUTH "+String.concat(" ",srvr.auth_methods),
                            "AUTH="+String.concat(" ",srvr.auth_methods)]
                         } else [],
                         ["STARTTLS"]
                        ])
         } else msgs
        send_multi_code(raiclient, 250, msgs)
        ServerReference.set(stateref, ~{state with mode:{message}, email:init_email(), client_domain, extended});
      }
    }

    function option(int) indexr(string substr, string str) {
      match (String.index(String.rev(substr),String.rev(str))) {
      case {some:i}: {some:String.length(str)-i-String.length(substr)};
      case {none}: {none};
      }
    }

    function option(string) between_angles(string prefix, string str) {
      if (String.has_prefix(String.lowercase(prefix)^":", String.lowercase(str))) {
        match ((String.index("<",str),indexr(">",str))) {
        case ({some:start},{some:finish}): {some:String.sub(start+1,finish-start-1,str)};
        default: {none};
        }
      } else {none}
    }

    function (int,string) get_cstring(binary bin, int pos) {
      len = Binary.length(bin)
      recursive function aux(int pos, string str) {
        if (pos >= len)
          (pos,str)
        else if (Binary.get_int8(bin,pos) == 0)
          (pos+1,str)
        else {
          aux(pos+1,str^Binary.get_string(bin,pos,1))
        }
      }
      aux(pos,"")
    }

    function (int,list(string)) get_cstrings(binary bin, int pos) {
      len = Binary.length(bin)
      recursive function aux(int pos, list(string) strs) {
        if (pos >= len)
          (pos,List.rev(strs))
        else {
          (pos,str) = get_cstring(bin,pos)
          aux(pos,[str|strs])
        }
      }
      aux(pos,[])
    }

    function string check_auth(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, string username, string password) {
      state = ServerReference.get(stateref)
      if (ServerReference.get(srvrref).auth_callback(username, password)) {
        ServerReference.set(stateref, {state with mode:{message}, username:"", authenticated:true})
        "235 2.7.0 Authentication successful"
      } else {
        ServerReference.set(stateref, {state with mode:{message}, username:"", authenticated:false})
        "535 5.7.8 Error: authentication failed: generic failure"
      }
    }

    function string authplain(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, string authdata) {
      state = ServerReference.get(stateref)
      authdata = Crypto.Base64.decode(authdata)
      (_,strs) = get_cstrings(authdata,0)
      match (strs) {
      case ["",username,password]
      case [username,_,password]:
        check_auth(srvrref, stateref, username, password);
      default:
        ServerReference.set(stateref, {state with username:"", authenticated:false})
        "500 5.5.2 Error: invalid userdata to decode"
      }
    }

    function is_authenticated(SmtpServer.smtp_server srvrref, SmtpServer.state stateref) {
      srvr = ServerReference.get(srvrref)
      state = ServerReference.get(stateref)
      List.length(srvr.auth_methods) == 0 || state.authenticated
    }

    function void message_mode(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (cb) {
      case ~{command, payload}:
        Log.info("%mcommand%d: %g{command}%d %y{payload}%d")
        match ((is_authenticated(srvrref,stateref),String.lowercase(command))) {
        case (_,"ehlo"):
          send_greeting(srvrref, stateref, payload, true);
        case (_,"helo"):
          send_greeting(srvrref, stateref, payload, false);
        case ({true},"mail"):
          match (between_angles("from", payload)) {
          case {some:from}:
            Log.info("from: %g{from}%d")
            // TODO: validate email address
            if (state.email.from == "") {
              ServerReference.set(stateref, {state with email:{state.email with ~from}})
              RAIServer.send(raiclient, "250 Ok")
            } else {
              RAIServer.send(raiclient, "503 5.5.1 multiple MAIL commands")
            }
          case {none}:
            RAIServer.send(raiclient, "500 Command not recognised")
          }
        case ({true},"rcpt"):
          if (state.email.from == "")
            RAIServer.send(raiclient,"503 5.5.1 MAIL command required")
          else {
            match (between_angles("to", payload)) {
            case {some:to}:
              Log.info("to: %g{to}%d")
              ServerReference.set(stateref, {state with email:{state.email with to:[to|state.email.to]}})
              RAIServer.send(raiclient, "250 Ok")
            case {none}:
              RAIServer.send(raiclient, "500 Command not recognised")
            }
          }
        case ({true},"data"): 
          if (List.length(state.email.to) <= 0)
            RAIServer.send(raiclient,"503 5.5.1 RCPT command required")
          else {
            RAIServer.send(raiclient, "354 End data with <CR><LF>.<CR><LF>");
            ServerReference.set(stateref, ~{state with mode:{data}});
            RAIServer.client_start_data_sequence(raiclient, ".");
          }
        case (_,"starttls"): 
          RAIServer.send(raiclient, "220 2.0.0 Ready to start TLS");
          RAIServer.client_start_tls(raiclient);
          ServerReference.set(stateref, ~{state with mode:{message}});
        case (_,"auth"):
          words = String.explode(" ",payload)
          match ((Option.map(String.lowercase,List.nth(0,words)),List.nth(1,words))) {
          case ({some:"plain"},{some:authdata}):
            msg = authplain(srvrref, stateref, authdata)
            RAIServer.send(raiclient, msg);
          case ({some:"plain"},{none}):
            ServerReference.set(stateref, ~{state with mode:{plain}});
            RAIServer.send(raiclient, "334");
          case ({some:"login"},{some:username}):
            ServerReference.set(stateref, ~{state with mode:{loginpass}, username});
            RAIServer.send(raiclient, "334");
          case ({some:"login"},_):
            ServerReference.set(stateref, ~{state with mode:{loginuser}});
            RAIServer.send(raiclient, "334");
          default:
            RAIServer.send(raiclient, "535 5.7.8 Error: authentication failed: no mechanism available")
          }
        case ({false},_):
          RAIServer.send(raiclient,"530 5.5.1 Authentication Required");
        case (_,command):
          command_common(srvrref, stateref, command, payload);
        }
      default:
        mode_common(srvrref, stateref, cb);
      }
    }

    function iter('a) irev(iter('a) i) { Iter.of_list(List.rev(Iter.to_list(i))) }

    function void data_mode(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (cb) {
      case {~data}:
        ServerReference.set(stateref, {state with email:{state.email with body:Iter.cons(data,state.email.body)}});
        Log.info("%gdata%d")
        Log.info("data:\n%c{bindump(data)}%d")
      case {ready}:
        data = binary_of_string("\r\n")
        ServerReference.set(stateref, ~{state with mode:{quit}, email:{state.email with body:Iter.cons(data,state.email.body)}});
        Log.info("%rready%d")
        Log.info("data:\n%c{bindump(data)}%d")
        (code, message) = ServerReference.get(srvrref).email_callback({state.email with body:irev(state.email.body)})
        RAIServer.send(raiclient, "{code} {message}");
      default:
        mode_common(srvrref, stateref, cb);
      }
    }

    function void plain_mode(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (cb) {
      case ~{command, payload}:
        Log.info("%mcommand%d: %g{command}%d %y{payload}%d")
        RAIServer.send(raiclient,authplain(srvrref, stateref, command))
      default:
        ServerReference.set(stateref, {state with mode:{message}, username:"", authenticated:false})
        RAIServer.send(raiclient,"535 5.7.8 Error: authentication failed: generic failure")
      }
    }

    function void loginuser_mode(SmtpServer.smtp_server _srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (cb) {
      case ~{command, payload}:
        Log.info("%mcommand%d: %g{command}%d %y{payload}%d")
        username = string_of_binary(Crypto.Base64.decode(command))
        ServerReference.set(stateref, {state with mode:{loginpass}, ~username})
        RAIServer.send(raiclient,"334")
      default:
        ServerReference.set(stateref, {state with mode:{message}, username:"", authenticated:false})
        RAIServer.send(raiclient,"535 5.7.8 Error: authentication failed: generic failure")
      }
    }

    function void loginpass_mode(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      state = ServerReference.get(stateref)
      raiclient = state.raiclient
      match (cb) {
      case ~{command, payload}:
        Log.info("%mcommand%d: %g{command}%d %y{payload}%d")
        password = string_of_binary(Crypto.Base64.decode(command))
        RAIServer.send(raiclient,check_auth(srvrref, stateref, state.username, password))
      default:
        ServerReference.set(stateref, {state with mode:{message}, username:"", authenticated:false})
        RAIServer.send(raiclient,"535 5.7.8 Error: authentication failed: generic failure")
      }
    }

    function void quit_mode(SmtpServer.smtp_server srvrref, SmtpServer.state stateref, RAIServer.client_callback cb) {
      mode_common(srvrref, stateref, cb);
    }

    function void client_callback(SmtpServer.smtp_server srvrref, SmtpServer.state stateref,
                                  RAIServer.raiserver _raiserver, RAIServer.raiclient _raiclient, RAIServer.client_callback cb) {
      state = ServerReference.get(stateref)
      match (state.mode) {
      case {message}: message_mode(srvrref, stateref, cb);
      case {data}: data_mode(srvrref, stateref, cb);
      case {plain}: plain_mode(srvrref, stateref, cb);
      case {loginuser}: loginuser_mode(srvrref, stateref, cb);
      case {loginpass}: loginpass_mode(srvrref, stateref, cb);
      case {quit}: quit_mode(srvrref, stateref, cb);
      case {closed}: void;
      }
    }

    function void server_callback(SmtpServer.smtp_server srvrref, RAIServer.raiserver raiserver, RAIServer.server_callback cb) {
      Log.info("%cconnect%d")
      srvr = ServerReference.get(srvrref)
      match (cb) {
      case {connect:raiclient}:
        RAIServer.send(raiclient, "220 {srvr.server_domain} ESMTP {srvr.hello_message}")
        stateref = ServerReference.create({~raiclient, mode:{message}, email:init_email(), client_domain:"", extended:false,
                                           username:"", authenticated:false})
        RAIServer.client_callback(raiserver, raiclient, client_callback(srvrref,stateref,_,_,_))
      case ~{error}:
        Log.info("Server %r{error.name}%d: %r{error.message}%d\n{error.stack}")
      }
    }

    function (int,string) default_email_callback(SmtpServer.email email) {
      Log.info("Email:")
      Log.info("  From: %y{email.from}%d")
      List.iter(function (to) { Log.info("  To: %y{to}%d") },List.rev(email.to))
      Iter.iter(function (bin) { Log.info("  Data:\n{string_of_binary(bin)}") },email.body)
      (250,"Ok")
    }

    function (int,string) default_verify_callback(string str) {
      Log.info("Verify: {str}")
      (502,"VRFY command is disabled")
    }

    function list((int,string)) default_expand_callback(string str) {
      Log.info("Expand: {str}")
      [(502,"EXPN command is disabled")]
    }

    function void default_tls_callback() {
      Log.info("Start TLS")
    }

    function bool default_auth_callback(string username, string _password) {
      Log.info("Unhandled AUTH for user {username}")
      false
    }

    function void default_error_callback(V8.Error error) {
      Log.info("Client %r{error.name}%d: %r{error.message}%d\n{error.stack}")
    }

  } // End of SmtpServer_private

  function void setEmailCallback(SmtpServer.smtp_server srvrref, (SmtpServer.email -> (int, string)) email_callback) {
    ServerReference.set(srvrref, {ServerReference.get(srvrref) with ~email_callback})
  }

  function void setVerifyCallback(SmtpServer.smtp_server srvrref, (string -> (int, string)) verify_callback) {
    ServerReference.set(srvrref, {ServerReference.get(srvrref) with ~verify_callback})
  }

  function void setExpandCallback(SmtpServer.smtp_server srvrref, (string -> list((int, string))) expand_callback) {
    ServerReference.set(srvrref, {ServerReference.get(srvrref) with ~expand_callback})
  }

  function void setTlsCallback(SmtpServer.smtp_server srvrref, (-> void) tls_callback) {
    ServerReference.set(srvrref, {ServerReference.get(srvrref) with ~tls_callback})
  }

  function void setAuthCallback(SmtpServer.smtp_server srvrref, (string, string -> bool) auth_callback) {
    ServerReference.set(srvrref, {ServerReference.get(srvrref) with ~auth_callback})
  }

  function void setErrorCallback(SmtpServer.smtp_server srvrref, (V8.Error -> void) error_callback) {
    ServerReference.set(srvrref, {ServerReference.get(srvrref) with ~error_callback})
  }

  function SmtpServer.smtp_server runServer(string host, int port, RAIServer.options options,
                                            string server_domain, string hello_message, int max_size, list(string) auth_methods) {
    srvr = ~{raiserver:RAIServer.make_server_options(port, host, options),
             server_domain, hello_message, max_size, auth_methods,
             email_callback:SmtpServer_private.default_email_callback,
             verify_callback:SmtpServer_private.default_verify_callback,
             expand_callback:SmtpServer_private.default_expand_callback,
             tls_callback:SmtpServer_private.default_tls_callback,
             auth_callback:SmtpServer_private.default_auth_callback,
             error_callback:SmtpServer_private.default_error_callback}
    srvrref = ServerReference.create(srvr)
    RAIServer.server_callback(srvr.raiserver, SmtpServer_private.server_callback(srvrref,_,_))
    srvrref
  }

  function void shutdownServer(SmtpServer.smtp_server srvrref, (void -> void) callback) {
    RAIServer.close_server(ServerReference.get(srvrref).raiserver, callback)
  }

}


