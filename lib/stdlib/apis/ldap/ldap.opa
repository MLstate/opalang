/*
    Copyright © 2011, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package stdlib.apis.ldap

import-plugin mail

import stdlib.crypto

/**
 * Ldap client binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * This is a binding for Ldap, based upon the Node.JS ldapjs library.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

type Ldap.options =
  // One time client instance
  {
    string url, // a valid LDAP url.
    int timeout, // How long the client should let operations live for before timing out. Default is Infinity.
    int connectTimeout, // How long the client should wait before timing out on TCP connections. Default is up to the OS.
  }
  or
  // Connection pooling
  {
    string url,
    int timeout,
    int connectTimeout,
    int maxConnections, // Whether or not to enable connection pooling, and if so, how many to maintain.
    string bindDN, // The DN all connections should be bound as.
    string bindCredentials // The credentials to use with bindDN.
  }

type Ldap.ldap = external

type Ldap.scope =
    {base}
 or {one}
 or {sub}

type Ldap.search_options = {
  string scope,
  string filter,
  list(string) attributes,
  bool attrsOnly,
  int sizeLimit,
  int timeLimit
}

type Ldap.search_result('a) =
    {'a entry}
 or {list(string) referral}
 or {string error}
 or {int status}

type Ldap.operation =
    {replace}
 or {add}
 or {delete}

// The ints are the lengths of the salts
type Ldap.SlapPasswd.algorithm =
    {md5}
 or {int smd5}
 or {sha}
 or {int ssha}

module Ldap {

  function outcome(Ldap.ldap,string) create(Ldap.options options) {
    %%bslLdap.createLdap%%(options)
  }

  Ldap.search_options default_search_options = {
    scope : "base",
    filter : "(objectclass=*)",
    attributes : [],
    attrsOnly : false,
    sizeLimit : 0,
    timeLimit : 10
  }

  function void search(Ldap.ldap ldap, string base, Ldap.search_options options, (Ldap.search_result('a) -> void) callback) {
    %%bslLdap.searchLdap%%(ldap, base, options, Continuation.make(callback))
  }

  function void bind(Ldap.ldap ldap, string dn, string password, (outcome(void,string) -> void) callback) {
    %%bslLdap.bindLdap%%(ldap, dn, password, Continuation.make(callback))
  }

  function void add(Ldap.ldap ldap, string dn, string entry, (outcome(void,string) -> void) callback) {
    %%bslLdap.addLdap%%(ldap, dn, entry, Continuation.make(callback))
  }

  function void del(Ldap.ldap ldap, string dn, (outcome(void,string) -> void) callback) {
    %%bslLdap.delLdap%%(ldap, dn, Continuation.make(callback))
  }

  function void exop(Ldap.ldap ldap, string name, string value, (outcome((string,string),string) -> void) callback) {
    %%bslLdap.exopLdap%%(ldap, name, value, Continuation.make(callback))
  }

  function string makeChange(Ldap.operation operation, RPC.Json.json modification) {
    op = match (operation) { case {replace}: "replace"; case {add}: "add"; case {delete}: "delete"; }
    Json.serialize({Record:[("operation",{String:op}),
                            ("modification",modification)
                           ]})
  }

  function void modify(Ldap.ldap ldap, string dn, string change, (outcome(void,string) -> void) callback) {
    %%bslLdap.modifyLdap%%(ldap, dn, change, Continuation.make(callback))
  }

  function void modifyDN(Ldap.ldap ldap, string dn, string newDN, (outcome(void,string) -> void) callback) {
    %%bslLdap.modifyDNLdap%%(ldap, dn, newDN, Continuation.make(callback))
  }

  function void unbind(Ldap.ldap ldap, (outcome(void,string) -> void) callback) {
    %%bslLdap.unbindLdap%%(ldap, Continuation.make(callback))
  }

  module SlapPasswd {

    function string string_of_algorithm(Ldap.SlapPasswd.algorithm alg) {
      match (alg) {
      case {md5}: "MD5";
      case {smd5:_}: "SMD5";
      case {sha}: "SHA";
      case {ssha:_}: "SSHA";
      }
    }

    function bool check(string slappasswd, string passwd) {
      len = String.length(slappasswd)
      if (len < 29 || String.sub(0,1,slappasswd) != "\{")
        false
      else {
        match (String.index("}",slappasswd)) {
        case {none}: false;
        case {some:i}:
          alg = String.lowercase(String.sub(1,i-1,slappasswd))
          encoded = String.sub(i+1,len-(i+1),slappasswd)
          decoded = Crypto.Base64.decode(encoded)
          dlen = Binary.length(decoded)
          hlen = match (alg) { case "md5" case "smd5": 16; case "sha" case "ssha": 20; default: 0; }
          data = Binary.get_binary(decoded,0,hlen)
          salt = Binary.get_binary(decoded,hlen,dlen-hlen)
          match (String.lowercase(alg)) {
          case "ssha" case "sha": Crypto.Salt.check_sha1(binary_of_string(passwd), ~{data, salt})
          case "smd5" case "md5": Crypto.Salt.check_md5(binary_of_string(passwd), ~{data, salt})
          default: false;
          }
        }
      }
    }

    function string generate(Ldap.SlapPasswd.algorithm alg, binary data) {
      ~{data, salt} =
        match (alg) {
        case {md5}: Crypto.Salt.md5(data, 0);
        case {smd5:salt_length}: Crypto.Salt.md5(data, salt_length);
        case {sha}: Crypto.Salt.sha1(data, 0);
        case {ssha:salt_length}: Crypto.Salt.sha1(data, salt_length);
        }
      decoded = Binary.create(Binary.length(data)+Binary.length(salt))
      Binary.add_binary(decoded,data)
      Binary.add_binary(decoded,salt)
      encoded = Crypto.Base64.encode(decoded)
      "\{{string_of_algorithm(alg)}}{encoded}";
    }

  } // module SlapPasswd

}

