/*
    Copyright © 2011, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package stdlib.apis.ldap

import-plugin mail

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

module Ldap {

  function Ldap.ldap create(Ldap.options options) {
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

}

