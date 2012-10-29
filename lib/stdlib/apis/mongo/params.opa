/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package stdlib.apis.mongo

@private
type Mongo.params = {
  conf : Mongo.conf
  seeds : list(Mongo.mongo_host)
}

@server_private
@package
MongoConf = {{

  @private
    params : Hashtbl.t(string, Mongo.params) = Hashtbl.create(3)

  @private
  auth_parser = parser
  | user=((![:] .)+)[:] password=((![@] .)+) ->
    {user=Text.to_string(user); password=Text.to_string(password)}

  @private
  family(name) =
    suffix = match name with
      | "" -> ""
      | _ -> ":{name}"
    {
      title = match name with
        | "" -> "Default options for MongoDB connection"
        | name -> "Option for MongoDB connection to the database '{name}'"
      init = {conf = default_conf seeds = []}
      anonymous = [];
      parsers = [
        {CommandLine.default_parser with
           names = ["--mongo-bufsize{suffix}"]
           description = "Hint for initial MongoDB connection buffer size"
           param_doc = "<int>"
           on_param(p) =
             parser n={Rule.natural} ->
               {no_params = { p with conf.bufsize=n }}
        },
        {CommandLine.default_parser with
           names = ["--mongo-pool{suffix}"]
           description = "Number of sockets in socket pool (>=2 enables socket pool)"
           param_doc = "<int>"
           on_param(p) =
             parser n={Rule.natural} ->
               {no_params = { p with conf.poolmax=Int.max(n,1) }}
        },
        {CommandLine.default_parser with
           names = ["--mongo-slaveok{suffix}"]
           description = "Allow SlaveOK mode (ReplSet only)"
           param_doc = "<bool>"
           on_param(p) =
             parser b={Rule.bool} ->
               {no_params = { p with conf.slaveok=b }}
        },
        {CommandLine.default_parser with
           names = ["--mongo-log{suffix}"]
           description = "Enable MongoLog logging"
           param_doc = "<bool>"
           on_param(p) =
             parser b={Rule.bool} ->
               {no_params = { p with conf.verbose=b }}
        },
        {CommandLine.default_parser with
           names = ["--mongo-seed{suffix}"]
           description = "Add a seed to a replica set, allows multiple seeds"
           param_doc = "<host>[:<port>]"
           on_param(p) =
             parser s={Rule.consume} ->
               {no_params =
                 { p with seeds=[MongoReplicaSet.mongo_host_of_string(s)|p.seeds] }}
        },
        {CommandLine.default_parser with
           names = ["--mongo-auth{suffix}"]
           description = "MongoDB user authentication"
           param_doc = "user:password"
           on_param(p) = parser auth={auth_parser} ->
             {no_params = {p with conf.auths=[auth|p.conf.auths]}}
        },
        {CommandLine.default_parser with
           names = ["--mongo-replset{suffix}"]
           description = "Set the name of the MongoDB replica set"
           param_doc = "<string>"
           on_param(p) = parser s={Rule.consume} ->
             {no_params = {p with conf.replica=some(s)}}
        },
     ];
   }

  @private
  command_line_conf(name) =
    match Hashtbl.try_find(params, name) with
    | {some = conf} -> conf
    | {none} ->
      conf = CommandLine.filter(family(name))
      do Hashtbl.add(params, name, conf)
      conf

  @private
  default_conf : Mongo.conf = {
    bufsize      = 50*1024
    poolmax      = 10
    slaveok      = false
    attempt      = 30
    verbose      = false
    auths        = []
    timeout      = 60 * 60 * 1000
    waiting      = 2000
    replica      = none
  }

  @private
  overload(c0, c1) =
    c1 = if c1.conf.bufsize == default_conf.bufsize then {c1 with conf.bufsize = c0.conf.bufsize} else c1
    c1 = if c1.conf.poolmax == default_conf.poolmax then {c1 with conf.poolmax = c0.conf.poolmax} else c1
    c1 = if c1.conf.slaveok == default_conf.slaveok then {c1 with conf.slaveok = c0.conf.slaveok} else c1
    c1 = if c1.conf.attempt == default_conf.attempt then {c1 with conf.attempt = c0.conf.attempt} else c1
    c1 = if c1.conf.verbose == default_conf.verbose then {c1 with conf.verbose = c0.conf.verbose} else c1
    c1 = if c1.conf.auths == default_conf.auths then {c1 with conf.auths = c0.conf.auths} else c1
    c1 = if c1.conf.timeout == default_conf.timeout then {c1 with conf.timeout = c0.conf.timeout} else c1
    c1 = if c1.conf.waiting == default_conf.waiting then {c1 with conf.waiting = c0.conf.waiting} else c1
    c1 = if c1.conf.replica == default_conf.replica then {c1 with conf.replica = c0.conf.replica} else c1
    c1 = if c1.seeds == [] then {c1 with seeds=[("localhost",MongoDriver.default_port)]} else c1
    c1 : Mongo.params

  default = get(default_conf, "")

  /**
   * Returns configuration for the database [name], overloaded by the command
   * line options.
   * @param conf The default configuration.
   * @param name The name of the database.
   * @return The same configuration of [conf] but overloaded by the command line.
   */
  get(conf, name) =
    overload(~{conf seeds=[]}, command_line_conf(name))

  /**
   * As get but with a default configuration.
   */
  get_default(name) = get(default_conf, name)

}}
