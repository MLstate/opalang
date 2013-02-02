(*
    Copyright © 2012 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
module Extension =
struct
  type extension = string
  let bypass = "bypass"
  let jsconf = "jsconf"
  let plugin = "opp"
end

module Suffix =
struct
  type suffix = string
  let nodejspackage = "NodeJsPackage"
  let jskeys = "JSkeys"
  let loader = "Loader"
  let marshalplugin = ""
  let mlruntime = "MLRuntime"
  let plugin = "Plugin"
end

let plugin_name name =
  let name = Filename.basename name in
  let name = File.chop_extension name in
  let name = String.uncapitalize name in
  name

type inclusion = {
  extrapath : string ;
  extralib : string ;
  plugin : string ;
}

let inclusion ~cwd opp =
  let plugin_name = plugin_name opp in
  let prefix =
    if Filename.is_relative opp
    then cwd
    else ""
  in
  let (/) = Filename.concat in
  let extrapath = prefix/opp in
  let extralib = plugin_name^Suffix.mlruntime^".cmxa" in
  let plugin = prefix/opp/(plugin_name^"."^Extension.bypass) in
  {
    extrapath ;
    extralib ;
    plugin ;
  }
