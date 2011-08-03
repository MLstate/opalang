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
/* ##exec_okko sample.opp with_plugin.opa */

my_function_in_plugin = %% myfile.myfunction %% : string -> string

check_value(_, mess, got, exp) = OK.check_equal(mess, got, exp)

_ =
  do check_value("I", "my_function_in_plugin(\"I\")", my_function_in_plugin("I"), "I")
  do check_value("like", "my_function_in_plugin(\"like\")", my_function_in_plugin("like"), "ekil")
  do check_value("bsl", "my_function_in_plugin(\"bsl\")", my_function_in_plugin("bsl"), "lsb")
  do check_value("plugins", "my_function_in_plugin(\"plugins\")", my_function_in_plugin("plugins"), "snigulp")
  do check_value("in", "my_function_in_plugin(\"in\")", my_function_in_plugin("in"), "ni")
     check_value("opa", "my_function_in_plugin(\"opa\")", my_function_in_plugin("opa"), "apo")

proj_string = %%myfile.proj.string%%

_ =
  do jlog(proj_string())
  void

proj_option = %%myfile.proj.option%%

_ =
  match proj_option("bibi") with
  | { ~some } -> jlog(some)
  | _ -> jlog("buggy")

init = %%myfile.init%%

_ =
  iter(i) = print("i:{i}")
  List.iter(iter, init(5))

// marshal

marshal = %%myfile.marshal%%
unmarshal = %%myfile.unmarshal%%

type toto = { mystructure : option(string) }

f(s1 :'a) =
  m = marshal(s1)
  do print("marshal: {m}\n")
  s2 = unmarshal(m) : 'a
  do print("s1: {Debug.dump(s1)}\ns2: {Debug.dump(s2)}\n")
  s2

_ =
  s = f( { mystructure = some("dudule") } )
  _ = f( { mystructure = none } )
  do
    match s with
    | { ~mystructure } -> print("mystructure: {Option.get(mystructure)}\n")
    | _ -> @fail("mystructure failing")
  void
