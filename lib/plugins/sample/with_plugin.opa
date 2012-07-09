/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
