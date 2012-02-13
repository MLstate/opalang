/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
`+` = %%BslPervasives.int_add%%
`-` = %%BslPervasives.int_sub%%
`*` = %%BslPervasives.int_mul%%
`/` = %%BslPervasives.int_div%%

`+.` = %%BslPervasives.float_add%%
`-.` = %%BslPervasives.float_sub%%
//`*.` = %%BslPervasives.float_mul%%
//`/.` = %%BslPervasives.float_div%%

//`^` = %%BslString.concat%%

`&&` = bool._and
`||` = bool._or
not = bool.not

`==` = bool.eq
`!=`(a, b) = not(a == b)

`<` = bool.lt
`>` = bool.gt
`<=` = bool.le
`>=` = bool.ge

mod = %% bslpervasives.mod %%

min(a, b) = if a <= b then a else b
max(a, b) = if a >= b then a else b

// instead of including all bslPervasives, juste usefull functions
compare = %%bslpervasives.compare_raw%%
compare_raw = %%bslpervasives.compare_raw%%
string_of_float = %% BslNumber.Float.to_string %%

string_of_int = %% BslString.of_int %%

float_of_int = %% BslNumber.Float.of_int %%
float_of_string = %% BslNumber.Float.of_string_opt %%

int_of_float = %% BslNumber.Int.of_float %%
int_of_string = %% BslNumber.Int.of_string_opt %%

print_int = %% bslpervasives.print_int %%
print_string = %% bslpervasives.print_string %%

jlog = %% bslpervasives.jlog %%
prerr_endline = %% bslpervasives.prerr_endline %%
print_endline = %% bslpervasives_print_endline %% : string -> void
println_string = print_endline


pred(x) = x - 1
succ(x) = x + 1

unary_minus = %%Bslpervasives.int_neg%%
unary_minus_dot = %%Bslpervasives.float_neg%%

`^` = %%bslstring_concat%% : string, string -> string
