/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
