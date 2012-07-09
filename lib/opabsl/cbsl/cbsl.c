/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

##module bypervasives

##register add_int\bypervasives_add : int -> int -> int
long long int bypervasives_add(long long int i, long long int j)
{
    return(i+j);
}

/*
long long int toto = 42;

register myconst : int
const 
42;

register myrec : record toto
const
(toto){4 , 6}
*/

##register bsl_print_int : int -> void
void bsl_print_int(long long int i)
{
    fprintf(stdout, "qml2llvm with cbsl (int) : %lld\n", i);
    fflush(stdout);
}

/*
register +\add : int -> int -> int
args(i, j)
{
    return(i+j);
}
*/


##register sub_int\bypervasives_sub : int -> int -> int
long long int bypervasives_sub(long long int i, long long int j)
{
    return(i-j);
}

##endmodule

##register print_endline : string -> unit
void print_endline(char *s)
{
    fprintf(stdout, "qml2llvm with cbsl (string) : %s\n", s);
    fflush(stdout);
}

##register concat : string -> string -> string
char *concat(char *u, char *v)
{
    int i, j;
    char *s;
    i = strlen(u);
    j = strlen(v);
    
    s = (char*)malloc((i+j+1)*sizeof(char));
    strcpy(s, u);
    strcpy(s+i, v);
    s[i+j] = 0;

    return(s);
}
