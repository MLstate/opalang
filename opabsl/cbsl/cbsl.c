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
