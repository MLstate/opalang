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
