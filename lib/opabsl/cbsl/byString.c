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
/* String */

##register lowercase : string -> string
##args(u)
{
    int t,i;
    t = strlen(u);
    char *s;
    s = (char*)malloc((t+1)*sizeof(char));
    for(i=0;i<t;i++) {
      s[i] = tolower(u[i]);
    }  
    s[t] = 0;
    return(s);
}

##register uppercase : string -> string
##args(u)
{
    int t,i;
    t = strlen(u);
       char *s;
    s = (char*)malloc((t+1)*sizeof(char));
    for(i=0;i<t;i++) {
      u[i] = toupper(u[i]);
    }  
    s[t] = 0;
    return(s);
}

##register concat : string -> string -> string
##args(u,v)
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

##register length : string -> int 
##args(u)
{     
  return(strlen(u));
}

##register get : string -> int -> char
##args(u,v)
{     
  return(u[v]);
}

##register unsafe_get : string -> int -> char
##args(u,v)
{     
  return(u[v]);
}

/*
__register set : string -> int -> char -> unit 
__args(u,v,w)
{ 
  u[v] = w;
  UNIT;
}
*/

##register unsafe_set : string -> int -> char -> unit
##args(u,v,w)
{ 
  u[v] = w;
  UNIT;
}

