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

