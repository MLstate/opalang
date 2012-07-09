/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

