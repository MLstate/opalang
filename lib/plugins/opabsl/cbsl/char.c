/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/* Char */

##register chr : int -> char option
##args(a)
{
    char * r = 0 ;
    if ((a < 0)||(a>255)) {
	return(r);
    }else{
	/* OUPS LA PA : garbage collector */
	r = (char *) malloc ( sizeof(char) ) ;
	*r = (char ) a ;
	return(r) ;
    }
}

##register unsafe_chr : int -> char
##args(a)
{
   return(a);
}

##register code : char -> int
##args(a)
{
  return(a);
}

##register compare : char -> char -> int
##args(a,b)
{
  return(a-b);
}

##register lowercase : char -> char
##args(a)
{
  return(tolower(a));
}

##register uppercase : char -> char
##args(a)
{
  return(toupper(a));
}
