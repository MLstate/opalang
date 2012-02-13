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
