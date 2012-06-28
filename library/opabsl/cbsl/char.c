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
