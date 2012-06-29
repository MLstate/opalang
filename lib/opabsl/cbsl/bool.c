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
/* bool */


//register jlog : string -> unit
//args(a)
//{
//  UNIT;
//}


##register _and : bool -> bool -> bool
##args(a ,b)
{
  return(a&&b);
}

##register _or : bool -> bool -> bool
##args(a,b)
{
  return(a||b);
}

##register not : bool -> bool 
##args(a)
{
  return(!a);
}

##register eq : 'a -> 'a -> bool
##args(a,b)
{
  return(a==b);
}


##register lt : 'a -> 'a -> bool
##args(a,b)
{
  return(a<b);
}

##register gt : 'a -> 'a -> bool
##args(a,b)
{
  return(a>b);
}

##register le : 'a -> 'a -> bool
##args(a,b)
{
  return(a<=b);
}

##register ge : 'a -> 'a -> bool
##args(a,b)
{
  return(a>=b);
}



/*
(* register &&\Pervasives.(&&) : bool -> bool -> bool *)
(* (\* let and_bool = ( && ) *\) *)

(* register or_bool : bool -> bool -> bool *)
(* register ||\or_bool : bool -> bool -> bool *)
(* let or_bool = ( || ) *)

(* register not_bool : bool -> bool *)
(* register not\not_bool : bool -> bool *)
(* let not_bool = ( not ) *)
*/
