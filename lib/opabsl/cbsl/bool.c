/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
