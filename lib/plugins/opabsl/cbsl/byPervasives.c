/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/* ============== Qml Bypass Standard Library =============== */
/*                    Implementation in c                     */
/* ========================================================== */

/*
 This is the C-implementation of standard bsl-bypervasives for qml
 

 Beware : the function must have the same name and same semantic 
 as the functions already define in the ml implementations
*/


/* int */


##register add_int : int -> int -> int
##args(a, b)
{
    return(a+b);
}

##register mult_int : int -> int -> int
##args(a, b)
{
  return(a*b);
}

##register sub_int : int -> int -> int
##args(a, b)
{
  return(a-b);
}

##register div_int : int -> int -> int
##args(a, b)
{
  return(a/b);
}

##register succ : int -> int
##args(a)
{
  return(a++);
}

##register pred : int -> int
##args(a)
{
  return(a--);
}

##register abs : int -> int
##args(a)
{
  if(a<0) {
    a=-a;
  }
  return(a);
}

##register lnot : int -> int
##args(a)
{
  return(~a);
}

##register land : int -> int -> int
##args(a, b)
{
  return(a&b);
}

##register lxor : int -> int -> int
##args(a, b)
{
  return(a^b);
}

##register lor : int -> int -> int
##args(a, b)
{
  return(a|b);
}


##register mod : int -> int -> int
##args(a, b)
{
  return(a%b);
}




/* float */

##register add_float : float -> float -> float
##args(a, b)
{
    return(a+b);
}

##register mult_float : float -> float -> float
##args(a, b)
{
    return(a*b);
}

##register sub_float : float -> float -> float
##args(a, b)
{
    return(a-b);
}

##register div_float : float -> float -> float
##args(a, b)
{
    return(a/b);
}

##register sqrt : float -> float
##args(a)
{
  return(sqrt(a));
}

##register log : float -> float
##args(a)
{
  return(log(a));
}

##register abs_float : float -> float
##args(a)
{
  return(abs(a));
}

##register ceil : float -> float
##args(a)
{
  return(ceil(a));
}

##register floor : float -> float
##args(a)
{
  return(floor(a));
}

##register sin : float -> float
##args(a)
{
  return(sin(a));
}

##register cos : float -> float
##args(a)
{
  return(cos(a));
}

##register tan : float -> float
##args(a)
{
  return(tan(a));
}

##register asin : float -> float
##args(a)
{
  return(asin(a));
}

##register acos : float -> float
##args(a)
{
  return(acos(a));
}

##register atan : float -> float
##args(a)
{
  return(atan(a));
}

##register sinh : float -> float
##args(a)
{
  return(sinh(a));
}

##register cosh : float -> float
##args(a)
{
  return(cosh(a));
}

##register tanh : float -> float
##args(a)
{
  return(tanh(a));
}


/* Polymorphism : oups ! */



/* Char */

##register char_lowercase : char -> char
##args(a)
{
  return(tolower(a));
}

##register char_uppercase : char -> char
##args(a)
{
  return(toupper(a));
}

/* Transtyping */
/* 
   We prefer here to return an option if a function can fail
*/

##register char_of_int : int -> char option
##args(i)
{
    char * r = 0 ;
    if ((i < 0)&&(i>255)) {
	return(r);
    }else{
	/* OUPS LA PA : garbage collector */
	r = (char *) malloc ( sizeof(char) ) ;
	*r = (char ) i ;
	return(r) ;
    }
}

##register int_of_char : char -> int
##args(a)
{
  return(a);
}


##register int_of_float : float -> int
##args(a)
{
  return(a);
}


##register float_of_int : int -> float
##args(a)
{
  return(a);
}


//register int_of_string : string -> int option
//let int_of_string s =
//  try
//    Some (Int64.of_string s)
//  with
//  | Failure "int_of_string" -> None

//register string_of_int : int -> string
//let string_of_int = Int64.to_string

//register float_of_string : string -> float option
//let float_of_string s =
//  try
//    Some (Pervasives.float_of_string s)
//  with
//  | Failure "float_of_string" -> None

//register string_of_float : float -> string
//let string_of_float = Pervasives.string_of_float

//register string_of_char : char -> string
//let string_of_char c = let s = " " in String.unsafe_set s 0 c; s

/* Print : debugging on stdout */

##register flush : unit -> unit
##args()
{
  fflush(stdout);
  UNIT;
}

##register print_string : string -> unit
##args(a)
{
  printf("%s",a);
  UNIT;
}

##register print_int : int -> unit
##args(a)
{
  printf("%lld",a);
  UNIT;
}

##register print_float : float -> unit
##args(a)
{
  printf("%lf",a);
  UNIT;
}

##register print_char : char -> unit
##args(a)
{
  printf("%c",a);
  UNIT;
}


##register print_endline : string -> unit
##args(a)
{
  printf("%s\n",a);
  UNIT;
}


##register println_int : int -> unit
##args(a)
{
  printf("%lld\n",a);
  UNIT;
}

##register println_float : float -> unit
##args(a)
{
  printf("%lf\n",a);
  UNIT;
}

##register println_char : char -> unit
##args(a)
{
  printf("%c\n",a);
  UNIT;
}

/* Linking with libqml */

##register sleep : float -> unit
##args(a)
{
  usleep((int)(a*1000000));
  UNIT;
}
