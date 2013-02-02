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
 This is the C-implementation of standard bsl-time for qml
 

 Beware : the function must have the same name and same semantic 
 as the functions already define in the ml implementations
*/

/* now */
/* Wait he dont work actually

//extern-type t = float
//register now : unit -> extern t
//args(X)
{
    struct timeval time;
    struct timezone timez;
    gettimeofday(&time, &timez);
    double result;
    result = (double)time.tv_sec + (double)time.tv_usec/1000000.0;
    return result;
}
*/

##register sleep : int -> unit
##args(a)
{
  sleep(a);
  UNIT;
}

