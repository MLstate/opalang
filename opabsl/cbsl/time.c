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

