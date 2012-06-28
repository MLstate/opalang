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

