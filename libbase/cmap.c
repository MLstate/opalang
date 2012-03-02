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
#include <unistd.h>
#include <stdio.h>
#include <caml/mlvalues.h>

value stack_pointer (value v) {
  int x[1]; /* don't use 0, some compilers don't like 0-sized arrays */
  return (Val_long(x));
}

value immediate_exit (value v) {
 #ifdef WIN32
   exit(Long_val(v));
 #else
   _exit (Long_val(v));
 #endif
  return Val_unit;
}
