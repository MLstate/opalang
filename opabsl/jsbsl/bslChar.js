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

##register lowercase : char -> char
##args(s)
{
  return s.toLowerCase();
}

##register uppercase : char -> char
##args(s)
{
  return s.toUpperCase();
}

##register chr : int -> option(char)
##args(i)
{
  return String.fromCharCode(i);
}

##register code : char -> int
##args(s)
{
  return s.charCodeAt(0);
}

##register leq:char, char -> bool
##args(c1,c2)
{
  return c1 <= c2
}

##register lt:char, char -> bool
##args(c1,c2)
{
  return c1 < c2
}

##register eq:char, char -> bool
##args(c1,c2)
{
  return c1 == c2
}

##register geq:char, char -> bool
##args(c1,c2)
{
  return c1 >= c2
}

##register gt:char, char -> bool
##args(c1,c2)
{
  return c1 > c2
}

##register neq:char, char -> bool
##args(c1,c2)
{
  return c1 != c2
}

##register ordering: char,char -> opa[Order.ordering]
##args(c1,c2)
{
  if(c1<c2) return %%BslPervasives.order_lt%%
  if(c1==c2) return %%BslPervasives.order_eq%%
  return %%BslPervasives.order_gt%%
}
