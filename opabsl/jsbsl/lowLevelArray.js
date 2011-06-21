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

##extern-type llarray('a)

##register set : llarray('a), int, 'a -> void
##args(a,i,v)
{
    a[i]=v
}

##register get : llarray('a), int -> 'a
##args(a,i)
{
    return a[i]
}

##register size : llarray('a) -> int
##args(a)
{
 return a.length
}

##register create : int, 'a -> llarray('a)
##args(i,v)
{
   var res = new Array(i)
   i=i-1
   for (;i>=0;i--){res[i]=v}
   return res
}

##register fast_create : int -> llarray('a)
##args(i)
{
   return (new Array(i))
}

##register concat : llarray('a), llarray('a) -> llarray('a)
##args(t1,t2)
{
    return t1.concat(t2)
}
