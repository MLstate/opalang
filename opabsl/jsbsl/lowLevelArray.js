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
