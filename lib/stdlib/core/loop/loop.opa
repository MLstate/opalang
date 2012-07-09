/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.{map,set}

Init = {{
    list(n)(f) = List.init(f,n)
    llarray(n)(f) = LowLevelArray.init(n)(f)
}}

Map_ = {{
    list(l)(f) = List.map(f,l)
    list2(l1,l2)(f) = List.map2(f,l1,l2)
    map(m)(f) = Map.map(f,m)
    set(s)(f) = Set.map(f,s)
}}

Mapi = {{
    list(l)(f) = List.mapi(f,l)
}}

Fold = {{
    list(l,acc)(f) = List.fold(f,l,acc)
    map(m,acc)(f) = Map.fold(f,m,acc)
    set(s,acc)(f) = Set.fold(f,s,acc)
}}
