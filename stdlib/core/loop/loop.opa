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
