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

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type Mutable.t('a) = {get: -> 'a; set: 'a -> void}
Mutable =
{{
   make(init:'a): Mutable.t('a) =
        r = Reference.create(init)
        {{
           get() = Reference.get(r)
           set(x)= Reference.set(r, x)
        }}

}}

/**
 * {1 Interface}
 */

@opacapi
Mutable_make = Mutable.make
@opacapi Mutable_set(mutable,v) : Mutable.t('a) = mutable.set(v)
