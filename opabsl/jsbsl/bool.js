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

##register eq : 'a, 'a -> bool
##args(a, b)
{ return compare_raw(a, b) === 0; }

##register _and : bool, bool -> bool
##args(a, b)
{ return a && b; }

##register _or : bool, bool -> bool
##args(a, b)
{ return a || b; }

##register not : bool -> bool
##args(a)
{ return !a; }

##register lt : 'a, 'a -> bool
##args(a, b)
{ return compare_raw(a, b) === -1; }

##register gt : 'a, 'a -> bool
##args(a, b)
{ return compare_raw(a, b) === 1; }

##register le : 'a, 'a -> bool
##args(a, b)
{ return compare_raw(a, b) !== 1; }

##register ge : 'a, 'a -> bool
##args(a, b)
{ return compare_raw(a, b) !== -1; }
