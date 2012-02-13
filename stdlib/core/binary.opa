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
 * Binary data handling: for any sequence of bytes that is not to be considered
 * as text.
 *
 * @destination PRIVATE
 * @stability UNSTABLE
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

//TODO:This should be [external]. However, for the moment, we rely on [static_file_include] and we just can't.
@opacapi
type binary = string

/**
 * {1 Interface}
 */

string_of_binary(x:binary) = @unsafe_cast(x): string
binary_of_string(x:string) = @unsafe_cast(x): binary
