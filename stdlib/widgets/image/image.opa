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
 * A (currently trivial) widget for embedding images.
 *
 * @author Mathieu Baudet, 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

import stdlib.widgets.core

WImage = {{

html(url : string, text : option(string)) = <img src={url} alt={(text ? "")}/>

}}
