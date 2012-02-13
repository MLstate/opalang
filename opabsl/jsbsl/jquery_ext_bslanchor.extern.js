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

/*
   @author Francois Pessaux
*/

/**
  This file provides prototypes for function added by side effect to jQuery
  by /opabsl/jsbsl/bslAnchors.js.
  This code is not intended to be compiled and included in any binary, it only
  serves to the Google Closure Compiler checker by giving it prototypes, hence
  avoiding warnings.
  In case the Google Closure Compiler checker is not more used by the build
  system, this source file can be discarded from the Opa sources tree.
  This source file is involved by the build system build_rules.ml for 2 rules:
   - rule "Client lib JS validation"
   - rule "opa-bslgenMLRuntime JS validation"
**/



/**
 * @param {!Function} callback
 */
jQuery.add_history_handler = function(callback) {};

/**
 * @param {!Function} callback
 */
jQuery.remove_history_handler = function(callback) {};

/**
 * @param {!Function} callback
 */
jQuery.push_state = function(callback) {};
