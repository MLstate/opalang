/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/

/*
   @author Francois Pessaux
*/

/**
  This file provides prototypes for function added by side effect to jQuery by
  opabsl/jsbsl/jquery-1.X.Y.js. We use the jquery-1.6.externs.js version of the
  JS prototypes from Google. The present file fills the holes and may
  become useless if the Google's prototype file gets updated to a more recent
  version. This code is not intended to be compiled and included in any binary,
  it only serves to the Google Closure Compiler checker by giving it prototypes,
  hence avoiding warnings.  In case the Google Closure Compiler checker is not
  more used by the build system, this source file can be discarded from the Opa
  sources tree.  This source file is involved by the build system build_rules.ml
  for 2 rules: - rule "Client lib JS validation" - rule "opa-bslgenMLRuntime JS
  validation"
**/



/**
  * @param {Event} evt
  */
jQuery.event.fix = function(evt) {} ;

/**
  * @type {{ready, live, remove, beforeunload, teardown}}
  */
jQuery.event.special = {
  ready : {},
  live : {},
  remove : {},
  beforeunload : {},
 teardown : {}
};

/**
  * @param {Event} evt
  */
jQuery.event.handle = function(evt) {} ;
