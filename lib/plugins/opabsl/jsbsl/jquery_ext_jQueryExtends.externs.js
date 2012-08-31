/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
   @author Francois Pessaux
*/

/**
  This file provides prototypes for function added by side effect to jQuery
  by /opabsl/jsbsl/jQueryExtends.js.
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
 * Bind an event handler to an event
 *
 * @param {string} name The name of the event (e.g. "click")
 * @param {function(Object)} fn The event handler
 * @param {?function(Object)} prop_fn The event propagation handler
 * @param {boolean} stop_propagation
 * @param {boolean} prevent_default
 * @return {string} An abstract value which can be passed to [opaunbind]
 * for unbinding at a later stage.
 */
jQuery.prototype.opabind = function(name, fn, prop_fn, stop_propagation, prevent_default) {};

Element.prototype.style.MozTransformOrigin = '' ;

Element.prototype.style.msTransformOrigin = '' ;

Element.prototype.style.MozTransform = '' ;

Element.prototype.style.msTransform = '' ;

