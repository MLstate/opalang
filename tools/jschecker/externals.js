/**
 * This files contains external definitions required by the JS checker, typically DOM functions.
 */

/**
 * {1 Compiler-defined}
 */

var page_server = 0;
var page_lang = "en";
var base_url = "";

/**
 * {1 Runtime-defined}
 */

/**
 * For deserialization purposes, bslClientOnly needs to define a variable [f] with [exec]. The validator
 * doesn't see it, so we declare it here.
 */
var f;



/**
 * {2 JSON}
 *
 * Defined conditionally, either by the browser or by json2.js
 */

var JSON = {}

/**
 * @param {!string} s
 * @return {!Object}
 */
JSON.parse = function(s) { return {} }

/**
 * @param {*} o
 * @return {!string}
 */
JSON.stringify = function(o){ return "" }


/**
 * {1 Browser-defined}
 */

var location;
var print = function(s) { }


