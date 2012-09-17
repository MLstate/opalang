/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * The signature of the CPS client lib, used for verification of the BSL
 *
 * @author David Rajchenbach-Teller, 2010
 */

/**
 * @interface
 */
function Continuation() {}

/**
 * @interface
 */
function Task() {}

/**
 * Create a synchronization barrier.
 * See prototype.
 *
 * @param {(number|string)=} name An optional name, used for debugging purposes
 * @constructor
 */
function Barrier(name) {}
Barrier.prototype = {
    /**
     * Release the barrier.
     *
     * Schedule all continuations waiting on this barrier. Can be called only once on a given barrier.
     *
     * @param {!*} result A result
     */
    release: function(result) {},

    /**
     * Add a continuation waiting for this barrier.
     *
     * If the barrier is already released, the task is immediately executed. Otherwise, it
     * will wait until [release] is called.
     *
     * @param {!Continuation} k A continuation
     */
    wait: function(k) {}
}

/**
 * Construct a new task from an application, i.e. a function and its arguments
 *
 * @param fun a function to be executed once the scheduler wakes up the task
 * @param args the arguments to pass to the function
 * @return {Task}
 *
 * Note: Some browsers may actually pass additional arguments to [fun]. Ignore them.
 */
function task_from_application(fun, args) {}

/**
 * @param {!Object} k
 * @param {Array.<!*>} args
 * @return {Task}
 */
function task_from_return(k, args) {}

/**
 * Construct a continuation.
 *
 * Once it has received its argument, a continuation may decide to [push] a [Task] or possibly
 * to execute some treatment.
 *
 * @param {!Function} payload a 1-argument function
 * @param {?*=}  context an optional object containing the execution context for [payload]
 * @param {?*=}  options placeholder for future passing of continuation options, must be [null] for the moment.
 * @return {!Continuation}
 */
function cont(payload, context, options){}

/**
 * Construct a continuation from a parent continuation
 * @param {!Continuation} k
 * @param {!Function} payload a 1-argument function
 */
function ccont(k, payload){}

/**
 * @param {!Continuation} k
 * @param {!*} arg
 */
function execute1(k, arg) {}

/**
 * @param {!Continuation} k
 * @param {!*} arg
 */
function executexn(k, arg) {}

/**
 * @param {!Continuation} k
 * @param {!Function} h
 */
function catch_(k, h) {}

/**
 * @param {!Function} f
 * @return {!Barrier}
 */
function spawn(f) {}

/**
 * @param {!Continuation} k
 * @param {!*} x
 */
function return_(k, x){}

/**
 * @param {!Continuation} k
 * @param {!*} x
 */
function return_tc(k, x){}

/**
 * @param {!Continuation} k
 * @param {!*} x
 */
function execute(k, x){}

/**
 * @param {!Barrier} barrier
 * @return {!*}
 */
function blocking_wait(barrier) {}

/**
 * Compute a function application in a given continuation.
 *
 * @param {!Function} f A function accepting one argument (and a continuation)
 * @param {!*} v The argument to function [f]
 * @param {!Continuation} k The continuation expeccting the result of [f(v)]
 */
function cps_apply(f, v, k){}

function loop_schedule() {}

/**
 * @param {!string} s
 */
function cps_debug(s) {}

/**
 * @param {!boolean} b
 * @param {!string} s
 */
function cps_assert(b,s) {}

function callcc_directive(f, k){}

function default_handler_cont(k){}

function handler_cont(k){}

function with_thread_context(tc, k){}

function thread_context(k){}

/**
 * Schedule a [Task] for future execution.
 *
 * @param {Task} task
 */
function push(task) {}

/**
 * @param {!Function} f
 * @return {!Function}
 */
function cps(f){}


/**
 * @param {!Continuation} k
 * @param {!Function} f
 * @param {?String} id
 * @return {!Function}
 */
function uncps(k,f,id){}

function opa_cps_callback_to_js_callback0(k, f) {}

function wrap_tc(opa) {}
