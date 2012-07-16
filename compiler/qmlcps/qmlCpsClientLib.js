/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * {1 Debugging}
 */

/**
 * A function provided for convenience when debugging the compiler.
 * Unless you're debugging the CPS parts of the JS compiler, you shouldn't need this function.
 *
 * By default, the function does nothing. Change the body of the function to make it dump
 * information to appropriate places.
 */
function cps_debug(s){
    js_debug("[CPS]"+s);
    return js_void; // FIXME: this return js_void is probably deprecated
}

/**
 * An assertion function provided for convenience when debugging the compiler.
 * Unless you're debugging the CPS parts of the JS compiler, you shouldn't need this function.
 *
 * Check a condition. If the condition is [false], raise an [Error] with a given debugging message.
 *
 * @param b A boolean condition. If it is [false], this is a fatal error, execution will stop.
 * @param s A debugging message.
 */
var cps_assert = js_assert

/**
 * {1 Scheduling}
 */

/**
 * {2 Synchronization barriers}
 *
 * Barriers support two operations: [wait] adds a continuation, waiting for the barrier to be released,
 * while [release] releases the barrier and sets a value which the waiting continuations can obtain.
 */

var default_options = {movable:true, atomic:false, lazy:false};

/**
 * Create a synchronization barrier.
 * See prototype.
 *
 * @param {(number|string)=} name An optional name, used for debugging purposes
 * @constructor
 */
function Barrier(name)
{
    this._waiters = [];
    this._id = name ? name : "anonymous";
}

Barrier.prototype = {
    /**
     * Determine if the barrier has been released already. [true] if the barrier has been
     * released, [false] otherwise.
     *
     * @type {boolean}
     */
    _is_computed: false,

    /**
     * The result of the computation. [null] until the barrier has been released, not-[null]
     * otherwise.
     *
     * @type {?Object}
     */
    _result:      null,

    /**
     * An array of [Continuation]s
     *
     * @type {Array.<Continuation>}
     */
    _waiters:     null,

    /**
     * Release the barrier.
     *
     * Schedule all continuations waiting on this barrier. Can be called only once on a given barrier.
     *
     * @param {!Object} result A result
     */
    release: function(result)
    {
        this._is_computed = true;
        this._result      = result;
        var waiters       = this._waiters;
        var len           = waiters.length;
        var i;
        for(i = 0; i < len; ++i)
        {
            var k      = waiters[i];
            k.execute1(result);
        }
        this._waiters = undefined;
    },

    /**
     * Add a continuation waiting for this barrier.
     *
     * If the barrier is already released, the task is immediately executed. Otherwise, it
     * will wait until [release] is called.
     *
     * @param {Continuation} k A continuation
     */
    wait: function(k)
    {
        if(this._is_computed)
        {
            k._payload(this._result);
        } else {
            this._waiters.push(k);
        }
    }
}


/**
 * {2 Tasks}
 */

/**
 * Construct a new task from an application, i.e. a function and its arguments
 *
 * @param fun a function to be executed once the scheduler wakes up the task
 * @param args the arguments to pass to the function
 *
 * Note: Some browsers may actually pass additional arguments to [fun]. Ignore them.
 * @constructor
 * @implements {Task}
 */
function task_from_application(fun, args) {
    return function () {return fun(args)};
}

/**
 * @param {!Continuation} k
 * @param {Array.<!*>} args
 * @constructor
 * @implements Task
 */
function task_from_return(k, args) {
    return function() {return k.execute(args)};
}

/**
 * Construct a continuation.
 *
 * Once it has received its argument, a continuation may decide to [push] a [Task] or possibly
 * to execute some treatment.
 *
 * @param {!Function} payload a 1-argument function
 * @param {?Object=}  context an optional object containing the execution context for [payload]
 * @param {?Object=}  options placeholder for future passing of continuation options, must be [null] for the moment.
 * @constructor
 *///TODO reintroduce options
function Continuation(payload, context, options) {
    this._payload = payload;
    this._context = context;
    this._options = options;
    if(context == null)//optimized execute1
        this.execute1 = this._execute1
}
Continuation.prototype = {
    _payload: null,
    _context: null,
    _options: null,
    _paylexn: null,
    /**
     * Apply a continuation to an array of arguments
     *
     * @param {?Array} args a possibly empty, possibly [null] array of arguments to pass to the continuation
     */
    execute: function(args) {
        return this._payload.apply(this._context, args)
    },
    /**
     * Non-optimized version of [apply] for exactly one argument.
     *
     * Transparently replaced by optimized version when possible (i.e. when context is [null]).
     */
    execute1: function(arg) {
        return this._payload.apply(this._context, [arg]);
    },
    /**
     * Optimized version of [execute1].
     *
     * Do not call directly.
     */
    _execute1: function(arg) {
        return this._payload(arg);
    },
    /**
     * Apply a continuation to an exception
     */
    executexn: function(exn) {
        var payload = this._paylexn;
        (payload ? payload : QmlCpsLib_default_handler_cont(this)._payload)
        .apply(this._context, [exn]);
    },

    ccont: function(f) {
        return new Continuation(f, this._context, this._options);
    },

    catch_: function(h) {
        var k = this.ccont(this._payload);
        k._paylexn = h;
        return k;
    }

}

function QmlCpsLib_callcc_directive(f, k){
    return f(k, new Continuation(function(){}, k._context));
}

function QmlCpsLib_default_handler_cont(k){
    return new Continuation(function(exn){console.error("Error : uncaught OPA exn", exn)}, k._context, k._options);
}

function QmlCpsLib_handler_cont(k){
    var paylexn = k._paylexn;
    return (paylexn ? new Continuation(paylexn, k._context, k._options) : QmlCpsLib_default_handler_cont(k));
}



/**
 * {2 The scheduling loop}
 */

/**
 * A queue of tasks waiting to be executed
 */
var ready = [];

/**
 * Schedule a [Task] for future execution.
 *
 * @param {Task} task
 */
function push(task)
{
    ready.push(task);
    launch_schedule();
}

/**
 * Indicates if there are a loop_schedule in the call stack.
 */
var is_schedule = false;

/**
 * Number of nested loop_schedule.
 */
var loop_level = 0;

/**
 * An infinite scheduling loop.
 *
 * Takes the tasks waiting in [ready], execute them. If [ready] is empty, sleep and wake up
 *
 * Stop on fatal error.
 */
function loop_schedule()
{
    var i;
    var fatal_error   = false;//[true] if we stopped scheduling because of a fatal error, [false] otherwise
    var nothing_to_do = false;//[true] if we stopped scheduling because there's nothing left to do
    var tasks         = ready;//Keep a local copy. In most JS VMs, this will speed-up code.
    var task;
    loop_level++;
    is_schedule = true;
    try
    {
        for(;;)
        {
            if(tasks.length == 0)
            {
                nothing_to_do = true;
                break;
            } else {
                task = tasks.shift();
                var r = task();
                for(var i=0; i<100 && r; i++) r = r[0].execute1(r[1]);
                if (r) push(task_from_return(r[0], [r[1]]))
            }
        }
    } catch(e) {
        fatal_error = true;
        console.log("Uncaught exception : " + e.toString());
        console.log(e.stack);
    }
    loop_level--
    is_schedule = loop_level != 0;
}

/**
 * As loop_schedule but only if not already launched.
 */
function launch_schedule(){
    if(is_schedule) return;
    loop_schedule();
}

function return_tc(k, x){
    if (is_schedule) return [k, x];
    else return_(k, x)
}

/**
 * Returns value [x] to Continuation [k].
 * BEWARE: The compiler manipulates the return_ ident as a "pure" function.
 */
function return_(k, x){
    push(task_from_return(k, [x]));
}

function execute(k, x){
    var r;
    if (r = k.execute1(x, true)) push(task_from_return(r[0], [r[1]]));
}

/**
 * Compute a function application in a given continuation.
 *
 * @param f A function accepting one argument (and a continuation)
 * @param v The argument to function [f]
 * @param k The continuation expeccting the result of [f(v)]
 */
function cps_apply(f, v, k){
    // TODO: decide whether we always [push]
    push(task_from_application(f, [v, k] ));
}

/**
 * Blocking wait for a barrier to be [release]d
 *
 * Execute all tasks until said barrier has been released. Fail if there are no more tasks
 * and the barrier is still not released. Does not yield time with setTimeout.
 */
function blocking_wait(barrier){
    var i;
    while(!barrier._is_computed){
        loop_schedule();
        if(!barrier._is_computed && ready.length == 0)
            error("Barrier not released : " + barrier._id);
    }
    return barrier._result;
}

function spawn(f) {
    var barrier = new Barrier();
    var task = function(){
        var k = new Continuation(barrier.release, barrier, null);
        f(js_void, k);
    }
    push(task);
    return barrier;
}

/**
 * Transform a cps function [f] (function (..., k)) to a non-cps function
 * [function(...)].
 */
function uncps(pk, f, name) {
    return function (){
        var b = new Barrier(name != undefined ? name : ("uncps : " + f));
        var k = pk.ccont(function(x){b.release(x)});
        var a = Array.prototype.slice.call(arguments);
        a.push(k);
        push(function(){return f.apply(this, a);});
        return blocking_wait(b);
    }
}

/**
 * Transform a non-cps function [f] (function (...)) to a cps function
 * [function(..., k)].
 */
function cps(f) {
    return function (){
        var a = Array.prototype.slice.call(arguments);
        var k = arguments.pop();
        return return_(k, f.apply(this, a));
    }
}

/**
 * Transform an opa cps callback (-> void) to a js_callback
 */
function opa_cps_callback_to_js_callback0(k, f){
    return function(){return f(k.ccont(function(){}));};
}

/**
 * Wrap opa function
 */
function wrap_tc(opa){
    return function(){
        var r = opa.apply(this, arguments);
        if (r) push(task_from_return(r[0], [r[1]]));
    };
}
