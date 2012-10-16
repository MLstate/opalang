/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @author Quentin Bourgerie
 *
 *
 * /!\ This file is shared between server (node) and client, it's why you can
 * found the Ifstatic preprocessing directive.  Indeed even if the client code
 * is not cps rewrited, the cps runtime is anyway used by the client (stdlib).
 * But the client cps runtime is a bit different than the server cps
 * runtime. It's why they are Ifstatic preprocessing directive.
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
            return_(k, result);
        }
        delete this._waiters;
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
            return_(k, this._result);
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
 * @return {Task}
 *
 * Note: Some browsers may actually pass additional arguments to [fun]. Ignore them.
 */
function task_from_application(fun, args) {
    return function () {return fun(args)};
}

/**
 * @param {!Continuation} k
 * @param {Array.<!*>} args
 * @return {Task}
 */
function task_from_return(k, args) {
    return function() {return execute_(k, args)};
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
 * @return {!Continuation}
 */
function cont(payload, context, options){
    return [payload, [context, options, null]]
}

/**
 * Apply a continuation to an array of arguments
 *
 * @param {!Continuation}
 * @param {!Array} args a possibly empty, possibly [null] array of arguments to pass to the continuation
 */
function execute_(k, args) {
    return k[0].apply(k[1][0], args);
}

/**
 * Apply a continuation to an array of arguments
 *
 * @param {!Continuation}
 * @param {*} args a possibly empty, possibly [null] array of arguments to pass to the continuation
 */
function execute1(k, arg) {
    return execute_(k, [arg]);
}

/**
 * Apply a continuation to an exception
 */
function executexn(k, exn) {
    var payload = k[1][2];
    payload = (payload?payload:default_handler_cont(k)[0]);
    payload.apply(k[1][0],[exn]);

}

function ccont(k, f) {
    return [f, k[1]];
}

function catch_(k, h) {
    var k = ccont(k, k[0]);
    k[1][2] = function(x){ return h(x, k) };
    return k;
}

function callcc_directive(f, k){
    return f(k, cont(function(){}, k[1][0]));
}

function default_handler_cont(k){
    return cont(function(exn){console.error("Error : uncaught OPA exn", exn)}, k[1][0], k[1][1]);
}

function handler_cont(k){
    var paylexn = k[1][2];
    return (paylexn ? cont(paylexn, k[1][0], k[1][1]) : default_handler_cont(k));
}

function with_thread_context(tc, k){
    return [k[0], [tc, k[1][1], k[1][2], k[1][3]]];
}

function thread_context(k){
    return k[1][0];
}

function update_cont(k, pk, name, pos, args){
    var parent = pk ? pk[1][3] : null;
    return [k[0], [tc, k[1][1], k[1][2], {name:name, pos:pos, args:args, parent:parent}]];
}

function get_cps_trace(k){
    var res = "";
    var tmp = k[1][3];
    while (tmp != null){
        res += tmp.name + "called at " + tmp.pos + "\n";
        tmp = tmp.parent;
    }
    return res;
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
 * Maximal number of consecutive return allowed by thread before it be scheduled.
 */
var max_return_by_thread = 100;

/**
 * Maximal number of thread computation before schedule to JS VM
 */
var max_consecutive_thread = 5;

/**
 * An infinite scheduling loop.
 *
 * Takes the tasks waiting in [ready], execute them. If [ready] is empty, sleep and wake up
 *
 * Stop on fatal error.
 */
function loop_schedule(blocking)
{
    var fatal_error   = false;//[true] if we stopped scheduling because of a fatal error, [false] otherwise
    var tasks         = ready;//Keep a local copy. In most JS VMs, this will speed-up code.
    var task;
    loop_level++;
    is_schedule = true;
    try
    {
        for(var t=0; tasks.length != 0 && (blocking || t < max_consecutive_thread); t++)
        {
            task = tasks.shift();
            var r = task();
            #<Ifstatic:OPABSL_NODE>
            for(var i=0; i<max_return_by_thread && r; i++) r =execute1(r[0], r[1]);
            if (r) push(task_from_return(r[0], [r[1]]));
            #<Else>
            execute1(r[0], r[1])
            #<End>
        }
        if(tasks.length != 0) {
            #<Ifstatic:OPABSL_NODE>
            process.nextTick(loop_schedule)
            #<Else>
            setTimeout(loop_schedule, 0)
            #<End>
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
    #<Ifstatic:OPABSL_NODE>
    if (r = execute1(k, x)) push(task_from_return(r[0], [r[1]]));
    #<Else>
    execute1(k, x);
    #<End>
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
        var k = cont(barrier.release, barrier, null);
        f = wrap_tc(f);
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
        var b = new Barrier(name);
        var r = function(x){b.release(x)};
        var k = pk == null?cont(r):ccont(pk, r);
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
        var k = a.pop();
        return return_(k, f.apply(this, a));
    }
}

/**
 * Transform an opa cps callback (-> void) to a js_callback
 */
function opa_cps_callback_to_js_callback0(k, f){
    return function(){
      if (f === undefined) return ccont(k,function (){}); // ??? We're getting undefined fns here
      return f(ccont(k, function(){}));
    };
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
