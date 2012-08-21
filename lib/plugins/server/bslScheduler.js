/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/** @externType continuation('a) */
/** @externType Scheduler.key */

/**
 * @register {int, (-> void) -> void}
 */
function sleep(delay, todo) {
    setTimeout(function(){ todo() },delay);
    return js_void;
}

/**
 * @register {int, (-> void) -> Scheduler.key}
 */
function asleep(delay, todo) {
    var key = setTimeout(function(){ todo() }, delay);
    return key;
}

/**
 * @register {(-> void) -> void} push
 */
function push_ (todo) {
    // WHAT ?
    #<Ifstatic:OPA_CPS_CLIENT>
    push(todo);
    return js_void;
    #<Else>
    return sleep(0, todo);
    #<End>
}

/**
 * @register {int, (-> void) -> void}
 */
function timer (delay,todo) {
    function js_todo(){
        todo();
        setTimeout(js_todo,delay);
    }
    setTimeout(js_todo,delay);
    return js_void;
}

/**
 * @register {Scheduler.key -> void}
 */
function abort (key) {
    clearTimeout(key);
    return js_void;
}

/**
 * @register {int, (continuation(opa[void]) -> void), \
              continuation(opa[void]) -> void}
 * @cpsBypass
 */
function sleep_cps(delay, todo, k) {
    sleep(delay, opa_cps_callback_to_js_callback0(k, todo));
    return_(k, js_void);
}

/**
 * @register {int, (continuation(opa[void]) -> void), \
              continuation(opa[Scheduler.key]) -> void}
 * @cpsBypass
 */
function asleep_cps(delay, todo, k) {
    var key = asleep(delay, opa_cps_callback_to_js_callback0(k, todo));
    return_(k, key);
}

/**
 * @register {(continuation(opa[void]) -> void), \
              continuation(opa[void]) -> void}
 * @cpsBypass
 */
function push_cps(todo, k) {
    push_(opa_cps_callback_to_js_callback0(k, todo));
    return_(k, js_void);
}

/**
 * @register {int, (continuation(opa[void]) -> void), \
              continuation(opa[void]) -> void}
 * @cpsBypass
 */
function timer_cps(delay, todo, k) {
    timer(delay, opa_cps_callback_to_js_callback0(k, todo));
    return_(k, js_void);
}

#<Ifstatic:OPABSL_NODE>
// TODO Something with the scheduler is probably missing

process.on('SIGINT', function (){process.exit(1)});
process.on('SIGKILL', function (){process.exit(1)});
process.on('SIGTERM', function (){process.exit(1)});
#<End>

/**
 * @register {(-> void) -> void}
 */
function at_exit (a) {

#<Ifstatic:OPABSL_NODE>
    process.on('exit', function () {
       a();
    });
#<Else>
    return js_void;
#<End>

}
