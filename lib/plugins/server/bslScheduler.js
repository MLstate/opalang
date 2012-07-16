/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
##extern-type continuation('a)
// '
##extern-type Scheduler.key

##register sleep : int, (-> void) -> void
##args(delay,todo)
{
    setTimeout(function(){ todo() },delay);
    return js_void;
}

##register asleep : int, (-> void) -> Scheduler.key
##args(delay, todo)
{
    var key = setTimeout(function(){ todo() }, delay);
    return key;
}

##register push : (-> void) -> void
##args(todo)
{
    // WHAT ?
    #<Ifstatic:OPA_CPS_CLIENT>
    push(todo);
    return js_void;
    #<Else>
    return %%BslScheduler.sleep%%(0, todo);
    #<End>
}

##register timer : int, (-> void) -> void
##args(delay,todo)
{
    function js_todo(){
        todo();
        setTimeout(js_todo,delay);
    }
    setTimeout(js_todo,delay);
    return js_void;
}

##register abort : Scheduler.key -> void
##args(key)
{
    clearTimeout(key);
    return js_void;
}

##register [cps-bypass] sleep_cps : \
int, (continuation(opa[void]) -> void), continuation(opa[void]) -> void
##args(delay, todo, k)
{
    %%BslScheduler.sleep%%(delay, opa_cps_callback_to_js_callback0(k, todo));
    return_(k, js_void);
}

##register [cps-bypass] asleep_cps : \
int, (continuation(opa[void]) -> void), continuation(opa[Scheduler.key]) -> void
##args(delay, todo, k)
{
    var key = %%BslScheduler.asleep%%(delay, opa_cps_callback_to_js_callback0(k, todo));
    return_(k, key);
}

##register [cps-bypass] push_cps : \
(continuation(opa[void]) -> void), continuation(opa[void]) -> void
##args(todo, k)
{
    %%BslScheduler.push%%(opa_cps_callback_to_js_callback0(k, todo));
    return_(k, js_void);
}

##register [cps-bypass] timer_cps :                     \
int, (continuation(opa[void]) -> void), continuation(opa[void]) -> void
##args(delay, todo, k)
{
    %%BslScheduler.timer%%(delay, opa_cps_callback_to_js_callback0(k, todo));
    return_(k, js_void);
}

#<Ifstatic:OPABSL_NODE>
// TODO Something with the scheduler is probably missing

process.on('SIGINT', function (){process.exit(1)});
process.on('SIGKILL', function (){process.exit(1)});
process.on('SIGTERM', function (){process.exit(1)});
#<End>

##register at_exit : (-> void) -> void
##args(a)
{

#<Ifstatic:OPABSL_NODE>
    process.on('exit', function () {
       a();
    });
#<Else>
    return js_void;
#<End>

}
