/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
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
    push(new Task_from_application(todo, []));
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

// Stub
// TODO Something with the scheduler is probably missing

var tty = require("tty");
process.openStdin().on("keypress", function(_chunk, key) {
  if(key && key.name === "c" && key.ctrl) process.exit(0);
});
tty.setRawMode(true);

process.on('SIGINT', function (){process.exit(1)});
process.on('SIGKILL', function (){process.exit(1)});
process.on('SIGTERM', function (){process.exit(1)});

##register at_exit : (-> void) -> void
##args(a)
{
    process.on('exit', function () {
       a();
    });
}
