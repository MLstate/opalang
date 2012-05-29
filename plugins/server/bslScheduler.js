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

##extern-type Scheduler.key

##register timer : int, (-> void) -> void
##args(delay,todo)
{
    function js_todo(){
        todo();
        setTimeout(js_todo,delay);
    }
    setTimeout(js_todo,delay);
}

##register sleep : int, (-> void) -> void
##args(delay,todo)
{
    setTimeout(function(){ todo() },delay);
}

##register asleep : int, (-> void) -> Scheduler.key
##args(delay, todo)
{
    var key = setTimeout(function(){ todo() }, delay);
    return key
}

##register push : (-> void) -> void
##args(todo)
{
    %%BslScheduler.sleep%%(0, todo);
}

##register abort : Scheduler.key -> void
##args(key)
{
    clearTimeout(key);
    return js_void;
}
