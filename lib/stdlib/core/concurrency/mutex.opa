/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin server
/**
 * Utils for concurrency (Mutex, exclusive section, exclusive resource access, reference guarded by a mutex ...)
 *
 * @category concurrency
 * @destination private
 * @author Rudy Sicard
 * @stability untested
 */

/**
 * {1 About this module}
 *
 * This module provide a generic mutex type to handle operation on exclusive resources.
 *
 * {1 Where should I start?}
 *
 *  Exclusive section:
 *    To guanrantee that two function f1 and f2 are never executed at the same time.
 *    mysection = Mutex.exclusive_section()
 *    f1() = mysection{ ->
 *        code1
 *    }
 *    f2() = mysection{ ->
 *        code2
 *    }
 *
 *  Reference suitable for concurrency:
 *     myref = MutexRef.create(int)
 *
 *     // Exclusive update operation
 *     incr() = MutexRef.update(myref){
 *        i -> i +1
 *    }
 *
 *     // Exclusive arbitrary operation
 *     alone() = MutexRef.exlcusive(myref){
 *       doing thing with myref
 *           without worrying about concurrent comptuation
 *    }
 *
 * {1 What if I need more?}
 */

// TODO check context preservation
@private
type Mutex.state('a) =
  {unlocked}
/ {locked:list('a) process:list('a)->void} // information waiting for unlock, and unlock processing function

/** Generic mutex type, the 'a type designates the type of information provide while locking */
@abstract
type Mutex.generic('a) = reference(Mutex.state('a))

/** Standard mutex, lock an already locked mutex freeze the computation until the mutex is unlocked */
type Mutex.t = Mutex.generic(continuation(void))

@private Scheduler = {{
  push  = @may_cps(%%BslScheduler.push%%)
}}

@server_private
Mutex = {{

  @private
  get(v:Mutex.generic) = Reference.get(v)
  @private
  set(v:Mutex.generic,st) = Reference.set(v,st)

  /** try to unlock the mutex, with the possiblity to stack a continuation to be executed when the mutex will be unlocked */
  @private
  try_lock_or_stack(v:Mutex.generic,k,process) = @atomic(
    match Reference.get(v)
    {unlocked} ->
      do Reference.set(v,{locked=[] ~process})
      true

    {locked=l process=_} as r ->
      do match k
        {some=k} ->
          Reference.set(v,{r with locked=[k|l]} <: Mutex.state)
        {none}   ->
          void
      false
  )

  /** unlock the mutex and returns the stack of continuation to be executed */
  @private
  try_unlock_with_stack(v:Mutex.generic) = @atomic(
    match Reference.get(v)
    {unlocked} -> none
    {locked=_ process=_} as r->
      do if r.locked == [] then Reference.set(v,{unlocked})
                 else Reference.set(v,{r with locked=[]} <: Mutex.state)
      some(r)
  )

  @private
  lock_or_reschedule_n_times_and_continue(v,n,k,e,process):void =
    to_stack = n<=0
    if try_lock_or_stack(v, if to_stack then some(e) else none, process)
    then Continuation.return(k,void)
    else
      if to_stack then void
      else Scheduler.push( -> lock_or_reschedule_n_times_and_continue(v,n-1,k,e,process) )

  @private
  lock_or_reschedule_n_times(v,n) = @callcc(k ->
    process = List.iter(Continuation.return(_,void),_)
    lock_or_reschedule_n_times_and_continue(v,n,k,k, process)
  )

  display_exc(msg,exc) =
    Scheduler.push( -> error("{msg}.\n Exception is {Debug.dump(exc)}") )

  unlock_error(exc) =
    display_exc("A error was raised during processing locked infromation of a Mutex, the unlocking will continue but something is probably wrong", exc)

  @private
  unlock_(v:Mutex.t) =
    match try_unlock_with_stack(v)
    {none} -> void

    {some=~{locked process}} ->
      if locked != [] then
        do @catch( unlock_error,
          process(locked)
        )
        unlock_(v)
      else void

  /** try to lock the mutex, faster than a simple lock */
  try_lock(v) = try_lock_or_stack(v, none, (_->void) )


  // TODO test if rescheduling is faster than locking
  /** lock the mutex */
  lock(v:Mutex.t) =
    NO_reschedule = 0
    lock_or_reschedule_n_times(v,NO_reschedule)

  /** unlock the mutex */
  unlock(v:Mutex.t) = unlock_(v)

  /** create a new standard mutex */
  create():Mutex.t = Reference.create({unlocked})

  exclusive_error(v)(exc) =
    do unlock(v)
    do display_exc("A error was raised during an exclusive section, the exclusive section has been unlocked but something is probably wrong", exc)
    error("Exclusive section error")

  /** exclusive operation wrt to the mutex */
  exclusive(v:Mutex.t)(f) =
    do lock(v)
    r=@catch( exclusive_error(v),
      f()
    )
    do unlock(v)
    r

  /** exclusive section (exclusive operation over an anonymous mutex) */
  exclusive_section() =
    v = create()
    exclusive(v)

  is_locked(v:Mutex.generic) = Reference.get(v)!={unlocked}
  lock_or_reschedule_n_times_generic(v,n,e,process) = @callcc(k -> lock_or_reschedule_n_times_and_continue(v,n,k,e,process))
  /** create a new generic mutex */
  create_generic():Mutex.generic = Reference.create({unlocked})
  /** lock the mutex with generic wait data and continue function */
  lock_generic(v:Mutex.generic,e,process) = lock_or_reschedule_n_times_generic(v,1,e,process)
  /** unlock a generic mutex */
  unlock_generic(v:Mutex.generic) = unlock_(v)



}}

@abstract
type MutexRef.t('a) = {content:reference('a) mutex:Mutex.t}

MutexRef = {{
  /** Create a reference with exclusive operation */
  create(v) = {content=Reference.create(v) mutex=Mutex.create()} : MutexRef.t
  immediate_get(t:MutexRef.t)      = Reference.get(t.content)
  immediate_set(t:MutexRef.t,v)    = Reference.set(t.content,v)
  @private
  immediate_update(t,f) =
    immediate_set(t, f(  immediate_get(t) ) )

  /** non exclusive get */
  get(t) = immediate_get(t)

  /** non exclusive set
    WARNING: you should not use set based on a value obtain by a get sooner, use update in this case */
  set(t,v) = immediate_set(t,v)

  /** exclusive update */
  update(t:MutexRef.t)(f) = Mutex.exclusive(t.mutex){ ->
    immediate_update(t,f)
  }

  /** exclusive operation */
  exclusive(t:MutexRef.t)(f) = Mutex.exclusive(t.mutex)(f)

  /** exclusive get */
  exclusive_get(t:MutexRef.t) = Mutex.exclusive(t.mutex){ ->
    immediate_get(t)
  }

}}
