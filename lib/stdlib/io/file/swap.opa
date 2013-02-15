/*
    Copyright © 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#<Ifstatic:OPA_BACKEND_QMLJS>
abstract
type Swap.t('a) = {
  string           path,
  File.descriptor  fd,
  Barrier.t('a)    bar,
  reference(int)   pos,
  reference(list(continuation(void or {binary chunk}))) wlist,
}

module Swap{

  function Swap.t('a) allocate(iter(binary) iter, 'a acc, (binary, 'a -> 'a) f){
    path = "/tmp/.opa/swap/{Random.string(20)}"
    ignore(File.mkdir("/tmp/.opa"))
    ignore(File.mkdir("/tmp/.opa/swap"))
    fd = File.open(path, "w+", none)
    bar = Barrier.make()
    pos = Reference.create(0)
    wlist = Reference.create([])
    /* Asynchonously save on disk */
    Scheduler.push{ function()
      res = Iter.fold(
        function(bin, acc){
          blen = Binary.length(bin)
          recursive function aux(pw){
            bw = File.d_write(fd, bin, some(Reference.get(pos)));
            x = @atomic(
              x = Reference.get(wlist);
              _ = Reference.set(wlist, [])
              x
            )
            List.iter(Continuation.return(_, {}), x)
            bw = pw + bw;
            if(bw < blen){
              aux(bw);
            } else {
              // Note: Only one thread can update this reference
              Reference.set(pos, Reference.get(pos)+bw);
            }
          }
          aux(0)
          f(bin, acc)
        }, iter, acc)
      // Indicates that the input is completly written
      Reference.set(pos, -1)
      List.iter(Continuation.return(_, {}), Reference.get(wlist))
      Barrier.release(bar, res)
    }
    ~{fd, pos, bar, wlist, path}
  }

  /**
   * Returns an iterator from the swap.
   */
  function iter(binary) iter(Swap.t('a) ~{fd, pos, wlist, ...}, int chunk_size){
    recursive function option((binary, iter(binary))) read(cur){
      result = @callcc(function(k){
        action = @atomic(
          p = Reference.get(pos)
          if(p != -1 && cur+chunk_size > p){
            Reference.set(wlist, [k|Reference.get(wlist)])
            {}
          } else {
            { read }
          }
        )
        match(action){
        case {}: void
        case {read}:
          Continuation.return(k, {chunk: File.d_read(fd, chunk_size, some(cur))})
        }
      })
      match(result){
      case ~{chunk}:
        clen = Binary.length(chunk)
        if(clen==0) {
          none
        } else {
          {some: (chunk, {function next(){ read(cur + clen)}})}
        }
      case {}: read(cur)
      }
    }
    {function next(){read(0)}}
  }

  /**
   *
   */
  function result(Swap.t swap){
    Barrier.wait(swap.bar)
  }

  function release(Swap.t swap){
    ignore(File.close(swap.fd));
    ignore(File.unlink(swap.path));
  }

}
#<End>
