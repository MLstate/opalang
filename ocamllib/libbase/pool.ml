(*
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
*)

(* TODO, to proper benchmark to known if this chicken beast has any positive effect *)

#<Debugvar:BUFFER_POOL_DEBUG>

module Pool = functor (Egg:Egg.Egg) -> struct
  type egg = Egg.egg
  type size = int
  type pool = {
    mutable list  : egg list; (* free buffers *)
    mutable free  : int;        (* number of free *)
    mutable total : int;        (* number of buffer = free + used *)
    mutable maximal_total : int;(* maximal pool total size *)
    mutable initial_size : size; (* default initial size for buffers *)
    mutable dealloc_size : size  (* automatic forget of bigger buffer *)
  }

  let bound_cardinal_and_total_size p ~cardinal ~total_size =
    p.list <- [];
    p.total <- p.total - p.free;
    p.free <- 0;
    p.maximal_total <- (max 0 cardinal);
    p.dealloc_size <- total_size/(min p.maximal_total 1)

  let debug str = Printf.eprintf "%s\n%!" str

  let _Kb = 1024
  let _Mb = 1024 * _Kb

  let max_total = #<If:BUFFER_POOL_ENABLE> 256 #<Else> 0 #<End>

  let create () = {
    list = [];
    total = 0;
    free = 0;
    maximal_total = max_total;
    initial_size = 128;
    dealloc_size = 256 * _Mb / (max 1 max_total)
  }

  let unsafe_free pool egg =
    let size = Egg.size egg in
    if (size < pool.dealloc_size) && (pool.total <= pool.maximal_total) then (
      #<If>debug (Printf.sprintf "free_buf(%d): return" pool.free)#<End>;
      pool.list <- egg::pool.list;
      pool.free <- pool.free + 1
    ) else (
      #<If>debug (Printf.sprintf "free_buf(%d): reset" pool.free)#<End>;
      (* Bug.reset ; USELESS *)
      pool.total <- pool.total - 1;
    )

  let shallow_copy (egg:'a) =
    let eggt = Obj.repr egg in
    assert(Obj.double_array_tag <> Obj.tag eggt);
    (Obj.obj (Obj.dup (Obj.repr egg)):'a)

  let shallow_transfer ~from ~to_ =
    let from = Obj.repr from in
    let to_ = Obj.repr to_ in
    for i = 0 to (min (Obj.size from) (Obj.size to_)) - 1 do
      Obj.set_field to_ i (Obj.field from i)
    done

  let finalize pool egg younger =
    shallow_transfer ~from:younger ~to_:egg;
    unsafe_free pool egg

  let mark_as_used pool egg =
    let younger = shallow_copy egg in
    Gc.finalise (finalize pool egg) younger;
    younger

  let collect () = () (*ignore(Gc.minor ())*) (* triggering gc changes almost nothing *)

  let independant_alloc _pool hint =
    #<If>debug "extra wild buffer"#<End>;
    Egg.create hint

  let pool_alloc pool hint =
    #<If>debug (Printf.sprintf "get_buf(%d/%d): %s" pool.free pool.total (if pool.list=[] then "new" else "old"))#<End>;
    if pool.list = [] then collect ();
    match pool.list with
    | [] ->
      pool.total <- pool.total + 1;
      Egg.create hint
    | b::t ->
      pool.free <- pool.free - 1;
      pool.list <- t;
      Egg.clear b;
      b

  let alloc pool =
    fun ?(hint=pool.initial_size) () ->
     if pool.total >= pool.maximal_total && pool.list==[] then independant_alloc pool hint
     else mark_as_used pool (pool_alloc pool hint)

end

module BufferEgg = struct
  type egg = Buf.t
  let create = Buf.create
  let size = Buf.real_length
  let clear = Buf.clear
end
