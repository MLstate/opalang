(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(*
    @author Louis Gesbert
**)

(* temporary place-holder (see mli) *)

type 'a t = {
  items : 'a array;
  flat_replication : int;
}

type key = int (* keep this abstract *)

let create ~flat_replication items = {
  items = Array.of_list items;
  flat_replication = flat_replication;
}

(* todo: move somewhere else *)
module Parallel_schedule = struct
  open Cps.Ops

  (** Runs a set of cps functions in parallel, ending with a barrier then
      a single continuation. Returns right away.

      Provided "iter" function should iter on all the tasks you want to run. It
      should never schedule, this function assumes it is atomic (consistently,
      it's not CPS). For example, to execute function [f] in parallel over array
      [arr], you would use the iter function
      [(fun g -> Array.iter (fun x -> g (f x)) arr)] *)
  let iter sched (iter: (unit Cps.t -> unit) -> unit) (k: unit -> unit) =
    let n = ref 0 in
    let k () = decr n; if !n > 0 then () else k () in
    iter (fun f -> incr n; Scheduler.push sched (fun () -> f @> k))

  (** Same as iter but passes the array of results to the continuation. *)
  let map sched (iter: ('a Cps.t -> unit) -> unit) (k: 'a array -> unit) =
    let n = ref 0 in
    let results = ref [||] in
    let ki =
      fun i x ->
        !results.(i) <- Some x;
        decr n;
        if !n > 0 then () else Array.map Option.get !results |> k
    in
    iter
      (fun f ->
         let i = !n in incr n;
         Scheduler.push sched (fun () -> f @> ki i));
    results := Array.make !n None

  (** Same as map but reduces the results with the given operator. Order
      of reduction is {b not} guaranteed, you should probably use an associative,
      commutative operator. *)
  let reduce sched
      (iter: ('a Cps.t -> unit) -> unit)
      (op: 'acc -> 'a -> 'acc)
      (acc: 'acc)
      (k: 'acc -> unit) =
    let n = ref 0 in
    let acc = ref acc in
    let k x = acc := op !acc x; decr n; if !n > 0 then () else !acc |> k  in
    iter (fun f -> incr n; Scheduler.push sched (fun () -> f @> k))

  let map_reduce sched
      (iter: ('a Cps.t -> unit) -> unit)
      (op: 'acc -> 'a -> 'b * 'acc)
      (acc: 'acc)
      (k: 'b array * 'acc -> unit) =
    let n = ref 0 in
    let acc_ref = ref acc in
    let results = ref [||] in
    let ki =
      fun i x ->
        let elt_i, acc = op !acc_ref x in
        acc_ref := acc;
        !results.(i) <- Some elt_i;
        decr n;
        if !n > 0 then () else (Array.map Option.get !results, !acc_ref) |> k
    in
    iter
      (fun f ->
         let i = !n in incr n;
         Scheduler.push sched (fun () -> f @> ki i));
    results := Array.make !n None
end

let to_list t = Array.to_list t.items

module P = Parallel_schedule
open Cps.Ops

let sched = Scheduler.default

let iter t f k = P.iter sched (fun g -> Array.iter (fun x -> g (f x)) t.items) @> k

let sequential_iter t f = Array.iter (fun x -> f x @> fun _ -> ()) t.items

let map t f k =
  P.map sched (fun g -> Array.iter (fun x -> g (f x)) t.items)
  @> fun arr -> { t with items = arr } |> k

let mapi t f k =
  P.map sched (fun g -> Array.iteri (fun key x -> g (f key x)) t.items)
  @> fun arr -> { t with items = arr } |> k

let reduce t op acc f k =
  P.reduce sched (fun g -> Array.iter (fun x -> g (f x)) t.items)
    op acc @> k

let map_reduce t op acc f k =
  P.map_reduce sched (fun g -> Array.iter (fun x -> g (f x)) t.items) op acc
  @> fun (arr,res) -> ({ t with items = arr }, res) |> k


let who_has t (path: Path.t) =
  (Hashtbl.hash path mod
     (Array.length t.items / t.flat_replication))
  (* * Random.int t.flat_replication
     -- we should take always the same random within a transaction *)

let who_has_all t (path: Path.t) =
  let offset = who_has t path in
  let chunksize = Array.length t.items / t.flat_replication in
  Base.List.init t.flat_replication
    (fun i -> i * chunksize + offset)

let at_path t path f k =
  let key = who_has t path in
  f key t.items.(key) @> k

let mapi_path t path f k =
  let where = who_has_all t path in
  P.map sched (fun g -> List.iter (fun key -> g (f key t.items.(key))) where)
  @> fun results ->
    let arr = Array.copy t.items in
    Base.List.iteri (fun key i -> arr.(key) <- results.(i)) where;
    { t with items = arr } |> k

let get_key t key = t.items.(key)

let set_key t key value =
  let arr = Array.copy t.items in
  t.items.(key) <- value;
  { t with items = arr }

let push_key t key value = t.items.(key) <- value
