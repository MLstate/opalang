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

type 'a t = ('a -> unit) -> unit

module Ops = struct
  let (@>) f k = f k
  let (|>) x k = k x
end
open Ops

module List = struct
  let rec map f l k = match l with
    | [] -> [] |> k
    | hd::tl -> f hd @> fun hd -> map f tl @> fun tl -> hd::tl |> k

  let rec fold f acc l k = match l with
    | [] -> acc |> k
    | hd::tl -> f acc hd @> fun acc -> fold f acc tl @> k
end

module Option = struct
  let map f opt k = match opt with
    | None -> None |> k
    | Some x -> f x @> fun x -> Some x |> k
end

module Lazy = struct
  type 'a t = {
    push : (unit -> unit) -> unit;
    mutable value : 'a option;
    mutable waiters : ('a -> unit) list;
    cps : ('a -> unit) -> unit;
  }

  let make push cps = {
    push = push;
    value = None;
    waiters = [];
    cps = cps;
  }

  let force l k =
    match l.value with
    | Some x -> x |> k
    | None when l.waiters != [] ->
        l.waiters <- k::l.waiters
    | None ->
        l.waiters <- k::l.waiters;
        l.cps
        @> function x ->
          Base.List.iter (fun k -> l.push (fun () -> k x)) l.waiters;
          l.value <- Some x;
          l.waiters <- []

  let get_state cps = cps.value

  let lazy_from_val x = {
    push = (fun _ -> ());
    value = Some x;
    waiters = [];
    cps = fun k -> k x;
  }
end
