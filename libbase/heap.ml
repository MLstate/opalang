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
    @author Damien Lefortier
    @author Corentin Gallet
**)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

(**

   This file contains multiple implementations of (min) heaps.

   [From Wikipedia, the free encyclopedia]

   A heap is a tree data structure with ordered nodes where the min
   (or max) value is the root of the tree and all children are less
   than (or greater than) their parent nodes.

   All the different implementations respect the following signature.

   - Binary Heaps:

   Imperative implementation, more efficient but not persistent.
   Do not support merging.

   - Binomial Heaps:

   Functional implementation.
   Support merging.

   - Soft Heaps.

   Functional implementation.
   Support merging.

   In comparison to previous structures, a better *amortized*
   complexity can be obtained by allowing some elements of the heap to
   be *corrupted*, i.e. to have their value increased after their
   insertion.  But more the number of extraction is important, more
   the soft heaps are efficient, thus it isn't interesting at all to
   use them just to do insertions.

**)

module type Sig = sig
  type elt
  type t
  val empty : unit -> t
  val is_empty : t -> bool
    (** If the optional argument minimum is true, then the minimum is
	not updated which is faster in some cases, this feature should
	be used when many insertions are done in a row *)
  val insert : t -> ?minimum:bool -> elt -> t
    (** This function removes the minimum of the heap *)
  val remove : t -> t
  val minimum : t -> elt option
  val merge : t -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val size : t -> int
end

module Binary(X : Ordered) = struct
  type elt = X.t

  type t = { mutable length : int; mutable data : X.t array }

  let empty() = { length = 0 ; data = [||] }

  let is_empty h =
    h.length = 0

  let insert h x =
    let resize h x =
      let n = Array.length h.data in
      let data = Array.create (2 * n + 1) x in
	Array.blit h.data 0 data 0 n ; h.data <- data ; h
    in
    let h = if h.length = Array.length h.data then resize h x else h in
    let rec aux i =
      let j = (i - 1) / 2
      in if i > 0 && X.compare h.data.(j) x > 0 then begin
	  h.data.(i) <- h.data.(j) ;
	  aux j
	end else begin
	  h.data.(i) <- x end
    in
      aux h.length ; h.length <- h.length + 1 ; h

  let remove h =
    let n = h.length - 1 in
    let rec aux i x =
      let j = 2 * i + 1 in
      let k = j + 1 in
      if j < n && (X.compare h.data.(j) x < 0 || X.compare h.data.(k) x < 0) then begin
	let j = if k < n && X.compare h.data.(j) h.data.(k) > 0 then k else j
	in
	h.data.(i) <- h.data.(j) ;
	aux j x
      end else
	h.data.(i) <- x
    in match h.length with
    | 0 -> h
    | _ -> aux 0 h.data.(n) ; h.length <- n ; h
	
  let minimum h = if h.length = 0 then None else Some h.data.(0)

  let merge _ _ = assert false

  let iter f h =
    if (h.length == 0) then
      ()
    else
      let rec aux k =
	f h.data.(k);
	if (k < h.length - 1) then
	  aux (k + 1);
      in aux 0

  let fold f h x0 =
    if (h.length == 0) then
      x0
    else
      let rec aux k l =
	match l with
	| 0 -> f h.data.(0) k
	| _ -> aux (f h.data.(l) k) (l - 1)
      in aux x0 (h.length - 1)

  let size h = h.length

  let to_string print_elt h =
    let data = Array.to_list h.data in
    let data = Base.String.concat_map "; " print_elt data in
    Printf.sprintf "heap{ len = %d; data = [%s]}" h.length data
end

module type Epsilon = sig
  val epsilon : float
end

(** This is based on the following paper.

\@InProceedings\{
 author		= \{Kaplan,, Haim and Zwick,, Uri\},
 title		= \{A simpler implementation and analysis of Chazelle's soft heaps\},
 booktitle	= \{SODA '09: Proceedings of the Nineteenth Annual ACM -SIAM
		  Symposium on Discrete Algorithms\},
 year		= \{2009\},
 pages		= \{477--485\},
 location	= \{New York, New York\},
 publisher	= \{Society for Industrial and Applied Mathematics\},
 address	= \{Philadelphia, PA, USA\},
\}

**)

module Soft(X : Ordered)(E : Epsilon) = struct
  type elt = X.t

  (* A tree is as follows: (left,ckey,list,size,right) *)
  type tree =
    | Empty
    | Node of tree * X.t * X.t list * int * tree

  (* A node is as follows: [(root, smin, rank)], rank *)
  type t = (tree * tree ref * int) list * int

  let rate =
    let log2 x = log10 x /. log10 2.0
    in int_of_float (log2 (E.epsilon)) + 5

  let empty() = [],0

  let is_empty p = match fst p with
  | [] -> true
  | _ -> false

  let leaf = function
    | Node (Empty,_,_,_,Empty) -> true
    | _ -> false
  let ckey = function
    | Empty -> assert false
    | Node (_,ckey,_,_,_) -> ckey

  let rec sift x =
    match x with
      | Empty -> assert false
      | Node (Empty,_,_,_,Empty) -> x
      | Node (_,_,c,s,_) when List.length c >= s -> x
      | Node (l,ckey_,c,s,r) ->
	  let aux = function
	    | Empty
	    | Node (Empty,_,_,_,_) -> assert false
	    | Node (Node (l2,ckey_,c2,s2,r2) as l1,_,c1,s1,r1) ->
		let l1 = match leaf l1 with
		  | true -> Empty
		  | _ -> sift (Node (l2,ckey_,[],s2,r2))
		in Node (l1,ckey_,c1 @ c2,s1,r1)
	  and x = match l,r with
	    | Empty,_ -> Node (r,ckey_,c,s,l)
	    | l,r when r <> Empty && ckey l > ckey r -> Node (r,ckey_,c,s,l)
	    | _ -> x
	  in sift (aux x)

  let rank = function
    | (_,_,rank) -> rank
  let smin = function
    | (_,smin,_) -> smin
  let root = function
    | (root,_,_) -> root

  let combine x y = match root x with
    | Empty -> assert false
    | Node (_,ckey,_,size,_) ->
	let size = if rank x + 1 <= rate then 1 else (3 * size + 1) / 2
	in sift (Node (root x,ckey,[],size,root y)), smin x, rank x + 1

  let repeated_combine (lq,rq) k =
    let rec aux = function
      | [] -> assert false
      | x :: [] -> [x], rank x
      | x :: y :: lq when rank x = rank y ->
	  (match lq with
	     | z :: lq when rank z = rank x ->
		 let lq,rq = aux (combine y z :: lq)
		 in x :: lq, rq
	     | _ -> aux (combine x y :: lq))
      | x :: lq when rank x > k -> x :: lq, rq
      | x :: y :: lq ->
	  let lq,rq = aux (y :: lq)
	  in x :: lq, rq
    in aux lq

  let update_smin q f =
    let rec aux = function
      | [] -> []
      | x :: lq ->
	  let update (root,_,rank) = function
	    | [] -> [(root,ref root,rank)]
	    | y :: lq -> (
		if ckey root <= ckey (!(smin y)) then (root,ref root,rank)
		else (root,ref !(smin y),rank)
	      ) :: y :: lq
	  in match f x with
	    | `continue x -> update x (aux lq)
	    | `break x -> update x lq
	    | `delete _x -> lq
    in aux (fst q), snd q

  let merge_ update p q =
    let merge_into (lp,rp) (lq,rq) =
      List.merge (fun x y -> compare (rank x) (rank y)) lp lq, max rp rq
    and p,q = match p,q with
      | p,q when snd p > snd q -> q,p
      | _ -> p,q in
    let q = repeated_combine (merge_into p q) (snd p)
    in match update with
      | true -> update_smin q (fun x -> `continue x)
      | false -> q

  let merge = merge_ true

  let insert p ?(minimum=true) e =
    let node = Node (Empty,e,[e],1,Empty)
    in merge_ minimum ([node,ref node,0],0) p

  let remove p = match fst p with
    | [] -> p
    | x :: _lq ->
	match !(smin x) with
	  | Empty -> assert false
	  | Node (_,_,[],_,_) -> assert false
	  | Node (l,ckey,_ :: c,s,r) ->
	      let n = Node (l,ckey,c,s,r) in
	      let aux (_root,size,rank) =
		if 2 * List.length c <= s then
		  match leaf n with
		    | false -> `break (sift n,size,rank)
		    | true ->
			if List.length c = 0 then `delete n
			else `break (n,size,rank)
		else `break (n,size,rank) in
	      let f = (fun y ->
			 if root y == !(smin x) then (aux y) else `continue y)
	      in update_smin p f

  let minimum p = match fst p with
    | [] -> None
    | x :: _lq ->
	match !(smin x) with
	  | Empty -> assert false
	  | Node (_,_,[],_,_) -> assert false
	  | Node (_,_,e :: _,_,_) -> Some e

  (* Useful for debug *)
  let display string_of_elt (lp,rp) =
    let string_of_list = function
      | [] -> ""
      | x :: xs -> List.fold_left (
	  fun acc x -> acc ^ "," ^ string_of_elt x
	) (string_of_elt x) xs
    in
    let rec dfs indent = function
      | Empty -> ()
      | Node (l,ckey,c,s,r) ->
	  Printf.printf "%s ckey = %s, list = [%s](%d), size = %d\n"
	    indent (string_of_elt ckey) (string_of_list c) (List.length c) s ;
	  dfs (indent ^ " ") l ;
	  dfs (indent ^ " ") r
    in
      Printf.printf "Heap of rank is %d with rate %d:\n" rp rate;
      List.iter (
	fun x ->
	  Printf.printf ">> Tree of rank is %d:\n" (rank x) ;
	  dfs "  " (root x)
      ) lp

  let rec iter f h =
    match h with
    | [],_ -> ()
    | (a, _, _)::t, _ ->
	let rec aux ff tree =
	  match tree with
	  | Empty -> ()
	  | Node (l,_,c,_,r) ->
	      aux ff l; List.iter ff c; aux ff r
	in aux f a; iter f (t, 0);;

  let rec fold f h h0 =
    match h with
    | [], _ -> h0
    | (a,_,_)::t, _ ->
	let rec aux ff tree accu =
	  match tree with
	  | Empty -> accu
	  | Node (l,_,c,_,r) ->
	      match c with
	      | [] -> accu
	      | h::_t -> aux ff r (aux ff l (ff h accu))
	in fold f (t, 0) (aux f a h0)
end

module ZeroCorruption = struct
  let epsilon = max_float
end

module Binomial (X : Ordered) = Soft(X)(ZeroCorruption)
