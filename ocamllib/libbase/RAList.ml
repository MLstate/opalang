(* see mli *)
type comparison = Lesser | Equal | Greater
exception Invalid_subscript of (int*int)
exception Empty
exception StopFold (*to implement fold until *)

(** the asked value is lesser than all entries *)
exception Not_found_lesser
(** the asked value is greater than all entries *)
exception Not_found_greater

(** Random Access List
    Happily copied from Purely Functional Random-Access Lists from Okasaki
    Add lookup for monotonuous list *)

(** CompleteBinaryTree ********************************************************************************************************)
module Tree :
sig
  type 'a tree
  type 'a treeview = Leaf of 'a | Node of 'a * 'a tree * 'a tree (* Duplication, NAAAAAAAAAAAAAAAAAAAAAAAAAAAAAN ! *)
  val view : 'a tree -> 'a treeview (* get a possibly simplified view *)
  val node : 'a -> 'a tree -> 'a tree -> 'a tree
  val leaf : 'a -> 'a tree
  val root : 'a tree -> 'a
  val get  : int(*size*) -> int(*pos*) -> 'a tree -> 'a
  val update : int(*size*) -> int(*pos*) -> 'a tree -> 'a -> 'a tree
  val generic_get : accept_lesser:bool -> compared_to:('a->comparison) -> 'a tree -> 'a
  val fold : ('a->'acc->'acc) -> 'a tree -> 'acc -> 'acc
  val rev_fold : ('a->'acc->'acc) -> 'a tree -> 'acc -> 'acc
end =
struct
(** public view *)
type 'a tree = 'a treeview
and  'a treeview = Leaf of 'a | Node of 'a * 'a tree * 'a tree
let view (x:'a tree) = (x:'a treeview) (* to abstract true tree representation *)

let node x l r =  Node( x,l,r)
let leaf x = Leaf x
let root tree =
  match tree with
  | Leaf x | Node(x,_,_) -> x

let rec get size i tree =
  match tree with
  | Node(x,l,r) ->
    if i==0 then x
    else
      let size = size / 2 in
      if i <= size
      then get size (i-1)      l
      else get size (i-1-size) r
  | Leaf(x) ->
    if i==0 then x
    else raise (Invalid_subscript(i,size))

let rec update size i tree x =
  match tree with
  | Node (o,l,r) ->
    if i==0 then Node (x,l,r) 
    else
      let size = size / 2 in
      if i <= size
      then Node (o, update size (i-1) l x, r)
      else Node (o, l, update size (i-1-size) r x)
  | Leaf _ ->
    if i==0 then Leaf x
    else raise (Invalid_subscript(i,size))


(**
   return the first entry that is equal according to comparison function (or lesser then if accept_lesser is true)
   assuming all entry have been consed in increasing order wrt to the comparison function
   raise Not_found_greater , asked is greater than all entry (when accept_lesser=false)
   raise Not_found_lesser  , asked is lesser  than all entry
*)
(* recursively search right subtree first than left subtree if needed (in this case first search is O(1))
   if searching a value bigger than all tree value => returning it in O(1) *)
let rec generic_get ~accept_lesser ~compared_to tree =
  let x = root tree in
  match (*asked*) compared_to x with
  | Greater when accept_lesser -> x
  | Greater -> raise Not_found_greater
  | Equal -> x
  | Lesser ->
    match tree with
    | Leaf _ -> raise Not_found_lesser
    | Node(_,l,r) ->
      try generic_get ~accept_lesser:false ~compared_to r with (* on root of r, will raise Not_found_greater in O(1) if l must be searched first *)
(*NOP | Not_found_lesser -> raise Not_found_lesser *)
      | Not_found_greater (* asked is greater than all r *)->
        try generic_get ~accept_lesser ~compared_to l with
        | Not_found_lesser (* asked is lesser than all l *) -> root r
(*NOP   | Not_found_greater -> raise Not_found_greater *)




let (|>) a f = f a

let rec fold f t acc =
  match t with
 | Leaf x ->
   (try
     f x acc
    with StopFold -> acc)
 | Node(x,l,r) ->
   (try
      f x acc |> fold f l |> fold f r
    with StopFold -> acc)

let rec rev_fold f t acc =
  match t with
 | Leaf x ->
   (try
     f x acc
    with StopFold -> acc)
 | Node(x,l,r) ->
   (try
      fold f r acc |> fold f l |> f x
    with StopFold -> acc)


end

(** RA list types *)
type 'a tree = { size : int ; tree : 'a Tree.tree} (* should be size1 size3 sizeN *)
type 'a ra_list = 'a tree list

(** List interface ********************************************************************************************************)
module AsList = struct
exception StopFold =StopFold
let empty = ([] : 'a ra_list)
let is_empty l = l=empty

 let cons x l =
  match l with
  | {size=s1;tree=t1} :: {size=s2;tree=t2} :: rest when s1=s2->
    {size=1+s1+s2;tree=Tree.node x t1 t2 } :: rest
  | _ ->
    {size=1;tree=Tree.leaf x} :: l

let head l =
  match l with
  | {size=_;tree=t} :: _ -> Tree.root t
  | [] -> raise Empty

let tail l =
  match l with
  | {size=1;tree=_}::rest -> rest
  | {size=s;tree=t}::rest ->
    let s=s/2 in
    begin match Tree.view t with
        | Tree.Node(_,t1,t2) -> {size=s;tree=t1} :: {size=s;tree=t2} :: rest
        | _ -> assert false
    end
  | [] -> raise Empty

let rec fold f (l:'a ra_list) acc  =
  match l with
  | [] -> acc
  | {size=_;tree=t}::rl ->
    try fold f rl (Tree.fold f t acc)
    with StopFold -> acc

let rev_fold f l acc =
  let rec aux l acc =
    match l with
    | [] -> acc
    | {size=_; tree=t}::rl ->
    try aux rl (Tree.rev_fold f t acc)
    with StopFold -> acc
  in aux (List.rev l) acc

end

(** Array interface ********************************************************************************************************)
module AsArray = struct

let size l = List.fold_left (fun acc {size=s;tree=_} -> acc+s) 0 l

let rec raw_get l i =
  match l with
  | [] -> raise (Invalid_subscript(i,0))
  | {size=s;tree=t}::rest ->
    if i < s
    then Tree.get s i t
    else raw_get rest (i-s)

let get l i =
  try raw_get l i
  with Invalid_subscript _  -> raise (Invalid_subscript(i,size l))

let rec raw_update l i v =
  match l with
  | [] -> raise (Invalid_subscript(i,0))
  | ({size=s;tree=t} as e)::rest ->
    if i < s
    then {e with tree=(Tree.update s i t v)}::rest
    else e::raw_update rest (i-s) v

let update l i v =
  try raw_update l i v
  with Invalid_subscript _  -> raise (Invalid_subscript(i,size l))

end

module AsMonotoniousList = struct

  let rec get_lesser compared_to l =
    match l with
    | [] -> raise Not_found
    | {size=_;tree=t}::rest ->
      try Tree.generic_get ~accept_lesser:true ~compared_to t
      with
      | Not_found_lesser  -> get_lesser compared_to rest
      | Not_found_greater -> raise Not_found

end


