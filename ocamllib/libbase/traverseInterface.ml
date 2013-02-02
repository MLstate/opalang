(*
    Copyright © 2011 MLstate

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
(**
   Generic Ast Rewriter Interfaces

   This module defines the interfaces of modules used in [Traverse].
   Since the module Traverse has a documented mli and there is a need
   to define severals module sig, putting them in this module dedicated
   for interfaces only, avoid the duplication of modules sig
   in [traverse.ml] and [traverse.mli]

   @author Louis Gesbert
   @author Valentin Gatien-Baron
   @author Mathieu Barbin
*)

(** The TRAVERSE main interface. *)
module type TRAVERSE =
sig
  (**
     The type of your trees, e.g. AST (expr). With up to 3 type variables for parametric trees.
     If you need more than 3 parameters, you can either go to hell, or patch this module (choose
     the less painful)
  *)
  type 'p t constraint 'p = _ * _ * _

  (**
     The type of elt which contain some [expr] inside. (like e.g. [code_elt])
     In the default TRAVERSE, [container = t], the type [expr] is recursive.
     When you use the [MakeLift] functor, you'll get [container <> t], with
     [container] a non recursive type containing some [t].
  *)
  type 'p container constraint 'p = _ * _ * _

  (** {6 Higher-order traverse functions} *)

  (**
     The function you specify controls the
     recursive calls by applying its argument. All the more simple functions
     below are implemented using those. Usually, you would use these functions
     somewhat like this:

     [ let f x = traverse_map (fun tra x -> ...) x ]

     where in the body of your function, you can do treatments on [x] and
     control the recursive calls by applying [tra]: [tra] corresponds to the
     recursive call on all children. Hence, you can apply a transformation and
     then descend on the result, or first descend and then transform, or choose
     not to descend in some cases, etc.

      You can also control, in the case of folds:
     - the information that goes up (it's that returned by your call to [tra])
     - the information that goes down (it's that passed to your call to [tra])
     - the information that goes to your brothers (it's your return value)
     - the information that comes from your brothers (it's the [acc] you write as parameter)

     Please see the implementation of the usual traverse functions below for
     examples.
  *)
  val traverse_iter : (('p t -> unit) -> 'p t -> unit) -> 'p container -> unit
  val traverse_map : (('p t -> 'p t) -> 'p t -> 'p t) -> 'p container -> 'p container
  val traverse_fold : (('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a) -> 'a -> 'p container -> 'a
  val traverse_foldmap : (('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t) -> 'a -> 'p container -> 'a * 'p container
  val traverse_exists : (('p t -> bool) -> 'p t -> bool) -> 'p container -> bool
  val traverse_forall : (('p t -> bool) -> 'p t -> bool) -> 'p container -> bool
  val traverse_findmap : (('p t -> 'a option) -> 'p t -> 'a option) -> 'p container -> 'a option

  val traverse_foldmap_context_down : (('env -> 'acc -> 'p t -> 'acc * 'p t) -> 'env -> 'acc -> 'p t -> 'acc * 'p t) -> 'env -> 'acc -> 'p container -> 'acc * 'p container
  val traverse_map_context_down : (('env -> 'p t -> 'p t) -> 'env -> 'p t -> 'p t) -> 'env -> 'p container -> 'p container
  val traverse_fold_context_down : (('env -> 'acc -> 'p t -> 'acc) -> 'env -> 'acc -> 'p t -> 'acc) -> 'env -> 'acc -> 'p container -> 'acc
  val traverse_iter_context_down : (('env -> 'p t -> unit) -> 'env -> 'p t -> unit) -> 'env -> 'p container -> unit
  val traverse_forall_context_down : (('env -> 'p t -> bool) -> 'env -> 'p t -> bool) -> 'env -> 'p container -> bool
  val traverse_exists_context_down : (('env -> 'p t -> bool) -> 'env -> 'p t -> bool) -> 'env -> 'p container -> bool

  (**
     These functions behave almost like their equivalent without "self_"
     Their are meant to replace patterns as:
     [let rec aux tra acc e = ... aux tra acc e ... in
      traverse_foldmap aux acc e]
     by
     [self_traverse_foldmap (fun self tra acc e -> ... self acc e ...)]
  *)
  val self_traverse_iter : (('p t -> unit) -> ('p t -> unit) -> 'p t -> unit) -> 'p container -> unit
  val self_traverse_map : (('p t -> 'p t) -> ('p t -> 'p t) -> 'p t -> 'p t) -> 'p container -> 'p container
  val self_traverse_fold : (('a -> 'p t -> 'a) -> ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a) -> 'a -> 'p container -> 'a
  val self_traverse_foldmap : (('a -> 'p t -> 'a * 'p t) -> ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t) -> 'a -> 'p container -> 'a * 'p container
  val self_traverse_exists : (('p t -> bool) -> ('p t -> bool) -> 'p t -> bool) -> 'p container -> bool
  val self_traverse_forall : (('p t -> bool) -> ('p t -> bool) -> 'p t -> bool) -> 'p container -> bool
  val self_traverse_findmap : (('p t -> 'a option) -> ('p t -> 'a option) -> 'p t -> 'a option) -> 'p container -> 'a option

  val self_traverse_foldmap_context_down : (('env -> 'acc -> 'p t -> 'acc * 'p t) -> ('env -> 'acc -> 'p t -> 'acc * 'p t) -> 'env -> 'acc -> 'p t -> 'acc * 'p t) -> 'env -> 'acc -> 'p container -> 'acc * 'p container
  val self_traverse_map_context_down : (('env -> 'p t -> 'p t) -> ('env -> 'p t -> 'p t) -> 'env -> 'p t -> 'p t) -> 'env -> 'p container -> 'p container
  val self_traverse_fold_context_down : (('env -> 'acc -> 'p t -> 'acc) -> ('env -> 'acc -> 'p t -> 'acc) -> 'env -> 'acc -> 'p t -> 'acc) -> 'env -> 'acc -> 'p container -> 'acc
  val self_traverse_iter_context_down : (('env -> 'p t -> unit) -> ('env -> 'p t -> unit) -> 'env -> 'p t -> unit) -> 'env -> 'p container -> unit
  val self_traverse_forall_context_down : (('env -> 'p t -> bool) -> ('env -> 'p t -> bool) -> 'env -> 'p t -> bool) -> 'env -> 'p container -> bool
  val self_traverse_exists_context_down : (('env -> 'p t -> bool) -> ('env -> 'p t -> bool) -> 'env -> 'p t -> bool) -> 'env -> 'p container -> bool

  (** {6 Usual traverse functions} *)
  (**
     Usual traverse functions. Up is bottom-up, down is top-down. Among
     brothers, the order is always that specified by the implementation of the module X

     Note : when up/down is not specified, order is not specified.
     Do not specify up/down only if it really does not matter to your pass.
  *)

  (** {9 iter} *)
  (** *)
  val iter : ('p t -> unit) -> 'p container -> unit
  val iter_up : ('p t -> unit) -> 'p container -> unit
  val iter_down : ('p t -> unit) -> 'p container -> unit

  (** {9 map} *)
  (** *)
  val map : ('p t -> 'p t) -> 'p container -> 'p container
  val map_up : ('p t -> 'p t) -> 'p container -> 'p container
  val map_down : ('p t -> 'p t) -> 'p container -> 'p container

  (** {9 fold} *)
  (** *)
  val fold : ('a -> 'p t -> 'a) -> 'a -> 'p container -> 'a
  val fold_up : ('a -> 'p t -> 'a) -> 'a -> 'p container -> 'a
  val fold_down : ('a -> 'p t -> 'a) -> 'a -> 'p container -> 'a

  (** {9 foldmap} *)
  (** *)
  val foldmap : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p container -> 'a * 'p container
  val foldmap_up : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p container -> 'a * 'p container
  val foldmap_down : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p container -> 'a * 'p container

  (** {9 exists} *)
  (** *)
  val exists : ('p t -> bool) -> 'p container -> bool
  val exists_up : ('p t -> bool) -> 'p container -> bool
  val exists_down : ('p t -> bool) -> 'p container -> bool

  (** {9 find} *)
  (** *)
  val find : ('p t -> bool) -> 'p container -> 'p t option
  val find_up : ('p t -> bool) -> 'p container -> 'p t option
  val find_down : ('p t -> bool) -> 'p container -> 'p t option

  (** {9 findmap} *)
  (** *)
  val findmap : ('p t -> 'a option) -> 'p container -> 'a option
  val findmap_up : ('p t -> 'a option) -> 'p container -> 'a option
  val findmap_down : ('p t -> 'a option) -> 'p container -> 'a option

  (** {9 iterating with a context}*)
  (** These functions allow you to keep some information about the context
      (for example in which toplevel declaration you are) *)
  (** *)
  val foldmap_context_down : ('env -> 'acc -> 'p t -> 'env * 'acc * 'p t) -> 'env -> 'acc -> 'p container -> 'acc * 'p container
  val fold_context_down : ('env -> 'acc -> 'p t -> 'env * 'acc) -> 'env -> 'acc -> 'p container -> 'acc
  val map_context_down : ('env -> 'p t -> 'env * 'p t) -> 'env -> 'p container -> 'p container
  val iter_context_down : ('env -> 'p t -> 'env) -> 'env -> 'p container -> unit


  (** {9 non rec}*)
  (**
     Non rec functions are the exportation in this module of the function
     taken as argument in the functor. ([X.iter], [X.map], [X.fold], [X.foldmap])

     It just apply the function to all child, but non recursivly, only to the direct
     children of the node.
  *)
  (** *)
  val iter_nonrec : ('p t -> unit) -> 'p container -> unit
  val map_nonrec : ('p t -> 'p t) -> 'p container -> 'p container
  val fold_nonrec : ('a -> 'p t -> 'a) -> 'a -> 'p container -> 'a
  val foldmap_nonrec : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p container -> 'a * 'p container
  val exists_nonrec : ('p t -> bool) -> 'p container -> bool
  val forall_nonrec : ('p t -> bool) -> 'p container -> bool
end

(** {6 Interfaces of Arguments for Traverse functors} *)

(** {9 A first version} *)
(**
   based on list cons/decons.
   After some bench, we'll see if this implementation will be keeped.
*)
module type S =
sig
  type 'p t constraint 'p = _ * _ * _
  val subs_cons : 'p t -> ('p t list -> 'p t) * 'p t list
end

(** {9 A second version} *)

(**
   Based on implementation of explicit traversal functions, without list manipulation.
   This implementation allow to be more specific about what should be reallocate,
   and permit some optimisations on traverse functions.

   After some bench to confirm this, we may keep only this version in a near future.
*)
module type S2 =
sig
  type 'p t constraint 'p = _ * _ * _

  (** the only function you need to define :
      given a function [tra] to foldmap all children of tree, foldmap the tree *)
  val foldmap : ('acc -> 'p t -> 'acc * 'p t) -> 'acc -> 'p t -> 'acc * 'p t

  (** other specific function *)
  (**
     Used for optimization purpose.
     If you dont want to write it for quick prototyping, use the constructor
     [Traverse.Unoptimized] on your foldmap function
  *)
  (** *)

  val iter : ('p t -> unit) -> 'p t -> unit
  val map : ('p t -> 'p t) -> 'p t -> 'p t
  val fold : ('acc -> 'p t -> 'acc) -> 'acc -> 'p t -> 'acc

  (** *)
  (** [exists], [find] and [findmap] are implemented from iter, using exceptions *)

end

(** {6 Lifting} *)
(**
   The lifting feature is used whenever you manipulate some bigger containers,
   which contains an arbitrary number of expressions
   that you want to traverse.

   It is almost the same interface than S2. If [container = t], you'll get S2.
   It is needed, since a functor in [Traverse] takes as argument a module of sig S2,
   and it is not possible to write something like :
   {[
   MakeLift ( LIFT2 with 'p t = 'p container )
   ]}

*)
module type LIFT2 =
sig
  type 'p t constraint 'p = _ * _ * _
  type 'p container constraint 'p = _ * _ * _

  val foldmap : ('acc -> 'p t -> 'acc * 'p t) -> 'acc -> 'p container -> 'acc * 'p container

  (** You can use [Traverse.Unoptimized] for quick prototyping.*)
  (** *)
  val iter : ('p t -> unit) -> 'p container -> unit
  val map : ('p t -> 'p t) -> 'p container -> 'p container
  val fold : ('acc -> 'p t -> 'acc) -> 'acc -> 'p container -> 'acc

end


(** {6 Traverse AB} *)

(**
   Traverse AB is a support for an ast defined with 2 mutual recursive types
   {[
   type 'a tA = phi('a tA, 'a tB)
   and 'b tB = xhi('a tA, 'a tB)
   ]}
   The rest is less documented, since it is more or less like the previous TRAVERSE,
   and mostly because it is used only by maniac geeks.
*)

module type TRAVERSE_12 =
sig

  type 'p t1 constraint 'p = _ * _ * _
  type 'p t2 constraint 'p = _ * _ * _

  (** {6 Higher-order traverse functions} *)
  (** *)
  val traverse_iter :
    (('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit) ->
    (('p t2 -> unit) -> ('p t1 -> unit) -> 'p t2 -> unit) ->
    'p t1 -> unit
  val traverse_map :
    (('p t1 -> 'p t1) -> ('p t2 -> 'p t2) -> 'p t1 -> 'p t1) ->
    (('p t2 -> 'p t2) -> ('p t1 -> 'p t1) -> 'p t2 -> 'p t2) ->
    'p t1 -> 'p t1
  val traverse_fold :
    (('acc -> 'p t1 -> 'acc) -> ('acc -> 'p t2 -> 'acc) -> 'acc -> 'p t1 -> 'acc) ->
    (('acc -> 'p t2 -> 'acc) -> ('acc -> 'p t1 -> 'acc) -> 'acc -> 'p t2 -> 'acc) ->
    'acc -> 'p t1 -> 'acc
  val traverse_foldmap :
    (('acc -> 'p t1 -> 'acc * 'p t1) -> ('acc -> 'p t2 -> 'acc * 'p t2) ->
       'acc -> 'p t1 -> 'acc * 'p t1) ->
    (('acc -> 'p t2 -> 'acc * 'p t2) -> ('acc -> 'p t1 -> 'acc * 'p t1) ->
       'acc -> 'p t2 -> 'acc * 'p t2) ->
    'acc -> 'p t1 -> 'acc * 'p t1
  val traverse_exists :
    (('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool) ->
    (('p t2 -> bool) -> ('p t1 -> bool) -> 'p t2 -> bool) ->
    'p t1 -> bool
  val traverse_forall :
    (('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool) ->
    (('p t2 -> bool) -> ('p t1 -> bool) -> 'p t2 -> bool) ->
    'p t1 -> bool
  val traverse_findmap :
    (('p t1 -> 'a option) -> ('p t2 -> 'a option) -> 'p t1 -> 'a option) ->
    (('p t2 -> 'a option) -> ('p t1 -> 'a option) -> 'p t2 -> 'a option) ->
    'p t1 -> 'a option

  val self_traverse_iter :
    (('p t1 -> unit) -> ('p t1 -> unit) -> ('p t2 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit) ->
    (('p t2 -> unit) -> ('p t2 -> unit) -> ('p t1 -> unit) -> ('p t1 -> unit) -> 'p t2 -> unit) ->
    'p t1 -> unit
  val self_traverse_map :
    (('p t1 -> 'p t1) -> ('p t1 -> 'p t1) -> ('p t2 -> 'p t2) -> ('p t2 -> 'p t2) -> 'p t1 -> 'p t1) ->
    (('p t2 -> 'p t2) -> ('p t2 -> 'p t2) -> ('p t1 -> 'p t1) -> ('p t1 -> 'p t1) -> 'p t2 -> 'p t2) ->
    'p t1 -> 'p t1
  val self_traverse_fold :
    (('acc -> 'p t1 -> 'acc) -> ('acc -> 'p t1 -> 'acc) -> ('acc -> 'p t2 -> 'acc) -> ('acc -> 'p t2 -> 'acc) -> 'acc -> 'p t1 -> 'acc) ->
    (('acc -> 'p t2 -> 'acc) -> ('acc -> 'p t2 -> 'acc) -> ('acc -> 'p t1 -> 'acc) -> ('acc -> 'p t1 -> 'acc) -> 'acc -> 'p t2 -> 'acc) ->
    'acc -> 'p t1 -> 'acc
  val self_traverse_foldmap :
    (('acc -> 'p t1 -> 'acc * 'p t1) -> ('acc -> 'p t1 -> 'acc * 'p t1) -> ('acc -> 'p t2 -> 'acc * 'p t2) -> ('acc -> 'p t2 -> 'acc * 'p t2) -> 'acc -> 'p t1 -> 'acc * 'p t1) ->
    (('acc -> 'p t2 -> 'acc * 'p t2) -> ('acc -> 'p t2 -> 'acc * 'p t2) -> ('acc -> 'p t1 -> 'acc * 'p t1) -> ('acc -> 'p t1 -> 'acc * 'p t1) -> 'acc -> 'p t2 -> 'acc * 'p t2) ->
    'acc -> 'p t1 -> 'acc * 'p t1
  val self_traverse_exists :
    (('p t1 -> bool) -> ('p t1 -> bool) -> ('p t2 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool) ->
    (('p t2 -> bool) -> ('p t2 -> bool) -> ('p t1 -> bool) -> ('p t1 -> bool) -> 'p t2 -> bool) ->
    'p t1 -> bool
  val self_traverse_forall :
    (('p t1 -> bool) -> ('p t1 -> bool) -> ('p t2 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool) ->
    (('p t2 -> bool) -> ('p t2 -> bool) -> ('p t1 -> bool) -> ('p t1 -> bool) -> 'p t2 -> bool) ->
    'p t1 -> bool
  val self_traverse_findmap :
    (('p t1 -> 'a option) -> ('p t1 -> 'a option) -> ('p t2 -> 'a option) -> ('p t2 -> 'a option) -> 'p t1 -> 'a option) ->
    (('p t2 -> 'a option) -> ('p t2 -> 'a option) -> ('p t1 -> 'a option) -> ('p t1 -> 'a option) -> 'p t2 -> 'a option) ->
    'p t1 -> 'a option

  (** {6 Usual traverse functions} *)

  (** {9 iter} *)
  (** *)
  val iter : ('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit
  val iter_up : ('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit
  val iter_down : ('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit

  (** {9 map} *)
  (** *)
  val map : ('p t1 -> 'p t1) -> ('p t2 -> 'p t2) -> 'p t1 -> 'p t1
  val map_up : ('p t1 -> 'p t1) -> ('p t2 -> 'p t2) -> 'p t1 -> 'p t1
  val map_down : ('p t1 -> 'p t1) -> ('p t2 -> 'p t2) -> 'p t1 -> 'p t1

  (** {9 fold} *)
  (** *)
  val fold : ('a -> 'p t1 -> 'a) -> ('a -> 'p t2 -> 'a) -> 'a -> 'p t1 -> 'a
  val fold_up : ('a -> 'p t1 -> 'a) -> ('a -> 'p t2 -> 'a) -> 'a -> 'p t1 -> 'a
  val fold_down : ('a -> 'p t1 -> 'a) -> ('a -> 'p t2 -> 'a) -> 'a -> 'p t1 -> 'a

  (** {9 foldmap} *)
  (** *)
  val foldmap : ('a -> 'p t1 -> 'a * 'p t1) -> ('a -> 'p t2 -> 'a * 'p t2) -> 'a -> 'p t1 -> 'a * 'p t1
  val foldmap_up : ('a -> 'p t1 -> 'a * 'p t1) -> ('a -> 'p t2 -> 'a * 'p t2) -> 'a -> 'p t1 -> 'a * 'p t1
  val foldmap_down : ('a -> 'p t1 -> 'a * 'p t1) -> ('a -> 'p t2 -> 'a * 'p t2) -> 'a -> 'p t1 -> 'a * 'p t1

  (** {9 exists} *)
  (** *)
  val exists : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool
  val exists_up : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool
  val exists_down : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool

  (** {9 find} *)
  (** *)
  val find : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> ('p t1, 'p t2) Base.either option
  val find_up : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> ('p t1, 'p t2) Base.either option
  val find_down : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> ('p t1, 'p t2) Base.either option

  (** {9 findmap} *)
  (** *)
  val findmap : ('p t1 -> 'a option) -> ('p t2 -> 'a option) -> 'p t1 -> 'a option
  val findmap_up : ('p t1 -> 'a option) -> ('p t2 -> 'a option) -> 'p t1 -> 'a option
  val findmap_down : ('p t1 -> 'a option) -> ('p t2 -> 'a option) -> 'p t1 -> 'a option

  val traverse_foldmap_context_down :
    (('env -> 'acc -> 'p t1 -> 'acc * 'p t1) -> ('env -> 'acc -> 'p t2 -> 'acc * 'p t2) ->
         'env -> 'acc -> 'p t1 -> 'acc * 'p t1) ->
    (('env -> 'acc -> 'p t2 -> 'acc * 'p t2) -> ('env -> 'acc -> 'p t1 -> 'acc * 'p t1) ->
       'env -> 'acc -> 'p t2 -> 'acc * 'p t2) ->
    'env -> 'acc -> 'p t1 -> 'acc * 'p t1
  val traverse_map_context_down :
    (('env -> 'p t1 -> 'p t1) -> ('env -> 'p t2 -> 'p t2) ->
         'env -> 'p t1 -> 'p t1) ->
    (('env -> 'p t2 -> 'p t2) -> ('env -> 'p t1 -> 'p t1) ->
       'env -> 'p t2 -> 'p t2) ->
    'env -> 'p t1 -> 'p t1
  val traverse_fold_context_down :
    (('env -> 'acc -> 'p t1 -> 'acc) -> ('env -> 'acc -> 'p t2 -> 'acc) ->
         'env -> 'acc -> 'p t1 -> 'acc) ->
    (('env -> 'acc -> 'p t2 -> 'acc) -> ('env -> 'acc -> 'p t1 -> 'acc) ->
       'env -> 'acc -> 'p t2 -> 'acc) ->
    'env -> 'acc -> 'p t1 -> 'acc
  val traverse_iter_context_down :
    (('env -> 'p t1 -> unit) -> ('env -> 'p t2 -> unit) ->
         'env -> 'p t1 -> unit) ->
    (('env -> 'p t2 -> unit) -> ('env -> 'p t1 -> unit) ->
       'env -> 'p t2 -> unit) ->
    'env -> 'p t1 -> unit
  val traverse_forall_context_down :
    (('env -> 'p t1 -> bool) -> ('env -> 'p t2 -> bool) ->
         'env -> 'p t1 -> bool) ->
    (('env -> 'p t2 -> bool) -> ('env -> 'p t1 -> bool) ->
       'env -> 'p t2 -> bool) ->
    'env -> 'p t1 -> bool
  val traverse_exists_context_down :
    (('env -> 'p t1 -> bool) -> ('env -> 'p t2 -> bool) ->
         'env -> 'p t1 -> bool) ->
    (('env -> 'p t2 -> bool) -> ('env -> 'p t1 -> bool) ->
       'env -> 'p t2 -> bool) ->
    'env -> 'p t1 -> bool

  val self_traverse_foldmap_context_down :
    (('env -> 'acc -> 'p t1 -> 'acc * 'p t1) -> ('env -> 'acc -> 'p t1 -> 'acc * 'p t1) -> ('env -> 'acc -> 'p t2 -> 'acc * 'p t2) -> ('env -> 'acc -> 'p t2 -> 'acc * 'p t2) -> 'env -> 'acc -> 'p t1 -> 'acc * 'p t1) ->
    (('env -> 'acc -> 'p t2 -> 'acc * 'p t2) -> ('env -> 'acc -> 'p t2 -> 'acc * 'p t2) -> ('env -> 'acc -> 'p t1 -> 'acc * 'p t1) -> ('env -> 'acc -> 'p t1 -> 'acc * 'p t1) -> 'env -> 'acc -> 'p t2 -> 'acc * 'p t2) ->
    'env -> 'acc -> 'p t1 -> 'acc * 'p t1
  val self_traverse_map_context_down :
    (('env -> 'p t1 -> 'p t1) -> ('env -> 'p t1 -> 'p t1) -> ('env -> 'p t2 -> 'p t2) -> ('env -> 'p t2 -> 'p t2) -> 'env -> 'p t1 -> 'p t1) ->
    (('env -> 'p t2 -> 'p t2) -> ('env -> 'p t2 -> 'p t2) -> ('env -> 'p t1 -> 'p t1) -> ('env -> 'p t1 -> 'p t1) -> 'env -> 'p t2 -> 'p t2) ->
    'env -> 'p t1 -> 'p t1
  val self_traverse_fold_context_down :
    (('env -> 'acc -> 'p t1 -> 'acc) -> ('env -> 'acc -> 'p t1 -> 'acc) -> ('env -> 'acc -> 'p t2 -> 'acc) -> ('env -> 'acc -> 'p t2 -> 'acc) -> 'env -> 'acc -> 'p t1 -> 'acc) ->
    (('env -> 'acc -> 'p t2 -> 'acc) -> ('env -> 'acc -> 'p t2 -> 'acc) -> ('env -> 'acc -> 'p t1 -> 'acc) -> ('env -> 'acc -> 'p t1 -> 'acc) -> 'env -> 'acc -> 'p t2 -> 'acc) ->
    'env -> 'acc -> 'p t1 -> 'acc
  val self_traverse_iter_context_down :
    (('env -> 'p t1 -> unit) -> ('env -> 'p t1 -> unit) -> ('env -> 'p t2 -> unit) -> ('env -> 'p t2 -> unit) -> 'env -> 'p t1 -> unit) ->
    (('env -> 'p t2 -> unit) -> ('env -> 'p t2 -> unit) -> ('env -> 'p t1 -> unit) -> ('env -> 'p t1 -> unit) -> 'env -> 'p t2 -> unit) ->
    'env -> 'p t1 -> unit
  val self_traverse_forall_context_down :
    (('env -> 'p t1 -> bool) -> ('env -> 'p t1 -> bool) -> ('env -> 'p t2 -> bool) -> ('env -> 'p t2 -> bool) -> 'env -> 'p t1 -> bool) ->
    (('env -> 'p t2 -> bool) -> ('env -> 'p t2 -> bool) -> ('env -> 'p t1 -> bool) -> ('env -> 'p t1 -> bool) -> 'env -> 'p t2 -> bool) ->
    'env -> 'p t1 -> bool
  val self_traverse_exists_context_down :
    (('env -> 'p t1 -> bool) -> ('env -> 'p t1 -> bool) -> ('env -> 'p t2 -> bool) -> ('env -> 'p t2 -> bool) -> 'env -> 'p t1 -> bool) ->
    (('env -> 'p t2 -> bool) -> ('env -> 'p t2 -> bool) -> ('env -> 'p t1 -> bool) -> ('env -> 'p t1 -> bool) -> 'env -> 'p t2 -> bool) ->
    'env -> 'p t1 -> bool

  (** {9 iterating with a context}*)
  (** *)
  val foldmap_context_down :
    ('env -> 'acc -> 'p t1 -> 'env * 'acc * 'p t1) ->
    ('env -> 'acc -> 'p t2 -> 'env * 'acc * 'p t2) ->
    'env -> 'acc -> 'p t1 -> 'acc * 'p t1
  val map_context_down :
    ('env -> 'p t1 -> 'env * 'p t1) ->
    ('env -> 'p t2 -> 'env * 'p t2) ->
    'env -> 'p t1 -> 'p t1
  val iter_context_down :
    ('env -> 'p t1 -> 'env) ->
    ('env -> 'p t2 -> 'env) ->
    'env -> 'p t1 -> unit

  (** {9 non rec}*)
  (** *)
  val iter_nonrec : ('p t1 -> unit) -> ('p t2 -> unit) -> 'p t1 -> unit
  val map_nonrec : ('p t1 -> 'p t1) -> ('p t2 -> 'p t2) -> 'p t1 -> 'p t1
  val fold_nonrec : ('a -> 'p t1 -> 'a) -> ('a -> 'p t2 -> 'a) -> 'a -> 'p t1 -> 'a
  val foldmap_nonrec : ('a -> 'p t1 -> 'a * 'p t1) -> ('a -> 'p t2 -> 'a * 'p t2) -> 'a -> 'p t1 -> 'a * 'p t1
  val exists_nonrec : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool
  val forall_nonrec : ('p t1 -> bool) -> ('p t2 -> bool) -> 'p t1 -> bool
end

(** {6 Interfaces of Arguments for Traverse functors} *)
module type AB =
sig
  type 'p tA constraint 'p = _ * _ * _
  type 'p tB constraint 'p = _ * _ * _

  val foldmapA :
    ('acc -> 'p tA -> 'acc * 'p tA) ->
    ('acc -> 'p tB -> 'acc * 'p tB) ->
    'acc -> 'p tA -> 'acc * 'p tA

  val foldmapB :
    ('acc -> 'p tB -> 'acc * 'p tB) ->
    ('acc -> 'p tA -> 'acc * 'p tA) ->
    'acc -> 'p tB -> 'acc * 'p tB

  (** other specific function *)
  (**
     Used for optimization purpose.
     If you dont want to write it for quick prototyping, use the constructor
     [Traverse.Unoptimized] on your [foldmapA/B] function
  *)
  (** *)

  val iterA : ('p tA -> unit) -> ('p tB -> unit) -> 'p tA -> unit
  val iterB : ('p tB -> unit) -> ('p tA -> unit) -> 'p tB -> unit

  val mapA : ('p tA -> 'p tA) -> ('p tB -> 'p tB) -> 'p tA -> 'p tA
  val mapB : ('p tB -> 'p tB) -> ('p tA -> 'p tA) -> 'p tB -> 'p tB

  val foldA : ('acc -> 'p tA -> 'acc) -> ('acc -> 'p tB -> 'acc) -> 'acc -> 'p tA -> 'acc
  val foldB : ('acc -> 'p tB -> 'acc) -> ('acc -> 'p tA -> 'acc) -> 'acc -> 'p tB -> 'acc
end

module type TRAVERSE_AB =
sig
  type 'p tA constraint 'p = _ * _ * _
  type 'p tB constraint 'p = _ * _ * _

  module A : TRAVERSE_12
    with type 'p t1 = 'p tA
    and type 'p t2 = 'p tB

  module B : TRAVERSE_12
    with type 'p t1 = 'p tB
    and type 'p t2 = 'p tA

  module AinB : TRAVERSE
    with type 'p t = 'p tA
    and type 'p container = 'p tB

  module BinA : TRAVERSE
    with type 'p t = 'p tB
    and type 'p container = 'p tA

  module AinA : TRAVERSE
    with type 'p t = 'p tA
    and type 'p container = 'p tA

  module BinB : TRAVERSE
    with type 'p t = 'p tB
    and type 'p container = 'p tB

  module OnlyA : TRAVERSE
    with type 'p t = 'p tA
    and type 'p container = 'p tA

  module OnlyB : TRAVERSE
    with type 'p t = 'p tB
    and type 'p container = 'p tB
end

(** {6 Lifting} *)
(**
   Are you serious ?
*)
