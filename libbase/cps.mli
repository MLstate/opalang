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

(** This module contains functions and types that are helpful when programming
    in Continuation Passing Style. Included are the Duck-Style-Cps© guidelines,
    for easier and more readable CPS code. *)

(** This type is convenient for more readable cps types:
    for example, [List.map] has type [('a -> 'b) -> 'a list -> 'b list],
    which in CPS becomes
    [('a -> ('b -> unit) -> unit) -> 'a list -> ('b list -> unit) -> unit]
    Using [Cps.t], one can write it:
    [('a -> 'b Cps.t) -> 'a list -> 'b list Cps.t]
*)
type 'a t = ('a -> unit) -> unit

(** Open this module for duck-style cps-programming©.

    The style is based on two operators, the Duck [@>] for "application of function
    to continuation" ("apply"), and the Pipe [|>] for "application of continuation
    to value" ("return"). It has the nice duality that [f x |> k] and [f x @> k] are
    to equivalent expressions for [f] resp. in non-cps and in cps. This makes style
    consistent even though we always use a mixed cps/non-cps style.

    The convention is as follows:
    - These operators should only be used for continuation application
    - Application of a function to its continuation must be written [@>]
    - Application of a continuation to a value (eg [return]) must be written with [|>]
    - when your code is multiline, always put line breaks just before [@>]. This
      gives a very readable indentation. Put them after [fun ... ->] as well if
      it's not enough.
    - [@>] is associative right, with a priority lower than function
      application, which allows to remove a {b lot} of parentheses. It also allows
      to write chains of operations without indentation: {[
      let f x k =
        cps_function_one x
        @> (fun k y -> cps_function_two y @> k)
        @> (fun k z -> cps_function_three z @> k)
        @> k
      ]}
      which is equivalent to: {[
      let f x =
        let y = noncps_function_one x in
        let z = noncps_function_two y in
        let a = noncps_function_three z in
        a
      ]}

    Remark: a specialised module exists in QmlCpsServerLib for the manipulation of
    run-time continuations (by opposition to ocaml functions).

    Remark: by convention, all CPS functions take their continuation as last
    parameter (except for the inline functions for composition, as in the example
    above). When you write a function that {b actually} works on the continuation
    (eg. does some transformation to it), take it as first parameter, so that there
    is no ambiguity.
*)
module Ops : sig
  val (|>): 'a -> ('a -> unit) -> unit (** More interestingly written ['a -> 'a t] *)
  val (@>): 'a t -> ('a -> unit) -> unit (** More interestingly written ['a t -> 'a t] :D *)
end

(** A few of the most common functions on lists, in CPS *)
module List : sig
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  (** While coding a cps-fold based on List.fold is a fun exercise, it's less efficient. *)
  val fold : ('acc -> 'a -> 'acc t) -> 'acc -> 'a list -> 'acc t
end

module Option : sig
  val map : ('a -> 'b t) -> 'a option -> 'b option t
end

module Lazy : sig
  (** A lazy cps value will be evaluted once time by the first
      [eval_lazy]. *)
  type 'a t

  (** [make_lazy push cps] Create an ['a] lazy [cps] value will be
      evaluated only once at the first call of [eval_lazy t k]. *)
  val make : ((unit -> unit) -> unit) -> (('a -> unit) -> unit) -> 'a t

  (** [force lazy_cps k] Eval [lazy_cps] value and call [k]
      continuation with ['a] computed value. *)
  val force : 'a t -> ('a -> unit) -> unit

  (** Access to the cps lazy state. Returns [None] if the cps lazy value
      is not evaluated, else return the evaluated value.*)
  val get_state : 'a t -> 'a option

  (** As Ocaml's [Lazy.lazy_from_val]: returns an already-forced suspension *)
  val lazy_from_val : 'a -> 'a t
end
