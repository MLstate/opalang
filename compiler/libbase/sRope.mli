(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   Functionnal structure for efficient string production.

   This module offers you an alternative to [FBuffer] implementing the same
   interface, whenever you need to have a constant complexity for the concat
   operation.

   With [FBuffer], this module is for producing big strings in a functionnal way
   with view allocations, e.g. for code generation.

   The function [add] is not so fast as [FBfufer.add] when it is used with small
   strings.

   <!> See the function [diverge], the implementation is not fully functionnal.
   Previous values of [t] are persistent but if you diverge from a point
   of your functionnal historic, you should absolutelly use [diverge] to
   avoid to compromise an other [t] value.

   Note that every function of this Module is tail recursive.

   <!> If you need to add a function to this module, respect this constraint.
   (this module manipulate hudge structures)

   @author Mathieu Barbin
   @author Rudy Sicard
   @author Valentin Gatien-Baron
*)

(** The abstract type of a SRope. Almost Functionnal (see [diverge] function) *)
type t

(** The empty value. The int is just a hint for the first memory allocation. *)
val create : int -> t

(** {6 Basic operations} *)

(** *)
val add : t -> string -> t

(** like [add] but add a new line at end *)
val addln : t -> string -> t

(** [concat a b] does not affect [a] or [b] (functionnal) *)
val concat : t -> t -> t

(** [add_substring t s start len] append to [t] the substring of s
    startgin from [start] from length [len] *)
val add_substring : t -> string -> int -> int -> t

(** diverge is used whenever you need to start from a point of your history
    and to diverge. (implementation is not fully functionnal, but by using
    this function, you are sure not to erase anything in memory *)
val diverge : t -> t

(** {6 Accessing Contents} *)
(** *)
val contents : t -> string
val output : out_channel -> t -> unit
val length : t -> int
val sub : t -> int -> int -> string

(** {6 Interactive accessors} *)

(**
   Interactive accessors are called with a string [s],
   a start position [p], and a number of characters [n];
   it is supposed to output characters [p] to [p + n - 1] of [s]

   If for any reason you need to interactivelly access sub string,
   you can use function [iter_sub] and [fold_sub] applied to
   a function [f] of your choice, which is stricly equivalent
   to
   {[iter (fun s start len -> f (String.sub s start len))]}
   and respectively
   {[fold (fun acc s start len -> f acc (String.sub s start len))]}
   (one more string allocation with [String.sub])

   [iter], [fold], [rev_iter], [rev_fold] are equivalent in term of performance,
   all are tail rec.

   @see "Format" of the ocaml standard library
   @see "Pervasives" of the ocaml standard library
*)

(** *)
val iter : (string -> int -> int -> unit) -> t -> unit
val fold : ('a -> string -> int -> int -> 'a) -> 'a -> t -> 'a

val rev_iter : (string -> int -> int -> unit) -> t -> unit
val rev_fold : ('a -> string -> int -> int -> 'a) -> 'a -> t -> 'a

val iter_sub : (string -> unit) -> t -> unit
val fold_sub : ('a -> string -> 'a) -> 'a -> t -> 'a

(** {6 Format Support} *)

(**
   For outputting a type [t] using format, you can use :
   [Format.printf "%a" SRope.fmt t]
   it will be more optimized than
   [Format.printf "%s" (SRope.contents t)]
 *)
val fmt : Format.formatter -> t -> unit

(**
   [SRope.printf t "I add %s" "this"]

   Using the two function combined will give you a quite powerfull
   concatenation possibility :

   if [a], [b], [c] be [3] value of type [SRope.t],
   you can do things like :
   [SRope.printf t "%a foo %a bar %a" fmt a fmt b fmt c]
*)
val printf : t -> ('params, Format.formatter, unit, t) format4 -> 'params

(** {6 Dynamic choice of implementation} *)

(**
   You may want to use this structure for having dynamic choice of implementation.
   Modules are not value in caml, this is a defun feature.

   @see "FBuffer.fbuffer" This is a record version of the module.
*)

val implementation : t FBuffer.implementation

(** {6 Backward Compatibility} *)

(**
   For historical reason, some allias may result in old code.
   Please do not use in new code.
*)

(** @deprecated use [create] instead *)
val make : ?name:string -> int -> t

(** @deprecated use [concat] instead *)
val union : t -> t -> t

(** @deprecated use [output] instead *)
val write : t -> out_channel -> unit
