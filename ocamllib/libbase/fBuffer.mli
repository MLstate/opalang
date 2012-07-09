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
   Functionnal structure for efficient string production.

   @author Unknown previous author
   @author Mathieu Barbin
   @author Rudy Sicard
   @author Valentin Gatien-Baron
*)

(**
   This module is for producing big strings in a functionnal way
   with view allocations, e.g. for code generation.

   If you need to have a better complexity for the [concat] operation,
   you can use [SRope] instead, which implement the same interface, but
   with a [concat] in constant time. However, if you need to use the [FBuffer.add]
   function with small strings, [FBuffer] is quicker.

   <!> See the function [diverge], the implementation is not fully functionnal.
   Previous values of [t] are persistent but if you diverge from a point
   of your functionnal historic, you should absolutelly use [diverge] to
   avoid to compromise an other [t] value.

   Note that every function of this Module is tail recursive.

   <!> If you need to add a function to this module, respect this constraint.
   (this module manipulate hudge structures)

*)

(** The abstract type of a FBuffer. Almost Functionnal (see [diverge] function) *)
type t

(** The empty value. The int is just a hint for the first memory allocation.
    The name is ignored and will be removed soon. not used, not implemented,
    weird. (if you create a Stack, do you need to name it ??)
*)
val create : ?name:string -> int -> t

(** {6 Basic operations} *)

(** [add] operations on not too big strings is effecienter
    using [FBuffer] rather than [SRope] *)
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
   it is supposed to output characters [p] to [p + n - 1] of [s].

   The split in several strings depends on the internal representation.

   If for any reason you need to interactivelly access sub string,
   you can use function [iter_sub] and [fold_sub] applied to
   a function [f] of your choice, which is stricly equivalent
   to
   {[iter (fun s start len -> f (String.sub s start len))]}
   and respectively
   {[fold (fun acc s start len -> f acc (String.sub s start len))]}
   (one more string allocation with [String.sub])

   [rev_iter] and [rev_fold] have better performance than [iter] and [fold].
   They are all tail rec.

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
   [Format.printf "%a" FBuffer.fmt t]
   it will be more optimized than
   [Format.printf "%s" (FBuffer.contents t)]
 *)
val fmt : Format.formatter -> t -> unit

(**
   [FBuffer.printf t "I add %s" "this"]

   Using the two function combined will give you a quite powerfull
   concatenation possibility :

   if [a], [b], [c] be [3] value of type [FBuffer.t],
   you can do things like :
   [FBuffer.printf t "%a foo %a bar %a" fmt a fmt b fmt c]
*)
val printf : t -> ('params, Format.formatter, unit, t) format4 -> 'params

(**
   Very usefull to replace [Format.sprintf] using a FBuffer implementation.
*)
val sprintf : ('params, Format.formatter, unit, string) format4 -> 'params

(** {6 Dynamic choice of implementation} *)

(**
   You may want to use this structure for having dynamic choice of implementation.
   Modules are not value in caml, this is a defun feature.
*)
type 'fbuffer implementation =
    {
      create : int -> 'fbuffer ;
      add : 'fbuffer -> string -> 'fbuffer ;
      addln : 'fbuffer -> string -> 'fbuffer ;
      concat : 'fbuffer -> 'fbuffer -> 'fbuffer ;
      add_substring : 'fbuffer -> string -> int -> int -> 'fbuffer ;
      diverge : 'fbuffer -> 'fbuffer ;
      contents : 'fbuffer -> string ;
      output : out_channel -> 'fbuffer -> unit ;
      length : 'fbuffer -> int ;
      sub : 'fbuffer -> int -> int -> string ;
      iter : (string -> int -> int -> unit) -> 'fbuffer -> unit ;
      fold : 'a. ('a -> string -> int -> int -> 'a) -> 'a -> 'fbuffer -> 'a ;
      rev_iter : (string -> int -> int -> unit) -> 'fbuffer -> unit ;
      rev_fold : 'a. ('a -> string -> int -> int -> 'a) -> 'a -> 'fbuffer -> 'a ;
      iter_sub : (string -> unit) -> 'fbuffer -> unit ;
      fold_sub : 'a. ('a -> string -> 'a) -> 'a -> 'fbuffer -> 'a ;
      fmt : Format.formatter -> 'fbuffer -> unit ;
      printf : 'params. 'fbuffer -> ('params, Format.formatter, unit, 'fbuffer) format4 -> 'params ;
      sprintf : 'params. ('params, Format.formatter, unit, string) format4 -> 'params ;
    }

val implementation : t implementation

(** {6 Backward Compatibility} *)

(**
   For historical reason, some allias may result in old code.
   Please do not use in new code.
*)

(** @deprecated use [union] instead *)
val make : ?name:string -> int -> t

(** @deprecated use [union] instead *)
val union : t -> t -> t

(** @deprecated use [output] instead *)
val write : t -> out_channel -> unit
