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
   Some utils for users of Ocaml lang.

   @author Mathieu Barbin
   @author Esther Baruk
*)

(**
   Nary support for applications
*)
module App :
sig
  (**
     Gather the arguments of an apply. Returns f * args
  *)
  val nary_app : Ocaml.expr -> Ocaml.expr -> Ocaml.expr * Ocaml.expr list

  (**
     Reconstruct an apply node from f * args
  *)
  val app : Ocaml.expr -> Ocaml.expr list -> Ocaml.expr
end

(**
    Operations on arrays
*)
module Array : sig
  (** Avoid an array construction by calling Array.make and filling the array *)
  val relax_make : Ocaml.expr -> Ocaml.expr
end

(** {6 Type alias} *)
(** *)
type filename = string

(** {6 Paths in modules} *)

(**
   A ocaml module name.
   This name is capitalized, like [String, List]
*)
type module_name = string

module Module :
sig

  (**
     The path to a module
  *)
  type path = module_name list


  val of_filename : filename -> module_name

  (**
     For generating code containing field from records,
     when you do not want to have conflicting open,
     it is better to prefix the field by their global
     module path.

     {[
     module_path ~full:["A" ; "B" ; "C"] ~pwd:["A"]
     ]}

     returns
     {[
     ["B" ; "C"]
     ]}

  *)
  val module_path : full:path -> pwd:path -> path
end

(** {6 Ident} *)

module Ident :
sig
  (**
     like {[asr, or, mod, lsr, +, etc...]}
  *)
  val is_operator : string -> bool
end

(** {6 Optimizing Ocaml code} *)

(**
 * Tries to optimize the code (without affecting its semantics).
 * For now it only does a couple of simplifications of match
 * expressions such as:
 *
 * [1]
 * match [EXPR] with
 * | [V1] -> [V1]     ->   [EXPR]
 * | ...
 * | [Vn] -> [Vn]
 *
 * [2]
 * match [EXPR] with       match e with
 * | [V] -> [V]       ->   | [V] as E -> E
 * | ...                   | ...
 *
 * [3]
 * match match [EXPR] with                           match [EXPR] with
 *       | A(a1, ..., an) -> B(e_b1, ..., e_bn)      | A(a1, ..., an) -> let (b1, ..., bn) = (e_b1, ..., e_bn) in X
 *       | C(c1, ..., cn) -> D(e_d1, ..., e_dn)  ->  | C(c1, ..., cn) -> let (c1, ..., cn) = (e_c1, ..., e_cn) in Y
 * with
 * | B(b1, ..., bn) -> X
 * | D(d1, ..., dn) -> Y
 *
 * All are needed for TRX code generation.
 *   But in the future more optimizations may be added so it's good
 * to apply this functions to your generated code so that it can
 * benefit from them as they come.
 *)
val optimize : Ocaml.code -> Ocaml.code

(**
   Misc
*)
module Misc :
sig
  (**
     Utils for passtracker.
     Returns the number of node of an expression.
     FIXME:
     currently, the pattern are not counted.
  *)
  val size : Ocaml.expr -> int
end

(**
   Simple dependencies computation.
*)
module Deps :
sig
  (**
     Fold on every ident.
     Used for computing the dependency of an expression.
  *)
  val deps : (Ocaml.param_effectif -> 'acc -> 'acc) -> 'acc -> Ocaml.expr -> 'acc
end
