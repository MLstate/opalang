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
   Core of the BSL.

   Values exported by this module must appear both on the client and on the server.

   @author David Rajchenbach-Teller (Review, clean-up)
*)

(** before printing somewhere, we can indicate that we will,
    all printing with such indication will respectfully flush correctly stdout stderr output to avoid mixed outputs
    take stdout or stderr in parameter
*)
let sync_to_print_on =
  let need_to_flush = ref stdout in
  fun std ->
    if not (!need_to_flush == std) then (
      flush !need_to_flush;
      need_to_flush := std
    )

(* used to be set to false at startup of server, disabled for now *)
let tokill = ref false;;


(**
 * {1 Arithmetic operations}
*)

##register int_add \ `Pervasives.( + )` : int, int -> int
##register float_add \ `Pervasives.( +. )` : float, float -> float

##register int_sub \ `Pervasives.( - )` : int, int -> int
##register float_sub \ `Pervasives.( -. )` : float, float -> float

##register int_mul \ `Pervasives.( * )` : int, int -> int
##register float_mul \ `Pervasives.( *. )` : float, float -> float

##register int_div \ `Pervasives.( / )` : int, int -> int
##register float_div  \ `Pervasives.( /. )` : float, float -> float

##register int_mod \ `Pervasives.( mod )` : int, int -> int
##register mod  \ `Pervasives.( mod )`  : int, int -> int

##register int_rem : int, int -> int
let int_rem l r =
  let r_pos = abs r in
  let l_pos = abs l in
  let res = l_pos - (r_pos * (l_pos / r_pos)) in
  if (r > 0) then res else (-res)

##register int_neg \ `Pervasives.( ~- )` : int -> int
##register float_neg \ `Pervasives.( ~-. )` : float -> float

##register int_of_first_char : string -> int
let int_of_first_char c = Pervasives.int_of_char (String.get c 0)

(**
 * Physical equality between OCaml objects
*)
##register areSameObject : 'a, 'b -> bool
let areSameObject x y = Obj.magic x == Obj.magic y



(**
 * Comparison functions
 *
 * The results of these functions is normalized to always return -1, 0 or 1 -- it's an important invariant
 * Caml has this invariant, even if it is unspecified
 * we rely on it, and check that it really holds in reftester
 *)

(* BE AWARE THAT THESE BYPASS ARE MANUALLY REGISTERED IN QMLEFFECT
   SO YOU SHOULD SYNCHRONISE THE TWO FILE FOR ANY NAME CHANGE*)

(* to trigger Ocaml optimisation for comparing ints/float/strings,
   3x faster than compare for int *)
##register compare_int \ `(Pervasives.compare : int -> int -> int)` : int, int -> int
##register compare_float \ `(Pervasives.compare : float -> float -> int)` : float, float -> int
##register compare_string \ `String.compare` : string, string -> int
##register [opacapi] compare_raw \ `ServerLib.compare` : 'a, 'a -> int

##register int_cmp_neq   \ `(Pervasives.(!=) : int -> int -> bool)` : int, int -> bool
##register int_cmp_eq    \ `(Pervasives.(==) : int -> int -> bool)` : int, int -> bool
##register int_cmp_lneq  \ `(Pervasives.(<)  : int -> int -> bool)` : int, int -> bool
##register int_cmp_leq   \ `(Pervasives.(<=) : int -> int -> bool)` : int, int -> bool
##register int_cmp_gneq  \ `(Pervasives.(>)  : int -> int -> bool)` : int, int -> bool
##register int_cmp_geq   \ `(Pervasives.(>=) : int -> int -> bool)` : int, int -> bool


##register jlog : string -> void
let jlog s =
  sync_to_print_on stderr;
  Logger.notice "%s" s

(**
 * Type-unsafe identity.
 * Not for casual user.
 * For bypassing only the Opa typer, use rather [\@unsafe_cast]
 **)
##module Magic
  ##register [opacapi] id \ `Obj.magic` : 'a -> 'b
##endmodule

module OpaExc = BslNativeLib.OpaExc


(**
   Bypass used in the compilation of the directive {[\@fail]}.
*)
##register [opacapi, cps-bypass] fail_cps : string, string, continuation('a) -> void
let fail_cps message position k =
  Logger.error "%s@\n@{<bright>@@fail@}: %s" position message ;
  let exc = OpaExc.fail ~message ~position in
  let k = QmlCpsServerLib.handler_cont k in
  QmlCpsServerLib.return k exc

(**
   Primitive used in the projection of function which may raise an ocaml exception
*)
##extern-type ocaml_exception = exn

##register [opacapi, no-projection] return_exc : string, ocaml_exception, continuation('a) -> void
let return_exc bslkey exc k =
  let exc = OpaExc.ocaml_exc bslkey exc in
  let k = QmlCpsServerLib.handler_cont k in
  QmlCpsServerLib.return k exc

(*
  This function is used only in non cps mode.
  In this case, this is an Ocaml exception
*)
##register [opacapi] fail : string, string -> 'a
let fail message position =
  Logger.error "%s@\n@{<bright>@@fail@}: %s" position message ;
  let exc = OpaExc.fail ~message ~position in
  raise (Failure (DebugPrint.print exc))

##register get_stack : -> string
let get_stack = Printexc.get_backtrace

##register print_string : string -> void
let print_string s = sync_to_print_on stdout ; Pervasives.print_string s

##register print_endline \ println_string: string -> void
let println_string s = sync_to_print_on stdout ; Pervasives.print_endline s

##register prerr_string : string -> void
let prerr_string s = sync_to_print_on stderr ; Pervasives.prerr_string s

##register prerr_endline : string -> void
let prerr_endline s = sync_to_print_on stdout ; Pervasives.prerr_endline s

##register print_int : int -> void
let print_int i = print_string (string_of_int i)

##extern-type black = unit




(**
 * Attempt to convert an arbitrary value to string.
 *)
##register dump : 'a -> string
let dump x =
  DebugPrint.print x


##opa-type Order.comparison
##opa-type Order.ordering

let ord_result_lt   = wrap_opa_order_ordering (ServerLib.make_simple_record (ServerLib.static_field_of_name "lt"))
let ord_result_eq   = wrap_opa_order_ordering (ServerLib.make_simple_record (ServerLib.static_field_of_name "eq"))
let ord_result_gt   = wrap_opa_order_ordering (ServerLib.make_simple_record (ServerLib.static_field_of_name "gt"))

let comp_result_lt   = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "lt"))
let comp_result_eq   = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "eq"))
let comp_result_gt   = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "gt"))
let comp_result_neq  = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "neq"))
