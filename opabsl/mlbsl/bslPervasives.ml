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

(**
 * {1 String/char conversions}
*)


##register string_of_char : char -> string
let string_of_char = String.make 1

##register string_to_char : string -> char
let string_to_char s = String.get s 0

##register int_of_char : char -> int
let int_of_char c = Pervasives.int_of_char c

##register int_of_first_char : string -> int
let int_of_first_char c = Pervasives.int_of_char (String.get c 0)

(**
 * Physical equality between OCaml objects
*)
##register areSameObject : 'a, 'b -> bool
let areSameObject x y = Obj.magic x == Obj.magic y

(**
 * Determine if the code is executed server-side
 *
 * @return true in this implementation
*)
##register webutils_server_side : -> bool
let webutils_server_side () = true



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
(* DO NOT USE Char.compare here, it would break the invariant described above *)
##register compare_char \ `(Pervasives.compare : char -> char -> int)` : char, char -> int

##register int_cmp_neq   \ `(Pervasives.(!=) : int -> int -> bool)` : int, int -> bool
##register int_cmp_eq    \ `(Pervasives.(==) : int -> int -> bool)` : int, int -> bool
##register int_cmp_lneq  \ `(Pervasives.(<)  : int -> int -> bool)` : int, int -> bool
##register int_cmp_leq   \ `(Pervasives.(<=) : int -> int -> bool)` : int, int -> bool
##register int_cmp_gneq  \ `(Pervasives.(>)  : int -> int -> bool)` : int, int -> bool
##register int_cmp_geq   \ `(Pervasives.(>=) : int -> int -> bool)` : int, int -> bool

##register stop: -> 'a
let stop () =
  Logger.warning "BslSyslog.stop has been called :  shutting down application ...";
  ServerLib.do_exit 1





(* this function is used by the pass that discard slicer directives *)
##register never_do_anything : 'a -> 'b
(* could take a string and display it so that we can see if something goes wrong *)
let rec never_do_anything _ = Obj.magic never_do_anything

##register warning : string -> void
let warning s =
  sync_to_print_on stderr;
  Logger.warning "%s" s

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

module OpaExc =
struct
  (**
     Keep synchronized with stdlib.core/exception.opa
  *)

  let f_fail = ServerLib.static_field_of_name "fail"
  let f_position = ServerLib.static_field_of_name "position"
  let fail ~message ~position =
    let r = ServerLib.empty_record_constructor in
    let r = ServerLib.add_field r f_fail (ServerLib.wrap_string message) in
    let r = ServerLib.add_field r f_position (ServerLib.wrap_string position) in
    ServerLib.make_record r

  let f_transaction_failure = ServerLib.static_field_of_name "Transaction_failure"
  let transaction_failure = ServerLib.make_simple_record f_transaction_failure

  let f_ocaml_exc = ServerLib.static_field_of_name "ocaml_exc"
  let f_bslkey = ServerLib.static_field_of_name "bslkey"
  let ocaml_exc bslkey exc =
    let message = Printexc.to_string exc in
    let r = ServerLib.empty_record_constructor in
    let r = ServerLib.add_field r f_ocaml_exc (ServerLib.wrap_string message) in
    let r = ServerLib.add_field r f_bslkey (ServerLib.wrap_string bslkey) in
    ServerLib.make_record r
end

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

##register print_float : float -> void
let print_float i = print_string (string_of_float i)

##register print_char : char -> void
let print_char i = print_string (string_of_char i)

##register println_int : int -> void
let println_int i = println_string (string_of_int i)

##register println_float : float -> void
let println_float i = println_string (string_of_float i)

##register println_char : char -> void
let println_char i = println_string (string_of_char i)

##register flush_stdout : -> void
let flush_stdout () = Pervasives.flush stdout

##register flush_all : -> void
let flush_all () = Pervasives.flush_all ()



##extern-type black = unit




(**
 * Attempt to convert an arbitrary value to string.
 *)
##register dump : 'a -> string
let dump x =
  DebugPrint.print x

##register debug_print: 'a -> string
let debug_print = dump



##opa-type Order.comparison
##opa-type Order.ordering

##register order_lt\ ord_result_lt: Order.ordering
##register order_eq\ ord_result_eq: Order.ordering
##register order_gt\ ord_result_gt: Order.ordering
##register compare_lt\ comp_result_lt: Order.comparison
##register compare_eq\ comp_result_eq: Order.comparison
##register compare_gt\ comp_result_gt: Order.comparison
##register compare_neq\ comp_result_neq: Order.comparison

let ord_result_lt   = wrap_opa_order_ordering (ServerLib.make_simple_record (ServerLib.static_field_of_name "lt"))
let ord_result_eq   = wrap_opa_order_ordering (ServerLib.make_simple_record (ServerLib.static_field_of_name "eq"))
let ord_result_gt   = wrap_opa_order_ordering (ServerLib.make_simple_record (ServerLib.static_field_of_name "gt"))

let comp_result_lt   = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "lt"))
let comp_result_eq   = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "eq"))
let comp_result_gt   = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "gt"))
let comp_result_neq  = wrap_opa_order_comparison (ServerLib.make_simple_record (ServerLib.static_field_of_name "neq"))
