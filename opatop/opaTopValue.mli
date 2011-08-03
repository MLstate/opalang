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
   OpaTop runtime values : algebra and utils.
   @author Mathieu Barbin
*)

(** {6 Refactoring WIP} *)

(**
   TODO : Ident and IdentMap are 2 toplevel module
   in compilerlib.
*)

(** {6 Type alias} *)

type pos = FilePos.pos
type 'a pprinter = 'a LangPrint.pprinter

(** {6 Runtime Value Algebra} *)

(**
   Value algebra.

   We use Lazy.t all the time for conciseness, it will always be
   already evaluated for non-lazy records. The [t option] is only
   used for lazy DB records.

   Note about the value albebra:
   opatop is not targetting performance. We have already some back-end
   for that. The goal is to have as much tracability as possible,
   for debbuging the all framework.

   StringMap and IdentMap are used for having a typer off mode.
   Positions are used to test errors reporting features.
*)
type t =
  | V_const   of pos * QmlAst.const_expr
  | V_record  of pos * (t Lazy.t) StringMap.t * t option ref
  | V_closure of pos * (t IdentMap.t) ref * QmlAst.expr
  | V_extern  of pos * string * BslTypes.t list * Obj.t
  | V_bypass  of pos * BslTypes.t list * BslTypes.t * Obj.t


(** {6 GUIDELINES for matching a [t]} *)
(**
  Verbose :
   {[
   | V_const   (pos, const)
   | V_record  (pos, fields, info)
   | V_closure (pos, env_val, env_type, expr)
   | V_extern  (pos, name, params, obj)
   | V_bypass  (pos, ByFun (arg, ret, obj))
   ]}

  Shorter :
   {[
   | V_const   (p, c)
   | V_record  (p, fds, i)
   | V_closure (p, ev, et, e)
   | V_extern  (p, n, pms, o)
   | V_bypass  (p, ByFun (arg, ret, o))
   ]}
*)

(** {6 Positions} *)
(**
   This module follows the guidelines for error reporting :
   + It uses [OManager] for printing localized error messages.
*)
(** *)
val pos : t -> pos
val reset_pos : t -> pos -> t
val merge_pos : t -> pos -> t

(**
   A default pos for [Value.t]
*)
val nopos : pos

(** {6 Printing} *)

(**
   Printer for values.
   You can add a type for hidding some structural informations.
   Since fields are lazy, you can force or not the evaluation before
   printing.
   Default :
   - [ty=None] No type, the printer is purelly structural.
   - [force=false] Does not force, will print just ["<lazy>"] in that case.
*)
val pp_value :
  ?ty:QmlAst.ty ->
  ?force:bool ->
  t pprinter

(**
   The default printer [pp_value] with default options
   - [ty] is [None]
   - [force] is [false]

   Exposed in the interface so that we have a pp following the interface
   of pp printers, as requested by the guidelines.
*)
val pp : t pprinter

(**
   Print the type of a value. This is semi structural.
*)
val pp_type : t pprinter

(** {6 Comparaison} *)

(**
   Runtime structural comparaison on values.
   TODO: option strong ==> clarify this, maybe remove or rename.
*)
val compare : ?strong:bool -> t -> t -> int

(**
   A dummy value, use it e.g. when eval is off.
   This is a extern value, equal to an Ocaml int [0].
*)
val t_null : ?pos:pos -> unit -> t

(** {6 Value Environment} *)

type env = t IdentMap.t

(** {6 Projections} *)

(**
   This implements the projection to Ocaml standard runtime values,
   for using external primitives from BSL libs. (bypass plugins)
*)
module Proj :
sig
  (** {6 BSL Types} *)

  val shared_void : t

  val t_int    : ?pos:pos -> int -> t
  val t_float  : ?pos:pos -> float -> t
  val t_string : ?pos:pos -> string -> t
  val t_int64  : ?pos:pos -> Int64.t -> t

  val t_void   : ?pos:pos -> unit -> t

  val t_true   : ?pos:pos -> unit -> t
  val t_false  : ?pos:pos -> unit -> t
  val t_bool   : ?pos:pos -> bool -> t

  val t_none   : ?pos:pos -> unit -> t
  val t_some   : ?pos:pos -> t -> t
  val t_option : ?pos:pos -> t option -> t

  val t_extern : ?pos:pos -> string -> BslTypes.t list -> 'a -> t

  (** {6 Projection} *)
  (**
     Building an Ocaml ['a] from a [Value.t].
     The [eval] function is passed for projecting closure into ml-functions.
  *)
  val ocaml_of_t :
    eval:(env -> QmlAst.expr -> t)  ->
    BslTypes.t ->
    t -> 'ocaml_value

  (**
     The projection in the other way.
  *)
  val t_of_ocaml :
    BslTypes.t -> 'ocaml_value -> t
end
