(*
    Copyright © 2011, 2012 MLstate

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
   Ident for AST.
*)

(**
   This kind of ident is used for generating code.
   It has a support for renaming, and packages separation.

   This was originally Ident, but now is exported
   there since we use it in ocamllang too.

   So, we need probably to change some opa-specificities.
   (in a next commit)
*)

type uniq
type t = private
         | Source of string
         | FakeSource of string
         | Internal of uniq

(** comparaison. equal implementation is [compare a b = 0]*)
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

(** Constructors *)
val next : ?filename:string -> ?descr:string -> string -> t  (** Fixed : don't allow anonymous internal *)
  (** Fixed : don't allow anonymous internal *)
val nextf : ?filename:string -> ?descr:string -> ('a, unit, string, t) format4 -> 'a

val source        : string -> t
val fake_source   : string -> t

(** this is like to_string but raise an error if it is a db *)
val stident       : t -> string

val is_operator   : t -> bool

(** see if it is really used -- it is possibly a good idea *)
(* val active_alpha_protection : unit -> unit *)

(** beware of usability of `id with backquote` : with libconvert it becomes a nightmare
    Beware: this function behaves differently whether we are testing mode
    or not => it is for debug
    If you want something else, then use either [stdident], [original_name]
    (or [refresh] insteand of [next (to_string _)])
*)
val to_string : t -> string

(** guaranties unicity of string, fails for source ident *)
val to_uniq_string: t -> string

(** gives back the non-uniq'ed name -- for hacking only *)
val original_name : t -> string

val refresh       : ?descr:string -> ?map:(string -> string) -> t -> t
  (**
     equivalent to fun x -> ExprIdent.next (map (origrepr x))
     except that the description and filename is kept
  *)

val refreshf : map:(string -> string, unit, string) format -> t -> t
  (**
     example of use: [refreshf ~map:"coucou_%s" ident]
  *)

val concrete_string  : t -> string

(** used for printing opa-ligth, before or after renaming
    dont_protect_operator is [false] by default, it means that
    the identitifier should not be protect with backquote, typically
    for infix application printing *)
val opa_syntax : ?dont_protect_operator:bool -> t -> string

(** used for printing a lighter code with opatrack, we do not print the
    description, just the local stamp.
    Beware: do not use for generating code, just for debugging.
*)
val light_ident : t -> string

(** works only on internal identifiers *)
val get_package_name : t -> string

(** returns none when the identifier is not internal *)
val safe_get_package_name : t -> string option

val renaming_should_warn_when : t -> [`used | `unused | `never]
