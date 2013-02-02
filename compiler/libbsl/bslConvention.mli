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
   Convention of naming for plugins.
   @author Mathieu Barbin
*)

(**
   These convention are shared knowledge between opa-plugin-* (bsl{register,browser}) and the Opa compiler,
   for simplifying plugin inclusion.
*)

module Extension :
sig
  (**
     Extensions produced for plugins.
  *)

  type extension = string
  val bypass : extension
  val jsconf : extension
  val plugin : extension
end

module Suffix :
sig
  (**
     Given the name of the lib and adding these suffix, gives the name of the corresponding file,
     <!> without the extension.
  *)

  type suffix = string
  val nodejspackage : suffix
  val jskeys : suffix
  val loader : suffix
  val marshalplugin : suffix
  val mlruntime : suffix
  val plugin : suffix
end

(**
   Convention over plugin names
*)
val plugin_name : string -> string

(** {6 Inclusions} *)

(**
   Regrouping implied options by the inclusion of a plugin
*)
type inclusion = {
  extrapath : string ;
  (**
     An absolute path name to a directory to include
  *)

  extralib : string ;
  (**
     An absolute filename to the mlruntime to link with
  *)

  plugin : string ;
  (**
     An absolute filename to the marshaled plugin to load
  *)
}

(**
   Given the location of an plugin directory (opp), return
   the list of implied options.
   If the location is relative, will prefix it by the current
   working directory.
*)
val inclusion : cwd:string -> string -> inclusion
