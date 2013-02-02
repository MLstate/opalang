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
   Install directories
*)

(**
   The install directory is where libs have been installed.
   This directory is needed by opa to compile applications.

   The location of this directory is pointed by an environment var,
   which use to be one of the following vars:
   MLSTATELIBS, OPADIR, (used to be also BASEDIR), whatever...

   We will now stabilize the build process, and choose only 1 name for this variable.
   There we do a toplevel getenv, and then in any other place in the compiler,
   we should refer to this value.
*)

(**
   Currently "MLSTATELIBS"
   This is used for generating e.g. Makefile, without duplicating the string
   in the code of the compiler.
*)
val name : string

(**
   This is the value of the installdir variable, returned once for all
   by a getenv executed at toplevel of this module.
*)
val getenv : string Lazy.t

(**
   The path into the install dir to find the lib of the Opa compiler.
   Without the first '/', the current value is ["lib/opa/static"]
*)
val lib_opa : string

(**
   The path into the install dir to find the compiled opa packages of the stdlib.
   The current value is ["lib/opa/stdlib"]
*)
val opa_packages : string
