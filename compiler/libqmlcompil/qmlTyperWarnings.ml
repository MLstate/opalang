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
   Regrouping all warnings for the typer.
   @author Mathieu Barbin
   @author Francois Pessaux
*)

(**
   This module is meant to contained all warnings class used by any typer.
   The file has no mli because is would just duplicate everything,
   everything is exported.
*)

(**
   The root of warnings handled by the typer.
*)
let typer =
  let doc = "Type system warnings" in
  WarningClass.create ~name:"typer" ~doc ~err:false ~enable:true ()

(**
   The class of warning for @@warncoerce directive.
   Principally used for ensuring that the expression after a [do]
   has type void.
*)
let warncoerce =
  let doc = "Unexpected type for an expression" in
  WarningClass.create ~parent:typer ~name:"warncoerce" ~doc ~err:false ~enable:true ()

(**
   The class of warning for @@warncoerce directive.
   Principally used for ensuring that the expression after a [do]
   has type void.
*)
let cyclic =
  let doc = "Cyclic type in an expression" in
  WarningClass.create ~parent:typer ~name:"cyclic" ~doc ~err:false ~enable:true ()

(**
   Regrouping all previous classes.
   Any application is free to load these warning or not.
*)
let warning_set = WarningClass.Set.create_from_list [typer]
(*
  adding the root node 'typer' in enough, since other classes
  are defined as children of the root
*)
