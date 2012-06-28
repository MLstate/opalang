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

(*
  Mathieu Wed Nov 17 16:33:22 CET 2010
    The following comments is deprecated.
    This module should rather not exists.
*)

(** The content of this file belongs *logically* to qmlTypes. It
    contains all the features that *should* be implemented there
    (directly on public types) but are not, and rather are implemented
    through HMX import. Of course this forbids them to be in qmlTypes
    because that would be a cyclic dependency. In the end, this file
    can be seen as a hack to work around the faulty Ocaml module
    system.
*)


val equal_ty : ?gamma:QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty -> bool

(* provides a list of types instantiated for argument variables during
   unification (makes sure these variables are substituted for, by refreshing
   and using unification invariant that newer variables are subsituted for);
   ignores the rest of subsitution; throws an exception if unification fails *)
val unify_and_show_instantiation :
  gamma:QmlTypes.gamma -> allow_partial_application:bool -> QmlAst.ty -> QmlTypes.typescheme ->
    QmlAst.ty list * QmlAst.ty_row list * QmlAst.ty_col list

(* for now no argument can be overloaded; *)
val unifiable :
  ?gamma:QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty -> bool
