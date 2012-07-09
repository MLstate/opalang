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
(** Handle [QmlAst.opavalue_directive] and extend corresponding magic
    function with the specialized function, by toplevel registering.

    For example : [\@stringifier f(x) = ...]
    is transformed to : {[
    f(x) = ...
    _ = OpaValue_add_to_string(f)
    ]}

    BEWARE : Enrich magic can works only if closures are activated.

*)

(** Process the qml code and returns a [map] that bind a generic
    funtion to specialized functions (see [Pass_SimplifyMagic]),
    updated [annotmap] and [code]. *)
val process_code :
  stdlib:QmlTypes.gamma ->
  gamma:QmlTypes.gamma ->
  annotmap:QmlAst.annotmap ->
  QmlAst.code ->
  Pass_SimplifyMagic.env * QmlAst.annotmap * QmlAst.code

(** Just purge [QmlAst.opavalue_directive]. Usefull if closures are
    unactivated.*)
val just_purge : QmlAst.code -> QmlAst.code
