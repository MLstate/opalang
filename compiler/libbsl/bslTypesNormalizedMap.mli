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
   Map based on equality on [BslTypes.t] under normalization.

   @author Mathieu Barbin
*)

(**
   Two types equal under alphaconversion are equal regarding to
   the comparaison used to build this map.

   If you need a structural comparaison, cf [BslTypesMap].

   Beware, this Map is particulary unefficient.
   However, the efficiency can be optimized if needed (cf TODO in
   comments in the implementation of [BslTypes.compare ~normalize:true])
*)

include BaseMapSig.S with type key = BslTypes.t
