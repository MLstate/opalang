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
##extern-type Hashtbl.t('key, 'value) = ('key, 'value) Hashtbl.t

##register create \ `Hashtbl.create` : int -> Hashtbl.t('key, 'value)

##register clear \ `Hashtbl.clear` : Hashtbl.t('key, 'value) -> void

##register add \ `Hashtbl.add` : Hashtbl.t('key, 'value), opa['key], opa['value] -> void

##register try_find : Hashtbl.t('key, 'value), opa['key] -> option(opa['value])
let try_find h k = try Some (Hashtbl.find h k) with Not_found -> None

##register remove \ `Hashtbl.remove` : Hashtbl.t('key, 'value), opa['key] -> void

##register size \ `Hashtbl.length`: Hashtbl.t('key, 'value) -> int

##register mem \ `Hashtbl.mem` : Hashtbl.t('key, 'value), opa['key] -> bool
