/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

import stdlib.core.rpc.core

/**
 * {1 Interface} This is deprecated, use module Limits in stdlib.core.rpc
 */

@deprecated({use="Limits.min_int"})
min_int = Limits.min_int

@deprecated({use="Limits.max_int"})
max_int = Limits.max_int
