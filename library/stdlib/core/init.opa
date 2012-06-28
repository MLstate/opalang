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

/**
 * @author Quentin Bourgerie, 2012
 * @destination internal
 * @stability unknown
 *
 * This module is used by the compiler to set some values at the [init] phase.
 */
@server_private
ExecInit = {{

  @private exec_id = Reference.create("")

  /**
   * Returns the id of the executable.
   */
  id() = Reference.get(exec_id)

  @package
  set_id(id) = Reference.set(exec_id, id)

}}

@opacapi
ExecInit_set_id = ExecInit.set_id
