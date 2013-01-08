/*
    Copyright © 2011, 2012, 2013 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa.  If not, see <http://www.gnu.org/licenses/>.
*/

import stdlib.apis.postgres
package stdlib.database.postgres

@opacapi @abstract type DbPostgres.t = Cps.future(Postgres.db)

@opacapi @abstract type DbPostgres.engine = void

@opacapi @abstract type DbPostgresSet.engine = void

@opacapi @abstract type DbPostgresSet.t('a) = dbset('a, DbPostgresSet.engine)

@opacapi type DbPostgres.private.val_path('a) = void

@opacapi type DbPostgres.private.ref_path('a) = void

type DbPostgres.val_path('a) = Db.val_path('a, DbPostgres.engine)

type DbPostgres.ref_path('a) = Db.ref_path('a, DbPostgres.engine)

/**
 * PostgreSQL database backend
 *
 * @category database
 * @author Quentin Bourgerie
 * @destination experimental
 */
DbPostgres = {{

}}


