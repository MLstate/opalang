/*
    Copyright © 2011 MLstate

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

import stdlib.core.{date}

type bson_buf = external

type RPC.Bson.bson0('bson) =
    { Double: (string, float) }
  / { String: (string, string) }
  / { Document: (string, 'bson) }
  / { Array: (string, list('bson)) }
  / { Binary: (string, string) }
  / { ObjectID: (string, string) }
  / { Boolean: (string, bool) }
  / { Date: (string, Date.date) }
  / { Null: (string, void) }
  / { Regexp: (string, (string, string)) }
  / { Code: (string, string) }
  / { Symbol: (string, string) }
  / { CodeScope: (string, (string, 'bson)) }
  / { Int32: (string, int) }
  / { Timestamp: (string, (int, int)) }
  / { Int64: (string, int) }

@opacapi
type RPC.Bson.bson = RPC.Bson.bson0(RPC.Bson.bson)

//@both <-- ??? why doesn't this work ???
Bson = {{

  /** Convenience function, dump string as hex and ascii */
  dump(base: int)(s: string): string =
    du=%% BslBson.Bson.dump %%
    du(base,s)

  /** Return new Bson Object ID */
  new_oid(_:void): string =
    no=%% BslBson.Bson.new_oid %%
    no(void)

  /**
   * Serialize a Bson record as a Bson.buf buffer (which is abstract).
   */
  serialize(hint: int)(b: RPC.Bson.bson): bson_buf =
    ser=(%% BslBson.Bson.serialize %%:int, RPC.Bson.bson -> bson_buf)
    ser(hint,b)

  /**
   * Access a serialized Bson value as a string.  You should only need this
   * for debug purposes since it involves copying the data.
   */
  get = (%% BslBson.Bson.get %%: bson_buf -> string)

  /**
   * Deserialize a string into a Bson value.  Note that the semantics
   * of {none} do not mean invalid data since a valid but empty object
   * will also return {none}.
   */
  deserialize(str: string): option(RPC.Bson.bson) =
    deser=(%% BslBson.Bson.deserialize %%:string -> option(RPC.Bson.bson))
    deser(str)

}}
