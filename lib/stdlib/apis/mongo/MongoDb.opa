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

/**
 * This file is no longer used, this is just a reminder of
 * features which will appear in future releases.
 *
 * MongoMap = {{
 *   // Implementation of map using underlying MongoDB.
 * }}
 * 
 * MongoArray = {{
 *   // Implementation of Array using underlying MongoDB.
 * }}
 * 
 * MongoTree = {{
 *   // Implementation of Tree using underlying MongoDB.
 *   // Note that there is an interesting page on the MongoDB
 *   // website on embedding trees in MongoDB:
 *   // http://www.mongodb.org/display/DOCS/Trees+in+MongoDB
 * }}
 * 
 * Db = {{
 *   - This will look like the current Db module (as far as we can implement it).  Mostly you
 *     will get: read/write/remove/{int/string}map_search/{int/string}map_fold_range.  Note that
 *     implementing history with MongoDB could prove heavy-weight and potentially disastrous
 *     for performance although I'm sure it could be done.  A primitive form of transaction,
 *     however, might be a good option.  Modification time could be handled low-level in
 *     the driver at the expense of some peformance.
 * }}
 *  
 **/

// End of file MongoDb.opa
