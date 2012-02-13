/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
