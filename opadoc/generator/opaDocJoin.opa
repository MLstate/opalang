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
 * Joining comments and types annotation
**/

/**
 * {1 About this module}
 *
 * Comments are read from opa files by opadoc, and types annotation are read
 * from {b api} files, which have been produced by {b opa --api}.
 *
 * This module is used for binding comments and types annotation,
 * in an association based on the elements positions.
**/

import stdlib.io.file

OpaDocJoin = {{

  /**
   * {2 Utils: get}
  **/

  /**
   * Get the position from a comment or an api element
  **/
  get_pos(x) =
    match x : Join.mix with
    | { api = { ~pos ; ... } }
    | { comment = { ~pos ; ... } } -> pos

  /**
   * Get the origin file from a comment or an api element
  **/
  get_file(x)=
    match x : Join.mix with
    | { api = { ~fname ; ...} }
    | { comment = {~fname ...} } -> fname

  /**
   * {2 Utils: make}
  **/

  /**
   * Build a mixed join element from a comment
  **/
  comment(x) = { comment = x } : Join.mix

  /**
   * Build a mixed join element from an api
  **/
  api(x) = { api = x } : Join.mix

  /**
   * Build a list of mixed join element from a list of comments
  **/
  from_comments(x) = List.map(comment, x)

  /**
   * Build a list of mixed join element from a list of apis
  **/
  from_apis(x) = List.map(api, x)

  /**
   * Build a list of mixed join element from the comments and apis
  **/
  join_all(comments, apis) = from_comments(comments) ++  from_apis(apis)

  /**
   * {2 Utils: association}
  **/

  /**
   * Given a list a mixed elements (comments and api), build a map
   * of mixed elements indexed by the original filenames.
  **/
  file_separation(all_join : list(Join.mix)) =
    // list ==> stringmap of list, indexed by filename
    upd(mix, map) =
      f = OpaDocUtils.uri_of_path(get_file(mix))
      new_list = List.cons(mix, StringMap.get(f, map) ? [])
      StringMap.add(f, new_list, map)
    List.foldl(upd, all_join, StringMap.empty)

  /**
   * Given a list of [Join.mix] elements from the same file, compute
   * the association between elements, by grouping comments with api
   * whenever it is possible.
  **/
  associate_one_file(l : list(Join.mix)) : Join.joined =
    // sort the list so that the order correspond to the order of definitions
    // in the original file.
    l = List.sort_by(get_pos, l)
    rec aux(res, l) =
      match l with
      | [] -> res

        /*
          FIXME: when the patas is implemented in qml,
          simplify the following patterns
          e.g.
           [ {~comment= { cat={Glob} ; ...  }} | rl ]
        */

      | [ {~comment= { cat={Glob} ; content=_ ; fname=_ ; pos=_ }} | rl ] ->
        aux([{support=none comment=some(comment)} | res], rl)

      | [ {~comment= { cat={Assoc} ; content=_ ; fname=_ ; pos=_ }}, {~api} | rl ] ->
        aux([{support=some(api) comment=some(comment)} | res], rl)

      |  [ {~api} | rl ] ->
        aux([{support=some(api) comment=none} | res], rl)

      |  [ {~comment= { cat={Assoc} ; content=_ ; fname=_ ; pos=_ }} | rl ] ->
         aux([{support=none comment=some(comment)} | res], rl)
    aux([], l)

  /**
   * Given a map of [Join.mix] elements indexed by origin filename,
   * perform the association between elements for each files.
  **/
  associate(byfilemap) = StringMap.map(associate_one_file, byfilemap)

}}
