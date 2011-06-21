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
 * {1 Utils for generation}
 *
**/

OpaDocUtils = {{

  /**
   * Build the xhtml concatenation from the elements of a list.
  **/
  fold_xhtml(f, l) = List.fold_left((acc, x -> <>{acc}{f(x)}</>),<></>, l)

  sanitize_path(path) =
    aux(c, acc) =
      r =
        match c with
        //delim
        | ">" -> "sup"
        | "<" -> "inf"
        | "#" -> "sharp"
        | "%" -> "percent"
        // unwise
        | "^" -> "hat"
        | "`" -> "bkquote"
        | "|" -> "pipe"
        | s -> s
        end
      acc^r
    String.fold(aux, path, "")


  /**
   * Given an entry, tell if the entry is private, meaning that it should not be displayed.
   * This function reads the current state of the parameters.
  **/
  is_private(entry : Api.entry) =
    params = OpaDocParameters.get()
    if params.private
    then false
    else
      match entry.code_elt with
      | { type_def = { ty_def_visibility = { TDV_private = _ } ; ... } } -> true
      | { type_def = _ } -> false
      | { value = { visibility = { public }; ... } } -> false
      | { value = _ } -> true


  /**
   * Tell if an entry is a type_def
  **/
  entry_is_type_def(entry:Api.entry) =
    match entry.code_elt with
    | { type_def = _ } -> true
    | _ -> false

  /**
   * Tell if an entry is a type_def
  **/
  entry_is_value(entry:Api.entry) =
    match entry.code_elt with
    | { value = _ } -> true
    | _ -> false

  /**
   * Sort using a string in the fst
  **/
  sort_by_string(list) =
    aux((p1, _), (p2, _)) =
      String.ordering(String.to_lower(p1), String.to_lower(p2))
    List.sort_with(aux, list)

}}
