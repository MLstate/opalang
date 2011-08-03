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
 * Simple value input/output in file
**/

/**
 * {1 About this module}
 *
 * This module defines function to load/store opadoc structures into the file system.
 * In particular, it is used to load {b api} files produced by {b opa} containing
 * the types of the entries defined in the application.
 *
 * Since {b opadoc} parses files, computes an association between comments and apis,
 * it is also possible to save the result of this work. That is {b apix} files.
**/

JsonFile = {{

  /**
   * {2 Output}
  **/

  /**
   * Write a value to a file, using Json format
  **/
  to_file(f, x) =
    file_of_string = %% BslFile.of_string %% : string, string -> void
    file_of_string(f, OpaSerialize.serialize(x))

  /**
   * {2 Input}
  **/

  /**
   * {3 Api files}
   *
   * Api files are produced by {b opa} and contained informations extracted from the compilation,
   * like types, visibility, special directives, etc.
  **/

  /**
   * Read an api file, and build the list of containing entries.
   * <!> The api file should be correct, and not corrupted, or
   * this function will fail.
  **/
  open_api(fname) : list(Api.entry) =
    i = (%% BslFile.is_regular %%) : string -> bool
    do if not(i(fname)) then error("JsonFile.from_file : file {fname} not found")

    file_content = %% BslFile.content %% : string -> string

    v = OpaSerialize.String.unserialize(file_content(fname)) : option(list(Api.entry))

    v ? (
      json = Json.of_string(file_content(fname)) ? error("JsonFile.from_file : bad json in file {fname}")
      do jlog(Json.to_string(json))
      error("JsonFile.from_file : bad type for {fname}")
    )

  /**
   * {3 Apix files}
   *
   * Api files are produced by {b opadoc} and contained the association
   * between comments and api informations extracted from the compilation.
   *
   * This format can be use for storing the result of the computed association,
   * for beeing used in a next execution of opadoc, and/or other programs.
  **/

  /**
   * Read an apix file, and build the map of joined entries, indexed by filenames.
   * <!> The apix file should be correct, and not corrupted, or
   * this function will fail.
  **/
  open_apix(fname) : Join.final =
    file_content = %% BslFile.content %% : string -> string
    i = (%% BslFile.is_regular %%) : string -> bool
    do if not(i(fname)) then error("File {fname} not found")
    // _ = Json.of_string(file_content(fname)) ? error("Bad json in file {fname}")
    v = OpaSerialize.String.unserialize(file_content(fname)):option(Join.final)
    v ? (
      json = Json.deserialize_opt(file_content(fname)) ? error("Bad json in file {fname}")
      do jlog(Json.serialize_opt(json))
      error("Bad type for {fname}")
    )

}}
