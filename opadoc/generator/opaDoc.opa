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
 * opadoc is a tool which generates a documentation.
 *
 * {1 Tests}
 *
 * [[**] ]
 * [ [ **] ]
 * [ [** ]]
 * [[* *] ]
 * [ [**] ]
 * [ [**] [**] ]
 * [ [ [**] ] ]
 *
 * [[]]
 * [[][]]
 * [[[]]]
 * [{}]
 * [{}{}]
 * [{{}}]
 *
 * [[t[]]]
 *
 * {{}}
 * {{}{}}
 * {{{}}}
 * {[]}
 * {[][]}
 * {[[]]}
 *
 * {[{t{}}}
 * {[{% \emph{Hello \latex}}}
 *
 * {1 Syntax of documentation comments}
 *
 * Comments in an OPA file are parsed by opadoc to produce associated
 * documentation. A special syntax can be used within these comments in order
 * to organize the produced documentation and make it look better.
 *
 * You can either write independent documentation comments or associate them to
 * code elements (functions, modules...) by placing them just before the
 * element they describe.
 *
 * {2 Structure}
 *
 * You can use the following tags to organize the documentation into different
 * parts.
 *
 * Square and curly brackets can be used inside formatting brackets without
 * being escaped as long as they go in pair (opening and closing ones). If
 * you'd like to use a single opening or closing bracket, you should escape it
 * with a backslash.
 *
 * {3 Headings}
 *
 * {[
 *   {1 Heading1}
 *   {2 Heading2}
 *   {3 Heading3}
 *   {4 Heading4}
 *   {5 Heading5}
 *   {6 Heading6}
 * }
 *
 * {3 Text formats}
 *
 * {4 Basic styles}
 *
 * {b bold}: [{b bold}]
 *
 * {it italic}: [{it italic}]
 *
 * {emp emphasize}: [{emp emphasize}]
 *
 * {^ superscript}: [{^ superscript}]
 *
 * {_ subscript}: [{_ subscript}]
 *
 * {4 Alignments }
 *
 * {C center} [{C
 *   center
 * }]
 * {L left} [{L
 *   left
 * }]
 * {R center} [{C
 *   center
 * }]
 *
 * {4 Code}
 *
 * {[
 *   {[
 *     ...
 *     your code here
 *     ...
 *   }
 * }
 *
 * {4 Verbatim}
 *
 * {[
 *   {v
 *     ...
 *     verbatim text here
 *     ...
 *   }
 * }
 *
 * {4 Lists}
 *
 * {ul ul}
 *
 * {enum enum}
 *
 * {4 Raw LaTeX}
 *
 * {[
 *   {% \emph{Hello \latex}}
 * }
 *
 * {4 Custom}
 *
 * {[
 *   {custom custom tag}
 * }
 */

import stdlib.*

is_dir = %% BslFile.is_directory %% : string -> bool
file_exists = %% BslFile.exists %% : string -> bool
make_dir = %% BslFile.make_dir %% : string -> bool
basename = %% BslFile.basename %% : string -> option(string)

make_path(path_str: string): void =
  aux(dir, acc) =
    dir_path = if not(String.is_empty(acc)) then "{acc}/{dir}" else dir
    do if not(File.is_directory(dir_path)) then
      if not(make_dir(dir_path)) then
        error("can't make dir '{dir_path}'")
    dir_path
  path = String.explode("/", path_str)
  _ = List.fold(aux, path, "")
  void

is_opafile(file) =
  len = String.length(file)
  if len > 4 then
    if String.substring((len-4), 4, file) == ".opa" then true
    else false
  else false

check_api(file) = file_exists("{file}.api")

/* get the list of opa files in the given dir */
walk_dir(path) =
  if String.equals(path,"") then error("Empty string is not a path")
  else
    // glance the dir and get all .opa files
    if is_dir(path) then
      all_files_fun = %%BslFile.fold_dir_rec_opt%% : ('a, string, string -> 'a), 'a, string -> option('a)
      all_files =
        fun4recup(accu,name,path) =
          if is_opafile(name) && (check_api(path)) then
            List.cons((name, path),accu)
          else accu
        all_files_fun(fun4recup,[],path)
      all_files
    else // just a file
       if is_opafile(path) && (check_api(path)) then
          some([(basename(path) ? error("basename error"), path)])
       else error("Not an .opa file or .api not found")

get_doc_info((acc_lc, acc_lt), (name, path)) =
  do jlog("extracting API and comments info for : {name}")
  lc = OpaDocComment.from_opa_file(path)
  lt = OpaApiFile.from_api_file("{path}.api")
  ((lc ++ acc_lc), (lt ++ acc_lt))

gen_doc(output_path, lc, lt) =

  mj = OpaDocJoin.join_all(lc, lt) |> OpaDocJoin.file_separation |> OpaDocJoin.associate
  do OpaDocHtml.gen_xhtml_final(output_path, mj)
  mj


sort_opafiles(opafiles) =
  aux((n1, _), (n2, _)) =
    String.ordering(String.to_lower(n1), String.to_lower(n2))
  List.sort_with(aux, opafiles)

group_by_packages(entries) =
  aux(acc, (_, (entry : Api.entry, _, _)) as item) =
    pkg = entry.pkg
    StringMap.add(pkg, item +> (StringMap.get(pkg, acc) ? []), acc)
  List.fold_right(aux, entries, StringMap.empty)

main_gendoc(output_path_opt, args) =
    aux(arg) = walk_dir(arg) ? error("no opafiles in given directory {arg}")
    opafiles = sort_opafiles(List.collect(aux, args))
    // get all opafiles comment and api info
    (all_lc, all_lt) = List.fold_left(get_doc_info, ([], []), opafiles)
    // type -> (value list) association
    type_table = StringMap.empty

    do jlog("indexing types and values")
    (_type_table, entries) = OpaApiFile.extract_by_path(type_table, all_lt)
    // TODO: use the type_table for an other index
    entries = OpaDocUtils.sort_by_string(entries)

    packages_tree = OpaDocTree.of_packages(entries)
    values_tree = OpaDocTree.of_values(entries)
    types_tree = OpaDocTree.of_types(entries)
    files_tree = OpaDocTree.of_files(opafiles)
    commonjs =
      (OpaDocTree.to_json(packages_tree)
      |> OpaDocHtml.js_of_json("packages_tree_json", _))
      ^
      (OpaDocTree.to_json(values_tree)
      |> OpaDocHtml.js_of_json("values_tree_json", _))
      ^
      (OpaDocTree.to_json(types_tree)
      |> OpaDocHtml.js_of_json("types_tree_json", _))
      ^
      (OpaDocTree.to_json(files_tree)
      |> OpaDocHtml.js_of_json("files_tree_json", _))

    /* Create output directory */
    output_path = output_path_opt ? "doc"
    //resources_path = "{output_path}/resources"
    //do make_path(resources_path)
    do OpaDocHtml.gen_css(output_path)
    do OpaDocHtml.gen_javascript(output_path, commonjs)
    do OpaDocHtml.gen_resources(output_path)

    // build all html doc
    mj = gen_doc(output_path, all_lc, all_lt)

    // gen the doc for all selected .opa files
    do jlog("generating opadoc static website")
    do jlog("generating index page")
    do OpaDocHtml.gen_index(output_path)

    // Serialize joined information for opadoc-launch
    do
      if false then // TODO: add a optional command line
        do jlog("opadoc.apix file serialization")
        do JsonFile.to_file("opadoc.apix", mj)
        void

    jlog("opadoc: ByeBye")

_ =
  /*
   * Command-line parameters
   */

  do OpaDocParameters.filter_command_line()
  params = OpaDocParameters.get()

  if params.help || List.is_empty(params.files) then
    do jlog("Usage: {CommandLine.executable()} [options] <files,dirs...>")
    jlog("Run opadoc with the --help argument to list available options.")
  else
    main_gendoc(params.output_dir, params.files)
