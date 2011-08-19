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
 * jsTree element management
 */

/**
 * OPA's jsTree collection of children
 */

type OpaDocTree.order = Order.default

type OpaDocTree.t = ordered_map(OpaDocTree.node_kind, OpaDocTree.node, OpaDocTree.order)

type OpaDocTree.node_kind =
    { package_ : string }
  / { module   : string }
  / { type_    : string }
  / { value    : string }
  / { file     : string }

/**
 * A node of an OPA's jsTree
 */
type OpaDocTree.node = {
  id    : string
  kind  : OpaDocTree.node_kind
  href  : string
  title : string
  tree  : option(OpaDocTree.t)
}

/**
 * A configuration of an OPA's jsTree
 */
type OpaDocTree.config = {
  html_titles    : bool
  animation      : int
  initially_open : list(string)
  initially_load : list(string)
  load_open      : bool
  open_parents   : bool
  notify_plugins : bool
  rtl            : bool
  strings        : { loading : string; new_node : string }
}

/**
 * An OPA's jsTree plugin
 */
// TODO: implement plugin options
type OpaDocTree.plugin =
    { cookies }
  / { json_data: {
        data: option(RPC.Json.json)
      }}
  / { search }
  / { themes: {
        theme : string
        url   : option(string)
        dots  : bool
        icons : bool
      }}
  / { types: RPC.Json.js_code }
  / { ui }
  / { hotkeys }

/*OpaDocTreeMap = Map_make(OpaDocTree.order): ordered_map(string, OpaDocTree.node, OpaDocTree.order)*/

OpaDocTree = {{

  default_config = {
    html_titles    = false
    animation      = 500
    initially_open = []
    initially_load = []
    load_open      = false
    open_parents   = true
    notify_plugins = true
    rtl            = false
    strings        = {loading = "Loading..."; new_node = "New node"}
  }

  default_plugins = [
    {json_data = {data = {none}}},
    {themes = {theme = "classic"; url = {none}; dots = true; icons = true}},
    {cookies},
    {search},
    {types = {Record = [("max_children", {Direct = "-1"}),
      ("max_depth", {Direct = "-1"}), ("valid_children", {Direct = "all"})]}},
    {ui},
    {hotkeys}
  ]

  /**
   * Node order
   */

  compare_kind(k1: OpaDocTree.node_kind, k2: OpaDocTree.node_kind)
      : Order.ordering =
    match (k1, k2) with
      | ({package_ = _} , {module = _})
      | ({package_ = _} , {type_ = _})
      | ({package_ = _} , {value = _})
      | ({package_ = _} , {file = _})
      | ({module = _}   , {type_ = _})
      | ({module = _}   , {value = _})
      | ({module = _}   , {file = _})
      | ({type_ = _}    , {value = _})
      | ({type_ = _}    , {file = _})
      | ({value = _}    , {file = _})
        -> {gt}
      | ({package_ = lbl1} , {package_ = lbl2})
      | ({module = lbl1}   , {module = lbl2})
      | ({type_ = lbl1}    , {type_ = lbl2})
      | ({value = lbl1}    , {value = lbl2})
      | ({file = lbl1}     , {file = lbl2})
        -> String.ordering(lbl1, lbl2)
      | (_, _) -> {lt}

  tree_order = Order.make(compare_kind): order(OpaDocTree.node_kind, OpaDocTree.order)
  /*tree_order = Order.make_by(label_of_kind, String.order): order(OpaDocTree.node_kind, OpaDocTree.order)*/

  /**
   * Turn the given ID into a jsTree, loading the listed plugins
   */
  make(_config: OpaDocTree.config, id: string,
      plugins: list(OpaDocTree.plugin), tab: string)
      : string =
    config_str = "\{animation: 0\}"
    /*config_str =*/
    /*  String.replace("; ", ", ", "{config}")*/
    /*    |> String.replace(" = ", ": ", _)*/
    plugins_str =
      List.map(string_of_plugin, plugins)
        |> String.concat("', '", _)
    "make_tree({config_str}, '{id}', ['{plugins_str}'], '{tab}');"

  /**
   * Convert API entries to a tree
   */
  of_packages(entries) =
    List.fold(of_package, entries, OpaDocTree.Map.empty)

  @private
  make_path(path_list: list(string)): string =
    path_list = List.filter((p -> if p == "" then false else true), path_list)
    String.concat(".", path_list)

  @private
  get_child(label: OpaDocTree.node_kind, tree: OpaDocTree.t): OpaDocTree.t =
    match OpaDocTree.Map.get(label, tree) with
      | {none} -> OpaDocTree.Map.empty
      | {some = node} -> node.tree ? OpaDocTree.Map.empty

  /*
   * Explode entries containing periods to fake inclusion in modules.
   * @return The entry name and its normalized path
   */
  @private
  normalize_path(orig_path: list(string)): (string, list(string)) =
    (entry_name_opt, remainder) = List.rev(orig_path)
      |> List.extract(0, _)
    entry_name = entry_name_opt ? ""
    /* Explode the entry name to fake types being in modules */
    path = List.rev(remainder) ++ String.explode(".", entry_name)
    real_name = (List.rev(path)
      |> List.get(0, _)) ? ""
    (real_name, path)

  @private
  of_package((path_name, (entry : Api.entry, path_html, name)), acc)
      : OpaDocTree.t =
    (entry_name, path) = normalize_path(entry.path)
    code_elt_str = OpaDocXhtml.string_of_code_elt(entry.code_elt)
    leaf = {
      tree  = none
      id    = "node_{code_elt_str}_{make_path([entry.pkg, path_name])}"
      kind  = kind_of_code_elt(entry_name, entry.code_elt)
      href  = "{name}.html#{code_elt_str}_{path_html}"
      title = "{code_elt_str}_{make_path([entry.pkg, path_name])}"
      }
    key = {package_ = entry.pkg}
    prev_val = get_child(key, acc)
    pkg_root = of_path(leaf, prev_val, name, entry.pkg, "", path)
    OpaDocTree.Map.add(key, {pkg_root with kind = key}, acc)

  /*@private*/
  /*key_equals(key1: OpaDocTree.node_kind, key2: OpaDocTree.node_kind): bool =*/
  /*  if compare_kind(key1, key2) == {eq} then*/
  /*    Order.equals(label_of_kind(key1), label_of_kind(key2), String.order_ci)*/
  /*  else*/
  /*    false*/

  @private
  rec of_path(leaf, tree: OpaDocTree.t, filename, pkg, path_name: string, path: list(string))
      : OpaDocTree.node =
    match path with
    | [] -> leaf
    | [hd|tl] ->
      key = {module = hd}
      /* If a value with the same name (case insensitive) exists, suffix it 
       * with '_bis' */
      /*(hd, path_name) =*/
      /*  if OpaDocTree.Map.exists((k, _ -> key_equals(k, key)), tree) then*/
      /*    ("{hd}_bis", "{path_name}_bis")*/
      /*  else*/
      /*    (hd, path_name)*/
      prev_val = get_child(key, tree)
      parent_name = "{make_path([path_name, hd])}"
      full_path = "value_{make_path([pkg, path_name])}"
      /*full_path = make_path([pkg, path_name])*/
      {
        id = "node_{full_path}"
        kind = key
        title = full_path
        href  = "{filename}.html#{full_path}"
        tree = some(OpaDocTree.Map.add(key, of_path(leaf, prev_val, filename, pkg, parent_name, tl),
            tree))
       }

  /**
   * Convert API value / type entries to a tree
   */

  of_values(entries) =
    of_entries((e -> match e.code_elt with
        | {value = _} -> true
        | {type_def = _} -> false)
      , entries)

  of_types(entries) =
    of_entries((e -> match e.code_elt with {value = _} -> false | {type_def = _} -> true), entries)

  @private
  of_entries(filter_entry, entries) =
    List.filter(((_, (entry, _, _)) -> filter_entry(entry)), entries)
      |> List.fold(of_entry, _, OpaDocTree.Map.empty)

  @private
  kind_of_code_elt(label: string, code_elt: Api.code_elt): OpaDocTree.node_kind =
    match code_elt with
      | {value = _}    -> { value = label }
      | {type_def = _} -> { type_ = label }

  @private
  of_entry((path_name, (entry : Api.entry, path_html, name)), acc)
      : OpaDocTree.t =
    (entry_name, path) = normalize_path(entry.path)
    code_elt_str = OpaDocXhtml.string_of_code_elt(entry.code_elt)
    full_path = "{code_elt_str}_{make_path([entry.pkg, path_name])}"
    leaf = {
      tree  = none
      id    = "node_{full_path}"
      kind  = kind_of_code_elt(entry_name, entry.code_elt)
      href  = "{name}.html#{code_elt_str}_{path_html}"
      title = full_path
    }
    root = of_path(leaf, acc, name, entry.pkg, "", path)
    root.tree ? acc

  /**
   * Convert a file list to a tree
   */

  of_files(files) =
    aux(acc, path) =
      key  = {file = path}
      path_dot = String.replace(File.dir_sep, ".", path)
      node = {
        tree  = none
        id    = "node_file_{path_dot}"
        kind  = key
        href  = "{path_dot}.html"
        title = "{path}"
      }
      OpaDocTree.Map.add(key, node, acc)
    List.fold_left(aux, OpaDocTree.Map.empty, files)

  /**
   * Convert a tree to XHTML
   */
  to_xhtml(tree: OpaDocTree.t): xhtml =
    <ul>
      {OpaDocTree.Map.mapi(xhtml_of_node, tree)
        |> OpaDocTree.Map.To.val_list(_)}
    </ul>

  /**
   * Convert a tree to JSON
   */
  to_json(tree: OpaDocTree.t) : RPC.Json.json = {
    List = (
      OpaDocTree.Map.mapi(json_of_node, tree)
      |> OpaDocTree.Map.To.val_list(_)
    )
  }:RPC.Json.json

  @private
  string_of_kind: OpaDocTree.node_kind -> string =
    | {package_ = _} -> "package"
    | {module   = _} -> "module"
    | {type_    = _} -> "type"
    | {value    = _} -> "value"
    | {file     = _} -> "file"

  @private
  label_of_kind: OpaDocTree.node_kind -> string =
    | {package_ = lbl}
    | {module   = lbl}
    | {type_    = lbl}
    | {value    = lbl}
      -> lbl
    | {file     = lbl}
      -> File.basename(lbl)

  @private
  xhtml_of_node(label: OpaDocTree.node_kind, node: OpaDocTree.node): xhtml =
    kind = string_of_kind(label)
    title = node.title
    mkli(x) = (<li id="{node.id}" rel="{kind}" class="{kind}_node">{x}</li>)
    <>
      <a title="{title}" href="{node.href}">{label}</a>
      {match node.tree with
        | {none} -> <></>
        | {some=tree} -> to_xhtml(tree)}
    </>
      |> mkli(_)

  @private
  json_of_node(label: OpaDocTree.node_kind, node: OpaDocTree.node)
      : RPC.Json.json =
    json_children =
      match node.tree with
      | {none} -> []
      | {some=tree} -> [("children", to_json(tree):RPC.Json.json)]
      end
    { Record = [
        ("data", { Record = [
          ("title", { String = label_of_kind(label) }:RPC.Json.json),
          ("attr", { Record = [
            ("title", { String = node.title }:RPC.Json.json),
            ("href", { String = node.href }:RPC.Json.json)
          ] }:RPC.Json.json)
        ]}:RPC.Json.json),
        ("attr", { Record = [
          ("id", { String = node.id }:RPC.Json.json),
          ("rel", { String = string_of_kind(node.kind) }:RPC.Json.json),
        ] }:RPC.Json.json)
        ] ++ json_children
    }

  @private
  string_of_plugin: OpaDocTree.plugin -> string =
    | { cookies=_ }   -> "cookies"
    | { json_data=_ } -> "json_data"
    | { themes=_ }    -> "themes"
    | { types=_ }     -> "types"
    | { search=_ }    -> "search"
    | { ui=_ }        -> "ui"
    | { hotkeys=_ }   -> "hotkeys"

  Map = Map_make(tree_order): Map(OpaDocTree.node_kind, OpaDocTree.order)

}}
