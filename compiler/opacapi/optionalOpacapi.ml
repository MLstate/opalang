(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

let table = Hashtbl.create 128
let (!!) s =
  if Hashtbl.mem table s then (prerr_endline s; assert false);
  Hashtbl.add table s s;
  s

module Db3 =
struct
  let (!!) s = !! ("Db3_" ^ s)
  let val_to_val = !! "val_to_val"
  let ref_to_ref = !! "ref_to_ref"
end

module Db3Set =
struct
  let (!!) s = !! ("Db3Set_" ^ s)
  let iterator = !! "iterator"
end

module DbVirtual =
struct
  let (!!) s = !! ("DbVirtual_" ^ s)
  let hack_coerce_default = !! "hack_coerce_default"
  let hack_coerce_option = !! "hack_coerce_option"
  let hack_coerce_vvpath = !! "hack_coerce_vvpath"
  let hack_coerce_vrpath = !! "hack_coerce_vrpath"
  let make_ref = !! "make_ref"
  let make_val = !! "make_val"
end

(**
   Types definitions
*)
module Types =
struct

  let badop_engine_database_options = !! "badop_engine_database_options"
  let badoplink_data_d = !! "badoplink_data_d"
  let badoplink_database = !! "badoplink_database"
  let badoplink_db_partial_key = !! "badoplink_db_partial_key"
  let badoplink_db_path_key = !! "badoplink_db_path_key"
  let badop_engine_t = !! "badop_engine_t"
  let badoplink_node_config = !! "badoplink_node_config"
  let badoplink_path = !! "badoplink_path"
  let badoplink_transaction = !! "badoplink_transaction"

  let dbgraph_diff = !! "dbgraph_diff"
  let db3set = !! "Db3Set.t"

  let path_embed_info = !! "path_embed_info"
  let path_embedded_obj = !! "path_embedded_obj"
  let path_ref_p = !! "path_ref_p"
  let path_t = !! "path_t"
  let path_val_p = !! "path_val_p"
  let val_path = !! "val_path"
  let ref_path = !! "ref_path"

  let transactions_t = !! "opa_transaction_t"
  let virtual_ref_path = !! "virtual_ref_path"
  let virtual_val_path = !! "virtual_val_path"

  module Db3 =
  struct
    let (!!) s = !! ("Db3." ^ s)
    let t  = !! "t"
  end

  module Db3Set =
  struct
    let (!!) s = !! ("Db3Set." ^ s)
    let engine  = !! "engine"
  end

end

(**
   Bypass inserted by the compiler
*)
module Opabsl =
struct

  let table = Hashtbl.create 128

  let (!!) s =
    let s = BslKey.normalize s in
    if Hashtbl.mem table s then (prerr_endline (BslKey.to_string s); assert false);
    Hashtbl.add table s s;
    s

  module BslClosure =
  struct
    let (!!) s = !! ("BslClosure." ^ s)
    let create_and_register = !! "create_and_register"
    let create_no_function_and_register = !! "create_no_function_and_register"
    let define_function = !! "define_function"
  end

  module BslCps =
  struct

    let (!!) s = !! ("BslCps." ^ s)
    let bt_add = !! "bt_add"

  end

  module Badoplink =
  struct
    let (!!) s = !! ("Badoplink." ^ s)
    let add_hole = !! "add_hole"
    let add_key = !! "add_key"
    let clear = !! "clear"
    let create_dbset = !! "create_dbset"
    let data_binary = !! "data_binary"
    let data_float = !! "data_float"
    let data_int = !! "data_int"
    let data_obj_binary = !! "data_obj_binary"
    let data_obj_float = !! "data_obj_float"
    let data_obj_int = !! "data_obj_int"
    let data_obj_text = !! "data_obj_text"
    let data_text = !! "data_text"
    let data_unit = !! "data_unit"
    let db_prefix = !! "db_prefix"
    let dbpath_add = !! "dbpath_add"
    let dbpath_root = !! "dbpath_root"
    let empty_partial_key = !! "empty_partial_key"
    let error = !! "error"
    let exists = !! "exists"
    let fatal_error = !! "fatal_error"
    let fold_children = !! "fold_children"
    let fold_int_keys = !! "fold_int_keys"
    let fold_string_keys = !! "fold_string_keys"
    let get_new_key = !! "get_new_key"
    let get_opt = !! "get_opt"
    let get_registered_db_ident = !! "get_registered_db_ident"
    let get_registered_root_edge = !! "get_registered_root_edge"
    let is_db_new = !! "is_db_new"
    let jlog = !! "jlog"
    let key_int = !! "key_int"
    let key_list = !! "key_list"
    let key_string = !! "key_string"
    let key_value_int = !! "key_value_int"
    let key_value_string = !! "key_value_string"
    let node_config_construct = !! "node_config_construct"
    let node_properties = !! "node_properties"
    let open_db = !! "open_db"
    let register_db_ident = !! "register_db_ident"
    let register_root_edge = !! "register_root_edge"
    let remove_children = !! "remove_children"
    let set = !! "set"
    let set_current_copy = !! "set_current_copy"
    let set_dbset_keys = !! "set_dbset_keys"
    let set_link = !! "set_link"
    let shall_i_upgrade = !! "shall_i_upgrade"
    let trans_abort = !! "trans_abort"
    let trans_commit = !! "trans_commit"
    let trans_start = !! "trans_start"
    let uppath = !! "uppath"
  end

  module BadopEngine =
  struct
    let (!!) s = !! ("Badop_engine." ^ s)
    let check_remaining_arguments = !! "check_remaining_arguments"
    let local_options = !! "local_options"
    let light_options = !! "light_options"
    let client_options = !! "client_options"
    let get = !! "get"
  end

  module Dbgraph =
  struct
    let (!!) s = !! ("Dbgraph." ^ s)
    let diff = !! "diff"
    let diff_message = !! "diff_message"
    let diff_status = !! "diff_status"
    let empty_diff = !! "empty_diff"
    let get_diffed_schema = !! "get_diffed_schema"
    let matching_edge = !! "matching_edge"
    let print_tree = !! "print_tree"
  end

  module Path =
  struct
    let (!!) s = !! ("Path." ^ s)
    let copy = !! "copy"
    let embed_record_data = !! "embed_record_data"
    let embedded_path = !! "embedded_path"
    let get_lazy_info_opt = !! "get_lazy_info_opt"
    let get_ref_path = !! "get_ref_path"
    let get_val_path = !! "get_val_path"
    let inject_record_data = !! "inject_record_data"
  end

  module Transactions =
  struct
    let (!!) s = !! ("Opa_transaction." ^ s)
    let commit = !! "commit"
    let continue = !! "continue"
    let fail = !! "fail"
    let get_global_transaction_opt = !! "get_global_transaction_opt"
    let set_global_transaction = !! "set_global_transaction"
    let start = !! "start"
  end

end
