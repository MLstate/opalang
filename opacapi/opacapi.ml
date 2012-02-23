(*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
(**
   Interface between the compiler and the standard library.
   Contain identifier and bslkeys of all opa functions inserted by the compiler.
*)

(**
   The hierachy of ocaml values follows the module hierarchy of opa values.
   The hierarchy must be respected.
   All ident content are opa pathes where dot has been replaced by '_'.
   Define ONLY CONSTANT STRING

   A check (cf checkopacapi.ml), called during mkinstall check that:
   - there is a strict equality between bypasses marked as [[opacapi]]
   in the bsl and bypass available there,
   - there is a strict equality between identifiers marked as [\@opacapi]
   in the stdlib and identifier available there.
*)

let table = Hashtbl.create 128
let (!!) s =
  if Hashtbl.mem table s then (prerr_endline s; assert false);
  Hashtbl.add table s s;
  s

(* toplevel *)
let (==) = !! "=="
let (!=) = !! "!="
let identity = !! "identity"
let internal__add_css_entry = !! "__internal__add_css_entry"
let magicToString = !! "magicToString"
let magicToXml = !! "magicToXml"
let never_do_anything = !! "never_do_anything"
let none = !! "none"
let callFA = !! "callFA"
let dom_event_to_opa_event = !! "dom_event_to_opa_event"
let some = !! "some"
let unary_minus = !! "unary_minus"
let unary_minus_dot = !! "unary_minus_dot"

module Client_code =
struct
  let (!!) s = !! ("Client_code_" ^ s)
  let register_css_declaration = !! "register_css_declaration"
  let register_js_code = !! "register_js_code"
  let register_js_code_elt = !! "register_js_code_elt"
end

module Db3 =
struct
  let (!!) s = !! ("Db3_" ^ s)
  let val_to_val = !! "val_to_val"
  let ref_to_ref = !! "ref_to_ref"
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

module Dom =
struct
  let (!!) s = !! ("Dom_" ^ s)
  let select_class = !! "select_class"
  let select_id = !! "select_id"
  let transform = !! "transform"
end

module Core_server_code =
struct
  let (!!) s = !! ("Core_server_code_" ^ s)
  let register_server_code = !! "register_server_code"
end

module FiniteSingleThreadLazy =
struct
  let (!!) s = !! ("FiniteSingleThreadLazy_" ^ s)
  let force = !! "force"
end

module FunActionServer =
struct
  let (!!) s = !! ("FunActionServer_" ^ s)
  let serialize_argument = !! "serialize_argument"
  let serialize_call = !! "serialize_call"
  let serialize_ty_argument = !! "serialize_ty_argument"
end

module I18n =
struct
  let (!!) s = !! ("I18n_" ^ s)
  let lang = !! "lang"
end

module IntMap =
struct
  let (!!) s = !! ("IntMap_" ^ s)
  let add = !! "add"
  let empty = !! "empty"
  let fold = !! "fold"
end

module List =
struct
  let (!!) s = !! ("List_"^s)
  let split_at_opt = !! "split_at_opt"
  let split_between = !! "split_between"
end

module Mutable =
struct
  let (!!) s = !! ("Mutable_" ^ s)
  let make = !! "make"
  let set = !! "set"
end

module Db =
struct
  let (!!) s = !! ("Db_" ^ s)
  let write = !! "write"
end

module Db3Set =
struct
  let (!!) s = !! ("Db3Set_" ^ s)
  let iterator = !! "iterator"
end

module DbMongo =
struct
  let (!!) s = !! ("DbMongo_" ^ s)
  let open_ = !! "open"
  let path_to_path = !! "path_to_path"
  let build_vpath = !! "build_vpath"
  let build_rpath = !! "build_rpath"
  let update_path = !! "update_path"
  let build_vpath_sub = !! "build_vpath_sub"
  let build_rpath_sub = !! "build_rpath_sub"
  let build_rpath_compose = !! "build_rpath_compose"
  let build_vpath_compose = !! "build_vpath_compose"
  let option = !! "option"
  let read = !! "read"
  let write = !! "write"
end

module DbSet =
struct
  let (!!) s = !! ("DbSet_" ^ s)
  let build = !! "build"
  let genbuild = !! "genbuild"
  let update = !! "update"
  let empty = !! "empty"
  let opa2doc = !! "opa2doc"
  let add_to_document = !! "add_to_document"
  let indexes = !! "indexes"
  let to_map = !! "to_map"
  let iterator = !! "iterator"
  let map_to_uniq = !! "map_to_uniq"
  let set_to_uniq = !! "set_to_uniq"
  let map_to_uniq_def = !! "map_to_uniq_def"
  let set_to_uniq_def = !! "set_to_uniq_def"
  let build_vpath = !! "build_vpath"
  let build_rpath = !! "build_rpath"
  let default = !! "default"
end

module Opa2Js =
struct
  let (!!) s = !! ("Opa2Js_" ^ s)
  let to_string = !! "to_string"
end

module OpaRPC =
struct
  let (!!) s = !! ("OpaRPC_" ^ s)
  let add_args_with_type = !! "add_args_with_type"
  let add_var_types = !! "add_var_types"
  let add_row_types = !! "add_row_types"
  let add_col_types = !! "add_col_types"
  let client_async_send_to_server = !! "Client_async_send_to_server"
  let client_dispatcher_register = !! "Client_Dispatcher_register"
  let client_send_to_server = !! "Client_send_to_server"
  let client_try_cache = !! "Client_try_cache"
  let empty_request = !! "empty_request"
  let error_stub = !! "error_stub"
  let extract_types = !! "extract_types"
  let extract_values = !! "extract_values"
  let fake_stub = !! "fake_stub"
  let serialize = !! "serialize"
  let server_async_send_to_client = !! "Server_async_send_to_client"
  let server_dispatcher_register = !! "Server_Dispatcher_register"
  let server_send_to_client = !! "Server_send_to_client"
  let server_try_cache = !! "Server_try_cache"
  let unserialize = !! "unserialize"
end

module OpaSerialize =
struct
  let (!!) s = !! ("OpaSerialize_" ^ s)
  let serialize = !! "serialize"
  let serialize_for_js = !! "serialize_for_js"
  let unserialize = !! "unserialize"
  let unserialize_ty = !! "unserialize_ty"
  let unserialize_unsafe = !! "unserialize_unsafe"
end

module OpaTsc =
struct
  let (!!) s = !! ("OpaTsc_" ^ s)
  let implementation = !! "implementation"
end

module OpaType =
struct
  let (!!) s = !! ("OpaType_" ^ s)
  let instantiate_row = !! "instantiate_row"
  let instantiate_col = !! "instantiate_col"
end

module OpaValue =
struct
  let (!!) s = !! ("OpaValue_" ^ s)
  let add_compare = !! "add_compare"
  let add_serializer = !! "add_serializer"
  let add_to_string = !! "add_to_string"
  let add_xmlizer = !! "add_xmlizer"
end

module Parser =
struct
  let (!!) s = !! ("Parser_" ^ s)
  let of_string = !! "of_string"
end

module Resource_private =
struct
  let (!!) s = !! ("Resource_private_"^s)
  let content_of_include = !!"content_of_include"
  let make_include = !! "make_include"
  let make_resource_include = !! "make_resource_include"
  let raw_resource_factory  = !! "raw_resource_factory"
end

module Scheduler =
struct
  let (!!) s = !! ("Scheduler_" ^ s)
  let push = !! "push"
end

module Server_private =
struct
  let (!!) s = !! ("Server_private_" ^ s)
  let add_service = !! "add_service"
  let run_services = !! "run_services"
end

module String =
struct
  let (!!) s = !! ("String_" ^ s)
  let flatten = !! "flatten"
end

module StringMap =
struct
  let (!!) s = !! ("StringMap_" ^ s)
  let add = !! "add"
  let empty = !! "empty"
  let fold = !! "fold"
end

module ThreadContext =
struct
  let (!!) s = !! ("ThreadContext_" ^ s)
  let no_client_calls = !! "no_client_calls"
end

module Xml =
struct
  let (!!) s = !! ("Xml_" ^ s)
  let find_attr = !! "find_attr"
  let match_number = !! "match_number"
  let match_plus = !! "match_plus"
  let match_question = !! "match_question"
  let match_range = !! "match_range"
  let match_star = !! "match_star"
end

(**
   Types definitions
*)
module Types =
struct

  let bool = !! "bool"

  let badop_engine_database_options = !! "badop_engine_database_options"
  let badoplink_data_d = !! "badoplink_data_d"
  let badoplink_database = !! "badoplink_database"
  let badoplink_db_partial_key = !! "badoplink_db_partial_key"
  let badoplink_db_path_key = !! "badoplink_db_path_key"
  let badop_engine_t = !! "badop_engine_t"
  let badoplink_node_config = !! "badoplink_node_config"
  let badoplink_path = !! "badoplink_path"
  let badoplink_transaction = !! "badoplink_transaction"

  let binary = !! "binary"
  let caml_list = !! "caml_list"
  let char = !! "char"
  let continuation = !! "continuation"
  let dbgraph_diff = !! "dbgraph_diff"
  let dbset = !! "dbset"
  let db3set = !! "Db3Set.t"
  let dbmongoset = !! "DbMongoSet.t"
  let dom = !! "dom"
  let finite_single_thread_lazy = !! "finite_single_thread_lazy"
  let float = !! "float"
  let handle_assoc = !! "handle_assoc"
  let event_handler = !! "event_handler"
  let int = !! "int"
  let ip = !! "ip"
  let itextrator = !! "itextrator"
  let list = !! "list"
  let llarray = !! "llarray"
  let map = !! "map"
  let option = !! "option"

  let path_embed_info = !! "path_embed_info"
  let path_embedded_obj = !! "path_embedded_obj"
  let path_ref_p = !! "path_ref_p"
  let path_t = !! "path_t"
  let path_val_p = !! "path_val_p"
  let val_path = !! "val_path"
  let ref_path = !! "ref_path"

  let string = !! "string"
  let stringmap = !! "stringmap"
  let text = !! "text"
  let iter = !! "iter"

  let transactions_t = !! "opa_transaction_t"
  let tuple_2 = !! "tuple_2"
  let virtual_ref_path = !! "virtual_ref_path"
  let virtual_val_path = !! "virtual_val_path"

  let void = !! "void"
  let xhtml = !! "xhtml"
  let xhtml_event = !! "xhtml_event"
  let xhtml_href   = !! "xhtml_href"
  let xml = !! "xml"

  (* module Bson = *)
  (* struct *)
  (*   let (!!) s = !! ("Bson." ^ s) *)
  (*   let document = !! "document" *)
  (* end *)

  module Db =
  struct
    let (!!) s = !! ("Db." ^ s)
    let ref_path = !! "ref_path"
    let val_path = !! "val_path"
  end

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

  module DbMongo =
  struct
    let (!!) s = !! ("DbMongo." ^ s)
    let t  = !! "t"
    let engine  = !! "engine"
    let val_path = !! "private.val_path"
    let ref_path = !! "private.ref_path"
  end

  module Cell =
  struct
    let (!!) s = !! ("Cell." ^ s)
    let timeout = !! "timeout"
  end

  module Css =
  struct
    let (!!) s= !! ("Css." ^ s)
    let background = !! "background"
    let event = !! "event"
    let length = !! "length"
    let percentage = !! "percentage"
    let prop_value_item = !! "prop_value_item"
    let selector_item = !! "selector_item"
    let size = !! "size"
    let size_or_none = !! "size_or_none"
    let size_or_normal = !! "size_or_normal"
  end

  module Cps =
  struct
    let (!!) s = !! ("Cps." ^ s)
    let future = !! "future"
  end

  module DbSet =
  struct
    let (!!) s = !! ("DbSet." ^ s)
    let query = !! "query"
  end

  module Deprecated =
  struct
    let (!!) s = !! ("Deprecated." ^ s)
    let argument = !! "argument"
  end

  module Dom =
  struct
    let (!!) s = !! ("Dom." ^ s)
    let transformation = !! "transformation"
    module Event =
    struct
      let (!!) s = !! ("event." ^ s)
      let kind = !! "kind"
    end
    module Transformation =
    struct
      let (!!) s = !! ("Transformation." ^ s)
      let subject = !! "subject"
    end
  end

  module Exception =
  struct
    let (!!) s = !! ("Exception." ^ s)
    let common = !! "common"
  end

  module FunAction =
  struct
        let (!!) s = ("FunAction." ^ s)
        let t = !! "t"
  end

  module OPA =
  struct
    let (!!) s = !! ("OPA." ^ s)
    module Init =
    struct
      let (!!) s = !! ("Init." ^ s)
      let value = !! "value"
    end
  end

  module OpaRPC =
  struct
    let (!!) s = !! ("OpaRPC." ^s)
    let request = !! "request"
    let timeout = !! "timeout"
  end

  module OpaSerialize =
  struct
    let (!!) s = !! ("OpaSerialize." ^s)
    let options = !! "options"
  end

  module OpaTsc =
  struct
    let (!!) s = !! ("OpaTsc." ^s)
    let t = !! "t"
  end

  module OpaType =
  struct
    let (!!) s = !! ("OpaType." ^s)
    let col = !! "col"
    let row = !! "row"
    let ty = !! "ty"
    let typevar = !! "typevar"
  end

  module Order =
  struct
    let (!!) s = !! ("Order." ^s)
    let comparison = !! "comparison"
  end

  module Parser =
  struct
    let (!!) s = !! ("Parser." ^ s)
    let general_parser = !! "general_parser"
  end

  module RPC =
  struct
    let (!!) s = !! ("RPC." ^s)
    module Json =
    struct
      let (!!) s = !! ("Json." ^ s)
      let json = !! "json"
    end
  end

  module ThreadContext =
  struct
    let (!!) s = !! ("ThreadContext." ^ s)
    let t = (!!) "t"
  end

  module Xml =
  struct
    let (!!) s = !! ("Xml." ^ s)
    let attribute = !! "attribute"
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

  module BslClientCode =
  struct
    let (!!) s = !! ("BslClientCode." ^ s)
    let serialize_string_length = !! "serialize_string_length"
  end

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
    let before_wait = !! "before_wait"
    let black_make_barrier = !! "black_make_barrier"
    let black_release_barrier = !! "black_release_barrier"
    let black_toplevel_wait = !! "black_toplevel_wait"
    let bt_add = !! "bt_add"
    let callcc_directive = !! "callcc_directive"
    let catch = !! "catch"
    let catch_native = !! "catch_native"
    let ccont = !! "ccont"
    let ccont_native = !! "ccont_native"
    let cont = !! "cont"
    let cont_native = !! "cont_native"
    let debug = !! "debug"
    let fun_args2string = !! "fun_args2string"
    let handler_cont = !! "handler_cont"
    let magic_func = !! "magic_func"
    let make_barrier = !! "make_barrier"
    let release_barrier = !! "release_barrier"
    let return = !! "return"
    let spawn = !! "spawn"
    let thread_context = !! "thread_context"
    let toplevel_wait = !! "toplevel_wait"
    let uncps_native = !! "uncps_native"
    let wait = !! "wait"
    let with_thread_context = !! "with_thread_context"
    module Notcps_compatibility =
    struct
      let (!!) s = !! ("Notcps_compatibility." ^ s)
      let thread_context = !! "thread_context"
      let dummy_cont = !! "dummy_cont"
      let max_cps_native = 5
      (* define an array of !! "cps%d_native" from min_cps_native to max_cps_native *)
      let cps_native =
        let cps_native_str arity = Printf.sprintf "cps%d_native" arity in
        let array = Array.init (max_cps_native+1) (fun i -> !! (cps_native_str i)) in
        fun arity -> array.(arity)
    end
  end

  module BslInit =
  struct
    let (!!) s = !! ("BslInit." ^ s)
    let set_executable_id = !! "set_executable_id"
  end

  module BslJsIdent =
  struct
    let (!!) s = !! ("BslJsIdent." ^ s)
    let define_rename = !! "define_rename"
    let set_cleaning_default_value = !! "set_cleaning_default_value"
  end

  module BslNativeLib =
  struct
    let (!!) s = !! ("BslNativeLib."^s)
    let cons = !! "cons"
    let empty_list = !! "empty_list"
  end

  module BslPervasives =
  struct
    let (!!) s = !! ("BslPervasives." ^ s)
    let compare_raw = !! "compare_raw"
    let fail = !! "fail"
    let fail_cps = !! "fail_cps"
    let return_exc = !! "return_exc"

    module Magic =
    struct
      let (!!) s = !! ("Magic." ^ s)
      let id = !! "id"
    end
  end

  module BslReference =
  struct
    let (!!) s = !! ("BslReference." ^ s)
    let create = !! "create"
  end

  module BslValue =
  struct
    let (!!) s = !! ("BslValue." ^ s)
    module Tsc =
    struct
      let (!!) s = !! ("Tsc." ^ s)
      let add = !! "add"
    end
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

  module BslAppSrcCode =
  struct
    let (!!) s = !! ("BslAppSrcCode." ^ s)
    let register_src_code = !! "register_src_code"
    let register_special_src_code = !! "register_special_src_code"
  end

end
