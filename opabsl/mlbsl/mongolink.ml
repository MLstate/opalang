
module String = Base.String
let sprintf = Printf.sprintf

##register path_to_bson: Path.t('a,'b) -> Bson.document
let path_to_bson = function
  | { Path.path; reader=_; kind=_; } ->
      let rec aux = function
        | [] -> BslMongo.Bson.shared_nil
        | (Badop.Key.IntKey i)::rest -> let e = BslMongo.Bson.make_int32 "IntKey" i in auxn e rest
        | (Badop.Key.StringKey i)::rest -> let e = BslMongo.Bson.make_string "StringKey" i in auxn e rest
        | _ -> assert false
      and auxn e rest = BslMongo.Bson.make_cons (BslMongo.wrap_opa_bson_document e) (aux rest)
      in
      BslMongo.wrap_opa_bson_document (aux (Badop.Path.to_list path))

##register path_to_string: Path.t('a,'b) -> string
let path_to_string p =
  match p with
  | { Path.path; reader=_; kind=_; } ->
      Badop.Path.to_string path

let rec string_of_key = function
  | Badop.Key.IntKey i -> string_of_int i
  | Badop.Key.StringKey s -> "\""^s^"\""
  | Badop.Key.ListKey r -> String.concat_map ~left:"<" ~right:">" "_" string_of_key (Array.to_list r)
  | Badop.Key.VariableKey i -> "V"^string_of_int i

let string_of_key_list kl = String.concat_map ~left:"_" "_" string_of_key kl

##register path_to_mongo: Path.t('a,'b) -> opa[tuple_3(string, string, string)]
let path_to_mongo = function
  | { Path.path; reader=_; kind=_; } ->
      let db, collection, key =
        (match Badop.Path.to_list path with
         | [] -> assert false
         | [k] -> (string_of_key_list [k], "ollection", "key")
         | [k1; k2] -> (string_of_key_list [k1;k2], "ollection", "key")
         | [k1; k2; k3] -> (string_of_key_list [k1;k2], string_of_key_list [k3], "key")
         | (k1::k2::k3::rest) -> (string_of_key_list [k1;k2], string_of_key_list [k3], string_of_key_list rest))
      in
      BslNativeLib.opa_tuple_3 (ServerLib.wrap_string db, ServerLib.wrap_string collection, ServerLib.wrap_string key)

##register path_length: Path.t('a,'b) -> opa[int]
let path_length = function
  | { Path.path; reader=_; kind=_; } ->
      ServerLib.wrap_int(List.length(Badop.Path.to_list path))

##opa-type MongoDb.key

let field_intkey    = ServerLib.static_field_of_name "IntKey"
let field_stringkey = ServerLib.static_field_of_name "StringKey"
let make_val fld x = ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor fld x)
let make_intkey = make_val field_intkey
let make_stringkey = make_val field_stringkey

##register get_path: Path.t('a,'b) -> opa[list(MongoDb.key)]
let get_path = function
  | { Path.path; reader=_; kind=_; } ->
      let l = List.map (function
                        | Badop.Key.IntKey i -> make_intkey i
                        | Badop.Key.StringKey s -> make_stringkey s
                        | Badop.Key.ListKey _ -> assert false
                        | Badop.Key.VariableKey _ -> assert false) (Badop.Path.to_list path)
      in
      let l = List.fold_right (fun k l -> BslMongo.Bson.make_cons (wrap_opa_mongodb_key k) l) l BslMongo.Bson.shared_nil
      in
      BslNativeLib.wrap_opa_list l

##register escaped: string -> string
let escaped = String.escaped

