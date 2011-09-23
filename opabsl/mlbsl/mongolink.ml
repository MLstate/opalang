
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

##register escaped: string -> string
let escaped = String.escaped

