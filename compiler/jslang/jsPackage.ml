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

module Format = BaseFormat
module List = BaseList

module J = JsonTypes

module JS = JsAst

type elt =
  | Verbatim of string
  | Code of JsAst.code

type t = {
  name : string;
  main : string;
  build_dir : string;
  version : string;
  dependencies: string StringMap.t;
  more : (string * J.json) list;
  files : (string * string) list;
  code : elt list;
  perm : Unix.file_perm;
}

let native_package = StringSet.from_list [
  "assert";
  "buffer_ieee754";
  "buffer";
  "child_process";
  "cluster";
  "console";
  "constants";
  "crypto";
  "_debugger";
  "dgram";
  "dns";
  "domain";
  "events";
  "freelist";
  "fs";
  "http";
  "https";
  "_linklist";
  "module";
  "net";
  "os";
  "path";
  "punycode";
  "querystring";
  "readline";
  "repl";
  "stream";
  "string_decoder";
  "sys";
  "timers";
  "tls";
  "tty";
  "url";
  "util";
  "vm";
  "zlib";
]

let package_json = "package.json"

let default ~name = {
  name;
  main = "main.js";
  build_dir = ".";
  version = BuildInfos.opa_version_name;
  dependencies = StringMap.empty;
  more = [];
  files = [];
  perm=0o644;
  code=[];
}

let set_main t main = { t with main }

let set_perm t perm = { t with perm }

let set_build_dir t build_dir = { t with build_dir }

let set_version t version = { t with version }

let add_dependencies t d = { t with dependencies =
    List.fold_left (fun dependencies (k,v) -> StringMap.add k v dependencies) t.dependencies d }

let add_file t file = { t with files = file::t.files }

let add_verbatim t verbatim = {t with code = Verbatim verbatim :: t.code }

let add_code t code = {t with code = Code code :: t.code }

let map f t = {t with code = List.map f t.code}

let foldr f acc t = List.fold_left f acc t.code

let fold f acc t = List.fold_left f acc (List.rev t.code)

let is_empty t = List.is_empty t.code

let get_code t =
  foldr
    (fun code -> function
     | Code c -> c @ code
     | Verbatim _ -> code
    ) [] t

let merge t1 t2 =
  { t2 with
      dependencies = StringMap.merge (fun x _ -> x) t1.dependencies t2.dependencies;
      code = t1.code @ t2.code
  }

let auto_dependencies ?(miss=fun _ -> ()) t =
  let dependencies =
    List.fold_left
      (fun dependencies -> function
       | Verbatim _ -> dependencies
       | Code code ->
           List.fold_left
             (fun dependencies stm ->
                JsWalk.ExprInStatement.fold
                  (fun dependencies -> function
                   | JS.Je_call (_, JS.Je_ident (_, JS.Native (`global _, "require")),
                                 [JS.Je_string (_, pack, _)], _) ->
                       if StringMap.mem pack dependencies || StringSet.mem pack native_package
                       then dependencies
                       else (
                         miss pack;
                         StringMap.add pack "*" dependencies
                       )
                   | _ -> dependencies
                  ) dependencies stm
             ) dependencies code
      ) t.dependencies t.code
  in {t with dependencies}

let to_json {name; version; main; dependencies; more} =
  J.Record
    (more @ [
       "name", J.String name;
       "version", J.String version;
       "main", J.String main;
       "dependencies", J.Record
         (StringMap.fold (fun k v acc -> (k, J.String v) ::acc) dependencies []);
     ])

let pp_json fmt t = JsonPrint.pp fmt (to_json t)

let pp_code0 fmt = function
  | Verbatim v -> Format.pp_print_string fmt v
  | Code c -> JsPrint.pp#code fmt c

let pp_code fmt t =
  List.iter_right (pp_code0 fmt) t.code

let write t =
  let build_dir = t.build_dir in
  let json = Filename.concat build_dir package_json in
  let main = Filename.concat build_dir t.main in
  if not (File.check_create_path main) then
    OManager.error "Cannot create directory @{<bright>%s@}" build_dir;
  let output filename pp x =
    match File.pp_output filename pp x with
    | None -> ()
    | Some msg ->
        OManager.error "Could not create package @{<bright>%s}: %s" t.name msg
  in
  List.iter (fun (name, content) -> output
               (Filename.concat build_dir name) Format.pp_print_string content)
    t.files;
  output json pp_json t;
  output main pp_code t;
  Unix.chmod main t.perm

