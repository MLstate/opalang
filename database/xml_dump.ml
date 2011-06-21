(*
    Copyright © 2011 MLstate

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
(*
    @author Louis Gesbert
**)
(* DB-serialisation: dump of the database, driven by the database schema --
   which is first read from the database itself. XML format is documented in the
   OPA stdlib, please keep up-to-date. *)

module D = Badop.Dialog
module G = Dbgraph

let xml_dump_version = "1.0"

(* This file follows the duck-style cps guidelines © *)

(* raises CharacterNull if character 0 encountered *)
exception CharacterNull
let escape_string ?(quote=true) s =
  let len =
    Base.String.fold
      (fun acc -> function
       | '&' -> acc + 5 (* &amp; *)
       | '<' | '>' -> acc + 4 (* &lt; &gt; *)
       | '"' when quote -> acc + 6 (* &quot; *)
       | '\000' -> raise CharacterNull (* Character 0 not allowed in XML strings *)
       | _ -> acc + 1)
      0 s
  in
  let s' = String.create len in
  let _len = Base.String.fold
    (fun pos -> function
     | '&' -> String.unsafe_blit "&amp;" 0 s' pos 5; pos + 5
     | '<' -> String.unsafe_blit "&lt;" 0 s' pos 4; pos + 4
     | '>' -> String.unsafe_blit "&gt;" 0 s' pos 4; pos + 4
     | '"' when quote -> String.unsafe_blit "&quot;" 0 s' pos 6; pos + 6
     | c -> String.unsafe_set s' pos c; pos + 1)
    0 s
  in s'

let rec key_to_xml = function
  | Badop.Key.IntKey i -> string_of_int i
  | Badop.Key.StringKey s ->
      (try Printf.sprintf "\"%s\"" (escape_string s) with
       | CharacterNull -> Printf.sprintf "<base64>%s</base64>" (Base.String.base64encode s))
  | Badop.Key.ListKey l -> Base.String.concat_map ~left:"{" ~right:"}" "," key_to_xml (Array.to_list l)
  | Badop.Key.VariableKey _ -> assert false

(* we take this as parameter so that the following functions can be used inside
   or outside of OPA, with or without a scheduler. It's tricky because it'll
   still be mixed with function-continuations from Badop. *)
module type SigCpsBackend = sig
  type 'a continuation
  val mkcont : 'b continuation -> ('a -> unit) -> 'a continuation
  val return : 'a -> 'a continuation -> unit
end

module F (C: SigCpsBackend) = struct

  (* our tiny CPS lib... *)
  let mkcont = C.mkcont
  let (@>) f k = f k
  let (|>) = C.return
  let rec fold_list f acc l k = match l with
    | [] -> acc |> k
    | hd::tl -> f acc hd @> mkcont k @> fun acc -> fold_list f acc tl @> k

  let print_tag f tag args contents k =
    Format.fprintf f "@[<hv><%s%s%s>@;<0 2>@[<hv>" tag (if args = "" then "" else " ") args;
    contents @> mkcont k
    @> fun _void_ ->
      Format.fprintf f "@]@,</%s>@]" tag;
      () |> k

  let path_schema = Badop.Path.of_list [ Badop.Key.IntKey 2; Badop.Key.IntKey 0 ]
  (* Path /2/0 to schema defined in dbGen_private.ml (/2 is config_keys, config key 0 is schema) *)
  let path_root = Badop.Path.of_list [ Badop.Key.IntKey 1 ]

  let to_format db_read f k =
    db_read path_schema (Badop.Contents (D.query ()))
    @> function
    | `Answer (Badop.Contents (D.Response (Badop.Data.Binary schema))) ->
        let tree = G.to_tree (G.import_schema schema) in
        let rec tree_node = function
          | G.Tlink id -> tree_node (G.find_id id tree)
          | G.Tnode (_id,node,edges) -> node, G.filter_dead edges
        in
        let rec tree_down key tree ~errk k =
          let _node,edges = tree_node tree in
          let find = List.fold_left
            (fun acc e -> match acc with Some _ as e -> e | None -> match e with
             | (G.Multi_edge _ | G.Hidden_edge), n -> Some n
             | G.SumCase id, n when Badop.Key.IntKey id = key -> Some n
             | G.Field (_str,id), n when Badop.Key.IntKey id = key -> Some n
             | _ -> None)
            None edges
          in
          match find with
          | None ->
              #<If:DEBUG_DB>
                Printf.eprintf "While dumping db, key %s not found in\n%s\n"
                (key_to_xml key)
                (G.print_tree ~color:true tree)
              #<End>;
              Logger.warning "Found key %s as a child of node %s, that doesn't match anything in the current database graph. This data has probably been made obsolete during a migration" (Keys.to_string key) (G.tnode_id tree);
              () |> errk
          | Some n -> n |> k
        in
        let rec aux tree f path k =
          match tree_node tree with
          | G.Multi, _ ->
              tree_down (Badop.Key.IntKey 0) tree ~errk:k @> mkcont k
              @> fun tree ->
              print_tag f "map" ""
                (fun k ->
                   db_read path (Badop.Children (D.query (None,0)))
                   @> function
                   | `Answer (Badop.Children (D.Response children)) ->
                       fold_list
                         (fun isfirst path k ->
                            if not isfirst then Format.pp_print_cut f ();
                            print_tag f "entry" ""
                              (fun k ->
                                 print_tag f "key" ""
                                   (fun k ->
                                      Format.fprintf f "%s" (key_to_xml (Badop.Path.last path)); () |> k)
                                 @> mkcont k
                                 @> fun _void_ ->
                                   Format.pp_print_cut f ();
                                   print_tag f "value" ""
                                     (aux tree f path)
                                   @> k)
                            @> mkcont k
                            @> fun _void_ -> false |> k)
                         true children
                       @> mkcont k
                       @> (fun _bool_ -> () |> k)
                   | `Linkto _ -> assert false
                   | `Answer _ | `Absent -> () |> k)
              @> k
          | G.Hidden, _ ->
              let key = Badop.Key.IntKey 0 in
              tree_down key tree ~errk:k @> mkcont k
              @> fun tree -> aux tree f (Badop.Path.add path key) @> k
          | G.Sum, _ ->
              (db_read path (Badop.Contents (D.query ()))
               @> function
               | `Answer (Badop.Contents (D.Response (Badop.Data.Int sumcase))) ->
                   let key = Badop.Key.IntKey sumcase in
                   tree_down key tree ~errk:k @> mkcont k
                   @> fun tree -> aux tree f (Badop.Path.add path key) @> k
               | `Answer (Badop.Contents (D.Response data)) ->
                   Logger.warning "During XML dump: invalid contents for sum type at %s, skipping: %s"
                     (G.tnode_id tree) (Badop.Data.to_string data);
                   () |> k
               | `Answer _ | `Linkto _ -> assert false
               | `Absent -> () |> k)
          | G.Product, edges ->
              fold_list
                (fun isfirst (e,_n) k ->
                   match e with
                   | G.Field (s,id) ->
                       if not isfirst then Format.pp_print_cut f ();
                       let key = Badop.Key.IntKey id in
                       tree_down key tree ~errk:(mkcont k (fun () -> isfirst |> k)) @> mkcont k
                       @> fun tree -> print_tag f s "" (aux tree f (Badop.Path.add path key))
                       @> mkcont k
                       @> (fun _void_ -> false |> k)
                   | _ -> failwith "bad field")
                true edges
              @> mkcont k
              @> fun _bool_ -> () |> k
          | G.Leaf lf, _ ->
              db_read path (Badop.Contents (D.query ()))
              @> function
              | `Answer (Badop.Contents (D.Response resp)) ->
                  (let x = DataImpl.get_string resp in
                   match lf with
                   | G.Leaf_binary ->
                       Format.fprintf f "<base64>%s</base64>" (Base.String.base64encode x)
                   | G.Leaf_text ->
                       (try Format.fprintf f "\"%s\"" (escape_string x) with
                        | CharacterNull -> Format.fprintf f "<base64>%s</base64>" (Base.String.base64encode x))
                   | G.Leaf_int | G.Leaf_float ->
                       Format.fprintf f "%s" x);
                  () |> k
              | `Answer _ | `Linkto _ -> assert false
              | `Absent -> () |> k
        in
        Format.fprintf f "<?xml version=\"1.0\"?>@\n";
        Format.fprintf f "@[<hv><opa_database_root version=\"%s\">@;<0 2>@[<hv>" xml_dump_version;
        aux tree f path_root @> mkcont k
        @> fun _void_ ->
          Format.fprintf f "@]@,</opa_database_root>@]@.";
          () |> k
    | `Answer _ | `Linkto _ | `Absent ->
        Logger.error "Database dump failed: could not read schema";
        () |> k

  let to_channel db_read ch k =
    let f = Format.formatter_of_out_channel ch in
    to_format db_read f @> k

  let to_file db_read file k =
    let ch = try Some (open_out file) with Sys_error _ -> None in
    match ch with
    | Some ch ->
        to_channel db_read ch @> mkcont k
        @> fun _void_ ->
          close_out ch;
          () |> k
    | None ->
        Logger.error "Could not open file %s for writing database XML dump" file;
        () |> k
end

(*
* import-db: todo...
let xml_from_file file =
  try
    let contents = File.content file in
    let _pos, (xml, ok) = Xparse.parse_xparse_file ~_filename:file contents in
    if not ok then failwith "xml structure error";
    Some xml
 with
 | Sys_error _ -> Printf.eprintf "import_db failed: could not open file %s.\n" file; None
 | Trx_runtime.SyntaxError (pos,msg) -> Printf.eprintf "import_db failed: syntax error in xml, %s\n" msg; None
 | Failure msg -> Printf.eprintf "import_db failed: %s\n" msg; None

%#register import_db: extern Dbgenlink.trans -> string -> unit
let import_db tr file =
  let rec aux tree path = function
    | [] -> ()
    | Xml.Text txt ->
        (match tree_node tree with
         | Hidden -> aux tree (Dbgenlink.dbpath_add path (Dbgenlink.key_int 0)) path
         | Leaf Leaf_int -> Dbgenlink.set tr path (int_of_string txt)
         | Leaf Leaf_string -> Dbgenlink.set tr path txt)
    | Xml.Node i ->
        let node = Stringmap.find nodes i in
        match tree_node tree with
        |
  match xml_from_file file with
  | Some { main = i } ->
  | _ -> Printf.eprintf "import_db failed: unrecognised xml format"
*)
