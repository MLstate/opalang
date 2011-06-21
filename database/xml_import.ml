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

module D = Badop.Dialog
module G = Dbgraph
module X = Xmlm

(* reverse of the function escape_string in Xml_dump *)
let unescape_string s =
  let slen = String.length s in
  let rec aux b i =
    if i >= slen then b else
      let c = s.[i] in
      let next () = aux (FBuffer.add b (String.make 1 c)) (i+1) in
      if c = '&' && i < slen - 1 then
        try
          let colon = String.index_from s (i+1) ';' in
          let elen = colon - i - 1 in
          if 0 < elen && elen <= 6 then
            match String.sub s (i+1) elen with
            | "amp" -> aux (FBuffer.add b "&") (colon+1)
            | "lt" -> aux (FBuffer.add b "<") (colon+1)
            | "gt" -> aux (FBuffer.add b ">")(colon+1)
            | "quot" -> aux (FBuffer.add b "\"") (colon+1)
            | "apos" -> aux (FBuffer.add b "'") (colon+1)
            | e when e.[0] = '#' ->
                (try
                   e.[0] <- '0'; (* convert XML-hex to ocaml-hex (#xB414 becomes 0xB414). No effect on decimal *)
                   let c = int_of_string e in
                   if c = 0 then next ()
                   else aux (FBuffer.add b (Cactutf.cons c)) (colon+1)
                 with Failure "int_of_string" -> next ())
            | _ -> next ()
          else next ()
        with Not_found -> next ()
      else next ()
  in
  FBuffer.contents (aux (FBuffer.create slen) 0)

let print_pos (li,col) = Printf.sprintf "line %d, column %d" li col
let warn fmt = Logger.warning ("XML import: "^^fmt)
let error fmt = Logger.error ("XML import: "^^fmt)

module F (B: Badop.S) (C: Xml_dump.SigCpsBackend) = struct

  (* our tiny CPS lib... *)
  let mkcont = C.mkcont
  let (@>) f k = f k
  let (|>) = C.return
  let rec fold_list f acc l k = match l with
    | [] -> acc |> k
    | hd::tl -> f acc hd @> mkcont k @> fun acc -> fold_list f acc tl @> k

  let commit_transaction_step =
    #<If:BADOP_XML_IMPORT_COMMIT_TRANSACTION_STEP$minlevel 1>
      int_of_string (Option.get DebugVariables.badop_xml_import_commit_transaction_step)
    #<Else>
      100
    #<End>

  let path_schema = Badop.Path.of_list [ Badop.Key.IntKey 2; Badop.Key.IntKey 0 ]
  let path_schema_version = Badop.Path.of_list [ Badop.Key.IntKey 2; Badop.Key.IntKey (-1) ]
  (* Path /2/0 to schema defined in dbGen_private.ml (/2 is config_keys, config key 0 is schema) *)
  let path_root = Badop.Path.of_list [ Badop.Key.IntKey 1 ]

  let from_input db t input k =
    let parsed_nodes = ref 0 in
    let written_nodes = ref 0 in
    let recursive_nodes = Hashtbl.create 16 in
    let do_link = ref (fun _ _ _ -> assert false) in
    let emergency_cont =
      mkcont k
      @> fun _ ->
        error "fatal error, stopping now (after %d nodes read, %d values written)"
          (!parsed_nodes) (!written_nodes);
        () |> k
    in
    let tr_init = None, commit_transaction_step
    in
    let tr_close (tropt,_n) k =
      match tropt with
      | None -> () |> k
      | Some tr ->
          B.Tr.prepare tr
          @> fun (tr, _ok) ->
            B.Tr.commit tr
            @> fun ok ->
              if not ok then
                warn "commit of a transaction failed. Your XML import may be incomplete.";
              () |> k
    in
    let write (tropt,n) path write_op k =
      (fun k -> match tropt with Some tr -> tr |> k | None -> B.Tr.start db @> fun tr -> tr |> k)
      @> mkcont k
      @> fun tr ->
        B.write tr path write_op
        @> fun resp ->
          incr written_nodes;
          let n = n-1 in
          let trn = (Some (Badop.Aux.result_transaction resp), n) in
          if n > 0 then trn |> k
          else tr_close trn @> mkcont k @> fun () -> tr_init |> k
    in
    let write_schema trn t k =
      let g = G.from_tree t in
      write trn path_schema_version (Badop.Set (D.query (Badop.Data.Int (G.version)))) @> mkcont k
      @> fun trn ->
        write trn path_schema (Badop.Set (D.query (Badop.Data.Binary (G.export_schema g))))
        @> k
    in
    let catch_read_error trn f x k =
      let r = try Some (f x) with
        | X.Error (pos, e) ->
            warn "parsing of XML input failed at %s:\n%s"
              (print_pos pos) (X.error_message e);
            None
      in
      match r with
      | Some r -> r |> k
      | None -> tr_close trn @> emergency_cont
    in
    let debug_print_signal = fun _ -> () (* function *)
      (* | `Dtd _ -> Printf.eprintf "[33m>> Read: Dtd[0m\n" *)
      (* | `El_start ((_ns,tag),_attr) -> Printf.eprintf "[33m>> Read: Tag <%s>[0m\n" tag *)
      (* | `El_end -> Printf.eprintf "[33m>> Read: Tag end[0m\n" *)
      (* | `Data d -> Printf.eprintf "[33m>> Read: Data (\"%s\")[0m\n" d *)
    in
    (* reads while catching errors from Xmlm, trimming `Data and skipping empty `Data *)
    let rec read trn k =
      catch_read_error trn X.input input @> mkcont k
      @> function
      | `Data d ->
          let d = Base.String.trim d in
          if d = "" then read trn @> k
          else
            let s = `Data d in
            incr parsed_nodes; debug_print_signal s; s |> k
      | s -> incr parsed_nodes; debug_print_signal s; s |> k
    in
    (* only applies f if there's something else than `El_end to read. Trims and
       skips `Data like read *)
    let rec read_content trn dflt f k =
      let pos = X.pos input in
      catch_read_error trn X.peek input @> mkcont k
      @> fun s -> match s with
      | `El_end -> dflt |> k
      | `Data d ->
          let d = Base.String.trim d in
          ignore (X.input input);
          incr parsed_nodes;
          if d = "" then read_content trn dflt f k
          else
            let s = `Data d in
            debug_print_signal s;
            f s pos @> k
      | _ ->
          ignore (X.input input);
          incr parsed_nodes;
          debug_print_signal s;
          f s pos @> k
    in
    let rec read_tag_end ?ign ?(silent=false) trn k =
      let pos = X.pos input in
      read trn @> mkcont k
      @> function
      | `El_end ->
          if not silent then
            (match ign with None -> () | Some ign_pos ->
               warn "ignored unexpected content from %s to %s"
                 (print_pos ign_pos) (print_pos pos));
          () |> k
      | `El_start _ ->
          read_tag_end ~silent:true trn @> mkcont k
          @> fun () -> read_tag_end ~ign:(Option.default pos ign) ~silent trn @> k
          | `Data d when Base.String.trim d = "" ->
              read_tag_end ?ign ~silent trn @> k
          | _ ->
              read_tag_end ~ign:(Option.default pos ign) ~silent trn @> k
    in
    (* Call f on all tags that can be read as direct children of the current
       tag; f is supposed to consume all contents of the tag. trn is always passed
       along. *)
    let rec read_tags trn f k =
      read_content trn trn
        (fun x pos k -> match x with
         | `El_start (("",tag), attrlist) ->
             f trn tag attrlist pos @> mkcont k
             @> fun trn ->
               read_tag_end trn @> mkcont k
               @> fun () -> read_tags trn f @> k
         | `Data _ ->
             let pos' = X.pos input in
             warn "ignored unexpected string data from %s to %s"
               (print_pos pos) (print_pos pos');
             read_tags trn f @> k
         | _ -> assert false)
      @> k
    in
    let rec read_tag trn expected_tag dflt f k =
      read_content trn dflt
        (fun s pos k -> match s with
         | `El_start (("",tag), attrs) when tag = expected_tag ->
             f attrs pos @> k
         | `El_start ((_,tag), _) ->
             warn "ignored tag <%s>, was expecting <%s> at %s"
               tag expected_tag (print_pos pos);
             read_tag_end ~ign:pos trn @> mkcont k
             @> fun () -> read_tag trn expected_tag dflt f @> k
         | `Data _ ->
             warn "ignored text data, was expecting <%s> at %s"
               expected_tag (print_pos pos);
             read_tag trn expected_tag dflt f @> k
         | _ -> assert false)
      @> k
    in
    let read_data trn dflt f k =
      read_content trn dflt
        (fun x pos k ->
           match x with
           | `Data str ->
               f (unescape_string (Base.String.strip_quotes str)) pos @> k
           | `El_start (("","base64"),[]) ->
               read_content trn dflt
                 (fun x pos k -> match x with
                  | `Data str ->
                      let dat =
                        try Some (Base.String.base64decode str)
                        with Invalid_argument "base64decode" ->
                          warn "skipping invalid base64 encoding at %s" (print_pos pos);
                          None
                      in
                      (match dat with
                       | Some dat -> f dat pos @> k
                       | None -> dflt |> k)
                  | `El_start _ ->
                      read_tag_end ~ign:pos trn @> mkcont k
                      @> fun () -> dflt |> k
                  | _ -> assert false)
               @> mkcont k
               @> fun res ->
                 read_tag_end trn @> mkcont k (* </base64> *)
               @> fun () -> res |> k
           | _ -> assert false)
      @> k
    in
    let warn_nonempty_attrs attrlist pos =
      if attrlist <> [] then
        warn "ignored unexpected tag attributes at %s" (print_pos pos);
    in
    let import_leaf trn path leaf k =
      read_data trn trn
        (fun dat pos k ->
           let dat =
             try
               match leaf with
               | G.Leaf_int -> Some (Badop.Data.Int (int_of_string dat))
               | G.Leaf_float -> Some (Badop.Data.Float (float_of_string dat))
               | G.Leaf_text -> Some (Badop.Data.Text dat)
               | G.Leaf_binary -> Some (Badop.Data.Binary dat)
             with
             | Failure "int_of_string" ->
                 warn "ignored invalid integer \"%s\" at %s" dat (print_pos pos);
                 None
             | Failure "float_of_string" ->
                 warn "ignored invalid float \"%s\" at %s" dat (print_pos pos);
                 None
           in
           match dat with
           | Some d ->
               write trn path (Badop.Set (D.query d)) @> k
           | None -> trn |> k)
      @> k
    in
    let rec import_record_aux path edges trn tag attrlist pos k =
      warn_nonempty_attrs attrlist pos;
      match List.filter (function G.Field (tag',_), _ when tag' = tag -> true | _ -> false) edges with
      | [ G.Field (_tag, id), child ] ->
          (* Printf.eprintf "[35mInside record field %s[0m\n%!" tag; *)
          import_node trn child (Badop.Path.add path (Badop.Key.IntKey id))
          @> k
      | [] ->
          warn "ignored unexpected tag <%s> at %s"
            tag (print_pos pos);
          trn |> k
      | _ -> assert false
    and import_record trn path edges k =
      read_tags trn (import_record_aux path edges) @> k
    and import_map trn path t kind k =
      let rec import_entries trn k =
        read_tag trn "entry" trn
          (fun attrlist pos_entry k ->
             warn_nonempty_attrs attrlist pos_entry;
             read_tag trn "key" None
               (fun attrlist pos k ->
                  warn_nonempty_attrs attrlist pos;
                  read_data trn None
                    (fun raw_key _pos k ->
                       let key_opt = match kind with
                         | G.Kint ->
                             (try Some (Badop.Key.IntKey (int_of_string raw_key))
                              with Failure "int_of_string" -> None)
                         | G.Kstring -> Some (Badop.Key.StringKey (unescape_string raw_key))
                         | G.Kfields _ ->
                             error "set keys unsupported yet";
                             None
                       in
                       key_opt |> k)
                  @> mkcont k
                  @> fun key_opt -> read_tag_end trn @> mkcont k (* </key> *)
                  @> fun () -> key_opt |> k)
             @> mkcont k
             @> function
             | None ->
                 warn "ignored invalid %smap entry with bad key at %s"
                   (match kind with G.Kint -> "int" | G.Kstring -> "string" | G.Kfields _ -> "record")
                   (print_pos pos_entry);
                 read_tag_end ~ign:pos_entry trn @> mkcont k (* </entry> *)
                 @> fun () -> import_entries trn @> k
             | Some key ->
                 read_tag trn "value" trn
                   (fun attrlist pos k ->
                      warn_nonempty_attrs attrlist pos;
                      import_node trn t (Badop.Path.add path key) @> mkcont k
                      @> fun trn -> read_tag_end trn @> mkcont k (* </value> *)
                      @> fun () -> trn |> k)
                 @> mkcont k
                 @> fun trn -> read_tag_end trn @> mkcont k (* </entry> *)
                 @> fun () -> import_entries trn @> k)
        @> k
      in
      read_tag trn "map" trn
        (fun attrlist pos k ->
           warn_nonempty_attrs attrlist pos;
           import_entries trn @> mkcont k
           @> fun trn -> read_tag_end trn @> mkcont k (* </map> *)
           @> fun () -> trn |> k)
      @> k
    and import_recursive trn id path (_e,t) trec k =
      let key =
        match t with
        | G.Tnode (id', _, _) ->
            let key = try
              let _, _, key = Hashtbl.find recursive_nodes id' in key+1
            with Not_found -> 0 in
            Hashtbl.add recursive_nodes id' (path, trec, key);
            key
        | _ ->
            error "Unexpected schema structure at recursive node %s" id;
            assert false
      in
      let next_path = Badop.Path.add path (Badop.Key.IntKey key) in
      if key = 0 then
        write trn path (Badop.Clear (D.query ())) @> mkcont k
          (fun trn -> import_node trn t next_path @> k)
      else
        !do_link trn next_path @> mkcont k
        @> fun trn -> import_node trn t next_path @> k
    and import_sum trn path el k =
      read_tags trn
        (fun trn tag attrlist pos k ->
           (* Find path of sum case *)
           match
             List.find_all
               (function
                | G.SumCase _, G.Tnode (_, G.Product, el) ->
                    List.exists (function G.Field (tag', _id), _ when tag' = tag -> true | _ -> false) el
                | _ -> false)
               el
           with
           | [G.SumCase id, G.Tnode (_, G.Product, el)] ->
               write trn path (Badop.Set (D.query (Badop.Data.Int id)))
               @> mkcont k
               @> fun trn ->
                 let path = Badop.Path.add path (Badop.Key.IntKey id) in
                 import_record_aux path el trn tag attrlist pos @> k
           | _::_ ->
               (* we don't handle sum types with a conflicting first field (that would need look-ahead) *)
               warn "Ignored tag <%s> which has several matching cases in the database schema at %s"
                 tag (print_pos pos);
               trn |> k
           | [] ->
               warn "Ignored tag <%s> which has no matching cases in the database schema at %s"
                 tag (print_pos pos);
               trn |> k
        ) @> k
    and import_link trn id path k =
      do_link :=
        (fun trn p k -> write trn path (Badop.Link (D.query p)) @> k);
      let recpath, trec, _ = Hashtbl.find recursive_nodes id in
      import_node trn trec recpath @> k
    and import_node trn t path k = match t with
      | G.Tnode (_id, G.Product, []) -> write trn path (Badop.Set (D.query (Badop.Data.Unit))) @> k
      | G.Tnode (_id, G.Product, el) -> import_record trn path el @> k
      | G.Tnode (_id, G.Multi, [G.Multi_edge kind, t]) -> import_map trn path t kind @> k
      | G.Tnode (id, G.Hidden, [e]) -> import_recursive trn id path e t @> k
      | G.Tnode (_id, G.Sum, el) -> import_sum trn path el @> k
      | G.Tnode (_id, G.Leaf lf, []) -> import_leaf trn path lf @> k
      | G.Tnode (_id, _, _) -> assert false (* invalid graph *)
      | G.Tlink id -> import_link trn id path @> k

    in
    read tr_init @> mkcont k
    @> fun _dtd ->
      read tr_init @> mkcont k
    @> function
    | `El_start (("","opa_database_root"), attr) ->
        let version =
          try Some (List.assoc ("","version") attr) with
          | Not_found -> error "could not read version number from database XML input"; None
        in
        if version = Some Xml_dump.xml_dump_version then
          write_schema tr_init t @> mkcont k
          @> fun trn ->
            import_node trn t path_root @> mkcont k
          @> fun trn ->
            read_tag_end trn @> mkcont k
          @> fun () ->
            tr_close trn @> mkcont k
          @> fun () ->
            let pos = X.pos input in
            if try not (X.eoi input) with X.Error _ -> true then
              warn "trailing garbage at end of file (after %s)" (print_pos pos);
            Logger.info
              "Database XML import finished. %d nodes read, %d values written." (!parsed_nodes) (!written_nodes);
            () |> k
        else
          (error "this XML database dump seems to be of an unknown version: %s (expected %s)"
             (Option.default "<no version number>" version) Xml_dump.xml_dump_version;
           () |> k)
    | _ ->
        error "can't read XML root node";
        () |> k

  let from_channel db t ch k =
    let input = X.make_input (`Channel ch) in
    from_input db t input @> k

  let from_function db t f k =
    let input = X.make_input (`Fun f) in
    from_input db t input @> k

  let from_file db t file k =
    let ch = try Some (open_in file) with Sys_error _ -> None in
    match ch with
    | Some ch ->
        from_channel db t ch @> mkcont k
        @> fun () ->
          close_in ch;
          () |> k
    | None ->
        error "could not open file %s for reading" file;
        () |> k
end
