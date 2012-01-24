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
(* CF mli *)

module List = BaseList

module Arg =
struct
  module A = Base.Arg

  (* overriding db options *)
  let commandline_override =
    ref (StringListMap.empty : QmlAst.Db.options list StringListMap.t)

  let parse_opts s =
    try
      let (engine, arg) = BaseString.split_char '(' s in
      let arg = if arg = "" then arg else BaseString.remove_prefix "\"" arg in
      let arg = if arg = "" then arg else BaseString.remove_suffix "\")" arg in
      let arg_opt = if arg = "" then None else Some arg in
      match engine with
      | "local" -> [`engine (`db3 arg_opt)]
      | "light" -> [`engine (`db3light arg_opt)]
      | "meta" -> [`engine `meta]
      | "shared" ->
          let (hostname, port) = BaseString.split_char ':' arg in
          let hostname = if hostname = "" then None else Some hostname in
          let port = if port = "" then None else Some (int_of_string port) in
          [`engine (`client (hostname, port))]
      | _ -> failwith "bad engine"
    with
    | Not_found
    | Failure _ -> failwith "The syntax of database options should be as in source code, e.g., --database 'db1:\"@local(./file)\"'."

  (* display enforce output *)
  let export_schema = (Hashtbl.create 1 : (string option,string) Hashtbl.t) (* db_name -> file *)
  let output_schema = ref false

  let options = [

    "--database",
    A.String (fun s ->
                try
                  let (name, opt_string) = BaseString.split_char '@' s in
                  let point =
                    if name = "" then []
                    else [BaseString.remove_suffix ":" name]
                  in
                  let opts = parse_opts opt_string in
                  commandline_override :=
                    StringListMap.add point opts !commandline_override;
                  ()
                with
                | Not_found -> failwith "Separate the name of the database and the options with a colon, e.g., --database 'db1:\"@shared(:4849)\"'."
             ),
    " Override options of a database";

    "--export-db-schema",
    A.String (fun s ->
                let db,file = if String.contains s ':' then Base.String.split_char ':' s else "",s in
                Hashtbl.add export_schema (if db = "" then None else Some db) file),
    " Exports the database schema to the given file (format depends on the extension: use gml for manipulating it with opa-db-tool, or dot (the default) for display). Use [database_name]:file_name if you have multiple databases.";

    "--print-dbschema",
    A.Set output_schema,
    " Dump the db-schema using the track system" ;

  ]
end

(*
  The name of the file where to output the schema.
  <!> keep synchro with opatrack
*)
let schema_filename = "schema"

(* displaying the schema if asked, and fork for seeing it with display if asked *)
let auto_disp_schema schema =
  if !Arg.output_schema then
    try
      let pp fmt schema =
        QmlDbGen.Schema.to_dot schema fmt
      in
      let filename = schema_filename in
      ignore (PassTracker.file ~filename pp schema)
    with
    | e ->
        OManager.warning ~wclass:WarningClass.dbgen_schema
          "@[<2>  An error occurred while trying to display the output the db schema@\n%s@]"
          (Printexc.to_string e)
  else
    if ObjectFiles.compilation_mode() = `init then (* Only export the full schema, ie after `init *)
      Hashtbl.iter
        (fun db filename ->
           try
             let ch = open_out filename in
             if Base.String.is_suffix ".gml" filename
             then QmlDbGen.Schema.db_to_gml schema db ch
             else QmlDbGen.Schema.db_to_dot schema db ch;
             close_out ch
           with
           | Not_found ->
               OManager.warning ~wclass:WarningClass.dbgen_schema
                 "@[<2> Could not dump the database schema: database \"%s\" not found@]"
                 (Option.default "<default>" db)
           | Sys_error msg ->
               OManager.warning ~wclass:WarningClass.dbgen_schema
                 "@[<2> Could not open file for outputting the database schema%s@\n%s@]"
                 (match db with Some db -> " of database "^db | None -> "")
                 msg)
        Arg.export_schema

(* separation *)
module S =
struct
  type t = QmlDbGen.Schema.t
  let pass = "pass_DbSchemaGeneration"
  let pp f _ = Format.pp_print_string f "<dummy>"
end
module R = ObjectFiles.Make(S)

let process_code gamma _annotmap schema code =
  assert(schema == QmlDbGen.Schema.initial);

  (* getting the schema of other packages *)
  let merge package schema1 schema2 =
    (* UNNEEDED: because the types aren't supposed to contain type variables
       let schema2 = QmlDbGen.Schema.map_types (QmlRefresh.refresh_typevars_from_ty package) schema2 in *)
    let schema2 = QmlDbGen.Schema.map_expr (QmlRefresh.refresh_expr_no_annotmap package) schema2 in
(*     let oc1 = open_out ("schema1"^(fst package)) in *)
(*     let _ = QmlDbGen.Schema.to_dot schema1 oc1 in *)
(*     let oc2 = open_out ("schema2"^(fst package)) in *)
(*     let _ = QmlDbGen.Schema.to_dot schema2 oc2 in *)
    let merged_schema = QmlDbGen.Schema.merge schema1 schema2 in
(*     let oc3 = open_out ("schema3"^(fst package)) in *)
(*     let _ = QmlDbGen.Schema.to_dot merged_schema oc3 in *)
(*     let _ = close_out oc1; close_out oc2; close_out oc3 in *)
    merged_schema
  in
  let schema =
    if ObjectFiles.compilation_mode() = `init
    then
      let sch = R.fold_with_name ~packages:true ~deep:true merge schema in
      sch
    else R.fold_with_name merge schema
  in
  (* registering Database definitions
     The construction of the schema needs to get
     Database nodes before NewDbValue.
  *)
  let (schema, gamma) =
    List.fold_left
      (fun ((schema, gamma) as acc) code_elt ->
         match code_elt with
         | QmlAst.Database (label, ident, p, opts) ->
             QmlDbGen.Schema.register_db_declaration
               schema gamma (label, ident, p, opts)
         | _ -> acc
      ) (schema, gamma) code in

  (* registering NewDbValue definitions *)
  let schema, code =
    List.fold_left_collect (
      fun schema code_elt ->
        match code_elt with
        | QmlAst.NewDbValue (label, value) ->
            let schema, o =
              QmlDbGen.Schema.register_new_db_value ~name_default_values:true
                schema gamma (label, value) in
            let code =
              match o with
              | Some (binding, value) ->
                  let code_elt = QmlAst.NewVal (Annot.refresh label, [binding]) in
                  [code_elt; QmlAst.NewDbValue (label, value)]
              | None -> [code_elt] in
            schema, code
        | _ -> schema, [code_elt]
    ) schema code in

  (* modify by commandline options: *)
  let schema =
    let override point (ident, db_options) =
      if StringListMap.mem point !Arg.commandline_override then begin
        let opts = StringListMap.find point !Arg.commandline_override in
        Arg.commandline_override :=
          StringListMap.remove point !Arg.commandline_override;
        (ident, opts)
      end else begin
        (ident, db_options)
      end
    in
    let schema = QmlDbGen.Schema.mapi override schema in
    if StringListMap.is_empty !Arg.commandline_override then schema
    else begin
      let (key, _) = StringListMap.min !Arg.commandline_override in
      match key with
      | [] -> failwith "No anonymous database declared in the source code."
      | l ->
          let n = String.concat "/" l in
          let msg = Printf.sprintf "No database with name '%s' declared." n in
          failwith msg (* TODO: register the error somewhere *)
    end
  in

  (* finalizing the schema *)
  let schema, partial_schema =
    match QmlDbGen.Schema.finalize schema with
    | Some schema ->
        schema,
        QmlDbGen.Schema.of_package schema (ObjectFiles.get_current_package_name())
    | None -> schema, schema (* empty schemas *)
  in

  let _ = R.save partial_schema in

  let _ = auto_disp_schema schema in

  (gamma, schema, code)
