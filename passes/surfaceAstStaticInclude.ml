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
(* see surfaceAstPasses.mli to have a description of what these passes do *)

module F : sig
  (** *)
  val chroot : string -> unit
  val normalize_relative_path : string -> string option
  val concat : string -> string -> string
  val path_sep : string
  val mimetype : ?mime_db:string -> string -> string
  val explicit_path : string -> string option -> string
  val is_relative : string -> bool

  val fold_dir_rec : ('a -> name:string -> path:string -> 'a) -> 'a -> string -> 'a
  val content : string -> string
end = struct
  include File

  let absolize prefix path =
    let path =
      if is_relative path then
        Printf.sprintf "%s/%s" prefix path
      else path
    in explicit_path path None

  let chroot_dir = ref None

  let chroot path =
    if not(is_directory path) then
      OManager.error "I/O error: @{<bright>%S@} -> No such directory" path
    else (
      let path = absolize (Unix.getcwd ()) path in
      OManager.verbose "Setting root inclusions to %s" path;
      chroot_dir := Some path
    )

  let checkroot path =
    match !chroot_dir with
    | None -> ()
    | Some root ->
        let path = absolize root path in
        if BaseString.is_prefix root path then ()
        else
          OManager.error "Try to include a file that not in your compiler root inclusion directory."

  let fold_dir_rec f a d = checkroot d; fold_dir_rec f a d

  let content d = checkroot d; content d

end

module SA = SurfaceAst
module C = SurfaceAstCons.ExprIdentCons
module List = Base.List

let warning_set = WarningClass.Set.create ()

let warning =
  let doc = "Static inclusion warnings" in
  WarningClass.create ~doc ~name:"inclusions" ~err:false ~enable:true ()

let warning_unsafe_directory =
  let doc = "Static inclusion: warn if the directory has a suspicious name." in
  WarningClass.create ~parent:warning ~doc ~name:"unsafe_directory" ~err:false ~enable:true ()

let warning_many_files_1000 =
  let doc = "Static inclusion: warn of you are including a directory that contains more than 1000 files." in
  WarningClass.create ~parent:warning ~doc ~name:"many_files" ~err:false ~enable: true ()

let warning_many_files_10000 =
  let doc = "Static inclusion: warn of you are including a directory that contains more than 10000 files." in
  WarningClass.create ~parent:warning ~doc ~name:"really_many_files" ~err:false ~enable:true ()

let warning_directory_does_not_exist =
  let doc = "Static inclusion: stop execution if a directory meant to be included cannot be read (generally because it doesn't exist)." in
  WarningClass.create ~parent:warning ~doc ~name:"directory_does_not_exist" ~err:false ~enable:true ()

let warning_directory_empty =
  let doc = "Static inclusion: warn when including an empty directory." in
  WarningClass.create ~parent:warning ~doc ~name:"directory_empty" ~err:false ~enable:true ()

let warning_file_does_not_exist =
  let doc = "Static inclusion: stop execution if a file meant to be included cannot be read (generally because it doesn't exist)." in
  WarningClass.create ~parent:warning ~doc ~name:"file_does_not_exist" ~err:true ~enable:true ()

let _ = WarningClass.Set.add_all warning_set [
  warning;
  warning_unsafe_directory;
  warning_many_files_1000;
  warning_many_files_10000;
  warning_directory_does_not_exist;
  warning_file_does_not_exist;
  warning_directory_empty;
]

let return_prefix_and_normalized_path options path =
  let prefix, path =
    match F.normalize_relative_path path with
    | None -> "", path
    | Some path -> F.concat options.OpaEnv.project_root "", Filename.concat options.OpaEnv.project_root path in
  let path = match F.normalize_relative_path path with (*The path may still be relative, e.g. if project_root is "."*)
    | None   -> path
    | Some x -> x in
  prefix, path
let fold_on_files_from_directory error_handler f acc path =
  try
    F.fold_dir_rec (fun acc ~name:_ ~path ->
                      f acc path
                   ) acc path
  with Unix.Unix_error (e, _, _) ->
    error_handler e

let pass_static_inclusion_directory ~options lcode =
  (* Seems stange that directory inclusions not use options then
     file inclusions use it. *)
  Option.iter (F.chroot) options.OpaEnv.root_inclusions;
  let aux ((e,label) as v)  =
    let handle_inclusion make_expr_include path args =
      (
        let factory_helper = match args with
        | []  -> None
        | [l] -> Some l
        | _   -> OManager.error "Internal error: directive @@static_content_directory/@@static_resource_directory has too many arguments"
        in
        (*let is_directory path = try F.is_directory path with | Unix.Unix_error _ -> false in*)

        if path = "." || path = "./" || path = "/" || path = "\\" then
          OManager.warning ~wclass:warning_unsafe_directory "You are attempting to include directory '%s'. This is suspicious." path;

        let prefix, path = return_prefix_and_normalized_path options path in
        let to_url my_path = match F.normalize_relative_path my_path with
            Some (normalized_path:string) ->
              Str.global_replace (Str.regexp F.path_sep) "/" normalized_path
          | None                 -> F.explicit_path (Str.global_replace (Str.regexp F.path_sep) "/" my_path) None (*TODO: check this line*)
        in

        OManager.verbose "Embedding the resources of directory %s" path;

        let (number, files) =
          fold_on_files_from_directory
            (fun e ->
               OManager.warning ~wclass:warning_directory_does_not_exist "Error reading directory: %s\nError encountered: %s." path (Unix.error_message e);
               (0, []))
            (fun (number, acc) y ->
               if number = 1000 then
                 OManager.warning ~wclass:warning_many_files_1000 "Suspicious include: you are attempting to include more than 1000 files in directory %s." path
               else if number = 10000 then
                 OManager.warning ~wclass:warning_many_files_1000 "Suspicious include: you are attempting to include more than 10000 files in directory %s." path
               ;
               let reduced = Base.String.remove_prefix_if_possible prefix y in
               OManager.verbose "Embedding file %s" reduced;
               (number+1, (Base.String.remove_prefix_if_possible prefix y)::acc)) (0,[])
            path
        in

        OManager.verbose "...that's a total of %d %s" number (if number = 1 then "file" else "files");

        SurfaceAstCons.with_label label
          (fun () ->
             let id_sempty = OpaMapToIdent.val_ Opacapi.StringMap.empty in
             let sempty = C.E.ident id_sempty in

             let id_sadd = OpaMapToIdent.val_ Opacapi.StringMap.add in
             let sadd = C.E.ident id_sadd in

             let map_ident n = SurfaceAstCons.ExprIdent.ns_fresh ~label (Printf.sprintf "map_%d" n) in

             let n = List.length files in
             let id_mapn = map_ident n in
             let mapn  = C.E.ident id_mapn in

             (* The following expression creates this term:
                [map_0 = StringMap.empty
                map_1 = StringMap.add("img.png", @static_include_resource("img.png"), map_0)
                ...
                map_n]
             *)
             let rec construct_expr n id_map_n let_in_n names =
               match names with
               | [] -> None
               | filename::[] ->
                   let expr_filename     = C.E.string(to_url filename) in
                   let expr_include      = make_expr_include factory_helper filename in
                   let application       = C.E.applys sadd [expr_filename; expr_include; sempty] in
                   let map_n             = C.E.letin id_map_n application let_in_n in
                   Some map_n
               | filename::y ->
                   let expr_filename     = C.E.string(to_url filename) in
                   let expr_include      = make_expr_include factory_helper filename in
                   let id_map_n_moins_un = map_ident (n-1) in
                   let map_n_moins_un    = C.E.ident id_map_n_moins_un in
                   let application       = C.E.applys sadd [expr_filename;expr_include;map_n_moins_un] in
                   let map_n             = C.E.letin id_map_n application let_in_n in
                   construct_expr (n-1) id_map_n_moins_un map_n y
             in

             let final_exp =
               match construct_expr n id_mapn mapn  files with
               | Some e -> e
               | None   ->
                   OManager.warning ~wclass:warning_directory_empty "Directory %s is empty." path;
                   sempty
             in
             final_exp
          ) (* end with_label *)
      ) (* end else *)
    in
    match e with
    | SA.Directive (`static_content_directory (path, eval), args, _) ->
        handle_inclusion (fun factory_helper filename -> C.D.static_content ?factory_helper eval filename) path args
    | SA.Directive (`static_resource_directory path, args, _) ->
        handle_inclusion (fun factory_helper filename -> C.D.static_resource ?factory_helper filename) path args
    | _ -> v in
  (* any 'map' function will do *)
  OpaWalk.Code.map_down aux lcode


let copy_label v = {v with QmlLoc.notes = SurfaceAstCons.Fresh.id ()}

let pass_static_inclusions  ~options lcode: (Ident.t, 'a) SurfaceAst.code =
  let aux acc ((e,label) as v)  =
    let lab() = copy_label label in
    let get_content s =
      let handle_error message =
        OManager.warning ~wclass:warning_file_does_not_exist "Could not open file %s. Error encountered: %s. I'll replace that file by some debugging code." s message;
        Printf.sprintf "This should have been the contents of file '%s'. However, this file could not be opened because of a compile-time error. This compile-time error was ignored because the compiler was launched with some warnings/errors deactivated.\n Detail of the error:\n %s." s message
      in
      try F.content s
      with
        Unix.Unix_error(e, _, _) ->
          let message = Unix.error_message e in
          handle_error message
      | Failure message ->
          handle_error message
    in
    match e with
    | SA.Directive ((`static_content (path, eval)), maybe_factory, _) ->
        OManager.verbose "I wish to embed content %S" path;
        let factory_expr  = match maybe_factory with
          | [] -> (C.E.ident  ~label:(lab()) (OpaMapToIdent.val_ Opacapi.identity))
          | [e] -> e
          | _   -> OManager.error "Internal error: directive @@static_*_content has too many arguments"
        in
        let full_path = if F.is_relative path then Filename.concat options.OpaEnv.project_root path else path in
        OManager.verbose "Embedding file @{<bright>%S@} as @{<bright>%S@}" full_path path;

        let getter_ident = SurfaceAstCons.ExprIdent.ns_fresh ~label:(lab()) "static_file_content" in
        let getter_expr  = C.E.applys ~label:(lab())
           (C.E.ident  ~label:(lab()) (OpaMapToIdent.val_ Opacapi.Resource_private.content_of_include))
           [C.E.string ~label:(lab()) path;
            C.E.record ~label:(lab()) ["misc", C.E.void ~label:(lab()) ()];
            C.E.ident  ~label:(lab()) (OpaMapToIdent.val_ Opacapi.identity);
            C.E.string ~label:(lab()) (get_content full_path);
            C.E.false_ ~label:(lab()) ();
            if options.OpaEnv.compile_release then C.E.true_ ~label:(lab()) () else C.E.false_ ~label:(lab()) ();
            factory_expr
        ] in
        let getter_code   = C.C.newval ~label:(lab()) getter_ident getter_expr in

        (*If [eval] is required, insert a call to <<$f$()>>*)
        let getter_call = if eval then C.E.applys ~label:(lab()) (C.E.ident ~label:(lab()) getter_ident) [] else C.E.ident ~label:(lab()) getter_ident
        in
        (getter_code::acc, getter_call)

    | SA.Directive ((`static_resource path), maybe_factory, _) ->
        (* Oops, duplication *)
        let relative_position = PathTransform.of_string options.OpaEnv.project_root in
        let full_path = PathTransform.string_to_mysys ~relative_position path in

        let mimetype  =
          try F.mimetype ?mime_db:options.OpaEnv.mime_database full_path
          with (File_mimetype.Open s) ->
            (OManager.warning ~wclass:warning_file_does_not_exist
              "Could not open file %s. Error encountered: %s. I'll replace that file by some debugging code." full_path s;
            "text/plain") in

        OManager.unquiet "Embedding file @{<bright>%S@} as resource @{<bright>%S@} with mimetype @{<bright>%S@}" full_path path mimetype;

        let factory_expr  = match maybe_factory with
          | [] -> C.E.applys ~label:(lab())
              (C.E.ident  ~label:(lab()) (OpaMapToIdent.val_ Opacapi.Resource_private.raw_resource_factory))
                [C.E.string ~label:(lab()) mimetype]
          | [e] -> e
          | _   -> OManager.error "Internal error: directive @@static_include_resource has too many arguments"
        in

        let getter_ident = SurfaceAstCons.ExprIdent.ns_fresh ~label:(lab()) "static_include_resource" in
        let content =
          let content =
            C.E.string ~label:(lab()) (BaseString.base64encode (get_content full_path))
          in
          let bin = (C.E.ident  ~label:(lab()) (OpaMapToIdent.val_ Opacapi.bin_of_base64)) in
          C.E.applys ~label:(lab()) bin [content]
        in
        let getter_expr  = C.E.applys ~label:(lab())
           (C.E.ident  ~label:(lab()) (OpaMapToIdent.val_ Opacapi.Resource_private.make_resource_include))
           [C.E.string ~label:(lab()) path;
            C.E.record ~label:(lab()) ["misc", C.E.void ~label:(lab()) ()];
            content;
            C.E.false_ ~label:(lab()) ();
            if options.OpaEnv.compile_release then C.E.true_ ~label:(lab()) () else C.E.false_ ~label:(lab()) ();
            C.E.record ~label:(lab()) ["permanent", C.E.void ~label:(lab()) ()];
            factory_expr] in
        let getter_code   = C.C.newval ~label:(lab()) getter_ident getter_expr in

        (*Insert a call to <<$f$()>>*)
        (*let getter_call = C.E.applys ~label:(lab()) (C.E.ident ~label:(lab()) getter_ident) []
        in*)
        (getter_code::acc, C.E.ident ~label:(lab()) getter_ident)

    | _ -> (acc, v) in

  let (adds, v) = OpaWalk.Code.foldmap aux [] lcode in
  adds @ v


let pass_analyse_static_include_deps ~options code =
  let update_map_with_path map path =
    let full_path = if F.is_relative path then Filename.concat options.OpaEnv.project_root path else path in
    let last_modification_time = try Some (Unix.stat full_path).Unix.st_mtime with Unix.Unix_error _ -> None in
    StringMap.add full_path last_modification_time map in
  OpaWalk.Code.fold
    (fun acc e ->
       match e with
       | (SA.Directive (((`static_resource path | `static_content (path, _)) : [< SurfaceAst.all_directives ]), _, _), _) ->
           update_map_with_path acc path
       | (SA.Directive ((`static_resource_directory path | `static_content_directory (path, _)), _, _), _) ->
           let _prefix, path = return_prefix_and_normalized_path options path in
           fold_on_files_from_directory
             (fun _e -> acc)
             (fun acc path -> update_map_with_path acc path)
             acc path
       | _ -> acc
    ) StringMap.empty code
