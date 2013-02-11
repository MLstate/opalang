(*
    Copyright © 2011-2013 MLstate

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


type wclass = {
  mutable loaded : bool;
  toplevel : bool;
  name : string;
  doc : string;
  public : bool ;
  mutable err : bool; (* whether triggering this warning is an error *)
  mutable enable : bool; (* true means the warning/error is enabled *)
  mutable children : wclass list;  (* to implement a hierarchy of warnings *)
}

module S =
struct
  type 'a t = wclass constraint 'a = _ * _ * _
  let subs_cons ({children=children} as wclass) =
    (fun children -> wclass.children <- children; wclass), children
end
module Traverse = Traverse.Make(S)

let root_warning = {
  loaded = true ; (* don't really care, this warning is the start point anyway *)
  toplevel = true ; (* idem *)
  name = "root" ;
  doc = "Regroups all the warnings of the compiler" ;
  public = true ;
  err = false ; (* don't really care, this warning shouldn't be used anyway *)
  enable = true ; (* don't really care, this warning shouldn't be used anyway *)
  children = [] ;
}

let get_children wclass name =
  List.find (fun {name=name'} -> name = name') wclass.children
let has_children wclass name =
  try ignore (get_children wclass name); true with Not_found -> false

let create ?parent ?(public=false) ~name ~doc ~err ~enable () =
  let wclass =
    let loaded = false in
    let toplevel = Option.is_none parent in
    let name =
      match parent with
      | Some parent when parent != root_warning -> parent.name^"."^name
      | _ -> name
    in
    {
      loaded = loaded ;
      toplevel = toplevel ;
      name = name ;
      doc = doc ;
      public = public ;
      err = err ;
      enable = enable ;
      children = [] ;
    } in
  let _ =
    match parent with
    | Some parent ->
        assert (not (has_children parent name));
        parent.children <- wclass :: parent.children
    | _ -> ()
  in
  wclass

module Set =
struct
  type t = wclass Stack.t
  let create () : t = Stack.create ()
  let add t (w:wclass) = Stack.push w t
  let add_all t wl = List.iter (add t) wl
  let add_set t t' = Stack.iter (add t) t'
  let create_from_list l =
    let t = create () in
    add_all t l;
    t
end

let load w =
  Traverse.iter (fun w -> w.loaded <- true) w;
  if w.toplevel
  then (
    try
      let w' = get_children root_warning w.name in
      if w != w' then (
        (* cannot call OManager :( *)
        Printf.eprintf "You have different warning classes called %s:\n  %s\nand\n  %s\n%!"
          w.name
          w.doc
          w'.doc;
        assert false
      )
    with Not_found ->
      root_warning.children <- w :: root_warning.children
  )
  ;
  ()

let load_set set =
  Stack.iter load set

let set_warn_error_not_rec wclass b = wclass.err <- b
let set_warn_error wclass b = Traverse.iter (fun wclass -> set_warn_error_not_rec wclass b) wclass
let is_warn_error wclass = wclass.loaded && wclass.err

let set_warn_not_rec wclass b = wclass.enable <- b
let set_warn wclass b = Traverse.iter (fun wclass -> set_warn_not_rec wclass b) wclass
let is_warn wclass = wclass.loaded && wclass.enable

let get_doc wclass = wclass.doc
let get_name wclass = wclass.name

let get_from_path sl =
  let rec aux wclass = function
    | [] -> Some wclass
    | s :: tl ->
        aux (get_children wclass s) tl in
  try aux root_warning sl with Not_found -> None

let rec fold_with_env f acc env wclass =
  let acc, env = f acc env wclass in
  List.fold_left (fun acc wclass -> fold_with_env f acc env wclass) acc wclass.children

let fold f acc =
  fold_with_env
    (fun acc env wclass ->
       let path = env @ [wclass.name] in
       let acc =
         if wclass.loaded
         then f acc path wclass
         else acc
       in
       acc, path) acc [] root_warning

(* OManager depends on WarningClass *)
let printf = Printf.eprintf

module Arg =
struct
  module A = Base.Arg
  let find_set set b name =
    match Traverse.find (fun w -> String.compare name w.name = 0) root_warning with
    | Some w -> set w b
    | None ->
        printf "There is no such warning-class : '%s'\n" name ;
        printf "Hint: try '--warn-help'\n";
        exit 1

  let warn = find_set set_warn
  let warn_error = find_set set_warn_error

  let help () =
    (* Alphabetic order *)
    let extract w = w.name, w.doc, w.enable, w.err in
    let unsorted = Traverse.fold
      (fun acc w -> if w == root_warning || not w.loaded then acc
       else (extract w)::acc) [] root_warning in
    let sorted = (extract root_warning) :: (List.sort (fun (m, _, _, _) (n, _, _, _) -> String.compare m n) unsorted) in
    let pprt b = if b then "Printed" else "NotPrinted" in
    let perr b = if b then "Error" else "Warning" in
    let iter (name, doc, prt, err) =
      printf "+ %s: %s %s\n\t%s\n\n" name (pprt prt) (perr err) doc
    in
    printf "List of available warning-classes\n" ;
    printf "wclass: warn warn-error (default value)\n\n" ;
    List.iter iter sorted ;
    exit 0

  let options = [
    ("--warn", A.String (warn true), "<wclass> Activate a warning class");
    ("--no-warn", A.String (warn false), "<wclass> Deactivate a warning class");
    ("--warn-error", A.String (warn_error true), "<wclass> Warnings of the class will be considered as errors");
    ("--no-warn-error", A.String (warn_error false), "<wclass> Warnings of the class will not be considered as errors");
    ("--warn-help", A.Unit help, " Print the list of warning-classes");
    (*TODO: --warn-bash-completion *)
  ]
end

(* **********************************************)
(* ADD YOUR WARNING CLASS HERE ******************)
(* AND COMMENT MLI ******************************)
(* **********************************************)

(*
  GUIDELINES:
  -----------

  1) Beware, if your warning is meant to be public,
  think about what you write in the documentation.
  External user do not have access to sources,
  and to name of AST nodes, structures, libname,
  etc...

  2) Names :
  You must necessary use the hierachy.
  The constructor already implements the name concatenation
  for making the Arg command line,
  so do not repeat the name of the parent.

*)

let bsl =
  let doc = "Link with external primitives" in
  create ~name:"bsl" ~doc ~err:false ~enable:true ()

let bsl_backend_restriction =
  let doc = "Backend Restriction" in
  create ~parent:bsl ~name:"backend-restriction" ~doc ~err:false ~enable:true ()

let bsl_loading =
  let doc = "Plugin Loading" in
  create ~parent:bsl ~name:"loading" ~doc ~err:false ~enable:true ()

let bsl_projection =
  let doc = "External Primitive Projection" in
  create ~parent:bsl ~name:"projection" ~doc ~err:true ~enable:true ()

let bsl_register =
  let doc = "External Primitive Plugin Builder" in
  create ~parent:bsl ~name:"register" ~doc ~err:false ~enable:true ()

let bsl_type_checking =
  let doc = "Type checking" in
  create ~parent:bsl ~name:"type-checking" ~doc ~err:false ~enable:true ()

let bsl_unknown_bypass =
  let doc = "Unknown external primitive" in
  create ~parent:bsl ~name:"unknown-bypass" ~doc ~err:false ~enable:true ()

(* CONDITIONS *)

let cond =
  let doc = "Pre/Post condition violation" in
  create ~name:"cond" ~doc ~err:true ~enable:true ()


(* DbGen ******************************)
let dbgen =
  create ~name:"dbgen" ~doc:"Database resolution" ~err:false ~enable:true ()

let dbgen_schema =
  create ~parent:dbgen ~name:"schema" ~doc:"Database Schemas" ~err:false ~enable:true ()

let dbgen_mongo =
  create ~parent:dbgen ~name:"mongo" ~doc:"Database MongoDb code generation" ~err:false ~enable:true ()

let dbgen_postgres =
  create ~parent:dbgen ~name:"postgres" ~doc:"Postgres code generation" ~err:false ~enable:true ()

(* Explicit instantiation *************)
let ei =
  let doc = "Explicit instantiation warnings" in
  create ~name:"ei" ~doc ~err:false ~enable:true ()
let ei_generalize =
  let doc = "Generalize a non-functionnal value" in
  create ~name:"generalize" ~doc ~err:false ~enable:false ()

(* Phandler ***************************)
let phandler =
  let doc = "Pass system warnings" in
  create ~name:"phandler" ~doc ~err:false ~enable:true ()
let phandler_consistency =
  let doc = "Warn if the pass system is not consistent" in
  create ~parent:phandler ~name:"consistency" ~doc ~err:false ~enable:true ()

(* Commons ****************************)
let warn =
  let doc = "Commons warnings that can used at any step of compilations" in
  create ~name:"warn" ~doc ~err:false ~enable:true ()
let warn_olevel =
  let doc = "Warn if an optimization level is wrong" in
  create ~parent:phandler ~name:"consistency" ~doc ~err:false ~enable:true ()

(* Pattern Matching *******************)
let pattern =
  let doc = "Warnings related to pattern matching" in
  create ~name:"pattern" ~doc ~err:true ~enable:true ()
