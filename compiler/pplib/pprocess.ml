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
module StringMap = Map.Make(
  struct
    type t = string
    let compare = String.compare
  end)

(* Environment **********************************)
type env = string StringMap.t

let empty_env = StringMap.empty

let add_env = StringMap.add

let fill_with_sysenv t =
  Array.fold_left
    (fun t s ->
       match Str.split (Str.regexp "=") s with
       | var::values ->
           let value = String.concat " " values in
           add_env var value t
       | _ -> assert false)
    t (Unix.environment ())


(* Options **************************************)
type options = {
  env : env;
  output_suffix : string option (**Destination file*);
  force_static : bool;
}

let default_options env = {
  env = env;
  output_suffix = None;
  force_static = false;
}

(* Preprocess functions *************************)
exception PPParse_error of string

type lang_description = {
  open_com : string;
  close_com : string;
  open_block : string;
  close_block : string;
  debug_module : string;
}

type cond =
  | Test of string * string * string
  | Set of string

(* Represent an if *)
type pp_if = {
  cond : cond;
  if_ : pp_expr list;
  else_ : pp_expr list option;
}

(* Represent an pp expr *)
and pp_expr =
  | Normal of string
  | Ifstatic of pp_if
  | If of pp_if

(* Print code *)
let print_code ?(doeval=false) ?(eval=fun _ -> true) description buf code =
  let doeval = ref doeval in
  let open_com ~comment =
    if comment then Buffer.add_string buf description.open_com;
  in
  let close_com ~comment =
    if comment then Buffer.add_string buf description.close_com in
  let open_block () =
    Buffer.add_string buf description.open_block in
  let close_block () =
    Buffer.add_string buf description.close_block in
  let dmodule = description.debug_module in

  let rec print_expr ~comment = function
    | Normal x ->
        open_com ~comment;
        Buffer.add_string buf x;
        close_com ~comment;
    | Ifstatic if_ ->
        open_com ~comment;
        print_if ~comment `static if_;
        close_com ~comment;
    | If if_ ->
        print_if ~comment `dyn if_;

  and print_lexpr ~block ~comment l =
    open_com ~comment;
    if block then open_block ();
    List.iter (print_expr ~comment:false) l;
    if block then close_block ();
    close_com ~comment

  and print_if_cond ~comment s cond =
    match s with
    | `static ->
        let str =
          (match cond with
           | Set c -> Printf.sprintf "<Ifstatic:%s>" c
           | Test (_, c1, c2) -> Printf.sprintf "<Ifstatic:%s %s>" c1 c2) in
        open_com ~comment;
        Buffer.add_string buf str;
        close_com ~comment;
      | `dyn ->
          let dyntest =
            (match cond with
             | Set c ->
                 Printf.sprintf " if (%s.default) %s.%s then " dmodule dmodule
                   (String.lowercase c);
             | Test (t, c1, c2) ->
                 Printf.sprintf " if (%s.%s %s) %s.%s then " dmodule t c2 dmodule
                   (String.lowercase c1)) in
          let pptest =
            (match cond with
             | Set c ->
                 Printf.sprintf "<If:%s>" c;
             | Test (t, c1, c2) ->
                 Printf.sprintf "<If:%s$%s %s>" c1 t c2 ) in
          open_com ~comment:!doeval;
          Buffer.add_string buf pptest;
          close_com ~comment:!doeval;
          if !doeval then Buffer.add_string buf dyntest;

  and print_if ~comment s if_ =
    ignore (comment);
    let evaluated = eval if_.cond in
    print_if_cond ~comment:!doeval s if_.cond;
    let sv = !doeval in
    doeval := sv && evaluated;
    print_lexpr ~block:(`static != s) ~comment:(if s = `static then sv && not evaluated else false) if_.if_;
    doeval := sv && not evaluated;
    (let comment = if s = `static then sv && evaluated else false in
     match if_.else_ with
     | Some else_ ->
         if (s = `dyn) then Buffer.add_string buf " else ";
         open_com ~comment:sv;
         Buffer.add_string buf "<Else>";
         close_com ~comment:sv;
         print_lexpr ~block:(s = `dyn) ~comment else_;
     | None -> ());
    doeval := sv;
    open_com ~comment:!doeval;
    Buffer.add_string buf "<End>";
    close_com ~comment:!doeval;
    ()
  in
  print_lexpr ~block:false ~comment:false code

(* we avoid the dependency to libbase *)
let rec compute_line content pos pos_line line pos_max =
  let len = min pos_max (String.length content) in
  if pos < len then
    if
      content.[pos] = '\n' ||
      content.[pos] = '\r' && ( ( (pos<len-1) && content.[pos+1]<>'\n' ) ||
                                  ( (pos>1    ) && content.[pos-1]<>'\n' ) )
    then
      compute_line content (pos+1) (pos+1) (line+1) pos_max
    else
      compute_line content (pos+1) pos_line line pos_max
  else
    (line, pos-pos_line)

(* Parse a string *)
let parse filename content options =
  let pp_pos remain =
    let remain_size =
      List.fold_left (fun acc e ->
        match e with
        | Str.Delim s | Str.Text  s -> acc+String.length(s)
      ) 0 remain
    in
    let pos_max = (String.length content) - remain_size in
    let (line, pos) = compute_line content 0 0 0 pos_max in
    Printf.sprintf "File \"%s\", line %d, character %d (%d:%d-%d:%d)" filename line pos line pos line pos
  in
  let set_debugvar, get_debugvar =
    let dvar = ref None in
    (fun str -> dvar := Some str),
    (fun () ->
       match !dvar with
       | None -> failwith ("The debug variable doesn't exists")
       | Some s -> s) in
  let content =
    Str.full_split (Str.regexp "#<[^<>]*>") content in

  let if_regexp = Str.regexp "#<\\([^ :]*\\):\\([^>]*\\)>" in
  let cond1_regexp = Str.regexp "\\([^ ]*\\)\\$\\([^ ]*\\) \\([^ ]*\\)" in
  let cond2_regexp = Str.regexp "\\$\\([^ ]*\\) \\([^ ]*\\)" in
  let cond3_regexp = Str.regexp "\\([^ ]*\\) \\([^ ]*\\)" in
  let dvar_regexp = Str.regexp "#<Debugvar: *\\([^ ]*\\) *" in

  let error i lst =
        raise (PPParse_error (Format.sprintf "Error %s.\n%s" i (pp_pos lst)))
  in
  let unknown tag lst =
    error ("Unknown preprocessing directive "^tag^" (authorized only #<{If,Ifstatic,Else,End}>)") lst
  in

  let rec aux (result, lst) =
    match lst with
    | Str.Delim "#<Else>"::_
    | Str.Delim "#<End>"::_ -> (List.rev result), lst
    | Str.Delim tag::queue ->
        (try
           if Str.string_match dvar_regexp tag 0 then (
             set_debugvar (Str.matched_group 1 tag);
             aux (result, queue)
           ) else if tag = "#<If>" || Str.string_match if_regexp tag 0 then (
             let typif_ =
               if options.force_static then `static
               else if tag = "#<If>" || Str.matched_group 1 tag = "If" then
                 `dyn
               else if tag = "#<Ifstatic>" || Str.matched_group 1 tag = "Ifstatic" then
                 `static
               else unknown tag lst
             in
             let cond =
               if tag = "#<If>" || tag = "#<Ifstatic>"then(
                 Set (get_debugvar ())
               )else
                 let cond = Str.matched_group 2 tag in
                 if Str.string_match cond1_regexp cond 0 then(
                   Test (Str.matched_group 2 cond,
                         Str.matched_group 1 cond,
                         Str.matched_group 3 cond)
                 )else if Str.string_match cond2_regexp cond 0 then(
                   Test (Str.matched_group 1 cond,
                         get_debugvar (),
                         Str.matched_group 2 cond)
                 )else if Str.string_match cond3_regexp cond 0 then(
                   Test ("",
                         Str.matched_group 1 cond,
                         Str.matched_group 2 cond)
                 )else(
                   Set cond)
             in
             let if_, queue = aux ([], queue) in
             let else_, queue =
               match queue with
               | Str.Delim "#<Else>"::queue ->
                   let else_, queue = aux ([], queue) in
                   Some else_, queue
               | _ -> None, queue in
             (* End if *)
             (match queue with
              | Str.Delim "#<End>"::queue ->
                  let result =
                    let if_ = {cond = cond; if_ = if_; else_ = else_} in
                    (match typif_ with
                     |`static -> Ifstatic if_
                     |`dyn -> If if_)::result
                  in
                  aux (result, queue)
              | _ -> error "Expected end" lst)
           ) else unknown tag lst
         with | PPParse_error _ -> aux (result, (Str.Text tag)::queue)
        )

    | Str.Text normal::queue ->
        aux (Normal normal::result, queue)
    | _ -> (List.rev result), lst
  in match aux ([], content) with
  | content, [] -> content
  | _, (t::_ as lst) ->
      (match t with
       | Str.Delim _r
       | Str.Text _r -> error "Unfinished parsing" lst)

(* Process *)
let process ~name description options content =
  (* Parsing *)
  let content = parse name content options in
  (* Eval function *)
  let eval cond =
    try
      match cond with
      | Set name ->
          StringMap.mem name options.env
      | Test (_, name, value) ->
          let v = StringMap.find name options.env in
          v = value
    with Not_found -> false in
  (* Print and eval *)
  let buf = Buffer.create 1024 in
  print_code ~doeval:true ~eval description buf content;
  Buffer.contents buf

(* Generic executable *)
module Exe = struct

  let files = ref []

  let options = ref (default_options StringMap.empty)

  let speclist = [
    ("--force-static",
     Arg.Unit (fun() -> options := {!options with force_static = true}),
     "Force all if to be static");
    ("--output-suffix",
     Arg.String (fun s -> options := {!options with output_suffix = Some s}),
     "Output to files using the given suffix instead of stdout")
  ]

  let usage_msg = Printf.sprintf "%s: Simple preprocessor for the needs of the Opa compiler \nUsage: %s [options] <files>\n" Sys.argv.(0) Sys.argv.(0)

  let parse () =
    Arg.parse speclist
      (fun file -> files := file::!files)
      (usage_msg^"Options:")

  let content_of_ic ic =
    let len = 10000 in
    let str = Bytes.create len in
    let buf = Buffer.create 10000 in
    let rec aux () =
      let read = input ic str 0 len in
      if read <> 0 then (
        Buffer.add_substring buf str 0 read;
        aux ()
      ) in
    aux ();
    buf

  (* Get a file content (cc from File) *)
  let content f =
    let stat = Unix.stat f in
    match stat.Unix.st_kind with
    | Unix.S_DIR -> failwith (Printf.sprintf "%S is a directory" f)
    | Unix.S_LNK -> assert false (* stat goes through symbolic links *)
    | Unix.S_CHR  (* Character device *)
    | Unix.S_BLK  (* Block device *)
    | Unix.S_FIFO  (* Named pipe *)
    | Unix.S_SOCK  (* Socket *) ->
        (* for these kind of files, the size information is meaningless *)
        let ic = open_in_bin f in
        let buf = content_of_ic ic in
        close_in ic;
        Buffer.contents buf
    | Unix.S_REG  (* Regular file *) ->
        let size = stat.Unix.st_size in
        assert (size <= Sys.max_string_length) ;
        let ic = open_in_bin f
        and buf = Bytes.create size in
        really_input ic buf 0 size ;
        close_in ic ;
        buf

  let run description =
    parse ();
    let options =
      let options = !options in
      { options with env = fill_with_sysenv options.env } in
    if !files = [] then
      let buf = Buffer.contents (content_of_ic stdin) in
      let result = process ~name:"/dev/stdin" description options buf in
      output_string stdout result
    else
    let rec aux files =
      match files with
      | t::q ->
          begin
            let result = process ~name:t description options (content t) in
            match options.output_suffix with
            | None   -> output_string stdout result
            | Some s ->
                let out = open_out (t^s) in
                output_string out result;
                close_out out;
                aux q
          end
      | [] -> ()
    in aux (List.rev !files)
end
