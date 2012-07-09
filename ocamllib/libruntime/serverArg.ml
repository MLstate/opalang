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
let print_err fmt = Printf.fprintf stderr fmt
  (* The logger is not yet set. And for invocation messages, better not use it *)

type args = string list

let get_argv, set_argv =
  let argv = ref (match Array.to_list Sys.argv with [] -> [] | _0::args -> args) in
  (fun () -> !argv),
  (fun x -> argv := x)

let is_empty args = [] = args

let to_list args = args
let from_list args = args

let argv_to_string () = String.concat " " (get_argv())

(* ------------------------------------------------------------ *)
(* Low-level parsers                                            *)
(* ------------------------------------------------------------ *)

type 'a param_parser = args -> 'a option * args * args
  (* Returns acc, skipped args, remaining args. Arguments parsed are not returned *)

let failopt f x = try Some (f x) with Failure _ -> None

let int = function
  | x::args -> failopt int_of_string x, [], args
  | [] -> None, [], []

let string = function
  | x::args when (String.length x = 0 || x.[0] <> '-') -> Some x, [], args
  | _ -> None, [], []

let anystring = function
  | x::args -> Some x, [], args
  | [] -> None, [], []

let float = function
  | x::args -> failopt float_of_string x, [], args
  | [] -> None, [], []

let bool = function
  | x::args -> failopt bool_of_string x, [], args
  | [] -> None, [], []

let unit args = Some (), [], args

let stringset list = function
  | x::args -> (try Some (List.assoc x list), [], args with Not_found -> None, [], args)
  | [] -> None, [], []

let list separator parse = function
  | [] -> Some [], [], []
  | x::args ->
      try
        Some (
          List.map
            (fun x -> match parse [x] with  Some x, [], [] -> x | _ -> raise Exit)
            (Base.String.slice separator x)
        ),
        [], args
      with Exit ->
        None, [], args


let option parse args = match parse args with
  | (Some _) as x, skipped, args -> Some x, skipped, args
  | _ -> Some None, [], args

let pair pa pb args = match pa args with
  | None, skipped, args -> None, skipped, args
  | Some x, skipped, args -> match pb args with
    | None, skipped2, args -> None, skipped@skipped2, args
    | Some y, skipped2, args -> Some (x,y), skipped@skipped2, args

let check parse f args = match parse args with
  | None, skipped, args -> None, skipped, args
  | Some x, skipped, args2 ->
      if f x then Some x, skipped, args2
      else None, [], args

let keep parse args =
  let res, _skipped, _args = parse args in
  res, [], args

let wrap param f args = match param args with
  | Some x, skipped, args -> Some (f x), skipped, args
  | None, skipped, args -> None, skipped, args

let wrap_opt param f args = match param args with
  | None, skipped, args -> None, skipped, args
  | Some x, skipped, args2 -> match f x with
    | None -> None, [], args
    | res -> res, skipped, args2

let func param f = fun acc -> fun args -> wrap param (f acc) args

let func_opt param f = fun acc -> fun args -> wrap_opt param (f acc) args

let skip = function
  | x::args -> Some (), [x], args
  | [] -> None, [], []

let skip_all args = Some (), args, []

let rec fold effect acc = function
  | [] -> Some acc, [], []
  | args ->
      match effect acc args with
      | Some acc, skipped, args ->
          let acc, skipped2, args = fold effect acc args in
          acc, skipped @ skipped2, args
      | _ -> Some acc, [], args

let fold_until str effect acc =
  fold
    (fun acc -> function
     | [] -> Some acc,[],[]
     | (x::_) as args -> if x = str then Some acc,args,[] else effect acc args)
    acc

let rec fold_all effect acc remainings_args =
  match remainings_args with
  | [] -> Some acc, [], []
  | arg::rest as args->
      match effect acc args with
      | Some acc, skipped, args ->
          let acc, skipped2, args = fold_all effect acc args in
          acc, skipped @ skipped2, args
      | None, skipped, _args ->
          let acc, skipped2, args = fold_all effect acc rest in
          acc, arg :: skipped @ skipped2, args

let skip_str str effect = function
  | [] -> Some None, [], []
  | (x::r) as args ->
      if x = str then option effect r
      else if List.mem str args then None, [], args
      else option effect args

let push str args = Some (), [str], args


(**
 * {1 Alternative API}
 *)
type 'a state = No_more_params of 'a
              | More_params    of 'a
              | Maybe_params   of 'a

type 'a instruction = 'a state option


let make_arg_parser ~(names: string list) ~(param_doc:string) ~(doc:string) ~(initialize:'a(*float*) -> 'a(*float*) state) ~(step:'a(*float*) -> string -> 'a(*float*) instruction)  =
  let rec parse_params (state: 'a(*float*) state) args used : ('a(*float*) option * args * args) =
    match state with
    | No_more_params x -> (*Nothing to do, so job finished*)
        (Some x, [], args)
    | Maybe_params x   ->
        begin
          match args with
          | [] ->             (*No args left, so job finished*)
              (Some x, [], args)
          | h::t ->
              match step x h with
              | None ->                     (*Optional arg doesn't parse, so job finished*)
                  (Some x, [], args)
              | Some (No_more_params x) ->  (*This was the last arg, job finished*)
                  (Some x, [], t)
              | Some i ->                   (*Otherwise, continue job*)
                  parse_params i t (h::used)
        end
    | More_params x ->
        begin
          match args with
          | [] ->                           (*No args left, this is a failure*)
              (None, used, [])
          | h::t ->
              match step x h with
              | None ->                     (*Arg doesn't parse, this is a failure*)
                  (None, used, args)
              | Some (No_more_params x) ->
                  (Some x, [], t)
              | Some i ->                   (*So far, so good*)
                  parse_params i t (h::used)
        end
  in
  (*let unwrap = function No_more_params x | Maybe_params x | More_params x -> x in
  let parse (state:'a(*float*)) args = match parse_params (initialize state) args [] with
    | (None,   skipped, rest) -> (None, skipped, rest)
    | (Some x, skipped, rest) -> (Some (unwrap x), skipped, rest)*)
  let parse state args = parse_params (initialize state) args []
  in
  (names, parse, param_doc, doc)


(* ------------------------------------------------------------ *)
(* High-level parsers                                           *)
(* ------------------------------------------------------------ *)

type 'a arg_parser = string list * ('a -> 'a param_parser) * string * string

let rec pp_justify f s =
  let a,b = Base.String.split_char ' ' s in
  Format.pp_print_string f a;
  if b <> "" then
    (Format.pp_print_space f ();
     pp_justify f b)

let doc_string title speclist =
  let rec pplist f = function
    | s::[] -> Format.pp_print_string f s
    | s::r -> Format.pp_print_string f s; Format.pp_print_space f (); pplist f r
    | [] -> ()
  in
  Format.fprintf Format.str_formatter "@[<v4>%s:@\n%a@]@."
    (String.capitalize title)
    (fun f ->
       List.iter
         (fun (names,_,params_doc,doc) ->
            Format.fprintf f "@[<v>@[<hov>%a%a@]@;<1 12>@[<hov>%a@]@]@," pplist names
              (fun f s -> if s <> "" then Format.fprintf f "@;<1 4>%s" s) params_doc
              pp_justify doc))
    speclist;
  Format.flush_str_formatter ()

(** write a simple manpage from serverArg specs *)
(* CAVEAT: unfortunately many Opa programs install _several_ arg parsers by top-level side effects. This function should be generalized to allow several sections of options to reflect this fact. *)
let write_simple_manpage
    ?(nohelp=false)
    ~cmdname ~section
    ?centerfooter
    ?leftfooter ?centerheader
    ?summary ?synopsis ?description ?options ?(other=[])
    file
    =
  let print_spec buf (names,_,params_doc,doc) =
    let names_str = List.fold_left (fun str name -> str ^ (BaseString.replace name "-" "\\-") ^ " ") "" names
    in
    Printf.bprintf buf ".TP\n%s%s\n%s\n" names_str params_doc doc
  in
  let help_dummy_spec = (["--help"; "-help"; "-h"; "-?"], (fun _ -> failwith "help_dummy_spec"), "", "Print this help")
  in
  let options_str =
    begin match options with
      None -> None
    | Some(speclist) ->
      let buf = Buffer.create 10
      in
      List.iter (print_spec buf) (if nohelp then speclist else speclist@[help_dummy_spec]);
      Some(Buffer.contents buf)
    end
  in
  BaseArg.write_simple_manpage
    ~cmdname ~section
    ?centerfooter
    ?leftfooter ?centerheader
    ?summary ?synopsis ?description ~other:(match options_str with None -> other | Some(str) -> ("OPTIONS", str)::other)
    file

let make_parser ?(final=false) ?(nohelp=false) title speclist acc0 args0 =
  let rec do_args (acc,rev_args) = function
    | [] -> (Some acc, List.rev rev_args, [])
    | arg::args ->
        let rec do_specs = function
          | (names,effect,params_doc,_doc)::specs ->
              if List.mem arg names then
                match effect acc args with
                | Some acc, skipped_args, args ->
                    do_args (acc, List.rev_append skipped_args rev_args) args
                | _ ->
                    print_err "Invalid parameter for option %s, in %s. Syntax:\n    %s %s\n"
                      arg title arg params_doc;
                    raise Exit
              else
                do_specs specs
          | [] ->
              if not(nohelp) && (arg = "--help" || arg = "-help" || arg = "-h" || arg = "-?") then
                (print_err "%s" (doc_string title speclist);
                 if final then raise Exit
                 else Some acc0,args0,[])
              else
                do_args (acc, arg::rev_args) args
        in
        do_specs speclist
  in
  do_args (acc0,[]) args0

let filter_functional args acc parse =
  match parse acc args with
  | None, _, _ -> acc, args
  | Some acc, skipped_args, args -> acc, skipped_args @ args

let filter acc parse =
  let args = get_argv() in
  match parse acc args with
  | None, _, _ -> acc
  | Some acc, skipped_args, args ->
      set_argv (skipped_args @ args);
      acc

let extract_prefix pfx =
  let rec do_args (take,leave) args = match args with
    | [] -> List.rev take, List.rev leave
    | arg::rest ->
        if arg = "--" then List.rev take, List.rev_append leave args
        else if arg = "--help" then do_args (arg::take, arg::leave) rest
        else if Base.String.is_prefix pfx arg then
          match rest with
          | param::rest2 when not (Base.String.is_prefix "-" param) ->
              do_args (param::arg::take, leave) rest2
          | _ ->
              do_args (arg::take, leave) rest
        else do_args (take, arg::leave) rest
  in
  let args = get_argv() in
  let take,leave = do_args ([],[]) args in
  set_argv leave;
  take

(* ------------------------------------------------------------ *)
(* Pre-defined parsers                                          *)
(* ------------------------------------------------------------ *)
let parse_addr_raw str =
  let host,port = Base.String.split_char_last ':' str in
  try
    let portopt =
      if port = "" then None
      else
        let p = int_of_string port in
        if p < 0xffff then Some p
        else
          failwith "Port number is too high: "
    in
    Some ((Unix.gethostbyname host).Unix.h_addr_list.(0), portopt)
  with
  | Failure s -> prerr_endline ("Error: invalid port. "^s^port); None
  | Not_found -> prerr_endline ("Error: host not found: "^host); None
let parse_addr =
  wrap_opt string parse_addr_raw


(* ------------------------------------------------------------ *)
(* Binding with arg                                             *)
(* ------------------------------------------------------------ *)

module A = Base.Arg
module String = Base.String

let (!>) tag f = func tag (fun acc arg -> f arg; acc)

let import_arg_spec = function
  | A.Unit f -> !> unit f
  | A.Bool f -> !> bool f
  | A.Set r -> !> unit (fun ()  -> r := true)
  | A.Clear r -> !> unit (fun ()  -> r := false)
  | A.String f -> !> string f
  | A.Set_string r -> !> string (fun s -> r := s)
  | A.Int f -> !> int f
  | A.Set_int r -> !> int (fun i -> r := i)
  | A.Float f -> !> float f
  | A.Set_float r -> !> float (fun f -> r := f)
  | A.Symbol (_l, f) -> !> string f

  (* The rest is not implemented, you can add it if you need *)
  | _ -> assert false

let import_arg_opt (key, spec, doc) =
  let spec = import_arg_spec spec in
  let arg_doc, doc = String.split_char ' ' doc in
  [key], spec, arg_doc, doc

let import_arg_options opts = List.map import_arg_opt opts
