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

module String = Base.String
module List = Base.List
module PI = Parse_idl
module O = Ocaml
module Cons = O.Cons

let testfile = false

let debug = ref false
let verbose = ref (!debug)
let module_name = ref ""
let output_suffix = ref "types"
let bsl_prefix = ref "bsl"
(*let encoding_number = ref 2*)
let ocaml_wrap_opt = ref true
let opa_wrap_opt = ref true
let native_parser = ref true
let hlnet_logging = ref false
let protocol_version = ref 1
let default_port = ref 49152
let default_addr = ref "Unix.inet_addr_loopback"
let logger_function = ref "Logger.log"
let create_functions = ref true
let tojson_functions = ref true
let fromjson_functions = ref true
let string_functions = ref true
let bsl_file = ref true
let no_ocaml = ref false
let no_opa = ref false

let mns = ref ""
let _Mns = ref ""
let bslmns = ref ""
let base = ref ""

type opts = {
    opt_debug : bool;
    opt_verbose : bool;
    opt_module_name : string;
    opt_output_suffix : string;
    opt_bsl_prefix : string;
    (*opt_encoding_number : int;*)
    opt_ocaml_wrap_opt : bool;
    opt_opa_wrap_opt : bool;
    opt_native_parser : bool;
    opt_hlnet_logging : bool;
    opt_protocol_version : int;
    opt_default_port : int;
    opt_default_addr : string;
    opt_logger_function : string;
    opt_create_functions : bool;
    opt_tojson_functions : bool;
    opt_fromjson_functions : bool;
    opt_string_functions : bool;
    opt_bsl_file : bool;
    opt_no_ocaml : bool;
    opt_no_opa : bool;
    opt_mns : string;
}

let default_opts = ref {
  opt_debug = false;
  opt_verbose = false;
  opt_module_name = "";
  opt_output_suffix = "types";
  opt_bsl_prefix = "bsl";
  (*opt_encoding_number = 2;*)
  opt_ocaml_wrap_opt = true;
  opt_opa_wrap_opt = true;
  opt_native_parser = true;
  opt_hlnet_logging = false;
  opt_protocol_version = 1;
  opt_default_port = 49152;
  opt_default_addr = "Unix.inet_addr_loopback";
  opt_logger_function = "Logger.log";
  opt_create_functions = true;
  opt_tojson_functions = true;
  opt_fromjson_functions = true;
  opt_string_functions = true;
  opt_bsl_file = true;
  opt_no_ocaml = false;
  opt_no_opa = false;
  opt_mns = "";
}

let save_opts () = {
    opt_debug = !debug;
    opt_verbose = !verbose;
    opt_module_name = !module_name;
    opt_output_suffix = !output_suffix;
    opt_bsl_prefix = !bsl_prefix;
    (*opt_encoding_number = !encoding_number;*)
    opt_ocaml_wrap_opt = !ocaml_wrap_opt;
    opt_opa_wrap_opt = !opa_wrap_opt;
    opt_native_parser = !native_parser;
    opt_hlnet_logging = !hlnet_logging;
    opt_protocol_version = !protocol_version;
    opt_default_port = !default_port;
    opt_default_addr = !default_addr;
    opt_logger_function = !logger_function;
    opt_create_functions = !create_functions;
    opt_tojson_functions = !tojson_functions;
    opt_fromjson_functions = !fromjson_functions;
    opt_string_functions = !string_functions;
    opt_bsl_file = !bsl_file;
    opt_no_ocaml = !no_ocaml;
    opt_no_opa = !no_opa;
    opt_mns = !mns;
}

let restore_opts {
    opt_debug;
    opt_verbose;
    opt_module_name;
    opt_output_suffix;
    opt_bsl_prefix;
    (*opt_encoding_number;*)
    opt_ocaml_wrap_opt;
    opt_opa_wrap_opt;
    opt_native_parser;
    opt_hlnet_logging;
    opt_protocol_version;
    opt_default_port;
    opt_default_addr;
    opt_logger_function;
    opt_create_functions;
    opt_tojson_functions;
    opt_fromjson_functions;
    opt_string_functions;
    opt_bsl_file;
    opt_no_ocaml;
    opt_no_opa;
    opt_mns;
} =
    debug := opt_debug;
    verbose := opt_verbose;
    module_name := opt_module_name;
    output_suffix := opt_output_suffix;
    bsl_prefix := opt_bsl_prefix;
    (*encoding_number := opt_encoding_number;*)
    ocaml_wrap_opt := opt_ocaml_wrap_opt;
    opa_wrap_opt := opt_opa_wrap_opt;
    native_parser := opt_native_parser;
    hlnet_logging := opt_hlnet_logging;
    protocol_version := opt_protocol_version;
    default_port := opt_default_port;
    default_addr := opt_default_addr;
    logger_function := opt_logger_function;
    create_functions := opt_create_functions;
    tojson_functions := opt_tojson_functions;
    fromjson_functions := opt_fromjson_functions;
    string_functions := opt_string_functions;
    bsl_file := opt_bsl_file;
    no_ocaml := opt_no_ocaml;
    no_opa := opt_no_opa;
    mns := opt_mns

let printf = Printf.printf
let eprintf = Printf.eprintf
let fprintf = Printf.fprintf
let sprintf = Printf.sprintf

let rec ll l = match l with | [] -> [] | [_] -> [] | h::t -> h::(ll t)
let lc1 s =  let ss = String.copy s in ss.[0] <- (Char.lowercase ss.[0]); ss
let ispfx pfxs name = List.exists (fun pfx -> String.is_prefix pfx name) pfxs
let name_of_prefix prefixes str =
  List.fold_left (fun name pfx -> if String.is_prefix pfx name then String.remove_prefix pfx name else name) str prefixes

let opt () = if !ocaml_wrap_opt then " option" else ""
let optS () = if !ocaml_wrap_opt then "Some " else ""
let fail () = if !ocaml_wrap_opt then "None" else "raise (Failure \"parse_error\")"

let string_of_idl = function
  | PI.IDLType (name,type_expr,None) -> sprintf "IDLType (%s,%s)" name (Tools.str_of_type_expr type_expr)
  | PI.IDLType (name,type_expr,Some s) -> sprintf "IDLType (%s,%s,\"%s\")" name (Tools.str_of_type_expr type_expr) s
  | PI.IDLSar (name,sndtype,rcvtype) -> sprintf "IDLSar (%s,%s,%s)" name sndtype rcvtype
  | PI.IDLLet expr -> sprintf "IDLLet %s" (Tools.str_of_expr expr)

(* Handlers for typenames:
 * Complicated by the presence of external types which are encoded in
 * the type name as: ["external";<name>;[<ocaml type name (list)>]]
 *)

type tn = string list
type ext = (string * string) list
type exts = (string * ext) list
type tyexts = tn list * exts

let string_of_tyns tyns = String.concat ", " (List.map (fun itn -> (String.concat "." itn)) tyns)
let string_of_tynames tynames = "["^(string_of_tyns tynames)^"]"
let string_of_al ?(sep1="; ") ?(sep2="=>") vtos al =
  String.concat sep1 (List.map (fun (k,v) -> sprintf "%s%s%s" k sep2 (vtos v)) al)
let string_of_exts = string_of_al (fun l -> "["^string_of_al (fun s -> s) l^"]")
let string_of_tyexts ((tyns,exts):tyexts) = sprintf "([%s],<%s>)" (string_of_tyns tyns) (string_of_exts exts)

let opa_tyn tn ((tyns,exts):tyexts) =
  match tn with
  | [tn] -> [tn]
  | [mn;tn] -> [(lc1 mn);tn]
  | "external"::_ -> failwith "external type expected for OPA type name"
  | _ -> assert false

let str_of_tyn = function
  | "external"::_::rest -> String.concat "." rest
  | tyn -> String.concat "." tyn

let tyn_base = function
  | [tn] -> tn
  | [_; tn] -> tn
  | "external"::t::_ -> t
  | _ -> assert false

let cmp_tyn tn1 tn2 = tyn_base tn1 = tyn_base tn2

type tyntype =
    ExternalTyn of (tn * ext)
  | InternalTyn of tn
  | UnknownTyn

let get_tyn tn ((tyns,exts):tyexts) =
  let tnb = tyn_base tn in
  match List.find_opt (fun (name,_) -> name = tnb) exts, List.find_opt (fun tyn -> cmp_tyn tn tyn) tyns with
  | (Some (_,ext),Some tn) -> ExternalTyn (tn,ext)
  | (None,Some tn) -> InternalTyn tn
  | _ -> UnknownTyn

let var_of_tyn tyn = "__"^(tyn_base tyn)

(* Module naming routines.
 * Centralised here because we have to be able to access all of these
 * from all phases.
 *)

let rmv_suffix from_suffix filename =
  if Filename.check_suffix filename from_suffix
  then Filename.chop_suffix filename from_suffix
  else filename

let getbase filename from_suffix =
  if !module_name = ""
  then rmv_suffix from_suffix (Filename.basename filename)
  else !module_name

let setsuffix filename prefix from_suffix to_suffix extension =
  let d = Filename.dirname filename in
  let base = getbase filename from_suffix in
  Filename.concat d ((Tools.add_prefix (Tools.add_suffix base to_suffix) prefix)^extension)

let modnamesuffix from_suffix filename = Tools.add_suffix (getbase filename from_suffix) !output_suffix

let prefixmodname prefix from_suffix filename = Tools.add_prefix (modnamesuffix from_suffix filename) prefix

let output_header oc filename intro cs ce =
  let tim = Time.localtime (Time.now ()) in
  fprintf oc "%s Translated from %s\n%s Date: %s %s\n %s\n\n%s %s %s\n\n%!"
             cs filename (if ce = "" then cs else " *") (Date.date2 tim) (Date.time tim) ce cs intro ce

let brre = Str.regexp "\\([{}]\\)"
let toopastr = Str.global_replace brre "\\\\\\1"

(* Generate OCaml types *)

let type_ocaml_cte = function
  | O.TypeString -> "string"
  | O.TypeInt -> "int"
  | O.TypeInt64 -> "int64"
  | O.TypeFloat -> "float"
  | O.TypeBool -> "bool"
  | O.TypeChar -> "char"
  | O.TypeUnit -> "unit"

let rec type_ocaml_te tyns = function
  | O.TypeName ([te],["option"]) -> sprintf "(%s) option" (type_ocaml_te tyns te)
  | O.TypeName ([te],["list"]) -> sprintf "(%s) list" (type_ocaml_te tyns te)
  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (*eprintf "n: %s\n%!" n;
      eprintf "tyns: %s\n%!" (string_of_tyexts tyns);*)
      (match get_tyn tn tyns with
       | InternalTyn tn
       | ExternalTyn (tn,_) ->
           if tes = []
           then sprintf "%s" (str_of_tyn tn)
           else sprintf "%s %s" (String.concat "," (List.map (type_ocaml_te tyns) tes)) (str_of_tyn tn)
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))
  | O.TypeConst cte -> type_ocaml_cte cte
  | O.TypeTuple tes ->
      let args = List.map (fun te -> type_ocaml_te tyns te) tes in
      sprintf "%s" (String.concat " * " args)
  | O.TypeRecord l ->
      let els = List.map (fun (_,lab,te) -> sprintf "%s:%s" lab (type_ocaml_te tyns te)) l in
      sprintf "{%s}" (String.concat "; " els)
  | O.TypeConstructor cl ->
      let cons = List.map (fun (name,teo) ->
                             match teo with
                             | Some te -> sprintf "%s of %s" name (type_ocaml_te tyns te)
                             | None -> sprintf "%s" name
                          ) cl in
      sprintf "%s" (String.concat " | " cons)

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Generate OCaml types *)

(* Generate OPA types *)

let type_opa_cte = function
  | O.TypeString -> "string"
  | O.TypeInt -> "int"
  | O.TypeInt64 -> "int64"
  | O.TypeFloat -> "float"
  | O.TypeBool -> "bool"
  | O.TypeChar -> "char"
  | O.TypeUnit -> "void"

let rec type_opa_te tyns = function
  | O.TypeName ([te],["option"]) -> sprintf "option(%s)" (type_opa_te tyns te)
  | O.TypeName ([te],["list"]) -> sprintf "list(%s)" (type_opa_te tyns te)
  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (*eprintf "type_opa_te: TypeName(%s,%s)\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;*)
      (match get_tyn tn tyns with
       | InternalTyn [tn] ->
           if tes = []
           then sprintf "%s.%s" !mns tn
           else sprintf "%s.%s(%s)" !mns tn (String.concat "," (List.map (type_opa_te tyns) tes))
       | InternalTyn [mn;tn] ->
           let on = str_of_tyn (opa_tyn [mn;tn] tyns) in
           if tes = []
           then sprintf "%s" on
           else sprintf "%s(%s)" on (String.concat "," (List.map (type_opa_te tyns) tes))
       | ExternalTyn (_,ext) ->
           (try List.assoc "opatype" ext
            with Not_found -> failwith (sprintf "No OPA type defined for external type %s" n))
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))
  | O.TypeConst cte -> type_opa_cte cte
  | O.TypeTuple tes ->
      let args = List.map (fun te -> type_opa_te tyns te) tes in
      sprintf "(%s)" (String.concat "," args)
  | O.TypeRecord l ->
      let els = List.map (fun (_,lab,te) -> sprintf "%s:%s" lab (type_opa_te tyns te)) l in
      sprintf "{%s}" (String.concat "; " els)
  | O.TypeConstructor cl ->
      let cons = List.map (fun (name,teo) ->
                             match teo with
                             | Some te -> sprintf "{%s:%s}" name (type_opa_te tyns te)
                             | None -> sprintf "{%s}" name
                          ) cl in
      sprintf "%s" (String.concat " / " cons)

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Generate OPA types *)

(* Generate OPA arg types *)

let opa_arg_type_cte = function
  | O.TypeString -> "opa[string]"
  | O.TypeInt -> "opa[int]"
  | O.TypeInt64 -> "opa[int]"
  | O.TypeFloat -> "opa[float]"
  | O.TypeBool -> "opa[bool]"
  | O.TypeChar -> "opa[char]"
  | O.TypeUnit -> "opa[void]"

let rec opa_arg_type_te name = function
  | O.TypeConst cte -> opa_arg_type_cte cte
  | _ -> sprintf "opa[%s_%s]" !mns name

(* End of Generate OPA arg types *)

(* Generate ServerLib types *)

let type_sl_cte = function
  | O.TypeString -> "ServerLib.ty_string"
  | O.TypeInt -> "ServerLib.ty_int"
  | O.TypeInt64 -> "ServerLib.ty_int"
  | O.TypeFloat -> "ServerLib.ty_float"
  | O.TypeBool -> "ServerLib.ty_bool"
  | O.TypeChar -> "ServerLib.ty_char"
  | O.TypeUnit -> "ServerLib.ty_void"

let rec type_sl_te tyns withmod = function
  | O.TypeName ([te],["option"]) -> "ServerLib.ty_record"
  | O.TypeName ([te],["list"]) -> "ServerLib.ty_record"
  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (match get_tyn tn tyns with
       | InternalTyn [tn] ->
           let m = if withmod then (!_Mns)^"." else "" in
           if tes = []
           then sprintf "%ssl_%s" m tn
           else sprintf "%ssl_%s(%s)" m tn (String.concat "," (List.map (type_sl_te tyns withmod) tes))
       | InternalTyn [mn;tn] ->
           let on = str_of_tyn (opa_tyn [mn;tn] tyns) in
           if tes = []
           then sprintf "sl_%s" on
           else sprintf "sl_%s(%s)" on (String.concat "," (List.map (type_sl_te tyns withmod) tes))
       | ExternalTyn (_,_) -> "ServerLib.ty_record"
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))
  | O.TypeConst cte -> type_sl_cte cte
  | O.TypeTuple tes -> "ServerLib.ty_record"
  | O.TypeRecord l -> "ServerLib.ty_record"
  | O.TypeConstructor cl -> "ServerLib.ty_record"

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Generate ServerLib types *)

(* Encoding descriptor *)

type lang = {
  make_some : string -> string;
  make_none : string;
  make_cons : string -> string option -> string;
  map_list : string -> string -> string;
  map_while_opt : string -> string -> string;
  list_sep : string;
  flush_str : encoding -> string -> string -> string;
  output_s : encoding -> string -> string -> string -> string;
  output_v : encoding -> string -> string -> string -> string -> string;
  output_a : encoding -> string -> string -> string -> string;
  output_f : encoding -> string -> string -> string -> string;
  output_m : encoding -> string -> string -> string -> (string * (string -> string)) list -> string;
  output_l : encoding -> string -> string -> string -> (string -> string -> string) -> (string -> string) -> string;
  make_ext_fn : encoding -> tyexts -> tn -> string;
  pat_tup : string list -> string;
  tup_v : O.type_expr -> string;
  pat_rec : encoding -> string list -> string list -> string;
  pat_con0 : string -> string; pat_con1 : string -> string -> string;
  error : string -> string;
  mtch : encoding -> string  -> (string * string) list -> string;
  fnctn : encoding -> (string * string) list -> string;
  app : encoding -> string -> string -> string;
}

(* TODO: use this... *)
(*and prepost = {
  first_pre : encoding -> string;
  pre : string;
  make : string -> string;
  void : string;
  sep1 : string;
  sep2 : string;
  post : string;
  last_post : string;
}*)

and enc = {
  string_to_string : string*string;
  int_to_string : string*string;
  int64_to_string : string*string;
  float_to_string : string*string;
  bool_to_string : string*string;
  char_to_string : string*string;
  unit_to_string : string*string;
  option_pre : string; option_post : string;
  list_pre : string; list_post : string;
  tuple_pre : int -> string; tuple_post : string;
  record_pre : encoding -> string; record_post : string;
  cons_pre : string; cons_post : string;
  con_pre : string; con_sep : string; con_post : string; make_con : string -> string;
  rec_pre : string; rec_sep1 : string; rec_sep2 : string; rec_post : string; make_rec : string -> string; rec_void : string;
  tup_pre : string; tup_sep1 : string; tup_sep2 : string; tup_post : string; make_tup : string -> string;
  lst_pre : string; lst_sep1 : string; lst_sep2 : string; lst_post : string;
  (*rcrd : prepost;*)
}

and prs = {
  wrap_opt : bool ref;
  succeed : encoding -> string -> string;
  fail : encoding -> string -> string;
  string_pat : string*string*string;
  int_pat : string*string*string;
  int64_pat : string*string*string;
  float_pat : string*string*string;
  bool_pat : string*string*string;
  char_pat : string*string*string;
  unit_pat : string*string*string;
  opt_pat : string option -> string;
  list_pat : string -> string;
  tup_field : int -> string;
  pre_tup : (encoding -> int -> string -> string) option;
  tuple_pat : int -> string;
  pre_rec : (encoding -> string -> string) option;
  record_pat : string list -> string;
  cons_pat : string -> string option -> (string option) * string;
  input_v : encoding -> string * string * string -> string;
}

and encoding = {
  (* Internal data structures *)
  foldstrs : bool;
  str : string ref;
  modname : string;

  (* Name prefix for generated functions *)
  prefix : string;

  (* Define host language constructs *)
  lang : lang;

  (* Define encoding *)
  enc : enc;

  (* Define input parser *)
  prs : prs;
}

(* Generic support routines *)

let flush_str enc o t =
  if enc.foldstrs && !(enc.str) <> ""
  then
    let t = enc.lang.flush_str enc o t in
    enc.str := ""; t
  else t

let make_ext enc o t tyexts tyn =
  enc.lang.app enc (enc.lang.app enc (enc.lang.make_ext_fn enc tyexts tyn) (flush_str enc o t)) (var_of_tyn tyn)

let make_ext2 enc tyexts tyn t = enc.lang.app enc (enc.lang.make_ext_fn enc tyexts tyn) t

let make_ext3 enc o t tyexts tyn =
  enc.lang.app enc (enc.lang.app enc o (flush_str enc o t))
                   (enc.lang.app enc (enc.lang.make_ext_fn enc tyexts tyn) (var_of_tyn tyn))

let make_ext4 enc tyexts tyn t =
  enc.lang.app enc
    (enc.lang.app enc "input_external" t)
    (enc.lang.make_ext_fn enc tyexts tyn)

let make_ext5 enc o t tyexts tyn =
  enc.lang.output_v enc (var_of_tyn tyn) o t (enc.lang.make_ext_fn enc tyexts tyn)

let output_br enc o t l r f a = enc.lang.output_s enc o (f (enc.lang.output_s enc o t l) a) r

let rec output_seq enc i o f sep t = function
  | [] -> t
  | [v] -> f i t v
  | v::vs -> output_seq enc (i+1) o f sep (enc.lang.output_s enc o (f i t v) sep) vs

let make_tup enc name value _ =
  sprintf "%s%s%s%s%s%s%s"
    (enc.enc.tuple_pre 0) enc.enc.rec_pre (enc.enc.make_tup name) enc.enc.tup_sep1 value enc.enc.tup_post enc.enc.tuple_post

let make_tups enc nvs =
  sprintf "%s%s%s"
    (enc.enc.tuple_pre (List.length nvs))
    (String.concat enc.enc.tup_sep2
       (List.map (fun (name,value) ->
                    sprintf "%s%s%s%s%s" enc.enc.tup_pre (enc.enc.make_tup name) enc.enc.tup_sep1 value enc.enc.tup_post) nvs))
    enc.enc.tuple_post

let make_rec enc name value _ =
  sprintf "%s%s%s%s%s%s%s"
    (enc.enc.record_pre enc) enc.enc.rec_pre (enc.enc.make_rec name) enc.enc.rec_sep1 value enc.enc.rec_post enc.enc.record_post

let make_recs enc nvs =
  sprintf "%s%s%s"
    (enc.enc.record_pre enc)
    (String.concat enc.enc.rec_sep2
       (List.map (fun (name,value) ->
                    sprintf "%s%s%s%s%s" enc.enc.rec_pre (enc.enc.make_rec name) enc.enc.rec_sep1 value enc.enc.rec_post) nvs))
    enc.enc.record_post

let ocaml_input_pvs enc (p,v,s) =
  if v = ""
  then enc.lang.fnctn enc [(enc.lang.pat_con0 p,enc.prs.succeed enc s); ("_",enc.prs.fail enc p)]
  else enc.lang.fnctn enc [(enc.lang.pat_con1 p v,enc.prs.succeed enc s); ("_",enc.prs.fail enc p)]

let ocaml_input_str enc (p,v,s) =
  enc.lang.app enc p v

(* End of generic support routines *)

(* Wrappers for optional inputs *)

let wrap_opt_some enc res v =
  let ro = enc.lang.app enc res v in
  if !(enc.prs.wrap_opt)
  then
    enc.lang.mtch enc ro [
      (enc.lang.make_some v, enc.lang.make_some (enc.lang.make_some v));
      (enc.lang.make_none, enc.lang.make_none);
    ]
  else enc.lang.make_some ro

let wrap_opt_none enc = if !(enc.prs.wrap_opt) then enc.lang.make_some enc.lang.make_none else enc.lang.make_none

let wrap_list enc res v = (if !(enc.prs.wrap_opt) then enc.lang.map_while_opt else enc.lang.map_list) res v

let wrap_tuple enc n ress =
  if !(enc.prs.wrap_opt)
  then
    let vs = List.init n (fun i -> sprintf "__%d" i) in
    let p = "("^String.concat ", " (List.map (fun v -> enc.lang.make_some v) vs)^")" in
    let e = sprintf "(%s)" (String.concat ", " vs) in
    enc.lang.mtch enc ress [
      (p, enc.lang.make_some e);
      ("_", enc.lang.make_none);
    ]
  else ress

let wrap_record enc names labs =
  if !(enc.prs.wrap_opt)
  then
    let vs = List.map (fun l -> sprintf "__%s" l) names in
    let p = sprintf "(%s)" (String.concat ", " (List.map (fun v -> enc.lang.make_some v) vs)) in
    let e = sprintf "{%s}" (String.concat "; " (List.map (fun v -> sprintf "%s=__%s" v v) names)) in
    enc.lang.mtch enc ("("^String.concat ",\n  " labs^")") [
      (p, enc.lang.make_some e);
      ("_", enc.lang.make_none);
    ]
  else
    sprintf "{%s}" (String.concat "; " (List.map2 (fun n l -> sprintf "%s=%s" n l) names labs))

let wrap_cons enc name reso =
  if !(enc.prs.wrap_opt)
  then
    match reso with
    | Some (te,res) ->
        let v = enc.lang.tup_v te in
        enc.lang.mtch enc res [(enc.lang.make_some v, enc.lang.make_some (enc.lang.make_cons name (Some v)));
                               ("_", enc.lang.make_none);]
    | None ->
        enc.lang.make_some (enc.lang.make_cons name None)
  else
    enc.lang.make_cons name (Option.map snd reso)

(* End of wrappers for optional inputs *)

(* Abstract output *)

let tup_v = function
  | O.TypeTuple ttes -> sprintf "(%s)" (String.concat "," (List.mapi (fun i _ -> sprintf "__v%d" i) ttes))
  | _ -> "__v"

let abs_output_cte enc o t = function
  | O.TypeString -> enc.lang.output_v enc (fst enc.enc.string_to_string) o t (snd enc.enc.string_to_string)
  | O.TypeInt -> enc.lang.output_v enc (fst enc.enc.int_to_string) o t (snd enc.enc.int_to_string)
  | O.TypeInt64 -> enc.lang.output_v enc (fst enc.enc.int64_to_string) o t (snd enc.enc.int64_to_string)
  | O.TypeFloat -> enc.lang.output_v enc (fst enc.enc.float_to_string) o t (snd enc.enc.float_to_string)
  | O.TypeBool -> enc.lang.output_v enc (fst enc.enc.bool_to_string) o t (snd enc.enc.bool_to_string)
  | O.TypeChar -> enc.lang.output_v enc (fst enc.enc.char_to_string) o t (snd enc.enc.char_to_string)
  | O.TypeUnit -> enc.lang.output_v enc (fst enc.enc.unit_to_string) o t (snd enc.enc.unit_to_string)

let rec abs_output_te enc o t tyns = function
  | O.TypeName ([te],["option"]) ->
      enc.lang.output_f enc o "__o"
        (output_br enc o t enc.enc.option_pre enc.enc.option_post
           (fun t te ->
              enc.lang.output_m enc o t "__o"
                [(enc.lang.make_some "__v",
                  (fun t ->
                     output_br enc o t (enc.enc.con_pre^(enc.enc.make_con "Some")^enc.enc.con_sep) enc.enc.con_post
                       (fun t te ->
                          enc.lang.output_a enc o (abs_output_te enc o t tyns te) "__v") te));
                 (enc.lang.make_none,
                  (fun t ->
                     enc.lang.output_s enc o t (enc.enc.con_pre^(enc.enc.make_con "None")^enc.enc.con_post)))]
           ) te)

  | O.TypeName ([te],["list"]) ->
      enc.lang.output_f enc o "__l"
        (output_br enc o t enc.enc.list_pre enc.enc.list_post
           (fun t te -> enc.lang.output_l enc o t "__l"
                                 (fun t h ->
                                    output_br enc o t enc.enc.lst_pre enc.enc.lst_post
                                      (fun t te -> enc.lang.output_a enc o (abs_output_te enc o t tyns te) h)
                                    te)
                                 (fun t -> enc.lang.output_s enc o t enc.enc.lst_sep1)
           ) te)

  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (match get_tyn tn tyns with
       | InternalTyn tn ->
           enc.lang.output_f enc o (var_of_tyn tn) (make_ext enc o t tyns tn)
       | ExternalTyn (tn,_) ->
           (*enc.lang.output_f enc o (var_of_tyn tn)*) (make_ext5 enc o t tyns tn)
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))

  | O.TypeConst cte -> abs_output_cte enc o t cte

  | O.TypeTuple tes ->
      let args = List.mapi (fun i _te -> sprintf "__%d" i) tes in
      enc.lang.output_f enc o (enc.lang.pat_tup args)
                 (output_br enc o t (enc.enc.tuple_pre (List.length tes)) enc.enc.tuple_post
                    (fun t te ->
                       output_seq enc 0 o
                         (fun i t te ->
                            output_br enc o t enc.enc.tup_pre enc.enc.tup_post
                              (fun t te ->
                                 enc.lang.output_a enc o (abs_output_te enc o t tyns te) (sprintf "__%d" i)) te)
                         enc.enc.tup_sep1 t tes) tes)

  | O.TypeRecord l ->
      let labs = List.map (fun (_,lab,_) -> lab) l in
      enc.lang.output_f enc o (enc.lang.pat_rec enc labs labs)
                 (output_br enc o t (enc.enc.record_pre enc) enc.enc.record_post
                    (fun t l ->
                       output_seq enc 0 o
                         (fun _ t (_,lab,te) ->
                            output_br enc o t (sprintf "%s%s%s" enc.enc.rec_pre (enc.enc.make_rec lab) enc.enc.rec_sep1)
                              enc.enc.rec_post
                              (fun t te ->
                                 enc.lang.output_a enc o (abs_output_te enc o t tyns te) lab) te) enc.enc.rec_sep2 t l) l)

  | O.TypeConstructor cl ->
      enc.lang.output_f enc o "__c"
        (output_br enc o t enc.enc.cons_pre enc.enc.cons_post
           (fun t cl ->
              enc.lang.output_m enc o t "__c" (
                List.map (fun (name,teo) ->
                            match teo with
                            | Some te ->
                                let v = enc.lang.tup_v te in
                                (enc.lang.pat_con1 name v,
                                 (fun t ->
                                    output_br enc o t (sprintf "%s%s%s" enc.enc.con_pre (enc.enc.make_con name) enc.enc.con_sep)
                                      enc.enc.con_post
                                      (fun t te -> enc.lang.output_a enc o (abs_output_te enc o t tyns te) v) te))
                            | None ->
                                (enc.lang.pat_con0 name,(fun t ->
                                         enc.lang.output_s enc o t (sprintf "%s%s%s" enc.enc.con_pre (enc.enc.make_con name)
                                                                      enc.enc.con_post)))
                         ) cl
              )
           ) cl)

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Abstract output *)

(* Abstract input2 *)

let abs_input2_cte enc = function
  | O.TypeString -> enc.prs.input_v enc enc.prs.string_pat
  | O.TypeInt ->    enc.prs.input_v enc enc.prs.int_pat
  | O.TypeInt64 ->  enc.prs.input_v enc enc.prs.int64_pat
  | O.TypeFloat ->  enc.prs.input_v enc enc.prs.float_pat
  | O.TypeBool ->   enc.prs.input_v enc enc.prs.bool_pat
  | O.TypeChar ->   enc.prs.input_v enc enc.prs.char_pat
  | O.TypeUnit ->   enc.prs.input_v enc enc.prs.unit_pat

let input_fix enc str success =
  if str = ""
  then success
  else sprintf "if input_fixed t \"%s\" then %s else %s" str success (enc.prs.fail enc str)

let make_list enc l = "["^String.concat enc.lang.list_sep l^"]"

let quote s = "\""^s^"\""

let input_rec enc recs =
  let strs = List.map (fun (s,c) -> quote s) recs in
  enc.lang.mtch enc
    (enc.lang.app enc (enc.lang.app enc "input_fixeds" "t") (make_list enc strs))
    ((List.map (fun (s,c) -> (enc.lang.make_some (quote s),c)) recs)@[("_",enc.prs.fail enc (String.concat "," strs))])

let input_m enc v m s =
  if !(enc.prs.wrap_opt)
  then enc.lang.mtch enc m [((enc.lang.make_some v),s); ("_",enc.prs.fail enc m)]
  else enc.lang.mtch enc m [(v,s)]

let input_seq enc pre sep post es output =
  input_fix enc pre
    (let rec aux l = function
       | [] -> input_fix enc post (output (List.rev l))
       | (v,e)::[] -> input_m enc v e (input_fix enc post (output (List.rev (v::l))))
       | (v,e)::rest -> input_m enc v e (input_fix enc sep (aux (v::l) rest))
     in
     aux [] es)

let apps enc fs = 
  let rec aux = function
    | [] -> assert false
    | [a] -> a
    | f::a::rest -> aux ((enc.lang.app enc f a)::rest)
  in
  aux fs

let rec abs_input2_te enc tyns = function
  | O.TypeName ([te],["option"]) ->
      input_fix enc enc.enc.option_pre
        (input_fix enc enc.enc.con_pre
           (input_rec enc
              [(enc.enc.make_con "Some",
                (input_fix enc enc.enc.con_sep
                   (input_m enc "__v" (abs_input2_te enc tyns te)
                      (input_fix enc enc.enc.con_post
                         (input_fix enc enc.enc.option_post
                            (enc.prs.succeed enc (enc.lang.make_some "__v")))))));
               (enc.enc.make_con "None",
                (input_fix enc enc.enc.con_post
                   (input_fix enc enc.enc.option_post (enc.prs.succeed enc enc.lang.make_none))))]))

  | O.TypeName ([te],["list"]) ->
      apps enc ["input_list"; (quote enc.enc.list_pre); (quote enc.enc.lst_sep1); (quote enc.enc.list_post);
                (enc.lang.fnctn enc [("t",
                                      (input_fix enc enc.enc.lst_pre
                                         (if enc.enc.lst_post = ""
                                          then (input_fix enc enc.enc.lst_pre
                                                  (abs_input2_te enc tyns te))
                                          else (input_m enc "__v" (abs_input2_te enc tyns te)
                                                  (input_fix enc enc.enc.lst_post
                                                     (enc.prs.succeed enc "__v"))))))]); "t"]

  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (match get_tyn tn tyns with
       | InternalTyn tn ->
           make_ext2 enc tyns tn "t"
       | ExternalTyn (tn,_) ->
           make_ext4 enc tyns tn "t"
       | _ -> (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
                  assert false))

  | O.TypeConst cte -> abs_input2_cte enc cte

  | O.TypeTuple tes ->
      let es = List.mapi (fun i te ->
                            let arg = sprintf "__%d" i in
                            arg,
                            (input_fix enc enc.enc.tup_pre
                               (input_m enc "__v" (abs_input2_te enc tyns te)
                                  (input_fix enc enc.enc.tup_post
                                     (enc.prs.succeed enc "__v"))))) tes in
      input_seq enc (enc.enc.tuple_pre (List.length tes)) enc.enc.tup_sep1 enc.enc.tuple_post
                    es (fun s -> enc.prs.succeed enc (String.concat ", " s))

  | O.TypeRecord l ->
      let labs = List.map (fun (_,lab,_) -> lab) l in
      let es = List.map (fun (_,lab,te) ->
                            "__"^lab,
                            (input_fix enc enc.enc.rec_pre
                               (input_fix enc (enc.enc.make_con lab)
                                  (input_fix enc enc.enc.rec_sep1
                                     (input_m enc "__v" (abs_input2_te enc tyns te)
                                        (input_fix enc enc.enc.rec_post
                                           (enc.prs.succeed enc "__v"))))))) l in
      input_seq enc (enc.enc.record_pre enc) enc.enc.rec_sep2 enc.enc.record_post es
                    (fun ls -> enc.prs.succeed enc (enc.lang.pat_rec enc labs ls))

  | O.TypeConstructor cl ->
      input_fix enc enc.enc.cons_pre
        (input_fix enc enc.enc.con_pre
           (input_rec enc
              (List.map (fun (name,teo) ->
                           match teo with
                           | Some te ->
                               let v = enc.lang.tup_v te in
                               ((enc.enc.make_con name),
                                (input_fix enc enc.enc.con_sep
                                   (input_m enc v (abs_input2_te enc tyns te)
                                      (input_fix enc enc.enc.con_post
                                         (input_fix enc enc.enc.cons_post
                                            (enc.prs.succeed enc (enc.lang.make_cons name (Some v))))))))
                           | None ->
                               ((enc.enc.make_con name),
                                (input_fix enc enc.enc.con_post
                                   (input_fix enc enc.enc.cons_post (enc.prs.succeed enc (enc.lang.make_cons name None)))))) cl)))

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Abstract input2 *)

(* Abstract json output *)

(* Note: we only use Record and List here. *)

(* Note: we ignore the "o" and "t" parameters but we keep the same encoding function signatures. *)

let match_record enc v pes =
  match pes, enc.prs.pre_rec with
  | (pes, Some pr) -> enc.lang.output_f enc "" v (enc.lang.mtch enc (pr enc v) (pes@[("_",enc.lang.error "match_record")]))
  | ([p,e], None) -> enc.lang.output_f enc "" p e
  | (_, _) -> assert false

let match_tuple enc i v p e =
  match enc.prs.pre_tup with
  | Some pr -> enc.lang.output_f enc "" v (enc.lang.mtch enc (pr enc i v) [(p,e); ("_",enc.lang.error "match_tuple")])
  | None -> enc.lang.output_f enc "" p e

let rec abs_tojson_te enc tyns = function
  | O.TypeName ([te],["option"]) ->
      enc.lang.output_m enc "" "" "__t"
        [(enc.lang.make_some "__v", (make_rec enc "Some" (enc.lang.output_a enc "" (abs_tojson_te enc tyns te) "__v")));
         (enc.lang.make_none, (make_rec enc "None" enc.enc.rec_void)); ]

  | O.TypeName ([te],["list"]) ->
      enc.lang.output_l enc "" "" "__l" (fun h _ -> enc.lang.output_a enc "" (abs_tojson_te enc tyns te) h) (fun x -> x)

  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (match get_tyn tn tyns with
       | InternalTyn tn | ExternalTyn (tn,_) ->
           enc.lang.output_f enc "" (var_of_tyn tn) (make_ext2 enc tyns tn (var_of_tyn tn))
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))

  | O.TypeConst cte -> abs_output_cte enc "" "" cte

  | O.TypeTuple tes ->
      let args = List.mapi (fun i _te -> enc.prs.tup_field i) tes in
      let nvs = List.mapi (fun i te -> (enc.prs.tup_field i,
                                        sprintf "(%s(%s))" (abs_tojson_te enc tyns te) (enc.prs.tup_field i))) tes in (*FIXME!!!*)
      enc.lang.output_f enc "" (enc.lang.pat_tup args) (make_tups enc nvs)
      (*match_tuple enc (List.length tes) "__t" (enc.lang.pat_tup args) (make_tups enc nvs)*)

  | O.TypeRecord l ->
      let labs = List.map (fun (_,lab,_) -> lab) l in
      let nvs = List.map (fun (_,lab,te) -> lab, sprintf "(%s(%s))" (abs_tojson_te enc tyns te) lab) l in
      enc.lang.output_f enc "" "__r" (enc.lang.mtch enc "__r" [(enc.lang.pat_rec enc labs labs),(make_recs enc nvs)])
      (*match_record enc "__r" [(enc.lang.pat_rec enc labs labs),(make_recs enc nvs)]*)

  | O.TypeConstructor cl ->
      enc.lang.output_m enc "" "" "__t"
        (List.map (fun (name,teo) ->
                     match teo with
                     | Some te ->
                         let v = enc.lang.tup_v te in
                         (enc.lang.pat_con1 name v,
                          (make_rec enc name (enc.lang.output_a enc "" (abs_tojson_te enc tyns te) v)))
                     | None ->
                         (enc.lang.pat_con0 name,
                          (make_rec enc name enc.enc.rec_void))) cl)

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of abstract json output *)

(* Concrete wrap/unwrap output/input *)

let rec abs_un_wrap_te enc tyns = function
  | O.TypeName ([te],["option"]) ->
      (match enc.prefix with
       | "wrap" -> sprintf "(fun __o -> wrap_opt %s __o)" (abs_un_wrap_te enc tyns te)
       | "unwrap" -> sprintf "(fun __o -> (match unwrap_rcrd __o with [(\"some\",__v)] -> Some (%s __v) | [(\"none\",_)] -> None | _ -> raise (Failure \"match_option\")))" (abs_un_wrap_te enc tyns te)
       | _ -> assert false)

  | O.TypeName ([te],["list"]) ->
      (match enc.prefix with
       | "wrap" -> sprintf "(fun __l -> wrap_lst %s __l)" (abs_un_wrap_te enc tyns te)
       | "unwrap" -> sprintf "(fun __l -> (match unwrap_lst %s __l with Some __l -> __l | _ -> raise (Failure \"match_list\")))"
                             (abs_un_wrap_te enc tyns te)
       | _ -> assert false)

  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (match get_tyn tn tyns with
       | InternalTyn tn ->
           enc.lang.output_f enc "" (var_of_tyn tn) (make_ext2 enc tyns tn (var_of_tyn tn))
       | ExternalTyn (tn,_) ->
           enc.lang.output_f enc "" (var_of_tyn tn) (make_ext2 enc tyns tn (var_of_tyn tn))
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))

  | O.TypeConst cte -> abs_output_cte enc "" "" cte

  | O.TypeTuple tes ->
      (match enc.prefix with
       | "wrap" ->
           enc.lang.fnctn enc
             ["("^(String.concat "," (List.mapi (fun i _te -> sprintf "f%d" (i+1)) tes))^")",
              "ServerLib.make_record ("^
                (snd (List.fold_left
                        (fun (i,t) te ->
                           (i+1,sprintf "(ServerLib.add_field %s (ServerLib.static_field_of_name \"f%d\") (%s f%d))"
                              t i (abs_un_wrap_te enc tyns te) i))
                        (1,"ServerLib.empty_record_constructor") tes))^")"]
       | "unwrap" ->
           let mtchs = List.mapi (fun i _te -> sprintf "ServerLib.dot __t (ServerLib.static_field_of_name \"f%d\")" (i+1)) tes in
           let mtche = sprintf "(%s)" (String.concat "," mtchs) in
           let vs = List.mapi (fun i te -> (te,sprintf "__v%d" (i+1))) tes in
           let vps = List.map (fun (_,v) -> sprintf "Some %s" v) vs in
           let vp = sprintf "(%s)" (String.concat "," vps) in
           let ve = sprintf "(%s)" (String.concat "," (List.map (fun (te,v) -> sprintf "%s %s" (abs_un_wrap_te enc tyns te) v) vs)) in
           enc.lang.fnctn enc ["__t",enc.lang.mtch enc mtche [(vp,ve); ("_","raise (Failure \"match_tuple\")")]]
       | _ -> assert false)

  | O.TypeRecord l ->
      let mname = if enc.modname = "" then "" else (String.capitalize enc.modname)^". " in
      (match enc.prefix with
       | "wrap" ->
           enc.lang.fnctn enc
             ["{"^mname^(String.concat ";" (List.map (fun (_,lab,_te) -> sprintf "%s=__%s" lab lab) l))^"}",
              "ServerLib.make_record ("^
                (List.fold_left
                        (fun t (_,lab,te) ->
                           sprintf "(ServerLib.add_field %s (ServerLib.static_field_of_name \"%s\") (%s __%s))"
                             t lab (abs_un_wrap_te enc tyns te) lab)
                        "ServerLib.empty_record_constructor" l)^")"]
       | "unwrap" ->
           let mtchs = List.map (fun (_,lab,_te) ->
                                   sprintf "ServerLib.dot __r (ServerLib.static_field_of_name \"%s\")" lab) l in
           let mtche = sprintf "(%s)" (String.concat "," mtchs) in
           let vs = List.mapi (fun i (_,lab,_te) -> sprintf "__%s" lab) l in
           let vps = List.map (fun v -> sprintf "Some %s" v) vs in
           let vp = sprintf "(%s)" (String.concat "," vps) in
           let ves = List.mapi (fun i (_,lab,te) -> sprintf "%s=(%s __%s)" lab (abs_un_wrap_te enc tyns te) lab) l in
           let ve = sprintf "{%s%s}" mname (String.concat ";" ves) in
           enc.lang.fnctn enc ["__r",enc.lang.mtch enc mtche [(vp,ve); ("_","raise (Failure \"match_record\")")]]
       | _ -> assert false)

  | O.TypeConstructor cl ->
      (match enc.prefix with
       | "wrap" ->
           enc.lang.output_m enc "" "" "__t"
             (List.map (fun (name,teo) ->
                          let mname = (if enc.modname = "" then name else (String.capitalize enc.modname)^"."^name) in
                          match teo with
                          | Some te ->
                              let v = enc.lang.tup_v te in
                              (enc.lang.pat_con1 mname v,
                               (make_rec enc name (enc.lang.output_a enc "" (abs_un_wrap_te enc tyns te) v)))
                          | None ->
                              (enc.lang.pat_con0 mname,
                               (make_rec enc name enc.enc.rec_void))) cl)
       | "unwrap" ->
           enc.lang.output_f enc "" "__t"
             (enc.lang.mtch enc "unwrap_rcrd __t"
                ((List.map
                    (fun (name,teo) ->
                       let mname = (if enc.modname = "" then name else (String.capitalize enc.modname)^"."^name) in
                       match teo with
                       | Some te ->
                           let v = enc.lang.tup_v te in
                           sprintf "[(\"%s\",_)]" name,
                           sprintf "let %s = (%s (ServerLib.unsafe_dot __t (ServerLib.static_field_of_name \"%s\"))) in (%s %s)"
                                   v (abs_un_wrap_te enc tyns te) name mname v
                       | None ->
                           sprintf "[(\"%s\",_)]" name, sprintf "%s" mname) cl)
                 @[("_",enc.lang.error "match_constructor")]))
       | _ -> assert false)

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Concrete wrap/unwrap output/input *)

(* Abstract input *)

let abs_input_cte enc = function
  | O.TypeString -> enc.prs.input_v enc enc.prs.string_pat
  | O.TypeInt ->    enc.prs.input_v enc enc.prs.int_pat
  | O.TypeInt64 ->  enc.prs.input_v enc enc.prs.int64_pat
  | O.TypeFloat ->  enc.prs.input_v enc enc.prs.float_pat
  | O.TypeBool ->   enc.prs.input_v enc enc.prs.bool_pat
  | O.TypeChar ->   enc.prs.input_v enc enc.prs.char_pat
  | O.TypeUnit ->   enc.prs.input_v enc enc.prs.unit_pat

let rec abs_input_te enc tyns = function
  | O.TypeName ([te],["option"]) ->
      enc.lang.fnctn enc [
        (enc.prs.opt_pat (Some "__o"), wrap_opt_some enc (abs_input_te enc tyns te) "__o");
        (enc.prs.opt_pat None, wrap_opt_none enc);
        ("_",enc.prs.fail enc (enc.prs.opt_pat None));
      ]
  | O.TypeName ([te],["list"]) ->
      enc.lang.fnctn enc [
        (enc.prs.list_pat "__l", wrap_list enc (abs_input_te enc tyns te) "__l");
        ("_",enc.prs.fail enc (enc.prs.list_pat "list"));
      ]
  | O.TypeName (tes,tn) ->
      let n = str_of_tyn tn in
      (match get_tyn tn tyns with
       | InternalTyn tn | ExternalTyn (tn,_) ->
           let v = var_of_tyn tn in
           enc.lang.fnctn enc [(v,(make_ext2 enc tyns tn v))]
       | _ ->
           (eprintf "Unknown: %s %s\n%!" (String.concat " " (List.map Tools.str_of_type_expr tes)) n;
            assert false))
  | O.TypeConst cte -> abs_input_cte enc cte
  | O.TypeTuple tes ->
      let n = List.length tes in
      let els = List.mapi (fun i te -> enc.lang.app enc (abs_input_te enc tyns te) (sprintf "__%d" i)) tes in
      enc.lang.fnctn enc [(enc.prs.tuple_pat n),(wrap_tuple enc n ("("^String.concat ",\n  " els^")"));
                          ("_",enc.prs.fail enc (enc.prs.tuple_pat 0));
                         ]
  | O.TypeRecord l ->
      let names = List.map (fun (_,lab,_) -> lab) l in
      let labs = List.map (fun (_,lab,te) -> enc.lang.app enc (abs_input_te enc tyns te) ("__"^lab)) l in
      enc.lang.fnctn enc [(enc.prs.record_pat names),(wrap_record enc names labs);
                          ("_",enc.prs.fail enc (enc.prs.record_pat []));
                         ]
  | O.TypeConstructor cl ->
      let mtches =
        List.map
          (fun (name,teo) ->
             match teo with
             | Some te ->
                 let v = enc.lang.tup_v te in
                 let vo, pat = enc.prs.cons_pat name (Some v) in
                 (pat,
                  wrap_cons enc name (Some (te,(enc.lang.app enc (abs_input_te enc tyns te) (Option.get vo)))))
             | None ->
                 let _, pat = enc.prs.cons_pat name None in
                 (pat,
                  wrap_cons enc name None)
          ) cl in
      let _, pat = enc.prs.cons_pat "label" (Some "value") in
      enc.lang.fnctn enc (mtches@[("_",enc.prs.fail enc pat)])

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Abstract input *)

(* OPA input/output *)

let get_external_opa_function enc exts t =
  let defnames =
    match enc.prefix with
    | "output" -> ["opastringof"]
    | "input" -> ["opaofstring"]
    | "tojson" -> ["opatojson"]
    | "tojson_ll" -> ["opatojsonll"]
    | "fromjson" -> ["opafromjson"]
    | "fromjson_ll" -> ["opafromjsonll"]
    | _ -> failwith (sprintf "Unknown encoding prefix '%s'" enc.prefix)
  in
  try
    let extdefs = List.assoc t exts in
    match List.find (fun (name,_) -> List.mem name defnames) extdefs with (_,code) -> code
  with Not_found -> failwith (sprintf "Unable to find handler for %s %s" enc.prefix t)

let opa_lang = {
  make_some = (fun s -> "{some="^s^"}");
  make_none = "{none}";
  make_cons = (fun l vo -> match vo with Some v -> sprintf "{%s=%s}" l v | None -> sprintf "{%s}" l);
  map_list = (fun f l -> sprintf "List.map((%s),(%s))" f l);
  map_while_opt = (fun f l -> sprintf "List.map_while_opt((%s),(%s))" f l);
  list_sep = ", ";
  flush_str = (fun enc o t -> sprintf "%s((%s),\"%s\")" o t !(enc.str));
  output_s = (fun enc o t s -> if enc.foldstrs then (enc.str := (!(enc.str))^s; t) else sprintf "%s((%s),\"%s\")" o t s);
  output_v = (fun enc vn o t to_s -> sprintf "(%s -> %s(%s,((%s -> %s(%s))(%s))))" vn o (flush_str enc o t) vn to_s vn vn);
  output_a = (fun enc o f a -> sprintf "(%s(%s))" (flush_str enc o f) (flush_str enc o a));
  output_f = (fun enc o p e -> sprintf "(%s -> %s)" p (flush_str enc o e));
  output_m = (fun enc o t m pes ->
                sprintf "((__t -> match %s with |%s)(%s))"
                  m (String.concat "| "
                       (List.map (fun (p,ef) ->
                                    sprintf "%s -> %s" p (flush_str enc o (ef "__t"))) pes)) (flush_str enc o t));
  output_l = (fun enc o t l ef sf ->
                sprintf "\
((__l ->\n   \
   (rec aux(__t,__l) =\n    \
    match __l with\n     \
    | [] -> __t\n    \
    | [__h] -> aux((%s),[])\n    \
    | [__h|__rest] -> aux((%s),__rest)\n    \
    aux((%s),__l)))(%s))" (flush_str enc o (ef "__t" "__h"))
                          (flush_str enc o (sf (ef "__t" "__h")))
                          (flush_str enc o t) l);
  make_ext_fn = (fun enc (tyns,exts) -> (function
                                         | [n] -> sprintf "%s_%s" enc.prefix n
                                         | [m;n] -> sprintf "%s_%s.%s_%s" enc.modname (lc1 m) enc.prefix n
                                         | ("external"::t::tn) -> get_external_opa_function enc exts t
                                         | _ -> assert false));
  pat_tup = (fun args -> (sprintf "(%s)" (String.concat ", " args)));
  tup_v = (fun _ -> "__v");
  pat_rec = (fun _enc labs vals -> (sprintf "{ %s }" (String.concat "; " (List.map2 (fun l v -> l^"="^v) labs vals))));
  pat_con0 = (fun s -> "{"^s^"}");
  pat_con1 = (fun s v -> "{"^s^"="^v^"}");
  error = (fun s -> sprintf "error(\"%s\")" (toopastr (String.escaped s)));
  mtch = (fun enc e pes ->
             sprintf "(match %s with %s)" e (String.concat " | " (List.map (fun (p,e) -> sprintf "%s -> %s" p e) pes)));
  fnctn = (fun enc pes -> sprintf "(__a -> %s)" (enc.lang.mtch enc "__a" pes));
  app = (fun enc f a -> sprintf "(%s(%s))" f a);
}

let opa_enc1 = {
  string_to_string = "__s", "(__s -> \"\\\"\"^__s^\"\\\"\")";
  int_to_string = "__i", "Int.to_string";
  int64_to_string = "__i64", "Int.to_string";
  float_to_string = "__f", "Float.to_string";
  bool_to_string = "__b", "Bool.to_string";
  char_to_string = "__c", "Char.to_string";
  unit_to_string = "__u", "(_ -> \"()\")";
  list_pre = "\\{List=["; list_post = "]\\}"; lst_pre = "("; lst_sep1 = ";"; lst_sep2 = ""; lst_post = ")";
  tuple_pre = (fun _ -> "\\{Tuple=["); tuple_post = "]\\}"; tup_pre = "("; tup_sep1 = ","; tup_sep2 = ","; tup_post = ")";
  make_tup = (fun s -> "\\\""^s^"\\\"");
  record_pre = (fun _ -> "\\{Record=["); record_post = "]\\}"; rec_pre = "("; rec_sep1 = ","; rec_sep2 = ","; rec_post = ")";
  make_rec = (fun s -> "\\\""^s^"\\\""); rec_void = "{Void}";
  option_pre = "\\{Option=["; option_post = "]\\}";
  cons_pre = "\\{Cons=["; cons_post = "]\\}"; con_pre = "("; con_sep = ","; con_post = ")";
  make_con = (fun s -> "\\\""^s^"\\\"");
}

let opa_enc2 = {
  opa_enc1 with
    unit_to_string = "__u", "(_ -> \"\\{\\}\")";
    list_pre = "["; list_post = "]"; lst_pre = ""; lst_sep1 = ","; lst_sep2 = ""; lst_post = "";
    tuple_pre = (fun _ -> "["); tuple_post = "]"; tup_pre = ""; tup_sep1 = ","; tup_sep2 = ","; tup_post = "";
    make_tup = (fun s -> "\\\""^s^"\\\"");
    record_pre = (fun _ -> "\\{"); record_post = "\\}"; rec_pre = ""; rec_sep1 = ":"; rec_sep2 = ","; rec_post = "";
    option_pre = "\\{"; option_post = "\\}";
    cons_pre = "\\{"; cons_post = "\\}"; con_pre = ""; con_sep = ":"; con_post = "";
}

let opa_prs = {
  wrap_opt = opa_wrap_opt;
  succeed = (fun enc s -> if !(enc.prs.wrap_opt) then "{some="^s^"}" else s);
  fail = (fun enc s -> if !(enc.prs.wrap_opt) then "{none}" else sprintf "error(\"%s\")" (toopastr (String.escaped s)));
  string_pat =  "String", "__s", "__s";
  int_pat = "Int", "__i", "__i";
  int64_pat = "Int64", "__i64", "__i64";
  float_pat = "Float", "__f", "__f";
  bool_pat = "Bool", "__b", "__b";
  char_pat = "Char", "__c", "__c";
  unit_pat = "Void", "", "void";
  opt_pat = (function Some s -> sprintf "{Record=[(\"Some\",%s)]}" s | None -> "{Record=[(\"None\",_)]}");
  list_pat = (fun s -> sprintf "{List=%s}" s);
  tup_field = (fun i -> sprintf "__%d" i);
  pre_tup = None;
  tuple_pat = (fun n -> sprintf "{Record=[%s]}" (String.concat ", " (List.init n (fun i -> sprintf "(\"__%d\",__%d)" i i))));
  pre_rec = None;
  record_pat = (fun ls -> sprintf "{Record=[%s]}" (String.concat ", " (List.map (fun l -> sprintf "(\"%s\",__%s)" l l) ls)));
  cons_pat = (fun l vo -> vo, sprintf "{Record=[%s]}" (sprintf "(\"%s\",%s)" l (match vo with Some v -> v | None -> "_")));
  input_v = ocaml_input_pvs;
}

let opa_encoding1 = {
  foldstrs = true;
  str = ref "";
  modname = "STR";
  prefix = "output";
  lang = opa_lang;
  enc = opa_enc1;
  prs = opa_prs;
}

let opa_encoding2 = {
  opa_encoding1 with enc = opa_enc2
}

let opa_encoding = ref ((*match !encoding_number with | 1 -> opa_encoding1 | 2 ->*) opa_encoding2 (*| _ -> assert false*))

let opa_prs3 = {
  opa_prs with
  string_pat =  "input_string", "t", "";
  int_pat = "input_int", "t", "";
  int64_pat = "input_int", "t", "";
  float_pat = "input_float", "t", "";
  bool_pat = "input_bool", "t", "";
  char_pat = "input_char", "t", "";
  unit_pat = "input_unit(\"{}\")", "t", "";
  input_v = ocaml_input_str;
}

(* End of OPA input/output *)

(* OCaml input/output *)

let get_external_function enc exts t =
  let defnames =
    match enc.prefix with
    | "output" -> ["ocamlstringof"]
    | "input" -> ["ocamlofstring"]
    | "tojson" -> ["ocamltojson"]
    | "fromjson" -> ["ocamlfromjson"]
    | "wrap" -> ["bslwrap"]
    | "unwrap" -> ["bslunwrap"]
    | _ -> failwith (sprintf "Unknown encoding prefix '%s'" enc.prefix)
  in
  try
    let extdefs = List.assoc t exts in
    match List.find (fun (name,_) -> List.mem name defnames) extdefs with (_,code) -> code
  with Not_found -> failwith (sprintf "Unable to find handler for %s %s" enc.prefix t)

let ocaml_lang = {
  make_some = (fun s -> "(Some ("^s^"))");
  make_none = "None";
  make_cons = (fun l vo -> match vo with Some v -> sprintf "(%s %s)" l v | None -> l);
  map_list = (fun f l -> sprintf "List.map (%s) (%s)" f l);
  map_while_opt = (fun f l -> sprintf "map_while_opt (%s) (%s)" f l);
  list_sep = "; ";
  flush_str = (fun enc o t -> sprintf "(%s (%s) \"%s\")" o t !(enc.str));
  output_s = (fun enc o t s -> if enc.foldstrs then (enc.str := (!(enc.str))^s; t) else sprintf "%s (%s) \"%s\"" o t s);
  output_v = (fun enc vn o t to_s -> sprintf "(fun %s -> %s (%s) (%s %s))" vn o (flush_str enc o t) to_s vn);
  output_a = (fun enc o f a -> sprintf "(%s %s)" (flush_str enc o f) (flush_str enc o a));
  output_f = (fun enc o p e -> sprintf "(function %s -> %s)" p (flush_str enc o e));
  output_m = (fun enc o t m pes ->
                sprintf "((fun __t -> match %s with %s) (%s))"
                  m (String.concat "|"
                       (List.map (fun (p,ef) ->
                                    sprintf "%s -> %s" p (flush_str enc o (ef "__t"))) pes)) (flush_str enc o t));
  output_l = (fun enc o t l ef sf ->
                sprintf "\
((fun __l ->\n   \
   (let rec aux __t = function\n    \
    | [] -> __t\n    \
    | [__h] -> aux (%s) []\n    \
    | __h::__rest -> aux (%s) __rest\n    \
    in\n   \
    aux (%s) __l)) (%s))" (flush_str enc o (ef "__t" "__h"))
                          (flush_str enc o (sf (ef "__t" "__h")))
                          (flush_str enc o t) l);
  make_ext_fn = (fun enc (tyns,exts) -> (function
                                         | [n] -> (sprintf "%s_%s" enc.prefix n)
                                         | [mn;n] -> (sprintf "%s.%s_%s" mn enc.prefix n)
                                         | ("external"::t::tn) -> get_external_function enc exts t
                                         | _ -> assert false));
  pat_tup = (fun args -> (sprintf "(%s)" (String.concat ", " args)));
  tup_v = tup_v;
  pat_rec = (fun enc labs vals -> (sprintf "{ %s%s }"
                                     (if enc.modname = "" then "" else (String.capitalize enc.modname)^". ")
                                     (String.concat "; " (List.map2 (fun l v -> l^"="^v) labs vals))));
  pat_con0 = (fun s -> s);
  pat_con1 = (fun s v -> s^" "^v);
  error = (fun s -> sprintf "raise (Failure \"%s\")" (String.escaped s));
  mtch = (fun enc e pes ->
            sprintf "(match %s with %s)" e (String.concat " | " (List.map (fun (p,e) -> sprintf "%s -> %s" p e) pes)));
  fnctn = (fun enc pes -> sprintf "(function %s)" (String.concat " | " (List.map (fun (p,e) -> sprintf "%s -> %s" p e) pes)));
  app = (fun enc f a -> sprintf "((%s) (%s))" f a);
}

let ocaml_enc1 = {
  string_to_string = "__s", "(fun __s -> \"\\\"\"^__s^\"\\\"\")";
  int_to_string = "__i", "string_of_int";
  int64_to_string = "__i64", "Int64.to_string";
  float_to_string = "__f", "string_of_float";
  bool_to_string = "__b", "string_of_bool";
  char_to_string = "__c", "(fun c -> String.make 1 c)";
  unit_to_string = "__u", "(fun () -> \"()\")";
  list_pre = "{List=["; list_post = "]}";
  lst_pre = "("; lst_sep1 = ";"; lst_sep2 = ""; lst_post = ")";
  tuple_pre = (fun _ -> "{Tuple=["); tuple_post = "]}";
  tup_pre = "("; tup_sep1 = ","; tup_sep2 = ","; tup_post = ")";
  make_tup = (fun s -> "\\\""^s^"\\\"");
  record_pre = (fun _ -> "{Record=["); record_post = "]}";
  rec_pre = "("; rec_sep1 = ","; rec_sep2 = ","; rec_post = ")"; make_rec = (fun s -> "\\\""^s^"\\\""); rec_void = "Void";
  option_pre = "{Option=["; option_post = "]}";
  cons_pre = "{Cons=["; cons_post = "]}";
  con_pre = "("; con_sep = ","; con_post = ")"; make_con = (fun s -> "\\\""^s^"\\\"");
}

let ocaml_enc2 = {
  ocaml_enc1 with
    unit_to_string = "__u", "(fun () -> \"{}\")";
    list_pre = "["; list_post = "]"; lst_pre = ""; lst_sep1 = ","; lst_sep2 = ""; lst_post = "";
    tuple_pre = (fun _ -> "["); tuple_post = "]"; tup_pre = ""; tup_sep1 = ","; tup_sep2 = ","; tup_post = "";
    make_tup = (fun s -> "\\\""^s^"\\\"");
    record_pre = (fun _ -> "{"); record_post = "}"; rec_pre = ""; rec_sep1 = ":"; rec_sep2 = ","; rec_post = "";
    option_pre = "{"; option_post = "}";
    cons_pre = "{"; cons_post = "}"; con_pre = ""; con_sep = ":"; con_post = "";
}

let ocaml_prs1 = {
  wrap_opt = ocaml_wrap_opt;
  succeed = (fun enc s -> if !(enc.prs.wrap_opt) then "(Some ("^s^"))" else s);
  fail = (fun enc s -> if !(enc.prs.wrap_opt) then "None" else sprintf "raise (Failure \"%s\")" (String.escaped s));
  string_pat =  "Parser.PT_String", "__s", "__s";
  int_pat = "Parser.PT_Int", "__i", "__i";
  int64_pat = "Parser.PT_Int64", "__i64", "__i64";
  float_pat = "Parser.PT_Float", "__f", "__f";
  bool_pat = "Parser.PT_Bool", "__b", "__b";
  char_pat = "Parser.PT_Char", "__c", "__c";
  unit_pat = "Parser.PT_Unit", "", "()";
  opt_pat = (function Some s -> sprintf "Parser.PT_Option (Some (%s))" s | None -> "Parser.PT_Option None");
  list_pat = (fun s -> sprintf "Parser.PT_List %s" s);
  tup_field = (fun i -> sprintf "__%d" i);
  pre_tup = None;
  tuple_pat = (fun n -> sprintf "Parser.PT_Tuple [%s]" (String.concat "; " (List.init n (fun i -> sprintf "__%d" i))));
  pre_rec = None;
  record_pat = (fun ls -> sprintf "Parser.PT_Record [%s]"
                  (String.concat "; " (List.map (fun l -> sprintf "(\"%s\",__%s)" l l) ls)));
  cons_pat = (fun l vo -> vo, sprintf "Parser.PT_Cons (\"%s\",%s)" l (match vo with Some v -> "Some "^v | None -> "None"));
  input_v = ocaml_input_pvs;
}

let ocaml_prs2 = {
  ocaml_prs1 with
  string_pat =  "Parser.PT_String", "__s", "__s";
  int_pat = "Parser.PT_Int", "__i", "__i";
  int64_pat = "Parser.PT_Int64", "__i64", "__i64";
  float_pat = "Parser.PT_Float", "__f", "__f";
  bool_pat = "Parser.PT_Bool", "__b", "__b";
  char_pat = "Parser.PT_Char", "__c", "__c";
  unit_pat = "Parser.PT_Unit", "", "()";
  opt_pat = (function
             | Some s -> sprintf "Parser.PT_Record [(\"Some\",Some %s)]" s
             | None -> "Parser.PT_Record [(\"None\",None)]");
  list_pat = (fun s -> sprintf "Parser.PT_ListTuple %s" s);
  tup_field = (fun i -> sprintf "__%d" i);
  pre_tup = None;
  tuple_pat = (fun n -> sprintf "Parser.PT_ListTuple [%s]"
                 (String.concat "; " (List.init n (fun i -> sprintf "__%d" i))));
  pre_rec = None;
  record_pat = (fun ls -> sprintf "Parser.PT_Record [%s]"
                  (String.concat "; " (List.map (fun l -> sprintf "(\"%s\",Some __%s)" l l) ls)));
  cons_pat = (fun l vo -> vo, sprintf "Parser.PT_Record [(\"%s\",%s)]" l (match vo with Some v -> "Some "^v | None -> "None"));
  input_v = ocaml_input_pvs;
}

let ocaml_encoding1 = {
  foldstrs = true;
  str = ref "";
  modname = "";
  prefix = "output";
  lang = ocaml_lang;
  enc = ocaml_enc1;
  prs = ocaml_prs1;
}

let ocaml_encoding2 = {
  ocaml_encoding1 with enc = ocaml_enc2; prs = ocaml_prs2
}

let ocaml_encoding = ref ((*match !encoding_number with 1 -> ocaml_encoding1 | 2 ->*) ocaml_encoding2 (*| _ -> assert false*))

let ocaml_prs3 = {
  ocaml_prs1 with
  string_pat =  "input_string", "t", "";
  int_pat = "input_int", "t", "";
  int64_pat = "input_int", "t", "";
  float_pat = "input_float", "t", "";
  bool_pat = "input_bool", "t", "";
  char_pat = "input_char", "t", "";
  unit_pat = "input_unit \"{}\"", "t", "";
  input_v = ocaml_input_str;
}

(* End of OCaml input/output *)

(* OPA JSON output *)

let opa_tojson_encoding = {
  foldstrs = false;
  str = ref "";
  modname = "JSON";
  prefix = "tojson";
  lang = {
    opa_lang with
    output_v = (fun enc vn o t to_j -> if vn = "" then sprintf "(_ -> %s)" to_j else sprintf "(%s -> {%s=(%s)})" vn to_j vn);
    output_m = (fun enc o t m pes ->
                  sprintf "(__t -> match %s with %s)"
                    m (String.concat "| " (List.map (fun (p,ef) -> sprintf "%s -> %s" p ((ef "__t"))) pes)));
    output_l = (fun enc o t l ef sf -> sprintf "(__l -> {List=(List.map((__e -> %s),__l))})" (ef "__e" ""));
  };
  enc = {
    opa_enc1 with
      string_to_string = "__s", "String";
      int_to_string = "__i", "Int";
      int64_to_string = "__i64", "Int";
      float_to_string = "__f", "Float";
      bool_to_string = "__b", "Bool";
      char_to_string = "__c", "Char";
      unit_to_string = "", "{Void}";
      tuple_pre = (fun _ -> "{Record=["); tuple_post = "]}";
      tup_pre = "("; tup_sep1 = ","; tup_sep2 = ","; tup_post = ")"; make_tup = (fun s -> "\""^s^"\"");
      record_pre = (fun _ -> "{Record=["); record_post = "]}";
      rec_pre = "("; rec_sep1 = ","; rec_sep2 = ","; rec_post = ")"; make_rec = (fun s -> "\""^s^"\""); rec_void = "{Void}";
  };
  prs = opa_prs;
}

(* End of OPA JSON output *)

(* OPA JSON low-level output *)

let opa_tojson_ll_encoding = {
  foldstrs = false;
  str = ref "";
  modname = "JSON";
  prefix = "tojson_ll";
  lang = {
    opa_lang with
    output_v = (fun enc vn o t to_j -> if vn = "" then sprintf "(_ -> %s)" to_j else sprintf "(%s -> (%s(%s)))" vn to_j vn);
    output_m = (fun enc o t m pes ->
                  sprintf "(__t -> match %s with %s)"
                    m (String.concat "| " (List.map (fun (p,ef) -> sprintf "%s -> %s" p ((ef "__t"))) pes)));
    output_l = (fun enc o t l ef sf -> sprintf "(__l -> opal2mll(List.rev_map((__e -> %s),__l)))" (ef "__e" ""));
  };
  enc = {
    opa_enc1 with
      string_to_string = "__s", "to_string";
      int_to_string = "__i", "to_int";
      int64_to_string = "__i64", "to_int";
      float_to_string = "__f", "to_float";
      bool_to_string = "__b", "to_bool";
      char_to_string = "__c", "(c -> String (String.make 1 c))";
      unit_to_string = "", "to_void()";
      tuple_pre = (fun _ -> "opar2mlr(["); tuple_post = "])";
      tup_pre = "("; tup_sep1 = ","; tup_sep2 = ","; tup_post = ")"; make_tup = (fun s -> "\""^s^"\"");
      record_pre = (fun _ -> "opar2mlr(["); record_post = "])";
      rec_pre = "("; rec_sep1 = ","; rec_sep2 = ","; rec_post = ")"; make_rec = (fun s -> "\""^s^"\""); rec_void = "to_void()";
  };
  prs = opa_prs;
}

(* End of OPA JSON low-level output *)

(* OCaml JSON output *)

let ocaml_tojson_encoding = {
  foldstrs = false;
  str = ref "";
  modname = "";
  prefix = "tojson";
  lang = {
    ocaml_lang with
    output_v = (fun enc vn o t to_j -> sprintf "(fun %s -> (%s %s))" vn to_j vn);
    output_m = (fun enc o t m pes ->
                  sprintf "(fun __t -> match %s with %s)"
                    m (String.concat "|" (List.map (fun (p,ef) -> sprintf "%s -> %s" p ((ef "__t"))) pes)));
    output_l = (fun enc o t l ef sf -> sprintf "(fun __l -> JsonTypes.Array (List.map (fun __e -> %s) __l))" (ef "__e" ""));
  };
  enc = {
    opa_enc1 with
      string_to_string = "__s", "JsonTypes.String";
      int_to_string = "__i", "JsonTypes.Int";
      int64_to_string = "__i64", "JsonTypes.Int";
      float_to_string = "__f", "JsonTypes.Float";
      bool_to_string = "__b", "JsonTypes.Bool";
      char_to_string = "__c", "(fun c -> JsonTypes.String (String.make 1 c))";
      unit_to_string = "__u", "(fun () -> JsonTypes.Void)";
      tuple_pre = (fun _ -> "JsonTypes.Record ["); tuple_post = "]";
      tup_pre = "("; tup_sep1 = ", "; tup_sep2 = "; "; tup_post = ")"; make_tup = (fun s -> "\""^s^"\"");
      record_pre = (fun _ -> "JsonTypes.Record ["); record_post = "]";
      rec_pre = "("; rec_sep1 = ", "; rec_sep2 = "; "; rec_post = ")"; make_rec = (fun s -> "\""^s^"\"");
      rec_void = "JsonTypes.Void";
  };
  prs = ocaml_prs1;
}

(* End of OCaml JSON output *)

(* OCaml BSL output *)

let ocaml_wrap_encoding () = {
  foldstrs = false;
  str = ref "";
  modname = !mns;
  prefix = "wrap";
  lang = {
    ocaml_lang with
    output_v = (fun enc vn o t to_j -> sprintf "(fun %s -> (%s %s))" vn to_j vn);
    output_m = (fun enc o t m pes ->
                  sprintf "(fun __t -> match %s with %s)"
                    m (String.concat "|" (List.map (fun (p,ef) -> sprintf "%s -> %s" p ((ef "__t"))) pes)));
    output_l = (fun enc o t l ef sf -> sprintf "(fun __l -> JsonTypes.Array (List.map (fun __e -> %s) __l))" (ef "__e" ""));
    make_ext_fn = (fun enc (tyns,exts) -> (function
                                           | [n] -> (sprintf "%s_%s" enc.prefix n)
                                           | [mn;n] -> (sprintf "%s.%s_%s" ("Bsl"^(String.lowercase mn)) enc.prefix n)
                                           | ("external"::t::tn) -> get_external_function enc exts t
                                           | _ -> assert false));
  };
  enc = {
    opa_enc1 with
      string_to_string = "__s", "ServerLib.wrap_string";
      int_to_string = "__i", "ServerLib.wrap_int";
      int64_to_string = "__i64", "ServerLib.wrap_int";
      float_to_string = "__f", "ServerLib.wrap_float";
      bool_to_string = "__b", "ServerLib.wrap_bool";
      char_to_string = "__c", "ServerLib.wrap_char";
      unit_to_string = "__u", "wrap_unit";
      record_pre = (fun _ -> "wrap_rcrd ["); record_post = "]";
      tuple_pre = (fun i -> sprintf "wrap_tuple%d(" i); tuple_post = ")";
      tup_pre = "("; tup_sep1 = ""; tup_sep2 = ","; tup_post = ")";
      make_tup = (fun _ -> "");
      rec_pre = "("; rec_sep1 = ", "; rec_sep2 = "; "; rec_post = ")"; make_rec = (fun s -> "\""^s^"\"");
      rec_void = "ServerLib.void";
  };
  prs = {
    ocaml_prs1 with
      tup_field = (fun i -> sprintf "f%d" (i+1));
  };
}

let ocaml_unwrap_encoding () = {
  foldstrs = false;
  str = ref "";
  modname = !mns;
  prefix = "unwrap";
  lang = {
    ocaml_lang with
    output_v = (fun enc vn o t to_j -> sprintf "(fun %s -> (%s %s))" vn to_j vn);
    output_m = (fun enc o t m pes ->
                  sprintf "(fun __t -> match %s with %s)"
                    m (String.concat "|" (List.map (fun (p,ef) -> sprintf "%s -> %s" p ((ef "__t"))) pes)));
    pat_rec = (fun enc labs vals -> (sprintf "[ %s ]"
                                       (String.concat "; " (List.map2 (fun l v -> sprintf "(\"%s\",%s)" l v) labs vals))));
    pat_tup = (fun args -> (sprintf "(Some(%s))" (String.concat ", " (List.map (fun a -> sprintf "(%s)" a) args))));
    tup_v = tup_v;
    make_ext_fn = (fun enc (tyns,exts) -> (function
                                           | [n] -> (sprintf "%s_%s" enc.prefix n)
                                           | [mn;n] -> (sprintf "%s.%s_%s" ("Bsl"^(String.lowercase mn)) enc.prefix n)
                                           | ("external"::t::tn) -> get_external_function enc exts t
                                           | _ -> assert false));
  };
  enc = {
    opa_enc1 with
      string_to_string = "__s", "ServerLib.unwrap_string";
      int_to_string = "__i", "ServerLib.unwrap_int";
      int64_to_string = "__i64", "ServerLib.unwrap_int";
      float_to_string = "__f", "ServerLib.unwrap_float";
      bool_to_string = "__b", "ServerLib.unwrap_bool";
      char_to_string = "__c", "ServerLib.unwrap_char";
      unit_to_string = "__u", "unwrap_unit";
      tuple_pre = (fun _ -> "("); tuple_post = ")";
      tup_pre = ""; tup_sep1 = ""; tup_sep2 = ", "; tup_post = ""; make_tup = (fun s -> "");
      record_pre = (fun enc -> sprintf "{%s" (if enc.modname = "" then "" else " "^(String.capitalize enc.modname)^". "));
      record_post = "}";
      rec_pre = ""; rec_sep1 = "="; rec_sep2 = "; "; rec_post = ""; make_rec = (fun s -> s);
      rec_void = "ServerLib.void";
  };
  prs = {
    ocaml_prs1 with
      pre_rec = Some (fun enc v -> sprintf "unwrap_rcrd %s" v);
      pre_tup = Some (fun enc i v -> sprintf "unwrap_tuple%d %s" i v);
  };
}

(* End of OCaml BSL output *)

(* OPA JSON input *)

(* Now abstract - common with opa output *)

(* End of OPA JSON input *)

(* OCaml JSON input *)

let ocaml_json_prs = {
  ocaml_prs1 with
    string_pat =  "JsonTypes.String", "__s", "__s";
    int_pat = "JsonTypes.Int", "__i", "__i";
    int64_pat = "JsonTypes.Int", "__i64", "__i64";
    float_pat = "JsonTypes.Float", "__f", "__f";
    bool_pat = "JsonTypes.Bool", "__b", "__b";
    char_pat = "JsonTypes.Char", "__c", "__c";
    unit_pat = "JsonTypes.Void", "", "()";
    opt_pat = (function
               | Some s -> sprintf "JsonTypes.Record [(\"Some\",%s)]" s
               | None -> "JsonTypes.Record [(\"None\",JsonTypes.Void)]");
    list_pat = (fun s -> sprintf "JsonTypes.Array %s" s);
    tup_field = (fun i -> sprintf "__%d" i);
    pre_tup = None;
    tuple_pat = (fun n -> sprintf "JsonTypes.Record [%s]"
                   (String.concat "; " (List.init n (fun i -> sprintf "(\"__%d\",__%d)" i i))));
    pre_rec = None;
    record_pat = (fun ls -> sprintf "JsonTypes.Record [%s]"
                    (String.concat "; " (List.map (fun l -> sprintf "(\"%s\",__%s)" l l) ls)));
    cons_pat = (fun l vo ->
                  (Some "__v",
                   sprintf "JsonTypes.Record [(\"%s\",%s)]" l (match vo with Some v -> "__v" | None -> "JsonTypes.Void")));
}

(* End of OCaml JSON input *)

(* Create functions *)

let create_ocaml_type tyns = function
  | O.TypeName (tes,tn) as te->
      (match get_tyn tn tyns with
       | InternalTyn tn -> str_of_tyn tn
       | _ -> type_ocaml_te tyns te)
  | te -> type_ocaml_te tyns te

let create_opa_type tyns = function
  | O.TypeName (tes,tn) as te->
      (match get_tyn tn tyns with
       | InternalTyn tn -> str_of_tyn tn
       | _ -> type_opa_te tyns te)
  | te -> type_opa_te tyns te

let rec create_te_fn tyns name = function
  | O.TypeName (tes,tn) -> [], [], [], ""
  | O.TypeConst cte -> ["__"^name], [type_ocaml_cte cte], [type_opa_cte cte], ("__"^name)
  | O.TypeTuple tes ->
      let args = List.mapi (fun i _te -> sprintf "__%d" i) tes in
      let typs = List.map (fun te -> create_ocaml_type tyns te) tes in
      let opatyps = List.map (fun te -> create_opa_type tyns te) tes in
      let els = List.mapi (fun i te -> sprintf "(__%d)" i) tes in
      args, typs, opatyps, sprintf "( %s )" (String.concat "," els)
  | O.TypeRecord l ->
      let args = List.map (fun (_,lab,_) -> "__"^lab) l in
      let typs = List.map (fun (_,_,te) -> create_ocaml_type tyns te) l in
      let opatyps = List.map (fun (_,_,te) -> create_opa_type tyns te) l in
      let tostrs = List.map (fun (_,lab,te) -> sprintf " %s=__%s " lab lab) l in
      args, typs, opatyps, sprintf "{ %s }" (String.concat ";" tostrs)
  | O.TypeConstructor cl -> [], [], [], ""

  | O.TypeVar (*of string (* 'a *)*) _ -> assert false
  | O.TypeRef (*of type_expr*) _ -> assert false
  | O.TypeArrow (*of type_expr * type_expr*) _ -> assert false
  | O.TypeLabel (*of bool (* optional *) * string * type_expr*) _ -> assert false
  | O.TypeVerbatim (*of string*) _ -> assert false

(* End of Create functions *)

(* Headers and footers *)

let hdr_all = "\n\
let map_while_opt f l =\n  \
  let rec aux = function\n  \
  | [] -> Some []\n  \
  | h::t -> match f h with | Some v -> (match aux t with | Some l -> Some (v::l) | None -> None) | None -> None\n  \
  in aux l\n\n"

let hdr_out mdls = sprintf "\n\
module type OUT =\n\
sig\n  \
  type t\n  \
  val output : t -> string -> t\n  \
end\n\n\
module MakeOutput ( Out : OUT ) =\n\
struct\n  \
  open Out\n  \
  type t = Out.t\n  \
\n  \
  %s\n\
\n  \
  let output_quoted t s = output t (\"\\\"\"^s^\"\\\"\")\n\n  \
\n\
" (String.concat "\n  " (List.map (fun mdl -> sprintf "module %s = %s.MakeOutput(Out)" mdl mdl) mdls))

let hdr_in mdls =
  (sprintf "\n\
module type IN =\n\
sig\n  \
  type t\n  \
  val s : string ref\n  \
  val p : int ref\n  \
  val input : t -> string\n\
end\n\n\
module MakeInput ( In : IN ) =\n\
struct\n  \
  open In\n  \
  type t = In.t\n\
\n  \
  %s\n" (String.concat "\n  " (List.map (fun mdl -> sprintf "module %s = %s.MakeInput(In)" mdl mdl) mdls)))^
(if !native_parser
 then "\
\n  \
    let rec next_s t =\n    \
      (*Printf.eprintf \"next_s: s=\\\"%s\\\" p=%d\\n%!\" !s !p;*)\n    \
      if !p >= (String.length !s)\n    \
      then (s := input t; p := 0; next_s t)\n    \
      else (incr p; (*Printf.eprintf \"next_s: c=%c\\n%!\" !s.[!p - 1];*) !s.[!p - 1])\n\
\n  \
    let rec peek_s t =\n    \
      (*Printf.eprintf \"peek_s: s=\\\"%s\\\" p=%d\\n%!\" !s !p;*)\n    \
      if !p >= (String.length !s)\n    \
      then (s := input t; p := 0; peek_s t)\n    \
      else ((*Printf.eprintf \"peek_s: c=%c\\n%!\" !s.[!p];*) !s.[!p])\n\
\n  \
  let rec lookahead_s t n =\n    \
    (*Printf.eprintf \"lookahead_s: s=\\\"%s\\\" p=%d\\n%!\" !s !p;*)\n    \
    if !p + n >= (String.length !s)\n    \
    then (s := (!s)^(input t); lookahead_s t n)\n    \
    else ((*Printf.eprintf \"lookahead_s: c=%c\\n%!\" !s.[!p+n];*) !s.[!p+n])\n\
\n  \
    let rec skip_s t = if !p >= (String.length !s) then (s := input t; p := 0; skip_s t) else incr p\n\
\n  \
    let rec skipn_s t n = if n > 0 then (skip_s t; skipn_s t (n-1))\n\
\n  \
    let rec clean_s () = if !p > 0 then (s := String.sub !s !p (String.length !s - !p); p := 0)\n\
\n  \
    let reset_s () = s := \"\"; p := 0\n\
\n  \
  let input_fixed_clean t str =\n    \
    (*Printf.eprintf \"input_fixed_clean: str=%s\\n%!\" str;*)\n    \
    let len = String.length str in\n    \
    let rec aux q =\n      \
      if q >= len\n      \
      then (skipn_s t len; clean_s (); true)\n      \
      else\n        \
        if (lookahead_s t q) <> str.[q]\n        \
        then false\n        \
        else (aux (q+1))\n    \
    in\n    \
    aux 0\n\
\n  \
    let input_fixed t str =\n    \
      (*Printf.eprintf \"input_fixed: str=%s\\n%!\" str;*)\n    \
      let len = String.length str in\n    \
      let rec aux q =\n      \
        if q >= len\n      \
        then true\n      \
        else\n        \
          if (peek_s t) <> str.[q]\n        \
          then false\n        \
          else (skip_s t; aux (q+1))\n    \
      in\n    \
      aux 0\n\
\n  \
    let input_fixed_ci t str =\n    \
      let len = String.length str in\n    \
      let rec aux q =\n      \
        if q >= len\n      \
        then true\n      \
        else\n        \
          if Char.lowercase (peek_s t) <> Char.lowercase str.[q]\n        \
          then false\n        \
          else (skip_s t; aux (q+1))\n    \
      in\n    \
      aux 0\n\
\n  \
    let input_fixeds t strs =\n    \
      let rec aux q = function\n      \
        | [] -> None\n      \
        | strs ->\n          \
            let rec aux2 acc = function\n            \
              | str::rest ->\n                \
                  if q >= String.length str\n                \
                  then Some str\n                \
                  else\n                  \
                    if str.[q] = peek_s t\n                  \
                    then aux2 (str::acc) rest\n                  \
                    else aux2 acc rest\n            \
              | [] -> (skip_s t; aux (q+1) acc)\n          \
            in\n          \
            aux2 [] strs\n    \
      in\n    \
      aux 0 strs\n\
\n  \
    let input_unit s t = if input_fixed t s then "^(optS())^"() else "^(fail())^"\n\
\n  \
    let input_string t =\n    \
      let b = Buffer.create 1024 in\n    \
      if input_fixed t \"\\\"\" \n    \
      then\n      \
        let rec aux = function\n        \
          | '\"' -> Buffer.contents b\n        \
          | c -> Buffer.add_char b c; aux (next_s t)\n      \
        in\n      \
        "^(optS())^"(aux (next_s t))\n    \
      else "^(fail())^"\n\
\n  \
    let input_char t =\n    \
      if input_fixed t \"'\" \n    \
      then\n    \
        let c = next_s t in\n      \
        if input_fixed t \"'\" \n      \
        then "^(optS())^"c\n      \
        else "^(fail())^"\n    \
      else "^(fail())^"\n\
\n  \
    let acc_class t b f =\n    \
      let rec aux tf = function\n      \
        | c when f c -> skip_s t; Buffer.add_char b c; aux true (peek_s t)\n      \
        | _ -> tf\n    \
      in\n    \
      aux false (peek_s t)\n\
\n  \
    let acc_dec t b = acc_class t b (function '0'..'9' -> true | _ -> false)\n\
\n  \
    let acc_one t b f =\n    \
      match peek_s t with\n    \
      | c when f c -> skip_s t; Buffer.add_char b c; true\n    \
      | _ -> false\n\
\n    \
    let input_int t =\n    \
      let b = Buffer.create 1024 in\n      \
      try\n       \
      (ignore (acc_one t b (function '-' -> true | _ -> false));\n        \
       ignore (acc_dec t b);\n        \
       if Buffer.length b = 0\n        \
       then "^(fail())^"\n        \
       else try "^(optS())^"(int_of_string (Buffer.contents b)) with Failure \"int_of_string\" -> "^(fail())^")\n      \
      with End_of_file -> if Buffer.length b = 0 then "^(fail())^" else "^(optS())^"(int_of_string (Buffer.contents b))\n\
\n  \
    let input_float t =\n    \
      let b = Buffer.create 1024 in\n      \
      try\n      \
      (ignore (acc_one t b (function '-'|'+' -> true | _ -> false));\n       \
       ignore (acc_dec t b);\n       \
       if acc_one t b (function '.' -> true | _ -> false) then ignore (acc_dec t b);\n       \
       if acc_one t b (function 'e'|'E' -> true | _ -> false)\n       \
       then (ignore (acc_one t b (function '-' -> true | _ -> false));\n             \
             ignore (acc_dec t b));\n       \
       if Buffer.length b = 0\n       \
       then "^(fail())^"\n       \
       else try "^(optS())^"(float_of_string (Buffer.contents b)) with Failure \"float_of_string\" -> "^(fail())^")\n      \
      with End_of_file -> if Buffer.length b = 0 then "^(fail())^" else "^(optS())^"(float_of_string (Buffer.contents b))\n\
\n  \
    let input_bool t =\n    \
      match peek_s t with\n    \
      | 't'|'T' -> skip_s t; if input_fixed_ci t \"rue\" then "^(optS())^"true else "^(fail())^"\n    \
      | 'f'|'F' -> skip_s t; if input_fixed_ci t \"alse\" then "^(optS())^"false else "^(fail())^"\n    \
      | _ -> "^(fail())^"\n\
\n  \
    let input_list pre sep post _in t =\n     \
      if input_fixed t pre\n     \
      then\n      \
        let rec aux l =\n        \
          if input_fixed_clean t post\n        \
          then "^(optS())^"(List.rev l)\n        \
          else\n          \
            (match _in t with\n           \
             | "^(if !ocaml_wrap_opt then "Some" else "")^" v ->\n             \
               (match input_fixeds t [sep;post] with\n               \
                 | Some s when s = sep -> aux (v::l)\n               \
                 | Some s when s = post -> "^(optS())^" (List.rev (v::l))\n               \
                 | _ -> "^(fail())^")\n"^
(if !ocaml_wrap_opt then "           | None -> "^(fail())^")\n" else "")^
"      in\n      \
       aux []\n    \
      else "^(fail())^"\n\
\n  \
  let input_external t input_ext =\n    \
    ignore (peek_s t);\n    \
    match input_ext !s !p with\n    \
    | Some (cnt,ext) -> (p := !p + cnt; Some ext)\n    \
    | None -> None\n\n\
"
 else "")

let ftr_out () = "\n    end\n\
\n"^(if !string_functions || !debug
     then "\
module B = MakeOutput (\n  \
  struct\n    \
    type t = Buffer.t\n    \
    let output b str = Buffer.add_string b str; b\n    \
  end)\n\n"
     else "")

let ftr_in () = "\n    end\n\
\n"^(if !string_functions || !debug
     then "\
module S = MakeInput (\n  \
  struct\n    \
    type t = string ref\n    \
    let (s, p) = ((ref \"\"), (ref 0))\n    \
    let input s = if !s = \"\" then raise End_of_file else let ss = !s in s := \"\"; ss\n    \
  end)\n\n"
     else "")

let hdri_out = "\n\
module type OUT =\n  \
  sig type t\n  \
  val output : t -> string -> t\n  \
end\n\
\n\
module MakeOutput :\n  \
  functor (Out : OUT) ->\n    \
    sig\n      \
      type t = Out.t\n"

let hdri_in = "\n\
module type IN =\n  \
  sig type t\n  \
  val s : string ref\n  \
  val p : int ref\n  \
  val input : t -> string\n\
end\n\
\n\
module MakeInput :\n  \
  functor (In : IN) ->\n    \
    sig\n      \
      type t = In.t\n"

let ftri = "    end\n\n"

let hdr_dbg () =
  (if !debug && (!string_functions || (!fromjson_functions && !tojson_functions))
   then
"let test name tos froms v =\n  \
  let tf = (try ("^(optS())^" v) = (froms (tos v)) with _ -> false) in\n  \
  Printf.printf \"%s_good=%b\\n\" name tf;;\n\
let ip = Ip.Ip (127,0,0,1);;\n\
let rabbit = (123, false);;\n\
let dog1 = Some 123;;\n\
let dog2 = None;;\n\
let cat0 = [];;\n\
let cat1 = [true; false];;\n\
let gender1 = Female 123;;\n\
let gender2 = Male true;;\n\
let gender3 = Neuter;;\n\
let linnaean = Linnaean (123,\"abc\");;\n\
let date = create_date 1961 8 24;;\n\
let hare = Some date;;\n\
let profile = create_profile \"abc\" 123 1.23 \"def\" true (Some \"ghi\") [\"jkl\";\"mno\";\"pqr\"] (Some (Female 123)) (Some (create_date 1961 8 24)) (999,\"SOS\") (Testopentypes.create_horse \"dobbin\" 12.0) ip;;\n"
   else "")^
    (if !debug && !string_functions
     then
"(test \"str_ip\" string_of_ip ip_of_string ip;\n\
test \"str_t0\" string_of_t0 t0_of_string ();\n\
test \"str_t1\" string_of_t1 t1_of_string 1234;\n\
test \"str_t2\" string_of_t2 t2_of_string 1234.;\n\
test \"str_t3\" string_of_t3 t3_of_string true;\n\
test \"str_rabbit\" string_of_rabbit rabbit_of_string rabbit;\n\
test \"str_dog1\" string_of_dog dog_of_string dog1;\n\
test \"str_dog2\" string_of_dog dog_of_string dog2;\n\
test \"str_cat0\" string_of_cat cat_of_string cat0;\n\
test \"str_cat1\" string_of_cat cat_of_string cat1;\n\
test \"str_gender1\" string_of_gender gender_of_string gender1;\n\
test \"str_gender2\" string_of_gender gender_of_string gender2;\n\
test \"str_gender3\" string_of_gender gender_of_string gender3;\n\
test \"str_linnaean\" string_of_linnaean linnaean_of_string linnaean;\n\
test \"str_date\" string_of_date date_of_string date;\n\
test \"str_hare\" string_of_hare hare_of_string hare;\n\
test \"str_profile\" string_of_profile profile_of_string profile);;\n\
" else "")^
    (if !debug && !fromjson_functions && !tojson_functions
     then
"(test \"json_ip\" tojson_ip fromjson_ip ip;\n\
test \"json_t0\" tojson_t0 fromjson_t0 ();\n\
test \"json_t1\" tojson_t1 fromjson_t1 1234;\n\
test \"json_t2\" tojson_t2 fromjson_t2 1234.;\n\
test \"json_json_t3\" tojson_t3 fromjson_t3 true;\n\
test \"json_rabbit\" tojson_rabbit fromjson_rabbit rabbit;\n\
test \"json_dog1\" tojson_dog fromjson_dog dog1;\n\
test \"json_dog2\" tojson_dog fromjson_dog dog2;\n\
test \"json_cat0\" tojson_cat fromjson_cat cat0;\n\
test \"json_cat1\" tojson_cat fromjson_cat cat1;\n\
test \"json_gender1\" tojson_gender fromjson_gender gender1;\n\
test \"json_gender2\" tojson_gender fromjson_gender gender2;\n\
test \"json_gender3\" tojson_gender fromjson_gender gender3;\n\
test \"json_linnaean\" tojson_linnaean fromjson_linnaean linnaean;\n\
test \"json_date\" tojson_date fromjson_date date;\n\
test \"json_hare\" tojson_hare fromjson_hare hare;\n\
test \"json_profile\" tojson_profile fromjson_profile profile);;\n"
     else "")

let opa_debug_vals () =
  if !debug
  then "\
ip = {a=127;b=0;c=0;d=1}:(ipopa.ip)\n\
t0 = void\n\
t1 = 123\n\
t2 = 123.456\n\
t3 = false\n\
t4 = \"abc\"\n\
rabbit = (456,true):((int,bool))\n\
dog1 = {none}:(option(int))\n\
dog2 = {some=789}:(option(int))\n\
cat = [true,false]:(list(bool))\n\
gender1 = {Female=123}:("^(!mns)^".gender)\n\
gender2 = {Male=false}:("^(!mns)^".gender)\n\
gender3 = {Neuter}:("^(!mns)^".gender)\n\
linnaean = {Linnaean=(123,\"abc\")}\n\
date = {day=24;month=8;year=1961}:("^(!mns)^".date)\n\
hare = {some=date}:("^(!mns)^".hare)\n\
profile = {id=\"id\";id_num=123;credits=123.456;email=\"email\";email_validated=false;real_name={some=\"me\"};about_me=[\"a\",\"b\",\"c\"];gender={some={Female=123}};date_of_birth={some={day=24;month=8;year=1961}};code=(111,\"aaa\");dobbin={name=\"horse\";height=1.23};ip={a=127;b=0;c=0;d=1}}:("^(!mns)^".profile)\n\n\
test(fromf,tof,v,s) =\n  \
  vs = tof(v)\n  \
  vv = fromf(vs)\n  \
  good = ({some=v} == vv)\n  \
  do println(\"{s}={vv} good={good}\")\n  \
  void\n\n"
  else ""

let hdr_ocbo_dbg () =
  if !debug
  then
    (opa_debug_vals())^"\
_ =\n\
\n  \
  do test(ip_of_string,string_of_ip,ip,\"of_string/string_of_ip\")\n  \
  do test(t0_of_string,string_of_t0,t0,\"of_string/string_of_t0\")\n  \
  do test(t1_of_string,string_of_t1,t1,\"of_string/string_of_t1\")\n  \
  do test(t2_of_string,string_of_t2,t2,\"of_string/string_of_t2\")\n  \
  do test(t3_of_string,string_of_t3,t3,\"of_string/string_of_t3\")\n  \
  do test(t4_of_string,string_of_t4,t4,\"of_string/string_of_t4\")\n  \
  do test(rabbit_of_string,string_of_rabbit,rabbit,\"of_string/string_of_rabbit\")\n  \
  do test(dog_of_string,string_of_dog,dog1,\"of_string/string_of_dog\")\n  \
  do test(dog_of_string,string_of_dog,dog2,\"of_string/string_of_dog\")\n  \
  do test(cat_of_string,string_of_cat,cat,\"of_string/string_of_cat\")\n  \
  do test(gender_of_string,string_of_gender,gender1,\"of_string/string_of_gender\")\n  \
  do test(gender_of_string,string_of_gender,gender2,\"of_string/string_of_gender\")\n  \
  do test(gender_of_string,string_of_gender,gender3,\"of_string/string_of_gender\")\n  \
  do test(linnaean_of_string,string_of_linnaean,linnaean,\"of_string/string_of_linnaean\")\n  \
  do test(date_of_string,string_of_date,date,\"of_string/string_of_date\")\n  \
  do test(hare_of_string,string_of_hare,hare,\"of_string/string_of_hare\")\n  \
  do test(profile_of_string,string_of_profile,profile,\"of_string/string_of_profile\")\n  \
  void\n"
  else ""

let hdr_bsl () = "\
let (@>) = Cps.Ops.(@>)\n\
let (|>) = Cps.Ops.(|>)\n\
\n\
let wrap_unit () = ServerLib.void\n\
let unwrap_unit (_:ServerLib.ty_void) = ()\n\
\n\
let wrap_opt wrap_a opt = ServerLib.wrap_option (match opt with Some a -> Some (wrap_a a) | None -> None)\n\
\n\
let unwrap_opt unwrap_a opt =\n  \
  match ServerLib.dot opt (ServerLib.static_field_of_name \"some\") with\n  \
  | Some a -> Some (Some (unwrap_a a))\n  \
  | None ->\n      \
      (match ServerLib.dot opt (ServerLib.static_field_of_name \"none\") with\n      \
       | Some _ -> Some None\n      \
       | None -> None)\n\
\n\
let wrap_lst wrap_a lst =\n  \
  let rec aux = function\n    \
    | [] ->\n        \
        ServerLib.make_record\n          \
          (ServerLib.add_field ServerLib.empty_record_constructor\n             \
             (ServerLib.static_field_of_name \"nil\") ServerLib.void)\n    \
    | (h::t) ->\n        \
        ServerLib.make_record\n          \
          (ServerLib.add_field\n             \
             (ServerLib.add_field ServerLib.empty_record_constructor\n               \
                (ServerLib.static_field_of_name \"tl\")\n               \
                (aux t))\n             \
             (ServerLib.static_field_of_name \"hd\")\n             \
             (wrap_a h))\n  \
  in\n  \
  aux lst\n\
\n\
let unwrap_lst unwrap_a l =\n  \
  let rec aux l =\n    \
    match ServerLib.dot l (ServerLib.static_field_of_name \"tl\") with\n    \
    | Some t ->\n        \
        (match ServerLib.dot l (ServerLib.static_field_of_name \"hd\") with\n         \
         | Some h ->\n             \
             (match aux t with\n              \
              | Some tt -> Some ((unwrap_a h)::tt)\n              \
              | None -> None)\n         \
         | None -> None)\n    \
    | None ->\n        \
        (match ServerLib.dot l (ServerLib.static_field_of_name \"nil\") with\n         \
         | Some _ -> Some []\n         \
         | None -> None)\n  \
 in\n  \
 aux l\n\
\n\
let wrap_rcrd fl =\n  \
  ServerLib.make_record\n    \
    (List.fold_right (fun (f,fv) r -> ServerLib.add_field r (ServerLib.static_field_of_name f) fv)\n      \
       fl ServerLib.empty_record_constructor)\n\
\n\
let unwrap_rcrd r =\n  \
  ServerLib.fold_record (fun f v acc -> match ServerLib.name_of_field f with | Some name -> (name, v)::acc | None -> acc) r []\n\
\n\
let wrap_tuple2 (wrap_a,wrap_b) (a,b) =\n  \
  ServerLib.make_record\n    \
    (ServerLib.add_field\n    \
    (ServerLib.add_field ServerLib.empty_record_constructor\n    \
    (ServerLib.static_field_of_name \"f2\") (wrap_b b))\n    \
    (ServerLib.static_field_of_name \"f1\") (wrap_a a))\n\
\n\
let unwrap_tuple2 (unwrap_a,unwrap_b) t =\n  \
  match (ServerLib.dot t (ServerLib.static_field_of_name \"f1\"),\n         \
         ServerLib.dot t (ServerLib.static_field_of_name \"f2\")) with\n  \
  | (Some v1,Some v2) -> Some (unwrap_a v1,unwrap_b v2)\n  \
  | _ -> None\n\
\n\
let wrap_tuple3 (wrap_a,wrap_b,wrap_c) (a,b,c) =\n  \
  ServerLib.make_record\n    \
    (ServerLib.add_field\n    \
    (ServerLib.add_field\n    \
    (ServerLib.add_field ServerLib.empty_record_constructor\n    \
    (ServerLib.static_field_of_name \"f3\") (wrap_c c))\n    \
    (ServerLib.static_field_of_name \"f2\") (wrap_b b))\n    \
    (ServerLib.static_field_of_name \"f1\") (wrap_a a))\n\
\n\
let unwrap_tuple3 (unwrap_a,unwrap_b,unwrap_c) t =\n  \
  match (ServerLib.dot t (ServerLib.static_field_of_name \"f1\"),\n         \
         ServerLib.dot t (ServerLib.static_field_of_name \"f2\"),\n         \
         ServerLib.dot t (ServerLib.static_field_of_name \"f3\")) with\n  \
  | (Some v1,Some v2,Some v3) -> Some (unwrap_a v1,unwrap_b v2,unwrap_c v3)\n  \
  | _ -> None\n\
\n\
let wrap_tuple4 (wrap_a,wrap_b,wrap_c,wrap_d) (a,b,c,d) =\n  \
  ServerLib.make_record\n    \
    (ServerLib.add_field\n    \
    (ServerLib.add_field\n    \
    (ServerLib.add_field\n    \
    (ServerLib.add_field ServerLib.empty_record_constructor\n    \
    (ServerLib.static_field_of_name \"f4\") (wrap_d d))\n    \
    (ServerLib.static_field_of_name \"f3\") (wrap_c c))\n    \
    (ServerLib.static_field_of_name \"f2\") (wrap_b b))\n    \
    (ServerLib.static_field_of_name \"f1\") (wrap_a a))\n\
\n\
let unwrap_tuple4 (unwrap_a,unwrap_b,unwrap_c,unwrap_d) t =\n  \
  match (ServerLib.dot t (ServerLib.static_field_of_name \"f1\"),\n         \
         ServerLib.dot t (ServerLib.static_field_of_name \"f2\"),\n         \
         ServerLib.dot t (ServerLib.static_field_of_name \"f3\"),\n         \
         ServerLib.dot t (ServerLib.static_field_of_name \"f4\")) with\n  \
  | (Some v1,Some v2,Some v3,Some v4) -> Some (unwrap_a v1,unwrap_b v2,unwrap_c v3,unwrap_d v4)\n  \
  | _ -> None\n\
\n\
let __idl_respond_client scheduler endpoint entrypoint_protocol userdata caml_client_cont caml_handler caml_final_cont =\n\
  (Hlnet.open_channel scheduler endpoint entrypoint_protocol.Hlnet.client_spec\n    \
   @> fun principal_channel ->\n      \
     Hlnet.sendreceive principal_channel None\n      \
     @> (fun chan ->\n           \
           let rec aux (userdata,continue) =\n             \
             if continue then\n                 \
                 (match caml_client_cont userdata with\n                  \
                  | (userdata,Some msg,true) ->\n                      \
                      (Hlnet.sendreceive chan msg\n                       \
                       @> (fun reply -> caml_handler userdata reply |> aux))\n                  \
                  | (userdata,Some msg,false) ->\n                      \
                      (Hlnet.send chan msg; caml_final_cont userdata)\n                  \
                  | (userdata,None,true) ->\n                      \
                      (Hlnet.receive chan\n                       \
                       @> (fun reply -> caml_handler userdata reply |> aux))\n                  \
                  | (userdata,None,false) ->\n                      \
                      caml_final_cont userdata)\n             \
             else caml_final_cont userdata\n           \
           in\n           \
           ignore (aux (userdata,true))));\n  \
  ServerLib.void\n\n\
"

let hdr_bslo () = ""

let opa_hdr modname =
  (opa_debug_vals())^(modname^" =\n\
{{\n\n  \
  output(t:'a, str:string) : 'a = t^str\n\n  \
  output_quoted(t:'a, s:string) :'a = output(t,(\"\\\"\"^s^\"\\\"\"))\n\n")

let opa_ftr () =
  if !debug
  then
"\n\
}}\n\
\n\
testout(out,v,n) =\n  \
  t = out(\"\")(v)\n  \
  do println(\"output_{n}({v})={t}\")\n  \
  void\n\
\n\
_ =\n  \
  do testout(STR_"^(!mns)^".output_ip,ip,\"ip\")\n  \
  do testout(STR_"^(!mns)^".output_t0,t0,\"t0\")\n  \
  do testout(STR_"^(!mns)^".output_t1,t1,\"t1\")\n  \
  do testout(STR_"^(!mns)^".output_t2,t2,\"t2\")\n  \
  do testout(STR_"^(!mns)^".output_t3,t3,\"t3\")\n  \
  do testout(STR_"^(!mns)^".output_t4,t4,\"t4\")\n  \
  do testout(STR_"^(!mns)^".output_rabbit,rabbit,\"rabbit\")\n  \
  do testout(STR_"^(!mns)^".output_dog,dog1,\"dog\")\n  \
  do testout(STR_"^(!mns)^".output_dog,dog1,\"dog\")\n  \
  do testout(STR_"^(!mns)^".output_cat,cat,\"cat\")\n  \
  do testout(STR_"^(!mns)^".output_gender,gender1,\"gender\")\n  \
  do testout(STR_"^(!mns)^".output_gender,gender2,\"gender\")\n  \
  do testout(STR_"^(!mns)^".output_gender,gender3,\"gender\")\n  \
  do testout(STR_"^(!mns)^".output_date,date,\"date\")\n  \
  do testout(STR_"^(!mns)^".output_linnaean,linnaean,\"linnaean\")\n  \
  do testout(STR_"^(!mns)^".output_hare,hare,\"hare\")\n  \
  do testout(STR_"^(!mns)^".output_profile,profile,\"profile\")\n  \
  void\n\n"
  else
    "\n}}\n\n"

let opa_json_hdr modname =
  modname^" =\n{{\n\n  \
  to_int = %%BslJson.Json.json_repr_int%%\n  \
  to_float = %%BslJson.Json.json_repr_float%%\n  \
  to_string = %%BslJson.Json.json_repr_string%%\n  \
  to_bool = %%BslJson.Json.json_repr_bool%%\n  \
  to_void = %%BslJson.Json.json_repr_void%%\n  \
  to_array = %%BslJson.Json.json_repr_array%%\n  \
  to_record = %%BslJson.Json.json_repr_record%%\n  \
  json_list_empty = %%BslJson.Json.json_list_empty%%\n  \
  json_list_cons = %%BslJson.Json.json_list_cons%%\n  \
  json_record_empty = %%BslJson.Json.json_record_empty%%\n  \
  json_record_cons = %%BslJson.Json.json_record_cons%%\n  \
  of_json_repr = %% BslJson.Json.of_json_repr%%\n\n  \
  opal2mll = (l -> to_array(List.fold((e, a -> json_list_cons(e, a)), l, json_list_empty())))\n\n  \
  opar2mlr = (l -> to_record(List.fold(((ch, j), a -> json_record_cons(ch, j, a)), l, json_record_empty())))\n\n\
"

let opa_json_ftr wrap_opt =
  if !debug
  then
    let _opt = if wrap_opt then "option" else "" in
sprintf
"\n}}\n\n\
_ =\n  \
  do test(JSON_"^(!mns)^".fromjson_ip,JSON_"^(!mns)^".tojson_ip,ip,\"from/tojson_ip\")\n  \
  do test(JSON_"^(!mns)^".fromjson_t0,JSON_"^(!mns)^".tojson_t0,t0,\"from/tojson_t0\")\n  \
  do test(JSON_"^(!mns)^".fromjson_t1,JSON_"^(!mns)^".tojson_t1,t1,\"from/tojson_t1\")\n  \
  do test(JSON_"^(!mns)^".fromjson_t2,JSON_"^(!mns)^".tojson_t2,t2,\"from/tojson_t2\")\n  \
  do test(JSON_"^(!mns)^".fromjson_t3,JSON_"^(!mns)^".tojson_t3,t3,\"from/tojson_t3\")\n  \
  do test(JSON_"^(!mns)^".fromjson_t4,JSON_"^(!mns)^".tojson_t4,t4,\"from/tojson_t4\")\n  \
  do test(JSON_"^(!mns)^".fromjson_rabbit,JSON_"^(!mns)^".tojson_rabbit,rabbit,\"from/tojson_rabbit\")\n  \
  do test(JSON_"^(!mns)^".fromjson_dog,JSON_"^(!mns)^".tojson_dog,dog1,\"from/tojson_dog1\")\n  \
  do test(JSON_"^(!mns)^".fromjson_dog,JSON_"^(!mns)^".tojson_dog,dog2,\"from/tojson_dog2\")\n  \
  do test(JSON_"^(!mns)^".fromjson_cat,JSON_"^(!mns)^".tojson_cat,cat,\"from/tojson_cat\")\n  \
  do test(JSON_"^(!mns)^".fromjson_gender,JSON_"^(!mns)^".tojson_gender,gender1,\"from/tojson_gender1\")\n  \
  do test(JSON_"^(!mns)^".fromjson_gender,JSON_"^(!mns)^".tojson_gender,gender2,\"from/tojson_gender2\")\n  \
  do test(JSON_"^(!mns)^".fromjson_gender,JSON_"^(!mns)^".tojson_gender,gender3,\"from/tojson_gender3\")\n  \
  do test(JSON_"^(!mns)^".fromjson_linnaean,JSON_"^(!mns)^".tojson_linnaean,linnaean,\"from/tojson_linnaean\")\n  \
  do test(JSON_"^(!mns)^".fromjson_date,JSON_"^(!mns)^".tojson_date,date,\"from/tojson_date\")\n  \
  do test(JSON_"^(!mns)^".fromjson_hare,JSON_"^(!mns)^".tojson_hare,hare,\"from/tojson_hare\")\n  \
  do test(JSON_"^(!mns)^".fromjson_profile,JSON_"^(!mns)^".tojson_profile,profile,\"from/tojson_profile\")\n  \
  /*do test(JSON_"^(!mns)^".fromjson_ll_ip,JSON_"^(!mns)^".tojson_ll_ip,ip,\"from/tojson_ll_ip\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_t0,JSON_"^(!mns)^".tojson_ll_t0,t0,\"from/tojson_ll_t0\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_t1,JSON_"^(!mns)^".tojson_ll_t1,t1,\"from/tojson_ll_t1\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_t2,JSON_"^(!mns)^".tojson_ll_t2,t2,\"from/tojson_ll_t2\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_t3,JSON_"^(!mns)^".tojson_ll_t3,t3,\"from/tojson_ll_t3\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_t4,JSON_"^(!mns)^".tojson_ll_t4,t4,\"from/tojson_ll_t4\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_rabbit,JSON_"^(!mns)^".tojson_ll_rabbit,rabbit,\"from/tojson_ll_rabbit\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_dog,JSON_"^(!mns)^".tojson_ll_dog,dog1,\"from/tojson_ll_dog\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_dog,JSON_"^(!mns)^".tojson_ll_dog,dog2,\"from/tojson_ll_dog\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_cat,JSON_"^(!mns)^".tojson_ll_cat,cat,\"from/tojson_ll_cat\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_gender,JSON_"^(!mns)^".tojson_ll_gender,gender1,\"from/tojson_ll_gender\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_gender,JSON_"^(!mns)^".tojson_ll_gender,gender2,\"from/tojson_ll_gender\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_gender,JSON_"^(!mns)^".tojson_ll_gender,gender3,\"from/tojson_ll_gender\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_linnaean,JSON_"^(!mns)^".tojson_ll_linnaean,linnaean,\"from/tojson_ll_linnaean\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_date,JSON_"^(!mns)^".tojson_ll_date,date,\"from/tojson_ll_date\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_hare,JSON_"^(!mns)^".tojson_ll_hare,hare,\"from/tojson_ll_hare\")\n  \
  do test(JSON_"^(!mns)^".fromjson_ll_profile,JSON_"^(!mns)^".tojson_ll_profile,profile,\"from/tojson_ll_profile\")*/\n  \
  void\n\
"
  else
    "\n}}\n\n"

(* End of Headers and footers *)

(* Useless because we can't define them in the mli file.*)
(*
module Pit = Parse_idltype\n\
module JT = JsonTypes\n\n
*)
let make_abbrevs oc =
  fprintf oc "\n\
let (@>) = Cps.Ops.(@>)\n\
let (|>) = Cps.Ops.(|>)\n\n"

let make_type_ocaml oc oci ocb ocbo tyns name te =
  let is_tyrec = (type_sl_te tyns false te) = "ServerLib.ty_record" in
  fprintf oc "type %s = %s\n" name (type_ocaml_te tyns te);
  if !bsl_file then fprintf oc "type sl_%s = %s\n\n" name (type_sl_te tyns false te);
  fprintf oci "type %s = %s\n" name (type_ocaml_te tyns te);
  if !bsl_file then fprintf oci "type sl_%s = %s\n\n" name (type_sl_te tyns false te);
  if !bsl_file
  then (if is_tyrec
        then fprintf ocb "##opa-type %s.%s\n" !mns name
        else fprintf ocb "##extern-type %s.%s = %s\n" !mns name (type_sl_te tyns true te);
        fprintf ocb "##extern-type %s.%s = %s.%s\n\n" !bslmns name !_Mns name;
        fprintf ocbo "type %s.%s = %s\n" !mns name (type_opa_te tyns te);
        fprintf ocbo "type %s.%s = external\n\n" !bslmns name)

let make_type_opa oco tyns name te =
  fprintf oco "type %s.%s = %s\n\n" !mns name (type_opa_te tyns te)

let make_output_ocaml ocaml_enc tynames oc oci name te =
  fprintf oc "let %s_%s t =\n  (*Printf.eprintf \"%s_%s\\n%%!\";*) %s\n\n"
             ocaml_enc.prefix name ocaml_enc.prefix name (flush_str ocaml_enc "output" (abs_output_te ocaml_enc "output" "t" tynames te));
  fprintf oci "      val %s_%s : Out.t -> %s -> Out.t\n" ocaml_enc.prefix name name

let make_wrap_bsl enc tyns ocb ocbo name te =
  let is_tyrec = (type_sl_te tyns false te) = "ServerLib.ty_record" in
  let wrap_enc = ocaml_wrap_encoding () in
  fprintf ocb "##register wrap_%s : %s -> opa[%s_%s]\n" name name !mns name;
  if is_tyrec
  then fprintf ocb "let %s_%s t = wrap_opa_%s_%s (%s t)\n\n" wrap_enc.prefix name !mns name (abs_un_wrap_te wrap_enc tyns te)
  else fprintf ocb "let %s_%s = %s\n\n" wrap_enc.prefix name (abs_un_wrap_te wrap_enc tyns te) ;
  fprintf ocbo "wrap_%s = %%%% %s.wrap_%s %%%% : %s.%s -> %s.%s\n" name !bslmns name !bslmns name !mns name;
  let unwrap_enc = ocaml_unwrap_encoding () in
  fprintf ocb "##register unwrap_%s : opa[%s_%s] -> %s\n" name !mns name name;
  if is_tyrec
  then fprintf ocb "let %s_%s t = %s (unwrap_opa_%s_%s t)\n\n"
                   unwrap_enc.prefix name (abs_un_wrap_te unwrap_enc tyns te) !mns name
  else fprintf ocb "let %s_%s = %s\n\n" unwrap_enc.prefix name (abs_un_wrap_te unwrap_enc tyns te);
  fprintf ocbo "unwrap_%s = %%%% %s.unwrap_%s %%%% : %s.%s -> %s.%s\n\n" name !bslmns name !mns name !bslmns name

let make_output_opa opa_enc tynames oco name te =
  fprintf oco "  %s_%s = (t -> %s)\n\n"
              opa_enc.prefix name (flush_str opa_enc "output" (abs_output_te opa_enc "output" "t" tynames te))

let make_input tynames oc oci name te =
  if !native_parser
  then
    let enc = { !ocaml_encoding with foldstrs=false; prefix="input"; prs=ocaml_prs3 } in
    fprintf oc "let %s_%s t =\n  (*Printf.eprintf \"%s_%s\\n\";*) %s\n\n" enc.prefix name enc.prefix name (abs_input2_te enc tynames te)
  else
    (fprintf oc "let _input_%s = %s\n\n" name (abs_input_te { !ocaml_encoding with prefix="_input" } tynames te);
     fprintf oc "let input_%s t =\n  \
  let (_pos,pt) = parse_function (input t) in\n  \
  _input_%s pt\n\n" name name);
  fprintf oci "      val input_%s : In.t -> %s%s\n" name name (opt())

let make_input_opa tynames oco name te = ()
  (*let enc = { !opa_encoding with foldstrs=false; prefix="input"; prs=opa_prs3 } in
  fprintf oco "let %s_%s t =\n  %s\n\n" enc.prefix name (abs_input2_te enc tynames te)*)

let make_jsonio_ocaml tynames oc oci name te =
  if !tojson_functions
  then (fprintf oc "let %s_%s = %s\n\n" ocaml_tojson_encoding.prefix name (abs_tojson_te ocaml_tojson_encoding tynames te);
        fprintf oci "val tojson_%s : %s -> JsonTypes.json\n\n" name name);

  if !fromjson_functions
  then (let enc = {!ocaml_encoding with
                     prefix="fromjson";
                     lang={ !ocaml_encoding.lang with tup_v = tup_v };
                     prs=ocaml_json_prs;} in
        fprintf oc "let fromjson_%s =\n  %s\n\n" name (abs_input_te enc tynames te);
        fprintf oci "val fromjson_%s : JsonTypes.json -> %s%s\n\n" name name (opt()))

let make_jsonio_opa tynames oco name te =
  if !tojson_functions
  then (fprintf oco "  %s_%s = %s\n\n" opa_tojson_encoding.prefix name (abs_tojson_te opa_tojson_encoding tynames te);
        (*fprintf oco "  %s_%s = %s\n\n" opa_tojson_ll_encoding.prefix name (abs_tojson_te opa_tojson_ll_encoding tynames te)*));

  if !fromjson_functions
  then (fprintf oco "  fromjson_%s = (__json -> ((%s)(__json)))\n\n"
          name (abs_input_te {!opa_encoding with modname="JSON"; prefix="fromjson"} tynames te);
        (*let err = if !opa_wrap_opt then "{none}" else "error(\"of_json_repr\")" in
        fprintf oco
          "  fromjson_ll_%s = (__json -> match of_json_repr(__json) with | {some=__j} -> ((fromjson_%s)(__j)) | {none} -> %s)\n\n"
          name name err*))

let make_create tyns oc oci _ocb _ocbo name te =
  match create_te_fn tyns name te with
  | [], [], [], "" -> ()
  | args, typs, opatyps, cr -> 
      (fprintf oc "let create_%s %s =\n  %s\n\n" name (String.concat " " args) cr;
       fprintf oci "val create_%s : %s -> %s\n\n" name (String.concat " -> " typs) name(*;
       We would have to wrap these. OPA can se debrouille.
       if !bsl_file
       then (fprintf ocb "##register create_%s : %s -> %s\n\n" name (String.concat " -> " opatyps) name
       )*)
      )

let make_string_of oc oci ocb ocbo name te =
  fprintf oc "let string_of_%s __%s = Buffer.contents (B.output_%s (Buffer.create 1024) __%s)\n\n" name name name name;
  fprintf oci "val string_of_%s : %s -> string\n\n" name name;
  if !bsl_file
  then (fprintf ocb "##register string_of_%s : %s -> opa[string]\n" name (opa_arg_type_te name te);
        fprintf ocb "let string_of_%s __%s = ServerLib.wrap_string (let res = %s.string_of_%s (unwrap_%s __%s) in (*Printf.eprintf \"string_of_%s: %%s\\n%%!\" res;*) res)\n\n"
                    name name !_Mns name name name name;
        fprintf ocbo "string_of_%s = %%%% %s.string_of_%s %%%% : %s.%s -> string\n\n" name !bslmns name !mns name)

let make_of_string oc oci ocb ocbo name te =
  fprintf oc "let %s_of_string str = try (*Printf.eprintf \"%s_of_string: str='%%s'\\n%%!\" str;*) %sS.input_%s (ref str) with End_of_file -> %s\n\n" name name (if !native_parser then "S.reset_s(); " else "") name (fail());
  fprintf oci "val %s_of_string : string -> %s%s\n\n" name name (opt());
  if !bsl_file
  then (fprintf ocb "##register %s_of_string : opa[string] -> opa[option(%s_%s)]\n" name !mns name;
        fprintf ocb "let %s_of_string __s = wrap_opt wrap_%s (%s.%s_of_string (ServerLib.unwrap_string __s))\n\n"
                    name name !_Mns name;
        fprintf ocbo "%s_of_string = %%%% %s.%s_of_string %%%% : string -> option(%s.%s)\n\n" name !bslmns name !mns name)

let sr_prefixes = ["sr_"; "sar"; "sndrcv"; "sendrecv"; "sendreceive"; "send_receive"; "sendandreceive"; "send_and_receive"]
let s_prefixes = ["s_"; "snd"; "send"]
let r_prefixes = ["r_"; "rcv"; "receive"]
let p_prefixes = ["p_"; "proto"; "protocol"]
let jp_prefixes = ["jp_"; "jproto"; "jsonprotocol"]
let rp_prefixes = ["rp_"; "rspnd"; "responder"]

let issrpfx = ispfx sr_prefixes
let isspfx = ispfx s_prefixes
let isrpfx = ispfx r_prefixes
let isppfx = ispfx p_prefixes
let isjppfx = ispfx jp_prefixes
let isrppfx = ispfx rp_prefixes

let make_sar_code oc base sars =
  let srs,rest = List.partition (function PI.IDLSar (name,_,_) -> issrpfx name | _ -> false) sars in
  let ss,rest = List.partition (function PI.IDLSar (name,_,_) -> isspfx name | _ -> false) rest in
  let rs,rest = List.partition (function PI.IDLSar (name,_,_) -> isrpfx name | _ -> false) rest in
  let ps,rest = List.partition (function PI.IDLSar (name,_,_) -> isppfx name | _ -> false) rest in
  let jps,rest = List.partition (function PI.IDLSar (name,_,_) -> isjppfx name | _ -> false) rest in
  let rps,rest = List.partition (function PI.IDLSar (name,_,_) -> isrppfx name | _ -> false) rest in
  if List.length rest <> 0
  then
    let names = List.map (function PI.IDLSar (name,_,_) -> name | _ -> assert false) rest in
    failwith (sprintf "Unknown Hlnet function prefix: %s" (String.concat ", " names))
  else
    let has_sar = List.length srs > 0 in
    let has_s = List.length ss > 0 in
    let has_r = List.length rs > 0 in
    let has_p = List.length ps > 0 in
    let has_jp = List.length jps > 0 in
    let has_rp = List.length rps > 0 in
    let get_log name =
      if !hlnet_logging
      then
        ((if name = "receive"
          then sprintf "%s \"entering %s function\";\n  " !logger_function name
          else sprintf "%s \"entering %s function for %%s\" msg_out;\n  " !logger_function name),
         sprintf "%s \"received %%s\" msg_in;\n    " !logger_function,
         sprintf "%s \"sending %%s\" msg_out;\n      " !logger_function,
         sprintf "%s \"receiving ...\";\n      " !logger_function,
         sprintf ";\n      %s \"sent %%s\" msg_out\n  " !logger_function,
         sprintf ";\n      %s \"... received\"\n  " !logger_function,
         (if name = "receive"
          then sprintf ";\n  %s \"exiting %s function\"" !logger_function name
          else sprintf ";\n  %s \"exiting %s function for %%s\" msg_out" !logger_function name))
      else ("","","","","","","")
    in
    if has_sar
    then begin
      let (l1,l2,l3,_,l5,_,l7) = get_log "sendreceive" in
      fprintf oc "\
let __idl_sendrecv (to_string : 'a -> string) (from_string : string -> 'b%s)\n                   \
                   (sched : Scheduler.t) (endpoint : Hlnet.endpoint) (chan_spec :(string,string) Hlnet.channel_spec) \n                   \
                   ?(on_disconnect=((fun () -> `abort):(unit -> [ `retry of Time.t | `abort ])))\n                   \
                   (msg : 'a) (cont : 'b%s -> unit) =\n  \
  let msg_out = to_string msg in\n  %s\
  let cont_s msg_in =\n    %s\
    cont (from_string msg_in)\n  \
  in\n  \
  (Hlnet.open_channel sched endpoint chan_spec ~on_disconnect) @> (\n    \
    fun chan ->\n      %s\
      Hlnet.sendreceive chan msg_out cont_s%s)%s\
\n\n" (opt()) (opt()) l1 l2 l3 l5 l7
    end;
    if has_s
    then begin
      let (l1,_,l3,_,l5,_,l7) = get_log "send" in
      fprintf oc "\
let __idl_send (to_string : 'a -> string)\n               \
               (sched : Scheduler.t) (endpoint : Hlnet.endpoint) (chan_spec : (string,string) Hlnet.channel_spec) \n               \
               ?(on_disconnect=((fun () -> `abort):(unit -> [ `retry of Time.t | `abort ])))\n               \
               (msg : 'a) =\n  \
  let msg_out = to_string msg in\n  %s\
  (Hlnet.open_channel sched endpoint chan_spec ~on_disconnect) @> (\n    \
    fun chan ->\n      %s\
      Hlnet.send chan msg_out%s)%s\
\n\n" l1 l3 l5 l7
    end;
    if has_r
    then begin
      let (l1,l2,_,l4,_,l6,l7) = get_log "receive" in
      fprintf oc "\
let __idl_recv (from_string : string -> 'b%s)\n               \
               (sched : Scheduler.t) (endpoint : Hlnet.endpoint) (chan_spec :(string,string) Hlnet.channel_spec) \n               \
               ?(on_disconnect=((fun () -> `abort):(unit -> [ `retry of Time.t | `abort ])))\n               \
               (cont : 'b%s -> unit) =\n  \
  %s\
  let cont_s msg_in =\n    %s\
    cont (from_string msg_in)\n  \
  in\n  \
  (Hlnet.open_channel sched endpoint chan_spec ~on_disconnect) @> (\n    \
    fun chan ->\n      %s\
      Hlnet.receive chan cont_s%s)%s\
\n\n" (opt()) (opt()) l1 l2 l4 l6 l7
    end;
    if has_p || has_jp || has_rp
    then begin
      let gets v f =
        if !hlnet_logging then sprintf "let s = %s in %s \"%s:%s '%%s'\" s; s" v !logger_function base f else v in
      let getu v f =
        if !hlnet_logging then sprintf "%s \"%s:%s '%%s'\" s; %s" !logger_function base f v else v in
      let sq = gets "string_of_q q" "serialise_query" in
      let uq = getu "q_of_string s" "unserialise_query" in
      let sr = gets "string_of_r r" "serialise_response" in
      let ur = getu "r_of_string s" "unserialise_response" in
      fprintf oc "\
let __idl_make_protocol q_of_string string_of_q r_of_string string_of_r =\n  \
  Hlnet.Aux.easy_protocol\n    \
    ~name:\"%s\" ~version:%d\n    \
    ~serialise_query:(fun q -> %s)\n    \
    ~unserialise_query: (fun _chan s -> %s)\n    \
    ~serialise_response: (fun r -> %s)\n    \
    ~unserialise_response: (fun _chan s -> %s)\n\n"
        base !protocol_version sq uq sr ur
    end;
    if has_rp
    then begin
      fprintf oc "\
let __idl_make_entrypoint_protocol client_protocol : (unit option, ('a, 'b) Hlnet.channel) Hlnet.protocol =\n  \
  Hlnet.define_protocol\n    \
    ~name:\"%s/entry\" ~version:%d\n    \
    ~serialise_query:      (fun _msg -> \"x\") (* FIXME: hlnet doesn't like \"\" *)\n    \
    ~unserialise_query:    (fun _chan _s _offset -> `data (None, 1))\n    \
    ~serialise_response:   (Hlnet.serialise_channel)\n    \
    ~unserialise_response: (Hlnet.unserialise_remote_channel client_protocol.Hlnet.client_spec)\n\
\n\
let respond_server entrypoint_protocol client_protocol scheduler endpoint (init_ud_server,init_ud_conn) responder =\n  \
  let ud_server_ref = ref init_ud_server in\n  \
  Hlnet.accept scheduler endpoint entrypoint_protocol.Hlnet.server_spec\n  \
  @> fun channel ->\n    \
    Hlnet.setup_respond channel\n      \
      (fun _msg fout ->\n      \
        fout (Hlnet.Aux.respond_on_new_channel channel client_protocol.Hlnet.server_spec\n                \
                (let ud_conn_ref = ref init_ud_conn in\n                \
                (fun msg ffout ->\n                   \
                   let ud_server, ud_conn, reply_opt, continue = responder (!ud_server_ref,!ud_conn_ref) msg in\n                   \
                   ud_server_ref := ud_server; ud_conn_ref := ud_conn;\n                   \
                   Option.iter ffout reply_opt;\n                   \
                   if not continue then Hlnet.refuse scheduler endpoint))))\n\
\n\
let respond_client entrypoint_protocol scheduler endpoint userdata client_cont final_cont =\n  \
  Hlnet.open_channel scheduler endpoint entrypoint_protocol.Hlnet.client_spec\n  \
  @> fun principal_channel ->\n    \
    Hlnet.sendreceive principal_channel None\n    \
    @> fun chan ->\n      \
      let rec aux userdata = function\n        \
        | true ->\n            \
            client_cont userdata\n            \
               (fun userdata -> function\n               \
                | (Some msg,Some handler) -> Hlnet.sendreceive chan msg @> (fun str -> handler userdata str aux)\n               \
                | (Some msg,None) -> (Hlnet.send chan msg; final_cont userdata)\n               \
                | (None,Some handler) -> Hlnet.receive chan @> (fun str -> handler userdata str aux)\n               \
                | (None,None) -> final_cont userdata)\n        \
        | false -> final_cont userdata\n      \
      in\n      \
      aux userdata true\n\n"
        base !protocol_version
    end

let opatyp = function "string" -> "string" | t -> sprintf "%s_%s" !mns t
let opa_typ = function "string" -> "string" | t -> sprintf "opa_%s_%s" !mns t
let opatyp_ = function "string" -> "string" | t -> sprintf "%s.%s" !mns t
(*let mltyp = function "string" -> "string" | t -> sprintf "%s.%s" !_Mns t in*)
let unwraptyp = function "string" -> "ServerLib.unwrap_string" | t -> sprintf "unwrap_%s" t
let wraptyp = function "string" -> "ServerLib.wrap_string" | t -> sprintf "wrap_%s" t

let make_sar_ocaml oc oci name st rt =
  if issrpfx name
  then
    (fprintf oc "let %s = __idl_sendrecv string_of_%s %s_of_string\n\n" name st rt;
     fprintf oci
       "val %s : Scheduler.t -> Hlnet.endpoint -> (string,string) Hlnet.channel_spec -> ?on_disconnect:(unit -> [ `retry of Time.t | `abort ]) -> %s -> (%s%s -> unit) -> unit\n\n"
       name st rt (opt()))
  else
  if isspfx name
  then
    (fprintf oc "let %s = __idl_send string_of_%s\n\n" name st;
     fprintf oci
       "val %s : Scheduler.t -> Hlnet.endpoint -> (string,string) Hlnet.channel_spec -> ?on_disconnect:(unit -> [ `retry of Time.t | `abort ]) -> %s -> unit\n\n"
       name st)
  else
  if isrpfx name
  then
    (fprintf oc "let %s = __idl_recv %s_of_string\n\n" name st;
     fprintf oci
       "val %s : Scheduler.t -> Hlnet.endpoint -> (string,string) Hlnet.channel_spec -> ?on_disconnect:(unit -> [ `retry of Time.t | `abort ]) -> (%s%s -> unit) -> unit\n\n"
       name st (opt()))
  else
  if isppfx name
  then
    let ofsfn = function "string" -> "(fun s -> Some s)" | t -> sprintf "%s_of_string" t in
    let soffn = function "string" -> "(fun s -> s)" | t -> sprintf "string_of_%s" t in
    (fprintf oc "let %s = __idl_make_protocol %s %s %s %s\n\n" name (ofsfn st) (soffn st) (ofsfn rt) (soffn rt);
     fprintf oci "val %s : (%s,%s) Hlnet.protocol\n\n" name st rt)
  else
  if isjppfx name
  then
    let ofsfn = function "string" -> "(fun s -> Some s)" | t -> sprintf "(fun __s -> fromjson_%s (Json_utils.from_string __s))" t in
    let soffn = function "string" -> "(fun s -> s)" | t -> sprintf "(fun __t -> Json_utils.to_string (tojson_%s __t))" t in
    (fprintf oc "let %s = __idl_make_protocol %s %s %s %s\n\n" name (ofsfn st) (soffn st) (ofsfn rt) (soffn rt);
     fprintf oci "val %s : (%s,%s) Hlnet.protocol\n\n" name st rt)
  else
  if isrppfx name
  then
    let bn = name_of_prefix rp_prefixes name in
    let ofsfn = function "string" -> "(fun s -> Some s)" | t -> sprintf "%s_of_string" t in
    let soffn = function "string" -> "(fun s -> s)" | t -> sprintf "string_of_%s" t in
    (fprintf oc "let protocol%s = __idl_make_protocol %s %s %s %s\n\n" bn (ofsfn st) (soffn st) (ofsfn rt) (soffn rt);
     fprintf oc "let entrypoint_protocol%s = __idl_make_entrypoint_protocol protocol%s\n\n" bn bn;
     fprintf oc "let port%s = ref %d\n" bn !default_port;
     fprintf oc "let addr%s = ref %s\n" bn !default_addr;
     fprintf oc "let endpoint%s = ref (Hlnet.Tcp (!addr%s, !port%s))\n" bn bn bn;
     fprintf oc "let scheduler%s = ref Scheduler.default\n\n" bn;
     fprintf oc "let init_responder%s port addr sched =\n  \
  port%s := port;\n  \
  addr%s := addr;\n  \
  endpoint%s := Hlnet.Tcp (!addr%s, !port%s);\n  \
  scheduler%s := sched\n\n" bn bn bn bn bn bn bn;
     fprintf oc "let respond_server%s (ud_shared,init_ud_conn) responder =\n  \
  respond_server entrypoint_protocol%s protocol%s !scheduler%s !endpoint%s (ud_shared,init_ud_conn) responder\n\n" bn bn bn bn bn;
     fprintf oc " let respond_client%s userdata client_cont final_cont =\n  \
  respond_client entrypoint_protocol%s !scheduler%s !endpoint%s userdata client_cont final_cont\n\n" bn bn bn bn;
     fprintf oc "let respond_client_single%s msg final_cont =\n  \
  let client_cont _ k = k None ((Some msg, Some (fun _ reply k -> k (Some reply) false))) in\n  \
  respond_client entrypoint_protocol%s !scheduler%s !endpoint%s None client_cont final_cont\n\n" bn bn bn bn;
     fprintf oc "\
let respond_client_send%s msg final_cont =\n  \
  let client_cont () k = k () ((Some msg, None)) in\n  \
  respond_client entrypoint_protocol%s !scheduler%s !endpoint%s () client_cont final_cont\n\n" bn bn bn bn;
     fprintf oc "let respond_client_receive%s final_cont =\n  \
  let client_cont _ k = k None ((None, Some (fun _ reply k -> k (Some reply) false))) in\n  \
  respond_client entrypoint_protocol%s !scheduler%s !endpoint%s None client_cont final_cont\n\n" bn bn bn bn;

     fprintf oci "val protocol%s : (%s,%s) Hlnet.protocol\n\n" bn st rt;
     fprintf oci "val entrypoint_protocol%s : (unit option, (%s,%s) Hlnet.channel) Hlnet.protocol\n\n" bn st rt;
     fprintf oci "val port%s : int ref\n" bn;
     fprintf oci "val addr%s : Unix.inet_addr ref\n" bn;
     fprintf oci "val endpoint%s : Hlnet.endpoint ref\n" bn;
     fprintf oci "val scheduler%s : Scheduler.t ref\n\n" bn;
     fprintf oci "val init_responder%s : int -> Unix.inet_addr -> Scheduler.t -> unit\n\n" bn;
     fprintf oci "val respond_server%s : ('a * 'b) -> (('a * 'b) -> %s -> 'a * 'b * %s option * bool) -> unit\n" bn st rt;
     fprintf oci "val respond_client%s : 'a -> ('a -> ('a -> %s option * ('a -> %s -> ('a -> bool -> unit) -> unit) option -> unit) -> unit) -> ('a -> unit) -> unit\n" bn st rt;
     fprintf oci "val respond_client_single%s : %s -> (%s option -> unit) -> unit\n" bn st rt;
     fprintf oci "val respond_client_send%s : %s -> (unit -> unit) -> unit\n\n" bn st;
     fprintf oci "val respond_client_receive%s : (%s option -> unit) -> unit\n" bn rt)
  else
    failwith (sprintf "Unknown Hlnet prefix '%s'\n" name)

let make_sar_bsl ocb ocbo name st rt =
  if issrpfx name then ()
  else if isspfx name then ()
  else if isrpfx name then ()
  else if isppfx name then ()
  else if isjppfx name then ()
  else
  if isrppfx name
  then
    let bn = name_of_prefix rp_prefixes name in
    (fprintf ocb "##opa-type Hlnet.protocol\n##extern-type endpoint = Hlnet.endpoint\n\n";
     fprintf ocb "##register get_protocol%s : opa[void] -> opa[hlnet_protocol]\n" bn;
     fprintf ocb "let get_protocol%s _ =\n  \
  wrap_opa_hlnet_protocol\n    \
    (ServerLib.make_record\n      \
      (ServerLib.add_field\n        \
         (ServerLib.add_field ServerLib.empty_record_constructor\n          \
            (ServerLib.static_field_of_name \"client_spec\") %s.protocol%s.Hlnet.client_spec)\n        \
         (ServerLib.static_field_of_name \"server_spec\") %s.protocol%s.Hlnet.server_spec))\n\n"
       bn !_Mns bn !_Mns bn;
     fprintf ocbo "get_protocol%s = %%%% %s.get_protocol%s %%%% : void -> Hlnet.protocol\n\n" bn !bslmns bn;

     fprintf ocb "##register get_entrypoint_protocol%s : opa[void] -> opa[hlnet_protocol]\n" bn;
     fprintf ocb "let get_entrypoint_protocol%s _ =\n  \
  wrap_opa_hlnet_protocol\n    \
    (ServerLib.make_record\n      \
      (ServerLib.add_field\n        \
         (ServerLib.add_field ServerLib.empty_record_constructor\n          \
            (ServerLib.static_field_of_name \"client_spec\") %s.entrypoint_protocol%s.Hlnet.client_spec)\n        \
         (ServerLib.static_field_of_name \"server_spec\") %s.entrypoint_protocol%s.Hlnet.server_spec))\n\n"
       bn !_Mns bn !_Mns bn;
     fprintf ocbo "get_entrypoint_protocol%s = %%%% %s.get_entrypoint_protocol%s %%%% : void -> Hlnet.protocol\n\n" bn !bslmns bn;

     fprintf ocb "##register get_port%s : opa[void] -> opa[int]\n" bn;
     fprintf ocb "let get_port%s _ = ServerLib.wrap_int !(%s.port%s)\n\n" bn !_Mns bn;
     fprintf ocbo "get_port%s = %%%% %s.get_port%s %%%% : void -> int\n\n" bn !bslmns bn;

     fprintf ocb "##register get_addr%s : opa[void] -> opa[string]\n" bn;
     fprintf ocb "let get_addr%s _ = ServerLib.wrap_string (Unix.string_of_inet_addr !(%s.addr%s))\n\n"
                 bn !_Mns bn;
     fprintf ocbo "get_addr%s = %%%% %s.get_addr%s %%%% : void -> string\n\n" bn !bslmns bn;

     fprintf ocb "##register get_endpoint%s : opa[void] -> endpoint\n" bn;
     fprintf ocb "let get_endpoint%s _ = !(%s.endpoint%s)\n\n" bn !_Mns bn;
     fprintf ocbo "get_endpoint%s = %%%% %s.get_endpoint%s %%%% : void -> Hlnet.endpoint\n\n" bn !bslmns bn;

     (*fprintf oc "let scheduler%s = ref Scheduler.default\n\n" bn; FIXME *)

     fprintf ocb "##register init_responder%s : opa[int], opa[string] -> opa[void]\n" bn;
     fprintf ocb "let init_responder%s port addr =\n  \
  %s.init_responder%s (ServerLib.unwrap_int port) (Unix.inet_addr_of_string (ServerLib.unwrap_string addr)) (*???BslScheduler.opa???*)Scheduler.default;\n  \
  ServerLib.void\n\n" bn !_Mns bn;
     fprintf ocbo "init_responder%s = %%%% %s.init_responder%s %%%% : int, string -> void\n\n" bn !bslmns bn;

     fprintf ocb "##opa-type serverdata('a,'b)\n";
     fprintf ocb "##opa-type serverarg('a,'b)\n";
     fprintf ocb "##opa-type serverres('a,'b)\n";
     fprintf ocb "##register respond_server%s : opa[serverdata('a,'b)], (opa[serverarg('a,'b)] -> opa[serverres('a,'b)]) -> opa[void]\n" bn;
     fprintf ocb "\
let respond_server%s (userdata:('a,'b) opa_serverdata) (opa_responder:('a,'b) opa_serverarg -> ('a,'b) opa_serverres) =\n  \
  let caml_userdata =\n    \
    match unwrap_tuple2 ((fun x -> x),(fun x -> x)) (unwrap_opa_serverdata userdata) with\n    \
    | Some (a,b) -> (a,b)\n    \
    | None -> raise (Failure \"respond_server%s: unwrap failure\")\n  \
  in\n  \
  let caml_responder __ud __v =\n    \
    match unwrap_tuple4 ((fun x -> x),(fun x -> x),unwrap_opt %s, ServerLib.unwrap_bool) (unwrap_opa_serverres (opa_responder (wrap_opa_serverarg (wrap_tuple2 ((wrap_tuple2 ((fun x -> x),(fun x -> x))),%s) (__ud,__v))))) with\n    \
    | Some (__ud_server,__ud_conn,Some __reply_opt,__continue) -> (__ud_server,__ud_conn,__reply_opt,__continue)\n    \
    | _ -> raise (Failure \"respond_server%s: unwrap failure\")\n  \
  in\n  \
  %s.respond_server%s caml_userdata caml_responder; ServerLib.void\n\n"
       bn bn (unwraptyp rt) (wraptyp st) bn !_Mns bn;
     fprintf ocbo "type serverdata('a,'b) = ('a,'b)\n";
     fprintf ocbo "type serverarg('a,'b) = (('a,'b),%s)\n" (opatyp_ st);
     fprintf ocbo "type serverres('a,'b) = ('a,'b,option(%s),bool)\n" (opatyp_ rt);
     fprintf ocbo "respond_server%s = %%%% %s.respond_server%s %%%% : serverdata('a,'b), (serverarg('a,'b) -> serverres('a,'b)) -> void\n\n" bn !bslmns bn;

     fprintf ocb "##opa-type clientres('a)\n";
     fprintf ocb "##opa-type handlerarg('a)\n";
     fprintf ocb "##opa-type handlerres('a)\n";
     fprintf ocb "##register respond_client%s : 'a, ('a -> opa[clientres('a)]), (opa[handlerarg('a)] -> opa[handlerres('a)]), ('a -> opa[void]) -> opa[void]\n" bn;
     fprintf ocb "\
let respond_client%s (userdata:'a) (opa_client_cont:'a -> 'a opa_clientres) (opa_handler:'a opa_handlerarg -> 'a opa_handlerres) (opa_final_cont:'a -> ServerLib.ty_void) =\n  \
  let caml_client_cont __v =\n  \
    match unwrap_tuple3 ((fun x -> x), unwrap_opt %s, ServerLib.unwrap_bool) (unwrap_opa_clientres (opa_client_cont __v)) with\n    \
    | Some (__userdata, Some __msg, __recv) -> (__userdata, __msg, __recv)\n    \
    | _ -> raise (Failure \"respond_client%s: unwrap failure\")\n  \
  in\n  \
  let caml_handler userdata reply =\n    \
    let __v = wrap_opa_handlerarg (wrap_tuple2 ((fun x -> x),%s) (userdata,reply)) in\n    \
    match unwrap_tuple2 ((fun x -> x),ServerLib.unwrap_bool) (unwrap_opa_handlerres (opa_handler __v)) with\n    \
    | Some (__userdata, __continue) -> (__userdata, __continue)\n    \
    | _ -> raise (Failure \"respond_client%s: unwrap failure\")\n  \
  in\n  \
  let caml_final_cont userdata = ignore (opa_final_cont userdata) in\n  \
  __idl_respond_client !(%s.scheduler%s) !(%s.endpoint%s) %s.entrypoint_protocol%s userdata caml_client_cont caml_handler caml_final_cont\n\n"
       bn (unwraptyp st) bn (wraptyp rt) bn !_Mns bn !_Mns bn !_Mns bn;
     fprintf ocbo "type clientres('a) = (('a,option(%s),bool))\n" (opatyp_ st);
     fprintf ocbo "type handlerarg('a) = (('a,%s))\n" (opatyp_ rt);
     fprintf ocbo "type handlerres('a) = (('a,bool))\n";
     fprintf ocbo "respond_client%s = %%%% %s.respond_client%s %%%% : 'a, ('a -> clientres('a)), (handlerarg('a) -> handlerres('a)), ('a -> void) -> void\n\n" bn !bslmns bn;

     fprintf ocb "##register respond_client_single%s : opa[%s], (opa[option(%s)] -> opa[void]) -> opa[void]\n"
       bn (opatyp st) (opatyp rt);
     fprintf ocb "\
let respond_client_single%s (opa_msg:%s) (opa_final_cont:ServerLib.ty_record -> ServerLib.ty_void) =\n  \
  let caml_client_cont _ = (None,Some (unwrap_%s opa_msg),true) in\n  \
  let caml_handler _ reply = (Some reply,false) in\n  \
  let caml_final_cont msg = ignore (opa_final_cont (wrap_opt %s msg)) in\n  \
  __idl_respond_client !(%s.scheduler%s) !(%s.endpoint%s) %s.entrypoint_protocol%s None caml_client_cont caml_handler caml_final_cont\n\n\
" bn (opa_typ st) st (wraptyp rt) !_Mns bn !_Mns bn !_Mns bn;
     fprintf ocbo "respond_client_single%s = %%%% %s.respond_client_single%s %%%% : %s, (option(%s) -> void) -> void\n\n"
       bn !bslmns bn (opatyp_ st) (opatyp_ rt);

     fprintf ocb "##register respond_client_send%s : opa[%s], (opa[void] -> opa[void]) -> opa[void]\n" bn (opatyp st);
     fprintf ocb "\
let respond_client_send%s (opa_msg:%s) (opa_final_cont:ServerLib.ty_void -> ServerLib.ty_void) =\n  \
  let caml_client_cont () = ((),Some (unwrap_%s opa_msg),false) in\n  \
  let caml_handler () _ = ((),false) in\n  \
  let caml_final_cont () = () in\n  \
  __idl_respond_client !(%s.scheduler%s) !(%s.endpoint%s) %s.entrypoint_protocol%s () caml_client_cont caml_handler caml_final_cont\n\n\
" bn (opa_typ st) st !_Mns bn !_Mns bn !_Mns bn;
     fprintf ocbo "respond_client_send%s = %%%% %s.respond_client_send%s %%%% : %s, (void -> void) -> void\n\n"
       bn !bslmns bn (opatyp_ st);

     fprintf ocb "##register respond_client_receive%s : (opa[option(%s)] -> opa[void]) -> opa[void]\n" bn (opatyp rt);
     fprintf ocb "\
let respond_client_receive%s (opa_final_cont:ServerLib.ty_record -> ServerLib.ty_void) =\n  \
  let caml_client_cont _ = (None,None,true) in\n  \
  let caml_handler _ reply = (Some reply,false) in\n  \
  let caml_final_cont reply_opt = ignore (opa_final_cont (wrap_opt %s reply_opt)) in\n  \
  __idl_respond_client !(%s.scheduler%s) !(%s.endpoint%s) %s.entrypoint_protocol%s None caml_client_cont caml_handler caml_final_cont\n\n" bn (wraptyp rt) !_Mns bn !_Mns bn !_Mns bn;
     fprintf ocbo "respond_client_receive%s = %%%% %s.respond_client_receive%s %%%% : (option(%s) -> void) -> void\n\n"
       bn !bslmns bn (opatyp_ rt))
  else
    failwith (sprintf "Unknown Hlnet prefix '%s'\n" name)


let output_trx oct =
  (*match !encoding_number with
  | 1 -> fprintf oct "%s" "\
read default.trx\n\
\n\
types:{{\n\
open Base\n\
open Tgrammar\n\
\n\
type pt_type =\n\
  | PT_Record of (string * pt_type) list\n\
  | PT_Option of pt_type option\n\
  | PT_List of pt_type list\n\
  | PT_Tuple of pt_type list\n\
  | PT_Cons of (string * pt_type option)\n\\n\
  | PT_String of string\n\
  | PT_Char of char\n\
  | PT_Int of int\n\
  | PT_Int64 of int64\n\
  | PT_Float of float\n\
  | PT_Bool of bool\n\
  | PT_Unit\n\
\n\\n\
}}\n\
\n\
quote <- '\"'\n\
literal <- quote (Default.stringchar* $_) quote {{ __2 }}\n\
\n\
recel <- \"(\" literal \",\" pt \")\" {{ (__2,__4) }}\n\
\n\
recels <- recel (\",\" recel {{ __2 }})* {{ __1::__2 }}\n\
\n\
record <- \"{\" \"Record\" \"=\" \"[\" recels \"]\" \"}\" {{ PT_Record __5 }}\n\
\n\
optel <- \"(\" quote \"Some\" quote \",\" pt \")\" {{ Some __6 }}\n\\n\
       / \"(\" quote \"None\" quote \")\" {{ None }}\n\
\n\
option <- \"{\" \"Option\" \"=\" \"[\" optel \"]\" \"}\" {{ PT_Option __5 }}\n\
\n\
lstel <- \"(\" pt \")\" {{ __2 }}\n\
\n\
lstels <- lstel (\";\" lstel {{ __2 }})* {{ __1::__2 }}\n\
\n\
list <- \"{\" \"List\" \"=\" \"[\" lstels \";\"? \"]\" \"}\" {{ PT_List __5 }}\n\
\n\
tupel <- \"(\" pt \")\" {{ __2 }}\n\\n\
\n\
tupels <- tupel (\",\" tupel {{ __2 }})* {{ __1::__2 }}\n\
\n\
tuple <- \"{\" \"Tuple\" \"=\" \"[\" tupels \",\"? \"]\" \"}\" {{ PT_Tuple __5 }}\n\
\n\
cnsel <- \"(\" literal \",\" pt \")\" {{ (__2,Some __4) }}\n\
       / \"(\" literal \")\" {{ (__2,None) }}\n\
\n\
cons <- \"{\" \"Cons\" \"=\" \"[\" cnsel \"]\" \"}\" {{ PT_Cons __5 }}\n\
\n\
string <- literal {{ PT_String __1 }}\n\
char <- Default.charsinglequote {{ PT_Char __1 }}\n\
integer <- Default.int {{ PT_Int __1 }}\n\
integer64 <- Default.int64 {{ PT_Int64 __1 }}\n\
float <- Default.float {{ PT_Float __1 }}\n\
bool <- Default.bool {{ PT_Bool __1 }}\n\
unit <- \"()\" {{ PT_Unit }}\n\
\n\
+pt : { pt_type } <- (record / option / list / tuple / cons / string / char / float / integer / integer64 / bool / unit) {{ __1 }}\n\
"
  | 2 ->*) fprintf oct "%s" "\
read default.trx\n\
\n\
types:{{\n\
open Base\n\
open Tgrammar\n\
\n\
type pt_type =\n\
  | PT_Record of (string * pt_type option) list\n\
  | PT_ListTuple of pt_type list\n\
  | PT_String of string\n\
  | PT_Char of char\n\
  | PT_Int of int\n\
  | PT_Int64 of int64\n\
  | PT_Float of float\n\
  | PT_Bool of bool\n\
  | PT_Unit\n\
\n\
}}\n\
\n\
quote <- '\"'\n\
literal <- quote (Default.stringchar* $_) quote {{ __2 }}\n\
\n\
recel <- literal \":\" pt {{ (__1,Some __3) }}\n\
       / literal {{ (__1,None) }}\n\
\n\
recels <- recel (\",\" recel {{ __2 }})* {{ __1::__2 }}\n\
\n\
record <- \"{\" recels \"}\" {{ PT_Record __2 }}\n\
\n\
lstels <- pt (\",\" pt {{ __2 }})* {{ __1::__2 }}\n\
\n\
listtuple <- \"[\" lstels \"]\" {{ PT_ListTuple __2 }}\n\
\n\
string <- literal {{ PT_String __1 }}\n\
char <- Default.charsinglequote {{ PT_Char __1 }}\n\
integer <- Default.int {{ PT_Int __1 }}\n\
integer64 <- Default.int64 {{ PT_Int64 __1 }}\n\
float <- Default.float {{ PT_Float __1 }}\n\
bool <- Default.bool {{ PT_Bool __1 }}\n\
unit <- \"()\" {{ PT_Unit }}\n\
\n\
+pt : { pt_type } <- (record / listtuple / string / char / float / integer / integer64 / bool / unit) {{ __1 }}\n\
"
  (*| _ -> assert false*)

let write_ml_types_file file mdls tynames exts idls sars =
  mns := modnamesuffix ".mlidl" file;
  _Mns := String.capitalize !mns;
  bslmns := prefixmodname !bsl_prefix ".mlidl" file;
  base := getbase file ".mlidl";
  (*eprintf "mns=%s bslmns=%s\n%!" (!mns) (!bslmns);*)
  let tyext = tynames, exts in

  if not !no_ocaml
  then (let mltfile = setsuffix file "" ".mlidl" !output_suffix ".ml" in
        if !verbose then eprintf "Output ML file=%s\n%!" mltfile;
        let oc = open_out mltfile in
        output_header oc file "ML types file" "(*" "*)";
        fprintf oc "let protocol_version = %d\n" !protocol_version;

        let mltifile = setsuffix file "" ".mlidl" !output_suffix ".mli" in
        if !verbose then eprintf "Output MLI file=%s\n%!" mltifile;
        let oci = open_out mltifile in
        output_header oci file "ML types interface file" "(*" "*)";
        fprintf oci "val protocol_version : int\n\n";

        let ocb, ocbo =
          if !bsl_file
          then (let bslfile = setsuffix file !bsl_prefix ".mlidl" !output_suffix ".ml" in
                if !verbose then eprintf "Output BSL file=%s\n%!" bslfile;
                let ocb = open_out bslfile in
                output_header ocb file "BSL file" "(*" "*)";

                let bslofile = setsuffix file !bsl_prefix ".mlidl" !output_suffix ".opa" in
                if !verbose then eprintf "Output OPA BSL file=%s\n%!" bslofile;
                let ocbo = open_out bslofile in
                output_header ocbo file "OPA BSL file" "/*" "*/";
                ocb, ocbo)
          else stderr, stderr
        in
        if not !native_parser
        then begin
          let trxfile = setsuffix file "" ".mlidl" !output_suffix "_parse.trx" in
          if !verbose then eprintf "Output TRX file=%s\n%!" trxfile;
          let oct = open_out trxfile in
          output_header oct file "TRX file" "#" "";
          output_trx oct;
          let base = rmv_suffix ".trx" (Filename.basename trxfile) in
          let pmod = String.capitalize base in
          let pname = pmod^"."^("parse_"^base^"_pt") in
          fprintf oc "module Parser = %s\nlet parse_function = %s\n\n" pmod pname;
          close_out oct
        end;

        make_abbrevs oc;
        List.iter (function
                   | PI.IDLType (name,te,_) -> make_type_ocaml oc oci ocb ocbo tyext name te
                   | _ -> assert false) idls;
        if !bsl_file
        then (fprintf ocb "%s" (hdr_bsl());
              fprintf ocbo "%s" (hdr_bslo());
              List.iter (function PI.IDLType (name,te,_) -> make_wrap_bsl !ocaml_encoding tyext ocb ocbo name te
                                | _ -> assert false) idls);
        fprintf oc "%s" hdr_all;
        fprintf oc "%s" (hdr_out mdls);
        fprintf oci "%s" hdri_out;
        List.iter (function PI.IDLType (name,te,_) ->
                     make_output_ocaml !ocaml_encoding tyext oc oci name te | _ -> assert false) idls;
        fprintf oc "%s" (ftr_out());
        fprintf oci "%s" ftri;
        fprintf oc "%s" (hdr_in mdls);
        fprintf oci "%s" hdri_in;
        List.iter (function PI.IDLType (name,te,_) -> make_input tyext oc oci name te | _ -> assert false) idls;
        fprintf oc "%s" (ftr_in());
        fprintf oci "%s" ftri;
        List.iter (function PI.IDLType (name,te,_) -> make_jsonio_ocaml tyext oc oci name te | _ -> assert false) idls;
        if !create_functions || !debug
        then List.iter (function
                        | PI.IDLType (name,te,_) -> make_create tyext oc oci ocb ocbo name te
                        | _ -> assert false) idls;
        (*if !bsl_file then fprintf ocb "##module Bsl%s\n\n" !_Mns;*)
        if !string_functions || !debug
        then (List.iter (function PI.IDLType (name,te,_) -> make_string_of oc oci ocb ocbo name te | _ -> assert false) idls;
              List.iter (function PI.IDLType (name,te,_) -> make_of_string oc oci ocb ocbo name te | _ -> assert false) idls);
        (*if !bsl_file then fprintf ocb "##endmodule\n\n";*)
        make_sar_code oc !base sars;
        List.iter (function PI.IDLSar (name,st,rt) -> make_sar_ocaml oc oci name st rt | _ -> assert false) sars;
        if !bsl_file
        then List.iter (function PI.IDLSar (name,st,rt) -> make_sar_bsl ocb ocbo name st rt | _ -> assert false) sars;
        fprintf oc "%s" (hdr_dbg ());
        if !bsl_file then fprintf ocbo "%s" (hdr_ocbo_dbg ());
        close_out oc; close_out oci;
        if !bsl_file then (close_out ocb; close_out ocbo));

  if not !no_opa
  then (let opafile = setsuffix file "" ".mlidl" !output_suffix ".opa" in
        if !verbose then eprintf "Output OPA file=%s\n%!" opafile;
        let oco = open_out opafile in
        output_header oco file "OPA file" "/*" "*/";
        List.iter (function PI.IDLType (name,te,_) -> make_type_opa oco tyext name te | _ -> assert false) idls;
        fprintf oco "%s" (opa_hdr ("STR_"^(!mns)));
        List.iter (function PI.IDLType (name,te,_) -> make_output_opa !opa_encoding tyext oco name te | _ -> assert false) idls;
        List.iter (function PI.IDLType (name,te,_) -> make_input_opa tyext oco name te | _ -> assert false) idls;
        fprintf oco "%s" (opa_ftr());
        fprintf oco "%s" (opa_json_hdr ("JSON_"^(!mns)));
        List.iter (function PI.IDLType (name,te,_) -> make_jsonio_opa tyext oco name te | _ -> assert false) idls;
        fprintf oco "%s" (opa_json_ftr !opa_wrap_opt);
        close_out oco)

let trimbr l r s =
  let s = String.trim s in
  if s.[0] = l && s.[String.length s - 1] = r
  then String.sub s 1 (String.length s - 2)
  else s

let getextal s0 =
  let s = trimbr '[' ']' s0 in
  let ss = String.split (function ';' -> true | _ -> false) s in
  let ss = List.map (trimbr '(' ')') ss in
  let ss =
    List.map (fun s ->
                match String.split (function ',' -> true | _ -> false) s with
                | [k;v] -> (k,v)
                | _ -> failwith (sprintf "Syntax error in external string '%s'" s0)) ss
  in
  if !verbose then eprintf "getextal: ss=%s\n" (string_of_al (fun s -> "\""^s^"\"") ss);
  (ss:((string * string) list))

(* Parsing artefact, FIX in parser!!! *)
let clean_idls idls =
  let idls = List.map (function
                       | PI.IDLType (name,te,so) -> PI.IDLType (String.trim name,te,Option.map String.trim so)
                       | PI.IDLSar (name,sndtype,rcvtype) -> PI.IDLSar (String.trim name,String.trim sndtype,String.trim rcvtype)
                       | PI.IDLLet expr -> PI.IDLLet expr
                      ) idls in
  let idls, rest = List.partition (function | PI.IDLType _ -> true | _ -> false) idls in
  let sars, rest = List.partition (function | PI.IDLSar _ -> true | _ -> false) rest in
  let opns, lets = List.partition (function | PI.IDLLet (Ocaml.Open [Ident.Source _]) -> true | _ -> false) rest in
  let tynames, exts =
    List.fold_left
      (fun (tynames,exts) -> function
       | PI.IDLType (name,_,None) -> ([name]::tynames), exts
       | PI.IDLType (name, Ocaml.TypeName ([], tn), Some s) -> (["external";name]@tn)::tynames, (name,(getextal s))::exts
       | _ -> assert false) ([],[]) idls
  in
  if !verbose && exts <> [] then eprintf "externals:\n%s\n%!" (string_of_exts exts);
  tynames, exts, idls, sars, opns, lets

let intersect tynames itynames =
  List.fold_right (fun itn acc -> if List.mem itn tynames then itn::acc else acc) itynames []

let read_idl_file file =
  if !verbose then eprintf "Input file: %s\n%!" file;
  let contents = File.content file in
  let (pos,idls) = PI.parse_parse_idl_idls contents in
  let tynames, exts, idls, sars, opns, lets = clean_idls idls in
  if pos <> String.length contents
  then eprintf "Parse error in file %s at %d\n%!" file pos
  else if !verbose then eprintf "%s\n%!" (String.concat "\n" (List.map string_of_idl idls));
  tynames, exts, idls, sars, opns, lets

let get_output_suffix mn os = List.fold_left (fun (mn,os) -> function
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "output_suffix")), Ocaml.Const (Ocaml.String str))]) ->
      (mn,str)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "module_name")), Ocaml.Const (Ocaml.String str))]) ->
      ((String.capitalize str),os)
  | _ -> (mn,os)) (mn,os)

let read_opens file tynames exts opns =
  let opts = save_opts () in
  let mdls, tynames, exts =
    List.fold_left (fun (mdls,tynames,exts) ->
                      (function
                       | PI.IDLLet (Ocaml.Open [Ident.Source _Modname]) ->
                           if !verbose then eprintf "Open: %s\n%!" _Modname;
                           let modname = String.copy _Modname in
                           modname.[0] <- Char.lowercase modname.[0];
                           let dir = Filename.dirname file in
                           let ifile = Filename.concat dir (modname^".mlidl") in
                           (*eprintf "ifile: %s\n" ifile;*)
                           let itynames, iexts, _idls, _sars, iopns, ilets = read_idl_file ifile in
                           let mn, os = get_output_suffix _Modname !output_suffix ilets in
                           let amn = mn^os in
                           if !verbose then eprintf "Open file output suffix: %s\n%!" os;
                           (*eprintf "tynames: %s\n%!" (string_of_tynames itynames);*)
                           let pimsg = sprintf ":\n  parent file: %s\n  opened file: %s\n%!" file ifile in
                           if iopns <> [] then (eprintf "Nested open statement%s\n%!" pimsg; assert false);
                           let isect = intersect tynames itynames in
                           if isect <> []
                           then (eprintf "Multiple type occurrences [%s]%s\n%!" (string_of_tynames isect) pimsg; assert false);
                           (amn::mdls, (tynames@(List.map (fun mn -> amn::mn) itynames)), iexts@exts)
                       | idl -> 
                           (eprintf "Unknown syntax: %s\n%!" (string_of_idl idl);
                            assert false))) ([],tynames,exts) opns
  in
  restore_opts opts;
  mdls, tynames, exts

(*let set_encoding_number = function
  | 1 -> (encoding_number := 1; ocaml_encoding := ocaml_encoding1; opa_encoding := opa_encoding1)
  | 2 -> (encoding_number := 2; ocaml_encoding := ocaml_encoding2; opa_encoding := opa_encoding2)
  | n -> failwith (sprintf "Unknown ocaml string output number: %d" n)*)

let set_bsl_prefix s =
  if s = ""
  then failwith "Can't set bsl_prefix to empty string because the BSL files would overwrite the ML files."
  else s

let process_inbuilt_options = List.iter (function
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "module_name")), Ocaml.Const (Ocaml.String str))]) ->
      (if !verbose then eprintf "Inbuilt: module_name = %s\n%!" str; module_name := str)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "output_suffix")), Ocaml.Const (Ocaml.String str))]) ->
      (if !verbose then eprintf "Inbuilt: output_suffix = %s\n%!" str; output_suffix := str)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "bsl_prefix")), Ocaml.Const (Ocaml.String str))]) ->
      (if !verbose then eprintf "Inbuilt: bsl_prefix = %s\n%!" str; bsl_prefix := (set_bsl_prefix str))
  (*| PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "encoding_number")), Ocaml.Const (Ocaml.Int n))]) ->
      (if !verbose then eprintf "Inbuilt: encoding_number = %d\n%!" n; set_encoding_number n)*)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "ocaml_wrap_opt")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: ocaml_wrap_opt = %b\n%!" b; ocaml_wrap_opt := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "opa_wrap_opt")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: opa_wrap_opt = %b\n%!" b; opa_wrap_opt := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "native_parser")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: native_parser = %b\n%!" b; native_parser := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "hlnet_logging")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: hlnet_logging = %b\n%!" b; hlnet_logging := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "logger_function")), Ocaml.Const (Ocaml.String str))]) ->
      (if !verbose then eprintf "Inbuilt: logger_function = %s\n%!" str; logger_function := str)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "protocol_version")), Ocaml.Const (Ocaml.Int n))]) ->
      (if !verbose then eprintf "Inbuilt: protocol_version = %d\n%!" n; protocol_version := n)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "default_port")), Ocaml.Const (Ocaml.Int n))]) ->
      (if !verbose then eprintf "Inbuilt: default_port = %d\n%!" n; default_port := n)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "default_addr")), Ocaml.Const (Ocaml.String str))]) ->
      (if !verbose then eprintf "Inbuilt: default_addr = %s\n%!" str; default_addr := str)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "create_functions")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: create_functions = %b\n%!" b; create_functions := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "tojson_functions")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: tojson_functions = %b\n%!" b; tojson_functions := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "fromjson_functions")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: fromjson_functions = %b\n%!" b; fromjson_functions := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "string_functions")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: string_functions = %b\n%!" b; string_functions := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "bsl_file")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: bsl_file = %b\n%!" b; bsl_file := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "no_ocaml")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: no_ocaml = %b\n%!" b; no_ocaml := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "no_opa")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: no_opa = %b\n%!" b; no_opa := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "verbose")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if b then eprintf "Inbuilt: verbose = %b\n%!" b; verbose := b)
  | PI.IDLLet (Ocaml.Let [(Ocaml.Pat (Ocaml.PatVar (Ident.Source "debug")), Ocaml.Const (Ocaml.Bool b))]) ->
      (if !verbose then eprintf "Inbuilt: debug = %b\n%!" b; debug := b)
  | PI.IDLLet expr ->
      failwith (sprintf "Mlidl.process_inbuilt_options: Unknown expr=%s" (Tools.str_of_expr expr))
  | _ -> assert false)

let files = ref ([]:string list)
let usage = "mlidl {options} <mlidl file>\n"

let translate_file file =
  let tynames, exts, idls, sars, opns, lets = read_idl_file file in
  let mdls, tynames, exts = read_opens file tynames exts opns in
  if !verbose then eprintf "tynames: %s\n%!" (string_of_tynames tynames);
  process_inbuilt_options lets;
  write_ml_types_file file mdls tynames exts idls sars

let _ =
  if testfile
  then ()
  else begin
    Arg.parse
      [("-output-suffix", (Arg.String (fun s -> default_opts := { !default_opts with opt_output_suffix = s })),
        "<string>\tOutput suffix (default: \"types\").");
       ("-bsl-prefix", (Arg.String (fun s -> default_opts := { !default_opts with opt_bsl_prefix = (set_bsl_prefix s) })),
        "<string>\tBsl prefix (default: \"bsl\").");
       ("-ocaml-wrap-options", (Arg.Bool (fun b -> default_opts := { !default_opts with opt_ocaml_wrap_opt = b })),
        (sprintf "<bool>\tWrap option around OCaml input (default: \"%b\").") (!default_opts).opt_ocaml_wrap_opt);
       ("-opa-wrap-options", (Arg.Bool (fun b -> default_opts := { !default_opts with opt_opa_wrap_opt = b })),
        (sprintf "<bool>\tWrap option around Opa input (default: \"%b\").") (!default_opts).opt_opa_wrap_opt);
       ("-native-parser", (Arg.Bool (fun b -> default_opts := { !default_opts with opt_native_parser = b })),
        (sprintf "<bool>\tUse native parser instead of TRX wrappers (default: \"%b\").")
          (!default_opts).opt_native_parser);
       (*("-encoding-number", (Arg.Int set_encoding_number),
        "<1,2>\tOutput encoding number, 1 or 2 (default: 1).");*)
       ("-hlnet-logging", (Arg.Bool (fun b -> default_opts := { !default_opts with opt_hlnet_logging = b })),
        (sprintf "<bool>\tAdd logger statements to Hlnet wrappers (default: \"%b\").") (!default_opts).opt_hlnet_logging);
       ("-logger-function", (Arg.String (fun s -> default_opts := { !default_opts with opt_logger_function = s })),
        "<string>\tLogger function (default: \"Logger.log\").");
       ("-protocol-version", (Arg.Int (fun n -> default_opts := { !default_opts with opt_protocol_version = n })),
        (sprintf "<int>\tProtocol version number (default: %d)." (!default_opts).opt_protocol_version));
       ("-default-port", (Arg.Int (fun n -> default_opts := { !default_opts with opt_default_port = n })),
        (sprintf "<int>\tDefault port number (default: %d)." (!default_opts).opt_default_port));
       ("-default-addr", (Arg.String (fun s -> default_opts := { !default_opts with opt_default_addr = s })),
        (sprintf "<string>\tDefault inet address (default: \"%s\")." (!default_opts).opt_default_addr));
       ("-create-functions", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_create_functions = s })),
        (sprintf "<bool>\tOutput create value from type functions (default: %b)." (!default_opts).opt_create_functions));
       ("-tojson-functions", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_tojson_functions = s })),
        (sprintf "<bool>\tOutput type to json functions (default: %b)." (!default_opts).opt_tojson_functions));
       ("-fromjson-functions", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_fromjson_functions = s })),
        (sprintf "<bool>\tOutput type from json functions (default: %b)." (!default_opts).opt_fromjson_functions));
       ("-string-functions", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_string_functions = s })),
        (sprintf "<bool>\tOutput type to/from string functions (default: %b)." (!default_opts).opt_string_functions));
       ("-bsl-file", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_bsl_file = s })),
        (sprintf "<bool>\tOutput BSL file for OCaml functions (default: %b)." (!default_opts).opt_bsl_file));
       ("-no-ocaml", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_no_ocaml = s })),
        (sprintf "<bool>\tDon't generate OCaml output (default: %b)." (!default_opts).opt_no_ocaml));
       ("-no-opa", (Arg.Bool (fun s -> default_opts := { !default_opts with opt_no_opa = s })),
        (sprintf "<bool>\tDon't generate OPA output (default: %b)." (!default_opts).opt_no_opa));
       ("-v", (Arg.Unit (fun () -> default_opts := { !default_opts with opt_verbose = true })), "\tVerbose.");
       ("-g", (Arg.Unit (fun () -> default_opts := { !default_opts with opt_debug = true })),
        "\tDebug (currently does nothing).");
      ]
      (fun str -> files := (!files)@[str])
      (usage^"Options:\n");
    if !files = []
    then printf "%s\n" usage
    else List.iter (fun file -> restore_opts !default_opts; translate_file file) (!files)
  end

(* End of file  mlidl.ml *)


(*
#use "../protocols/mlidl.ml";;
#use "../libnet/tests/testtypes.ml";;
let contents = File.content "../libnet/tests/test.mlidl";;
let (pos,idls) = Parse_idl.parse_parse_idl_idls contents;;
*)
