(*
    Copyright © 2011 MLstate

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


#<Debugvar:MIMETYPE_DEBUG>

module StringMap = Map.Make (struct type t = string let compare = Pervasives.compare end)
module String = BaseString
exception MimeType_NotFound
exception Malformed
exception Open of string

(* -- *)

type match_type =
  | String
  | Host16
  | Host32
  | Big16
  | Big32
  | Little16
  | Little32
  | Byte

type offset =
  | Range of int * int
  | Single of int

type magic =
    { m_type : match_type;
      m_value : string ;
      m_offset : offset;
      m_mask : string option;
      m_imbrik : magic list;
    }


type pattern =
  | Suffix of string
  | Other of string

type all_infos =
    { ai_mimetype : string;
      ai_patterns : pattern list;
      ai_magics   : magic list;
      ai_human_readable : string option;
      ai_subclassof : string option;
    }

type mimetype_database = all_infos list

(*TO STRING *)

let string_of_pattern = function
  | Suffix x -> "*"^x
  | Other x -> x

let string_of_offset = function
  | Range (a,b) -> Printf.sprintf "%d:%d" a b
  | Single s -> string_of_int s

let string_of_match_type = function
  | String  -> "string"
  | Host16  -> "host16"
  | Host32  -> "host32"
  | Big16   -> "big16"
  | Big32   -> "big32"
  | Little16-> "little16"
  | Little32-> "little32"
  | Byte    -> "byte"

let string_of_magic m =
  Printf.sprintf "%s / %s / %s / %s"
    (string_of_match_type m.m_type)
    m.m_value
    (string_of_offset m.m_offset)
    (match m.m_mask with | Some x -> x | None -> "-")


let string_of_all_infos ai =
  let bf = Buffer.create 100 in
  let add = Buffer.add_string bf in
  let addn () = add "\n" in
  add "[30m";
  add "Mime-type : ";
  add ai.ai_mimetype;
  addn ();
  add "Patterns : ";
  List.iter (fun x ->
               add (string_of_pattern x);
               add " - ") ai.ai_patterns;
  addn ();
  add "Magics : ";
  List.iter (fun x ->
               add (string_of_magic x);
               add " \n\t ")
    ai.ai_magics;
  addn();
  add "Humanreadable : ";
  add (match ai.ai_human_readable with | None -> "" | Some s -> s);
  addn ();
  add "Sub class of : ";
  add (match ai.ai_subclassof with None -> "" | Some s -> s);
  add "[0m";
  addn ();
  Buffer.contents bf


let pp fmt =
  Printf.printf (fmt^^"[0m\n%!")

let ipp fmt =
  Printf.ifprintf stdout (fmt^^"[0m\n%!")

let debug ?(level=0) fmt =
  let _level = succ level in
  #<If$minlevel _level> Printf.printf ("[1m[30m[MimeType][0m "^^fmt^^"\n%!")
  #<Else> Printf.ifprintf stdout fmt #<End>


(*TO STRING (end) *)


(*FROM STRING *)
let string_to_pattern s =
  if String.length s> 1 && s.[0] = '*' then
    Suffix (String.sub s 1 (String.length s -1))
  else
    Other s

let string_to_offset x =
  let beg,eend = String.split_char ':' x in
  if eend = "" then
    Single (int_of_string beg)
  else
    Range (int_of_string beg, int_of_string eend)


let string_to_match_type s =
  match s with
  | "string"  -> String
  | "host16"  -> Host16
  | "host32"  -> Host32
  | "big16"   -> Big16
  | "big32"   -> Big32
  | "little16"-> Little16
  | "little32"-> Little32
  | "byte"    -> Byte
  | _         -> raise Malformed



(*FROM STRING (end) *)

(*UTILS  *)
let replace avant apres =
  Str.global_replace (Str.regexp avant) apres


let chaine_de_remplacement s =
  let rmx s =
    let hexa = Str.regexp "\\\\x\\([0-9a-eA-E][0-9a-eA-E]\\)" in
    Str.global_substitute hexa
      (fun x ->
         let recup = "0x"^(Str.matched_group 1 x) in
         let valeur = int_of_string recup in
         let rempl = String.make 1 (Char.chr valeur) in
         rempl) s
  in

  let rmo s =
    let hexa = Str.regexp "\\\\\\([0-8][0-8][0-8]\\)" in
    Str.global_substitute hexa
      (fun x ->
         let recup = "0o"^(Str.matched_group 1 x) in
         let valeur = int_of_string recup in
         let rempl = String.make 1 (Char.chr valeur) in
         rempl) s
  in

  let last_modif s = rmx (rmo s) in

  let chaine = [
    ("&lt;",   "<");
    ("&gt;",   ">");
    ("&quot;", "\"");
  ] in

  last_modif (List.fold_left (fun acc (av,ap) -> replace av ap acc) s chaine)
;;


let petit_boutiste value =
  let length= String.length value in
  if length < 2 || length mod 2 != 0 then value
  else
    (let rec to_list x acc =
       if x >= length then
         acc
       else
         to_list (x+2) ((String.sub value x 2)::acc)
     in
     let maliste =
       if String.is_prefix "0x" value then
         "0x"::(to_list 2 [])
       else to_list 0 [] in
     let result = String.concat "" maliste in
     result)

let to_byte s =
  String.make 1 (Char.chr (int_of_string s))




let mise_enforme v = function
  | String -> chaine_de_remplacement v
  | Byte -> to_byte v
  | Little16
  | Little32 -> chaine_de_remplacement (petit_boutiste v)
      (*  | Host16 | Host32 -> failwith "refuse"*)
  | _ -> v



let list_find_opt f l = try Some (List.find f l) with Not_found -> None
let is_some = function Some _ -> true | None -> false

(*UTILS (end) *)

(*CREATE FUNCTIONS *)
let create_all_infos mt patterns magics hr sbc=
  { ai_mimetype = mt;
    ai_patterns = patterns;
    ai_magics   = magics;
    ai_human_readable = hr;
    ai_subclassof = sbc;
  }

let create_magic ?mask ttype value offset imbrik =
  { m_type = ttype;
    m_value = mise_enforme value ttype;
    m_offset = offset;
    m_mask = mask;
    m_imbrik = imbrik;
  }

(*CREATE FUNCTIONS (end) *)

let get_mimetype_aux filename database =
  let file =
    try open_in_bin filename
    with e -> raise (Open (Printexc.to_string e)) in
  let length = in_channel_length file in
  let content = String.make length ' ' in
  really_input file content 0 length;
  close_in_noerr file;

  let rec check_mime_list accumulator mimelist =
    match mimelist with
    | [] -> accumulator
    | mime::mime_rest ->
        (let rec check_magic_list magiclist =
           match magiclist with
           | [] -> false
           | magic::magic_rest ->
               (let value = magic.m_value in

                let relance cond =
                  if cond then
                    let c =
                      match magic.m_imbrik with
                     | [] -> true
                     | _ -> check_magic_list magic.m_imbrik
                    in
                    if c then true
                    else check_magic_list magic_rest
                  else
                    check_magic_list magic_rest
                in

                let aux () =
                  match magic.m_offset with
                  | Range (debut, fin) ->
                      (if length <= debut && length < fin then check_magic_list magic_rest
                       else
                         (let check_string =
                            is_some
                              (String.is_contained_from_until value content debut fin) in
                          relance check_string)
                      )

                  | Single x ->
                      (let debut, a_lire = x, (String.length value) in

                       (*TODO a optimiser *)
                       if length <= debut && length < (debut + a_lire) then check_magic_list magic_rest
                       else
                         (let fin = debut + a_lire in
                          let rec check_single_string i =
                            if i >= fin then true
                            else
                              (let r = content.[i] = value.[i-debut] in
                               if r then check_single_string (succ i)
                               else false)
                          in

                          relance (check_single_string debut))
                      )
                in aux ()
               ) in

         if check_magic_list mime.ai_magics then check_mime_list (mime::accumulator) mime_rest
         else check_mime_list accumulator mime_rest

        ) in

  let checkpatt pattern =
    match pattern with
    | Suffix suff ->
        String.is_suffix suff filename
    | Other pattern ->
        (match String.findi '*' pattern with
         | None -> filename = pattern
         | Some index ->
             (let length_pattern = String.length pattern in
              if index = String.length pattern -1 then
                String.is_prefix
                  (String.sub pattern 0 (length_pattern - 2)) filename
              else failwith (Printf.sprintf "je ne sais pas traiter ce genre de pattern %s" pattern)
             )
        )
  in


  let second_try mimelist =
    debug "no result : check_pattern";
    let res =
      list_find_opt
        (fun mime ->
           match mime.ai_patterns with
           | [] -> false
           | _ ->
               let rec aux = function
                 | [] -> false
                 | x::y ->
                     let r = checkpatt x in
                     if r then true
                     else aux y
               in aux mime.ai_patterns)
       mimelist
    in
    match res with
    | Some x -> x
    | None -> raise MimeType_NotFound
  in


  let accumulator = check_mime_list [] database in

  match accumulator with
  | [] -> second_try database
  | [x] -> x
  | _ ->
      let _ = debug "more than one result : %s" (BaseList.print string_of_all_infos accumulator) in
      try List.find (fun x -> is_some (list_find_opt (fun y -> checkpatt y) x.ai_patterns)) accumulator
      with Not_found -> second_try database


let get_mimetype filename database =
  debug "";
  debug "Check mimetype of %s" filename;
  try (get_mimetype_aux filename database).ai_mimetype
  with Sys_error s -> debug "problem !"; failwith ("Sys_error in mymetype detection : "^s)


let path_database mlstatedir =
  let p = PathTransform.string_to_mysys
           ~relative_position:(PathTransform.of_string (Lazy.force mlstatedir))
           "share/opa/mimetype_database.xml" in
  debug "Use database at : %s" p;
  p

let build_mimetype_database database =
  let ic = open_in database in
  let inic = Xmlm.make_input ~enc:(Some `UTF_8) ~strip:true (`Channel ic) in
  let input () = try Xmlm.input inic with (Xmlm.Error ((p1,p2), error)) as e -> (pp "[31m%d:%d : %s" p1 p2 (Xmlm.error_message error); raise e) in
  let is_end = function `El_end -> true | _ ->  false in
  let assert_end e = assert (is_end e) in
  let check s tag attr = match s with `El_start (("", t), lst) -> assert (t = tag && lst = attr)  | _ -> raise Malformed in
  let n t = ("",t) in

  let signal = input () in
  let _ = match signal with `Dtd None -> () | _ -> raise Malformed in

  let signal = input () in
  check signal "mimetype-database" [ (n "version"), "1.0"];

  let rec continue mimetypelist =
    match input () with
    | `El_end -> mimetypelist
    | `El_start (("", "mimetype"), (((("", "type"), ai_mimetype))::[])) ->
        (let signal = input () in
         check signal "patterns" [];

         (* on a au moins 1 pattern *)
         let rec aux acc =
           match input () with
           | `El_end -> acc
           | `El_start (("","patt"), [(("", "type"), "suffix"); (("", "value"), value) ]) ->
               (assert_end (input ());
                aux ((Suffix value) :: acc))
           | `El_start (("","patt"), [(("", "type"), "pattern"); (("", "value"), value) ]) ->
               (assert_end (input ());
                aux ((Suffix value) :: acc))
           | _ -> raise Malformed in

         let ai_patterns = aux [] in

         let signal = input () in
         let ai_human_readable =
           check signal "human-readable" [];
           match input () with
           | `El_end -> None
           | `Data value -> (assert_end (input ()); Some value)
           | _ -> raise Malformed
         in

         let signal = input () in
         let ai_subclassof =
           check signal "subclass-of" [];
           match input () with
           | `El_end -> None
           | `Data value -> (assert_end (input ()); Some value)
           | _ -> raise Malformed
         in

         let signal = input () in
         check signal "magics" [];

         let rec aux acc =
           match input () with
           | `El_end -> acc
           | `El_start (("","magic-number"), ((("","type"), m_type) :: ((("","offset")), m_offset) :: ((("","value")), m_value) :: y)) ->
               let m_type = string_to_match_type m_type in
               let m_offset = string_to_offset m_offset in
               let m_imbrik = aux [] in
               let m_mask = match y with [] -> None | [((("","mask")), m)] -> Some m | _ -> raise Malformed in

               (* replace escaped chars by them real values *)
               let m_value =
                 let res =
                   Str.global_substitute (Str.regexp "\\\\\\([0-9][0-9][0-9]\\)")
                     (fun x -> String.make 1 (Char.chr (int_of_string (Str.matched_group 1 x)))) m_value
                 in mise_enforme res m_type
               in

               let magic = { m_type; m_value; m_offset; m_mask; m_imbrik } in
               aux (magic :: acc)
           | _ -> raise Malformed
         in
         let ai_magics = aux [] in

         let signal = input () in
         assert_end signal;

         let mimetype = { ai_mimetype; ai_patterns; ai_magics; ai_human_readable; ai_subclassof } in

         continue (mimetype::mimetypelist))
    | _ -> raise Malformed
  in
  let db = continue [] in
  close_in_noerr ic;

(*  let mimetypes_by_hierarchie =
    let premium, others = List.partition (fun x -> match x.ai_subclassof with None -> true | _ -> false) db in
    let themap = List.fold_left (fun acc elem -> StringMap.add elem.ai_mimetype 0 acc) StringMap.empty premium in
    let rec aux (themap: int StringMap.t) thelist therest last =
      match thelist with
      | [] ->
          (match therest with
           | [] -> themap
           | _ ->
               (if last = List.length therest then
                 (pp "probleme, pas de parents pour : %s" (String.concat "; " (List.map (fun x -> x.ai_mimetype) therest)); raise Malformed)
                else
                  (aux themap therest [] (List.length therest))))
      | elem::rest ->
          (try
            let prof = StringMap.find (Option.get elem.ai_subclassof) themap in
               let themap = StringMap.add elem.ai_mimetype (succ prof) themap in
               aux themap rest therest last
          with Not_found -> aux themap rest (elem::therest) last)
    in aux themap others [] 0
  in

  let database =
    List.sort
      (fun elem1 elem2 ->
         let prof1 = StringMap.find elem1.ai_mimetype mimetypes_by_hierarchie in
         let prof2 = StringMap.find elem2.ai_mimetype mimetypes_by_hierarchie in
         (compare prof2 prof1))  db
  in database
*)

  db

let get_mimetype_database =
  let h = Hashtbl.create 3 in
  fun database ->
    try
      Hashtbl.find h database
    with Not_found ->
      let mdb = build_mimetype_database database in
      Hashtbl.add h database mdb;
      mdb


let mimetype_database mlstatedir = get_mimetype_database (path_database mlstatedir)
