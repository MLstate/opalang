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
(** Encodings:
    Some routines to allow fast encode/decode of strings.

    Intended as replacements for cases where trx-generated parsers aren't fast
    enough.  Only use these routines if speed is your utmost priority.
*)
module List = Base.List
module String = Base.String
module Char = Base.Char

let us = String.unsafe_sub

(* HTTP encodings *)

(* %<hex><hex> -> <char> *)
let re1 = Str.regexp "\\(%[0-9a-fA-F][0-9a-fA-F]\\)"
let pchxhx =
  ((fun s i l -> i < l && s.[i] = '%'),
   re1,
   (fun s d -> Buffer.add_char d (Char.chr (Charf.c2h s.[1] s.[2]))))

let decode_string convlst s =
  let l = String.length s in
  let b = Buffer.create l in
  let rec aux i =
    if i < l
    then
        let (i,found) =
          List.fold_left (fun (i,found) (tr,re,fn) ->
                            if not found && tr s i l && Str.string_match re s i
                            then
                              let mtch = Str.matched_group 1 s in
                              fn mtch b;
                              (i+String.length mtch,true)
                            else (i,found)) (i,false) convlst in
        if found
        then aux i
        else (Buffer.add_char b s.[i]; aux (i+1))
    else ()
  in
  aux 0;
  Buffer.contents b

let http_unencode s =
  let l = String.length s in
  let r = String.copy s in
  let rec aux i j =
    if i < l
    then
      (match s.[i] with
       | '%' ->
           let k = i + 1 in
           if k + 1 < l
           then
             (match s.[k],s.[k+1] with
              | (ch1,ch2) when (Charf.is_hexf ch1 && Charf.is_hexf ch2) ->
                  (r.[j] <- Char.chr (Charf.c2h s.[k] s.[k+1]); aux (i+3) (j+1))
              | _ -> (r.[j] <- '%'; aux (i+1) (j+1)))
           else (r.[j] <- '%'; aux (i+1) (j+1))
       | ch -> (r.[j] <- ch; aux (i+1) (j+1)))
    else j
  in
  us r 0 (aux 0 0)

(* Encode a string according to a character predicate and an arbitrary encode function. *)
let encode_chars_filter ?(hint=(fun l -> (l + (l asr 4)))) is_char encode_char s =
  let l = String.length s in
  let b = Buffer.create (hint l) in
  let rec aux i j =
    if i < l
    then
      if is_char s.[i]
      then (Buffer.add_char b s.[i]; aux (i+1) (j+1))
      else
        let code = encode_char s.[i] in
        let clen = String.length code in
        Buffer.add_string b code; aux (i+1) (j+clen)
    else j
  in
  Buffer.sub b 0 (aux 0 0)

(* Encode all characters according to selective encode function *)
let encode_chars ?(hint=(fun l -> (l + (l asr 4)))) encode_char s =
  let l = String.length s in
  let b = Buffer.create (hint l) in
  let rec aux i j =
    if i < l
    then
      let code = encode_char s.[i] in
      let clen = String.length code in
      Buffer.add_string b code; aux (i+1) (j+clen)
    else j
  in
  Buffer.sub b 0 (aux 0 0)

(* Build a char map from a list of (char,string) pairs. *)
let encode_list_to_map encode_list =
  let chmap = Array.init 256 (fun i -> String.make 1 (Char.chr i)) in
  List.iter (fun (ch,code) -> chmap.(Char.code ch) <- code) encode_list;
  chmap

(* Generic URL encode. *)
let chhxmp = Array.init 256 (fun i -> Base.sprintf "%02X" i)
let pc_encode ch = "%"^(chhxmp.(Char.code ch))

(* RFC 1738 but excluding , / ? : @ & = + $ #
 * See  encodeURIComponent() in http://www.w3schools.com/jsref/jsref_encodeURIComponent.asp
 *)
let encode_uri_component = (fun s -> encode_chars_filter Charf.is_urlxf pc_encode s)

let encode_aws_uri = (fun s -> encode_chars_filter Charf.is_awsf pc_encode s)

let decode_uri_component = decode_string [ pchxhx ]

let escaped1 = [ "'","&#39;"; "&","&amp;"; ]
let escaped2 = (* ' redefined here for revert operation *)
  [ ">","&gt;"; "<","&lt;"; "\"","&quot;"; "'","&apos;";
    "\194\163","&pound;"; "\194\176","&deg;"; "\194\167","&sect;"
  ]

(* Generic, more optimised HTTP encode *)
let http_encode s =
  String.multi_replace (String.multi_replace s escaped1) escaped2

(* Fixme:
   There is already http_decode and http_unencode, but none are
   human-readable, so I add a function that revert http_encode
*)
let revert_http_encode s =
  let unescaped = List.map (fun (a,b) -> (b,a)) (escaped1 @ escaped2) in
  String.multi_replace s unescaped

exception HttpBodyRewriteError of int * string

let http_body_rewrite s =
  let l = String.length s in
  let skip is i = let rec aux i = if i >= l then i else if is s.[i] then aux (i+1) else i in aux i in
  let skip_space = skip Charf.is_spacef in
  let get_name i = let ii = skip Charf.is_namef i in (ii,String.sub s i (ii-i)) in
  let b = Buffer.create l in
  let get_value i =
    Buffer.clear b;
    let rec aux i =
      if i >= l
      then (i,Buffer.contents b)
      else
        match s.[i] with
        | '+' -> Buffer.add_char b ' '; aux (i+1)
        | '%' ->
            let i = i + 1 in
            if i >= l then raise (HttpBodyRewriteError (i,"expected 'u' or <hex>")) else
              (match s.[i] with
               | 'u' ->
                   let i = i + 1 in
                   if i + 4 > l then raise (HttpBodyRewriteError (i,"expected <hex><hex><hex><hex>")) else
                     (match s.[i],s.[i+1],s.[i+2],s.[i+3] with
                      | (ch1,ch2,ch3,ch4) when (Charf.is_hexf ch1 && Charf.is_hexf ch2 && Charf.is_hexf ch3 && Charf.is_hexf ch4) ->
                          Buffer.add_string b (Charf.c4u ch1 ch2 ch3 ch4); aux (i+4)
                      | _ ->
                          raise (HttpBodyRewriteError (i,"expected <hex><hex><hex><hex>")))
               | _ ->
                   if i + 2 > l then raise (HttpBodyRewriteError (i,"expected <hex><hex>")) else
                     (match s.[i],s.[i+1] with
                      | (ch1,ch2) when (Charf.is_hexf ch1 && Charf.is_hexf ch2) ->
                          Buffer.add_char b (Char.chr (Charf.c2h ch1 ch2)); aux (i+2)
                      | _ ->
                          raise (HttpBodyRewriteError (i,"expected <hex><hex>"))))
        | ch ->
            if Charf.is_charf ch
            then (i,Buffer.contents b)
            else (Buffer.add_char b ch; aux (i+1))
    in
    aux i
  in
  let rec aux i es =
    let i = skip_space i in
    if i >= l then (i,es) else
      let i,n = get_name i in
      if i >= l || n = "" then raise (HttpBodyRewriteError (i,"expected <name>")) else
        let i = skip_space i in
        if i >= l || s.[i] <> '=' then raise (HttpBodyRewriteError (i,"expected '='")) else
          let i,v =
            if i + 1 >= l
            then (i,"")
            else
              let i = skip_space (i+1) in
              if i >= l then raise (HttpBodyRewriteError (i,"expected <value>")) else
                if s.[i] = '&'
                then (i,"")
                else
                  let i,v = get_value i in
                  if v = "" then raise (HttpBodyRewriteError (i,"expected <value>")) else
                    i,v
          in
          let es = ((n,v)::es) in
          if i >= l then (i,es) else
            let i = skip_space i in
            if i + 1 >= l || s.[i] <> '&' then (i,es) else
              let i = skip_space (i+1) in
              if i + 1 >= l then raise (HttpBodyRewriteError (i,"expected <name>=<value>")) else
                aux i es
  in
  let i, es = aux 0 [] in
  (i,List.rev es)

(* End of file encodings.ml *)

