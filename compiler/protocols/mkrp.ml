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
(*
  FIXME: remove these open
*)
let eprintf fmt = Format.kfprintf (fun _ -> Format.pp_print_flush Format.err_formatter ()) Format.err_formatter fmt
let fprintf = Printf.fprintf

module List = Base.List
module Array = Base.Array
module String = Base.String
module Char = Base.Char

let ug = String.unsafe_get
let us = String.unsafe_sub

let allhdrs =
["Accept"; "Accept-Charset"; "Accept-Encoding"; "Accept-Language";
  "Accept-Ranges"; "Age"; "Allow"; "Authorization"; "CONNECT";
  "Cache-Control"; "Connection"; "Content-Disposition"; "Content-Encoding";
  "Content-Language"; "Content-Length"; "Content-Location"; "Content-MD5";
  "Content-Range"; "Content-Type"; "Cookie"; "Cookie2"; "DELETE"; "Date";
  "ETag"; "Expect"; "Expires"; "From"; "GET"; "HEAD"; "Host"; "If-Match";
  "If-Modified-Since"; "If-None-Match"; "If-Range"; "If-Unmodified-Since";
  "Keep-Alive"; "Last-Modified"; "Location"; "Max-Forwards"; "MyCookie";
  "Origin"; "POST"; "PUT"; "Pragma"; "Proxy-Authenticate";
  "Proxy-Authorization"; "Referer"; "ReqRange"; "Retry-After"; "Server";
  "Set-Cookie"; "Set-Cookie2"; "TE"; "TRACE"; "Trailer"; "Transfer-Encoding";
  "Upgrade"; "User-Agent"; "Vary"; "Via"; "WWW-Authenticate"; "Warning"]

let mnmx ci b mn mx =
  if ci
  then (min (Char.uppercase b) mn),(max (Char.lowercase b) mx)
  else (min b mn),(max b mx)

let mktab aname failfn mms =
  let rec mkatab0 mms =
    match mms with
    | [] -> ""
    | (mn,mx)::[] -> Printf.sprintf "Array.make %d %s" (Char.code mx - Char.code mn + 1) failfn
    | (mn,mx)::rest -> Printf.sprintf "Array.init %d (fun _ -> %s)" (Char.code mx - Char.code mn + 1) (mkatab0 rest) in
  "let "^aname^" = "^mkatab0 mms

let rplcpn str s = Str.global_replace (Str.regexp_string "%n") s str

let afilt p a l =
  let cnt = ref 0 in
  for i = 0 to l - 1 do
    if p i a.(i) then incr cnt;
  done;
  if (!cnt) = 0
  then None
  else
    let b = Array.make (!cnt) a.(0) in
    cnt := 0;
    for i = 0 to l - 1 do
      if p i a.(i) then begin
        b.(!cnt) <- a.(i); incr cnt
      end
    done;
    Some b

let saminmax sa = Array.fold_left (fun (a1,a2) b -> let l = String.length b in ((min a1 l),(max a2 l))) (max_int,min_int) sa

let minmax ci sa n = Array.fold_left (fun (a1,a2) b -> (mnmx ci b.[n] a1 a2)) ('\xff','\x00') sa

let saconcat sep a = String.concat sep (Array.to_list a)

let rec ipow x y = match y with | 0 -> 1 | 1 -> x | y -> x * (ipow x (y-1))

let cics cnt cs =
  Array.init (ipow 2 cnt)
             (fun i -> Array.init cnt (fun j ->
                                         if i land (ipow 2 j) <> 0
                                         then Char.lowercase cs.(j)
                                         else Char.uppercase cs.(j)))

let mkatab ci cnt cs mms =
  (Array.map (fun cs -> saconcat "." (Array.init cnt (fun i -> Printf.sprintf "(%d)" (Char.code cs.(i) - Char.code (fst (mms.(i)))))))
            (if ci then cics cnt cs else [|cs|]),
   saconcat "" (Array.map (fun ch -> (* (String.make 1) ch *) (Printf.sprintf "%02x" (Char.code ch))) cs))

let mkaccess cnt mms =
  saconcat "." (Array.init cnt (fun i -> Printf.sprintf "((Char.code c%d)-%d)" i (Char.code (fst (mms.(i))))))

let mkchk from_n mms cnt inname error =
  let get_n i = if from_n then Printf.sprintf "(n+%d)" i else Printf.sprintf "%d" i in
  (saconcat "\n" (Array.init cnt (fun i -> Printf.sprintf "  let c%d = String.unsafe_get %s %s in" i inname (get_n i))))^"\n"^
  (saconcat "\n" (Array.init cnt (fun i -> Printf.sprintf "  if c%d < '%s' || c%d > '%s' then %s;"
                                                    i (Char.escaped (fst (mms.(i)))) i (Char.escaped (snd (mms.(i)))) error)))

let mkus args = String.concat " " (List.map (fun s -> "_"^s) (String.slice ' ' args))

let mkerrargs errordef argtys =
  String.concat " " (List.map (fun (a,t) -> "("^(if List.mem_assoc a errordef then "" else "_")^a^":"^t^")") argtys)

let mkargs def = String.concat " " (List.map fst def)

let mktype def = String.concat " * " (List.map snd def)

let mkfntype def = String.concat " -> " (List.map snd def)

let mktuple = function [] -> "" | [(a,_)] -> a | def -> "("^String.concat "," (List.map fst def)^")"

let runtime prefix cnt _ci =
  Printf.sprintf "let %s_scmp_ ug s1 s2 m n =\n\
    let p = ref m in\n\
    let q = ref %d in\n\
    while !p < n && ug s1 (!p) = ug s2 (!q) do incr p; incr q done;\n\
    !p = n\n\
    let %s_scmp = %s_scmp_ String.unsafe_get\n\
    let %s_scmp_ci = %s_scmp_ (fun str n -> Char.lowercase (String.unsafe_get str n))\n\
" prefix cnt
  (*(if ci then "Char.lowercase (" else "") (if ci then ")" else "")
  (if ci then "Char.lowercase (" else "") (if ci then ")" else "")*)
  prefix prefix prefix prefix

let index_of str a = Array.fold_left_i (fun ii b i -> if b = str then i else ii) (-1) a

let iter_norpt f a = ignore (Array.fold_left (fun dn x -> if List.mem x dn then dn else (f x; x::dn) ) [] a)

let mktab1 ?(header="")
               ?(prefix="is")
               ?(failfn_opt=None)
               ?(ci = false)
               ?(from_n=false)
               ?(str_to_arg=(fun s -> "%n","",s))
               ?(fn_array=false)
               ?(argtys=[("str","string");("strlen","int");("nxt","int")])
               ?(errordef=[("nxt","int")])
               ?(intype="unit")
               ?(restype="unit")
               filename ushdrs cnt n =
  let hdrs = Array.copy ushdrs in
  Array.sort String.compare hdrs; (* <--- essential! *)
  let (samin,_samax) = saminmax hdrs in
  if samin < cnt then begin eprintf "mkrp: minimum string length is less than dimension\n"; exit 1 end;
  let oc = if filename = "stdout" then stdout else open_out filename in
  let aname = prefix^"_tab" in
  let failfn = Option.default (prefix^"_fail") failfn_opt in
  let inname = fst (List.hd argtys) in
  let args = mkargs argtys in
  let fntype = mkfntype (argtys@[("","int")]@[("",intype)]@[("",restype)]) in
  let fn = if from_n then "n " else "" in
  let ffn = if from_n then "(_n:int) " else "" in
  let error,exntype =
    if errordef = []
    then Printf.sprintf "raise ParseFail_%s" prefix,""
    else Printf.sprintf "raise (ParseFail_%s %s)" prefix (mktuple errordef),Printf.sprintf " of %s" (mktype errordef) in
  Array.sort String.compare hdrs;
  let hlen = Array.length hdrs in
  let mms = Array.init cnt (minmax ci hdrs) in
  (*let _attlens = Array.map String.length hdrs in*)
  let lcs = Array.create cnt '\000' in
  fprintf oc "(* Generated by mkrp.ml - %s *)\n%s\n" (Date.rfc1123 (Unix.gmtime (Unix.gettimeofday()))) header;
  fprintf oc "exception ParseFail_%s%s\n\n" prefix exntype;
  fprintf oc "%s\n" (runtime prefix cnt ci);
  if Option.is_none failfn_opt
  then fprintf oc "let %s (_:%s%s%s) %s %s= %s\n\n"
               failfn (if fn_array then "(" else "") fntype (if fn_array then ") array" else "")
               (mkerrargs errordef argtys) ffn error;
  fprintf oc "%s\n" (mktab aname failfn (Array.to_list mms));
  for i = 0 to hlen - 1 do
    let cs = Array.init cnt (fun j -> ug hdrs.(i) (n+j)) in
    let inds,ss = mkatab ci cnt cs mms in
    let ffn j _c = Array.init cnt (fun k -> hdrs.(j).[n+k]) = cs in
    if cs <> lcs then begin
      match afilt ffn hdrs hlen with
        Some subhdrs ->
          Array.sort (fun s1 s2 -> Pervasives.compare (String.length s2) (String.length s1)) subhdrs;
          fprintf oc "\nlet %s%s rpfn %s %s=\n" prefix ss args fn;
          let fst = ref true in
          for k = 0 to Array.length subhdrs - 1 do
            let shlen = String.length subhdrs.(k) in
            if not (!fst) then fprintf oc "  else " else fprintf oc "  ";
            let cntn = if from_n then "n" else Printf.sprintf "%d" cnt in
            let shlenn =
              if from_n then if shlen = cnt then "n" else Printf.sprintf "(n+%d)" (shlen-cnt) else Printf.sprintf "%d" shlen in
            let tst =
              if cnt = shlen
              then "true"
              else Printf.sprintf "%s_scmp%s %s \"%s\" %s %s" prefix (if ci then "_ci" else "") inname subhdrs.(k) cntn shlenn in
            let idx = index_of subhdrs.(k) ushdrs in
            let n, lts, v = str_to_arg subhdrs.(k) in
            let n = rplcpn n shlenn in
            let lts = rplcpn lts shlenn in
            let v = rplcpn v shlenn in
            let fnidx = if fn_array then Printf.sprintf ".(%d)" idx else "" in
            fprintf oc "if %s\n" tst;
            fprintf oc "  then%s rpfn%s %s %s %s\n" lts fnidx args n v;
            fst := false
          done;
          fprintf oc "  else %s\n" error;
          iter_norpt (fun ind -> fprintf oc "let _ = %s.%s <- %s%s\n" aname ind prefix ss) inds;
      | None -> ();
    end;
    Array.blit cs 0 lcs 0 cnt
  done;
  fprintf oc "\nlet %s_mms = [|%s|]\n\n"
    prefix (saconcat ";" (Array.map (fun (mn,mx) -> Printf.sprintf "('%s','%s')" (Char.escaped mn) (Char.escaped mx)) mms));
  fprintf oc "let %s_call rpfn %s %s=\n  try\n%s\n  %s.%s rpfn %s%s\n  with _ -> %s\n"
             prefix args fn (mkchk from_n mms cnt inname error) aname (mkaccess cnt mms) args
             (if from_n then Printf.sprintf " (n+%d)" cnt else "") error;
  if filename <> "stdout" then close_out oc

(*
let rhtype = "[`string of string | `value of (string * string option) list] RequestHeader.t"

let cwd = Unix.getcwd()

let reqhdrs =
 [| "Cache-Control"; "Connection"; "Date"; "Pragma"; "Trailer"; "Transfer-Encoding"; "Upgrade"; "Via"; "Warning";
    "Allow"; "Content-Encoding"; "Content-Language"; "Content-Length"; "Content-Location"; "Content-MD5"; "Content-Range";
    "Content-Type"; "Content-Disposition"; "Expires"; "Last-Modified"; "Accept"; "Accept-Charset"; "Accept-Encoding";
    "Accept-Language"; "Authorization"; "Expect"; "From"; "Host"; "If-Match"; "If-Modified-Since"; "If-None-Match";
    "If-Range"; "If-Unmodified-Since"; "Max-Forwards"; "Proxy-Authorization"; "ReqRange"; "Referer"; "TE"; "User-Agent";
    "Cookie"; "NewCookie"; |]

let minus_to_under str = String.map (function '-' -> '_' | c -> c) str

let _ =
  let file = "libnet/http/rp_hdr.ml" in
  printf "mkrp: Generating %s/%s\n" cwd file;
  mktab1
    ~str_to_arg:(fun s -> "%n","","`"^minus_to_under s)
    ~prefix:"hdr"
    ~ci:true
    ~header:"open Requestdef\nopen RequestType\n"
    ~argtys:[("hdr","string");("hdrlen","int");("nxt","int");("rh",rhtype)]
    ~intype:"request_header"
    ~restype:rhtype
    ~errordef:[("nxt","int")]
    file reqhdrs 2 0

let typhdrs = [| "CONNECT"; "DELETE"; "GET"; "HEAD"; "OPTIONS"; "POST"; "PUT"; "TRACE" |]

let _ =
  let file = "libnet/http/rp_typ.ml" in
  printf "mkrp: Generating %s/%s\n" cwd file;
  mktab1
    ~str_to_arg:(fun s -> "%n","",String.capitalize (String.lowercase s))
    ~prefix:"typ"
    ~ci:true
    ~header:"open Requestdef\nopen RequestType\n"
    ~argtys:[("typ","string");("typlen","int");("nxt","int");("req","string");("reqlen","int")]
    ~intype:"_method"
    ~restype:"int * parse_request"
    ~errordef:[("nxt","int")]
    file typhdrs 1 0

let browhdrs = [| "Mozilla"; "Nokia"; "Opera"; "Microsoft"; "MOT"; "HTC"; "w3m"; "Seamonkey"; "Dillo"; "PSP";
                  "Wget"; "lwp-trivial"; "Lynx"; "Links"; "amaya";
                  "Googlebot"; "msnbot"; "MSNBOT"; "Yahoo! Slurp"; "YahooSeeker"; |]

let _ =
  let file = "libnet/http/rp_brow.ml" in
  printf "mkrp: Generating %s/%s\n" cwd file;
  mktab1
    ~str_to_arg:(fun s -> "%n","","\""^s^"\"")
    ~fn_array:true
    ~prefix:"brow"
    ~ci:false
    ~header:"open UserCompatType"
    ~argtys:[("brow","string");("browlen","int")(*;("nxt","int")*)]
    ~intype:"string"
    ~restype:"UserCompatType.renderer_engine"
    ~errordef:[(*("nxt","int")*)]
    file browhdrs 2 0
*)
