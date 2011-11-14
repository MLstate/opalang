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
(** HttpTools:

    Just some support routines for handling HTTP requests and responses.

*)

#<Debugvar:HTTP_DEBUG>

let http = NetAddr.mk_protocol "HTTP"

module List = Base.List
module String = Base.String
module Char = Base.Char

let ug = String.unsafe_get
let us = String.unsafe_sub

(* Local profiling code
 * To be deleted when no longer required *)
let rpt n f a =
  let rec aux = function 0 -> () | n -> (f a; aux (pred n)) in
  aux n

let timefn n f a =
  let start = Unix.gettimeofday () in
  rpt n f a;
  Printf.printf "time: %f\n" ((Unix.gettimeofday()) -. start)

let verifyfn f pa px lst =
  List.for_all
    (fun (a,x) ->
       let res = f a in
       if res = x
       then true
       else
         (Logger.debug "Failed: '%s'\nExpected: '%s'\nGot: '%s'"
                  (String.escaped (pa a)) (String.escaped (px x)) (String.escaped (px res));
          false)) lst

(** Strip a header comment from a string.
    Comments can be nested.
    NOTE: only removes the final comment at the end of the line if it exists.
*)
let strcom str strlen =
  let rec aux n p =
    let ch = ug str p in
    if p >= 0
    then
      if ch = '('
      then if n <= 1 then p else aux (pred n) (pred p)
      else if ch = ')' then aux (succ n) (pred p) else aux n (pred p)
    else 0
  in
  let rec aux2 p =
    let ch = ug str p in
    if p >= 0
    then
      if Charf.is_spacef ch
      then aux2 (pred p)
      else
        if ch = ')'
        then aux 1 (pred p)
        else (succ p)
    else 0
  in
  let p = (aux2 (pred strlen)) in
  us str 0 p,
  if p = strlen then None else Some (us str p (strlen-p))

(** Remove leading and trailing spaces from a string.
*)
let rmldtrsp0 str strlen =
  let rec aux p =
    if p < strlen && Charf.is_spacef (ug str p)
    then aux (succ p)
    else p in
  let rec aux2 p =
    if p >= 0 && Charf.is_spacef (ug str p)
    then if p <= 0 then 0 else aux2 (pred p)
    else p in
  let st = aux 0 and nd = aux2 (pred strlen) in
  us str st ((nd-st)+1)

let rmldtrsp str = rmldtrsp0 str (String.length str)

let rmldtrsp2 (str1,str2) = (rmldtrsp str1, rmldtrsp str2)

let rmldtrspl sl = List.map rmldtrsp sl

(* Used by generated mkrp.ml code, provide string up to marker for
 * parsing http headers.  Note that if we can't find the marker
 * we actually return the whole string (multiline?).
 *)

let pos_mark ug mark mlen str strlen n =
  if mlen <= 0
  then Some (n,n)
  else
    let rec pc0 pos =
      let rec at m =
        if m >= mlen
        then true
        else
          if (pos+m) >= strlen
          then false
          else
            if (ug str (pos+m)) = (ug mark m)
            then at (m+1)
            else false
      in
      if pos >= strlen
      then None
      else
        if at 0
        then Some (pos, pos+mlen)
        else pc0 (pos+1) in
    pc0 n

let upto_mark_ ug mark mlen str strlen n =
  if n >= strlen
  then (n, 0, "")
  else
    if mlen >= strlen - n
    then (strlen, strlen - n, us str n (strlen-n))
    else
      match pos_mark ug mark mlen str strlen n with
      | Some (pos, p2) ->
          let l = pos - n in
          let l = min l ((strlen-l)+1) in
          (p2, l, (us str n l))
      | None ->
          (strlen, strlen - n, us str n (strlen-n))

let upto_mark = upto_mark_ ug
let upto_mark_ci = upto_mark_ (fun str n -> Char.lowercase (ug str n))

(* Stream-based versions *)

let cmp b mark =
  let blen = Buffer.length b in
  let mlen = String.length mark in
  if blen < mlen
  then false
  else
    let p = ref (blen - mlen) in
    let m = ref 0 in
    while !m < mlen && Buffer.nth b (!p) = String.unsafe_get mark (!m) do incr p; incr m done;
    !m = mlen

let upto_mark_stream get from mark =
  let rec aux b =
    if cmp b mark
    then Buffer.sub b 0 (Buffer.length b - String.length mark)
    else
      try
        Buffer.add_char b (get from);
        aux b
      with End_of_file -> Buffer.contents b
  in
  aux (Buffer.create 1024)

let rec get_char_cps read (str_ref, pos_ref, conn) cont2 =
  (*#<If>Logger.debug "HttpTools.get_char_cps: pos=%d str='%s'" (!pos_ref) (String.escaped (!str_ref))#<End>;*)
  let str, pos = !str_ref, !pos_ref in
  if pos >= String.length str
  then
    let k (_, str) =
      (*#<If>Logger.debug "HttpTools.get_char_cps: read '%s'" (String.escaped str)#<End>;*)
      str_ref := str; pos_ref := 0;
      get_char_cps read (str_ref, pos_ref, conn) cont2
    in
    read conn k
  else
    cont2 (incr pos_ref; str.[pos])

let upto_mark_stream_cps ?(inclusive=true) buf get from mark cont =
  (*#<If>Logger.debug "upto_mark_stream_cps: mark='%s'" (String.escaped mark)#<End>;*)
  let rec aux b =
    if cmp b mark
    then
      let str =
        if inclusive
        then Buffer.contents b
        else Buffer.sub b 0 ((Buffer.length b) - (String.length mark)) in
      (*#<If>Logger.debug "HttpTools.upto_mark_stream_cps: returning '%s'" (String.escaped str)#<End>;*)
      cont str
    else
      try get from (fun c -> Buffer.add_char b c; aux b);
      with End_of_file -> cont (Buffer.contents b)
  in
  Buffer.clear buf;
  aux buf

let upto_stream_cps ?(inclusive=true) buf read (str_ref, pos_ref, conn) mark cont =
  (*#<If>Logger.debug "upto_stream_cps: mark='%s'" (String.escaped mark)#<End>;*)
  (*#<If>Logger.debug "upto_stream_cps: pos=%d str='%s'" (!pos_ref) (String.escaped (!str_ref))#<End>;*)
  try
    upto_mark_stream_cps ~inclusive buf (get_char_cps read) (str_ref, pos_ref, conn) mark cont
  with exn ->
    (*#<If>Logger.debug "upto_stream_cps: exn='%s'" (Printexc.to_string exn)#<End>;*)
    raise exn

let read_upto_stream_cps ?(inclusive=true) buf (str_ref, pos_ref, conn) mark sched ?err_cont ?timeout cont =
  let read = Scheduler.read sched ?err_cont ?timeout in
  upto_stream_cps ~inclusive buf read (str_ref, pos_ref, conn) mark cont

let fixed_stream_cps buf read (str_ref, pos_ref, conn) count cont =
  let rec aux b cnt =
    if cnt >= count
    then
      let str = Buffer.contents b in
      (*#<If>Logger.debug "HttpTools.fixed_stream_cps: returning '%s'" (String.escaped str)#<End>;*)
      cont str
    else
      let strlen = String.length (!str_ref) in
      if !pos_ref >= strlen
      then
        let aux2 (_, s) =
          (*#<If>Logger.debug "HttpTools.fixed_stream_cps: read '%s'" (String.escaped s)#<End>;*)
          let len = String.length s in
          if cnt + len < count
          then (Buffer.add_string b s;
                str_ref := ""; pos_ref := 0;
                aux b (cnt + len))
          else (Buffer.add_string b (String.unsafe_sub s 0 (count - cnt));
                str_ref := String.unsafe_sub s (count - cnt) (len - (count - cnt));
                pos_ref := 0;
                aux b count)
        in
        (try read conn aux2
         with End_of_file ->
           if Buffer.length b > 0
           then
             let str = Buffer.contents b in
             (*#<If>Logger.debug "HttpTools.fixed_stream_cps: returning '%s'" (String.escaped str)#<End>;*)
             cont str
           else raise End_of_file)
      else
        let len = strlen - (!pos_ref) in
        if cnt + len < count
        then (Buffer.add_string b (String.unsafe_sub (!str_ref) (!pos_ref) len);
              str_ref := ""; pos_ref := 0;
              aux b (cnt + len))
        else (Buffer.add_string b (String.unsafe_sub (!str_ref) (!pos_ref) (count - cnt));
              pos_ref := (!pos_ref) + (count - cnt);
              aux b count)
  in
  Buffer.clear buf;
  aux buf 0

let read_fixed_stream_cps buf (str_ref, pos_ref, conn) count sched ?err_cont ?timeout cont =
  let read = Scheduler.read sched ?err_cont ?timeout in
  fixed_stream_cps buf read (str_ref, pos_ref, conn) count cont

let buf_clean (b,pos) =
  #<If$minlevel 10>Logger.debug "buf_clean: blen=%d pos=%d" (Buffer.length b) (!pos)#<End>;
  let blen = Buffer.length b in
  if !pos >= blen
  then (#<If$minlevel 2>Logger.debug "buf_clean: clear"#<End>; Buffer.clear b; pos := 0)
  else
    let tq x = (x lsr 1) + (x lsr 2) in
    if blen >= 1024 && !pos >= (tq blen)
    then
      let str = Buffer.sub b !pos (blen-(!pos)) in
      Buffer.clear b;
      Buffer.add_string b str;
      #<If$minlevel 2>Logger.debug "buf_clean: remove %d shift %d" (blen - Buffer.length b) (String.length str)#<End>;
      pos := 0

let cmp2 b pos mark =
  let mlen = String.length mark in
  if pos < mlen - 1
  then false
  else
    let p = ref pos in
    let m = ref (mlen - 1) in
    while !m >= 0 && Buffer.nth b (!p) = String.unsafe_get mark (!m) do decr p; decr m done;
    !m < 0

exception CallbackAbort

(* We can't optionalize payload because ocaml_parser can't handle optional types. *)
let get_callback ?callback payload ?(blocksize=4096) ?err_cont start pos buf () =
  let cb_start = ref (!pos) in
  match callback with
  | Some cb ->
      (fun () ->
         if !pos - !cb_start > blocksize
         then (cb_start := !pos;
               if cb payload (!pos - start) buf
               then true
               else ((match err_cont with
                      | Some err_fn -> err_fn CallbackAbort
                      | None -> raise CallbackAbort);
                     false))
         else true)
  | None ->
      (fun () -> true)

let upto_mark_stream_cps2 ?(inclusive=true) sched conn (buf,pos) mark
                          ?callback payload ?blocksize ?err_cont ?timeout cont =
  #<If>Logger.debug "upto_mark_stream_cps2(%d): mark='%s'" conn.Scheduler.conn_id (String.escaped mark)#<End>;
  buf_clean (buf,pos);
  let start = (!pos) in
  let mlen = String.length mark in
  let ch = String.unsafe_get mark (mlen - 1) in
  #<If$minlevel 2>Logger.debug "upto_mark_stream_cps2: start=%d buflen=%d" start (Buffer.length buf)#<End>;
  let call_callback = get_callback ?callback payload ?blocksize ?err_cont start pos buf () in
  let rec aux () =
    (*#<If$minlevel 10>Logger.debug "upto_mark_stream_cps2: pos=%d buflen=%d" (!pos) (Buffer.length buf)#<End>;*)
    if (!pos) >= Buffer.length buf
    then
      try
        Scheduler.read_more2 ?err_cont ?timeout sched conn buf
          (fun (n,_buf) ->
             (*#<If$minlevel 10>match oc_opt with
             | Some oc -> (output_string oc (Buffer.sub _buf !pos n); Pervasives.flush oc)
             | None -> ()#<End>;*)
             #<If$minlevel 2>
             Logger.debug "upto_mark_stream_cps2(%d): read %d" conn.Scheduler.conn_id n;
             let s = Buffer.sub _buf !pos n in
             Logger.debug "upto_mark_stream_cps2: buf='%s'..'%s'"
                     (String.escaped (String.limit 128 s))
                     (if String.length s < 128 then "" else (String.escaped (String.sub s (String.length s - 128) 128)))
             #<End>;
             if n <= 0 then raise End_of_file else aux ())
      with
      | End_of_file -> (Logger.debug "upto_mark_stream_cps2(%d): End_of_file Remaining='%s'"
                                     conn.Scheduler.conn_id (String.limit 128 (Buffer.sub buf start ((!pos)-start)));
                        cont (buf,start,(!pos)-start))
      | exn -> (Logger.debug "upto_mark_stream_cps2(%d): exn=%s Remaining='%s'"
                             conn.Scheduler.conn_id (Printexc.to_string exn)
                             (String.limit 128 (Buffer.sub buf start ((!pos)-start)));
                raise exn)
    else
      (let blen = Buffer.length buf in
       (*let posstart = !pos in*)
       while (!pos) < blen && Buffer.nth buf (!pos) <> ch do incr pos done;
       if call_callback ()
       then
         (*Logger.debug "skipped %d" (!pos - posstart);*)
         if (!pos) >= blen
         then aux ()
         else
           (if cmp2 buf (!pos) mark
            then
              (incr pos;
               let res =
                 if inclusive
                 then (buf,start,(!pos)-start)
                 else (buf,start,(!pos)-start-(String.length mark)) in
               #<If$minlevel 2>Logger.debug "HttpTools.upto_mark_stream_cps2(%d): returning pos=%d '%s'"
                                            conn.Scheduler.conn_id
                 (!pos) (String.escaped (String.limit 80 (Buffer.sub buf start ((!pos)-start))))#<End>;
               cont res)
            else (incr pos; aux ()))
       else ())
  in
  aux ()

let upto_mark_stream_cps3 ?inclusive sched conn (buf,pos) mark ?callback payload ?blocksize ?err_cont ?timeout cont =
  upto_mark_stream_cps2 ?inclusive sched conn (buf,pos) mark ?callback payload ?blocksize ?err_cont ?timeout
    (fun (buf,start,len) -> cont (Buffer.sub buf start len))

let fixed_stream_cps2 sched conn (buf,pos) count ?callback payload ?blocksize ?err_cont ?timeout cont =
  let conn_id = conn.Scheduler.conn_id in
  buf_clean (buf,pos);
  let start = !pos in
  let call_callback = get_callback ?callback payload ?blocksize ?err_cont start pos buf () in
  pos := Buffer.length buf;
  let rec aux () =
    #<If$minlevel 2>Logger.debug "fixed_stream_cps2: conn_id:%d pos=%d start=%d count=%d buflen=%d"
                            conn_id !pos start count (Buffer.length buf)#<End>;
    if call_callback ()
    then begin
      if !pos - start >= count
      then
        (if !pos - start > count then pos := start + count;
         (*#<If>Logger.debug "HttpTools.fixed_stream_cps2: returning pos=%d '%s'"
                        !pos (String.escaped (Buffer.sub buf start count))#<End>;*)
         cont (buf,start,count))
      else
        let err_cont = Option.default (fun exn -> Logger.debug "fixed_stream_cps2(A): conn_id=%d exn=%s"
                                                          conn_id (Printexc.to_string exn)) err_cont in
        (#<If$minlevel 2>Logger.debug "fixed_stream_cps2(read_more2): pos=%d buflen=%d" !pos (Buffer.length buf)#<End>;
          try Scheduler.read_more2 ?timeout sched conn buf
                                  ~err_cont:(function
                                             | End_of_file -> (Logger.debug "fixed_stream_cps2: got End_of_file";
                                                               cont (buf,start,(!pos)-start))
                                             | exn -> (Logger.debug "fixed_stream_cps2(B): conn_id=%d exn=%s"
                                                               conn_id (Printexc.to_string exn);
                                                       err_cont exn))
                                  (fun (n,_buf) ->
                                    (*#<If$minlevel 10>match oc_opt with
                                    | Some oc -> (output_string oc (Buffer.sub buf !pos n); Pervasives.flush oc)
                                    | None -> ()#<End>;*)
                                     #<If$minlevel 2>Logger.debug "fixed_stream_cps2: conn_id=%d  read %d" conn_id n;
                                     (*Logger.debug "fixed_stream_cps2: buf='%s'" (String.escaped (Buffer.contents buf))*)
                                     #<End>;
                                     if n <= 0
                                     then (Logger.debug "fixed_stream_cps2: raising End_of_file"; raise End_of_file)
                                     else (pos := !pos + n; aux ()))
         with exn -> (Logger.debug "fixed_stream_cps2(C): conn_id=%d exn=%s"
                              conn_id (Printexc.to_string exn); cont (buf,start,(!pos)-start)))
    end
    else ()
  in
  aux ()

let fixed_stream_cps3 sched conn (buf,pos) count ?callback payload ?blocksize ?err_cont ?timeout cont =
  fixed_stream_cps2 sched conn (buf,pos) count ?callback payload ?blocksize ?err_cont ?timeout
    (fun (buf,start,len) -> cont (Buffer.sub buf start len))

let putback2 str (b, p) =
  #<If$minlevel 1>Logger.debug "HttpTools.putback2 %d to (buflen=%d,pos=%d)" (String.length str) (Buffer.length b) (!p)#<End>;
  (*#<If$minlevel 2>Logger.debug "HttpTools.putback2 '%s' to '%s'" (String.escaped str) (String.escaped (!s))#<End>;*)
  buf_clean (b, p);
  let blen = Buffer.length b in
  if !p = 0 && blen = 0
  then Buffer.add_string b str
  else
    (* TODO: this is very inefficient, we need to hack into Buffer and
     * write a Buffer.blit_in or Buffer.prepend
     *)
    let bufstr = Buffer.sub b !p (blen - !p) in
    Buffer.clear b;
    Buffer.add_string b str;
    Buffer.add_string b bufstr;
    p := 0

let buflst = ref ([]:Buffer.t list)
let bufcnt = ref 0

let collect_bufs needed =
  let target = !bufcnt lsr 1 in
  #<If$minlevel 10>Logger.debug "collect_bufs: needed=%d target=%d bufcnt=%d" needed target !bufcnt#<End>;
  if target >= 2 && needed <= target
  then
    let rec aux () =
      if !bufcnt > target
      then (Buffer.reset (List.hd (!buflst));
            buflst := List.tl (!buflst);
            decr bufcnt;
            aux ())
      else
        #<If$minlevel 2>Logger.debug "collect_bufs: reduced to %d" !bufcnt#<End>
    in
    aux ()

let get_buf ?(hint=4096) () =
  match !buflst with
  | [] -> (#<If$minlevel 2>Logger.debug "get_buf(%d): new" !bufcnt#<End>; Buffer.create hint)
  | b::t -> (#<If$minlevel 2>Logger.debug "get_buf(%d): old" !bufcnt#<End>; buflst := t; decr bufcnt; Buffer.clear b; b)

let free_buf b =
  if Buffer.length b <= (10*1024*1024)
  then (#<If$minlevel 2>Logger.debug "free_buf(%d): return" !bufcnt#<End>; buflst := b::(!buflst); incr bufcnt)
  else (#<If$minlevel 2>Logger.debug "free_buf(%d): reset" !bufcnt#<End>; Buffer.reset b)

let upto mark read conn cont = upto_mark_stream_cps (Buffer.create 1024) (get_char_cps read) conn mark cont

let putback str (s, p, _) =
  (*#<If>Logger.debug "HttpTools.putback '%s' to '%s'" (String.escaped str) (String.escaped (!s))#<End>;*)
  if !p = 0
  then s := str^(!s)
  else
    if !p >= String.length (!s)
    then (s := str; p := 0)
    else
      let strlen = String.length str in
      if !p >= strlen
      then (String.unsafe_blit str 0 (!s) ((!p)-strlen) strlen; p := (!p) - strlen)
      else (String.unsafe_blit str (strlen - (!p)) (!s) 0 (!p);
            s := (String.unsafe_sub str 0 (strlen-(!p)))^(!s);
            p := 0)

let skip_ is_ s l n = let rec sl p = if p >= l then l else if is_ (ug s p) then sl (p+1) else p in sl n
let skip_sptab = skip_ Charf.is_sptabf
let skip_lws = skip_ Charf.is_spacef

(* field value can span multiple lines with newline + space *)
let upto_mark_lws_ ug mark mlen str strlen n =
  let pos = skip_sptab str strlen n in
  let pos0 = pos in
  match pos_mark ug mark mlen str strlen pos with
  | Some (pos_, pos) ->
      let len0 = pos_ - pos0 in
      let str0 = String.sub str pos0 len0 in
      if pos >= strlen
      then (strlen,len0,str0)
      else if ug str pos <> ' ' && ug str pos <> '\t'
      then (pos,len0,str0)
      else
        let rec uhc str1 len1 pos =
          let pos = skip_sptab str strlen pos in
          let pos0 = pos in
          match pos_mark ug mark mlen str strlen pos with
          | Some (pos_, pos) ->
              let l = pos_ - pos0 in
              let len1 = len1 + l + 1 in
              let str1 = (String.sub str pos0 l)::str1 in
              if pos >= strlen
              then (strlen,len1,String.rev_sconcat " " str1)
              else if ug str pos <> ' ' && ug str pos <> '\t'
              then (pos,len1,String.rev_sconcat " " str1)
              else uhc str1 len1 pos
          | None ->
              let l = strlen - pos0 in
              let len1 = len1 + l + 1 in
              let str1 = (String.sub str pos0 l)::str1 in
              (strlen,len1,String.rev_sconcat " " str1)
        in
        uhc [str0] len0 pos
  | None ->
      (strlen, strlen - n, us str n (strlen-n))

let upto_mark_lws = upto_mark_lws_ ug
let upto_mark_lws_ci = upto_mark_lws_ (fun str n -> Char.lowercase (ug str n))

(* Had to move this in here because of dependencies. *)

let content_compress sched gzip deflate compression_level cache_response content content_len cont =
    match content with
    | Rcontent.ContentString str ->
        Compression.compress_content sched gzip deflate compression_level cache_response str content_len
          (function (compressed, str) -> cont (compressed, Rcontent.ContentString str))
    | Rcontent.ContentBuffer buf ->
        Compression.compress_content sched gzip deflate compression_level cache_response
          (Buffer.contents buf) content_len
          (function (compressed, str) -> cont (compressed, Rcontent.ContentString str))
    | Rcontent.ContentFBuffer buf ->
        Compression.compress_content sched gzip deflate compression_level cache_response
          (FBuffer.contents buf) content_len
          (function compressed, str -> cont (compressed, Rcontent.ContentString str))
    | Rcontent.ContentFile (file,ic_opt,oc_opt,fstat_opt,unlinkable) ->
        Compression.compress_file sched gzip deflate compression_level cache_response file fstat_opt content_len
          (function compressed, file, fstat_opt -> cont (compressed, Rcontent.ContentFile (file,ic_opt,oc_opt,fstat_opt,unlinkable)))
    | Rcontent.ContentNone ->
        cont (false, Rcontent.ContentNone)

let make_ssl_cert ssl_cert ssl_key ssl_pass =
  if ssl_cert <> "" then
    if ssl_key <> "" then
      Some (SslAS.make_ssl_certificate ssl_cert ssl_key ssl_pass)
    else begin
      Logger.log "Error : ssl-cert option MUST be used with ssl-key option";
      exit 1
    end
  else
    None

let make_ssl_verify ssl_ca_file ssl_ca_path ssl_client_cert_path ssl_client_ca_file ssl_accept_fun ssl_always =
  if ssl_ca_file <> "" || ssl_ca_path <> "" || ssl_client_cert_path <> "" then
    Some (SslAS.make_ssl_verify_params ~client_ca_file:ssl_client_ca_file
      ~accept_fun:ssl_accept_fun ~always:ssl_always
      ssl_ca_file ssl_ca_path ssl_client_cert_path)
  else
    None
