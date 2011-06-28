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
(*
   @author Frederic Ye
**)

(* depends *)
module String = BaseString
module List = BaseList

open Requestdef

(** This module is used for compressing response in an HTTP server.
    For optimisation, it uses a cache for caching what it compressed before,
    using the digest of the content to compress.
    The content is cached only from a certain size *)

(** When we compress a file we do it on disc which means keeping the
    compressed file for the duration of the server because its name
    has to be kept in the cache.  We therefore unlink at exit time.
*)
let compress_temporary_files = ((ref []):string list ref)

let compress_unlink_temporary_files () =
  (*Printf.eprintf "compress_unlink_temporary_files\n%!";*)
  List.iter (fun f -> if File.exists f then ((*Printf.eprintf "Unlinking: %s\n%!" f;*) Unix.unlink f))
            (!compress_temporary_files);
  compress_temporary_files := []

(* Note this doesn't work.  For some reason at_exit
 * functions don't get called when the server exits.
 *)
let compress_exit_unlink_enabled = ref false
let enable_compress_exit_unlink () =
  (*Printf.eprintf "enable_compress_exit_unlink\n%!";*)
  if not !compress_exit_unlink_enabled
  then (compress_exit_unlink_enabled := true;
        at_exit compress_unlink_temporary_files)

module CacheOrder : (Heap.Ordered with type t = float * string) =
struct
  type t = float * string
  let compare a b = Pervasives.compare (fst a) (fst b)
end

module CacheHeap = Heap.Binomial (CacheOrder)

module QuickCache =
struct
  type 'a t =
      { mutable datas : ('a option StringMap.t)
      ; mutable lru : CacheHeap.t
      ; mutable max_size : int }
  let make size =
    { datas = StringMap.empty
    ; lru = CacheHeap.empty()
    ; max_size = size }
  let add k v cache =
    let oldest = CacheHeap.minimum cache.lru in
    let datas, lru = match oldest with
    | Some o when (StringMap.size cache.datas) >= cache.max_size ->
        StringMap.remove (snd o) cache.datas,
        CacheHeap.remove cache.lru
    | _ -> cache.datas, cache.lru in
    cache.datas <- StringMap.add k v datas;
    cache.lru <- CacheHeap.insert lru (Unix.gettimeofday (), k)
  let get k cache = StringMap.find_opt k cache.datas
  let change_max new_max cache =
    let lru, datas =
      if cache.max_size > new_max then
        let rec aux n (heap, datas) =
          if n <= 0 then
            heap, datas
          else
            match CacheHeap.minimum heap with
            | Some (_a, b) ->
                let heap = CacheHeap.remove heap in
                aux (pred n) (heap, StringMap.remove b datas)
            | _ ->
                heap, datas
        in aux (cache.max_size - new_max) (cache.lru, cache.datas)
      else
        cache.lru, cache.datas
    in
    cache.datas <- datas;
    cache.lru <- lru;
    cache.max_size <- new_max
  let print cache =
    Logger.log "CACHE SIZE: %d elements" (StringMap.size cache.datas);
    StringMap.iter (
      fun k _v -> Logger.log "%s" k
    ) cache.datas;
    CacheHeap.display (fun (_a, b) -> b) cache.lru
end

let size_from_to_cache = 100
let cache_size = 500

type server_cache = {
  gzip_response_cache : string QuickCache.t;
  deflate_response_cache : string QuickCache.t
}

let server_cache = {
  gzip_response_cache = QuickCache.make cache_size;
  deflate_response_cache = QuickCache.make cache_size
}

(* FIXME : This is a basic support of gzip and deflate,
   for a more complete implementation we should see :
   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html *)
let is_gzip_deflate = function
  | Some req ->
      begin match RequestHeader.get `Accept_Encoding req.request_header with
      | Some (`string s) ->
          (String.is_contained "gzip" s) && not (String.is_contained "gzip;q=0" s),
          (String.is_contained "deflate" s) && not (String.is_contained "deflate;q=0" s)
      | Some (`value _) -> false, false
      | _ -> false, false
      end
  | _ -> false, false

let max_chunk = ref (512*1024)

let compress_content_cps sched gzip deflate compression_level cache_response content
                         content_len cont =
  if (not gzip && not deflate) || compression_level < 1 || size_from_to_cache > content_len then (
    (* Logger.log "CACHE: DO NOT COMPRESS"; *)
    cont(false, content)
  ) else (** COMPRESS CONTENT **)
    try
      let cache = (* We give priority to deflate *)
        if deflate then ((* Logger.log "DEFLATE"; *) server_cache.deflate_response_cache)
        else if gzip then ((* Logger.log "GZIP"; *) server_cache.gzip_response_cache)
        else raise (Gzip.Error "invalid encoding method")
      in
      let key = Digest.to_hex (Digest.string content) in
      match QuickCache.get key cache with
      | Some (Some v) ->
          (* Logger.log "CACHE: FROM CACHE"; *)
          cont(true, v)
      | Some (None) ->
          (* Logger.log "CACHE: NO NEED TO COMPRESS"; *)
          cont(false, content)
      | _ ->
          let oc = Sgzip.open_out ~level:compression_level ~only_deflate:deflate () in
          let return () =
            Sgzip.close_out oc;
            let c_len, f_res = (String.length oc.Sgzip.out_string), oc.Sgzip.out_string in
            (* Logger.log (sprintf "CACHE: OLD LEN %d : NEW LEN %d" content_len c_len); *)
            if content_len > c_len then (
              if cache_response then
                QuickCache.add key (Some f_res) cache;
              cont(true, f_res)
            ) else (
              if cache_response then
                QuickCache.add key None cache;
              cont(false, content)) in
          let rec aux (pos) =
            if pos < content_len
            then
              let t = min !max_chunk (content_len - pos) in
              Sgzip.output oc content pos t; Scheduler.push sched (fun () ->aux(pos+t))
            else return ()
          in aux 0
    with exn -> (Logger.error "compress_content: got exception %s" (Printexc.to_string exn);
                 cont(false, content))

let compress_content sched gzip deflate compression_level cache_response content content_len =
  let res = ref (false, content) in
  compress_content_cps sched gzip deflate compression_level cache_response content content_len
                       (fun rres -> res := rres);
  !res

let compress_file_cps sched gzip deflate compression_level cache_response file fstat_opt file_len cont =
  if (not gzip && not deflate) || compression_level < 1 || size_from_to_cache > file_len then (
    (*Logger.log "CACHE: DO NOT COMPRESS";*)
    cont (false, file, fstat_opt)
  ) else (** COMPRESS CONTENT **)
    try
      let cache = (* We give priority to deflate *)
        if deflate then ((*Logger.log "DEFLATE";*) server_cache.deflate_response_cache)
        else if gzip then ((*Logger.log "GZIP";*) server_cache.gzip_response_cache)
        else raise (Gzip.Error "invalid encoding method")
      in
      let key = Digest.to_hex (Digest.file file) in
      (match QuickCache.get key cache with
       | Some (Some v) ->
           (*Logger.log "CACHE: FROM CACHE";*)
           cont (true, v, (Some (Unix.stat v)))
       | Some (None) ->
           (*Logger.log "CACHE: NO NEED TO COMPRESS";*)
           cont (false, file, fstat_opt)
       | _ ->
           let ic = open_in_bin file in
           (* TODO: delete these files when server exits!!! *)
           let compfile, oc = Filename.open_temp_file ~mode:[Open_binary] "Gzip_" "_pizG.gz" in
           enable_compress_exit_unlink ();
           compress_temporary_files := compfile::(!compress_temporary_files);
           if deflate
           then
             Zlib.compress ~level:compression_level ~header:false
                           (fun buf ->
                              let cnt = input ic buf 0 (min !max_chunk (String.length buf)) in
                              Scheduler.push sched (fun () -> ());
                              cnt)
                           (fun buf len -> output oc buf 0 len)
           else begin
             let oc = Gzip.open_out_chan ~level:compression_level oc in
             let chunk = !max_chunk in
             let buf = String.create chunk in (* TODO: match with file_len *)
             let len = ref (input ic buf 0 chunk) in
             while !len > 0 do
               Gzip.output oc buf 0 (!len);
               len := input ic buf 0 chunk;
               Scheduler.push sched (fun () -> ())
             done;
             Gzip.flush oc
           end;
           close_in ic;
           close_out oc;
           let c_len = (Unix.stat compfile).Unix.st_size in
           (*Logger.log "CACHE: OLD LEN %d : NEW LEN %d" file_len c_len;*)
           if file_len > c_len then (
             if cache_response then
               QuickCache.add key (Some compfile) cache;
             cont (true, compfile, (Some (Unix.stat compfile)))
           ) else (
             if cache_response then
               QuickCache.add key None cache;
             cont (false, file, fstat_opt)
           ))
       with exn -> (Logger.error "compress_file: got exception %s" (Printexc.to_string exn);
                 cont (false, file, fstat_opt))

let compress_file sched gzip deflate compression_level cache_response file fstat_opt file_len =
  let res = ref (false, file, fstat_opt) in
  compress_file_cps sched gzip deflate compression_level cache_response file fstat_opt file_len
                    (fun rres -> res := rres);
  !res
