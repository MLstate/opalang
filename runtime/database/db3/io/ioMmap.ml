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

(* shorthands *)
module U = Unix
module B = Bigarray
module BA = B.Array1

(* depends *)
module String = BaseString
module List = BaseList


#<Debugvar:DEBUG_DB$flag "io">


let debug fmt =
  #<If> Printf.fprintf stdout ("[35m[IoMmap][0m"^^fmt^^"\n%!")
  #<Else> Printf.ifprintf stderr ("[35m[IoMmap][0m"^^fmt^^"\n%!")
  #<End>

let print fmt = Logger.info ("[IoMmap] "^^fmt)

let error ?(critical=false) fmt =
  if critical then Logger.critical ("[IoMmap] "^^fmt)
  else Logger.critical ("[IoMmap] "^^fmt)


(* -- *)
exception EOF
exception ReadOnly

type bigArray = (char, B.int8_unsigned_elt, B.c_layout) BA.t
type stat =
    { date : float;
      size : int;
    }
type chan =
    { mutable bigarr : bigArray;
      mutable real_size : int;
      mutable fd_inpos : int;
      mutable fd_outpos : int;
      mutable fd : U.file_descr;
      mutable beginning : int;
      mutable statistic : (float * int) list;
      name : string;
      readonly : bool;
    }



let string_of_chan chan =
  let name = #<If:TESTING> "" #<Else> chan.name #<End> in
  let fd : int = #<If:TESTING> 0 #<Else> Obj.magic chan.fd #<End> in
  Printf.sprintf "{ name : %s; fd: %d; size: %d; inpos: %d; outpos: %d; beginning: %d; dim: %d }"
    name fd chan.real_size chan.fd_inpos chan.fd_outpos chan.beginning (BA.dim chan.bigarr)

let print_chan c = print "%s" (string_of_chan c)

let fd_length fd = (U.fstat fd).U.st_size

let dummy_tab =  BA.create B.char B.c_layout 1

let mmap ?(size=(-1)) ?pos w =
  let englobe ~act ~inval ~badfd =
    try act()
    with Sys_error "Invalid argument" -> inval()
    | Sys_error "Bad file descriptor" -> badfd()
  in
  let m size = BA.map_file w.fd B.char B.c_layout (not w.readonly) ~pos:(Option.default Int64.zero (Option.map Int64.of_int pos)) size in

  englobe
    ~act:(fun () -> m size)
    ~inval:(fun () ->
      englobe
        ~act:(fun () -> m 1024)
        ~inval:(fun () -> assert false)
        ~badfd:(fun () -> dummy_tab))
    ~badfd:(fun () -> dummy_tab)

let make ?(readonly=false) ?(create=true) name =
  let options = if readonly then [U.O_RDONLY] else [ U.O_RDWR; U.O_CREAT ] in
  let fd = U.openfile name options File.default_rights in
  let first_size = if create then 0 else fd_length fd in

  let dummy =
    { bigarr = dummy_tab;
      real_size = first_size;
      fd_inpos = 0;
      fd_outpos = 0;
      fd;
      beginning = 0;
      statistic = [];
      name;
      readonly;
    } in

  debug "Open %s : %s file, fd : %d, size : %d" name (if create then "new" else "old") (Obj.magic fd) first_size;
  let tab = if create then mmap dummy ~size:1024 else mmap dummy in

  { dummy with bigarr = tab}

let make_chan_append s = make ~create:false s
let make_chan_create s = make s
let make_chan_readonly s = make s ~create:false ~readonly:true


let interpret_stat w =
  if List.length w.statistic < 4 then
    None
  else
    ( let rec aux moy lst prvt prva =
        match lst with
        | [] -> moy
        | (t,a) :: y ->
            let m = (moy +. ((float_of_int prva) /. (let tt=prvt -. t in if tt < 1. then 1. else tt))) in
            aux m y t a
      in
      let pt,pa = List.hd w.statistic in
      let moy = aux 0. (List.tl w.statistic) pt pa in
      let moyenne = moy /. 4. in
      let moyenne = (int_of_float moyenne) * 10 in
      Some moyenne)


let update_stat w add =
  let add = (U.time (), add) in
  let stat = w.statistic in
  let nl =
    if List.length stat < 4 then add :: stat
    else add :: List.tl (List.rev stat)
  in
  w.statistic <- nl


let check_out w size =
  let dim = BA.dim w.bigarr in
  let curr_pos = w.fd_outpos - w.beginning in

  if curr_pos + size > dim then
    (let need = curr_pos + size in
     let add = max (need / 5) 5020 in
     let should = interpret_stat w in

     debug  "Reload MMap (%d): dim %d, size %d, add %d (beginnig at %d)"
       (Obj.magic w.fd) dim size add w.beginning;
     let add = match should with None -> add | Some x -> max x add in
     w.bigarr <- mmap ~size:(need + add) ~pos:w.beginning w;
     update_stat w (need + add - dim))

let write_list w chars =
  if w.readonly then raise ReadOnly
  else (
  let length = List.length chars in
  let curr_pos = w.fd_outpos - w.beginning in
  check_out w length;
  (try List.iteri (fun c i -> BA.set w.bigarr (i+curr_pos) c) chars
   with Invalid_argument "index out of bounds" ->
     (error ~critical:true "MMapIo: Write error. Want to write %d chars at %d in %s"
         length (curr_pos) (string_of_chan w);
      raise EOF));
  w.fd_outpos <- w.fd_outpos + length;
  if w.fd_outpos > w.real_size then
    w.real_size <- w.fd_outpos
  )


let factorized size elem tochar shift =
  let rec aux n compt acc =
    if compt < size then
      let char = tochar n in
      aux (shift n) (succ compt) (char::acc)
    else List.rev acc
  in aux elem 0 []


let of_int32 i =
  factorized 4 i
    (fun n -> char_of_int (Int32.to_int (Int32.shift_right_logical n 24)))
    (fun n -> Int32.shift_left n 8)

let of_int i = of_int32 (Int32.of_int i)

let add_int32 w i = write_list w (of_int32 i)
let add_int w i = write_list w (of_int i)

let add_int64 w i =
  let int64 =
    factorized 8 i
      (fun n -> char_of_int (Int64.to_int (Int64.shift_right_logical n (64-8))))
      (fun n -> Int64.shift_left n 8) in
  write_list w int64

let add_string w s =
  let size = of_int (String.length s) in
  let string = String.char_list_of_string s in
  write_list w (size @ string)

let add_char w c =
  write_list w [c]

let add_float w f =
  let float =
    factorized 8 (Int64.bits_of_float f)
      (fun n -> char_of_int (Int64.to_int (Int64.shift_right_logical n 56)))
      (fun n -> Int64.shift_left n 8) in
  write_list w float


let output _  = ()


let seek_out fd i =
  fd.fd_outpos <- i

let position_out fd =
  fd.fd_outpos


let check_in w size =
  let dim = BA.dim w.bigarr in
  let to_read = w.fd_inpos - w.beginning + size in
  if not (dim >= to_read) || not (w.real_size >= to_read) then
    if not (dim >= to_read) then
      (error ~critical:true "MmapIo: Read check error (EOB) want to read %d (%d) in %s"
         size to_read (string_of_chan w);
       raise EOF)
    else if not (w.real_size >= to_read) then
      (error "MmapIo: Read check error (EOF) want to read %d (%d) in %s" size to_read (string_of_chan w);
       raise EOF)

let read_char w =
  let char =
    try BA.get w.bigarr w.fd_inpos
    with Invalid_argument "index out of bounds" ->
      (error ~critical:true "MmapIo: Read error. Tryed to read a character from %d(%d) on %s"
          w.fd_inpos (w.fd_inpos - w.beginning) (string_of_chan w);
       raise EOF) in
  w.fd_inpos <- succ w.fd_inpos;
  char

let rfactorize w count zero logor shift ofint =
  check_in w count;
  let curr_pos = w.fd_inpos - w.beginning in
  let r = ref zero in
  let _ = try
    for i = curr_pos to curr_pos + count - 1 do
      r := logor (shift !r 8) (ofint (int_of_char (BA.get w.bigarr i)))
    done
  with Invalid_argument "index out of bounds" ->
    (error ~critical:true "MmapIo: Read error. Tryed to read from %d to %d on %s"
        curr_pos (curr_pos + count -1) (string_of_chan w);
     raise EOF) in
  w.fd_inpos <- w.fd_inpos + count;
  !r

let read_int32 w = rfactorize w 4 Int32.zero Int32.logor Int32.shift_left Int32.of_int

let read_int =
  if Sys.word_size = 64 then fun w ->
    let int32 = rfactorize w 4 0 (lor) (lsl) (fun x -> x) in
    (int32 lsl 31) asr 31 (* propagate the sign bit *)
  else fun w ->
    Int32.to_int (read_int32 w)

let read_int64 w = rfactorize w 8 Int64.zero Int64.logor Int64.shift_left Int64.of_int

let read_float w =
  Int64.float_of_bits (read_int64 w)

let read_string w =
  let size = read_int w in
  check_in w size;
  let s = String.create size in
  let curr_pos = w.fd_inpos - w.beginning in
  let _ = try
    for i = 0 to size - 1 do
      String.unsafe_set s i (BA.get w.bigarr (i+curr_pos))
    done
  with Invalid_argument "index out of bounds" ->
    (error ~critical:true "MmapIo: Read error. Tryed to read %d chars from %d on %s"
       size curr_pos (string_of_chan w);
     raise EOF) in
  w.fd_inpos <- w.fd_inpos + size;
  s

let length fd = fd.real_size

let seek_in fd i =
  fd.fd_inpos <- i

let position_in fd =
  fd.fd_inpos

let close w =
  if not w.readonly then (
  debug "Truncate file %d at %d (%d)" (Obj.magic w.fd) (w.real_size) (BA.dim w.bigarr);
  w.bigarr <- BA.create B.char B.c_layout 0;
  U.ftruncate w.fd w.real_size;
  );
  U.close w.fd

let erase_file w =
  debug "Erase file: %s" (string_of_chan w);
  if not w.readonly then U.ftruncate w.fd 0;
  w.real_size <- 0;
  w.fd_outpos <- 0;
  w.fd_inpos <- 0;
  w.beginning <- 0;
  w.statistic <- [];
  w.bigarr <- mmap w ~size:1024

let reset_file w =
  let bef = string_of_chan w in
  let newb = min w.fd_outpos (length w) in
  let dim = BA.dim w.bigarr in
  w.bigarr <- mmap ~size:(dim + 2048) ~pos:newb w;
  w.beginning <-newb;
  w.statistic <- [];
  debug "Reset file %s -> %s" bef (string_of_chan w)

let truncate_file w s =
  (* we should do a truncate only in recovery case *)
  (* or at the creation of the flag file *)
  (* and this is done only when we are at the beginnig *)
  (* so the field beginning should be at 0 *)
  assert (w.beginning = 0);
  let bef = string_of_chan w in
  w.real_size <- s;
  if w.fd_outpos > s then w.fd_outpos <- s;
  if w.fd_inpos > s then w.fd_inpos <- s;
  w.bigarr <- mmap w ~size:s;
  debug "Truncate files %s -> %s" bef (string_of_chan w)

let reload w =
  let bef = string_of_chan w in
  Unix.close w.fd;
  let newfd = U.openfile w.name [ U.O_RDWR; U.O_CREAT ] File.default_rights in
  w.fd <- newfd;
  let size = (U.fstat newfd).U.st_size in
  w.real_size <- size;
  let bigarr = mmap w ~size in
  w.bigarr <- bigarr;
  w.fd_inpos <- 0;
  w.fd_outpos <- 0;
  w.statistic <- [];
  debug "Reloaded file %s -> %s" bef (string_of_chan w)
