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
(* CF mli *)

(* depends *)
module Char = BaseChar

include String

let compare_int (a:int) b = Pervasives.compare a b

(* {6 Allocation (returns a string)} *)

let insert s pos text =
  let ls = length s in
  if pos > ls then invalid_arg "String.insert" else
    let lt = length text in
    let r = create (ls + lt) in
    unsafe_blit s 0 r 0 pos ;
    unsafe_blit text 0 r pos lt ;
    (* behavior of unsafe_blit is unspecified for negative length *)
    ( if pos < ls then
        unsafe_blit s pos r (pos + lt) (ls - pos) ) ;
    r

let unsafe_sub s p l =
  let r = create l in
  unsafe_blit s p r 0 l ;
  r

let init n f =
  if n >= 0
  then
    let s = create n in
    for i = 0 to pred n do
      unsafe_set s i (f i)
    done ;
    s
  else
    let n = (- n) in
    let s = create n in
    for i = pred n downto 0 do
      unsafe_set s i (f (n-i-1))
    done ;
    s

let fold_init n f acc =
  if n >= 0
  then
    let s = create n in
    let rec aux acc i =
      if i = n then s
      else (
        let c, acc = f acc in
        unsafe_set s i c ;
        aux acc (succ i)
      )
    in aux acc 0
  else
    let n = (- n) in
    let s = create n in
    let rec aux acc i =
      if i < 0 then s
      else (
        let c, acc = f acc in
        unsafe_set s i c ;
        aux acc (pred i)
      )
    in aux acc (pred n)

let of_chars l =
  let s = create (List.length l) in
  let _ =
    (* do not use iteri because it is not in OCaml stdlib
       and we want to be as free as possible regarding dependancy
       between Base.List and Base.String *)
    let p = ref 0 in
    List.iter
      (fun c ->
         unsafe_set s !p c ;
         incr p) l
  in
  s

let map f s =
  let l = length s in
  init l (fun i -> f (unsafe_get s i))
    (* or inline ? *)
    (*     let s' = create l in *)
    (*     for i = 0 to pred l do *)
    (*       unsafe_set s' i (f (unsafe_get s i)) *)
    (*     done ; *)
    (*     s' *)

(* replace is implemented after is_substring *)

let citation sep len s =
  let len_s = length s in
  if len_s <= len then s else
    let len_sep = length sep in
    let left = ( len_s - len ) / 2 in
    let len' = left + left + len_sep in
    let s' = create len' in
    unsafe_blit s 0 s' 0 left;
    unsafe_blit sep 0 s' left len_sep;
    unsafe_blit s (len_s - left) s' (left + len_sep) left;
    s'

let random n =
  init n (fun _ -> char_of_int (Random.int 26 + 97))

(* TODO: check and use unsafe_sub *)
let ltrim ?(is_space=Char.is_space) s =
  let l = length s in
  let rec aux p =
    if p = l then ""
    else
      let c = unsafe_get s p in
      if is_space c then aux (succ p)
      else String.sub s p (l - p)
  in
  aux 0

(* TODO: check and use unsafe_sub *)
let rtrim ?(is_space=Char.is_space) s =
  let l = length s in
  let rec aux p =
    if p < 0 then ""
    else
      let c = unsafe_get s p in
      if is_space c then aux (pred p)
      else String.sub s 0 (succ p)
  in
  aux (pred l)

let trim ?(is_space=Char.is_space) s =
  (* do not combine ltrim + rtrim (double allocation) *)
  let l = length s in
  if l = 0 then s else
  let left =
    let rec aux p =
      if p = l then l
      else
        let c = unsafe_get s p in
        if is_space c then aux (succ p)
        else p
    in aux 0 in
  let right =
    let rec aux p =
      if p < 0 then 0
      else
        let c = unsafe_get s p in
        if is_space c then aux (pred p)
        else p
    in aux (pred l) in
  let len = right - left + 1 in
  if len <= 0 then "" else unsafe_sub s left len

(* "s" -> s *)
let strip_quotes s =
  let l = length s in
  if l >= 2 && unsafe_get s 0 = '"' && unsafe_get s (pred l) = '"' then sub s 1 (l - 2)
  else s

let repeat s n =
  if n > 0 then
    let l = length s in
    let len = n * l in
    init len (fun i -> unsafe_get s (i mod l))
  else ""

(* TODO:
   1) check and use unsafe_sub
   2) see utilisation, and maybe optimized return s if l = len
*)
(* this is better of using sub because sub does the check,
   and the error message does not tell we are in a left *)
let left s l =
  if l = 0 then ""
  else
    let len = length s in
    if l > 0 then
      if l > len then invalid_arg "String.left" else
        sub s 0 l
    else (* l =< 0 *)
      let len' = l + len in
      if len' < 0 then invalid_arg "String.left" else
        sub s 0 len'

let right s l =
  if l = 0 then ""
  else
    let len = length s in
    if l > 0 then
      if l > len then invalid_arg "String.right" else
        sub s (String.length s - l) l
    else (* l =< 0 *)
      let len' = l + len in
      if len' < 0 then invalid_arg "String.right" else
        sub s (-l) len'

(* left_at & right_at are implemented after findi & rev_findi *)

let remove_trail s =
  let l = length s in
  let rec aux p =
    if p = 0 then ""
    else
      let pm = pred p in
      let c = unsafe_get s pm in
      if c = '\n' or c = '\r' then aux pm
      else sub s 0 p
  in aux l

(* remove_prefix is implemeted after is_prefix *)

let complete_left n c s =
  if n < 0 then invalid_arg "String.complete_left" else
    let l = length s in
    if l < n then ( make (n - l) c ) ^ s
    else if l > n then unsafe_sub s (l - n) n
    else s

let complete_right n c s =
  if n < 0 then invalid_arg "String.complete_right" else
    let l = length s in
    if l < n then s ^ make (n - l) c
    else if l > n then unsafe_sub s 0 n
    else s

let complete = complete_right

(* TODO: recode using concat_map applied with %identity *)
let sconcat ?(left="") ?(right="") ?nil sep l =
  match l with
    [] -> Option.default (left^right) nil
  | hd :: tl ->
      let len_left = length left
      and len_right = length right
      and len_hd = length hd
      and len_sep = length sep in
      let num = ref 0 and len = ref (len_left + len_right) in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = create (!len + len_sep * (!num - 1)) in
      unsafe_blit left 0 r 0 len_left ;
      unsafe_blit hd 0 r len_left len_hd ;
      let pos = ref (len_left + len_hd) in
      List.iter
        (fun s ->
           unsafe_blit sep 0 r !pos len_sep ;
           pos := !pos + len_sep ;
           let len_s = length s in
           unsafe_blit s 0 r !pos len_s ;
           pos := !pos + len_s)
        tl ;
      unsafe_blit right 0 r !pos len_right ;
      r

(* next commit, with the optimized version *)
let rev_sconcat ?(left="") ?(right="") ?nil sep l =
  sconcat ~left ~right ?nil sep (List.rev l)

(* TODO: optimized 1-1 (cf doc) *)
let concat_map ?(left="") ?(right="") ?nil sep f l =
  let l = List.rev_map f l in
  match l with
  | [] -> Option.default (left^right) nil
  | hd :: tl ->
      let len_left = length left
      and len_right = length right
      and len_hd = length hd
      and len_sep = length sep in
      let num = ref 0 and len = ref (len_left + len_right) in
      List.iter (fun s -> incr num; len := !len + length s) l ;
      len := !len + len_sep * (!num - 1) ;
      let r = create !len in
      unsafe_blit left 0 r 0 len_left ;
      let pos = ref (!len - len_right) in
      unsafe_blit right 0 r !pos len_right ;
      pos := !pos - len_hd ;
      unsafe_blit hd 0 r !pos len_hd ;
      List.iter
        (fun s ->
           let len_s = length s in
           pos := !pos - len_sep ;
           unsafe_blit sep 0 r !pos len_sep ;
           pos := !pos - len_s ;
           unsafe_blit s 0 r !pos len_s)
        tl ;
      r

(* Same remark, 1-1 *)
let rev_concat_map ?(left="") ?(right="") ?nil sep f l =
  let l = List.rev_map f l in
  match l with
  | [] -> Option.default (left^right) nil
  | hd :: tl ->
      let len_left = length left
      and len_right = length right
      and len_hd = length hd
      and len_sep = length sep in
      let num = ref 0 and len = ref (len_left + len_right) in
      List.iter (fun s -> incr num; len := !len + length s) l ;
      len := !len + len_sep * (!num - 1) ;
      let r = create !len in
      unsafe_blit left 0 r 0 len_left ;
      let pos = ref (!len - len_right) in
      unsafe_blit right 0 r !pos len_right ;
      pos := len_left ;
      unsafe_blit hd 0 r !pos len_hd ;
      pos := !pos + len_hd ;
      List.iter
        (fun s ->
           let len_s = length s in
           unsafe_blit sep 0 r !pos len_sep ;
           pos := !pos + len_sep ;
           unsafe_blit s 0 r !pos len_s ;
           pos := !pos + len_s)
        tl ;
      r

(* {6 Iterators} *)

let fold f acc s =
  let l = length s in
  let rec aux p acc =
    if p = l then acc
    else
      aux (succ p) (f acc (unsafe_get s p))
  in aux 0 acc

let rev_fold f acc s =
  let l = length s in
  let rec aux p acc =
    if p < 0 then acc
    else aux (pred p) (f acc (unsafe_get s p))
  in
  aux (pred l) acc

let exists f s =
  let l = length s in
  let rec aux p =
    if p = l then false
    else
      let c = unsafe_get s p in
      if f c then true else aux (succ p)
  in aux 0

let for_all f s = not (exists (fun c -> not (f c)) s)

(* {6 Condition (returns a bool) } *)

let equal_insensitive s1 s2 =
  let l = length s1 in
  let rec aux i =
    i=l or (Char.equal_insensitive (unsafe_get s1 i) (unsafe_get s2 i) && aux (succ i))
  in
  l = length s2 && aux 0

let compare_insensitive s1 s2 =
  let l1 = length s1
  and l2 = length s2 in
  let rec aux i =
    if i=l1 or i=l2 then compare_int l1 l2
    else
      let c = Char.compare_insensitive (unsafe_get s1 i) (unsafe_get s2 i) in
      if c = 0 then aux (succ i) else c
  in
  aux 0

let is_word = for_all (fun c -> Char.is_alpha c || c = '_')
let is_int = for_all Char.is_digit

let is_float s =
  let l = length s in
  let rec aux dot p =
    if p = l then true
    else
      let c = unsafe_get s p in
      if Char.is_digit c then aux dot (succ p)
      else
        if c = '.' then
          if dot then false
          else aux true (succ p)
        else false
  in
  aux false 0

(* s is a substring of s1 starting at position p1 *)
let is_substring s s1 p1 =
  if p1 < 0 then invalid_arg "String.is_substring" else
    let l = length s and l1 = length s1 in
    let rec aux p =
      p = l or (
        let pp1 = p1 + p in
        pp1 < l1 && unsafe_get s p = unsafe_get s1 pp1 && aux (succ p)
      )
    in
    aux 0

let is_substring_compare compare s s1 p1 =
  let l = length s
  and l1 = length s1 in
  let rec aux p =
    let pp1 = p1 + p in
    p = l or (pp1 < l1 &&
                compare (unsafe_get s p) (unsafe_get s1 pp1) = 0
        && aux (succ p))
  in
  aux 0

let is_substring_insensitive =
  let f a b = Char.compare (Char.lowercase a) (Char.lowercase b) in
  is_substring_compare f

type replace_expr =
  | RepNode of replace_expr CharMap.t
  | RepLeaf of int * string * int
      (* path_len * replacement_text * replacement_len *)

let prepare_replace_data data =
  List.fold_left (
    fun acc (f, t) ->
      let flen = String.length f in
      let tlen = String.length t in
      let rec create_end expr start stop =
        if start = stop then expr
        else
          let expr = RepNode (CharMap.singleton f.[stop] expr) in
          create_end expr start (pred stop)
      in
      let rec add map i =
        if i < flen then
          let next = f.[i] in
          match CharMap.find_opt next map with
          | Some (RepLeaf _) -> invalid_arg "String.multi_replace"
              (* no overlapping replace *)
          | Some (RepNode node) ->
              CharMap.add next (RepNode (add node (succ i))) map
          | None ->
              CharMap.add next (create_end (RepLeaf (flen, t, tlen)) i (pred flen)) map
        else map
      in add acc 0
  ) CharMap.empty data

(* Replacement algorithm based on a tree (generalisation of old http_encode) *)
let multi_replace s data =
  let data = prepare_replace_data data in
  let len = String.length s in
  let b = Buffer.create (4*len) in
  let rec aux i j =
    if i < len then
      let next_char = s.[i] in
      let default () = Buffer.add_char b next_char; aux (i+1) (j+1) in
      match CharMap.find_opt next_char data with
      | Some (RepLeaf (di,t,dj)) ->
          Buffer.add_string b t; aux (i+di) (j+dj)
      | Some (RepNode e) ->
          let rec sub_aux map p =
            if p < len then
              let sub_char = s.[p] in
              match CharMap.find_opt sub_char map with
              | Some (RepLeaf (di,t,dj)) ->
                  Buffer.add_string b t;
                  aux (i+di) (j+dj)
              | Some (RepNode e) -> sub_aux e (succ p)
              | None -> default ()
            else default ()
          in sub_aux e (succ i)
      | None -> default ()
    else j
  in Buffer.sub b 0 (aux 0 0)

let replace s pat repl = multi_replace s [pat, repl]

(* s contains p at any char. Naif implementation (KMP?) *)
(* less tests as is_contained_from_until p s 0 (length s) *)
let is_contained_from p s ofs =
  let ofs = max 0 ofs in
  let ls = length s and lp = length p in
  let rec aux i =
    if i < ofs then None else
      if is_substring p s i then Some i else aux (pred i)
  in aux (ls - lp)

let is_contained p s = (is_contained_from p s 0) <> None

let is_contained_from_until p s start stop =
  let start = max 0 start in
  let ls = length s and lp = length p in
  if stop - start < lp then None
  else
    let rec aux i =
      if i < start then None else
        if is_substring p s i then Some i else aux (pred i)
    in aux ((min ls stop) - lp)

let is_prefix pre s = is_substring pre s 0
let is_suffix suf s =
  let ofs = length s - length suf in
  if ofs < 0 then false else
    is_substring suf s ofs

(* remove_prefix is implemented out of its classification
   because its implementation uses is_prefix *)
(* example: remove_prefix "foo " "foo bar" gives "bar"
   raises Not_found if the prefix is not good *)
let remove_prefix pre s =
  if String.length pre = 0 then s
  else
    if is_prefix pre s
    then right s (- (length pre))
    else raise Not_found
let remove_prefix_if_possible pre s =
  if String.length pre = 0 then s
  else
    if is_prefix pre s
    then right s (- (length pre))
    else s

let remove_suffix suf s =
  if is_suffix suf s
  then left s (- (length suf))
  else raise Not_found
let remove_suffix_if_possible suf s =
  if is_suffix suf s
  then left s (- (length suf))
  else s

let is_escaped s index =
  let rec rindex_inv c s i = if i >= 0 && s.[i] = c then rindex_inv c s (i-1) else i in
  ((index-1) - rindex_inv '\\' s (index-1)) mod 2 = 1

(* {6 Search} *)

let find_opt f s =
  let l = length s in
  let rec aux p =
    if p = l then None
    else
      let c = unsafe_get s p in
      if f c then Some c else aux (succ p)
  in aux 0

let findif f s =
  let l = length s in
  let rec aux i =
    if i = l then None
    else
      let c = unsafe_get s i in
      if f c then Some i else aux (succ i)
  in aux 0

let findi (c : char) = findif ((=) c)

let rev_findif f s =
  let l = length s in
  let rec aux i =
    if i < 0 then None
    else
      let c = unsafe_get s i in
      if f c then Some i else aux (pred i)
  in aux (pred l)

let rev_findi (c : char) = rev_findif ((=) c)

(* left_at & right_at are there because of findi *)
(* TODO: check and use unsafe_sub *)
let left_at s c =
  match findi c s with
  | None -> s
  | Some i -> sub s 0 i

let right_at s c =
  match rev_findi c s with
  | None -> s
  | Some i ->
      let i = i+1 in
      sub s i ((length s) - i)

(* {6 Decons} *)

let tail ?(lines=10) s =
  if lines = 0 then "" else begin
    let l = length s in
    let lines = ref lines in
    let pos = ref (pred l) in
    while !pos >= 0 && !lines > 0 do
      if unsafe_get s !pos = '\n' then
        decr lines;
      decr pos
    done;
    incr pos;
    if unsafe_get s !pos = '\n' then incr pos;
    sub s !pos (l - !pos)
  end


let last_char s =
  let len = length s in
  if len = 0 then invalid_arg "String.last_char" else
    unsafe_get s (pred len)

let len_from func str index =
  let len = length str in
  let max = len - index in
  let rec aux cur =
    if cur < max && func str.[index + cur] then aux (succ cur)
    else cur
  in
  aux 0

let hash = Hashtbl.hash

let char_list_of_string s =
  let rec aux acc i =
    if (i < 0) then acc
    else aux ((unsafe_get s i)::acc) (pred i)
  in aux [] (pred (length s))

let rev_char_list_of_string s =
  let len = length s in
  let rec aux acc i =
    if (i >= len) then acc
    else aux ((unsafe_get s i)::acc) (succ i)
  in aux [] 0

(* =========================================================================== *)
(*
  Splitting, dicing & slicing
*)

let slice cut str =
  let rec aux pos =
    try
      let i = String.index_from str pos cut in
      if i==pos then aux (succ pos)
      else unsafe_sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
      let l = String.length str in
      if l==pos then []
      else [ unsafe_sub str pos (l - pos) ]
  in
  aux 0

let slice_chars cut str =
  let rec index_from pos = if String.contains cut str.[pos] then pos else index_from (succ pos) in
  let rec aux pos =
    try
      let i = index_from pos in
      if i==pos then aux (succ pos)
      else unsafe_sub str pos (i - pos) :: aux (succ i)
    with Invalid_argument _ ->
      let l = String.length str in
      if l==pos then []
      else [ unsafe_sub str pos (l - pos) ]
  in
  aux 0

let split_char c s =
  try
    let i = String.index s c in
    unsafe_sub s 0 i, unsafe_sub s (i + 1) (String.length s - i - 1)
  with Not_found -> s, ""

let split_char_last c s =
  try
    let i = String.rindex s c in
    unsafe_sub s 0 i, unsafe_sub s (i + 1) (String.length s - i - 1)
  with Not_found -> s, ""

(* =========================================================================== *)

(* largeur approximative de la chaîne en fonte proportionnelle *)
let width = fold (fun acc x -> acc + Char.width x) 0

(* limite une chaîne à lim caractères et ajoute des '...' si nécessaire *)
let limit lim s =
  if length s > lim then
    let r = sub s 0 lim in
    if lim >= 10 then (
      unsafe_set r (lim - 3) '.' ;
      unsafe_set r (lim - 2) '.' ;
      unsafe_set r (lim - 1) '.'
    ) ;
    r
  else s

let limit_width lim s =
  let l = length s in
  let rec aux sum pos =
    if pos = l or sum >= lim then pos
    else
      let w = Char.width (unsafe_get s pos) in
      aux (sum + w) (succ pos)
  in
  let last = aux 0 0 in
  if last = l then s
  else String.sub s 0 last ^ "..."

let name_of_int n =
  assert (n>0) ;
  let rec aux n =
    let q = n / 26
    and r = n mod 26 in
    if q=0 then make 1 (Char.chr (97+r))
    else (aux (pred q)) ^ (make 1 (Char.chr (97+r)))
  in aux (pred n)

let name_of_int_upper n =
  assert (n>0) ;
  let rec aux n =
    let q = n / 26
    and r = n mod 26 in
    if q=0 then make 1 (Char.chr (65+r))
    else (aux (pred q)) ^ (make 1 (Char.chr (65+r)))
  in aux (pred n)

let remove_first_char x = sub x 1 (pred (length x))

(* example of use: replace_chars "éèà" "eea"  "névé" gives "neve" *)
let remove_accents s =
  let sa = "ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÇçÌÍÎÏìíîïÙÚÛÜùúûüÿÑñ"
  and sb = "AAAAAAaaaaaaOOOOOOooooooEEEEeeeeCcIIIIiiiiUUUUuuuuyNn" in
  (* nb: "sa" is twice as long as "sb" since this is utf8. Code below relies on this! *)
  let len = String.length sb in
  let rec aux i res =
    if i >= len then res
    else
      let from = String.sub sa (2*i) 2 in
      let sto = String.sub sb i 1 in
      aux (succ i) (replace res from sto)
  in
  aux 0 s


(** Optimized deleting of trailing whitespace, like emacs M-x delete-trailing-whitespace do *)
let delete_trailing_whitespace s =
  let aux' i =
    let rec aux j =
      if j < i then i - 1
      else
        if Char.is_space (unsafe_get s j) then aux (pred j)
        else j
    in aux
  in
  let rec aux len lines i = match try Some (String.index_from s i '\n') with Not_found -> None with
    | Some j ->
        let j' = aux' i (pred j) in
        let l' = j'-i+1 in
        aux (len+l'+1) ((len, i, l')::lines) (succ j)
    | None ->
        let j' = aux' i (pred (String.length s)) in
        let l' = j'-i+1 in
        (len+l'), (len, i, l'), lines
  in
  let len, (last_line_ir, last_line_is, last_line_l), lines = aux 0 [] 0 in
  let r = String.create len in
  unsafe_blit s last_line_is r last_line_ir last_line_l;
  List.iter (fun (ir, is, l) -> unsafe_blit s is r ir l; unsafe_set r (ir+l) '\n') lines;
  r

let to_hex s =
  let hex_char i = Char.chr (if i <= 9 then i + 48 else i + 87) in
  init (String.length s lsl 1) (
    fun i ->
      hex_char (
        let c = Char.code (String.unsafe_get s (i lsr 1)) in
        if i land 1 = 1 then c land 15
        else  c lsr 4
      )
  )

let from_hex s =
  let l = String.length s in
  if l land 1 = 1 then invalid_arg "String.from_hex" ;
  init (l lsr 1) (
    fun i ->
      let i = i lsl 1 in
      Char.chr (
        (Char.hexa_value (String.unsafe_get s i) lsl 4) +
          (Char.hexa_value (String.unsafe_get s (i+1)))
      )
  )

(* {6 Conversion and escaping} *)

let base64encode s =
  let cA,ca,c0 = int_of_char 'A', int_of_char 'a', int_of_char '0' in
  let code i =
    if i < 26 then char_of_int (cA + i)
    else if i < 52 then char_of_int (ca + i - 26)
    else if i < 62 then char_of_int (c0 + i - 52)
    else if i = 62 then '+'
    else if i = 63 then '/'
    else assert false
  in
  let len = String.length s in
  let buf = String.create (4 * ((len + 2) / 3)) in
  let rec aux i =
    let rem = len - i in
    if rem <= 0 then () else
      let c1 = int_of_char (unsafe_get s i)
      and c2 = if rem < 2 then 0 else int_of_char (unsafe_get s (i+1))
      and c3 = if rem < 3 then 0 else int_of_char (unsafe_get s (i+2))
      in
      let e1 = c1 lsr 2
      and e2 = ((c1 land 0x3) lsl 4) lor (c2 lsr 4)
      and e3 = ((c2 land 0xf) lsl 2) lor (c3 lsr 6)
      and e4 = c3 land 0x3f
      in
      let offset = 4 * i / 3 in
      unsafe_set buf offset (code e1);
      unsafe_set buf (offset+1) (code e2);
      unsafe_set buf (offset+2) (if rem < 2 then '=' else code e3);
      unsafe_set buf (offset+3) (if rem < 3 then '=' else code e4);
      aux (i+3)
  in
  aux 0;
  buf

let base64decode s =
  let cA,ca,c0 = int_of_char 'A', int_of_char 'a', int_of_char '0' in
  let decode c =
    if 'A' <= c && c <= 'Z' then int_of_char c - cA
    else if 'a' <= c && c <= 'z' then 26 + int_of_char c - ca
    else if '0' <= c && c <= '9' then 52 + int_of_char c - c0
    else if c = '+' then 62
    else if c = '/' then 63
    else if c = '=' then 0
    else raise (Invalid_argument "base64decode")
  in
  let len = String.length s in
  if len mod 4 <> 0 then raise (Invalid_argument "base64decode");
  let real_len =
    if len = 0 then 0
    else if unsafe_get s (len-2) = '=' then 3 * len / 4 - 2
    else if unsafe_get s (len-1) = '=' then 3 * len / 4 - 1
    else 3 * len / 4
  in
  let buf = String.create real_len in
  let rec aux i =
    if i >= len then () else
      let c1 = decode (unsafe_get s i)
      and c2 = decode (unsafe_get s (i+1))
      and c3 = decode (unsafe_get s (i+2))
      and c4 = decode (unsafe_get s (i+3))
      in
      let e1 = (c1 lsl 2) lor (c2 lsr 4)
      and e2 = ((c2 lsl 4) land 0xf0) lor (c3 lsr 2)
      and e3 = ((c3 lsl 6) land 0xc0) lor c4
      in
      let offset = 3 * i / 4 in
      unsafe_set buf offset (char_of_int e1);
      if offset + 1 < real_len then unsafe_set buf (offset+1) (char_of_int e2);
      if offset + 2 < real_len then unsafe_set buf (offset+2) (char_of_int e3);
      aux (i+4)
  in
  aux 0;
  buf

let escape ~valid_chars ~escape_char s =
  assert (not (valid_chars escape_char)); (* the escape char should be escaped *)
  assert (for_all valid_chars "0123456789abcdef"); (* hexadecimals characters should be valid *)
  if for_all valid_chars s then s else
    (let b = Buffer.create (length s * 3) in (* the string cannot be longer *)
     for i = 0 to length s - 1 do
       let c = s.[i] in
       if valid_chars c then
         Buffer.add_char b c
       else (
         Buffer.add_char b escape_char;
         Buffer.add_string b (Printf.sprintf "%.2x" (Char.code c))
       )
     done;
     Buffer.contents b)

let equal : string -> string -> bool = (=)
