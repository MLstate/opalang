(*-
 * Copyright (c) 2007 Eugene Ossintsev
 * Copyright (c) 2002 Douglas Crockford
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

exception Failure' = Failure
open Stream

let is_unsquashable a b =
  match a, b with
  | '+', '+' -> true
  | '-', '-' -> true
  | _ -> false

let is_alphanum = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '$' | '\\' -> true
  | c when ((Char.code c) > 126) -> true
  | _ -> false

let is_pre_lf = function
  | c when is_alphanum c -> true
  | '}' | ']' | ')' | '+' | '-' | '\"' | '\'' | '.' -> true
  | _ -> false

let is_post_lf = function
  | c when is_alphanum c -> true
  | '{' | '[' | '(' | '+' | '-' -> true
  | _ -> false

let is_space = function
  | '\r' | '\n' -> false
  | c when ((Char.code c) <= (Char.code ' ')) -> true
  | _ -> false

let is_pre_regexp = function
  | '(' | ',' | '='| ':' | '['| '!'| '&' | '|' | '?' | '{' | '}' | ';' | '\n' -> true
  | _ -> false

let minify str =
  let s = of_string str in
  let buff = Buffer.create (String.length str) in
  let prev_ch = ref '\000' in
  let print_ch c =
    if not (is_space c) then prev_ch := c;
    Buffer.add_char buff c
  in
  let rec next_ch () =
    match peek s with
    | Some '/' -> junk s; maybe_comment_or_regexp ()
    | Some ('\'' as c) | Some ('\"' as c) -> junk s; print_ch c; quote c
    | Some '\r' | Some '\n' -> junk s; squeeze_linefeeds ()
    | Some c when is_space c -> junk s; squeeze_spaces ()
    | Some c -> junk s; print_ch c; next_ch ()
    | None -> ()
  and squeeze_spaces () =
    match peek s with
    | Some c when is_space c -> junk s; squeeze_spaces ()
    | Some c ->
        if (is_unsquashable !prev_ch c) || ((is_alphanum !prev_ch) && (is_alphanum c))
        then print_ch ' ';
        next_ch ()
    | None -> ()
  and squeeze_linefeeds () =
    match peek s with
    | Some c when c = '\r' || c = '\n' || (is_space c) -> junk s; squeeze_linefeeds ()
    | Some c -> if (is_pre_lf !prev_ch) && (is_post_lf c) then print_ch '\n'; next_ch ()
    | None -> ()
  and maybe_comment_or_regexp () =
    match peek s with
    | Some '/' -> junk s; line_comment ()
    | Some '*' -> junk s; block_comment ()
    | Some _ when is_pre_regexp !prev_ch -> print_ch '/'; quote '/'
    | _ -> print_ch '/'; next_ch ()
  and line_comment () =
    match peek s with
    | Some '\r' | Some '\n' -> junk s; squeeze_linefeeds ()
    | Some _ -> junk s; line_comment ()
    | None -> ()
  and block_comment () =
    match peek s with
    | Some '*' -> junk s; maybe_block_comment_end ()
    | Some _ -> junk s; block_comment ()
    | None -> failwith "Unterminated comment"
  and maybe_block_comment_end () =
    match peek s with
    | Some '/' -> junk s; squeeze_spaces ()
    | Some '*' -> junk s; maybe_block_comment_end ()
    | _ -> junk s; block_comment ()
  and quote c =
    match peek s with
    | Some '\\' ->
        junk s; print_ch '\\';
        begin match peek s with
        | Some c2 -> junk s; print_ch c2; quote c
        | None -> quote c
        end
    | Some c2 when c2 = c -> junk s; print_ch c; next_ch ()
    | Some '\r' | Some '\n' | None ->
        let str = Buffer.contents buff in
        let i = max 0 ((String.length str)-10) in
          Printf.printf "Minify error: %s\n" (String.sub str i (min (i+100) (String.length str)));
          failwith ("Unterminated " ^ (if c = '/' then "regular expression" else "string"))
    | Some c2 -> junk s; print_ch c2; quote c
  in
    try
      next_ch ();
      Buffer.contents buff
    with _ -> str
