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
(* CF mli *)
type t = Buffer.t
type input = Directive of string | Code of string

let create () = Buffer.create 10000
let reset accu = Buffer.clear accu

let add accu b =
  Buffer.add_string accu b;
  Buffer.add_char accu '\n'

let accumulate accu str =
  let s = Base.String.rtrim str in
  let len = String.length s in
  let predlen = pred len in
  let rec aux i =
    if i >= predlen then (add accu s; None)
    else
      if (String.unsafe_get s i) = ';'
      then
        if String.unsafe_get s (succ i) = ';'
        then
          begin
            let input = Base.String.rtrim (Base.String.unsafe_sub s 0 i) in
            Buffer.add_string accu input;
            let input = Buffer.contents accu in
            reset accu;
            if String.length input > 0
            then
              if input.[0] = '#'
              then Some (Directive input)
              else Some (Code input)
            else None
          end
        else aux (i + 2)
      else aux (succ i)
  in aux 0

let flush accu = accumulate accu ";;"

module Directive =
struct
  type arguments = string list
  type 'env action = 'env -> arguments -> 'env

  type regexp = string
  type argument_number = int

  type 'env directive = regexp * argument_number * 'env action

  type 'env handler = ('env -> string -> 'env option) list

  let empty () = []
  let add handler directive =
    match directive with
    | regexp, argument_number, action ->
        let reg = Str.regexp regexp in
        let filter env dir =
          if Str.string_match reg dir 0
          then
            try
              let args =
                let rec aux accu i =
                  if i < 1 then accu else aux ((Str.matched_group i dir)::accu) (pred i)
                in aux [] argument_number
              in
              Some (action env args)
            with
            | Not_found -> None
          else None
        in
        filter :: handler

  let parse handler env input =
    let rec aux = function
      | [] -> None
      | hd::tl -> (
          match hd env input with
          | ( Some _ ) as result -> result
          | None -> aux tl
        )
    in aux handler

end
