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
include Format

type 'a pprinter = Format.formatter -> 'a -> unit

let pp = Format.fprintf

let pp_fmt fmt = (fun oc -> Format.fprintf oc fmt)

let pp_fst pp fmt (a, _) = pp fmt a
let pp_snd pp fmt (_, b) = pp fmt b

let pp_list sep ?singleton p f l =
  match l with
    | [] -> ()
    | h :: t ->
        (match singleton, t with Some p, [] -> p f h | _ -> p f h);
        let rec aux = function
          | [] -> ()
          | h :: t -> pp f sep; p f h; aux t in
        aux t

let sprintf fmt =
  Format.kfprintf (fun _ -> Format.flush_str_formatter ()) Format.str_formatter fmt

let ksprintf k fmt =
  let k _ =
    let s = Format.flush_str_formatter () in
    k s
  in
  Format.kfprintf k Format.str_formatter fmt

let to_string pp a = FBuffer.sprintf "%a" pp a
