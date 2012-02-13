(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
