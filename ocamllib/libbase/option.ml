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
    @author Rudy Sicard
    @author Pascal Rigaux
    @author Mehdi Bouaziz
    @author Mathieu Barbin
    @author François-Régis Sinot
    @author Henri Binsztok
*)

let default d = function (* in module Base too *)
  | None -> d
  | Some e -> e

let default_lazy d = function (* in module Base too *)
  | None -> Lazy.force d
  | Some e -> e

let is_none = function None -> true | _ -> false (* in module Base too *)
let is_some = function None -> false | _ -> true (* in module Base too *)

let if_none o a b = match o with None -> a | _ -> b
let if_some o a b = match o with None -> b | _ -> a

let get = function
  | None -> failwith "Option.get"
  | Some e -> e

let get_exn exn = function
  | None -> raise exn
  | Some e -> e

let map f = function
  | None -> None
  | Some e -> Some (f e)

let map2 f o1 o2 =
  match o1, o2 with
  | Some e1, Some e2 -> Some (f e1 e2)
  | _ -> None

let apply o v =
  match o with
  | Some f -> f v
  | None -> v

let fold f acc = function
  | None -> acc
  | Some e -> f acc e

let fold_right f opt acc =
  match opt with
  | None -> acc
  | Some e -> f e acc

let foldmap f acc = function
  | None -> acc, None
  | Some e ->
      let acc, e = f acc e in
      acc, Some e

let foldmap_stable tra acc opt =
  match opt with
  | None -> acc, opt
  | Some e ->
      let acc, fe = tra acc e in
      acc,
      if e == fe then opt else
        Some fe

let iter f = function
  | None -> ()
  | Some x -> f x

let bind f = function
  | None -> None
  | Some e -> f e

let to_list = function
  | None -> []
  | Some x -> [x]

let default_map d f = function
  | None -> d
  | Some e -> f e

let default_lazy_map d f = function
  | None -> Lazy.force d
  | Some e -> f e

(* monadic join -- don't eta-reduce *)
let join m =  bind (fun x -> x) m

let make_compare cmp o1 o2 =
  match o1, o2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x1, Some x2 -> cmp x1 x2

let merge conflict o1 o2 =
  match o1, o2 with
  | _, None -> o1
  | None, Some _ -> o2
  | Some x1, Some x2 -> Some (conflict x1 x2)

let to_string fct = function
  | None -> "None"
  | Some o -> Printf.sprintf "Some %s" (fct o)

let pp_none pp fmt = function
  | None -> Format.pp_print_string fmt "none"
  | Some o -> pp fmt o

let pp pp fmt = function
  | None -> ()
  | Some o -> pp fmt o

let pp_sep sep pp fmt = function
  | None -> ()
  | Some o ->
      Format.fprintf fmt sep ;
      pp fmt o

let pp_default none pp fmt = function
  | None -> pp fmt none
  | Some some -> pp fmt some

let pp_meta pp fmt = function
  | None -> Format.fprintf fmt "None"
  | Some o -> Format.fprintf fmt "Some (%a)" pp o

let exists p = function
  | None -> false
  | Some v -> p v
let for_all p = function
  | None -> true
  | Some v -> p v
