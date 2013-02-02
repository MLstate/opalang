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

let get_closest_names list typo =
  let distances =
    List.rev_map (
      fun name -> (name, Sed.sed Sed.qwerty_distance name typo)
    ) list in
  let sorted =
    List.sort (
      fun (_, (value_1 : float)) (_, (value_2 :float)) -> compare value_1 value_2
    ) distances in
  List.map fst sorted


let get_closest_names_2 list typo =
  match get_closest_names list typo with
  | tag1 :: tag2 :: _ -> tag1, tag2
  | _ -> invalid_arg "HintUtils.get_closest_names_2"


let pp_suggestion all fmt typo =
  match get_closest_names all typo with
  | [] -> ()
  | [name] ->
      Format.fprintf fmt "@[<2>@{<bright>Hint@}:@\nPerhaps you meant @{<bright>%s@} ?@]@\n" name
  | (name_1)::(name_2)::_ ->
      Format.fprintf fmt "@[<2>@{<bright>Hint@}:@\nPerhaps you meant @{<bright>%s@} or @{<bright>%s@} ?@]@\n" name_1 name_2
