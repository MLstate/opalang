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
(* see mli *)


let alphanum =

  (* some math utils *)
  let sq x = x *x in
  let rec (^^) x p =
    if p = 0 then 1
    else (sq (x ^^ (p/2))) * (if p mod 2 = 0 then 1 else x)

  in

  (* we use geometric serie to denumbrate the number of name up to a given size *)

  (* compute a geometric serie ,
     n is the reason, the number of q multiplication *)
  let serie_geo a q n = a * ( 1 - (q^^n) ) / (1 - q) in

  (* the approximate reverse computation *)
  let inverse_serie_geo a q sn =
    (* log[q]((1-q+a)/a * sn) *)
    int_of_float (floor
                    ((log (float ((q-1) * sn + a)) -. log (float a)) /. (log (float q)))
    )
  in

  let alpha="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in

  (* the first invalid pos for the first char *)
  let zero_pos = String.index alpha '0' in

  (* the first invalid pos for the others char *)
  let bad_pos = String.length alpha in

  let int_to_char i = String.unsafe_get alpha i in

  assert(int_to_char zero_pos='0');

  let name_of_int i =
    let to_char_list i n =
      let rec aux i n acc =
        if n<=0 then acc
        else
          let dizaines = i / bad_pos in
          let unites = i mod bad_pos in
          let acc = (int_to_char unites)::acc in
          aux dizaines (n-1) acc
      in aux i n []
    in
    let first_digit, remain, n_last_digits =
      let n = inverse_serie_geo zero_pos bad_pos i in
      let bound = serie_geo zero_pos bad_pos n in
      assert(bound<=i);
      let offset = i - bound  in
      let scale = bad_pos ^^ n in
      let remain = offset mod scale in
      let first_digit = offset / scale in
      first_digit, remain, n
    in
    let char_list = to_char_list remain n_last_digits in
    let first_char = int_to_char first_digit in
    assert( first_char <> '0' );
    Base.String.of_chars (first_char::char_list)
  in
  name_of_int

let alphanum_generator ~prefix =
  let ref_int = ref 0 in
  fun () ->
    incr ref_int;
    prefix^(alphanum !ref_int)

(*
let f = alphanum_generator ~prefix:"_";;
for i = 0 to 200 do print_endline (f ()) ; done;;
*)
