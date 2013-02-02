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

let _output_channel = stderr

module Album =
struct

  let walker =
    let walk_anim = [|
      [|
        "  @ ";
        "</| ";
        ")\\| "
      |] ; [|
        "  @ ";
        "</\\ ";
        ")\\ \\";
      |] ; [|
        "  @ ";
        "</ \\";
        ")\\ |";
      |] ; [|
        "  @ ";
        "// \\";
        "/ )|";
      |] ; [|
        " _ @";
        "  /|";
        "/ )|";
      |] ; [|
        " _ @";
        "  /|";
        " )\\|"
      |] |]
    in
    let turn_anim = [|
      [|
        " @ ";
        " | ";
        " )\\";
      |] ; [|
        " @ ";
        " | ";
        "/( "
      |] |]
    in
    ConsoleAnim.init ~och:_output_channel ~auto_walking:true walk_anim turn_anim

  let arrow =
    let walk_anim = [| [| "====>" |] |]
    in
    let turn_anim = [|
      [| " ====" |];
      [| "  ===" |];
      [| "   ==" |];
      [| "    =" |];
      [| "     " |];
      [| "    <" |];
      [| "   <=" |];
      [| "  <==" |];
      [| " <===" |];
      [| "<====" |]
    |]
    in
    ConsoleAnim.init ~och:_output_channel ~auto_walking:true walk_anim turn_anim

  let simple =
    let a = [|
      [| "...." |];
      [| " ..." |];
      [| ". .." |];
      [| ".. ." |];
      [| "... " |];
    |]
    in
    ConsoleAnim.init ~och:_output_channel ~auto_walking:false a a
  let pong =
    let a = [|
      [| "|   o|" |];
      [| "|  o |" |];
      [| "| o  |" |];
      [| "|o   |" |];
      [| "| o  |" |];
      [| "|  o |" |];
    |]
    in
    ConsoleAnim.init ~och:_output_channel ~auto_walking:false a a
  let opa =
    let a = [|
      [| "OPA    " |];
      [| " OPA   " |];
      [| "  OPA  " |];
      [| "   OPA " |];
      [| "    OPA" |];
      [| "A    OP" |];
      [| "PA    O" |];
    |]
    in
    ConsoleAnim.init ~och:_output_channel ~auto_walking:false a a

  let all = [|
    walker ;
    arrow ;
    simple ;
    pong ; opa;
  |]
end

module Options =
struct

  type disp = True | False | ForceExit

  let och = _output_channel
  let chosen_walker = ref Album.simple (*Album.all.(Random.int (Array.length Album.all))*)
  let time_interv = ref 0.10
  let disp = ref False

  let available_walkers = ["grandpa";"arrow";"simple";"pong";"opa";"random"]
  let set_opa_walker = function
    | "grandpa" -> chosen_walker := Album.walker
    | "arrow" -> chosen_walker := Album.arrow
    | "simple" -> chosen_walker := Album.simple
    | "pong" -> chosen_walker := Album.pong
    | "opa" -> chosen_walker := Album.opa
    | "random" -> chosen_walker := Album.all.(Random.int (Array.length Album.all))
    | other -> failwith (Printf.sprintf "set_opa_walker: unknown argument %S" other)

end

let _ =
  let do_walker () =
    while !Options.disp <> Options.ForceExit do
      Thread.delay !Options.time_interv;
      if !Options.disp = Options.True then
        ConsoleAnim.update !Options.chosen_walker
    done
  in
  ignore (Thread.create do_walker ())
