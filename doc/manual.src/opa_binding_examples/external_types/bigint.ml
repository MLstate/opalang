(**
   Minimal Opa Binding for the Big_int module (Opa Manual)
*)

(**
   External type definition.
   The left part is the name of the type as it will be available in Opa.
   The right part is the name of the type as it is defined by the Ocaml library
*)
##extern-type BigInt.t = Big_int.big_int

(**
   For this example, we will export a reduced API, just to add 2 big int
   given by the user of a very simple web page.
*)

(**
   This function exists already 'as is' in the Ocaml library,
   we can use the \ ` ` syntax : the function will be inlined in
   the generated code.
*)
##register add \ `Big_int.add_big_int` : BigInt.t, BigInt.t -> BigInt.t

(**
   We prefer to deal with option for handling error cases in Opa.
*)
##register of_string : string -> option(BigInt.t)
let of_string string =
  try
    Some (Big_int.big_int_of_string string)
  with
  | _ -> None

(**
   And we want to see the result
*)
##register to_string \ `Big_int.string_of_big_int` : BigInt.t -> string
