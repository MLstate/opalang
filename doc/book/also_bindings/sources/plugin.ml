(* This is an Ocaml file, containing opa preprocessing directives *)

##register stammer : string -> string
let stammer s = "server is stammering: " ^ s ^ s ^ s
