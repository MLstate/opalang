module String = BaseString
module Arg = Base.Arg

type t = Classic | Js

module Args = struct

  type options = {
    files : t StringMap.t;
    parser : t;
    printer : t;
  }

  let default_options = {
    files = StringMap.empty;
    parser = Js;
    printer = Js;
  }

  let is_default t = if default_options.parser = t then " (default)" else ""

  let descr = function
    | Classic -> "classic"
    | Js      -> "js-like"

  let assoc = [("js-like", Js); ("classic"), Classic]

  let r = ref default_options

  let parser_options = function
    | "js-like" -> r:= {!r with parser=Js}
    | "classic" -> r:= {!r with parser=Classic}
    | str ->
        let add_files files t =
          let files =
            List.fold_left
              (fun m f -> StringMap.add f t m)
              !r.files (String.slice ',' files)
          in r := {!r with files}
        in
        match String.split_char ':' str with
        | ("classic", files) -> add_files files Classic
        | ("js-like", files) -> add_files files Js
        | (_, _) -> failwith (Printf.sprintf "'%s' unexpected syntax" str)

  let get_printer () = !r.printer

  let get_parser filename =
    match filename with
    | None -> !r.parser
    | Some filename -> try StringMap.find filename !r.files with Not_found -> !r.parser

  let set_parser p = r := { !r with parser = p }

  let options = [
    ("--parser", Arg.String parser_options,
     "Select kind of the input syntax (classic or js-like)");
    ("--printer", Arg.spec_fun_of_assoc (fun s -> r := {!r with printer=s}) assoc,
     "Select kind of the ouput syntax (classic or js-like)")
  ]

end
