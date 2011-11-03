module Arg = Base.Arg

type t = Classic | Js

module Args = struct

  type options = {
    parser : t;
    printer : t;
  }

  let default_options = {
    parser = Classic;
    printer = Classic;
  }

  let is_default t = if default_options.parser = t then " (default)" else ""

  let descr = function
    | Classic -> "classic"
    | Js      -> "js-like"

  let assoc = [("js-like", Js); ("classic"), Classic]

  let r = ref default_options

  let options = [
    ("--parser", Arg.spec_fun_of_assoc (fun s -> r := {!r with parser=s}) assoc,
     "Select kind of the input syntax (classic or js-like)");
    ("--printer", Arg.spec_fun_of_assoc (fun s -> r := {!r with printer=s}) assoc,
     "Select kind of the ouput syntax (classic or js-like)")
  ]

end
