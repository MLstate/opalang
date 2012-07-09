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
(* THIS FILE HAS A DOCUMENTED MLI *)

type at_exit = { at_exit : 'a. int -> 'a }
let main_at_exit = ref { at_exit = (fun i -> exit i) }
let exit code = !main_at_exit.at_exit code

(* Default formatter for OManager is stderr *)
let oformatter = ref Format.err_formatter
let odescr = Unix.stderr
  (* don't use [odescr] for outputting because you would
   * have troubles with order of output since the formatter
   * and the file descriptor have different buffers
   * it is just used for setting a default behavior for colors *)

type ('params,'output) oformat = ('params, Format.formatter, unit, 'output) format4

module Color = struct
  module Arg = Base.Arg
  let set_color b = Format.pp_set_tags !oformatter b

  let () = set_color (Unix.isatty odescr)
    (* by default, color on a terminal
     * no color if stderr is redirected to a file *)

  let console_set_color b =
    Ansi.set_ignore_color (not b) ;
    set_color b ;
    ()

  let args = [ "--no-color", Arg.Unit (fun () -> console_set_color false), " Disable any ANSI escape in the output (default for non ttys)"
             ; "--force-color", Arg.Unit (fun () -> console_set_color true), " Enable ANSI escape in the compiler output (default for ttys)" ]

  (* the state allows us to nest escapes *)
  type state =
      { reversed : string
      ; crossed : string
      ; underlined : string
      ; intensity : string
      ; foreground : string
      ; background : string
      ; old_state : state
      ; undo : string } (* the escapes to restore old_state *)

  let rec default_state =
    { reversed = "[27m" (* not reversed *)
    ; crossed = "[29m" (* not crossed *)
    ; underlined = "[24m" (* not underlined *)
    ; intensity = "[22m" (* normal intensity *)
    ; foreground = "[39m" (* default foreground *)
    ; background = "[49m" (* default background *)
    ; old_state = default_state (* shouldn't be used *)
    ; undo = "" } (* since old_state is default_state, you don't have anything to undo to restore it *)
  let default_state_of state =
    { reversed = "[27m"
    ; crossed = "[29m"
    ; underlined = "[24m"
    ; intensity = "[22m"
    ; foreground = "[39m"
    ; background = "[49m"
    ; old_state = state
    ; undo = "" }

  (* the current state *)
  let state = ref default_state

  (* the function that restores the previous state *)
  let undo () =
    let s = !state.undo in
    state := !state.old_state;
    s

  (* a bunch of functions to update the stack *)
  let fg s =
    let state' = !state in
    state := {state' with foreground = s; undo = state'.foreground; old_state = state'};
    s
  let bg s =
    let state' = !state in
    state := {state' with background = s; undo = state'.background; old_state = state'};
    s
  let reverse () =
    let state' = !state in
    let reversed = state'.reversed in
    let reverse = "[7m" in
    let unreverse = "[27m" in
    let s = if reversed == reverse then unreverse else reverse in
    (* if you try to reverse the colors when they are already reversed, you get the normal colors *)
    state := {state' with reversed = s; undo = state'.reversed; old_state = state'};
    s
  let cross () =
    let state' = !state in
    let s = "[9m" in
    state := {state' with crossed = s; undo = state'.crossed; old_state = state'};
    s
  let underline () =
    let state' = !state in
    let s = "[4m" in
    state := {state' with underlined = s; undo = state'.underlined; old_state = state'};
    s
  let intensity s =
    let state' = !state in
    state := {state' with intensity = s; undo = state'.intensity; old_state = state'};
    s
  let nop () =
    state := {!state with undo = ""; old_state = !state};
    ""

  (* a brutal way to restore a state: just concatenate all its ansi escapes *)
  let make_state { reversed = reversed
                 ; crossed = crossed
                 ; underlined = underlined
                 ; intensity = intensity
                 ; foreground = foreground
                 ; background = background } =
    reversed ^ crossed ^ underlined ^ intensity ^ background ^ foreground
  let full_undo () =
    let state' = !state in
    state := {(default_state_of state') with undo = make_state state' };
    "[0m"

  (* the strings that are accepted inside @{<blabla> *)
  let color = function
    | "reset" -> full_undo ()
    | "reverse" -> reverse ()
    | "cross" -> cross ()
    | "underline" -> underline ()
    | "dim" -> intensity "[2m"
    | "bright" -> intensity "[1m"
    | "normal" ->  intensity "[22m"

    | "black" -> fg "[30m"
    | "red" -> fg "[31m"
    | "green" -> fg "[32m"
    | "yellow" -> fg "[33m"
    | "blue" -> fg "[34m"
    | "magenta" -> fg "[35m"
    | "cyan" -> fg "[36m"
    | "white" -> fg "[37m"
    | "default" -> fg "[39m"

    | "Black" -> bg "[40m"
    | "Red" -> bg "[41m"
    | "Green" -> bg "[42m"
    | "Yellow" -> bg "[43m"
    | "Blue" -> bg "[44m"
    | "Magenta" -> bg "[45m"
    | "Cyan" -> bg "[46m"
    | "White" -> bg "[47m"
    | "Default" -> bg "[49m"

    | _ -> nop ()

  (* allowing people to give several comma separated tags in one @{<...>@} *)
  let delim = Str.regexp "[ \t]*,[ \t]*"
  let mark_open s =
    let sl = Str.split delim s in
    String.concat "" (List.map color sl)
  let mark_close s =
    let sl = Str.split delim s in
    String.concat "" (List.map (fun _ -> undo ()) sl)

  let color_tags =
    { Format.mark_open_tag = mark_open
    ; Format.mark_close_tag = mark_close
    ; Format.print_open_tag = ignore
    ; Format.print_close_tag = ignore }

  let () =
    #<If:OMANAGER_DEBUG>()#<Else>
    Format.pp_set_formatter_tag_functions !oformatter color_tags
    #<End>
end

let printf fmt = Format.fprintf !oformatter fmt

let kfprintf = fun f fmt -> Format.kfprintf f !oformatter fmt

let ifprintf fmt = Format.ifprintf !oformatter fmt

module Verbose = struct

  let verb = ref false
  let quiet = ref false

  let set_verbose v = verb := v

  let is_verbose () = !verb

  let verbose fmt =

    if !verb then
      printf ("@{<blue>"^^fmt^^"@}@.")
    else
      ifprintf fmt

  let set_quiet q = quiet := q

  let is_quiet () = !quiet

  let unquiet fmt =

    if not !quiet then
      printf ("@{<blue>"^^fmt^^"@}@.")
    else
      ifprintf fmt
end

module WarnSet =
  BaseSet.Make(struct
            type t = WarningClass.wclass
            let compare = compare
          end)

module Error = struct

  let warn_set = ref WarnSet.empty

  let add_warn_error warn =
    warn_set := WarnSet.add warn !warn_set

  let error_fmt fmt = ("@{<red>Error@}@\n"^^fmt^^"@.")

  let print_public_error fmt =
    printf (error_fmt fmt)

  let public fmt =
    kfprintf (fun _ -> exit 1) (error_fmt fmt)

  let public_error = ref false
  let s_public fmt =
    kfprintf (fun _ -> public_error := true) (error_fmt fmt)

  let print_internal_error = print_public_error

  let internal_fmt fmt =
    ("@{<red>Internal Error@}@\n"^^fmt^^"@.")

  let quit_internal _ =
    if Printexc.backtrace_status ()
    then failwith "OManager.i_error: backtrace"
    else exit 2

  let internal fmt =
    kfprintf quit_internal (internal_fmt fmt)

  let internal_error = ref false
  let s_internal fmt =
    kfprintf (fun _ -> internal_error := true) (internal_fmt fmt)

  let flush () =
    if not (WarnSet.is_empty !warn_set) then (
      let _, msg =
        WarnSet.fold
          (fun w (pre,acc) ->
             ",", Printf.sprintf "%s%s '%s'" acc pre (WarningClass.get_name w))
          !warn_set ("","Fatal warning:") in
      s_public "%s\n" msg;
    );
    printf "%!";
    if !public_error then exit 10;
    if !internal_error then quit_internal ();
    ()
end

module Warning = struct

  let warning ~wclass fmt =
    if WarningClass.is_warn wclass then
      (if WarningClass.is_warn_error wclass then
         Error.add_warn_error wclass;
       printf ("@{<yellow>Warning %s@}@\n"^^fmt^^"@.")
                 (WarningClass.get_name wclass))
    else ifprintf fmt

end

let set_color = Color.console_set_color

let set_verbose = Verbose.set_verbose

let is_verbose = Verbose.is_verbose

let set_quiet = Verbose.set_quiet

let is_quiet = Verbose.is_quiet

let verbose = Verbose.verbose

let unquiet = Verbose.unquiet

let error = Error.public

let serror = Error.s_public

let i_error = Error.internal

let i_serror = Error.s_internal

let flush_errors = Error.flush

let warning = Warning.warning

let warn_error_status () = WarnSet.elements !Error.warn_set

module CompilerAsLib =
struct
  let set_stderr stderr =
    (* odescr is just for having a default behavior for colors *)
    oformatter := stderr

  let set_stdout _ = assert false

  let at_exit at_exit =
    main_at_exit := at_exit
 end

let apologies () =
  printf "Our apologies, an @{<bright>internal error@} has stopped the process@\nYou may get support, and contribute to the Opa development by @{<bright>reporting@} this problem to MLstate@\n"


let this_is_tool ?(force=false) tool =
  let pp = if force then printf else verbose in
  pp "This is @{<bright>%s@} version @{<bright>%s@}: (c) @{<bright>MLstate@} %s"
    tool BuildInfos.version_id BuildInfos.year ;
  if force then printf "@\n" else ()

module Arg =
struct
  module Arg = Base.Arg (* Base.Arg *)
  (* todo: hide printf with --quiet *)
  let options = Color.args @ [

    "--verbose",
    Arg.Unit (fun () -> Verbose.set_verbose true),
    " Compiler is more verbose (print some logs)" ;

    "--quiet",
    Arg.Unit (fun () -> Verbose.set_verbose false; Verbose.set_quiet true),
    " The compiler is quiet (less logs)"

  ]

  let version tool =
    "--version",
    Arg.Unit (fun () -> this_is_tool ~force:true tool; exit 0),
    " Print version and exit"
end
