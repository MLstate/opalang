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
(* Please cf mli first *)
(* todo: continue to hoist base *)
(* once Base is hoisted and clean enough, let's restart open Base *)
module String = Base.String
module List = Base.List
module Format = Base.Format
let (@**) f g x y = f (g x) (g y)

(* The map storing all possible conditions defined in WarningClass *)
let cond_map = ref StringMap.empty

(* The map storing all possible printers *)
let printer_map = ref StringMap.empty

(* The map storing all possible trackers *)
let tracker_map = ref StringMap.empty

(************************************************)
(* Options **************************************)
module Arg = struct

  module Arg = Base.Arg

  let split =
    String.slice ','

  let add_strs ?ids str set =
    let () =
      match ids with
      | None -> (* no check are perform *) ()
      | Some (name, help, possible) ->
          if not (StringMap.mem str possible)
          then
            OManager.error (
              "The %s @{<bright>%s@} is not defined !@\n"^^
              "Check the list of available %s with the option @{<bright>%s@}@\n"^^
              "%a"
            )
              name str
              name help
              (HintUtils.pp_suggestion (StringMap.keys possible)) str
    in
    match set with
    | None -> None
    | Some set ->
        Some (List.fold_left
                (fun set pass -> StringSet.add pass set)
                set (split str))

  (* Checks *************************************)
  let checks = ref (Some StringSet.empty)

  let check_all () = checks := None

  let add_check_pass str = checks := add_strs str !checks

  let add_check_conds str =
    let ids = "condition", "--check-help", !cond_map in
    checks := add_strs ~ids str !checks

  (* Tracks *************************************)
  (*
    None means everything, <=> Some (infinity)
  *)
  let tracks = ref (Some StringSet.empty)

  let track_all () = tracks := None

  let add_tracker str =
    let ids = "tracker", "--track-help", !tracker_map in
    tracks := add_strs ~ids str !tracks

  let add_track_pass str = tracks := add_strs str !tracks

  (* Prints *************************************)
  (*
    None means everything, <=> Some (infinity)
  *)
  let prints = ref (Some StringSet.empty)

  let print_all () = prints := None

  let add_printer str =
    let ids = "printer", "--print-help", !printer_map in
    prints := add_strs ~ids str !prints

  let add_print_pass str = prints := add_strs str !prints

  (* Compilation shorcuts ***********************)
  let marshals = ref (Some StringSet.empty)
  let marshal_all () = marshals := None
  let add_marshal_pass str = marshals := add_strs str !marshals
  let unmarshal = ref None
  let set_unmarshal s = unmarshal := Some s

  (* Consistency ********************************)
  let consistency = ref false

  let check_consistency () = consistency := true

  (* Helps **************************************)
  let help_factory name map =
    OManager.printf "List of available %ss :@\n" name ;
    let _ =
      StringMap.fold
        (fun k _ a ->
           OManager.printf "%s%s" a k;
           ", ")
        map "" in
    OManager.printf "@\n";
    exit 0

  let help_checker () = help_factory "condition" !cond_map
  let help_printer () = help_factory "printer" !printer_map
  let help_tracker () = help_factory "tracker" !tracker_map

  (* Options ************************************)
  let (!>) = Format.sprintf

  (* following guidelines about arguments, as in bslregister.ml, bslbrowser.ml *)
  let options = [


    (* c *)


    "--check",
    Arg.String add_check_conds,
    "\"cond1,cond2,...\" Check all specified preconditions and postconditions" ;


    "--check-all",
    Arg.Unit check_all,
    " Check all preconditions and postconditions on all passes" ;


    "--check-consistency",
    Arg.Unit check_consistency,
     " Check if the pre/post conditions are consistent" ;


    "--check-help",
    Arg.Unit help_checker,
    " Print the list of conditions" ;


    "--check-pass",
    Arg.String add_check_pass,
    "\"pass1,pass2,...\" Check all preconditions and postconditions for specified passes" ;


    (* m *)


    "--marshal-all",
    Arg.Unit marshal_all,
    " Marshal all the environments" ;


    "--marshal-pass",
    Arg.String add_marshal_pass,
    " Marshal the environment BEFORE the pass" ;


    (* p *)


    "--print",
    Arg.String add_printer,
    "\"prt1,prt2,...\" Print specified printer (like code,size,...)" ;


    "--print-all",
    Arg.Unit print_all,
    " Print all printers on all passes" ;


    "--print-pass",
    Arg.String add_print_pass,
    "\"pass1,pass2,...\" Print all printers at specified passes" ;


    "--print-help",
    Arg.Unit help_printer,
    " Print the list of printers" ;


    (* t *)

    "--track",
    Arg.String add_tracker,
    "\"prt1,prt2,...\" Print specified tracker" ;


    "--track-all",
    Arg.Unit track_all,
    " Track all passes" ;


    "--track-dir",
    Arg.String PassTracker.set_directory,
    !>
      "<dir> Specify the output directory for tracking (def is %s)" (PassTracker.get_directory ());


    "--track-pass",
    Arg.String add_track_pass,
     "\"pass1,pass2,...\" Track specified passes" ;


    "--track-help",
    Arg.Unit help_tracker,
    " Print the list of trackers" ;


    (* u *)


    "--unmarshal-pass",
    Arg.String set_unmarshal,
    " Read the previously marshalled environment to shortcut the compilation and go directly to the pass" ;
    (* most useful to profile just one pass, and not the whole compiler *)


  ]

end

(************************************************)
(* Environments *********************************)
type passname = string
type 'env printer = 'env PassTracker.printer
type 'env tracker = PassTracker.iter_tracker -> 'env -> unit

type printer_id = PassTracker.printer_id
type tracker_id = PassTracker.tracker_id

type ('opt, 'env) one_env = {
  env : 'env;
  options : 'opt;
  printers : 'opt -> (printer_id * 'env printer) list;
  trackers : 'opt -> (tracker_id * 'env tracker) list;
}

let make_env
    ?(printers = fun _ -> [])
    ?(trackers = fun _ -> [])
    options env =
  {
    env = env;
    options = options;
    printers = printers;
    trackers = trackers;
  }

let title = ref "Pass"

let set_title ti = title := ti

module RegisterPrinter =
struct
  let printers = ref []
  let register printer = printers := Obj.magic printer :: !printers
  let retrieve passname =
    let rec aux = function
      | [] -> fun _ -> []
        | h :: t ->
            match h passname with
            | None -> aux t
            | Some real_printer -> Obj.magic real_printer in
    aux !printers
end

(************************************************)
(* Conditions ***********************************)
type cond_id = string
type 'env cond = cond_id * ('env -> unit)

let compose_fun_condition (f : 'a -> 'b) ( (cond_id, g) : 'b cond) : 'a cond =
  (cond_id, (fun env -> g (f env)))

external cond_id : cond_id -> string = "%identity"

(* Internal exception, does not escape out of this module *)
exception CompilerInternalError of (cond_id option * string) HdList.t

(* Sometime for a checker it is more convenient to use an error function *)
exception CheckCondition

let _ =
  Printexc.register_printer
    (function
       | CompilerInternalError hdl -> Some (
           String.concat_map "\n\n\t"
             (fun (id, msg) ->
                match id with
                | Some id -> Printf.sprintf "The condition '%s' is violated\n%s\n" id msg
                | None -> msg)
             (HdList.unwrap hdl)
         )
       | CheckCondition -> Some "CheckCondition"
       | _ -> None)


let define_factory ?(uniq=true) name ref_map id elt =
  if uniq && StringMap.mem id !ref_map
  then
    invalid_arg (Printf.sprintf "%s %s is already defined" name id)
  else (
    ref_map := StringMap.add id elt !ref_map ;
    id
  )

let define_cond wclass =
  let cond_id = WarningClass.get_name wclass in
  define_factory "Condition" cond_map cond_id wclass

let define_printer id = define_factory ~uniq:false "Printer" printer_map id ()
let define_tracker id = define_factory ~uniq:false "Tracker" tracker_map id ()

external printer_id : printer_id -> string = "%identity"
external tracker_id : tracker_id -> string = "%identity"

let make_condition name f = (name, f)

let compose_condition conds =
  let name = fst (List.hd conds) in
  make_condition name
    (fun env ->
       List.iter
         (fun (n, f) ->
            assert (n=name);
            f env) conds)

let and_if lif =
  fun ~options env ->
    List.for_all (fun if_ -> if_ ~options env) lif

let or_if lif =
  fun ~options env ->
    List.exists (fun if_ -> if_ ~options env) lif

let check_condition env (_, check) =
  check env

let check_cond env (name, check) =
  OManager.printf "Checking condition : '@{<bright>%s@}'@\n" name;
  try
    check_condition env (name, check)
  with
  | CheckCondition -> ()
    (*
      The check_fail calls done by the checker has produced warnings.
      If the warnings are activated, they have been printed already.
      If the warnings are errors, exceptions has been caught already.
    *)

let check_conds env conds = List.iter (check_cond env) conds

(************************************************)
(* Invariants ***********************************)
type ('env, 'env2) invariant = {
  id : cond_id;
  pre : 'env cond;
  post : 'env2 cond;
}

let compose_fun_invariant f g i = {
  id = i.id;
  pre = compose_fun_condition f i.pre;
  post = compose_fun_condition g i.post
}

let make_invariant cond1 cond2 =
  if not ((fst cond1) = (fst cond2)) then
    invalid_arg (
      Printf.sprintf
        "Try to make invariant with not equals conditions : %s != %s"
        (fst cond1) (fst cond2)
    )
  else
    {
      id = (fst cond1);
      pre = cond1;
      post = cond2;
    }

let make_cons_invariant cond = make_invariant cond cond

(************************************************)
(* Errors ***************************************)
(* support for multi errors: do not crash at the first error if possible *)
let pending_internal_errors = ref []
let push_internal_error ie = pending_internal_errors := ie :: !pending_internal_errors
let raise_internal_error ie =
  let all = ie :: !pending_internal_errors in
  pending_internal_errors := [];
  raise (CompilerInternalError (HdList.wrap (List.rev all)))
let flush_internal_errors () =
  match !pending_internal_errors with
  | [] -> ()
  | ies ->
      pending_internal_errors := [];
      raise (CompilerInternalError (HdList.wrap (List.rev ies)))

let sinternal_error cont printer cond_id context fmt =
  let k message =
    let printer oc context =
      Format.fprintf oc "@{<red>Internal Error@}@\n%a@\n%s@\n" printer context message in
    (* Do not print everything, the function handler does thing too *)
    (* By having some code there, we can print the context is the file system *)
    (* (if not, the Exception should have a type variable in its type !) *)
    PassTracker.internal ~filename:(Option.default "internal_error" cond_id) printer context;
    let message = Format.sprintf "%a" printer context in
    push_internal_error (cond_id, message);
    cont ()
  in
  Format.ksprintf k fmt

let scond_violation printer cond_id = sinternal_error ignore printer (Some cond_id)
let i_serror p = sinternal_error ignore p

let internal_error p =
  let k _ = flush_internal_errors (); assert false (* will necessarly do a raise during sinternal_error *) in
  sinternal_error k p

let cond_violation printer cond_id = internal_error printer (Some cond_id)
let i_error = internal_error

let cont_check_fail cont ~full:full_printer ~console:reduced_printer cond_id context fmt =
  let k message =
    let full_printer oc context =
      Format.fprintf oc "@{<red>Check Failed@}: %s@\n%a@[<2>@\n%s@]@\n" cond_id full_printer context message in
    let wclass =
      match StringMap.find_opt cond_id !cond_map with
      | None -> assert false (* all cond_id are create by using define_cond *)
      | Some wclass -> wclass in
    OManager.printf "@{<red>Internal Error@}, a condition check is broken: '@{<bright>%s@}'@\n" cond_id;
    OManager.printf "The full context is being stored (opatrack) ...@.";
    PassTracker.check_fail ~filename:cond_id full_printer context;
    OManager.printf "Here is a reduced context (use opatrack for full context):@\n";
    reduced_printer OManager.oformatter.contents context;
    OManager.warning ~wclass "%s" message ;
    cont ()
  in
  Format.ksprintf k fmt

let scheck_fail ~full = cont_check_fail ignore ~full
let check_fail ~full = cont_check_fail (fun _ -> raise CheckCondition) ~full

(************************************************)
(* Passes & Handlers ****************************)
type ('opt, 'opt2, 'env, 'env2) pass = {
  invariant : ('env, 'env2) invariant list;
  precond  : 'env cond list;
  postcond : 'env2 cond list;
  f : ('opt, 'env) one_env -> ('opt2, 'env2) one_env;
}

type ('opt, 'env, 'env2) old_pass = options:'opt -> 'env -> 'env2

let make_pass ?(invariant = []) ?(precond=[]) ?(postcond=[]) f = {
  f = f;
  invariant = invariant;
  precond = precond;
  postcond = postcond;
}

let init = make_env () ()

(* New handlers *********************************)

(* Current conditions, used for check consistency *)
let current_conds = ref StringSet.empty

(* Keep in mind post cond of prev pass *)
let prev_post_cond = ref StringSet.empty

let handler ?(count_time=true) passname pass env =
  (* common filter for conds and prints *)
  let common_filter set list =
    match set with
    | None -> list
    | Some set ->
        if StringSet.mem passname set then list
        else List.filter
          (fun (str, _) ->
             (StringSet.mem str set)
             || StringSet.exists (fun elt -> String.is_prefix elt str) set)
          list in

  (* Keep in mind what is checked *)
  let checked_conds = Hashtbl.create 32 in

  (* checks conditions to check *)
  let perform_conds str env conds =
    let conds = common_filter !Arg.checks conds in
    if conds != [] then (
      List.iter (fun (id, _) -> Hashtbl.add checked_conds id ()) conds;
      OManager.printf "%sconditions check.@\n" str;
      check_conds env.env conds
    ) else () in

  (* extra print env *)
  let perform_print env =
    let printers = common_filter !Arg.prints (env.printers env.options) in
    List.iter
      (fun (printer_id, printer) ->
         PassTracker.print ~passname ~printer_id printer env.env)
      printers
  in

  (* track pass *)
  let perform_track env =
    let trackers = common_filter !Arg.tracks (env.trackers env.options) in
    List.iter
      (fun (tracker_id, tracker) ->
         PassTracker.track ~passname ~tracker_id tracker env.env)
      trackers
  in

  (* Get invariant to check *)
  let get_invariants =
    let inv =
      ref (
        List.filter
          (fun iv -> StringSet.mem iv.id !prev_post_cond)
          pass.invariant)
    in fun _ -> !inv
  in

  (* Checking invariant on input env - Maybe useless but two check
     worth better than one...*)
  let pre_perform_invariant env =
    let ivs = List.map (fun iv -> iv.pre) (get_invariants ()) in
    let ivs = common_filter !Arg.checks ivs in
    if ivs != [] then (
      OManager.printf "Preinvariants check.@\n";
      check_conds env.env ivs
    ) else ()
  in

  (* Checking invariant on output env *)
  let post_perform_invariant env =
    let ivs = List.map (fun iv -> iv.post) (get_invariants ()) in
    let ivs = common_filter !Arg.checks ivs in
    if ivs != [] then (
      OManager.printf "Postinvariants check.@\n";
      check_conds env.env ivs
    ) else ();
    prev_post_cond :=
      List.fold_left (fun a (id, _) -> StringSet.add id a)
        StringSet.empty pass.postcond;
    prev_post_cond :=
      List.fold_left (fun a (id, _) -> StringSet.add id a)
        !prev_post_cond ivs
  in

  (* Check consistency *)
  let consistency () =
    if !Arg.consistency then
      let unconsistent =
        List.filter
          (fun (id, _) -> not (StringSet.mem id !current_conds))
          pass.precond in
      if unconsistent != [] then
        OManager.warning ~wclass:WarningClass.phandler_consistency
          "The following conditions are inconsistent : %s"
          (fst
             (List.fold_left
                (fun (s, p) (id, _) ->
                   (Printf.sprintf "%s%s%s" s p id), ", ")
                ("", "") unconsistent
             ));
      current_conds :=
        StringSet.filter
          (fun id -> not (List.exists (fun iv -> iv.id = id) pass.invariant))
          !current_conds;
      current_conds :=
        List.fold_left
          (fun acc pcond -> StringSet.add (fst pcond) acc)
          !current_conds
          pass.postcond;
  in

  (* Store passname for tracking specific interal logs *)
  PassTracker.set_current_passname passname;

  let run =
    match !Arg.unmarshal with
    | Some passname' when passname = passname' ->
        (* loading the environment from the disk *)
        OManager.verbose "%s: unmarshalling env for @{<bright>%s@}" !title passname;
        let env,options = PassTracker.unmarshal ~passname in
        let printers = RegisterPrinter.retrieve passname in
        `run ({env = env; options=options; printers=printers; trackers = (fun _ -> [])})
    | Some _ ->
        (* we are loading one pass, but not this one *)
        `skip
    | None ->
        (* normal compilation *)
        (match !Arg.marshals with
         | None -> PassTracker.marshal ~passname (env.env,env.options)
         | Some set ->
             if StringSet.mem passname set then
               PassTracker.marshal ~passname (env.env,env.options));
        `run env in

  match run with
  | `skip ->
      OManager.verbose "%s: skipping @{<bright>%s@}" !title passname;
      Obj.magic env
  | `run env -> (

  (* Pre traitment *)
  if not BuildInfos.is_release && not (passname = "") then
    OManager.verbose "%s: %s" !title passname;
  consistency ();
  pre_perform_invariant env;
  perform_conds "Pre" env pass.precond;
  OManager.flush_errors ();

  (* Pass *)
  let chrono = Chrono.make () in
  Chrono.start chrono;
  let env2 =
    let save_bck, get_bck =
      let bck = ref "No backtrace" in
      ((fun () -> bck := (Printexc.get_backtrace ())),
       (fun _ -> !bck)) in
    try (
      try let env = pass.f env in flush_internal_errors (); env with
      | (CompilerInternalError internal_errors) as e ->
          save_bck ();
          let internal_errors = HdList.unwrap internal_errors in
          OManager.printf "An internal error has occurred@\n";
          List.iter
            (function (_, msg) ->
               OManager.printf "Message:@\n@{<magenta>%s@}@\n" msg) internal_errors;
          let internal_errors = List.uniq_unsorted ~cmp:(Pervasives.compare @** fst) internal_errors in
          List.iter (fun (id, _) ->
          begin
            match id with
            | Some id ->
                OManager.printf (
                  "The condition '@{<bright>%s@}' has been declared violated by @{<bright>%s@}@\n"
                ) id passname;
                (match List.find_opt (fun (s , _) -> id = s ) pass.precond with
                 | None ->
                     OManager.printf "'@{<bright>%s@}' is not a precondition of '@{<bright>%s@}'@\n  This pass is incoherent !!@\n" id passname
                 | Some precond ->
                     let is_warn_error = Option.default_map false (fun w -> WarningClass.is_warn w && WarningClass.is_warn_error w)
                       (StringMap.find_opt id !cond_map) in
                     let fatal_inconsistency () = OManager.printf (
                       "No error during the execution of the condition checker.@\n"^^
                       "The corresponding '@{<bright>--warn-error @{<red>%s@}@}' was activated.@\n"^^
                       "@{<red>FATAL INCONSISTENCY !@}@\n"^^
                       "@[<2>@{<bright>Hint@}:@\nPlease fix the coherence between the check impl., the pass impl., and the cond semantic.@]@\n" )
                       id
                     in
                     if Hashtbl.mem checked_conds id
                     then
                       begin
                         OManager.printf "'%s' has already been tested as precondition of pass '%s'@\n" id passname;
                         if is_warn_error
                         then fatal_inconsistency ()
                       end
                     else
                       begin
                         OManager.printf "Checking %s@\n" id;
                         check_cond env.env precond;
                         if is_warn_error && not (List.exists (fun w -> String.compare id (WarningClass.get_name w) = 0) (OManager.warn_error_status ()))
                         then fatal_inconsistency ()
                       end
                )
            | None -> ()
          end;
            ) internal_errors;
          raise e
      | e ->
          save_bck ();
          OManager.serror "An internal error has occurred during the pass @{<bright>%s@}" passname;
          raise e )
    with
    | e ->
        (* Print all that is possible - TODO add the possibility to
           make filter *)
        (match e with CompilerInternalError _ -> () | _ -> OManager.printf "@[<2>Uncaught Exception:@\n@{<magenta>%s@}@]@\n" (Printexc.to_string e));
        OManager.printf "@[<2>Backtrace:@\n%s@]@\n" (get_bck ());
        PassTracker.internal ~filename:"backtrace" (fun fmt _ -> Format.fprintf fmt "%s@\nBacktrace:\n%s@\n" (Printexc.to_string e) (get_bck ())) ();
        OManager.printf "Saving the current environment for report (opatrack) ...@.";
        Arg.prints := None;
        perform_print env;
        Arg.tracks := None;
        perform_track env;
        OManager.flush_errors ();
        exit 1
  in
  Chrono.stop chrono;
  let c = Chrono.read chrono in
  let () =
    if not BuildInfos.is_release && count_time then PassTracker.time ~passname c
  in

  (* Post traitment *)
  perform_print env2;
  perform_track env2;
  post_perform_invariant env2;
  perform_conds "Post" env2 pass.postcond;
  OManager.flush_errors ();
  #<If:TESTING> ()
  #<Else>
    if not BuildInfos.is_release && count_time && c > 0.2 then
      OManager.verbose "duration %s" (Date.pretty_duration c)
  #<End>;
  env2
    )

let (|+>) env (passname, pass) = handler passname pass env

let if_handler
    ?(if_ = fun ~options:_ _ -> true)
    name pass env =
  if if_ ~options:env.options env.env then
    handler name pass env
  else env

let (|?>) env (if_, passname, pass) =
  if_handler ~if_ passname pass env

let alt_handler if_ (name1, pass1) (name2, pass2) env =
  if if_ ~options:env.options env.env then
    handler name1 pass1 env
  else
    handler name2 pass2 env

let (<?>) env (if_, (name1, pass1), (name2, pass2)) =
  alt_handler if_ (name1, pass1) (name2, pass2) env

let switch_handler switch pass env =
  let switcher = switch ~options:env.options env.env in
  let name, pass = pass switcher in
  handler name pass env

let (|?|) env (switch, pass) =
  switch_handler switch pass env

let return env = env.env

let (|>)  = InfixOperator.(|>)

let register_printer = RegisterPrinter.register

(* Old handlers *********************************)
let old_if_handler
    ?(if_ = fun ~options:_ _ -> true)
    ?(precond = []) ?(postcond = [])
    name pass env =
  if if_ ~options:env.options env.env then
    let pass =
      make_pass ~precond ~postcond
        (fun env ->
           { env with
               env = pass ~options:env.options env.env })
    in
    handler name pass env
  else env

(* This handler should be no used because create env with default
   printer... *)
let old_handler
    ?(precond = []) ?(postcond = [])
    name pass env =
  let pass =
    make_pass ~precond ~postcond
      (fun e ->
         make_env e.options (pass ~options:e.options e.env)) in
  handler name pass env


let (&) if1 if2 = fun ~options env -> if1 ~options env && if2 ~options env
let (or) if1 if2 = fun ~options env -> if1 ~options env || if2 ~options env
let neg if_ = fun ~options env -> not (if_ ~options env)
