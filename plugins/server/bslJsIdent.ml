(*
    Copyright © 2011, 2012 MLstate

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
(* depends *)
module Hashtbl = Base.Hashtbl

(* -- *)

type renaming = [
  | `no
  | `yes
  | `fake
]

let cleaning_default_value = ref true
let cleaning = ref None
let renaming = ref `yes
let root_table = Hashtbl.create 1024

module Sa = ServerArg

let spec = [
  ["--js-cleaning"],
  Sa.func Sa.string (
    fun () string ->
      match string with
      | "yes" ->
          cleaning := Some true
      | "no" ->
          cleaning := Some false
      | _ ->
          Printf.eprintf "--js-cleaning: unknown mode %s (ignored)%!\n" string ;
          ()
  ),
  "",
  "Js runtime cleaning options (no, yes)"
  ;

  ["--js-renaming"],
  Sa.func Sa.string (
    fun () string ->
      match string with
      | "yes" ->
          renaming := `yes
      | "no" ->
          renaming := `no
      | "fake" ->
          renaming := `fake
      | _ ->
          Printf.eprintf "--js-renaming: unknown mode %s (ignored)%!\n" string ;
          ()
  ),
  "",
  "Js runtime renaming options (no, yes, fake)"
  ;

  ["--js-root"],
  Sa.func Sa.string (
    fun () string ->
      Hashtbl.add root_table string ()
  ),
  "",
  "Js root declaration (testing)"
  ;

]

let _ = ServerArg.filter () (ServerArg.make_parser "bslJsIdent" spec)

(**
   associate a ident name to a key ident,
   ident are generated sequentially from an ordered set
   always returns the same ident for the same key
   always returns the different ident for different key
   until clear is called.

   rename only identifier defined with [define]
*)
##register rename : string -> string

##register define : string -> void

##register [opacapi] define_rename : string -> string

(**
  break rename properties, restart the generation to the first element of the set
*)
##register clear : -> void

let rename,
    define,
    define_rename,
    clear
  =
  let defined = Hashtbl.create 1024 in
  let ref_ = Hashtbl.create 100024 in
  let gen = IdentGenerator.alphanum_generator ~prefix:"_" in
  let cleared = ref false in

  let define (key_ident:string) =
    Base.Hashtbl.replace defined key_ident ()
  in

  let rename (key_ident:string) =
    assert (not !cleared);
    match !renaming with
    | `no -> key_ident
    | (`fake | `yes) as kind ->
        if Hashtbl.mem defined key_ident
        then (
          match Base.Hashtbl.find_opt ref_ key_ident with
          | Some ident ->
              #<If:JS_RENAMING>
                Printf.printf "BslJsIdent.lookup: %s -> %s\n%!" key_ident ident
              #<End>;
              ident
          | None ->
              let ident =
                match kind with
                | `fake -> Printf.sprintf "rename_%s" key_ident
                | `yes -> gen () in
              #<If:JS_RENAMING>
                Printf.printf "BslJsIdent.renaming: %s -> %s\n%!" key_ident ident
              #<End>;
              OpabslgenMLRuntime.BslClosure.replace_identifier key_ident ident;
              Base.Hashtbl.add ref_ key_ident ident;
              ident
        ) else (
          #<If:JS_RENAMING>
            Printf.printf "BslJsIdent.not_defined: %s\n%!" key_ident
          #<End>;
          Base.Hashtbl.add ref_ key_ident key_ident;
          key_ident
        )
  in

  let define_rename (key_ident:string) =
    define key_ident;
    rename key_ident in

  let clear () =
    #<If:JS_RENAMING> Printf.printf "END OF RENAMING\n%!"#<End>;
    cleared := true;
    Base.Hashtbl.clear ref_;
    Base.Hashtbl.clear defined;

  in rename, define, define_rename, clear

(**
   Tell if the option for the cleaning was activated
*)
##register js_cleaning : -> bool
let js_cleaning () =
  match !cleaning with
  | Some b -> b
  | None -> !cleaning_default_value

(**
   Set the value of --js-cleaning if the user doesn't set its value
   It is set in the init module to [true] in full separation and to
   [false] otherwise
*)
##register [opacapi] set_cleaning_default_value : bool -> void
let set_cleaning_default_value b =
  cleaning_default_value := b

(**
   External mechanism for registering roots.
   <!> Works with identifier before renaming.
*)
##register is_root : string -> bool
let is_root ident =
  Hashtbl.mem root_table ident
