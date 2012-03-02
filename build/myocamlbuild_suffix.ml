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
      (* -- Preprocessing -- *)
      let extern_on_windows = false in

      (* pp_script *)

      let mlstate_debug, mlstate_nodebug, pa_ulex =
        if windows_mode then
          let pp_script s=
            let win_exists s = try ignore(Unix.stat s);true with _ -> false in (* under cygwin, Unix.*link* does not work *)
            let location = (Sys.getenv "LIBQML")^"/preprocessing/" in
            location ^ s ^ ".bat"
          in
          (* pre-defined wrapper scripts to workaround the "no space in ppdebug string" bug on windows *)
          (* (they do the same as the commands below) *)
          P (pp_script "mlstate_debug"),
          P (pp_script "mlstate_nodebug"),
          P (pp_script "pa_ulex")
        else
          S [ P "perl"; P (".."/"utils"/"ppdebug.pl") ],
          S [ P "perl"; P (".."/"utils"/"ppdebug.pl"); A "-r" ],
          S [ P "bash"; P (".."/"utils"/"camlp4o-ulex.sh") ; P "pa_ulex.cma"; P "pr_o.cmo" ]
      in

      flag ["ocaml"; "pp"; "with_mlstate_debug"]
        (if List.mem "release" !Options.tags then mlstate_nodebug else mlstate_debug);

      (** Ulex *)

      flag ["ocaml"; "pp"; "use_ulex_pp"] pa_ulex;

    (* This additional rule needs to be after the rule for plugins in
       opa. Hence its presence here *)

    (* This rule can probably be improved *)
    rule "ocaml shared object: cmx -> cmxs"
      ~dep: "%.cmx"
      ~prod: "%.cmxs"
      (fun env build ->
         Cmd(S[!Options.ocamlopt;
               T(tags_of_pathname (env "%.cmx"));T(tags_of_pathname (env "%.cmxs"));
               A"-shared";A"-o";P(env "%.cmxs");P(env "%.cmx")]));

    (try set_tool ~internal:false "trx" (Sys.getenv "TRX_OVERRIDE")
     with Not_found -> ());

    ()
  | _ -> ()
end
