(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

      (* -- Preprocessing -- *)
      let extern_on_windows = false in

      (* pp_script *)

      let mlstate_debug, mlstate_nodebug, pa_ulex =
        S [ P "perl"; P (Pathname.pwd/opalang_prefix/"tools"/"utils"/"ppdebug.pl")],
        S [ P "perl"; P (Pathname.pwd/opalang_prefix/"tools"/"utils"/"ppdebug.pl"); A "-r" ],
	S [ P Config.camlp4o;
	    (match Config.Libdir.ulex with
	     | Some dir -> S [A "-I"; P dir]
	     | None -> N);
	    P "pa_ulex.cma"; P "pr_o.cmo" ]
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
