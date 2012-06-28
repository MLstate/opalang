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
(* CF mli *)

(* depends *)
module Format = Base.Format

(* alias *)
module ByPassMap = Flat_Bsl.FlatBSL.ByPassMap
module Implementation = Flat_Bsl.FlatBSL.Implementation
module ByPass = Flat_Bsl.FlatBSL.ByPass
module FCons = Flat_Common.FCons

let compile ~context ~bymap ?restriction key =
  let skey = BslKey.to_string key in
  let compiler_repr =
    match ByPassMap.find_opt bymap key with
    | None ->
        QmlError.error context (
          "unknown primitive @{<bright>%s@}"
        )
          skey
    | Some bypass -> (
        match ByPass.implementation bypass ~lang:BslLanguage.ml with
        | Some ((Implementation.Compiled compiled) as implementation) ->
            let bsltags = Implementation.bsltags implementation in
            (* Check if this bypass is authorized *)
            let _ =
              let authorized = BslTags.authorized_bypass ~restriction bsltags in
              if not authorized
                then
                  QmlError.error context (
                    "bypass @{<bright>%s@} is not authorized in this context@\n"^^
                      "there are some restrictions : << %s >>"
                  )
                    skey (BslTags.string_of_restricted bsltags.BslTags.restricted)
              else ()
            in (
              (* Check if this is in the segfault *)
              match Flat_Bsl.is_segfault key (ByPassMap.ml_ctrans_env bymap) with
              | None ->
                  let repr = Implementation.CompiledFunction.compiler_repr compiled in
                  if Implementation.CompiledFunction.is_transtype compiled
                  then Printf.sprintf "%s.%s" (Qml2ocaml.bsl_init_module ()) repr
                  else repr
              | Some message ->
                  (* TODO Let the possibility to have dirtags here (ctrans_env maybe contains a BSL.ByPassMap.t) *)
                  QmlError.error context (
                    "%s"
                  )
                    message
            )
        | _ ->
            QmlError.error context (
              "The primitive @{<bright>%s@} has been found@\n"^^
               "but there is no ocaml implementation@\n"^^
                "%a"
            ) skey
              (Format.pp_list "@\n" Implementation.pp)
              (ByPass.all_implementations bypass)
      )
  in
  Ocaml.make_Var compiler_repr
