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
open Odoc_info
open Odoc_html
open Module

(* overring the existing html generator
 * to be able to say
 * [@inline doc] in a signature documentation to
 * tell ocamldoc to inline the signature instead of just
 * putting a link to it
 *)
class my_gen =
  object(self)
    inherit Odoc_html.html

    method html_of_module b ?(info=true) ?complete:(_=true) ?(with_link=true) m =
      let complete = true in
      let (html_file, _) = Naming.html_files m.m_name in
      let father = Name.father m.m_name in
      bs b "<pre>";
      bs b ((self#keyword "module")^" ");
      if with_link then
        bp b "<a href=\"%s\">%s</a>" html_file (Name.simple m.m_name)
      else
        bs b (Name.simple m.m_name);
      ( match m.m_kind with
        | Module_functor _ when !Odoc_info.Args.html_short_functors  -> ()
        | _ -> bs b ": "
      );
      let modu =
        match m.m_info with
        | Some {i_custom=i_custom} when
            List.exists (function ("inline",[Odoc_info.Raw "doc"]) -> true | _ -> false) i_custom && with_link -> None
        | _ -> Some m in
      self#html_of_module_kind b father ?modu m.m_kind;
      bs b "</pre>";
      if info then
        (if complete then
            self#html_of_info ~indent: false
         else
            self#html_of_info_first_sentence
        ) b m.m_info

    method no_html _ = ""

    initializer
      tag_functions <- ("inline", self#no_html) :: tag_functions
  end

let my_generator = new my_gen
let _ = Odoc_args.set_doc_generator (Some (my_generator :> Odoc_args.doc_generator))
