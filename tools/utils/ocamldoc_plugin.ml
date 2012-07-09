(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
