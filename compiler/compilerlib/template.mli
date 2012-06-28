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

(**
   Guidelines for organizing modules in the framework.

   @author Esther Baruk
*)

(**
   This module explains the guidelines to follow for organizing your code.
   It follows itself the guidelines !
*)

(** {6 Guidelines for interfaces} *)

(**
   {ul {- If your comment takes several lines you must put a blank line before
     and after it, this way :
   {[(**
   My comment
   on several
   lines
*)]}}
   {- If you are the author of the module, you must write your name
     in an author tag}
   {- You must export only the functions that need to be.}}
*)



(** {6 Documentation about module} *)

(**
   Please follow these guidelines :

   {[(**
   Module Foo, with a short explanation about Foo

   \@author Jane Doe
*)]}

   {[(**
   Then comes a longer explanation about module Foo.
   Please take time to provide examples and documentation.
*)]}
*)


(** {6 Error report} *)

(**
   If your error must stay internal and should not be caught, don't
   write anything in this section.
   Else, you must export the informations about reporting errors
   (i.e. type error, exception and printer, as said in the implementation)
*)

type error
exception Exception of error
val pp_error : error LangPrint.pprinter


(** {6 Signature of module} *)

(** Provide documentation about this function, how to use it, etc. *)
val function2 : unit -> unit
