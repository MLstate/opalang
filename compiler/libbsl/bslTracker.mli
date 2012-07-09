(*
    Copyright © 2011, 2012 MLstate

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
(**
   Some utils related to Pass Tracking for Bypasses.
   @author Quentin Bourgerie
*)

(**
   This module defines some utils for branching compilers working on QmlAst with
   the pass tracking system developed in passlib, in [PassHandler] and [PassTracker]

   In particular, it defines, given a [QmlAst.code] extractor some :
   + [printers]
   + [trackers]

   Any language should implement a module named [LangTracker], [LangCheck], etc...

   @see "passlib/passdesign.ml" for a global overview of the Pass System
*)

(** {6 Printers} *)

(** Regroup some of common printer of the module [Printer] in one list .
    The ignored argument is for an optimal branching with PassHandler.

    + bymap
*)
val printers : ('env -> BslLib.env_bsl) -> 'opt -> (PassHandler.printer_id * 'env PassHandler.printer) list

module Printer :
sig
  (**
     Regular bymap printer:
     print all keys of bypasses
  *)
  val bymap : BslLib.env_bsl PassHandler.printer

end
