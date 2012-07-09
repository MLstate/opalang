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
(**
   Some utils related to Pass Tracking for Opa.
*)

(** This module defines some utils for branching compilers working on
    OpaAst (SurfaceAst) with the pass tracking system developed in
    passlib, in [PassHandler] and [PassTracker]

    In particular, it defines, given a [SurfaceAst.code] extractor some :
    + [printers]
    + [trackers]

    Any language should implement a module named [LangTracker],
    [LangCheck], etc...

    @see "passlib/passdesign.ml" for a global overview of the Pass
    System
*)
(** {6 Printers} *)

(** Regroup some of common printer of the module [Printer] in one list .
    The ignored argument is for an optimal branching with PassHandler.

    + code

    TODO: some more flexibility in passhandler options so that we can
    export in this list douzen of specific printers without risking
    that they will all be printed with a --print-all.
*)
val printers_uids : ('env -> (SurfaceAst.uids, [< SurfaceAst.all_directives ]) SurfaceAst.code) -> 'opt -> (PassHandler.printer_id * 'env PassHandler.printer) list

val printers_nonuid : ('env -> (SurfaceAst.nonuid, [< SurfaceAst.all_directives ]) SurfaceAst.code) -> 'opt -> (PassHandler.printer_id * 'env PassHandler.printer) list

module Printer :
sig
  (**
     Regular ast printer:
     print the full code using the Pretty Printer of the language
     and insering multiple new lines so that meld can better do
     the correspondancy between values.
  *)
  val code_uids : (SurfaceAst.uids, [< SurfaceAst.all_directives ]) SurfaceAst.code PassHandler.printer

  val code_nonuid : (SurfaceAst.nonuid, [< SurfaceAst.all_directives ]) SurfaceAst.code PassHandler.printer

  val size : ('ident, [< SurfaceAst.all_directives ]) SurfaceAst.code PassHandler.printer
end

(** {6 Trackers} *)

(** Regroup all the trackers of the module [Tracker] in one list .
    The ignored argument is for an optimal branching with PassHandler.*)
val trackers : ('env -> ('a, 'b) SurfaceAst.code) -> 'opt -> (PassHandler.tracker_id * 'env PassHandler.tracker) list

module Tracker : sig end
