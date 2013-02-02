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
    Provides all checks on Opa for pre/post conditions.

    @author Esther Baruk
*)

(** {6 Guidelines} *)

(**
    If you wish to write a new implementation of a checker :
    + Use [OpaError.check_fail] or [OpaError.scheck_fail] functions
    + Do not use [scond_violation], [cond_violation], [i_error] or [i_serror],
    those are for passes only (not checkers).
    + All checks in this module should use the internal error system with the
    warnings related to conditions.
    + There is no interaction between [OManager] and this module
    Function there should not perform direct calls to [OManager], but to the
    overlayer defined in OpaError.
    + You should never use [flush_error] like a function
    + please keep alphabetical order of modules and functions inside modules.
    + You should necessarly export the id of conditions.

    Follow this schema :

    in WarningClass :
    {[
    val cond_category_subcategory_name : wclass
    ]}

    There :
    {[
    module Category =
    struct
      module Subcategory =
      struct
        let name_id = PassHandler.define_cond WarningClass.cond_category_subcategory_name
        let name = .... PassHandler.make_condition name_id ...
      end
    end
    ]}

    For more informations, you can refer to [passlib/passdesign.ml] which contains a reference
    implementation for this module.
*)

(**
   The type of all checkers. Take an extractor and computes some checks in it.
   In case of errors, the check should use functions from [OpaError] only.

   @see "WarningClass" for the corresponding definitions of warning classes
*)

type ('env, 'a) checker = ('env -> 'a) -> 'env PassHandler.cond

(** Checks related to identifiers *)
module Ident :
sig
  (** Checks whether an identifier is unbound or not

      + Condition name : ["cond.ident.unbound"]
      + Warning class : [cond_ident_unbound]
  *)
  (* TODO *)
  val unbound : ('env, (SurfaceAst.nonuid, SurfaceAst.all_directives) SurfaceAst.code) checker
end
