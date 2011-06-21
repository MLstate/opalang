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
   Provides all check on QML for pre/post conditions.

   @author Esther Baruk
   @author Mathieu Barbin
   @author Quentin Bourgerie
 *)

(** {6 Guidelines} *)
(**
   If you write a new implementation of a checker :

   + USE [QmlError.check_fail] or [QmlError.scheck_fail]
   + DO NOT USE [scond_violation], [cond_violation], [i_error] and [i_serror],
   those are for passes only (not checkers)
   + All checks in this module should use the internal error system
   with warning related to conditions.
   + There is no interaction between [OManager] and this module.
   Function there should not perform direct call to OManager, but
   to the overlayer defined in QmlError.
   + You should never use :[flush_errors] like function.
   + please, order modules alphabetically,
   + and functions inside module.
   + You should necessarily export the id of conditions.

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
   The type of all checkers. Take an extractor and compute some checks in it.
   In case of errors, the check should use functions from [QmlError] only.

   @see "WarningClass" for the corresponding definitions of warning classes
*)
type ('env, 'a) checker = ('env -> 'a)  -> 'env PassHandler.cond

(** Checks related to [QmlAst.annotation] (types, pos, unicity, etc..) *)
module Annot :
sig
  (**
     The id of all cond related to annot
  *)
  val id : PassHandler.cond_id

  (** {6 Find} *)

  (**
     Given an annotmap and a code, checks if every annotation is present in the annotmap.
     - Condition name : ["cond.annot.find"]
     - Warning class : [cond_annot_find]
  *)
  val find : ('env , (QmlAst.annotmap * QmlAst.code)) checker
  val find_id : PassHandler.cond_id

  (** {6 Unicity} *)

  (**
     Checks that annotation are unique in a given code
     - Condition name : ["cond.annot.unicity"]
     - Warning class : [cond_annot_unicity]
  *)
  val unicity : ('env, QmlAst.code) checker
  val unicity_id : PassHandler.cond_id
end

(** Checks related to [QmlAst.Bypass] *)
module Bypass :
sig
  (**
     The id of all cond related to bypass
  *)
  val id : PassHandler.cond_id

  (** {6 Applied } *)

  (**
     Checks that every bypass is totally applied with respect to its
     arity and to the number of arguments which it is applied on.
     Example :
       {[ let x = %%plus%% 2 3 ]} is ok.
       {[ let x = %%minus%% 4 6 7 ]} is not ok.
     - Condition name : ["cond.bypass.applied"]
     - Warning class : [cond_bypass_applied]
  *)
  val applied : ('env, (QmlTypes.bypass_typer * QmlAst.code)) checker
  val applied_id : PassHandler.cond_id

  (** {6 Expanded} *)

  (**
     Checks that every bypass is correctly protected with the directive
     [`expanded_bypass] and that the expression inside has the correct
     form, which is :
     {[fun x1 x2 ... xn -> %%bypass%% x1 x2 ... xn]} for a bypass of arity [n]

     If the type is unknown (the bypass typer returns [None]),
     simply checks the form.
     - Condition name : ["cond.bypass.expanded"]
     - Warning class : [cond_bypass_expanded]
  *)
  val expanded : ('env, (QmlTypes.bypass_typer * QmlAst.code)) checker
  val expanded_id : PassHandler.cond_id

  (**
     Perform the same check as 'expanded' on bypass which are expanded,
     but do not crash if some bypass are not yet expanded.
     (i.e. not protected with [`expanded_bypass] directive).

     - Condition name : ["cond.bypass.well-formed"]
     - Warning class : [cond_bypass_well_formed]
  *)
  val well_formed : ('env, (QmlTypes.bypass_typer * QmlAst.code)) checker
  val well_formed_id : PassHandler.cond_id
end

(** General checks on the code *)
module Code :
sig
  (**
     The id of all cond related to code
  *)
  val id : PassHandler.cond_id

  (** {6 Recursives Values} *)

  (** the core checking function
      undot -> tolerate `module directive
      second bool arg -> do checking instead of making error
      called by both S2 and S3
      CLEANING S2 : DELETE THE PREVIOUS LINE
  *)
  val check_valrec : undot:bool -> bool -> (QmlAst.annotmap * QmlAst.code) -> unit


  (** {6 Kind of code-elt } *)

  (**
     The type to express what kind of code_elt are still in the code.
     A code elt tagged 'false' should no more appear in the code.
  *)
  type contents_code_elt = {
    c_Database : bool ;
    c_NewDbValue : bool ;
    c_NewType : bool ;
    c_NewVal : bool ;
    c_NewValRec : bool ;
  }

  (** check the kind of code_elt in the code *)
  val contents_all : contents_code_elt
  val contents_id : PassHandler.cond_id
  val contents : contents_code_elt -> ('env, (QmlAst.annotmap * QmlAst.code)) checker

  (** Checks that the defininition of recursives values are
      avialable.
          - Condition name : ["cond.code.valrec"]
          - Warning class : [cond_code_valrec]
        *)
  val valrec : ('env, (QmlAst.annotmap * QmlAst.code)) checker
  val valrec_id : PassHandler.cond_id

  (** Like [valrec] but bool argument indicates if the checker make
      condition error or user error. [true] indicates condition
      error. *)
  val valrec_user : undot:bool -> ('env, (QmlAst.annotmap * QmlAst.code)) checker

end
