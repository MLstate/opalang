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
(*
   @author Fran�ois Pessaux
*)



let (reset_type_exception, enrich_type_exception, type_exception) =
  let seed_type = ref None in
  (
    (* ********************************************************************** *)
    (** {b Descr}: Effective body of the function [reset_type_exception].
        See .mli file for documentation.
        {b Visibility}: Exported outside this module.                         *)
    (* ********************************************************************** *)
    (fun () ->
       (* Exceptions are an opened sum. Initially, it contains no case. We
          create a local type to accumulate cases encountered while
          typechecking the  program. *)
       let opened_column =
         { W_Algebra.ct_value =
             ([], (W_CoreTypes.column_ending_variable ())) } in
       let ty_desc = W_Algebra.SType_sum_of_records opened_column in
       let ty =
         { W_Algebra.sty_desc = ty_desc ;
           W_Algebra.sty_link = None ;
           W_Algebra.sty_mark = W_Algebra.TM_not_seen } in
       W_TypeInfo.add_expn_object ty_desc;
       seed_type := Some ty),



    (* ********************************************************************** *)
    (** {b Descr}: Effective body of the function [enrich_type_exception].
        See .mli file for documentation.
        {b Visibility}: Exported outside this module.                         *)
    (* ********************************************************************** *)
    (fun env ty ->
       match !seed_type with
       | None -> assert false
       | Some exn_ty ->
          let exn_ty = W_CoreTypes.simple_type_repr exn_ty in
          W_TypeInfo.addrec_expn_object exn_ty.W_Algebra.sty_desc ;
          W_Unify.unify_simple_type env exn_ty ty),



    (* ********************************************************************** *)
    (** {b Descr}: Effective body of the function [type_exception].
        See .mli file for documentation.
        {b Visibility}: Exported outside this module.                         *)
    (* ********************************************************************** *)
    (fun () ->
       (* Each time a type exception is created, we give the same physical
          type, hence known cases of exceptions will be accumulated in this
          unique type. *)
       match !seed_type with
       | None -> assert false
       | Some exn_ty ->
          let exn_ty = W_CoreTypes.simple_type_repr exn_ty in
          W_TypeInfo.addrec_expn_object exn_ty.W_Algebra.sty_desc ;
          exn_ty)
  )
