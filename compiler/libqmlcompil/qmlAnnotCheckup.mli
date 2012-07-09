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
(* THIS FILE IS NOW DEPRECATED:
   Todo: follow guidelines for QmlCheck,
   and merge with the module QmlCheck.Annot
*)

(**
   Deprecated module for Checks related to annotation, in QmlAst.

   This module provides high level function which bring a readable log-output to help the debug of annotations
   passing along a convertion between the old and the new world.
   It verify, walking into the ast, that every annotation is well defined in the given map

   Where it is used :

   with libconvert and the introduction of several passes in opa which are directly high dependant,
   annotations must be very stricly wonderfully well provided and not lost at all in any step of converting

   What passes exactly ?

   new-typer / force-db3 / new-slicer / new-back-end

   @author Mathieu Barbin
   @author Francois-Régis Sinot
   @deprecated See QmlCheck which is more general, and branched to the Track system.
*)

type options =
    {
      dump_found : bool ; (** produce on stderr all the logs of found annots if true *)
      dump_not_found : bool ; (** same for not found annots *)
      pat_not_found_are_ok : bool ; (** special case : in old backend of opa, annotation on pattern are not used
                                        and not provided by libconvert -- so, this option says that it must
                                        not be taken as an error that thing are like that *)
      freshness_only : bool ; (** only checks that all integers are distinct (disables the other tests) *)
      dump_position : bool ; (** dumps positions *)
    }

(** Default values for options, there to be used like : [{ QmlAnnotCheckup.default with my_option = ... }]
    default values are :
    {[
    dump_found : false ;
    dump_not_found : true ;
    pat_not_found_are_ok : false ;
    freshness_only = false ;
    dump_position = false ;
    ]}
*)
val default : options

(** The generic type for a checker.
    The returned value is : EVERYTHING IS OK ( + do something depending on options ) *)
type 'a annotation_checker = ?options:options -> QmlAst.annotmap -> 'a -> bool

val expr     : QmlAst.expr annotation_checker
val pat      : QmlAst.pat annotation_checker
val code_elt : QmlAst.code_elt annotation_checker
val code     : QmlAst.code annotation_checker
