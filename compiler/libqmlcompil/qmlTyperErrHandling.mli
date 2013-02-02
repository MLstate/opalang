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



(* ************************************************************************** *)
(** {b Descr}: Prints an error message describing what triggered the exception
    passed as argument. Expected exceptions are [QmlTyperException.Exception]
    and [QmlTypes.Exception (QmlTypes.TyperError _)] in order to have a really
    informative message. Any other exception will be printed as a raw exception
    description via [PrintExc.to_string].
    Prints an ending \n.                                                      *)
(* ************************************************************************** *)
val pp_report_from_typer_exception :
  'a QmlAnnotMap.gen_annotmap -> Format.formatter -> exn -> unit

val typechecking_exception_handler : 'a QmlTypes.public_env -> exn -> 'b
