/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * {2 Module OpaTypeCast}
 * This module provides some functions for manipulate a value with the right type
 * using dynamic verification.
 * It should be used instead of Magic.id when you are manipulating type in conjunction of typeof until this mecanism is made type safe
**/

OpaTypeCast = {{

  int(v) =
    match @typeof(v)
    | {TyConst={TyInt}} -> Magic.id(v):int
    | ty                -> error("type error OpaTypeCast.int : {OpaType.to_string(ty)}")

  string(v) =
    match @typeof(v)
    | {TyConst={TyString}} -> Magic.id(v):string
    | ty                   -> error("type error OpaTypeCast.string : {OpaType.to_string(ty)}")

  float(v) =
    match @typeof(v)
    | {TyConst={TyFloat}} -> Magic.id(v):float
    | ty                  -> error("type error OpaTypeCast.float : {OpaType.to_string(ty)}")

  bool(v) =
    match @typeof(v)
    | {TyName_ident="bool" ...} -> Magic.id(v):bool
    | ty                        -> error("type error OpaTypeCast.bool : {OpaType.to_string(ty)}")

  parser(v)=
    match @typeof(v)
    | {TyName_ident="Parser.general_parser"  ...} -> Magic.id(v):Parser.general_parser
    | ty                  -> error("type error OpaTypeCast.parser : {OpaType.to_string(ty)}")

 }}
