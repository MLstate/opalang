/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * {1 About this module}
 *
 * This module provides some functions for manipulate a value with the right
 * type using dynamic verification.
 *
 * It should be used instead of Magic.id when you are manipulating type in conjunction of typeof until this mechanism is made type safe
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
**/

/**
 * {1 Interface}
 */

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
