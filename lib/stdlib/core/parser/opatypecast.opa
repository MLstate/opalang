/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
