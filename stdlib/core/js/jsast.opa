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
 * Runtime of Js language, for server initialization.
 *
 * @author Rudy Sicard
 * @author Mathieu Barbin
**/

import stdlib.core.{map,set}

/**
 * {1 JsAst module}
 *
 * Run-time manipulation of js ast.
 */

@server_private JsAst = {{

  /**
   * {2 Iterators}
  **/

  /**
   * Using this iterators rather than the iterator corresponding to the current implementation
   * of the ast assures that the code won't need to adapt to ast changes.
  **/

  fold_content = ( LowLevelArray.fold : (JsAst.mini_expr, 'acc -> 'acc), JsAst.content, 'acc -> 'acc )
  fold_code = ( LowLevelArray.fold : (JsAst.code_elt, 'acc -> 'acc), JsAst.code, 'acc -> 'acc )
  iter_code = ( LowLevelArray.iter : (JsAst.code_elt -> void), JsAst.code -> void )

  // direct dependency on the bsl (BslClosure, more precisely)
  set_distant = "set_distant"

  @private mini_expr_to_string(infos,e:JsAst.mini_expr) =
    match e with
    | ~{verbatim} -> verbatim
    | ~{ident} -> JsIdent.rename(ident)
    | {set_distant=idents} ->
      idents = LowLevelArray.filter_map_to_list((
        ident -> match JsCleaning.check_if_kept(infos,ident) with
                 | {kept} | {no_cleaning} -> {some=JsIdent.rename(ident)}
                 | {client_cleaned} ->
                   do %%bslclosure_set_distant_false%%(ident)
                   {none}
                 | {server_cleaned} ->
                   // FIXME? could empty the rpc hashtbl
                   // we must not let the client think that it can serialize this ident
                   // because it was considered dead code during clean up
                   // and so it may call dead code of the client (which won't work of course)
                   {none}),
        idents)
      match idents with
      | [] -> ""
      | [_|_] ->
        ident = JsIdent.rename(set_distant)
        args = List.to_string_using("(\"","\")",",",idents)
        ident ^ args
      end
    | {type_def=s}
    | {type_use=s}
    | {rpc_use=s}
    | {rpc_def=s} -> s

  @private lexems = List.empty : JsAst.lexems

  /**
   * Accumulate lexems, folding elements of a [JsAst.code_elt].
  **/
  @private fold_elt_lexems(infos : JsCleaning.infos)(elt : JsAst.code_elt, lexems : JsAst.lexems) : JsAst.lexems =
    fold_me(me : JsAst.mini_expr, lexems : list(string)) = [mini_expr_to_string(infos,me)|lexems]
    lexems = fold_content(fold_me, elt.content, lexems)
    lexems

  /**
   * Accumulate lexems, folding elements of a [JsAst.code_elt].
  **/
  @private fold_code_lexems(infos : JsCleaning.infos)(code : JsAst.code, lexems : JsAst.lexems) : JsAst.lexems =
    lexems = fold_code(fold_elt_lexems(infos), code, lexems)
    lexems

  /**
   * stringify all accumulated lexems
  **/
  @private lexems_to_string(lexems : JsAst.lexems) =
    // FIXME use String.rev_concat instead
    lexems = List.rev(lexems)
    String.concat("", lexems)

  /**
   * Generate a stringified version of JsAst.code with some processing (alpha-renaming and cleaning)
   * Use by [Client_code] to obtain the js code to send to the client.
   * Should be called only once, on all js_code without omitting any package.
  **/
  js_codes_to_string(server_ast:list(ServerAst.code), js_codes : list(JsAst.code)) : string =
    // first, preventing the renaming from renaming any of the closure serialized by opa2js at toplevel
    // because even if we rename the client identifier and the string in the server closure
    // it is too late, the old string was already read from the server closure
    do JsCleaning.Closure.iter_on_all_deps(ident -> ignore(JsIdent.rename(ident))) // this prevents renaming on ident
    lexems = lexems // empty
    lexems =
      if JsIdent.cleaning()
      then
        JsCleaning.perform(fold_elt_lexems, js_codes, server_ast, lexems)
      else
        // define identifier for the renaming
        iter(elt : JsAst.code_elt) =
          match elt.ident with
          | { ~ident ; ... } ->
            JsIdent.define(ident)
          | _ -> void
        iter(code) = iter_code(iter, code)
        do List.iter(iter, js_codes)
        infos = JsCleaning.empty_infos
        List.fold(fold_code_lexems(infos), js_codes, lexems)
    do JsIdent.clear() // relax the global map, for the GC
    lexems_to_string(lexems)
}}

/**
 * {1 JsIdent module}
 *
 * Binding with the bsl module for managing runtime alpha renaming.
**/

JsIdent = {{

  /**
   * Used by resolution of directives @js_ident
   * and by the [JsAst.apha_rename] function.
   * The language of fresh idents should have an empty intersection with the language of local idents
   * used by the local renaming. This is for toplevel idents only.
   * This works by side-effect in a local reference. You can clean the reference with the [JsIdent.clear] function
   * for memory utilization.
  **/
  // Local idents : [a-zA-Z][a-zA-Z0-9]*
  // Toplevel idents : _local-ident
  rename = %%bslJsIdent.rename%% : JsAst.ident -> JsAst.ident

  /**
   * We only rename variables that are defined in the code, by calling this bypass on each toplevel ident,
   * before performing the cleaning.
  **/
  define = %%bslJsIdent.define%% : JsAst.ident -> void

  /**
   * Clear the local reference used by the renaming. After a [JsIdent.clear], the renaming restarts.
  **/
  clear = %%bslJsIdent.clear%% : -> void

  /**
   * Tell if the option for the cleaning was activated
  **/
  cleaning = %%bslJsIdent.js_cleaning%% : -> bool

  /**
   * Tell if an identifier is a root.
  **/
  is_root = %%bslJsIdent.is_root%% : string -> bool
}}

/**
 * {1 JsAst maps and sets}
 */

/**
 * The type of map indexed by [JsAst.ident]
**/
type JsIdentMap.t('a) = ordered_map(JsAst.ident, 'a, String.order)
type ServerIdentMap.t('a) = ordered_map(ServerAst.ident, 'a, String.order)

/**
 * A map indexed by {!JsAst.ident}
**/
JsIdentMap = StringMap
ServerIdentMap = StringMap

/**
 * The type of sets indexed by [JsAst.ident]
**/
type JsIdentSet.t = ordered_set(JsAst.ident, String.order)

/**
 * A set indexed by {!JsAst.ident}
**/
JsIdentSet = StringSet
