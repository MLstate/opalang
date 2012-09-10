/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Runtime of Js language, for server initialization.
 *
 * @author Rudy Sicard
 * @author Mathieu Barbin
 * @author Quentin Bourgerie
**/
import-plugin server
import stdlib.core.{map, set, args, parser, date}


/**
 * {1 JsOptions module}
 *
 * Options for the JsAst stuff (cleaning, renaming and additional roots).
 * Builded by the command line.
 */
@private
JsOptions =
  CommandLine.filter({
    title = "Options for client JavaScript AST"
    init = {cleaning = true renaming = {yes} roots=[]}
    anonymous = []
    parsers = [
      {CommandLine.default_parser with
        names = ["--js-cleaning"]
        description = "Set the cleaning of the client JavaScript AST"
        param_doc = "yes | no"
        on_param(acc) =
          parser
          | "yes" -> {no_params = {acc with cleaning = true}}
          | "no"  -> {no_params = {acc with cleaning = false}}
      },
      {CommandLine.default_parser with
        names = ["--js-renaming"]
        description = "Set the kind of renaming of the client JavaScript AST"
        param_doc = "yes | no | fake"
        on_param(acc) =
          parser
          | "yes"  -> {no_params = {acc with renaming = {yes}}}
          | "no"   -> {no_params = {acc with renaming = {no}}}
          | "fake" -> {no_params = {acc with renaming = {fake}}}
      },
      CommandLine.string(
        ["--js-root"],
        "Add a root declaration for the cleaning of the client JavaScript",
        "jsident"
      )(arg, acc -> {acc with roots = arg +> acc.roots})
    ]
  })

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
    | ~{v} -> v
    | ~{i} -> JsIdent.rename(i)
    | {s=idents} ->
      idents = LowLevelArray.filter_map_to_list((
        ident -> match JsCleaning.check_if_kept(infos,ident) with
                 | {kept} | {no_cleaning} -> {some=JsIdent.rename(ident)}
                 | {client_cleaned} ->
                   do %%bslclosure_set_distant%%(ident, false)
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
    | {td=s}
    | {tu=s}
    | {ru=s}
    | {rd=s} -> s

  @private lexems = List.empty : JsAst.lexems

  /**
   * Accumulate lexems, folding elements of a [JsAst.code_elt].
  **/
  @private fold_elt_lexems(infos : JsCleaning.infos)(elt : JsAst.code_elt, lexems : JsAst.lexems) : JsAst.lexems =
    fold_me(me : JsAst.mini_expr, lexems : list(string)) = [mini_expr_to_string(infos,me)|lexems]
    lexems = fold_content(fold_me, elt.c, lexems)
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


  @expand
  profile(m:string, x) =
    #<Ifstatic:JSAST_PROFILE>
      a = Date.now()
      r = x
      d = Date.between(a, Date.now())
      do jlog("[JSAST] {m} : {Duration.to_formatted_string(Duration.long_time_with_ms_printer, d)}")
      r
    #<Else>
      _ = m
      x
    #<End>

  /**
   * Generate a stringified version of JsAst.code with some processing (alpha-renaming and cleaning)
   * Use by [Client_code] to obtain the js code to send to the client.
   * Should be called only once, on all js_code without omitting any package.
  **/
  js_codes_to_string(server_ast:list(ServerAst.code), js_codes : list(JsAst.code)) : string =
    // first, preventing the renaming from renaming any of the closure serialized by opa2js at toplevel
    // because even if we rename the client identifier and the string in the server closure
    // it is too late, the old string was already read from the server closure
    // this prevents renaming on ident
    do profile(
      "First renaming",
      JsCleaning.Closure.iter_on_all_deps(ident -> ignore(JsIdent.rename(ident)))
    )
    lexems = lexems // empty
    lexems =
      if JsOptions.cleaning
      then
        profile(
          "Cleaning",
          JsCleaning.perform(fold_elt_lexems, js_codes, server_ast, lexems)
        )
      else
        profile(
          "Fake Cleaning",
          // define identifier for the renaming
          iter(elt : JsAst.code_elt) =
            match elt.i with
            | { ~i } | { k=i } ->
              JsIdent.define(i)
          iter(code) = iter_code(iter, code)
          do List.iter(iter, js_codes)
          infos = JsCleaning.empty_infos()
          List.fold(fold_code_lexems(infos), js_codes, lexems)
        )
    do JsIdent.clear() // relax the global map, for the GC
    profile(
      "Serialize",
      lexems_to_string(lexems)
    )
}}

JsMinifier = {{
  minify(code) =
    #<Ifstatic:OPA_BACKEND_QMLJS>
    code
    #<Else>
    %% BslMinJs.minify %%(code)
    #<End>
}}

/**
 * {1 JsIdent module}
 *
 * Binding with the bsl module for managing runtime alpha renaming.
 */
@server_private
JsIdent = {{

  @private defined = Hashtbl.create(1024) : Hashtbl.t(JsAst.ident, void)

  @private ref = Hashtbl.create(1024) : Hashtbl.t(JsAst.ident, JsAst.ident)

  @private js_roots =
    t = Hashtbl.create(1024) : Hashtbl.t(JsAst.ident, void)
    do List.iter(Hashtbl.add(t, _, void), JsOptions.roots)
    t

  @private gen =
    g = String.fresh(0)
    -> "_{g()}"

  /**
   * Used by resolution of directives @js_ident and by the [JsAst.apha_rename]
   * function. The language of fresh idents should have an empty intersection
   * with the language of local idents used by the local renaming. This is for
   * toplevel idents only.  This works by side-effect in a local reference. You
   * can clean the reference with the [JsIdent.clear] function for memory
   * utilization.
  */
  rename(key : JsAst.ident):JsAst.ident =
    match JsOptions.renaming with
    | {no} -> key
    | {fake} as renaming | {yes} as renaming ->
      if Hashtbl.mem(defined, key) then
        Hashtbl.try_find(ref, key) ?
          ident = match renaming with
            | {fake} -> "rename_{key}"
            | {yes}  -> gen()
          do %%BslClosure.replace_identifier%%(key, ident)
          _ = Hashtbl.add(ref, key, ident)
          ident
      else
        do Hashtbl.add(ref, key, key)
        key

  /**
   * We only rename variables that are defined in the code, by calling this
   * function on each toplevel ident, before performing the cleaning.
   */
  define(ident:JsAst.ident):void =
    if not(Hashtbl.mem(defined, ident)) then
      Hashtbl.add(defined, ident, void)

  define_rename(key:JsAst.ident) =
    do define(key)
    rename(key)

  /**
   * Clear the local reference used by the renaming. After a [JsIdent.clear],
   * the renaming restarts.
   */
  clear() =
    do Hashtbl.clear(ref)
    do Hashtbl.clear(defined)
    void

  /**
   * Tell if an identifier is a root.
   */
  is_root(key) = Hashtbl.mem(js_roots, key)

}}

/**
 * {1 JsAst maps and sets}
 */

/**
 * The type of map indexed by [JsAst.ident]
**/
type JsIdentHash.t('a) = Hashtbl.t(JsAst.ident, 'a)
type ServerIdentHash.t('a) = Hashtbl.t(ServerAst.ident, 'a)

/**
 * The type of sets indexed by [JsAst.ident]
**/
type JsIdentSet.t = ordered_set(JsAst.ident, String.order)

/**
 * A set indexed by {!JsAst.ident}
**/
JsIdentSet = StringSet

@opacapi JsIdent_define_rename = JsIdent.define_rename
