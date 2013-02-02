/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Runtime client dead code elimination.
 *
 * @author Mathieu Barbin
 * @author Quentin Bourgerie (speedup, partial imperative version)
**/


/**
 * {1 About this module}
 *
 * The cleaning algorithm is a mark and sweep, based on a graph accessibility
 * from root nodes.
 * As we prefer that the stdlib does not depend on fgraph, there is an ad-hoc implementation,
 * without graphs construction, dealing with specialized structures.
 *
 * We have several [JsAst.code] registered from several packages. The cleaning is global.
 * That's why environment of cleaning (infos) and the cleaning itself should be performed
 * on the full [list(JsAst.code)], without omitting any package.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
**/

/**
 * {1 Types defined in this module}
 */

/**
 * Accessing [code_elt] by their names.
**/
type JsCleaning.elements = JsIdentHash.t(JsAst.code_elt)

/**
 * The stack of [code_elt] to inspect. The fact that this is a stack is not required,
 * the order of inspection is not important. There are already mem checks performed
 * before adding identifier into the marked set, so it is more efficient if we
 * use a simple structure without ordering (a list there is preferable than e.g. a set)
**/
type JsCleaning.stack = list(JsAst.ident)

/**
 * Be sure an elt tagged with an uniq code is seen only once.
**/
type JsCleaning.unicity = stringset

/**
 * The type of information used for the cleaning.
**/
type JsCleaning.infos = {
  elements : JsCleaning.elements
  server_elements : JsIdentHash.t(ServerAst.code_elt)
  types_server : Hashtbl.t(string, ServerAst.ident)
  types_client : Hashtbl.t(string, JsAst.ident)
  rpc : Hashtbl.t(string, JsAst.ident) // or ServerAst.ident actually
  client_roots : list(JsAst.code_elt);
  server_roots : list(ServerAst.code_elt);
  unicity : JsCleaning.unicity
  correspondence : JsIdentHash.t(ServerAst.ident)
    // maps client ident to the same ident in the server
    // (if any)
}

/**
 * The type use by the marking function
**/
type JsCleaning.marked = JsIdentSet.t

/**
 * {1 Interface}
 */

/**
 * Module for dead code elimination
**/
@server_private JsCleaning = {{

  /**
    This module deals with the cleaning of closures
    which is quite complicated, because of opa2js
    that inserts indirect references to closures
    (ie closure identifiers) in some verbatim code :/
  */
  Closure = {{

    @private table = ServerReference.create(StringMap.empty:stringmap(Server.reference(list(string))))
    @private after_js_renaming = ServerReference.create({false}:bool)

    deps_of_var_for_opa2js(var:JsAst.ident) : option(Server.reference(list(JsAst.ident))) =
      if ServerReference.get(after_js_renaming) then
        {none}
      else
        match StringMap.get(var,ServerReference.get(table)) with
        | {none} ->
          l = ServerReference.create([])
          do ServerReference.set(table,StringMap.add(var,l,ServerReference.get(table)))
          {some = l}
        | v -> v

    deps_of_var_for_cleaning(var:JsAst.ident) : list(JsAst.ident) =
      match StringMap.get(var,ServerReference.get(table)) with
      | {none} -> []
      | {some = l} -> ServerReference.get(l)

    iter_on_all_deps(f:JsAst.ident -> void) : void =
      table = ServerReference.get(table)
      StringMap.iter((_,ref ->
        deps = ServerReference.get(ref)
        List.iter(f,deps)
      ),table)

    end_of_renaming() : void =
      /* emptying the table, because it is useless to
       * keep it in ram */
      do ServerReference.set(table,StringMap.empty)
      ServerReference.set(after_js_renaming,{true})
  }}

  /**
   * The empty infos
  **/
  empty_infos() = {
    elements = Hashtbl.create(111)
    server_elements = Hashtbl.create(111)
    types_server = Hashtbl.create(111)
    types_client = Hashtbl.create(111)
    rpc = Hashtbl.create(111)
    server_roots = []
    client_roots = []
    unicity = StringSet.empty
    correspondence = Hashtbl.create(111)
  } : JsCleaning.infos

  /**
   * Identifiers declared as root.
  **/
  @private is_root_ident(ident : JsAst.ident) =
    // do jlog("is_root_ident : {ident}")
    JsIdent.is_root(ident)

  @private add_type_to_stack(infos:JsCleaning.infos, stack, type_use) =
    stack =
      match Hashtbl.try_find(infos.types_client, type_use) with
      | {none} ->
        //do jlog("JsAst Cannot find the client identifier defining type {type_use}")
        stack
      | {some = ident} -> [ident|stack]
      end
    match Hashtbl.try_find(infos.types_server, type_use) with
    | {none} ->
      //do jlog("JsAst Cannot find the server identifier defining type {type_use}")
      stack
    | {some = ident} -> [ident|stack]
    end

  @private add_rpc_to_stack(infos, stack, rpc_use) =
    match Hashtbl.try_find(infos.rpc, rpc_use) with
    | {none} -> error("Cannot find the identifier defining rpc {rpc_use}")
    | {some = ident} -> [ident|stack]
    end

  /**
   * Add in the stack all ident contained in the [code_elt]
  **/
  @private add_stack(infos:JsCleaning.infos, code_elt:JsAst.code_elt, stack : JsCleaning.stack) =
    fold(me, stack) =
      match me : JsAst.mini_expr with
      | { ~i } ->
        //do jlog("CLIENT: {code_elt.ident} is using ident {ident}")
        [i|stack]
      | { td = _ }
      | { rd = _ }
      | { v = _ } -> stack
      | { s = _ } -> [JsAst.set_distant|stack]
      | ~{ tu } ->
         // do jlog("CLIENT: {code_elt.ident} is using type {type_use}")
         add_type_to_stack(infos, stack, tu)
      | ~{ ru } ->
        //do jlog("CLIENT: {code_elt.ident} is using rpc {rpc_use}")
        add_rpc_to_stack(infos, stack, ru)
      end
    stack = JsAst.fold_content(fold, code_elt.c, stack)
    // extra deps that opa2js adds when serializing
    // @insert_server_value
    stack =
      match code_elt.i with
      | {k=_} -> stack
      | {~i} ->
        closure_keys = Closure.deps_of_var_for_cleaning(i)
        //do List.iter(closure_key -> jlog("CLIENT: {ident} is using ident {closure_key}"), closure_keys)
        List.append(closure_keys, stack)
    stack

  /**
   * Say if a code_elt is a root, which means the start point of the accessibility.
  **/
  @private is_root(code_elt : JsAst.code_elt) =
    is_root =
      ServerReference.get(code_elt.r)
      || (
        match code_elt.i with
        | {~i}
        | { k = i } ->
          is_root_ident(i)
      )
    do if is_root then ServerReference.set(code_elt.r, true)
    is_root

  /**
   * Say if a code_elt is a root, which means the start point of the accessibility.
  **/
  @private is_root_server(code_elt : ServerAst.code_elt) =
    is_root =
      ServerReference.get(code_elt.r)
      || (
        match code_elt.i with
        | {none} -> true
        | {some = ident} -> is_root_ident(ident)
      )
    do if is_root then ServerReference.set(code_elt.r, true)
    is_root

  check_if_kept(infos:JsCleaning.infos,ident) : {no_cleaning} / {kept} / {server_cleaned} / {client_cleaned} =
    match Hashtbl.try_find(infos.elements, ident) with
    | {none} -> {no_cleaning} /* cleaning is not activated */
    | {some=elt} ->
       if ServerReference.get(elt.r) then
         match Hashtbl.try_find(infos.correspondence, ident) with
         | {none} ->
           // useful code but no server counterpart
           {kept}
         | {some=server_ident} ->
           // code on both sides
           match Hashtbl.try_find(infos.server_elements, server_ident) with
           | {none} -> @fail(ident)
           | {some = ~{r ...}} ->
             if ServerReference.get(r) then
               // keeping the client and the server
               {kept}
             else
               // keeping only the client code
               {server_cleaned}
             end
       else
         {client_cleaned}

  /**
   * Return true if the code_elt should be kept, which means
   * that it is a root, or this is marked.
  **/
  @private is_marked(code_elt : JsAst.code_elt) =
    ServerReference.get(code_elt.r)

  @private unsafe_get_ident(key_ident:JsAst.key_ident) =
    match key_ident with
    | {~i} -> i
    | ~{k} -> k

  /**
   * Initialize the structure needing for marking
  **/
  @private fold_infos(code : JsAst.code, infos : JsCleaning.infos) : JsCleaning.infos =
    fold(code_elt, infos : JsCleaning.infos) =
      unicity = infos.unicity
      not_uniq =
        match code_elt.i : JsAst.key_ident with
        | {k=i} ->
          StringSet.mem(i, unicity)
        | {i=_} ->
          false
      if not_uniq
      then
        do ServerReference.set(code_elt.r, false)
        infos
      else
        client_roots = infos.client_roots
        client_roots = if is_root(code_elt) then [code_elt|client_roots] else client_roots
        elements = infos.elements
        types_client = infos.types_client
        rpcs = infos.rpc
        do
          match code_elt.d with
          | {} -> void
          | ~{t} ->
            //do jlog("Found the definition of the type {t} in the client")
            Hashtbl.add(types_client, t, unsafe_get_ident(code_elt.i))
          | ~{r} ->
            //do jlog("Found the definition of the rpc {rpc} in the client")
            Hashtbl.add(rpcs, r, unsafe_get_ident(code_elt.i))
          end
        infos =
          match code_elt.i with
          | { ~i } ->
            do JsIdent.define(i)
            do Hashtbl.add(elements, i, code_elt)
            ~{ infos with client_roots }

          | { ~k } ->
            do JsIdent.define(k)
            do Hashtbl.add(elements, k, code_elt)
            unicity = StringSet.add(k, unicity)
            ~{ infos with client_roots unicity }

        infos
    JsAst.fold_code(fold, code, infos)

   add_stack_server(infos, code_elt:ServerAst.code_elt, stack) =
     // extra deps that opa2js adds dynamically when serializing
     // fun actions arguments at toplevel
     stack =
       match code_elt.i with
       | {none} -> stack
       | {some=ident} ->
         closure_keys = Closure.deps_of_var_for_cleaning(ident)
         //do List.iter(closure_key -> jlog("SERVER: {code_elt.ident} is using ident {closure_key}"), closure_keys)
         List.append(closure_keys, stack)
     stack = LowLevelArray.fold(ident,stack ->
        //do jlog("SERVER: {code_elt.ident} is using ident {ident}")
        [ident|stack], code_elt.id,stack)
     stack =
         match code_elt.d
         {r=_} ->
           LowLevelArray.fold(type_,stack ->
           //do jlog("SERVER: {code_elt.ident} is using type {type_}")
           add_type_to_stack(infos, stack, type_), code_elt.td,stack)
         _ -> stack
     stack = LowLevelArray.fold(rpc,stack ->
        //do jlog("SERVER: {code_elt.ident} is using rpc {rpc}")
        add_rpc_to_stack(infos, stack, rpc), code_elt.rd,stack)
     stack

   fold_infos_server(server_code : ServerAst.code, infos:JsCleaning.infos) : JsCleaning.infos =
     fold(code_elt:ServerAst.code_elt, infos:JsCleaning.infos) : JsCleaning.infos =
       server_roots = infos.server_roots
       server_roots = if is_root_server(code_elt) then [code_elt|server_roots] else server_roots
       server_elements = infos.server_elements
       correspondence = infos.correspondence
       do
         match code_elt.i with
         | {none} -> void
         | {some=ident} ->
           do Hashtbl.add(server_elements, ident,code_elt)
           match code_elt.c with
           | {none} -> void
           | {some=client_ident} -> Hashtbl.add(correspondence, client_ident,ident)
           end
         end
       types_server = infos.types_server
       rpcs = infos.rpc
       do
         match code_elt.d with
         | {} -> void
         | ~{t} ->
           //do jlog("Found the definition of the type {t} in the server")
           Hashtbl.add(types_server, t,Option.get(code_elt.i))
         | ~{r} ->
           //do jlog("Found the definition of the rpc {rpc} in the server")
           Hashtbl.add(rpcs, r,Option.get(code_elt.i))
         end
       ~{ infos with server_roots }
     ServerAst.fold_code(fold, server_code, infos)

  /**
   * Marking the code. This is the implementation of the accessibility.
   * The [infos] should be the infos returned by the function [infos] called
   * on all the [code], which means the full [list(JsAst.code)].
  **/
  @private mark(infos : JsCleaning.infos) : void =
    elements = infos.elements
    server_elements = infos.server_elements
    rec mark(stack) =
      match stack with
      | [] -> void
      | [ ident | stack ] ->
        stack =
          match Hashtbl.try_find(elements, ident) with
          | { none } ->
            match Hashtbl.try_find(server_elements, ident) with
            | {none} -> stack
            | {some = code_elt} ->
              if ServerReference.get(code_elt.r)
              then stack
              else
                do ServerReference.set(code_elt.r, true)
                add_stack_server(infos, code_elt, stack)
            end
          | { some = code_elt } ->
            if ServerReference.get(code_elt.r)
            then stack
            else
              do ServerReference.set(code_elt.r, true)
              add_stack(infos, code_elt, stack)
        mark(stack)

    start_mark_server(code_elt) =
      stack = []
      stack = add_stack_server(infos, code_elt, stack)
      mark(stack)
    start_mark_client(code_elt) =
      stack = []
      stack = add_stack(infos, code_elt, stack)
      mark(stack)

    do List.iter(start_mark_server,infos.server_roots)
    List.iter(start_mark_client,infos.client_roots)

  /**
   * Sweeping unmarked elts. This is a [filter], so we could return a filtered [code],
   * but this would be inefficient in most of the case, because this make a huge reallocation,
   * where we do not care about keeping the reallocate code as we will just refold it.
   * This function does a fold on the filtered code, folding only kept code_elt.
   * The [marked] argument should be the one returned by the function mark, called
   * with the same code.
  **/
  @private fold_sweep(fold : JsAst.code_elt, 'acc -> 'acc, code : JsAst.code , acc : 'acc) : 'acc =
    fold(code_elt, acc) =
      if is_marked(code_elt) then fold(code_elt, acc) else acc
    JsAst.fold_code(fold, code, acc)

  /**
   * The main function, composing how it should be done the other function of this module.
   * Should be called on the full js code, without omitting any package.
  **/
  perform(fold : JsCleaning.infos -> (JsAst.code_elt, 'acc -> 'acc), code : list(JsAst.code), server_code : list(ServerAst.code), acc : 'acc) : 'acc =
    //do Log.debug("JsAst","Cleaning up javascript")
    infos = empty_infos()
    infos = List.fold(fold_infos, code, infos)
    infos = List.fold(fold_infos_server, server_code, infos)
    do mark(infos)
    acc = List.fold(fold_sweep(fold(infos), _, _), code, acc)
    do Closure.end_of_renaming()
    acc
}}
