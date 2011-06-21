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
 * Runtime client dead code elimination.
 * @author Mathieu Barbin
**/


/**
 * The cleaning algorithm is a mark and swip, based on a graph accessibility from root nodes.
 * As we prefer that the stdlib does not depend on fgraph, there is an ad-hoc implementation,
 * without graphs construction, dealing with specialized structures.
 *
 * We have several [JsAst.code] registered from several packages. The cleaning is global.
 * That's why environment of cleaning (infos) and the cleaning itself should be performed
 * on the full [list(JsAst.code)], without omitting any package.
**/

/**
 * Accessing [code_elt] by their names.
**/
type JsCleaning.elements = JsIdentMap.t(JsAst.code_elt)

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
 * The type of the infos used for the cleaning.
**/
type JsCleaning.infos = {
  elements : JsCleaning.elements
  server_elements : JsIdentMap.t(ServerAst.code_elt)
  types_server : stringmap(ServerAst.ident)
  types_client : stringmap(JsAst.ident)
  rpc : stringmap(JsAst.ident) // or ServerAst.ident actually
  client_roots : list(JsAst.code_elt);
  server_roots : list(ServerAst.code_elt);
  unicity : JsCleaning.unicity
  correspondence : JsIdentMap.t(ServerAst.ident)
    // maps client ident to the same ident in the server
    // (if any)
}

/**
 * The type use by the marking function
**/
type JsCleaning.marked = JsIdentSet.t

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
  empty_infos = infos
  @private infos =
    elements = JsIdentMap.empty
    server_elements = JsIdentMap.empty
    types_server = StringMap.empty
    types_client = StringMap.empty
    rpc = StringMap.empty
    server_roots = []
    client_roots = []
    unicity = StringSet.empty
    correspondence = JsIdentMap.empty
    cleaning = ~{
      elements
      server_elements
      server_roots
      client_roots
      unicity
      types_server
      types_client
      rpc
      correspondence
    }
    cleaning : JsCleaning.infos

  /**
   * Identifiers declared as root.
  **/
  @private is_root_ident(ident : JsAst.ident) =
    // do jlog("is_root_ident : {ident}")
    JsIdent.is_root(ident)

  @private add_type_to_stack(infos, stack, type_use) =
    stack =
      match StringMap.get(type_use,infos.types_client) with
      | {none} -> error("Cannot find the client identifier defining type {type_use}")
      | {some = ident} -> [ident|stack]
      end
    match StringMap.get(type_use,infos.types_server) with
    | {none} -> error("Cannot find the server identifier defining type {type_use}")
    | {some = ident} -> [ident|stack]
    end

  @private add_rpc_to_stack(infos, stack, rpc_use) =
    match StringMap.get(rpc_use,infos.rpc) with
    | {none} -> error("Cannot find the identifier defining rpc {rpc_use}")
    | {some = ident} -> [ident|stack]
    end

  /**
   * Add in the stack all ident contained in the [code_elt]
  **/
  @private add_stack(infos, code_elt:JsAst.code_elt, stack : JsCleaning.stack) =
    fold(me, stack) =
      match me : JsAst.mini_expr with
      | { ~ident } ->
        //do jlog("CLIENT: {code_elt.ident} is using ident {ident}")
        [ident|stack]
      | { type_def = _ }
      | { rpc_def = _ }
      | { verbatim = _ } -> stack
      | { set_distant = _ } -> [JsAst.set_distant|stack]
      | ~{ type_use } ->
        //do jlog("CLIENT: {code_elt.ident} is using type {type_use}")
         add_type_to_stack(infos, stack, type_use)
      | ~{ rpc_use } ->
        //do jlog("CLIENT: {code_elt.ident} is using rpc {rpc_use}")
        add_rpc_to_stack(infos, stack, rpc_use)
      end
    stack = JsAst.fold_content(fold, code_elt.content, stack)
    // extra deps that opa2js adds when serializing
    // @insert_server_value
    stack =
      match code_elt.ident with
      | {key=_} -> stack
      | {~ident}
      | {~ident key=_} ->
        closure_keys = Closure.deps_of_var_for_cleaning(ident)
        //do List.iter(closure_key -> jlog("CLIENT: {ident} is using ident {closure_key}"), closure_keys)
        List.append(closure_keys, stack)
    stack

  /**
   * Say if a code_elt is a root, which means the start point of the accessibility.
  **/
  @private is_root(code_elt : JsAst.code_elt) =
    is_root =
      ServerReference.get(code_elt.root)
      || (
        match code_elt.ident with
        | {~ident}
        | {~ident key=_} ->
          is_root_ident(ident)
        | { key = _ } -> true
      )
    do if is_root then ServerReference.set(code_elt.root, true)
    is_root

  /**
   * Say if a code_elt is a root, which means the start point of the accessibility.
  **/
  @private is_root_server(code_elt : ServerAst.code_elt) =
    is_root =
      ServerReference.get(code_elt.root)
      || (
        match code_elt.ident with
        | {none} -> true
        | {some = ident} -> is_root_ident(ident)
      )
    do if is_root then ServerReference.set(code_elt.root, true)
    is_root

  check_if_kept(infos,ident) : {no_cleaning} / {kept} / {server_cleaned} / {client_cleaned} =
    match JsIdentMap.get(ident,infos.elements) with
    | {none} -> {no_cleaning} /* cleaning is not activated */
    | {some=elt} ->
       if ServerReference.get(elt.root) then
         match JsIdentMap.get(ident,infos.correspondence) with
         | {none} ->
           // useful code but no server counterpart
           {kept}
         | {some=server_ident} ->
           // code on both sides
           match ServerIdentMap.get(server_ident,infos.server_elements) with
           | {none} -> @fail(ident)
           | {some = ~{root ...}} ->
             if ServerReference.get(root) then
               // keeping the client and the server
               {kept}
             else
               // keeping only the client code
               {server_cleaned}
             end
       else
         {client_cleaned}

  /**
   * Bypass used to perform a side effect on the server to tell than
   * a registered client function was cleaned, and so is no more
   * defined, and no more available.
  **/
  @private set_distant_false = %%bslclosure_set_distant_false%%

  /**
   * Return true if the code_elt should be kept, which means
   * that it is a root, or this is marked.
  **/
  @private is_marked(code_elt : JsAst.code_elt) =
    ServerReference.get(code_elt.root)

  @private unsafe_get_ident(key_ident:JsAst.key_ident) =
    match key_ident with
    | {~ident} -> ident
    | {~ident key=_} -> ident
    | ~{key} -> error("unsafe_get_ident on {key}")

  /**
   * Initialize the structure needing for marking
  **/
  @private fold_infos(code : JsAst.code, infos : JsCleaning.infos) : JsCleaning.infos =
    fold(code_elt, infos : JsCleaning.infos) =
      unicity = infos.unicity
      not_uniq =
        match code_elt.ident : JsAst.key_ident with
        | {~key}
        | {~key ident=_} -> StringSet.mem(key, unicity)
        | {ident=_} -> false
      if not_uniq
      then
        do ServerReference.set(code_elt.root, false)
        infos
      else
        client_roots = infos.client_roots
        client_roots = if is_root(code_elt) then [code_elt|client_roots] else client_roots
        elements = infos.elements
        types_client = infos.types_client
        rpcs = infos.rpc
        (rpc, types_client) =
          match code_elt.definition with
          | {nothing} -> (rpcs, types_client)
          | ~{`type`} ->
            //do jlog("Found the definition of the type {`type`} in the client")
            (rpcs, StringMap.add(`type`,unsafe_get_ident(code_elt.ident),types_client))
          | ~{rpc} ->
            //do jlog("Found the definition of the rpc {rpc} in the client")
            (StringMap.add(rpc,unsafe_get_ident(code_elt.ident),rpcs), types_client)
          end
        infos =
          match code_elt.ident with
          | { ~ident } ->
            do JsIdent.define(ident)
            elements = JsIdentMap.add(ident, code_elt, elements)
            ~{ infos with elements client_roots types_client rpc }

          | { ~key } ->
            unicity = StringSet.add(key, unicity)
            ~{ infos with client_roots unicity types_client rpc }

          | ~{ key ident } ->
            do JsIdent.define(ident)
            elements = JsIdentMap.add(ident, code_elt, elements)
            unicity = StringSet.add(key, unicity)
            ~{ infos with elements unicity client_roots types_client rpc }
        infos
    JsAst.fold_code(fold, code, infos)

   add_stack_server(infos, code_elt:ServerAst.code_elt, stack) =
     // extra deps that opa2js adds dynamically when serializing
     // fun actions arguments at toplevel
     stack =
       match code_elt.ident with
       | {none} -> stack
       | {some=ident} ->
         closure_keys = Closure.deps_of_var_for_cleaning(ident)
         //do List.iter(closure_key -> jlog("SERVER: {code_elt.ident} is using ident {closure_key}"), closure_keys)
         List.append(closure_keys, stack)
     stack = LowLevelArray.fold(ident,stack ->
        //do jlog("SERVER: {code_elt.ident} is using ident {ident}")
        [ident|stack], code_elt.ident_deps,stack)
     stack = LowLevelArray.fold(type_,stack ->
        //do jlog("SERVER: {code_elt.ident} is using type {type_}")
        add_type_to_stack(infos, stack, type_), code_elt.type_deps,stack)
     stack = LowLevelArray.fold(rpc,stack ->
        //do jlog("SERVER: {code_elt.ident} is using rpc {rpc}")
        add_rpc_to_stack(infos, stack, rpc), code_elt.rpc_deps,stack)
     stack

   fold_infos_server(server_code : ServerAst.code, infos:JsCleaning.infos) : JsCleaning.infos =
     fold(code_elt:ServerAst.code_elt, infos:JsCleaning.infos) : JsCleaning.infos =
       server_roots = infos.server_roots
       server_roots = if is_root_server(code_elt) then [code_elt|server_roots] else server_roots
       server_elements = infos.server_elements
       correspondence = infos.correspondence
       (server_elements,correspondence) =
         match code_elt.ident with
         | {none} -> (server_elements,correspondence)
         | {some=ident} ->
           server_elements = StringMap.add(ident,code_elt,server_elements)
           correspondence =
             match code_elt.client_equivalent with
             | {none} -> correspondence
             | {some=client_ident} -> JsIdentMap.add(client_ident,ident,correspondence)
             end
           (server_elements,correspondence)
         end
       types_server = infos.types_server
       rpcs = infos.rpc
       (rpc, types_server) =
         match code_elt.defines with
         | {nothing} -> (rpcs, types_server)
         | ~{`type`} ->
           //do jlog("Found the definition of the type {`type`} in the server")
           (rpcs, StringMap.add(`type`,Option.get(code_elt.ident),types_server))
         | ~{rpc} ->
           //do jlog("Found the definition of the rpc {rpc} in the server")
           (StringMap.add(rpc,Option.get(code_elt.ident),rpcs), types_server)
         end
       ~{ infos with server_roots server_elements types_server rpc correspondence }
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
          match JsIdentMap.get(ident, elements) with
          | { none } ->
            match JsIdentMap.get(ident, server_elements) with
            | {none} -> stack
            | {some = code_elt} ->
              if ServerReference.get(code_elt.root)
              then stack
              else
                do ServerReference.set(code_elt.root, true)
                add_stack_server(infos, code_elt, stack)
            end
          | { some = code_elt } ->
            if ServerReference.get(code_elt.root)
            then stack
            else
              do ServerReference.set(code_elt.root, true)
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
    do Log.debug("JsAst","Cleaning up javascript")
    infos = List.fold(fold_infos, code, infos)
    infos = List.fold(fold_infos_server, server_code, infos)
    // do println("end of infos")
    do mark(infos)
    // do println("end of marking")
    acc = List.fold(fold_sweep(fold(infos), _, _), code, acc)
    // do println("end of sweeping")
    do Closure.end_of_renaming()
    acc
}}
