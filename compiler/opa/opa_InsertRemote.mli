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
(* TODOK1 - Documents pre and post condition*)
(** Resolve slicer directives by stubs and skeletons.

    {6 Resolving directive}
    This module provides 2 main functions, for transform server and
    client code.  Both functions have the same behavior for resolve
    directives. We have 3 types of directives where functions works :

    - [publish_directive] : Resolved by skeleton generation and by
    registration of this in a rpc dispatcher.

    - [call_directive] : Resolved by stub generation and by
    replacement of the call_directive by the ident of generated
    stub. Of course stubs are shared. For example if in code we have
    several @*_call(toto) we generate only one toto_stub.

    - [insert_directive] : This directive have a sense only on client
    side. It's resolved by generation of a new directive for the
    client code compiler. This directive is [\@hybrid_value] and it
    contains two expressions. First of them is a client expression,
    this expression is type [string -> 'a]. Second is a expression that
    must be computed on server, and this expression is a [string].


    {9 Skeleton generation}

    Skeleton maybe generated for functional value, or non-functional
    value. Generated skeletons get only one arguments, it's a
    [string]. This arguments represents the serialized request. This
    request should be contains all arguments of the functions (or any
    if it's a skeleton for value). And should contains for all type
    variable (in type of this function) an type argument (or any if
    it's a skeleton for value). After several verification on request,
    it call a non optimized (for explicit instantiation) function.
    Finally it serialize the result.

    A generic example (in Opa Style) :
    {[
      fun str ->
        match OpaRPC.unserialize str with
        | {some = request} ->
          (match OpaRPC.extract_type request with
           | [i1; i2; ...] ->
             (match OpaTsc.implementation [i1; i2; ...] tsc with
              | { TyArrow_params = [t1; t2; t3; ...]; TyArrow_res = tres } ->
                (match  OpaRPC.extract_value [t1; t2; t3; ...] request  with
                | { some = [a1; a2; a3; ...] } ->
                  { some = OpaValue.serialize tres
                        (sliced_fun i1 i2 ... a1 a2 a3 ...) }
                | _ -> { none = () })
              | _ -> { none = () })
           | _ -> { none = () })
        | _ -> { none = () }
      : string -> string option
    ]}

    An concret example : skeleton for toto of type [list('a) -> int ->
    option('a)]
    {[
    val tsc = {
      quantifier = [{TyVar = "'a"}];
      body = {TyArrow_params = [list('a); int];
              TyArrow_res = option('a)}
    }
    fun str ->
      match OpaRPC.unserialize str with
      | {some = request} ->
        (match OpaRPC.extract_type request with
         | [i1] ->
           (match OpaTYpe.instantiate [i1] tsc with
            | { TyArrow_params = [t1; t2]; TyArrow_res = tres } ->
              (match  OpaRPC.extract_value [t1; t2] request with
               | { some = [a1; a2] } ->
                 { some = OpaValue.serialize tres (toto_exp i1 a1 a2) }
               | _ -> { none = () })
             | _ -> { none = () })
         | _ -> { none = () })
      | _ -> { none = () }
    : string -> string option
    ]}

    For conclusion, generated skeleton make :
    - Unserialize RPC request represented by argument of the
    skeleton.
    - Extract instantiate types include in request (if original type
    scheme have type variable).
    - Extract value and check with instantiate type of function.
    - Call to the function with type arguments and values.
    - Serialize the response.

    {9 Stub generation}
    Generated stub is just a function that make a correct request for
    the correspondig skeleton, and execute a distant call to the other
    side. For finish it check response.

    A generic example (in Opa Style) :
    {[
    fun i1 -> fun i2 -> ... ->
      fun a1 -> fun a2 -> fun a3 ->
        match OpaTsc.implementation [i1; i2; ...] tsc /*type scheme of the function*/ with
        | { TyArrow_params = [t1; t2; t3; ...]; TyArrow_res = tres } } ->
          match
            send_to_<side>
              "<function_id>"
              (OpaRPC.serialize
                (OpaRPC.add_args t3 a3
                (OpaRPC.add_args t2 a2
                (OpaRPC.add_args t1 a1
                  (OpaRPC.add_var_types i2
                  (OpaRPC.add_var_types i1 OpaRPC.empty_request))))))
            tres
          with
          | { some = res } -> res
          | _ -> /* Make an error */
        | _ -> /* Make an error */
    ]}

    A concret example : stub for toto of type [list('a) -> int ->
    option('a)]
    {[
    val tsc = {
        quantifier = [{TyVar = "'a"}];
        body = {TyArrow_params = [list('a); int];
                TyArrow_res = option('a)}
      }
    fun t1 -> fun a1 -> fun a2 ->
      match OpaTsc.implementation [i1] tsc with
      | { TyArrow_params = [t1; t2]; TyArrow_res = tres } } ->
        match
          send_to_<side>
            "<function_id>"
            (OpaRPC.serialize
              (OpaRPC.add_args t2 a2
              (OpaRPC.add_args t1 a1
                (OpaRPC.add_var_types i1 OpaRPC.empty_request))))
          tres
        with
        | { some = res } -> res
        | _ -> /* Make an error */
      | _ -> /* Make an error */
    ]}

    For conclusion, generated skeleton make :
    - Instantiate type scheme
    - Match the instantiated type scheme
    - Add instantiate type variable of the function to an empty
     request. (Type given at runtime, like exp-inst).
    - Add argument (with here type) to request
    - Send serialized request, and check returned value with tres.

    {9 Pre-conditions}

    {9 Post-conditions}

    - All generated stubs, skeletons (and registration of skeletons) are
    added on the code, but it's not guaranteed that the code is well
    ordered.

    - Code is already typed.

    - Doesn't break lambda lifting.

    @author Quentin Bourgerie
*)

(** Check if expression on hybrid_value is well formed. *)
val check_hybrid_value :
  QmlAst.annotmap -> QmlAst.expr -> QmlAst.expr -> bool

(**{6 Main functions} *)
(** Options for main functions. That contains levels of
    optimizations :
    [optimize_insert] :
    - 0 : No optimizations. Server value is fully serialize
    and fully unserialize on client.
    - 1 : Server values are inserted with their back-end
    representations. No unserialization on client.
    [optimize_publish] :
    - 0 : No optimizations.
    - 1 : Use factorized skeletons.
    [optimize_stub] :
    - 0 : No optimizations.
*)
type options = {
  optimize_insert : int;
  optimize_publish : int;
  optimize_call : int;
}

(** Default options (0,0,0) *)
val default_options : options

val prelude : gamma:QmlTypes.gamma -> annotmap:QmlAst.annotmap -> QmlAst.code -> QmlAst.code -> unit
val postlude : QmlRenamingMap.t -> QmlRenamingMap.t -> QmlRenamingMap.t * QmlRenamingMap.t
val need_to_process_code : QmlAst.code -> QmlAst.code -> bool

(** Work on server code. Resolve :

    [\@ajax_publish] :
    - Generate server skeletons for catch RPC request.
    - Register generated skeletons on a dispatcher for RPC request.

    [\@comet_call] : Like [\@ajax_call] (see :
    perform_on_client_code), but for server side.

    [perform_on_sever_code ~annotmap ~gamma expmap_server
    expmap_client renaming_server renaming_client code] Perform
    transformations on server code.
    [exmap_sever] It's a map which contains non-optimized server
    functions in view of explicit instantiation
    [exmap_client] It's a map which contains non-optimized client
    functions in view of explicit instantiation
    [renaming_server] It's the renaming map of server, this map it's
    updated
    [renaming_client] It's the renaming map of client.
    [code] The server code
*)
val perform_on_server_code :
  ?options:options ->
  annotmap:QmlAst.annotmap ->
  stdlib_gamma:QmlTypes.gamma ->
  gamma:QmlTypes.gamma ->
  Pass_ExplicitInstantiation.published_map ->
  Pass_ExplicitInstantiation.published_map ->
  QmlRenamingMap.t -> QmlRenamingMap.t ->
  QmlAst.code ->
  QmlAst.annotmap * QmlTypes.gamma * QmlAst.code * QmlAst.code

(** Work on client code. Resolve :

    [\@ajax_call] :
    - Generate client stubs for send a RPC request on server, and
    catch result.
    - Replace call of remote functions by call of generated stubs.

    [\@comet_publish] :
    - Like [\@ajax_publish] (see : perform_on_server_code), but for client side.

    [\@insert_server_value] :
    - Replace by a directive [\@hybrid_value] for qmljs. More info, see
    [QmlAst.qml_directive].

    [perform_on_sever_code ~annotmap ~gamma expmap_client
    expmap_server code] Perform transformations on client code.
    [exmap_client] It's a map which contains non-optimized client
    functions in view of explicit instantiation
    [exmap_sever] It's a map which contains non-optimized server
    functions in view of explicit instantiation
    [renaming_client] It's the renaming map of client, this map it's
    updated
    [renaming_server] It's the renaming map of server.
    [code] The client code

*)
val perform_on_client_code :
  ?options:options ->
  annotmap:QmlAst.annotmap ->
  stdlib_gamma:QmlTypes.gamma ->
  gamma:QmlTypes.gamma ->
  Pass_ExplicitInstantiation.published_map ->
  Pass_ExplicitInstantiation.published_map ->
  QmlRenamingMap.t -> QmlRenamingMap.t ->
  QmlAst.code ->
  QmlAst.annotmap * QmlTypes.gamma * QmlAst.code * QmlAst.code

(**
   Saving and loading of explicit maps across compilation units
*)
module R2 :
sig
  val save : side:[`client|`server] -> QmlAst.annotmap -> Pass_ExplicitInstantiation.published_map -> unit
  val load : side:[`client|`server] -> QmlAst.annotmap -> Pass_ExplicitInstantiation.published_map ->
    QmlAst.annotmap * Pass_ExplicitInstantiation.published_map
end
