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
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

import stdlib.core.loop
import stdlib.core.{map, web.core}


/**
 * {1 Types defined in this module}
 */

type OpaSerialize.unser = RPC.Json.json

@abstract
type OpaSerialize.closure_argument = external

/**
 * Type that describe option for some serialization functions.
 */
@opacapi
type OpaSerialize.options = {

  to : {server} / {client};
  /** Indicates who should receive the serialized structure */

  closure : {local} / {distant : -> void} / {at_best};
  /** Indicates how serialize closure :
      - {local} : Keep the call site on local
      - {distant} : If a distant implementation exists, serialize
        the closure with this environment (Currently we can serialize
        only empty closure on this way, when try to serialize a
        non-empty closure call the given function (example : for print
        a warning) and serialize as {local}).
      - {at_best} : As {distant} if possible else as {local}
  */

  to_session : option(Session.entity);
  /** Indicates if a value is serialized for a session

  */

  serialize_closure_callback : string -> void;
  /** a function called whenever a closure is serialized
      as such (not as a cell)
      Used only by opa2js for now
  */

}

/**
  * Form of the closure serialization (mostly for documentation, it is created and serialized on the fly)
  */
//@private
type OpaSerializeClosure.intermediate = {
  func_name : string        // fun_name suitable for the receiver pre serialized closure
  args      : list(RPC.Json.json)  // environment of the closure
  ty_args   : list(RPC.Json.json)  // type of the environment
}
//@private
@both
OpaSerializeClosure = {{
  /* road map:
     closure serialize on server should be fingerprinted so there deserialisation is possible with confidence
     closure deserialisation on server should:
       check for well-formed closure (type verification)
       verify the closure is not trying to access server_private without being authorized (simple precomputed dependency analysis)
       limit computation time of the closure
  */

  /** return a optional OpaSerializeClosure.intermediate from json */
  @private
  intermediate_from_json(json):option(OpaSerializeClosure.intermediate) =
    match json:RPC.Json.json
    // keep sync with OpaSerializeClosure.intermediate
    {Record = [ // in reverse order ? why ?
      ("ty_args",{List=ty_args}:RPC.Json.json),
      ("func_name",{String=func_name}:RPC.Json.json),
      ("args",{List=args}:RPC.Json.json)
    ]} -> some(~{func_name args ty_args=ty_args})

    _ -> none
    end

  to_intermediate(on_distant,clot:'cloture): option(OpaSerializeClosure.intermediate) =
    if WebUtils.is_client() then none else // DISABLE CLIENT TO SERVER SEND
    match on_distant(clot) {none} -> none
    {some=func_name} ->
    opa_args = Closure.get_args(@unsafe_cast(clot))
    opa_ty_args = Closure.get_ty_args(@unsafe_cast(clot))
    nb_args = Closure.Args.length(opa_args)
    nb_types = Closure.Args.length(opa_ty_args)
    if nb_args != nb_types then
/*     do if nb_types == 0 then
      println("cannot serialize no type in closure")
        else
      println("cannot serialize incoherent types in closure (nb_args={nb_args} nb_ty={nb_types})") */
     none
    else
    args = Init.list(nb_args)(i->
      arg = Closure.Args.get(opa_args,i)
      ty  = Closure.Args.get(opa_ty_args,i):OpaType.ty
      OpaSerialize.partial_serialize(arg,ty):RPC.Json.json
    )
    ty_args = Init.list(nb_types)(i->
      ty  = Closure.Args.get(opa_ty_args,i):OpaType.ty
      // @unsafe_cast is necessary until heteromorph recursivity is accepted
      OpaSerialize.partial_serialize(@unsafe_cast(ty),@typeval(OpaType.ty)):RPC.Json.json
    )
    some(~{func_name args ty_args})

  @private
  from_intermediate(~{func_name args ty_args} : OpaSerializeClosure.intermediate):option(Closure.t) =
    /* SECURITY, DO NOT CHANGE */ if WebUtils.is_server() then none else // FORBID THE SERVER TO UNSERIALIZE A CLOSURE
    match Closure.get_local(func_name) {none} -> none
    {some=clot_empty} ->
    // TODO SHOULD CHECK THAT TYPES ARE OK FOR THE FUNCTION BEFORE ACCEPTING CLIENT TO SERVER TRANSFER
    ty_args = Map_.list(ty_args)(ty ->
      @unsafe_cast(OpaSerialize.Json.unserialize_with_ty(ty, @typeval(OpaType.ty) ))
      :option(OpaType.ty)
    )
    args = Map_.list2(args,ty_args)(arg,ty ->
      match ty
      {some=ty} -> OpaSerialize.Json.unserialize_with_ty(arg,ty)
      {none} -> none
    )
    if List.exists(x->x==none,args) then none else
    args = Map_.list(args)(Option.get)
    ty_args = Map_.list(ty_args)(Option.get)
    some(
      Closure.env_apply_with_ty(
        @unsafe_cast(clot_empty),
        Closure.Args.of_list(args),
        Closure.Args.of_list(ty_args)
      )
    )

   closure_from_json(json:RPC.Json.json,_ty_res/*for checks*/:OpaType.ty):option(Closure.t) =
     Option.map(from_intermediate,intermediate_from_json(json)) ? none

  @private
  Closure = OpaValue.Closure
}}




/**
 * {1 Interface}
 */

@both OpaSerialize = {{

  JsonTop = @toplevel.Json

  Record = OpaValue.Record
  Closure = OpaValue.Closure


  /**
   * Make an error message...
   */
  error_message = OpaValue.error_message

  to_string = OpaValue.to_string

  /**
   * {2 Serialization}
   */

  /**
   * Default serialization options
   */
  default_options = {
    to = @sliced_expr({
      client = {server}
      server = {client}
    });
    closure = {at_best};
    to_session = {none};
    serialize_closure_callback = ignore;
  }

  /**
   * Serialize given value to string.
   */
  serialize(value) =
    serialize_with_type(@typeof(value),value)

  /**
   * Serialize a value, given both the value and its type
   *
   * Note : [value] must be have the type represented by [ty] if isn't
   * behavior is not defined.
   */
  serialize_with_type(ty:OpaType.ty, value:'a) =
    finish_serialize(partial_serialize(value, ty))

  serialize_with_type_options(ty:OpaType.ty, value:'a, options:OpaSerialize.options) =
    finish_serialize(partial_serialize_options(value, ty, options))

  /**
   * Serialize in intermediate format according to a type.
   *
   * Note : [value] must be have the type represented by [ty] if isn't
   * behavior is not defined.
   */
  partial_serialize_options(value, ty:OpaType.ty, options:OpaSerialize.options) =
    original_ty = ty

    /* Continuation *****************************/
    rec aux_cont(k, ty) : RPC.Json.json =
      unserialize = Magic.id(finish_unserialize(_, ty))
      on_message(k, msg) =
        do Continuation.return(k, msg)
        {continue}
      sess = Session_private.llmake(k, unserialize, {concurrent = on_message})
      aux(Magic.id(sess), @typeof(sess))

    /* For closure ******************************/
    and aux_clos(_ty_clos, clos, params, res) =
      n_params = List.length(params)
      /* This function make a cell that encapsulate the closure */
      make_cell() =
        do match options.closure with ~{distant} -> distant() | _ -> void
        msg_unserialize(jsonmsg) =
           match jsonmsg : RPC.Json.json with
           | {List = ljson} ->
             List.fold2(
               (ty, json, acc ->
                 Option.bind(
                   (l_acc ->
                     Option.map((_ +> l_acc),
                                finish_unserialize(json, ty))),
                   acc)),
               params, ljson, {some = []}
             ) |> Option.map(List.rev,_)
           | _ ->
             do ( error("Cell for closure : unexpected message") : void )
             none
        on_message(f, msg) =
          { return =
            #<Ifstatic:OPA_CLOSURE>
              args = Closure.Args.create(n_params)
              do List.iteri(Closure.Args.set(args,_,_), msg)
              Closure.apply(f, args)
            #<Else>
              match msg with
              | [] -> Magic.id(f)()
              | [a1] -> Magic.id(f)(a1)
              | [a1,a2] -> Magic.id(f)(a1,a2)
              | [a1,a2,a3] -> Magic.id(f)(a1,a2,a3)
              | [a1,a2,a3,a4] -> Magic.id(f)(a1,a2,a3,a4)
              | [a1,a2,a3,a4,a5] -> Magic.id(f)(a1,a2,a3,a4,a5)
              | [a1,a2,a3,a4,a5,a6] -> Magic.id(f)(a1,a2,a3,a4,a5,a6)
              | [a1,a2,a3,a4,a5,a6,a7] -> Magic.id(f)(a1,a2,a3,a4,a5,a6,a7)
              | [a1,a2,a3,a4,a5,a6,a7,a8] -> Magic.id(f)(a1,a2,a3,a4,a5,a6,a7,a8)
              | _ -> error("[OpaSerialize.partial_serialize] Closure with more than 8 arguments can't be serialized (try to compile with --closure)")
              end
            #<End>
            instruction = {continue} }
        cell =
          Cell_private.llmake(Magic.id(clos),
                              Magic.id({concurrent = on_message}),
                              {some = Magic.id(msg_unserialize)},
                              {some = res}
                              )
        aux(Magic.id(cell), @typeof(cell))
      /* Select the way to serialize the closure (as identifier or as
       * cell) */
      #<Ifstatic:OPA_CLOSURE>
        on_distant(clos) = @sliced_expr({
            server =
              match options.to with
              | {server} -> Closure.on_local(clos)
              | {client} -> Closure.on_distant(clos)
            client =
              match options.to with
              | {server} -> Closure.on_distant(clos)
              | {client} -> Closure.on_local(clos)
          })
        cell =
         match Closure.get_stored(clos)
	     /* CASE 1 : closure is already serialized as a session */
         {some = ~{cell arity}} ->
          if arity == n_params then
                aux(Magic.id(cell), @typeval(Cell.cell))
          else error("runtime")
         {none} ->
          /* closure may be serialized as a closure */
          /* Depends where we send the serialized structure */
          if options.closure == {local} then make_cell() /* CASE 2 : as a session ; must not serialize closure */
	      else match OpaSerializeClosure.to_intermediate(on_distant,clos)
		  {none} -> make_cell() /* CASE 2 : as a session ; cannot serialize closure */
          {some = intermediate} ->
            /* CASE 3 : as a partial call */
            do options.serialize_closure_callback(intermediate.func_name)
            aux(@unsafe_cast(intermediate), @typeval(OpaSerializeClosure.intermediate))
          end
          end
      #<Ifstatic:CLOSURE_DEBUG>
        do jlog("closure {Closure.get_identifier(clos)} empty={Closure.is_empty(clos)} serialized as {JsonTop.to_string(cell)}\nClosure dump = {Debug.dump(clos)}\nOptions = {options}")
      #<End>
        cell
      #<Else>
        make_cell()
      #<End>


    /* For record and sum, take value and fields */
    and aux_rec(value, fields) =
      {Record = Record.fold_with_fields(
        (field, tyfield, value, json ->
          name = Record.name_of_field_unsafe(field)
          res = aux(value, tyfield)
          [(name, res) | json]
        ), value, fields, []
      )}

    /* For list */
    and aux_list(value, ty) =
       l = List.foldr((v, acc ->
         r = aux(v, ty)
         r +> acc), value, [])
       {List = l}

    /* For abstract type ************************/
    and aux_abstract_client(value, client) =
      t = @typeval(Cell.cell)
      if client then
         cell =
           Magic.id(
             Cell_private.llmake(
               Magic.id(value),
               Magic.id({concurrent =
                         v, _ -> {return = v; instruction = {continue}}}),
               some(_ -> error("Cell for abstract serialization, can't receive remote messages")),
               some({TyVar = "Cell for abstract serialization, can't send remote result"})
               ))
         aux(Magic.id(cell), t)
      else
        aux(value, t)

    /* Transform an 'a value in json value (see json.opa). */
    and aux(value, ty) =
      match ty with
      /* Basic case *****************************/
      | {TyConst = {TyInt}}    -> {Int = Magic.id(value)}
      | {TyConst = {TyString}} -> {String = Magic.id(value)}
      | {TyConst = {TyFloat}}
        ->
          fvalue = Magic.id(value) : float
          if Math.is_normal(fvalue) then {Float=fvalue}
          else
            anormal = if Math.is_NaN(fvalue) then "NaN"
                      else if fvalue<0.0 then "-Infinity" else "Infinity"
            aux(Magic.id(anormal),{TyConst = {TyString}})

      /* Record case ****************************/
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
        aux_rec(value, row)
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        aux_rec(value, OpaType.fields_of_fields_list(value, col).f1)

      /* List case ******************************/
      | {TyName_ident = "list"; TyName_args = [ty_arg]} ->
        aux_list(Magic.id(value), ty_arg)

      /* Particular named type ******************/
      /* Session */
      | {TyName_ident = "Session.private.native"; TyName_args = _}
      | {TyName_ident = "channel"; TyName_args = _}
      | {TyName_ident = "Cell.cell"; TyName_args = _} ->
         Session_private.serialize(Magic.id(value), options)
      /* Json */
      | {TyName_ident = "OpaSerialize.unser"; TyName_args = _}
      | {TyName_ident = "RPC.Json.json"; TyName_args = _} ->
        Magic.id(value)

      /* Continuation */
      | {TyName_ident = "continuation"; TyName_args = [ty]} ->
        aux_cont(Magic.id(value), ty)
      /* Client abstract */
      | {TyName_ident = "Dom.private.element"; TyName_args = _}
      | {TyName_ident = "style_constructor"; TyName_args = _} ->
        aux_abstract_client(value, WebUtils.is_client())

      /* Encapsulated types ***********************/
      | {TyName_args = args; TyName_ident = ident} ->
         match %%BslValue.MagicContainer.serializer_get%%(ident) with
         | {none} -> aux(value, OpaType.type_of_name(ident, args))
         | {some = serializer} ->
           serializer = serializer.f1
           nargs = List.length(args)
           match nargs with
           | 0 -> serializer(value)
           | _ ->
              clos_arg = OpaValue.Closure.Args.create(nargs + 2)
              do List.iteri(
                (i, ty ->
                  OpaValue.Closure.Args.set(clos_arg, i,
                    partial_serialize_options(_, ty, _))
                ), args)
              do Closure.Args.set(clos_arg, nargs, value)
              do Closure.Args.set(clos_arg, nargs + 1, options)
              OpaValue.Closure.apply(@unsafe_cast(serializer), clos_arg)
           end
         end
      | {TyForall_quant = _; TyForall_body = body} ->
        aux(value, body)

      /* Closure ********************************/
      | {TyArrow_params = params; TyArrow_res = res} ->
        aux_clos(ty, Magic.id(value), params, res)

      /* Error case *****************************/
      | {TyAbstract}
      | {TyVar = _} ->
        do jlog(error_message("OpaSerialize.partial_serialize", original_ty, ty))
        do jlog("value: {Debug.dump(value)}")
        error("OpaSerialize.partial_serialize")
  aux(value, ty)



  /**
   * Serialize in intermediate format according to a type.
   *
   * Note : [value] must be have the type represented by [ty] if isn't
   * behavior is not defined.
   */
  partial_serialize(value, ty) =
    partial_serialize_options(value, ty, default_options)

  /**
   * Finalize serialization from the intermediate format to an
   * unserializable string.
   */
  finish_serialize(unser) = JsonTop.serialize_opt(unser)

 /**
   * {2 Unserialization}
   */
  /**
   * Unserialize one value represented with string [str], and check if
   * this value respect structure of type [ty].
   */
  unserialize(str, ty) =
    match partial_unserialize(str) with
    | {none} ->
      do jlog("[OpaSerialize.unserialize] Failed to unserialize from a string")
      {none}
    | {some = unser} ->
      finish_unserialize(unser, ty)

  /**
   * Unserialize a value of type [OpaType.ty]
   */
  unserialize_ty(str_ty) =
    unserialize(str_ty, @typeval(OpaType.ty)) : option(OpaType.ty)

  /**
   * Unserialize the given string according to given type.
   */
  unserialize_unsafe(str:string, ty) =
  (
    match unserialize(str, ty) with
     | {none} -> error("[OpaSerialize.unserialize_unsafe] error when unserializing {str:string}, with type {OpaType.to_pretty(ty)}")
     | ~{some}-> some
   )

  /**
   * Unserialize in intermediate format.
   */
  partial_unserialize(str) = JsonTop.deserialize_opt(str)


  /**
   * Check and finish unserialize value. This function ensures that
   * the returned value match with type [ty].
   */
  finish_unserialize(unser, ty) = finish_unserialize_with_sort(unser, ty, {false})
  finish_unserialize_with_sort(unser, ty, opt) =
    //do jlog("Try to unserialize {Json.to_string(unser)} with {OpaType.to_pretty(ty)}")
    original_ty = ty
    error_ret(str, v) =
      do Log.error("Finish unserialize", str)
      v
    magic_some(v) = some(Magic.id(v))
    /* Function for continuation ****************/
    rec aux_cont(json, ty) =
      t = @typeval(channel)
      fsess =
        Option.map(
          (sess ->
            (message ->
              options = Session.serialization_options(Magic.id(sess))
              serialize = Magic.id(partial_serialize_options(_, ty, options))
              Session_private.llsend(Magic.id(sess),
                                     {serialize = Magic.id(serialize); ~message}))
          ), aux(json, t))
      Option.map(
        fsess -> Continuation.make(fsess),
        fsess)

    /* Function for closure *********************/
    and aux_clos(json, params, ty_res) =
      closure_opt = OpaSerializeClosure.closure_from_json(json,ty_res):option(Closure.t)
      if closure_opt!=none then @unsafe_cast(closure_opt):option
      else
        begin
        t = @typeval(Cell.cell)
        basic_unser = aux(json, t)
        match basic_unser
        {none} -> {none}
        {some=cell_alpha}->
            cell = Magic.id(cell_alpha):Cell.cell
            options = Session.serialization_options(cell)
            /* This is the serialize function for message */
            msg_serialize(msg) =
              { List =
                  List.rev(
                    List.fold2(
                      (ty, value, acc ->
                        partial_serialize_options(Magic.id(value), ty, options) +> acc),
                      params, msg, [])) }
            /* This is it for make a function with n arguments */
            call(cell, args) =
              Magic.id(Cell_private.llcall(cell,
                                  Magic.id(args),
                                  {some = Magic.id(msg_serialize)},
                                  {some = ty_res})
                      )
            arity = List.length(params)
            f =
              #<Ifstatic:OPA_CLOSURE>
                clos =
                  Closure.create(
                    cargs ->
                      args = Closure.Args.to_list(cargs)
                      call(cell, args),
                    arity,
                    "UnserializedClosure"
                  )
                do Closure.set_stored(clos, {~cell ~arity})
                Magic.id(clos)
              #<Else>
                match arity with
                | 0 -> Magic.id(a1->call(cell, [a1] ))
                | 1 -> Magic.id(a1 -> call(cell, [a1]))
                | 2 -> Magic.id(a1,a2 -> call(cell, [a1,a2]))
                | 3 -> Magic.id(a1,a2,a3 -> call(cell, [a1,a2,a3]))
                | 4 -> Magic.id(a1,a2,a3,a4 -> call(cell, [a1,a2,a3,a4]))
                | 5 -> Magic.id(a1,a2,a3,a4,a5 -> call(cell, [a1,a2,a3,a4,a5]))
                | 6 -> Magic.id(a1,a2,a3,a4,a5,a6 -> call(cell, [a1,a2,a3,a4,a5,a6]))
                | 7 -> Magic.id(a1,a2,a3,a4,a5,a6,a7 -> call(cell, [a1,a2,a3,a4,a5,a6,a7]))
                | 8 -> Magic.id(a1,a2,a3,a4,a5,a6,a7,a8 -> call(cell, [a1,a2,a3,a4,a5,a6,a7,a8]))
                | _ -> //This case should be never occurs because serialization failed
                  error("[OpaSerialize.finish_unserialize] Closure with more than 8 arguments can't be unserialized (try to compile with --closure)")
                end
              #<End>
            some(f)
            end
        end

    /* Function for record **********************/
    and aux_rec(js_lst:list, fields:OpaType.fields) =
      match fields with
      | [{label=name; ty=ty}] ->
        if OpaType.is_void(ty) then
          /* build the optimized representation */
          match Record.field_of_name(name) with
          | {none} -> {none}
          | {some=field} -> {some = Record.make_simple_record(field)}
          end
        else
          aux_rec_unoptimized(js_lst,fields)
      | _ ->
        aux_rec_unoptimized(js_lst,fields)

    and aux_rec_unoptimized(js_lst, fields) =
        js_lst =
          if opt then
            List.sort_with(
                ((name1, _), (name2, _) ->
                  Order.compare(name1, name2, Order.reverse(Order.default))
                ), js_lst)
          else
            js_lst
        res =
          List.foldr(
            ((name, json), (acc, fields, err) ->
              if err then (acc, [], err)
              else
                match fields with
                | [] ->
                  error_ret("Type of field " ^ name ^ " is not found",
                            (acc, [], true))
                | [hd | tl] ->
                  do (if hd.label != name then
                         do Log.error("Improper name while deserializing field \"{hd.label}\" -- expected \"{name}\"", json)
                         @fail("Deserialization error")  // if it breaks, you are generating
                                // json that is not ordered properly
                     else void)
                  match aux(json, hd.ty) with
                  | {none} ->
                    error_ret("Unserialization of field {hd.label} with json {json} and with type {OpaType.to_pretty(hd.ty)} fail", (acc, [], true))
                  | {some = value} ->
                    match Record.field_of_name(name) with
                    | {none} -> error_ret("No field are named " ^ name,
                                (acc, [], true))
                    | {some = field} ->
                      (Record.add_field(acc, field, value), tl, err)
            ), js_lst, (Record.empty_constructor(), fields, false))
        if res.f3 then
          do Log.error("Failed to deserialize with fields {OpaType.to_pretty_fields(fields)}", js_lst)
          none
        else some(Record.make_record(res.f1))

    /* For abstract type ************************/
    and aux_abstract_client(json, client) =
      cell_alpha = aux(json, @typeval(Cell.cell))
      cell = Magic.id(cell_alpha):option(Cell.cell)
      match cell with
       {some=cell} -> if client
          then {some=Magic.id(Cell.call(cell, Magic.id(void)))}
          else {some=Magic.id(cell)}
       {none} -> {none}
      end

    /* For list, slow but tail-rec
       construct a couple (should be removed automatically)
       and an option (need to have a special folder)
     */
    and aux_list(l,ty_arg_opt)=
       match ty_arg_opt
        {some=ty_arg}->
           l=List.foldr(elmt,l -> match (l,aux(elmt,ty_arg))
                                ({some=l},{some=hd}) -> some([hd|l])
                                _ -> none,
                      l,some([]))
           Magic.id(l)
        {none} -> error_ret("Empty list with a record wihtout hd field",none)
        end

    /* Main aux function ************************/
    and aux(json:RPC.Json.json, ty:OpaType.ty) =
      //do jlog("value : {Json.to_string(json)}\n on type : {OpaType.to_pretty(ty)}\n")
      match (json, ty) with
      /* Basic case *****************************/
      | ({Int = value}, {TyConst = {TyInt}})
      | ({Float = value}, {TyConst = {TyFloat}})
      | ({String = value}, {TyConst = {TyString}}) -> magic_some(value)

      /* Degenerate float case ******************/
      | ({~String} , {TyConst = {TyFloat}}) -> magic_some(
        match String
        "Infinity"  ->   1.0 / 0.0 // temporary hack break Math dependencies
        "-Infinity" ->  -1.0 / 0.0
        "NaN"       ->   0.0 / 0.0
        _ -> @fail
        )
      | ({Int = value}, {TyConst = {TyFloat}}) -> magic_some( Float.of_int(value) )

      /* Record case ****************************/
      | ({Record = js_lst}, {TyRecord_row = row})
      | ({Record = js_lst}, {TyRecord_row = row; TyRecord_rowvar = _}) ->
        aux_rec(js_lst, row)
      | ({Record = js_lst}, {TySum_col = lfields})
      | ({Record = js_lst}, {TySum_col = lfields; TySum_colvar = _}) ->
        ltyfield =
          List.fold(((name, _), acc ->
            OpaType.Field.of_string(name) +> acc),
            js_lst, [])
        (match fields_of_fields_list2(ltyfield, lfields) with
        | {none} -> error_ret("Fields ({OpaType.to_pretty_lfields(lfields)}) are not found in type sum ({List.to_string(ltyfield)})", {none})
        | {some = fields} ->
          //do jlog("Select {OpaType.to_pretty_fields(fields)} from sum {OpaType.to_pretty_lfields(lfields)} and json fields {List.to_string(ltyfield)}")
          aux_rec(js_lst, fields))

      /* List case ******************************/
      | ({List = []}, _ ) -> magic_some([])

      | ({List = l}, {TyName_ident = "list"; TyName_args = [ty_arg]}) ->
        aux_list(l, some(ty_arg) )

      /* List case when deserialization case is not a record type */
      | ({List = l}, {TyRecord_row = row ...}) ->
        aux_list(l, OpaType.type_of_field(row, OpaType.Field.of_string("hd")) )
      | ({List = l}, {TySum_col = col ...}) ->
        match fields_of_fields_list2(["hd","tl"],Magic.id(col)) with
        | {none} -> error_ret("Fields hd and tl are not found in type sum for a json list", {none})
        | {some = row} -> aux_list(l, OpaType.type_of_field(row,OpaType.Field.of_string("hd")) )
        end

      /* List case when serialization type is not list */
      | ({Record = _}, {TyName_ident = "list"; TyName_args = [ty_arg]}) ->
         rec record_to_list(r,acc)=
           match r:RPC.Json.json
           {Record = js_lst} ->
              match List.assoc("hd",js_lst) with
              | {some=hd} ->
                match aux(hd,ty_arg) with
                | {some=hd} ->
                  match List.assoc("tl",js_lst) with
                  | {some=tl} -> record_to_list(tl , [hd|acc])
                  | {none} -> error_ret("missing tl field",none)
                  end
                | {none} -> error_ret("TODO",none)
                end
              | {none} -> magic_some(List.rev(acc))
              end
           _ -> error_ret("mixed record/list case",none)
           end
         record_to_list(json,[])

      /* Particular named type ******************/
      /* Session */
      | (_, {TyName_ident = "Session.private.native"; TyName_args = _})
      | (_, {TyName_ident = "channel"; TyName_args = _})
      | (_, {TyName_ident = "Cell.cell"; TyName_args = _}) ->
        Magic.id(Session_private.unserialize(json))
      /* Json */
      | (_, {TyName_ident = "OpaSerialize.unser"; TyName_args = _})
      | (_ ,{TyName_ident = "RPC.Json.json"; TyName_args = _}) -> magic_some(json)

      /* Continuation */
      | (_, {TyName_ident = "continuation"; TyName_args = [ty]}) ->
        Magic.id(aux_cont(json, ty))
      /* Client abstract */
      | (_, {TyName_ident = "dom_element"; TyName_args = _})
      | (_, {TyName_ident = "style_constructor"; TyName_args = _}) ->
        aux_abstract_client(json, WebUtils.is_client())

      /* Encapsulated types ***********************/
      | (_, {TyName_args = args; TyName_ident = ident}) ->
        OpaValue.todo_magic_container(
          (ident ->
             Option.map((r -> r.f2),
                        %%BslValue.MagicContainer.serializer_get%%(ident))
          ),
          ident, args, (ty, x -> aux(x, ty)),
          aux(_, OpaType.type_of_name(ident, args)),
          json)
      | (_, {TyForall_quant = _; TyForall_body = body}) ->
        aux(json, body)

      /* Closure ********************************/
      | (_, {TyArrow_params = params; TyArrow_res = ty_res}) ->
        aux_clos(json, params, ty_res)

      /* unknown type, legal in some situation */
      | ({String=_},{TyVar = _}) ->
         do Log.warning("UNSERIALIZE","unknown type TyVar in {OpaType.to_string(original_ty)}" ^
                " - Suspicious but legal, TODO eliminate legal case - replaced by a dummy value")
         some(Magic.id("I was unserialized from a TyVar"))
         //none

      /* Error case *****************************/
      | (a,b) ->
        do jlog("[OpaSerialize.finish_unserialize] Type doesn't match value :\nvalue : {to_string(a)}\n on type : {to_string(b)}\n inside the main type {OpaType.to_pretty(original_ty)}")
        {none}
    aux(unser, ty)


  /* [ltyfield] and list on [lfields] must be ordered */
  fields_of_fields_list2(ltyfield : list(string), lfields:list(OpaType.fields)) =
    List.find(
      (fields ->
        rec aux(l1 : list, l2 : list)=
          match l1 with
          | [] ->
            (match l2 with
            | [] -> true
            | _ -> false)
          | [hd1 | tl1] ->
            (match l2 with
             | {hd = hd2; tl = tl2} ->
               if @toplevel.String.equals(hd1, hd2.label) then aux(tl1, tl2)
               else false
             | _ -> false)
          | _ -> false
        aux(ltyfield, fields)
      ), lfields)

  /**
   * (De)serialization of arrays
  **/
  @private serialize_llarray(sera, a, opt) =
    {List = LowLevelArray.fold((x, acc -> sera(x, opt) +> acc), a, [])}

  @private unserialize_llarray(unser : RPC.Json.json -> option('a), json) =
    match json : RPC.Json.json with
    | {List = l} ->
      size = List.length(l)
      a = LowLevelArray.create(size, @unsafe_cast(1))
      x = List.foldi(
        (i, x, acc ->
          match acc with
          | {none} -> none
          | _ ->
            match unser(x) with
            | {none} -> {none}
            | {some = x} ->
              do LowLevelArray.set(a, size-1-i, x)
              acc
        ), l, some(a))
      x
    | _ -> none

  @private @serializer(llarray('a)) serialization_llarray = (serialize_llarray, unserialize_llarray)

  /**
   * (De)serialization of itextrator
  **/
  @private serialize_itextrator(it, _) : RPC.Json.json =
    txt = Itextrator.txt(it)
    pos = Itextrator.pos(it)
    // In Javascript, index are unicode_index
    // and in Ocaml, bytes_index
    // We use the unicode_index for the serialization
    unicode_index = Cactutf.length_until(txt, pos)
    { Record = [
        ("unicode_index", { Int = unicode_index } : RPC.Json.json),
        ("txt", { String = txt })
      ]
    }

  @private unserialize_itextrator(json) =
    match json : RPC.Json.json with
    | { Record = [
        ("unicode_index", { Int = unicode_index } : RPC.Json.json),
        ("txt", { String = txt })
      ] } ->

      it = Itextrator.make(txt)
      bytes = Cactutf.nth(txt, unicode_index)
      it = Itextrator.forward(it, bytes)
      some(it)

    | _ -> none

  @private @serializer(itextrator) serialization_itextrator = (serialize_itextrator, unserialize_itextrator)


  String = {{
    serialize = OpaSerialize.serialize
    unserialize(v) =
      typeof_alpha = @typeval('a)
      OpaSerialize.unserialize(v, typeof_alpha) : option('a)
  }}

  Json = {{
    serialize(v) = OpaSerialize.partial_serialize(v, @typeof(v))

    unserialize(v:RPC.Json.json):option('a) = unserialize_with_ty(v,@typeval('a))
    unserialize_with_ty(v:RPC.Json.json,ty:OpaType.ty) = OpaSerialize.finish_unserialize(v,ty)

    unserialize_unsorted(v:RPC.Json.json) =
     typeof_alpha = @typeval('a)
     OpaSerialize.finish_unserialize_with_sort(v, typeof_alpha, {true}) : option('a)

  }}

}} /* disabled for S3: : OpaValue.interface */

/**
  * Deprecated, [un]serialize
  */

@deprecated({use="OpaSerialize.String.serialize"})
@both magic_serialize = OpaSerialize.serialize

@deprecated({use="OpaSerialize.Json.serialize"})
@both magic_serialize_json = OpaSerialize.Json.serialize

@deprecated({use="OpaSerialize.Json.unserialize"})
@both magic_unserialize_json = OpaSerialize.Json.unserialize

@deprecated({use="OpaSerialize.Json.unserialize"})
unserialize_json = OpaSerialize.Json.unserialize

@deprecated({use="OpaSerialize.String.unserialize"})
unserialize=OpaSerialize.String.unserialize

@deprecated({use="OpaSerialize.String.unserialize"})
magic_unserialize = OpaSerialize.String.unserialize
