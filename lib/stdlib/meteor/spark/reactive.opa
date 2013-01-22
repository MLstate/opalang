/*
    Copyright © 2012, 2013 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa.  If not, see <http://www.gnu.org/licenses/>.
*/

package stdlib.meteor.spark

/**
 * Reactive User Interface
 *
 * @category UI
 * @author Cedric Soulas
 * @destination Work in progress
 */


type Cursor.callback('a) = {
    ('a, int -> void) added,
    ('a, int -> void) changed,
    ('a, int -> void) removed,
    ('a, int, int -> void) moved
}

type Cursor.t('a) = {
    (Cursor.callback('a) -> void) observe,
}

type Cursor.network_message('a) =
    { ('a, int) added }
or  { ('a, int) changed }
or  { ('a, int) removed }
or  { ('a, int, int) moved }

type Reactive.value('a) = {
    string id,
    (->'a) get,
    ('a->{}) set,
    (->'a) get_silently,
    ('a->{}) set_silently,
    ((xhtml->xhtml)->xhtml) render,
    (('a->xhtml)->xhtml) render_value,
    OpaType.ty ty
}

@both_implem @serializer(Reactive.value('a)) (Reactive.value('a), OpaSerialize.options -> RPC.Json.json, RPC.Json.json -> option(Reactive.value('a))) serialization_reactive = (Reactive.serialize, Reactive.unserialize)

type serialized_reactive('a) = {
    string id,
    'a initial_value
}

type Reactive.list('a) = {
    Cursor.t('a) cursor,
    ('a->xhtml) itemFunc,
    (->xhtml) emptyFunc,
    ('a, int -> void) add,
    ('a->void) push,
    ('a, int, int -> void) move,
    (->int) length,
    ('a, int -> void) change,
    ('a, int -> void) remove,
    (->list('a)) get,
    (list('a)->void) set,
    (->bool) is_empty,
    ('a->bool) mem,
}

private CoreList = List

module Reactive {

    /**
        Utils
    */
    private server unique_class = String.fresh(200)
    private function xts(xhtml) {
        Xhtml.serialize_as_standalone_html(xhtml);
    }

    private function `@>>`(g,f) { function() { f(g()) } }

    /**
        Table of all the Reactive values on the client
    */
    private client client_side_reactive_table = (Hashtbl.t(string, Reactive.value(black))) Hashtbl.create(1)
    private client function Reactive.value('a) get_or_make(id, network, 'a v) {
        match(Hashtbl.try_find(client_side_reactive_table, id)){
        case {some:r} : @unsafe_cast(r)
        case {none} : make_on_client(id, network, v);
        }
    }

    /**
        Serialization / Unserialization.
    */
    private function unserialize_error(json) { void @fail("Impossible to unserialize the reactive value: {json}"); none }

    // Impossible to write a generic unserialize function (value restrictions)
    private client function option(Reactive.value('a)) client_unserialize(json) {
        alpha_unserialization = OpaSerialize.finish_unserialize(_, @typeval('a))
        match (json){
        case { Record : [ ("initial_value", alpha), ("id", { String : id }) ] } :
            value = (alpha_unserialization(alpha) ? @fail)
            get_or_make(string id, none, value)
            |> some(_)
        default : unserialize_error(json);
        }
    }

    private server function option(Reactive.value('a)) server_unserialize(json) {
        alpha_unserialization = OpaSerialize.finish_unserialize(_, @typeval('a))
        match (json){
        case { Record : [ ("initial_value", alpha), ("id", { String : id }) ] } :
            value = (alpha_unserialization(alpha) ? @fail)
            make_on_server(string id, none, value)
            |> some(_)
        default : unserialize_error(json);
        }
    }

    function RPC.Json.json serialize(Reactive.value('a) reactive, OpaSerialize.options opt) {
        alpha_serialization = OpaSerialize.partial_serialize_options(_, @typeof(reactive), opt)
        initial_value =
            reactive.get_silently()
            |> alpha_serialization(_);
        { Record : [ ("initial_value", initial_value), ("id", { String : reactive.id }) ] }
    }


    function option(Reactive.value('a)) unserialize(RPC.Json.json j) {
        @sliced_expr( { client : client_unserialize(j),
                        server : server_unserialize(j) }
                    )
    }

    private function make_on_client(id, network, v){
        make_on_client_with_ty(id, network, v, @typeof(v))
    }

    /**
        Reactive.make on the client side.
    */
    private client (string,option(Network.network('a)), 'a, OpaType.ty->Reactive.value('a)) function make_on_client_with_ty(id, network, v, ty) {
        value = Mutable.make(v)
        ctx_table = Hashtbl.t(int, Context.t) Hashtbl.create(1)

        function get() {
            ctx = Context.get()
            ctx_id = Context.getId(ctx)
            if (ctx_id > -1) {
                Hashtbl.add(ctx_table, ctx_id, ctx)
                Context.onInvalidate(ctx,{ function() Hashtbl.remove(ctx_table, ctx_id)})
            }
            v = value.get()
            v
        }

        function set(n) {
            value.set(n)
            keys = Hashtbl.bindings(ctx_table);
            LowLevelArray.iter({ function(v) Context.invalidate(v.value)}, keys)
        }

        match (network){
        case {some:network}: Network.add_callback({ function(n) set(n) }, network);
        case {none}: void;
        }

        function get_silently() {
            value.get()
        }

        function set_silently(v) {
            value.set(v)
        }

        Reactive.value('a) self = {~id, ~get, ~set, ~get_silently, ~set_silently, ~ty,
                function render(_){ <></> },
                function render_value(_){ <></> }
        }

        function self_render(html_fun) {
           render(html_fun)(self)
        }

        function self_render_value(value_fun) {
           render_value(value_fun)(self)
        }

        self = { self with render:self_render, render_value:self_render_value }

        Hashtbl.add(client_side_reactive_table, id, @unsafe_cast(self));

        self
    }

    /**
        Reactive.make on the server side.
    */
    private gom = get_or_make
    private client function get_value_from_client(id,network,v)() { gom(id,network,v)|>_.get() }
    private client function set_value_on_client(id,network)(v) { gom(id,network,v)|>_.set(v) }
    private client function get_value_silently_from_client(id,network,v)() { gom(id,network,v)|>_.get_silently() }
    private client function set_value_silently_on_client(id,network)(v) { gom(id,network,v)|>_.set_silently(v) }

    private function make_on_server(id, network, v){
        make_on_server_with_ty(id, network, v, @typeof(v))
    }

    private server (string,option(Network.network('a)),'a, OpaType.ty ->Reactive.value('a)) function make_on_server_with_ty(id,network,v,ty) {

        value = Mutable.make(v)

        /** TODO: add in the stdlib:
        exposed function current_client_is_ready(){
            match (ThreadContext.get({current}).key){
            case ~{ `client` } :
                match (ClientEvent.ClientTbl.try_find((ClientEvent.state_tbl), `client` )){
                case {some:_} : true
                default : false
                }
            default : false
            }
        }
        */

        function client_exists() {
            b = true // TODO: current_client_is_ready()
            //jlog("Client ready: {b}");
            b
        }

        function get_silently() {
            if(client_exists()) {
                get_value_silently_from_client(id,network,v)()
            }else{
                value.get();
            }
        }

        function set_silently(v) {
            if(client_exists()) {
                set_value_silently_on_client(id,network)(v)
            }else{
                value.set(v);
            }
        }

        function get() {
            if(client_exists()) {
                get_value_from_client(id,network,v)()
            }else{
                get_silently();
            }
        }

        function set(v) {
            if(client_exists()) {
                set_value_on_client(id,network)(v)
            }else{
                set_silently(v);
            }
        }

        self = {~id, ~get, ~set, ~get_silently, ~set_silently, ~ty,
                function render(_){ <></> },
                function render_value(_){ <></> },
        }

        function self_render(html_fun) {
           render(html_fun)(self)
        }

        function self_render_value(value_fun) {
           render_value(value_fun)(self)
        }

        { self with render:self_render, render_value:self_render_value }
    }

    /**
        Create a new Reactive value
    */
    ('a->Reactive.value('a)) function make(v) {
        id = Random.base64_url(6);
        @sliced_expr({
            client : make_on_client(id,none,v),
            server : make_on_server(id,none,v)
        })
    }

    /**
        Default function to display the reactive value in html.
    */
    client function default_html_func(string id, RPC.Json.json json, ty, (xhtml->xhtml) xhtml_deco) {
        v = OpaSerialize.Json.unserialize_with_ty(json, ty) ? @fail
        Reactive.value('a) r = @unsafe_cast(get_or_make(id,none,@unsafe_cast(v)))
        x = xhtml_deco(XmlConvert.of_alpha_with_ty(ty,r.get()))
        Log.notice("html_func", "{Debug.dump(x)} / {Debug.dump(r.get())}")
        x
    }

    client function default_value_func(string id, RPC.Json.json json, ty, ('a->xhtml) xhtml_deco) {
        v = OpaSerialize.Json.unserialize_with_ty(json, ty) ? @fail
        Reactive.value('a) r = @unsafe_cast(get_or_make(id,none,@unsafe_cast(v)))
        x = xhtml_deco(r.get())
        x
    }

    // The function is not inside render with a client directive on purpose
    // (OPA BUG: can't have a local function with client directive, cf serialization optimization)
    private function replace(html_func,class)(Dom.event _ev) {
        function() { Spark.isolate({ function() xts(html_func()) } ) }
        |> Spark.render_f
        |> Spark.replace_f(Dom.select_class(class), _)
        |> ignore
    }

    /**
        Render a reactive value in html.
        It will be automatically re-rendered each time the reactive value is modified.
    */
    private function placeholder((->xhtml) html_func) {
        class = "__{unique_class()}"
        <span class={[class]} onready={replace(html_func,class)}/>
    }

    function render(html_fun)(r) {
        placeholder(
            v = r.get_silently()
            // Impossible to public_env with unknown 'a (cf EI)
            // this is why we store the type in r.ty and serialize by hand:
            json = OpaSerialize.partial_serialize(v, r.ty)
            @public_env(function() { default_html_func(r.id, json, r.ty, html_fun) })
        )
    }

    function render_value(value_fun)(r) {
        placeholder(
            v = r.get_silently()
            // Impossible to public_env with unknown 'a (cf EI)
            // this is why we store the type in r.ty and serialize by hand:
            json = OpaSerialize.partial_serialize(v, r.ty)
            @public_env(function() { default_value_func(r.id, json, r.ty, value_fun) })
        )
    }

    client function bind(dom dom, Dom.event.kind e, (Dom.event -> void) f) {
        Scheduler.push(
            { function() Dom.bind(dom, e, f) |> ignore }
        )
    }

    ('a,string->Reactive.value('a)) function make_sync(init, network_name) {

        Network.network('a) network = Network.cloud(network_name)

        id = Random.base64_url(6);
        reactive = make_on_server(id, some(network), init)

        //Network.add_callback({ function(n) reactive.set(n) }, network);

        function set(n) {
            Network.broadcast(n, network);
        }

        { reactive with ~set }
    }

    @xmlizer(Reactive.value('a)) function xhtml to_xml(Reactive.value('a) r) {
        render(identity)(r)
    }

    module List {

        (list('a), ('a->xhtml), (->xhtml) -> Reactive.list('a)) function _make(init, itemFunc, emptyFunc) {

            cb_map = Mutable.make(StringMap.empty)
            list = Hashtbl.create(10)

            recursive function observe(cb) {
                id = Random.base64_url(6)
                CoreList.iteri(function(i, v) {add(v, i)}, init)
                cb_map.set(Map.add(id, cb, cb_map.get()))
            }

            and cursor = { ~observe }

            and function cb(f) {
                Map.iter({ function(_,cb) f(cb) }, cb_map.get())
            }

            // function iter('a) iter() {
            //   llarray = Hashtbl.bindings(list);
            //   LowLevelArray.iter({ function(v) Context.invalidate(v.value)}, keys)
            // }

            and function length() {
                Hashtbl.size(list)
            }

            and function add(v, index) {
                cb(_.added(v, index))
                Hashtbl.add(list, index, v)
            }

            and function push(v) {
                add(v, length())
            }

            and function move(v, from, to) {
                cb(_.moved(v, from, to))
                //Hashtbl.remove(list, from)
                Hashtbl.replace(list, to, v)
            }

            and function change(v, index) {
                cb(_.changed(v, index))
                Hashtbl.replace(list, index, v)
            }

            and function remove(v, index) {
                cb(_.removed(v, index))
                //Hashtbl.remove(list, index)
            }

            and function get() {
              llarray = Hashtbl.bindings(list);
              LowLevelArray.fold(function(b, acc) {
                [b.value|acc]
              }, llarray, [])
            }

            and function set(list('a) l) {
              llarray = Hashtbl.bindings(list);
              LowLevelArray.iter(function(b) {
                remove(b.value, b.key)
              }, llarray)
              CoreList.iter(push, l)
            }

            and function is_empty() {
              Hashtbl.is_empty(list)
            }

            and function mem(x) {
              llarray = Hashtbl.bindings(list);
              LowLevelArray.fold(function(b, acc) {
                b.value == x || acc
              }, llarray, false)
            }

            { ~cursor, ~itemFunc, ~emptyFunc, ~length, ~get, ~set, ~is_empty, ~mem, ~add, ~push, ~move, ~change, ~remove }

        }

        /**
          WIP
        */
        client function make_on_client(init, itemFunc, emptyFunc) {
            _make(init, itemFunc, emptyFunc)
        }

        server function make_on_server(init, itemFunc, emptyFunc) {
            _make(init, itemFunc, emptyFunc)
        }
        /**
            Create a new Reactive value
        */
        (list('a), ('a->xhtml), (->xhtml) -> Reactive.list('a)) function make(init, itemFunc, emptyFunc) {
            @sliced_expr({
                client : make_on_client(init, itemFunc, emptyFunc),
                server : make_on_server(init, itemFunc, emptyFunc)
            })
        }

        Reactive.list('a), ('a->xhtml), (->xhtml) -> Reactive.list('a) function clone(reactive, item_fun, empty_fun) {
            { reactive with itemFunc:item_fun, emptyFunc:empty_fun }
        }

        function render(cursor_getter, (->('a->xhtml)) item_func_getter, (->(->xhtml)) empty_func_getter) {
            class = "__{unique_class()}"
            client function replace(Dom.event _e) {
                // See comment in Reactive.render
                cursor = cursor_getter()
                item_func = item_func_getter()
                empty_func = empty_func_getter()
                Spark.render_f(function () {
                    Spark.list(cursor, function (item) {
                        // TODO: handle case when item has no _id field:
                        Spark.labelBranch(@unsafe_cast(item)._id, function () {
                            Spark.isolate({ function() item_func(item) } @>> xts);
                        })
                    }, (empty_func @>> xts))
                })
                |> Spark.replace_f(Dom.select_class(class), _)
                |> ignore
            }
            <span class={[class]} onready={replace} />
        }

        (list('a), ('a->xhtml), (->xhtml), string -> Reactive.list('a)) function make_sync(init, itemFunc, emptyFunc, network_name) {

            Network.network(Cursor.network_message('a)) network = Network.cloud(network_name)

            Reactive.list('a) list = make(list('a) init, itemFunc, emptyFunc)

            client function on_message(msg) {
                match(msg) {
                case { added : (v, index) }: list.add(v, index)
                case { moved : (v, from, to) }: list.move(v, from, to)
                case { changed : (v, index) }: list.change(v, index)
                case { removed : (v, index) }: list.remove(v, index)
                }
            }

            Network.add_callback(on_message, network);

            client function add(v, index) {
                Network.broadcast({ added : (v, index) }, network);
            }

            client function push(v) {
                add(v, list.length())
            }

            client function move(v, from, to) {
                Network.broadcast({ moved : (v, from, to) }, network);
            }

            client function change(v, index) {
                Network.broadcast({ changed : (v, index) }, network);
            }

            client function remove(v, index) {
                Network.broadcast({ removed : (v, index) }, network);
            }

            { list with ~add, ~push, ~move, ~change, ~remove}
        }

        @xmlizer(Reactive.list('a)) function xhtml to_xml(Reactive.list('a) r) {
            render({ function() r.cursor }, { function() r.itemFunc }, { function() r.emptyFunc })
        }
    }
}

@expand function render_list(r) {
    Reactive.List.render({ function() r.cursor }, { function() r.itemFunc }, { function() r.emptyFunc })
}
