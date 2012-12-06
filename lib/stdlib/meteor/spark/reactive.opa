/*
    Copyright © 2012 MLstate

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

// TODO: type origin = {server} or {client}

type Reactive.value('a) = {
    string id,
    (->'a) get,
    ('a->{}) set,
    (->'a) get_silently,
    ('a->{}) set_silently,
    ((xhtml->xhtml)->xhtml) render,
    (string->Reactive.value('a)) sync,
    OpaType.ty ty
}

@both_implem @serializer(Reactive.value('a)) serialization_reactive = (Reactive.serialize, Reactive.unserialize)

type serialized_reactive('a) = {
    string id,
    'a initial_value
}

type Reactive.list('a) = {
    Cursor.t('a) cursor,
    ('a->xhtml) itemFunc,
    (->xhtml) emptyFunc,
    ('a, int -> void) add,
    ('a, int, int -> void) move,
    ('a, int -> void) change,
    ('a, int -> void) remove
}

/**
    Fake type for the client_side_reactive_table
    storing different type of values
*/
private type black_t = external

CoreList = List

module Reactive {

    /**
        Utils
    */
    private server unique_class = String.fresh(200)
    private xts = Xhtml.serialize_as_standalone_html
    private function `@>>`(g,f) { function() { f(g()) } }

    /**
        Table of all the Reactive values on the client
    */
    private client client_side_reactive_table = (Hashtbl.t(string, Reactive.value(black_t))) Hashtbl.create(1)
    private client function Reactive.value('a) get_or_make(id, 'a v) {
        match(Hashtbl.try_find(client_side_reactive_table, id)){
        case {some:r} : @unsafe_cast(r)
        case {none} : make_on_client(id, v);
        }
    }

    /**
        Serialization / Unserialization.
    */
    private function unserialize_error(json) { void @fail("Impossible to unserialize the reactive value: {json}"); none }

    // Impossible to write a generic unserialize function (value restrictions)
    private client function client_unserialize(alpha_unserialization, json) {
        match (json){
        case { Record : [ ("initial_value", alpha), ("id", { String : id }) ] } :
            value = (alpha_unserialization(alpha) ? @fail)
            get_or_make(string id, value)
            |> some(_)
        default : unserialize_error(json);
        }
    }

    private server function server_unserialize(alpha_unserialization, json) {
        match (json){
        case { Record : [ ("initial_value", alpha), ("id", { String : id }) ] } :
            value = (alpha_unserialization(alpha) ? @fail)
            make_on_server(string id, value)
            |> some(_)
        default : unserialize_error(json);
        }
    }

    function serialize(alpha_serialization, Reactive.value('a) reactive, opt) {
        initial_value =
            reactive.get_silently()
            |> alpha_serialization(_, opt);
        { Record : [ ("initial_value", initial_value), ("id", { String : reactive.id }) ] }
    }


    function option(Reactive.value('a)) unserialize((RPC.Json.json->option('a)) u,j) {
        @sliced_expr( { client : @unsafe_cast(client_unserialize(@unsafe_cast(u),j)),
                        server : @unsafe_cast(server_unserialize(@unsafe_cast(u),j)) }
                    )
    }

    /**
        Reactive.make on the client side.
    */
    private client (string,'a->Reactive.value('a)) function make_on_client(id,  v) {
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

        function get_silently() {
            value.get()
        }

        function set_silently(v) {
            value.set(v)
        }

        ty = @typeof(v)

        Reactive.value('a) self = {~id, ~get, ~set, ~get_silently, ~set_silently, ~ty,
                function render(_){ <></> },
                sync:function(_){@fail("The reactive value is already in sync mode")}
        }

        function self_render(html_fun) {
           render(html_fun)(self)
        }

        self = { self with render:self_render }

        self_sync = sync(self)

        self = { self with sync:self_sync }

        Hashtbl.add(client_side_reactive_table, id, @unsafe_cast(self));

        self
    }

    /**
        Reactive.make on the server side.
    */
    private gom = get_or_make
    private client function get_value_from_client(id,v)() { gom(id,v)|>_.get() }
    private client function set_value_on_client(id)(v) { gom(id,v)|>_.set(v) }
    private client function get_value_silently_from_client(id,v)() { gom(id,v)|>_.get_silently() }
    private client function set_value_silently_on_client(id)(v) { gom(id,v)|>_.set_silently(v) }

    private server (string,'a->Reactive.value('a)) function make_on_server(id,v) {

        value = Mutable.make(v)

        function client_exists() {
            //b = ClientEvent.current_client_is_ready()
            false // TODO
        }

        function get_silently() {
            if(client_exists()) {
                get_value_silently_from_client(id,v)()
            }else{
                value.get();
            }
        }

        function set_silently(v) {
            if(client_exists()) {
                set_value_silently_on_client(id)(v)
            }else{
                value.set(v);
            }
        }

        function get() {
            if(client_exists()) {
                get_value_from_client(id,v)()
            }else{
                get_silently();
            }
        }

        function set(v) {
            if(client_exists()) {
                set_value_on_client(id)(v)
            }else{
                set_silently(v);
            }
        }

        ty = @typeof(v)

        self = {~id, ~get, ~set, ~get_silently, ~set_silently, ~ty,
                function render(_){ <></> },
                sync:function(_){@fail("The reactive value is already in sync mode")}
        }

        function self_render(html_fun) {
           render(html_fun)(self)
        }

        self = { self with render:self_render }

        self_sync = sync(self)

        { self with sync:self_sync }

    }

    /**
        Create a new Reactive value
    */
    ('a->Reactive.value('a)) function make(v) {
        id = Random.base64_url(6);
        @sliced_expr({
            client : make_on_client(id,v),
            server : make_on_server(id,v)
        })
    }

    /**
        Default function to display the reactive value in html.
    */
    client function default_html_func(string id, RPC.Json.json json, ty, (xhtml->xhtml) xhtml_deco) {
        v = OpaSerialize.Json.unserialize_with_ty(json, ty) ? @fail
        Reactive.value('a) r = @unsafe_cast(get_or_make(id,@unsafe_cast(v)))
        xhtml_deco(XmlConvert.of_alpha_with_ty(ty,r.get()))
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

    function bind(dom dom, Dom.event.kind e, (Dom.event -> void) f) {
        Scheduler.push(
            { function() Dom.bind(dom, e, f) |> ignore }
        )
    }

    exposed function network(name){
        Network.network('a) Network.cloud(name)
    }

    (Reactive.value('a)->(string->Reactive.value('a))) function sync(value)(network_name) {

        Network.network('a) network = network(network_name)

        Network.add_callback({ function(n) value.set(n) }, network);

        function set(n) {
            Network.broadcast(n, network);
        }

        { value with ~set }

    }


    @xmlizer(Reactive.value('a)) function to_xml(('a->xhtml) _alpha_to_xml, r) {
        render(identity)(r)
    }

    module List {

        client (list('a), ('a->xhtml), (->xhtml) -> Reactive.list('a)) function make(_init, itemFunc, emptyFunc) {

            cb_map = Mutable.make(IntMap.empty)
            new_id = Fresh.client(identity)

            function observe(cb) {
                cb_map.set(Map.add(new_id(), cb, cb_map.get()))
            }

            cursor = { ~observe }

            function cb(f) {
                Map.iter({ function(_,cb) f(cb) }, cb_map.get())
            }

            function add(v, index) {
                cb(_.added(v, index))
            }

            function move(v, from, to) {
                cb(_.moved(v, from, to))
            }

            function change(v, index) {
                cb(_.changed(v, index))
            }

            function remove(v, index) {
                cb(_.removed(v, index))
            }

            { ~cursor, ~itemFunc, ~emptyFunc, ~add, ~move, ~change, ~remove}

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
                        Spark.labelBranch(item._id, function () {
                            Spark.isolate({ function() item_func(item) } @>> xts);
                        })
                    }, (empty_func @>> xts))
                })
                |> Spark.replace_f(Dom.select_class(class), _)
                |> ignore
            }
            <span class={[class]} onready={replace} />
        }

        @xmlizer(Reactive.list('a)) function to_xml(_alpha_to_xml, r) {
            render({ function() r.cursor }, { function() r.itemFunc }, { function() r.emptyFunc })
        }
    }
}

@expand function render_list(r) {
    Reactive.List.render({ function() r.cursor }, { function() r.itemFunc }, { function() r.emptyFunc })
}