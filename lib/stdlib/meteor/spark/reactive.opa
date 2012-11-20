/*
    Copyright © 2011, 2012 MLstate

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

type Reactive.value('a) = {
    (->'a) get,
    ('a->{}) set,
    (->xhtml) html_func
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

module Reactive {

    private server unique_class = String.fresh(200)

    private xts = Xhtml.to_string
    private function `@>>`(g,f) { function() { f(g()) } }

    client ('a->Reactive.value('a)) function make(v) {

        value = Mutable.make(v)
        ctx_map = Mutable.make(intmap(Context.t) IntMap.empty)

        function get() {
            ctx = Context.get()
            ctx_id = Context.getId(ctx)
            ctx_map.set(Map.add(ctx_id, ctx, ctx_map.get()))
            // TODO: cleanup on invalidate
            v = value.get()
            v
        }
        function set(n) {
            value.set(n)
            Map.iter({ function(_, ctx_id) Context.invalidate(ctx_id)}, ctx_map.get())
        }

        function html_func() {
            <>{get()}</>
        }

        {~get, ~set, ~html_func}
    }

    function render((->(->xhtml)) html_func_getter) {
        class = "__{unique_class()}"
        client function replace(Dom.event _e) {
            // html_func_getter (->(->xhtml)) can be server side
            // and should return a client side rendering function
            // (isolate have to use a full client side version
            //  to avoid network ping/pang each rendering)
            html_func = html_func_getter()
            function() { Spark.isolate(html_func @>> xts) }
            |> Spark.render_f
            |> Spark.replace_f(Dom.select_class(class), _)
            |> ignore
        }
        <span class={[class]} onready={replace} />
    }

    @xmlizer(Reactive.value('a)) function to_xml(_alpha_to_xml, r) {
        render({ function() r.html_func })
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

// Macros can't be inside a module:

@expand function render(r) {
    Reactive.render({ function() r.html_func })
}

@expand function render_list(r) {
    Reactive.List.render({ function() r.cursor }, { function() r.itemFunc }, { function() r.emptyFunc })
}
