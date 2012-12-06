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
    ('a, int, int -> void) moved,
    ('a, int -> void) changed,
    ('a, int -> void) removed
}

type Cursor.t('a) = {
    (Cursor.callback('a) -> void) observe,
}

type Reactive.value('a) = {
    (->'a) get,
    ('a->{}) set
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

client module Reactive {

    module Render {

        @both_implem unique_class = String.fresh(200)

        private function placeholder((->dom_element) frag) {
            class = "__{unique_class()}"
            function replace(_) {
                ignore(Spark.replace_f(Dom.select_class(class), frag));
            }
            <div class={[class]} onready={replace}/>
        }

        xts = Xhtml.to_string
        function `@>>`(g,f) {
            function() { f(g()) }
        }

        function value(htmlFunc) {
            function() { Spark.isolate(htmlFunc @>> xts) }
            |> Spark.render_f
            |> placeholder
        }

        function list(cursor, itemFunc, elseFunc) {
            Spark.render_f(function () {
                Spark.list(cursor, function (item) {
                    Spark.labelBranch(item._id, function () {
                        Spark.isolate({ function() itemFunc(item) } @>> xts);
                    })
                }, (elseFunc @>> xts))
            })
            |> placeholder
        }
    }

    ('a->Reactive.value('a)) function value(v) {

        value = Mutable.make(v)
        ctx_map = Mutable.make(intmap(Context.t) IntMap.empty)

        function get() {
            ctx = Context.get()
            ctx_map.set(Map.add(Context.getId(ctx), ctx, ctx_map.get()))
            // todo: context.onInvalidate()
            value.get()
        }

        function set(n) {
            value.set(n)
            Map.iter({ function(_, value) Context.invalidate(value)}, ctx_map.get())
        }

        {~get, ~set}
    }

    (list('a), ('a->xhtml), (->xhtml) -> Reactive.list('a)) function list(_init, itemFunc, emptyFunc) {

        new_id = Fresh.client(identity)

        cb_map = Mutable.make(IntMap.empty)

        cursor = {
            function observe(cb) {
                cb_map.set(Map.add(new_id(), cb, cb_map.get()))
            }
        }

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
}

@xmlizer(Reactive.value('a)) function reactive_to_xml(alpha_to_xml, r) {
    Reactive.Render.value({ function() alpha_to_xml(r.get()) })
}

@xmlizer(Reactive.list('a)) function reactive_list_to_xml(_alpha_to_xml, r) {
    Reactive.Render.list(r.cursor, r.itemFunc, r.emptyFunc)
}
