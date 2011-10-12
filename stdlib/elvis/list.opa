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
 * Elvis Lists
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Lists}
 *
 * Lists, menus, tabs...
 */

/**
 * {1 Theming}
 *
 * The list always has classes "mlstate", "elvis", "list".
 */

type EList.options('key) =
{
   items: list(('key, Elvis.elvis(Elvis.masked, Elvis.masked)))
}

type EList.sources =
{
}

@abstract type EList.implementation('key) = {
  dom:   dom
  id:    string
  current_display: Client.reference(Elvis.theme -> {xhtml: xhtml dom:dom})
  current_theme:   Client.reference(option(Elvis.theme))
  current_items:   Client.reference(list(('key, Elvis.elvis(Elvis.masked, Elvis.masked))))
}

type EList.elvis('key) = Elvis.elvis(EList.sources, EList.implementation('key))

EList =
{{
/**
 * {1 Constructors}
 */
   empty(): EList.elvis('a) =
      make({items = []})

   simple(items: list(Elvis.elvis(Elvis.masked, Elvis.masked))): EList.elvis(int) =
      make({items = List.map((item -> (Random.int(1000000), item)), items)})

   make(options: EList.options('a)): EList.elvis('a) =
   (
      id = "elist_{Random.string(32)}"
      dom     = Dom.select_id(id)
      display = make_display(options.items, id)
      current_display = Client_reference.create(display)
      current_theme   = Client_reference.create({none})
      current_items   = Client_reference.create(options.items)
      display(theme) =
      (
         do Client_reference.set(current_theme, {some = theme})
         Client_reference.get(current_display)(theme)
      )
      implementation = (~{dom id current_display current_theme current_items})
      Elvis.make({}, implementation, display)
   )

   default_options =
   {
      items = []
   }

/**
 * {1 Accessors}
 */

 /**
  * Change the items in this list. Redisplay the elvis if necessary.
  */
 set_items(list: EList.elvis('a), items: list(('a, Elvis.elvis(Elvis.masked, Elvis.masked)))): void =
 (
    implem = (Elvis.implem(list))

    //1. Rebuild function [display], so that further displays of this elvis are possible
    current_display = implem.current_display
    id              = implem.id
    display         = make_display(items, id)
    do Client_reference.set(current_display, display)

    //2. Update browser UI if the elvis is currently displayed
    current_theme   = implem.current_theme
    do match Client_reference.get(current_theme) with
     | {none} -> void //The elvis is not displayed for the moment
     | ~{some}->
        target  = Dom.resolve(implem.dom)
        content = display(some).dom
        _ = Dom.put_replace(target, content)
        void

    //3. Update reference
    do Client_reference.set(implem.current_items, items)

    void
 )

 set_items_nokey(list: EList.elvis('a), items: list(Elvis.elvis(Elvis.masked, Elvis.masked))): void =
 (
    set_items(list, List.map(item -> (Random.int(1000000), item), items))
 )

 /**
  * Add an item to the list, as first element
  *
  * Note: Behavior is undefined if the item is already displayed somewhere
  */
 add_item(list: EList.elvis('a), key: 'a, item: Elvis.elvis(Elvis.masked, Elvis.masked)): void =
 (
    implem = (Elvis.implem(list))

    do set_items(list, [(key, item) | Client_reference.get(implem.current_items)])
    void
 )

 /**
  * Remove an item from the list
  */
 remove_item(list: EList.elvis('a), key:'a): void =
 (
    implem = (Elvis.implem(list))

    do set_items(list, List.remove_p(((current_key, _) -> current_key == key), Client_reference.get(implem.current_items)))
    void
 )

  @private make_display(items:list(('a, Elvis.elvis(Elvis.masked, Elvis.masked))), id:string) =
   (
      display(theme) =
      (
        theme_classes= Elvis.Theme.get_classes(theme)
        xhtml =
          <ul id={id} class="{theme_classes} mlstate elvis list">
          {
              List.map((child -> <li class="list_child">{Elvis.for_display_in_theme(child.f2, theme)}</li>), items)
          }
          </ul>
        dom = Dom.of_xhtml(xhtml)
        ~{xhtml dom}
      )
      display
   )

}}

