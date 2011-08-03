/*
    Copyright Â© 2011 MLstate

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

package opace

/**
 * {1 About this module}
 * Ace editor is a standalone code editor written in JavaScript. This
 * module provides a binding to this client code editor.
 *
 * {1 Where should I start?}
 * You should start by define an editable zone like that :
 * [<div id="myzone">This is an editable zone</div>]
 * Then you can use [Ace.edit("myzone")] this function replace the dom
 * node with id "myzone" by an Ace editor.
 * Finally you can access to Ace properties ('Set & Get Ace editor
 * properties') and do some action on your Ace editor ('Action on Ace
 * editor').
 */

/**
 * {1 Types defined in this module}.
 */
/**
 * Type of an Ace editor.
 */
type Ace.t = external

Ace = {{

  /**
   * {1 Initialization}
   */
  /**
   * [edit(id)] Create an ACE editor which edit element with the given
   * [id].
   */
  edit(id) = (%%plugin_ace.edit%%)(id)

  /**
   * {1 Set & Get Ace editor properties}
   */
  /**
   * [set_mode(ace, mode)] Load [mode] for the given [ace]
   * editor. List of mode are defined on ace plugin. If the mode
   * doesn't exists returns [false].
   */
  set_mode(ace:Ace.t, mode:string) =
    (%%plugin_ace.set_mode%%)(ace, mode)

  /**
   * Get content of ACE editor.
   */
  get_content = %%plugin_ace.get_content%%

  /**
   * Set content of ACE editor.
   */
  set_content = %%plugin_ace.set_content%%

  /**
   * Add an event handler.
   */
  add_event_listener(ace : Ace.t, kind : Dom.event.kind, action : -> void)  =
    (%%plugin_ace.add_event_listener%%)(ace, Dom.Event.get_name(kind), action)

  /**
   * {1 Action on Ace editor}
   */
  /**
   * Undo the last action.
   */
  undo = %%plugin_ace.undo%%

  /**
   * Redo the last action.
   */
  redo = %%plugin_ace.redo%%

  /**
   * Set editor as read only
   */
  read_only = %%plugin_ace.read_only%%

  /**
   * Set cursor position.
   */
  move_cursor = %%plugin_ace.move_cursor%%

}}
