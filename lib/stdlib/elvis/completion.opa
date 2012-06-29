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
 * Elvis Completions
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * An input with auto-completion.
 *
 * When users type, a popup menu appears, prompting to choose one value in the menu. Only these values can be accepted (note that the popup menu can
 * be programmed to display the value currently written by the user). Once the value is accepted, it is displayed as a button and the user can restart
 * typing another value, etc.
 */


type ECompletion.options('value) =
{
  welcome_text:            string
  accept_multiple_entries: bool
  suggest_completions:     string -> list('value)
  display_suggestion:      'value -> xhtml
  display_choice:          'value -> xhtml
  text_suggestion:         'value -> string
}

type ECompletion.sources('value) =
{
  added_value: Event.source({new:'value all:list('value)})
  //TODO: added_value
  //TODO: changed_value
}

@abstract type ECompletion.implem('value) = {}

type ECompletion.elvis('value) = Elvis.elvis(ECompletion.sources('value), ECompletion.implem('value))

ECompletion =
{{
   make(options: ECompletion.options('value)): ECompletion.elvis('value) =
   (
      //Setup events
      added_value_net         = Network.empty()

      //Setup UI
      elvis_input             = EInput.simple()
      elvis_accepted_list     = EList.empty()
      elvis_accepted_panel    = EPanel.make({EPanel.default_options with
         children   = [Elvis.pack(elvis_accepted_list)]
         classes    = ["completion", "choices"]
      })
      elvis_suggestions_list  = EList.empty()
      elvis_suggestions_panel = EPanel.make({EPanel.default_options with
         children   = [Elvis.pack(elvis_suggestions_list)]
         classes    = ["completion", "suggestions"]
         is_visible = {false}
      })
      elvis_panel       = EPanel.make({EPanel.default_options with
         classes  = ["completion", "root"]
         children = [Elvis.pack(elvis_input), Elvis.pack(elvis_suggestions_panel), Elvis.pack(elvis_accepted_panel)]
      })

      //Show/hide suggestions panel
      set_suggestions_visible(visible) =
      (
         EPanel.set_visible(elvis_suggestions_panel, visible)//Note: In the future, we could replace this by an animation
      )

      //When value is accepted, show it in [elvis_accepted], store it somewhere, clear [elvis_input], hide [elvis_suggestions_panel]
      on_value_accepted(value) = (
         do set_suggestions_visible({false})
         do EInput.set_text(elvis_input, "", {false})
         elvis_value_for_display = EClickable.simple(options.display_choice(value))
         do EList.add_item(elvis_accepted_list, value, Elvis.pack(elvis_value_for_display))
         //TODO: Store value
         //value_key   = Random.int(10000000) //An arbitrary key, used for storage. Big number to avoid collisions.
         //TODO: On single click upon [elvis_value], reselect value/text
         //TODO: On double click upon [elvis_value], remove value
         //TODO: Trigger event
         {}
      )

      //When text changes, show/hide suggestions
      on_changing_text(text) = (
         match options.suggest_completions(text) with
             []          -> //Hide suggestions
               do set_suggestions_visible({false})
               void
           | suggestions -> //Show suggestions
               do set_suggestions_visible({true})
               make_suggestion(value) = (
                   clickable = EClickable.simple(options.display_suggestion(value))
                   _ = Event.callback(Elvis.sources(clickable).chosen, (_ -> on_value_accepted(value)))
                   Elvis.pack(clickable)
               )
               elvis_suggestions = List.map(x -> (x, make_suggestion(x)), suggestions)
               do EList.set_items(elvis_suggestions_list, elvis_suggestions)
               void
      )
      _ = Event.callback(Elvis.sources(elvis_input).changing_text, on_changing_text)

      //Finish construction
      sources = {
        added_value = (added_value_net)
      }
      implem = {}
      display = elvis_panel.display
      Elvis.make(sources, implem, display)
   )
}}

