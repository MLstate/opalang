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
 * Elvis Inputs
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Polymorphic text input}
 *
 * A simple text field, holding a value of polymorphic type.
 *
 * Expected syntactic sugar:
 *
 * <EInput id=#foo default_value="Please enter some text" />
 *
 * Translated into:
 *
 * foo = let()
 * ...
 * bind(foo, EInput.make({EInput.default_options with default_value = "Please enter some text"}))
 */

/**
 * {1 Theming}
 *
 * The input always has classes "mlstate", "elvis", "input".
 * At start, the field also has class "init". This class disappears once the field is modified.
 * When filled with something incorrect, it has class "incorrect". When filled with something correct, it has class "correct".
 * If enabled, it has class "enabled", otherwise "disabled"
 */

/**
 * {1 Sources for this elvis}
 */
type EInput.sources('value) =
{
  changed_value:   Event.source({old:option('value) new:option('value) text:string})

  /**
   * An event sent when the text is being modified.
   * Use this e.g. for auto-completion.
   */
  changing_text:   Event.source(string)
  changed_focus:  Event.source({received}/{lost})
}

type EInput.options('value) =
{
  welcome_text:  string
  default_value: option('value)
  value_accept:  string  -> option('value)

  /**
   * Determine how the value should be displayed.
   * Used to initialize the elvis, or when the value is set with [set_value].
   */
  value_display: option('value) -> string
  size_columns:  int
  is_enabled:    bool
  is_password:   bool

  unfocus_validates: bool
  newline_validates:  bool
  tab_validates:     bool
}

@abstract type EInput.implementation('value) =
{
  /**
   * A reference to the dom node containing the real input
   */
  dom_input: dom

  /**
   * A reference to the dom node containing the welcome pseudo-input
   */
  dom_welcome: dom
  state: Client.reference((string, option('value)))
  options: EInput.options('value)
}

type EInput.elvis('a) = Elvis.elvis(EInput.sources('a), EInput.implementation('a))

@client EInput = {{
  /**
   * {2 Constructors}
   */
  simple(): EInput.elvis(string) =
  (
     make(default_options)
  )

  make(options: EInput.options('a)): EInput.elvis('a) =
  (
     value_accept = options.value_accept
     default_text = options.value_display(options.default_value)
     id_input     = "einput_{Random.string(32)}"
     dom_input    = Dom.select_id(id_input)
     id_welcome   = "einput_welcome_{Random.string(32)}"
     dom_welcome  = Dom.select_id(id_welcome)

     //Setup sources
     changed_value_net = Network.empty()
     changing_text_net = Network.empty()
     changed_focus_net = Network.empty()

     //Handle validation (and UI side effects)
     state = Client_reference.create(("", {none}))
     validator(text:string): void =
     (
         old = Client_reference.get(state)
         if old.f1 != text then
           new = value_accept(text)
             do match new with
                | {none} -> //Value rejected, set class correspondingly
                    do Dom.remove_class(dom_input, class_init)
                    do Dom.remove_class(dom_input, class_correct)
                    do Dom.add_class(dom_input,    class_incorrect)
                    void
                | {some = _} -> //Value accepted, set class correspondingly
                    do Dom.remove_class(dom_input, class_init)
                    do Dom.remove_class(dom_input, class_incorrect)
                    do Dom.add_class(dom_input, class_correct)
                    void
             do Client_reference.set(state, (text, new))
             do Network.broadcast({old=old.f2 ~new ~text}, changed_value_net)
             void
       )

     //Propagate information that input is changing
     text = Client_reference.create(default_text)
     input_changed(_) =
     (
         new_text = Dom.get_value(dom_input)
         if new_text != Client_reference.get(text) then
         (
            do Network.broadcast(new_text, changing_text_net)
            do Client_reference.set(text, new_text)
            void
         )
     )


     display(theme) =
     (
         theme_name = Elvis.Theme.get_classes(theme)

         //Handle substitution between a placeholder input containing a welcome text and an input actually used for input
         focus_real_input(enable) =
         (
            if enable then
            (
               do Dom.hide(dom_welcome)
               do Dom.show(dom_input)
               _ = Dom.give_focus(dom_input)
               void
            ) else (
               if Dom.get_value(dom_input) == "" then//No text entered, restore placeholder
                  do Dom.hide(dom_input)
                  do Dom.show(dom_welcome)
                  void
            )
         )
         xhtml = <input id={id_welcome}
                             class="{theme_name} mlstate elvis input {class_init} {if options.is_enabled then class_enabled else class_disabled}"
                             size={options.size_columns}
                             value={options.welcome_text}
                             onready={_ -> Dom.set_enabled(dom_welcome, options.is_enabled)}

                             //Setup substitution
                             onfocus={_ -> focus_real_input({true})}

                             type="text"
                       />
         <input id={id_input}
                             class="{theme_name} mlstate elvis input {class_init} {if options.is_enabled then class_enabled else class_disabled}"
                             size={options.size_columns}
                             value={default_text}
                             onready={_ -> Dom.set_enabled(dom_input, options.is_enabled)}

                             //Setup substitution
                             style="display:none"
                             onblur={_ -> do Network.broadcast({lost},    changed_focus_net); focus_real_input({false})}
                             onfocus={_ -> Network.broadcast({received}, changed_focus_net)}

                             //Setup validation of input
                             onchange={_ ->  if options.unfocus_validates then validator(Dom.get_value(dom_input))}
                             options:onchange="stop_propagation"
                             onnewline={_ -> validator(Dom.get_value(dom_input))}
                             options:onnewline="stop_propagation"

                             //Setup input change monitoring
                             oninput={input_changed}
                             options:oninput="stop_propagation"
                             onkeyup={input_changed}
                             options:onkeyup="stop_propagation"
                             onpaste={input_changed}
                             options:onpaste="stop_propagation"

                             //Setup password
                             type={if options.is_password then "password" else "text"}
                       />
        dom = Dom.of_xhtml(xhtml)
        ~{dom xhtml}
     )
     sources : EInput.sources('a) =
        {
           changed_value  = (changed_value_net)
           changing_text  = (changing_text_net)
           changed_focus  = (changed_focus_net)
        }
     implem : EInput.implementation('a) =
        ({
           ~dom_input
           ~dom_welcome
           ~state
           ~options
        })
     Elvis.make(sources, implem, display)
  )

  /**
   * A default set of options
   */
  default_options: EInput.options(string) =
          {default_value = {none}
           welcome_text  = ""
           value_accept  = Option.some
           value_display(x) = x?""
           size_columns  = 20
           is_enabled    = {true}
           is_password   = {false}
           unfocus_validates = {true}
           newline_validates  = {true}
           tab_validates     = {true}
         }

  int_options: EInput.options(int) =
          {default_value = {none}
           welcome_text  = ""
           value_accept  = Parser.try_parse(Rule.integer, _)
           value_display(x)= match x with {none} -> "" | ~{some} -> String.of_int(some)
           size_columns  = 20
           is_enabled    = {true}
           is_password   = {false}
           unfocus_validates = {true}
           newline_validates  = {true}
           tab_validates     = {true}
        }

  password_options: EInput.options(string) =
          {default_value = {none}
           welcome_text  = "password"
           value_accept  = Option.some
           value_display(x) = x?""
           size_columns  = 20
           is_enabled    = {true}
           is_password   = {true}
           unfocus_validates = {true}
           newline_validates  = {true}
           tab_validates     = {true}
          }

  /**
   * {2 Accessors}
   */

  /**
   * Reset the contents of this input
   *
   * @param trigger If true, inform sources of the change, otherwise remain silent.
   */
  reset(einput:EInput.elvis('a), trigger:bool): void =
  (
    implem    = (Elvis.implem(einput))
    options   = implem.options
    do set_text(einput,  options.value_display(options.default_value),  false)
    do set_value(einput, options.default_value, trigger)
    void
  )

  /**
   * {3 Get/set the polymorphic value behind this input}
   */
  get_value(einput:EInput.elvis('a)): option('a)     =
  (
     Client_reference.get((Elvis.implem(einput)).state).f2
  )
  set_value(einput:EInput.elvis('a), value:option('a), trigger:bool): void =
  (
    implem    = (Elvis.implem(einput))
    options   = implem.options
    dom       = Dom.resolve(implem.dom_input)
    old_value = Client_reference.get(implem.state)
    do Client_reference.set(implem.state, (options.value_display(value), value))
    do Dom.set_value(dom, options.value_display(value))
    do if trigger then
        Event.trigger(Elvis.sources(einput).changed_value, {old = old_value.f2 text=options.value_display(value) new = value})
    void
  )

  /**
   * {3 Get/set the text of this input}
   */
  get_text(einput: EInput.elvis('a)): string =
  (
     Dom.get_value((Elvis.implem(einput)).dom_input)
  )
  /**
   * @param as_user If true, simulate a user entering new content, updating sources, etc. Otherwise, just update the text.
   */
  set_text(einput:EInput.elvis('a), text:string, as_user:bool): void =
  (
    implem    = (Elvis.implem(einput))
    do Dom.set_value(implem.dom_input, text)
    do if as_user then Dom.trigger(implem.dom_input, {change})
    void
  )


  /**
   * {3 Get/set editability of this input}
   */
   is_editable(einput:EInput.elvis('a)): bool =
   (
     implem = (Elvis.implem(einput))
     Dom.is_enabled(implem.dom_input)
   )
   set_editable(einput:EInput.elvis('a), editable:bool): void =
   (
     implem = (Elvis.implem(einput))
     dom    = Dom.resolve(Dom.unsplit([implem.dom_input, implem.dom_welcome]))
     do Dom.set_enabled(dom, editable)
     do if editable then
         do Dom.remove_class(dom, class_disabled)
         do Dom.add_class(dom,    class_enabled)
         void
       else
         do Dom.remove_class(dom, class_enabled)
         do Dom.add_class(dom,    class_disabled)
         void
     void
   )

   @private class_init      = "init"
   @private class_correct   = "correct"
   @private class_incorrect = "incorrect"
   @private class_enabled   = "enabled"
   @private class_disabled  = "disabled"
}}
