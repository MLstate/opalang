/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * A configurable radiobutton widget.
 *
 * @author Jessica Castejon, 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

package stdlib.widgets.radiobutton

import stdlib.widgets.core
import stdlib.interactions.editable

/*
 * {1 About this module}
 *
 * Radiobutton widget
 * Inspired by http://jqueryui.com/themeroller/
 *
 */


type WRadiobutton.display = {vertical} / {horizontal}

type WRadiobutton.state = {checked} / {unchecked} / {invalid}

/**
 * Parameters of a radiobutton
 */
type WRadiobutton.parameters = {
  state: WRadiobutton.state
  value: string
  id: string
  text: string
}

/**
 * Configuration of a radiobutton
 */
type WRadiobutton.config = {
     name: string
     editable: bool
     display: WRadiobutton.display
     global_style: WStyler.styler
     lbl_style: WStyler.styler
     lbl_checked_style: WStyler.styler
     lbl_unchecked_style: WStyler.styler
     lbl_invalid_style: WStyler.styler
     other_stylers: IEditable.stylers
}

WRadiobutton = {{
/**
 *Default configuration of a widget
 */
  default_config = {
    name = "radiobutton"
    editable = true
    display = {horizontal}
    global_style = WStyler.empty
    lbl_style = WStyler.empty
    lbl_checked_style = WStyler.empty
    lbl_unchecked_style = WStyler.empty
    lbl_invalid_style = WStyler.empty
    other_stylers = IEditable.default_config.stylers
  }

/*
 *Displays a non-editable radio button
 *
 *@param config The widget configuration
 *@param parameters The button parameters
 *@return The HTML corresponding to the button
 */
  @private
  show(config: WRadiobutton.config, parameters: WRadiobutton.parameters): xhtml =
    button_input = match parameters.state with 
                   |{checked} -> <input type="radio" id="{parameters.id}" class="internal_opa_checked" name="{config.name}" value="{parameters.value}" readonly="readonly" disabled="disabled" checked="checked"/>
                   |{unchecked} -> <input type="radio" id="{parameters.id}" class="internal_opa_unchecked" name="{config.name}" value="{parameters.value}" readonly="readonly" disabled="disabled"/>
                   |{invalid} -> <input type="radio" id="{parameters.id}" class="{parameters.state}" name="{config.name}" value="{parameters.value}" readonly="readonly" disabled="disabled"/>
    WStyler.add(config.other_stylers.show,button_input)


/*
 *Displays a non-editable radio button with its label
 *
 *@param config The widget configuration
 *@param parameters The button parameters
 *@return The HTML corresponding to the button
 */
  @private
  show_with_label(config: WRadiobutton.config, parameters: WRadiobutton.parameters): xhtml =
    button_input = show(config, parameters)
    button_input = WStyler.add(config.global_style,button_input)
    button_label = <label for="parameters.id">{parameters.text}</label>
    style = match parameters.state with
            |{checked} -> WStyler.merge([config.lbl_style,config.lbl_checked_style])
            |{unchecked} -> WStyler.merge([config.lbl_style,config.lbl_unchecked_style])
            |{invalid} -> WStyler.merge([config.lbl_style,config.lbl_invalid_style])
            |_ -> config.lbl_style
    button_label = WStyler.add(style,button_label)
    <>{button_input}
      {button_label}
    </>

/**
 *Main display function of the accordion
 *
 *@param config The widget configuration
 *@param buttons The parameters of the buttons of the widget
 *@param id The widget identifier
 *@return The HTML corresponding to the widget
 */
  html(config: WRadiobutton.config, buttons:list(WRadiobutton.parameters), id: string): xhtml =
    widget = match config.display with
             |{horizontal} -> <span id=#{id}>
                                    {get_buttons(config,buttons,id)}
                              </span>
             |_ -> <div id=#{id}>
                        {get_buttons(config,buttons,id)}
                   </div>
    WStyler.add(config.global_style,widget)

/*
 *Function that generates the HTML corresponding to a buttons list
 *
 *@param config The widget configuration
 *@param buttons The parameters of the buttons of the widget
 *@param id The widget identifier
 *@return A list containing the HTML of each button
 */
  @private
  get_buttons(config: WRadiobutton.config, buttons:list(WRadiobutton.parameters), id: string): list(xhtml) =
    add_button(parameters)=
      if config.editable
      then (
      (style, button_input) = match parameters.state with
              |{checked} -> (WStyler.merge([config.lbl_style,config.lbl_checked_style]),<input type="radio" id="{parameters.id}" name="{config.name}" class="internal_opa_checked" value="{parameters.value}" checked="checked" onclick={_->change_state(config,parameters.id,id)}/>)
              |{unchecked} -> (WStyler.merge([config.lbl_style,config.lbl_unchecked_style]),<input type="radio" id="{parameters.id}" name="{config.name}" class="internal_opa_unchecked" value="{parameters.value}" onclick={_->change_state(config,parameters.id,id)}/>)
              |{invalid} -> (WStyler.merge([config.lbl_style,config.lbl_invalid_style]),<input type="radio" id="{parameters.id}" name="{config.name}" class="internal_opa_invalid" value="{parameters.value}" disabled="disabled" onclick={_->change_state(config,parameters.id,id)}/>)
      button_label = <label id="{parameters.id}_label" for="{parameters.id}" onclick={_->change_state(config,parameters.id,id)}>{parameters.text}</label>
      button_label = WStyler.add(style,button_label)
      button_input = WStyler.add(config.global_style,button_input)
      match config.display with
      |{vertical} -> <>{button_input}
                         {button_label}
                         <br/>
                       </>
      |_ -> <>{button_input}
              {button_label}
            </>
      )
      else show_with_label(config,parameters)
    List.map(add_button,buttons)


/**
 *Tests whether a button is checked or not
 *
 *@param id The button identifier
 *@param id_widget The widget identifier
 */
  is_checked(id: string, id_widget: string): bool =
    Dom.has_class(Dom.select_inside(Dom.select_id(id_widget),Dom.select_id(id)),"internal_opa_checked")

/**
 *Tests whether a button is valid or not
 *
 *@param id The button identifier
 *@param id_widget The widget identifier
 */
  is_enabled(id: string, id_widget:string): bool =
    not(Dom.has_class(Dom.select_inside(Dom.select_id(id_widget),Dom.select_id(id)),"internal_opa_invalid"))


/**
 *Gives the value corresponding to the chosen button of the widget
 *
 *@param id_widget The widget identifier
 *@return The value corresponding to the chosen button
 */
  get_value(id_widget: string): string =
    dom_checked = Dom.select_inside(Dom.select_id(id_widget),Dom.select_class("internal_opa_checked"))
    Dom.get_value(dom_checked)

/**
 *Enables a button of the widget
 *
 *@param config The widget configuration
 *@param if The button identifier
 *@param id_widget The widget identifier
 *@param state The configuration option (true to enable, false to disable)
 */
  do_enable(config: WRadiobutton.config, id: string, id_widget: string, state: bool): void =
    dom_input = Dom.select_inside(Dom.select_id(id_widget),Dom.select_id(id))
    is_checked = Dom.has_class(dom_input,"internal_opa_checked")
    (remove_class, add_class, style) = if state
    then ("internal_opa_invalid","internal_opa_unchecked",WStyler.merge([config.lbl_style,config.lbl_unchecked_style]))
    else ("internal_opa_unchecked","internal_opa_invalid",WStyler.merge([config.lbl_style,config.lbl_invalid_style]))
    if not(is_checked)
    then (
         do Dom.set_enabled(dom_input,state)
         do Dom.remove_class(dom_input,remove_class)
         do Dom.add_class(dom_input,add_class)
         WStyler.set_dom(style,"{id}_label")
    )


/**
 *Function that checks an unchecked button
 *
 *@param config The widget configuration
 *@param id The button identifier
 *@param id_widget The widget identifier
 */
  do_check(config: WRadiobutton.config, id: string, id_widget: string): void =
    change_state(config, id, id_widget)

/*
 *Function called when the user clicks on the radio button or the label
 *attached to the button
 *
 *@param config The widget configuration
 *@param id The button identifier
 *@param id_widget The widget identifier
 */
  @private
  change_state(config: WRadiobutton.config, id: string,id_widget: string): void =
    dom_input = Dom.select_id(id)
    is_checked = Dom.has_class(dom_input,"internal_opa_checked")
    is_invalid = Dom.has_class(dom_input,"internal_opa_invalid")
    if is_checked||is_invalid
    then void
    else
      check(config,id,id_widget,dom_input)


/*
 *Function that applies the changes on a button and its label when clicked
 *
 *@param config The widget configuration
 *@param id The button identifier
 *@param id_widget The widget identifier
 *@param dom_input The button to modify
 */
  @private
  check(config: WRadiobutton.config, id: string, id_widget: string, dom_input: dom): void =
    dom_checked = Dom.select_inside(Dom.select_id(id_widget),Dom.select_class("internal_opa_checked"))
    id_checked = Dom.get_id(dom_checked)
    style_checked = WStyler.merge([config.lbl_style,config.lbl_checked_style])
    style_unchecked = WStyler.merge([config.lbl_style,config.lbl_unchecked_style])
    do Dom.set_checked(dom_checked,false)
    do Dom.set_checked(dom_input,true)
    do Dom.remove_class(dom_checked,"internal_opa_checked")
    do Dom.add_class(dom_checked,"internal_opa_unchecked")
    do Dom.remove_class(dom_input,"internal_opa_unchecked")
    do Dom.add_class(dom_input,"internal_opa_checked")
    do WStyler.set_dom(style_unchecked,"{id_checked}_label")
    WStyler.set_dom(style_checked,"{id}_label")
}}
