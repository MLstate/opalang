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
 * A button widget
 *
 * @category WIDGET
 * @author Guillem Rieu, 2010
 * @destination PUBLIC
 * @stability TRANSITIONAL
 */

import stdlib.widgets.core

/**
 * {1 Types defined in this module}
 */

type WButton.config = {
    button_type : {button} / {reset} / {submit}
    image: option(string) /** Image to display on the button (optional) */

    common_style: WStyler.styler /** Style common to all button states */

    default_style: WStyler.styler /** Default style of a button */
    over_style: WStyler.styler /** Style of a focused button */
    active_style: WStyler.styler /** Style of a button being activated (i.e. pushed) */
    disabled_style: WStyler.styler /** Style of a button being disabled */
    toggled_style: WStyler.styler /** Style of a "toggled" button */
 }

WButton = {{

/**
 * {1 Configuration}
 */

  /**
   * Empty config of a WButton.
   */
  bare_config : WButton.config = {
    image = none
    button_type = {button}
    common_style = WStyler.empty
    default_style = WStyler.empty
    over_style = WStyler.empty
    active_style = WStyler.empty
    disabled_style = WStyler.empty
    toggled_style = WStyler.empty
  }

  /**
   * The default config of a WButton. CSS is taken from
   * the BlueTrip CSS Framework (released in the public domain).
   * See http://bluetrip.org/
   */
  default_config : WButton.config = {
      image = none
      button_type = {button}

      common_style = WStyler.make_style(
          Css_build.font2( [{bold}], {percent=100.},
                           some({percent=130.}), {Verdana}) +>
          css {
            display: inline-block;
            margin: 0 0.583em 0.667em 0;
            padding: 5px 10px 5px 7px;   /** Links **/
            border: 1px solid #dedede;
            border-top: 1px solid #eee;
            border-left: 1px solid #eee;
            background: #f5f5f5;
            color: #565656;
            cursor: pointer;

//             width: auto;
            overflow: visible;
            padding: 4px 10px 3px 7px;
          })

      default_style = WStyler.empty

      over_style = WStyler.make_style(css {
          background: #dff4ff;
          border: 1px solid #c2e1ef;
          color: #336699;
        })

      active_style = WStyler.make_style(css {
          background: #6299c5;
          border: 1px solid #6299c5;
          color: #fff;
        })
      disabled_style = WStyler.make_style(css {
          border: 1px solid #eee;
          color: #A6A6A6;
        })
      toggled_style = WStyler.make_style(css {
          border-left: 1px solid #6299c5;
          border-top: 1px solid #6299c5;
          border-right: 1px solid #5289b5;
          border-bottom: 1px solid #5289b5;
        })
    }

 /**
   * A default config with a green theme, well suited for
   * 'OK' and validating buttons.
   */
  green_config : WButton.config = {
      default_config with
        over_style = WStyler.make_style(css {
          background: #E6EFC2;
          border: 1px solid #C6D880;
          color: #529214;
        })

      active_style = WStyler.make_style(css {
          background: #529214;
          border: 1px solid #529214;
          color: #fff;
        })
      toggled_style = WStyler.make_style(css {
          border-left: 1px solid #529214;
          border-top: 1px solid #529214;
          border-right: 1px solid #428204;
          border-bottom: 1px solid #428204;
        })
  }

  /**
   * A default config with a green theme, well suited for
   * 'Cancel' and denying buttons.
   */
  red_config : WButton.config = {
    default_config with
      over_style = WStyler.make_style(css {
          background:#fbe3e4;
          border:1px solid #fbc2c4;
          color:#d12f19;
        })

      active_style = WStyler.make_style(css {
          background: #d12f19;
          border: 1px solid #d12f19;
          color:#fff;
        })
      toggled_style = WStyler.make_style(css {
          border-left: 1px solid #d12f19;
          border-top: 1px solid #d12f19;
          border-right: 1px solid #c11f09;
          border-bottom: 1px solid #c11f09;
        })
    }

/**
 * {1 High-level interface}
 */

  /**
   * Constructs an instance of the button widget as a XHTML value
   */
  html(config: WButton.config, id: string,
      handles: list((Dom.event.kind, (Dom.event -> void))), content: xhtml)
      : xhtml =
    inner_id = get_inner_id(id)
    button_type_string =
    | {button} -> "button"
    | {reset} -> "reset"
    | {submit} -> "submit"
    style(stl)(_) = apply_style(config, id, stl)
     /* ADAM is it a good idea to stop propagation of onclick events for buttons?
        I think in most cases this is what one wants... how about other events? */
    <button id={inner_id} type={button_type_string(config.button_type)}
      onmouseover={style(config.over_style)}
      onmouseout={style(config.default_style)}
      onmousedown={style(config.active_style)}
      onmouseup={style(config.over_style)}
      options:onclick="stop_propagation"
    >
    { match config.image with
      | {none} ->  <></>
      | {some=img} -> <img src={img} alt="" />}
      {content}
    </button>
    |> WStyler.set(button_style(config, config.default_style), _)
    |> WCore.add_binds(handles, _)

/**
 * {2 Imperative interface}
 */

  set_enabled_state(config: WButton.config, id: string, enabled: bool) : void =
    do Dom.set_enabled(#{get_inner_id(id)}, enabled)
    do apply_style(config, id, if enabled then WStyler.empty else config.disabled_style)
    void

  set_enabled = set_enabled_state(_, _, true)
  set_disabled = set_enabled_state(_, _, false)

  set_toggle_state(config: WButton.config, id: string, toggled: bool) : void =
    do Dom.set_checked(#{get_inner_id(id)}, toggled)
    do apply_style(config, id, if toggled then config.toggled_style else WStyler.empty)
    void

  set_toggled = set_toggle_state(_, _, true)
  unset_toggled = set_toggle_state(_, _, false)

  /**
   * {2 Old and deprecated button}
   */

  @deprecated({use = "WButton.html or WSimpleButton.html"})
  make_default(click_handle, label) =
    <button onclick={click_handle}>{label}</button>
      |> WStyler.add(default_config.common_style, _)

  /**
   * {1 Private functions}
   */

  @private get_inner_id(id) = id ^ "_button"

  @private button_style(config, stl) =
    WStyler.merge([config.common_style, stl])

  @private apply_style(config, id, stl) =
    inner_id = get_inner_id(id)
    style = button_style(config, stl)
    WStyler.set_dom(style, inner_id)

}}

/**
 * A button with 'onclick' as the only possible event
 */
WSimpleButton = {{

  /**
   * DEPRECATED
   * Old and limited button widget
   */
  html(id: string, handle : Dom.event -> void, content : string) : xhtml =
    <input id="{id}_input" type="button" value={content} onclick={handle} />

}}

