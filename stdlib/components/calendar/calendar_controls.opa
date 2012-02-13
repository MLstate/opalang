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
 * @category COMPONENT
 * @author Adam Koprowski, 2011
 */

import stdlib.widgets.core
import stdlib.widgets.button
import stdlib.widgets.datepicker
import stdlib.components.fragment

// ***************************************************************************************
/**
 * {1 Types}
**/
// ***************************************************************************************

type CCalendarControls.config('event) =
{
  generate : string, CCalendar.instance('event) -> xhtml
}

// ***************************************************************************************
/**
 * {1 Implementation}
**/
// ***************************************************************************************

@client CCalendarControls = {{

  @private generate_button(cal, style, cmd, id, content) =
    onclick(_) = CCalendar.perform(cal, cmd)
    WButton.html(style, id, [({click}, onclick)], content)

  @private make_buttons_style(class) =
    mk_button_stl(state) = WStyler.make_class([class, state])
    { WButton.bare_config with
      common_style = mk_button_stl("common")
      default_style = mk_button_stl("default")
      over_style = mk_button_stl("over")
      active_style = mk_button_stl("active")
      disabled_style = mk_button_stl("disabled")
      toggled_style = mk_button_stl("toggled")
    }

  @private register_date_range_view(cal) =
    update_date(_state, {ViewChanged=~{first_weekday mode}}) =
      {re_render=<>{CCalendar.date_range_string(first_weekday, mode)}</>}
    (date_range_xhtml, date_range_fragment) = CFragment.create(void, <></>, update_date)
    ViewChanged(vc) = CFragment.notify(date_range_fragment, {ViewChanged=vc})
    do CCalendar.perform(cal, {UpdateCallbacks=(callbacks -> {callbacks with ~ViewChanged })})
    date_range_xhtml

  extensible_style_config =
  {
    generate(id, cal) =
      date_range_xhtml = register_date_range_view(cal)
       // buttons
      button = generate_button(cal, make_buttons_style("ccalendar_ext_ctrl_btn"), _, _, _)
       // left panel
      today = button({GoToday}, "{id}_today", <>Today</>)
      next = button({Next}, "{id}_next", <>»</>)
      prev = button({Prev}, "{id}_prev", <>«</>)
      do_goto(date) = CCalendar.perform(cal, {SetDate=date})
      goto_ctrl = WDatepicker.edit_default(do_goto, "{id}_goto", Date.now())
      goto = <span>{goto_ctrl}</>
          |> style_stl(WStyler.make_class(["ccalendar_ext_ctrl_goto"]))
       // right panel
//      day_view = button({ChangeMode={day}}, "{id}_day", <>Day</>)
//      week_view = button({ChangeMode={week}}, "{id}_week", <>Week</>)
      two_weeks_view = button({ChangeMode={weeks=2}}, "{id}_two_weeks", <>2 Weeks</>)
      month_view = button({ChangeMode={month}}, "{id}_month", <>Month</>)
       // style
      left_panel_style = css { float: left }
      right_panel_style = css { float: right }
      panel_style = css
      {
        border: 1px solid #99BBE8;
        border-bottom: none;
        height: 22px;
        background: #D0DEF0
      }
       // html
      <div id={id} style={panel_style}>
        <span style={left_panel_style}>
          {today}{prev}{next}{date_range_xhtml}{goto}
        </>
        <span style={right_panel_style}>
          {two_weeks_view}{month_view}
        </>
      </>
  }

  bootstrap_style_config =
    extensible_style_config

  google_style_config =
  {
    generate(id, cal) =
      date_range_xhtml = register_date_range_view(cal)
       // buttons
      buttons_config = make_buttons_style("ccalendar_ext_ctrl_btn")
      blue_button = generate_button(cal, buttons_config, _, _, _)
      gray_button(op, id, label) =
        WSimpleButton.html(id, (_ -> CCalendar.perform(cal, op)), label)
        |> style_css(css { margin: 0px 5px })
       // left panel
      today = gray_button({GoToday}, "{id}_today", "Today")
      next = blue_button({Next}, "{id}_next", <>»</>)
      prev = blue_button({Prev}, "{id}_prev", <>«</>)
       // right panel
//      day_view = gray_button({ChangeMode={day}}, "{id}_day", "Day")
//      week_view = gray_button({ChangeMode={week}}, "{id}_week", "Week")
      two_weeks_view = gray_button({ChangeMode={weeks=2}}, "{id}_two_weeks", "2 Weeks")
      month_view = gray_button({ChangeMode={month}}, "{id}_month", "Month")
      do WButton.set_toggled(buttons_config, "{id}_month")
       // styles
      left_panel_style = css { float: left }
      right_panel_style = css { float: right }
      panel_style = css
      {
        height: 24px;
        background: #BCF;
        padding: 4px 10px 4px 5px;
        overflow: hidden;
      }
       // xhtml
      <div style={panel_style} id={id}>
        <span style={left_panel_style}>
          {today}{prev}{next}{date_range_xhtml}
        </>
        <span style={right_panel_style}>
          {month_view}{two_weeks_view}
        </>
      </>
  }

  create( id : string
        , config : CCalendarControls.config
        , cal : CCalendar.instance
        ) : void =
    do Dom.transform([#{id} <- config.generate(id, cal)])
    void

}}

ccalendar_controls_css = css
  td{}
  .ccalendar_ext_ctrl_btn
  {
    display: inline-block;
    cursor: pointer;
    line-height: 12px;
    heigth: 16px;
    background: transparent;
    padding: 2px 10px;
    border: 1px solid transparent;
    color: #555;
    font: normal 11px arial,tahoma,verdana,helvetica;
  }
  .ccalendar_ext_ctrl_btn.over
  {
    background: #E0EEFF;
    border: 1px solid #99BBE8;
  }
  .ccalendar_ext_ctrl_btn.active
  {
    background: #C0CEE0;
    border: 1px solid #99BBE8;
  }
  .ccalendar_ext_ctrl_goto input
  {
    border: 1px solid #7EADD9;
    padding: 3px 3px 0px;
    width: 70px;
    height: 16px;
    margin: 0px;
    margin-left: 5px;
    font: normal 11px arial,tahoma,verdana,helvetica;
  }

  .ccalendar_google_ctrl_btn
  {
    display: inline-block;
    color: #FFF;
    background: #68E;
    cursor: pointer;
    width: 29px;
    height: 17px;
    line-height: 3px;
    vertical-align: middle;
    font: Arial, sans-serif;
    border: 1px solid transparent;
    padding: 0px 2px;
    margin: 0px 3px;
    border-radius: 2px;
  }
  .ccalendar_google_ctrl_btn.active
  {
    border: 1px solid #CFB17F;
  }
