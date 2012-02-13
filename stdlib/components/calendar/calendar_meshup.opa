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
import stdlib.web.client

// ***************************************************************************************
/**
 * {1 Types}
**/
// ***************************************************************************************

type CCalendar.Meshup.config('event) =
{
  id : string
  calendar_style : CCalendar.Style.config
  controls_style : CCalendarControls.config('event)
  loading_html : xhtml
}

type CCalendar.Meshup.callbacks('event) =
{
  cal_callbacks : CCalendar.callbacks('event)
  cal_created : CCalendar.instance('event) -> void
}

// ***************************************************************************************
/**
 * {1 Implementation}
**/
// ***************************************************************************************

@client CCalendarMeshup = {{

  @private main_id(id) = "{id}_main"
  @private controller_id(id) = "{id}_controller"

  empty_callbacks : CCalendar.Meshup.callbacks =
  {
    cal_callbacks = CCalendar.empty_callbacks
    cal_created(_) = void
  }

  default_config(id : string) : CCalendar.Meshup.config =
  {
    ~id
    calendar_style = CCalendar.google_style_config
    controls_style = CCalendarControls.google_style_config
    loading_html =
      load_style = css { vertical-align: middle }
      <div style={load_style}>Loading the calendar... please wait</>
  }

  @private setup_calendar(config, callbacks : CCalendar.Meshup.callbacks, calendar) =
    id = config.id
    controller_config = config.controls_style
    do CCalendarControls.create(controller_id(id), controller_config, calendar)
    do CCalendar.redraw(calendar)
    do callbacks.cal_created(calendar)
    void

  create( config : CCalendar.Meshup.config
        , callbacks : CCalendar.Meshup.callbacks('event)
        , event_config : CCalendar.Event.config('event)
        ) : (CCalendar.instance, xhtml) =
    id = config.id
    main_config = CCalendar.default_config(main_id(id), event_config,
      config.calendar_style)
    calendar = CCalendar.create(main_config, callbacks.cal_callbacks)
    xhtml =
       /* FIXME for some reason I didn't manage to replace the cellpadding
          attribute below with CSS */
      <table class="CCalendar_container" cellpadding="0"
        onready={_ -> setup_calendar(config, callbacks, calendar)}>
        <tr>
          <td id=#{controller_id(id)} />
        </>
        <tr>
          <td class="CCalendar_main" id=#{main_id(id)}>{config.loading_html}</>
        </>
      </>
      |> style_css(css
           { padding: 0px
           ; border-spacing: 0px
           ; border-collapse: collapse
           })
    (calendar, xhtml)

  shutdown(cal : CCalendar.instance) =
    CCalendar.shutdown(cal)

}}

default_resizable = css
  html, body {
    height: 100%;
    margin: 0px;
  }
  table {
    border-spacing: 0px
  }
  .CCalendar_container {
    height: 100%;
    width: 100%;
  }
  .CCalendar_main {
    height: 100%;
    vertical-align: top;
    overflow: hidden;
  }

ccalendar_meshup_css =
  [ ccalendar_controls_css
  , ccalendar_extensible_style_css
  , ccalendar_google_style_css
  , ccalendar_bootstrap_style_css
  , default_resizable
  ]
