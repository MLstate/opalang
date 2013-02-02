/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
