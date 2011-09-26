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
 * @category COMPONENT
 * @author Adam Koprowski, 2011
 * @stability WIP
 */

/* FIXME, after changing calendar style, there is some flickering;
          as if old one was still in the DOM? */

import stdlib.core.date
import stdlib.widgets.{core, button, grid}
import stdlib.components.fragment

// ***************************************************************************************
/**
 * {1 Types}
**/
// ***************************************************************************************

// [?] introduce keys for events, change event requesting model

type CCalendar.config('event) =
{
   /** DOM id at which the calendar should be placed */
  id : string
   /** First day of the week; usually Sunday or Monday */
  first_weekday : Date.weekday
   // [?] How do we want to handle internationalization?

  style_config : CCalendar.Style.config
  event_config : CCalendar.Event.config('event)
}

type CCalendar.Animation.duration =
    {slow}
  / {fast}
  / {default}
  / {millisec : int}

type CCalendar.Style.config =
{
  calendar_style : WStyler.styler
  animation : {no_animation}
            / {fade duration : CCalendar.Animation.duration}
            / {slide duration : CCalendar.Animation.duration}
  weeks_view :
  {
    main_table_style : WStyler.styler
    corner_style : WStyler.styler
    border_width_px : (int, int) // left & right border size
    day_cells :
    {
      header :
      {
        height_px : int
        render : { is_today:bool; is_topleftmost:bool }, Date.date -> xhtml
      }
      active_cell_style : WStyler.styler
      inactive_cell_style : WStyler.styler
      today_cell_style : WStyler.styler
    }
    top_header :
    {
      height_px : int
       /** asks for top-header cell content given day of the week */
      cell_content : Date.weekday -> xhtml
    }
    left_header : option(
    {
      width_px : int
       /** asks for left-header cell content given week number */
      cell_content : int -> xhtml
    })
    events :
    {
      height_px : int
      table_style : WStyler.styler
      event_button_style : { is_multiday: bool; color: color } -> WButton.config
      generate_content : { is_multiday: bool
                         ; time: Date.date
                         ; event_xhtml: xhtml
                         } -> xhtml
    }
  }
}

type CCalendar.Event.config('event) =
{
   /* A callback to query for all events in a given date range */
  request_events : DateRange.range -> list('event)
   /* Order on events */
   // [?] do we want to allow labelled (non-default) orders?
  order : order('event, Order.default)
   /* date range of the event */
  get_date_range : 'event -> DateRange.range
   /* is the event to be rendered as a multi-day box? */
  is_multiday : 'event -> bool
   /* event's color */  // [?] shouldn't we enforce same colors within a category?
  get_event_color : 'event -> color
   /* event category */
  get_category : 'event -> string
   /* A function that given an event and the information on calendar mode, renders it
      into XHTML. */
   /* [?] I guess we want more than a string here; on the other hand should the event
      "container" be provided by the calendar component? or its clients? */
  render : CCalendar.mode, 'event -> xhtml
}

type CCalendar.mode =
// TODO: Implement daily & weekly views for the calendar
//    {day : Date.date}
//  / {week : {start_at : Date.date}}
  / {weeks : { no:int; start_at:Date.date }}
  / {month : { month : Date.month; year : Date.year }}

type CCalendar.msg('event) =
  /* changing configuration of a running calendar */
   { ChangeConfig : CCalendar.config('event) }

  // ---------- data model manipulations ----------
  /* new event */
 / { AddEvent : 'event }
  /* removal of an event */
 / { RemoveEvent : 'event }
  /* modification of an event - with old & new values */
 / { ModifyEvent : { old : 'event; new : 'event } }
  /* add a new category */
 / { AddCategory : string }
  /* remove an old category */
 / { RemoveCategory : string }

  // ---------------- presentation ----------------
  /* changes the view of the calendar */
 / { SetMode : CCalendar.mode }
  /* changes the view of the calendar, while trying to retain the displayed date range */
 / { ChangeMode : /*{ day } / { week } / */ { weeks : int } / { month } }
  /* moves the current date by a given number of units (forward if value positive,
   * backward if negative). The units depend on the view, i.e. are either days, weeks
   * or months (see CCalendar.mode) */
 / { Move : int }
  /* moves the current date by one unit forward */
 / { Next }
  /* moves the current date by one unit backward */
 / { Prev }
  /* jumps to today */
 / { GoToday }
  /* jumps to a specific date */
 / { SetDate : Date.date }
  /* re-draw the calendar */
 / { Refresh }

  // ------------ component management ------------
  /* shutting down the calendar component */
 / { Shutdown }

  // ------------------ callbacks -----------------
 / { UpdateCallbacks : CCalendar.callbacks('event) -> CCalendar.callbacks('event) }

type CCalendar.callbacks('event) =
{
  /* the viewing mode or the visible date range has changed */
   ViewChanged : { mode : CCalendar.mode; first_weekday : Date.weekday } -> void
    // FIXME, first_weekday above is a bit ugly...
  /* click in the calendar (depending on the view, the date may be rounded off to a day
   * at noon (week/month views), or more precise (day view) */
   DayClick : Date.date -> void
  /* click on a particular event */
   EventClick : 'event -> void
}

@abstract
type CCalendar.internal_msg('event) =
     CCalendar.msg('event)
   /
     {Startup redraw_handler : Dom.event_handler}

@abstract type CCalendar.state('event) =
{
  config : CCalendar.config('event)
  mode : CCalendar.mode
  callbacks : CCalendar.callbacks('event)
  redraw_handler : option(Dom.event_handler)
}

@abstract type CCalendar.instance('event) = channel(CCalendar.internal_msg('event))

// ***************************************************************************************
/**
 * {1 Implementation}
**/
// ***************************************************************************************

@client CCalendar = {{

// ***************************************************************************************
  /**
   * {2 Private functions}
  **/
// ***************************************************************************************

  @private update_state_and_refresh(old_state, new_state) =
    do draw_calendar(some(old_state), new_state)
    {set = new_state}

  @private move_by(by : int, state : CCalendar.state) =
    mode =
      match state.mode with
//      | ~{day} -> Date.advance(day, Duration.days(by))
//      | {week} -> Date.advance(state.date, Duration.weeks(by))
      | {weeks=~{no start_at}} ->
          {weeks={~no start_at=Date.advance(start_at, Duration.weeks(by))}}
      | ~{month} ->
          rec aux(x, date) =
            if x == 0 then
              date
            else
              cfg =
                if x > 0 then
                  { next_month=Date.Month.next
                    carry_month={january}
                    next_year=_ + 1
                    update_x = _ - 1
                  }
                else
                  { next_month=Date.Month.prev
                    carry_month={december}
                    next_year=_ - 1
                    update_x = _ + 1
                  }
              new_date =
                match cfg.next_month(date.month) with
                | {some=month} -> {date with ~month}
                | {none} -> {date with month=cfg.carry_month year=cfg.next_year(date.year)}
              new_x = cfg.update_x(x)
              aux(new_x, new_date)
          {month=aux(by, month)}
    update_state_and_refresh(state, {state with ~mode})

  @private calendar_shutdown(state) =
    do
      match state.redraw_handler with
      | {none} -> void
      | {some=handler} -> Dom.unbind(Dom.select_window(), handler)
    {stop}

  @private get_date(state) =
    match state.mode with
    | {month=~{month year}} ->
        Date.build(~{month year day=1})
    | {weeks=~{start_at ...}} ->
        start_at

  @private set_date(state, date) =
    mode =
      match state.mode with
      | {month=_} ->
          {month = {year=Date.get_year(date); month=Date.get_month(date)}}
      | {weeks=~{no start_at=_}} ->
           // let's try to put the 'date' in the center of the visible date range.
          start_at = Date.advance(date, Duration.weeks(-no/2))
          {weeks=~{no start_at}}
    {state with ~mode}

  @private change_mode(state, change_to) =
     // get current date
    date = get_date(state)
     // switch mode with dummy date
    new_mode =
      match change_to with
      | {weeks=no} -> {weeks={~no start_at=Date.epoch}}
      | {month} -> {month={month={january} year=1980}}
    new_state = {state with mode=new_mode }
     // restore current date
    set_date(new_state, date)

  @private on_message(state : CCalendar.state, msg, channel) =
    upgrade = update_state_and_refresh(state, _)
    rec process(msg) =
      match msg with
      | {Next} -> on_message(state, {Move = 1}, channel)
      | {Prev} -> on_message(state, {Move = -1}, channel)
      | {Move = by} -> move_by(by, state)
      | {GoToday} -> on_message(state, {SetDate = Date.now()}, channel)
      | {SetDate = date} -> upgrade(set_date(state, date))
      | {SetMode = mode} -> upgrade({state with ~mode})
      | {ChangeMode = mode} -> upgrade(change_mode(state, mode))
      | {ChangeConfig = config} -> upgrade({ state with ~config })
      | {Refresh} -> upgrade(state)
      | {Startup ~redraw_handler} -> upgrade({state with redraw_handler=some(redraw_handler)})
      | {Shutdown} -> calendar_shutdown(state)
      | {UpdateCallbacks=f} -> upgrade({state with callbacks=f(state.callbacks)})
      | {AddEvent=_}
      | {RemoveEvent=_}
      | {ModifyEvent=_}
      | {AddCategory=_}
      | {RemoveCategory=_} ->
          error("on_message: {msg} not implemented")
    result = process(msg)
    do
      // taking care of triggering the ViewChanged callback
      match (msg, result) with
      | ({Startup redraw_handler=_}, _)
      | ({Refresh}, _) ->
            state.callbacks.ViewChanged({mode=state.mode first_weekday=state.config.first_weekday})
      | (_, {set=new_state}) ->
          if state.mode == new_state.mode then
            void
          else
            state.callbacks.ViewChanged({mode=new_state.mode first_weekday=state.config.first_weekday})
      | _ -> void
    result

  @private roll_to_beg_of_the_week(state, date) =
    Date.move_to_weekday(date, {backward}, state.config.first_weekday)

  @private render_weeks_view(state, weeks, size) =
    date = roll_to_beg_of_the_week(state, weeks.start_at)
    render_many_weeks_view(state, date, (_ -> none), size, weeks.no)

  @private render_month_view(state, date, size) =
    start_at = Date.build({year=date.year; month=date.month; day=1})
            |> roll_to_beg_of_the_week(state, _)
    week_no =
       /* assuming we show 5 weeks, let's check the first date that is not visible --
        * if it's still in the same month then we need to show 6 weeks to make
        * the complete month visible. Otherwise 5 will do.
        * TODO take care of a special case of 4 weeks being enough in a February
        *      of a leap year, starting on Monday? :)
        */
      end_date = Date.advance(start_at, Duration.weeks(5))
      if Date.get_month(end_date) == date.month then 6 else 5
    day_style(d) =
      if Date.get_month(d) != date.month then
        some(state.config.style_config.weeks_view.day_cells.inactive_cell_style)
      else
        none
    render_many_weeks_view(state, start_at, day_style, size, week_no)

  @private render_event(state, evt, event_style) =
    config = state.config
    cfg_evts = config.event_config
    cfg_wv = config.style_config.weeks_view
    cfg_sevt = cfg_wv.events
    dr = cfg_evts.get_date_range(evt)
    time = DateRange.get_beg(dr)
    event_xhtml = cfg_evts.render(state.mode, evt)
    onclick(_) = state.callbacks.EventClick(evt)
    is_multiday = cfg_evts.is_multiday(evt)
    content = cfg_sevt.generate_content(~{time event_xhtml is_multiday})
    style = cfg_sevt.event_button_style(
      { ~is_multiday
      ; color=cfg_evts.get_event_color(evt)
      }
    )
    xhtml = WButton.html(style, Random.string(6), [({click}, onclick)], content)
         |> style_css(css { height: {cfg_sevt.height_px}px })
    style = event_style
    ~{xhtml style}

  @private create_events_grid(state, max_evts_per_day, visible_range, events) =
    config = state.config
    cfg_evts = config.event_config
    grid = WGrid.create({rows=max_evts_per_day cols=7})
    visible_range_beg = DateRange.get_beg(visible_range)
    rec aux(grid, events) =
      match events with
      | [] -> grid
      | [evt | evts] ->
          event_range = cfg_evts.get_date_range(evt)
          match DateRange.intersection(event_range, visible_range) with
          | {none} -> aux(grid, evts)
          | {some=vrange} ->
               evt_length =
                if cfg_evts.is_multiday(evt) then
                  if DateRange.is_empty(vrange) then
                    1
                  else
                    DateRange.length(vrange)
                    |> Duration.in_full_days(_)
                    |> _ + 1
                else
                  1
              evt_pos = Duration.between(visible_range_beg, DateRange.get_beg(vrange))
                     |> Duration.in_full_days(_)
              do Log.debug("[CAL]", "Placing event {cfg_evts.render(state.mode, evt)} : {DateRange.to_string(event_range)} -> {DateRange.to_string(vrange)}, pos={evt_pos}, size={evt_length}")
              pos = {row=0 col=evt_pos}
              size = {rows=1 cols=evt_length}
              new_grid =
                match WGrid.try_put_at(grid, evt, pos, size, {down}) with
                | {none} ->
                     // FIXME, make a +X entry and allow its expansion on click
                    do Log.debug("[CAL]", "too many events...")
                    grid
                | {some=new_grid} ->
                    do Log.debug("[CAL]", "New grid: {WGrid.to_debug_string(new_grid)}")
                    new_grid
              aux(new_grid, evts)
    aux(grid, events)

  @private render_weekly_events(state, start_at, size, max_events_per_day) =
    do Log.debug("[CAL]", "Rendering weekly events starting at: {Date.to_string(start_at)}")
    config = state.config
    cfg_wv = config.style_config.weeks_view
    cfg_evts = config.event_config
    (left_border_width_px, right_border_width_px) = cfg_wv.border_width_px
    left_header_size =
      match cfg_wv.left_header with
      | {none} -> 0
      | {some=cfg_lh} -> cfg_lh.width_px
     // get all events
    events_order =
      Order.make_by(
        (evt -> ( cfg_evts.is_multiday(evt)
                , cfg_evts.get_date_range(evt) |> DateRange.get_beg)
                ),
        Order.product(Order.reverse(Bool.order), Date.order)
      )
    end_at = Date.advance(start_at, Duration.weeks(1))
    visible_range = DateRange.between(start_at, end_at)
    evts = cfg_evts.request_events(visible_range)
        |> List.sort_with_order(events_order, _)
     // create events grid & fill it with events
    evts_grid = create_events_grid(state, max_events_per_day,
                  visible_range, evts)
     // render events grid
    event_style = { css_props = css
      { overflow: hidden
      ; height: {cfg_wv.events.height_px}px
      ; line-height: {cfg_wv.events.height_px}px
      }}
    WGrid.crop_with(evts_grid, (dir -> dir == {bottom})) //drop unused bottom cells
    |> WGrid.render(_, render_event(state, _, event_style), event_style)
    |> style_stl_css(cfg_wv.events.table_style, css
         { width: {size.width_px - left_header_size - left_border_width_px -
                    right_border_width_px}px
         ; table-layout: fixed
         ; z-index: 1000
         })

  @private render_many_weeks_view(state, start_date, day_style, size, week_no) =
    config = state.config
    cfg_wv = config.style_config.weeks_view
    get_date(week, day) =
      delta = Duration.add(Duration.weeks(week), Duration.days(day))
      Date.advance(start_date, delta)
     /* FIXME, -1 to accommodate the border; this is no good, there must be a better way
      * to take provisions of border space */
    day_height_px = ((size.height_px - cfg_wv.top_header.height_px - 2) / week_no) - 1
    daily_events_height_px = day_height_px - cfg_wv.day_cells.header.height_px - 2
    max_events_per_day = Int.max(daily_events_height_px / cfg_wv.events.height_px, 0)
    grid = WGrid.create({ rows = week_no + 1 /* visible weeks + top header*/
                        ; cols = 8 /* 7 days + left header */
                        })
        |> WGrid.fill(_, (~{row col} ->
              match (row, col) with
              | (0, 0) -> {corner}
              | (0, j) -> {top_header day=Date.get_weekday(get_date(0, j-1))}
              | (i, 0) -> {left_header week_number=Date.get_week_number(get_date(i-1, 0))}
              | (i, j) -> {cell
                             topleftmost=(i == 1 && j == 1)
                             date=get_date(i-1, j-1)
                             events=(
                               if j == 1 then
                                 events_xhtml =
                                   render_weekly_events(state, get_date(i-1, 0), size,
                                     max_events_per_day)
                                 some(events_xhtml)
                               else
                                 none)
                          }
           ))
        |> WGrid.render(_, (cell ->
             match cell with
             | {corner} ->
                 render_weeks_view_corner(config)
             | {top_header ~day} ->
                 render_weeks_view_top_header(config, size, day)
             | {left_header ~week_number} ->
                 render_weeks_view_left_header(config, day_height_px, week_number)
             | {cell ~topleftmost ~date ~events} ->
                 render_weeks_view_cell(state, day_height_px, day_style, topleftmost,
                   date, events)
           ), {})
        |> style_stl_css(cfg_wv.main_table_style, css { width: {size.width_px}px })
    <div style={css {position:relative}}>
      {grid}
    </>

  @private render_weeks_view_corner(config) =
    cfg_wv = config.style_config.weeks_view
    style =
      match cfg_wv.left_header with
      | {none} -> {} // no left header
      | {some=cfg_lh} ->
          { styler=cfg_wv.corner_style
          ; css_props=css
            { height: {cfg_wv.top_header.height_px}px
            ; width: {cfg_lh.width_px}px
            }
          }
    {xhtml=<></> ~style}

  @private render_weeks_view_top_header(config, size, day) =
    cfg_wv = config.style_config.weeks_view
    cfg_th = cfg_wv.top_header
    left_header_width_px = Option.map((v -> v.width_px + 1), cfg_wv.left_header) ? 0
     /* FIXME, -1 to accommodate the border; this is no good, there must be a better way to
        take provisions of border space */
    day_width_px = ((size.width_px - left_header_width_px - 1) / 7) - 1
    xhtml = cfg_th.cell_content(day)
    style = { css_props = css { height: {cfg_th.height_px}px; width: {day_width_px}px } }
    ~{xhtml style}

  @private render_weeks_view_left_header(config, cell_height_px, week_number) =
    cfg_wv = config.style_config.weeks_view
    match cfg_wv.left_header with
    | {none} -> {xhtml=<></> style={}}
    | {some=cfg_lh} ->
        xhtml = cfg_lh.cell_content(week_number)
        style = { css_props = css { height: {cell_height_px}px } }
        ~{xhtml style}

  @private render_weeks_view_cell(state, cell_height_px, day_style, is_topleftmost,
    date, events) =
    config = state.config
    cfg_wv = config.style_config.weeks_view
    cfg_days = cfg_wv.day_cells
    today = Date.now() |> Date.round_to_day
    header =
      <div>
        {cfg_days.header.render(
          { ~is_topleftmost
          ; is_today=date == today
          }, date)
        }
      </> |> style_css(css { height: {cfg_days.header.height_px}px; overflow: hidden })
    content = <div>{header}</> |> style_css(css { height: 100%; overflow: hidden; })
    events_xhtml = events ? <></> |> style_css(css
      { position: absolute
      ; left: 0px
        // FIXME 2 depends on the border thickness
      ; top: {cfg_days.header.height_px + 2}px
      })
    xhtml =
      onclick(_) = state.callbacks.DayClick(date)
      style = css { height: 100%; width: 100%; position: relative }
      <div style={style} onclick={onclick}>
        {content}
        {events_xhtml}
      </>
    cell_style =
      today = Date.now() |> Date.round_to_day
      match day_style(date) with
      | ~{some} -> some
      | {none} ->
          if date == today then
            cfg_days.today_cell_style
          else
            cfg_days.active_cell_style
    style = { styler = cell_style; css_props = css { height: {cell_height_px}px; } }
    ~{xhtml style}

  @private render_calendar(state, size) =
    content =
      match state.mode with
      | ~{weeks} -> render_weeks_view(state, weeks, size)
      | ~{month} -> render_month_view(state, month, size)
    <div>
      {content}
    </> |> style_stl(state.config.style_config.calendar_style)

  @private draw_calendar(old_state_opt, state) =
    id = state.config.id
    show(content) = Dom.transform([#{id} <- content])
    dummy_content = <div></> |> style_css(css {height: 100%; width: 100%})
    do show(dummy_content)
    size = {width_px=Dom.get_width(#{id}) height_px=Dom.get_height(#{id})}
    show_calendar(state) = show(render_calendar(state, size))
    match (old_state_opt : option, state.config.style_config.animation) with
    | ({none}, _)
    | (_, {no_animation}) ->
        do show_calendar(state)
        void
    | ({some=old_state}, anim) ->
        duration_time =
          // FIXME, duplication from Dom.Effect, refactor
          | {slow} -> 600
          | {default} -> 400
          | {fast} -> 200
          | ~{millisec} -> millisec
        (in, out, duration) =
          match anim with
          | {slide ~duration} ->
            (Dom.Effect.slide_in, Dom.Effect.slide_out, duration_time(duration))
          | {fade ~duration} ->
            (Dom.Effect.fade_in, Dom.Effect.fade_out, duration_time(duration))
          | _ ->
            error("[Calendar] internal error #27482")
        do show_calendar(old_state)
        show_effect(effect) =
          dur_effect = Dom.Effect.with_duration({millisec=duration}, effect())
          Dom.transition(#{id}, dur_effect)
        _ = show_effect(out)
        do sleep(duration, ( ->
          /* FIXME, this is not the way to do it, but Dom.Effect.sequence seems not
             to work (?) */
          do show_calendar(state)
          _ = show_effect(in)
          void
        ))
        void

// ***************************************************************************************
  /**
   * {2 Component configuration}
  **/
// ***************************************************************************************

  extensible_style_config : CCalendar.Style.config =
    extensible_style_date_format = Date.generate_printer("%l:%M%P")
  {
    calendar_style = {class=["CCalendar_extensible"]}
    animation = {no_animation}
    weeks_view =
    {
      main_table_style = {class=["monthly"]}
      border_width_px = (1, 1)
      corner_style = {class=["wday_hd"]}
      day_cells =
      {
        active_cell_style = {class=["act"]}
        inactive_cell_style = {class=["inact"]}
        today_cell_style = {class=["today"]}
        header =
        {
          height_px = 17
          render(params, date) =
            <div class="day_hd">{
              if params.is_today then
                "Today"
              else
                d = Date.to_human_readable(date)
                month =
                  if params.is_topleftmost || d.day == 1 then
                    "{Date.Month.to_short_string(d.month)} "
                  else
                    ""
                year =
                  if params.is_topleftmost || (d.day == 1 && d.month == {january}) then
                    ", {d.year}"
                  else
                    ""
                "{month}{d.day}{year}"
            }</>
        }
      }
      top_header =
      {
        cell_content(day) = <div class="wday_hd">{Date.Weekday.to_short_string(day)}</>
        height_px = 20
      }
      left_header = some(
      {
        cell_content(week) = <div class="week_hd">{if week < 10 then "0" else ""}{week}</>
        width_px = 20
      })
      events =
      {
        height_px = 17
        table_style = {class=["events"]}
        event_button_style(~{is_multiday color}) =
          { WButton.bare_config with
            common_style  : WStyler.styler =
              if is_multiday then
                { class = ["multiday"]
                ; style = css
                    { background: {Css_build.background_color(color)}
                    ; color: white
                    }
                }
              else
                { style = css { color: {color} } }
            default_style = WStyler.make_class(["event"])
            over_style = WStyler.make_class(["event", "over"])
            active_style = WStyler.make_class(["event", "active"])
          }
        generate_content(e) =
          time = Date.to_formatted_string(extensible_style_date_format, e.time)
          <div>
            {if e.is_multiday then <></> else <>{time}</>}
            {e.event_xhtml}
          </>
      }
    }
  }

  google_style_config : CCalendar.Style.config =
    google_style_date_format = Date.generate_printer("%H:%M")
  {
    calendar_style = {class=["CCalendar_google"]}
    animation = {no_animation}
    weeks_view =
    {
      main_table_style = {class=["monthly"]}
      border_width_px = (5, 5)
      corner_style = {class=["wday_hd"]}
      day_cells =
      {
        active_cell_style = {class=["act"]}
        inactive_cell_style = {class=["inact"]}
        today_cell_style = {class=["today"]}
        header =
        {
          height_px = 16
          render(params, date) =
            <div class="day_hd">{
              d = Date.to_human_readable(date)
              month =
                if params.is_topleftmost || d.day == 1 then
                  " {Date.Month.to_short_string(d.month)}"
                else
                  ""
              "{d.day}{month}"
            }</>
        }
      }
      top_header =
      {
        cell_content(day) = <div class="wday_hd">{Date.Weekday.to_short_string(day)}</>
        height_px = 14
      }
      left_header = none
      events =
      {
        height_px = 16
        table_style = {class=["events"]}
        event_button_style(~{is_multiday color}) =
          { WButton.bare_config with
            common_style  : WStyler.styler =
              if is_multiday then
                { class = ["multiday"]
                ; style = css
                    { background: {Css_build.background_color(color)}
                    ; color: white
                    }
                }
              else
                { style = css { color: {color} } }
            default_style = WStyler.make_class(["event"])
            over_style = WStyler.make_class(["event", "over"])
            active_style = WStyler.make_class(["event", "active"])
          }
        generate_content(e) =
          time =
            <span class="time">
              {Date.to_formatted_string(google_style_date_format, e.time)}
            </>;
          <div>
            {if e.is_multiday then <></> else <>{time}</>}
            {e.event_xhtml}
          </>
      }
    }
  }

  default_config( id : string
                , event_config : CCalendar.Event.config
                , style_config : CCalendar.Style.config
                ) : CCalendar.config =
  {
    first_weekday = {monday}
    ~id
    ~style_config
    ~event_config
  }

// ***************************************************************************************
  /**
   * {2 Component creation}
  **/
// ***************************************************************************************

  empty_callbacks =
  {
    ViewChanged(_) = void
    DayClick(_) = void
    EventClick(_) = void
  }

  create( config : CCalendar.config('event)
        , callbacks : CCalendar.callbacks('event)
        ) : CCalendar.instance('event) =
     // we initialize the calendar on a monthly view with this current month
    start_at = Date.now()
    init_mode = {month={month=Date.get_month(start_at) year=Date.get_year(start_at)}}
    init_state = { mode=init_mode ~config ~callbacks redraw_handler=none}
    rec val chan = Session.make(init_state, on_message(_, _, chan))
    redraw(_) = perform(chan, {Refresh})
    redraw_handler = Dom.bind(Dom.select_window(), {resize}, redraw)
    do Session.send(chan, {Startup ~redraw_handler})
    do draw_calendar(none, init_state)
    chan

// ***************************************************************************************
  /**
   * {2 Component manipulation}
  **/
// ***************************************************************************************

  shutdown(cal : CCalendar.instance) : void =
    perform(cal, {Shutdown})

  perform(c : CCalendar.instance, op) : void =
    msg =
      match op with
      | {~ChangeConfig} -> {~ChangeConfig}
      | {~Next} -> {~Next}
      | {~Prev} -> {~Prev}
      | {~GoToday} -> {~GoToday}
      | {~Move} -> {~Move}
      | {~SetMode} -> {~SetMode}
      | {~ChangeMode} -> {~ChangeMode}
      | {~SetDate} -> {~SetDate}
      | {~Refresh} -> {~Refresh}
      | {~Shutdown} -> {~Shutdown}
      | {~UpdateCallbacks} -> {~UpdateCallbacks}
    Session.send(c, msg)

  redraw(c : CCalendar.instance) : void =
    perform(c, {Refresh})

// ***************************************************************************************
  /**
   * {2 Auxilary functions}
  **/
// ***************************************************************************************
   date_range_string(first_weekday : Date.weekday, mode : CCalendar.mode) : string =
     match mode with
     | ~{month} -> "{month.month} {month.year}"
     | {weeks=~{no start_at}} ->
         beg_date = Date.move_to_weekday(start_at, {backward}, first_weekday)
         end_date = Date.advance(beg_date, Duration.weeks(no))
                 |> Date.advance(_, Duration.days(-1))
         beg_day = Date.get_day(beg_date)
         dont_repeat(f) =
           if f(beg_date) == f(end_date) then
             ""
           else
             "{f(beg_date)} "
         beg_month = dont_repeat(Date.get_month)
         beg_year = dont_repeat(Date.get_year)
         "{beg_day} {beg_month}{beg_year}– {Date.get_day(end_date)} {Date.get_month(end_date)} {Date.get_year(end_date)}"

}}

// ***************************************************************************************
  /**
   * {1 CSS styles}
  **/
// ***************************************************************************************

ccalendar_extensible_style_css = css
   // FIXME, I don't want the style for 'td', but otherwise I get a syntax error
  td {}
  .CCalendar_extensible table.monthly {
    border-collapse: collapse;
    border-spacing: 0px;
  }
  .CCalendar_extensible .monthly td {
    border: 1px solid #BCF;
    padding: 0px;
    spacing: 0px;
  }
  .CCalendar_extensible .monthly .events td {
    border: none;
    padding: 0px;
    spacing: 0px;
  }
  .CCalendar_extensible .monthly .week_hd, .CCalendar_extensible .monthly .wday_hd {
    line-height: 20px;
    text-align: center;
    font-family: helvetica, arial, sans-serif;
    font-weight: normal;
    font-size: 12px;
    background-color: #EFEFEF;
  }
  .CCalendar_extensible .monthly .week_hd {
    height: 100%;
  }
  .CCalendar_extensible .monthly .week_hd {
    color: #999
  }
  .CCalendar_extensible .monthly .wday_hd {
    color: #555
  }
  .CCalendar_extensible .monthly .day_hd {
    color: #A7C6DF;
    font-family: helvetica, arial, sans-serif;
    font-size: 16px;
    line-height: 14px;
    text-align: right;
    padding: 2px 4px 1px 4px
  }
  .CCalendar_extensible .monthly .inact {
    background-color: #EFEFEF
  }
  .CCalendar_extensible .monthly .today {
    background-color: #FFF4BF
  }
  .CCalendar_extensible .monthly .inact .day_hd {
    color: #BBB;
  }
  .CCalendar_extensible .monthly .today .day_hd {
    color: #BFA52F
  }
  .CCalendar_extensible .monthly .events .event {
    display: block;
    width: 100%;
    border: none;
    background: none;
    text-align: left;
    white-space: nowrap;
//    padding: 1px 1px 0px 2px; -- this should go on the parent
    line-height: 14px;
    font-size: 11px;
    font-family: Verdana, sans-serif;
    cursor: pointer;
  }
  .CCalendar_extensible .monthly .events .event.multiday {
    border-radius: 5px;
  }
  .CCalendar_extensible .monthly .events .event div {
    overflow: hidden;
  }
  .CCalendar_extensible .monthly .events .event.over {
    opacity: .8;
  }

ccalendar_google_style_css = css
   // FIXME, I don't want the style for 'td', but otherwise I get a syntax error
  td {}
  .CCalendar_google table.monthly {
    font-size: 11px;
    font-family: Arial, sans-serif;
    border-collapse: collapse;
    border-left: 5px solid #BCF;
    border-right: 5px solid #BCF;
    border-bottom: 5px solid #BCF;
    border-spacing: 0px;
  }
  .CCalendar_google .monthly tr td {
    border: 1px solid #DDD;
    padding: 0px;
    spacing: 0px;
  }
  .CCalendar_google .monthly td.today {
    border: 1px solid #FAD163;
    background-color: #FFF7D7
  }
  .CCalendar_google .monthly .events td {
    border: none;
    padding: 0px;
    spacing: 0px;
  }
  .CCalendar_google .monthly .wday_hd {
    color: #20C;
    padding-top: 2px;
    font-weight: normal;
    line-height: 12px;
    background-color: #E3E9FF;
    text-align: center;
    border-bottom-color: #20C
  }
  .CCalendar_google .monthly .day_hd {
    line-height: 16px;
    color: #666;
    background-color: #F8F9FF;
    text-align: right;
    padding-right: 2px;
  }
  .CCalendar_google .monthly .today .day_hd {
    background-color: #FAD163
  }
  .CCalendar_google .monthly .inact .day_hd {
    color: #AAA
  }
  .CCalendar_google .monthly .events .event {
    display: block;
    width: 100%;
    border: none;
    background: none;
    text-align: left;
    white-space: nowrap;
//    padding: 1px 1px 0px 2px; -- this should go on the parent
    font-size: 11px;
    font-family: Verdana, sans-serif;
    cursor: pointer;
  }
  .CCalendar_google .monthly .events .event.multiday {
    border-radius: 5px;
  }
  .CCalendar_google .monthly .events .event div {
    overflow: hidden;
  }
  .CCalendar_google .monthly .events .event .time {
    font-family: Arial, sans-serif;
    font-size: 10px;
    font-weight: bold;
  }
  .CCalendar_google .monthly .events .event.over {
    opacity: .8;
  }
