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
 * A configurable date picker widget.
 *
 * @category WIDGET
 * @author Guillem Rieu, 2010
 * @destination PUBLIC
 * @stability TRANSITIONAL
 */

import stdlib.widgets.core

// TODO: focus sweeps to the month <select> when selecting a day of an inline WDate (the whole calendar being replaced)
// TODO: change month when clicking on surrounding days
// TODO: higher-level API with setters and getters (in a record?)
// TODO: N-months version of the date picker (?)

/**
 * {1 About this module}
 *
 * This widget is aimed at letting the user choose a date with the mouse from
 * a graphical calendar, though the date can also be edited by hand directly
 * in the input field.
 *
 *
 * {1 Where should I start?}
 *
 * Use the function [WDatepicker.edit] to construct a datepicker.
 *
 */
 // TODO: talk about high-level and low-level APIs

/**
 * {1 Types defined in this module}
 */

/**
 * The configuration of a widget.
 *
 * You can set in this record all the configurable parameters of a widget.
 * Recommended usage is to start from the default config [WDatepicker.default_config]
 * and to modify the wanted parameters. For example:
 *
 */
// TODO: write a rather complete example using the { record with ... } syntax
type WDatepicker.config = {
  inline          : bool                        /** Display the calendar inline, or attached to an input field */
  menus           : bool                        /** Display or not date and month menus */
  prev_next       : (xhtml, xhtml)              /** The XHTML displayed in the "previous" and "next" links */

  first_wday      : Date.weekday                /** The first day of a week */
  format          : WDatepicker.format                /** Date display format string */
  year_range      : (int, int)                  /** Starting and ending year of the year select menu */

  is_enabled      : Date.date -> bool           /** A function deciding if a date is enabled */
  get_styler      : Date.date -> WStyler.styler /** Associate a styler to a given date */
  links           : list((string, Date.date))   /** List of links to special days */

  global_style    : WStyler.styler              /** Global CSS style of the widget */
  prev_next_style : (WStyler.styler, WStyler.styler) /** CSS style for "previous" and "next" links */
  enabled_style   : WStyler.styler              /** CSS style for active dates */
  disabled_style  : WStyler.styler              /** CSS style for inactive dates */
  selected_style  : WStyler.styler              /** CSS style for the currently selected date */
  today_style     : WStyler.styler              /** CSS style for today */

  on_open         : string -> void              /** Function called when showing the calendar (with the ID as parameter) */
  on_close        : string -> void              /** Function called when hiding the calendar (with the ID as parameter) */
  open_class    : string                        /** Name of the class used to mark the widget as open (when the date picker is shown) */
}

/**
 * Custom type to use instead of date format strings for performance reasons.
 *
 * Hopefully, this should be replaced by the standard {!Date.format} type
 * in a near future.
 */
// TODO: add more formats
// TODO: why not using [Date.printer] as format?
type WDatepicker.format =
//        { float }
    { F }  /** (equivalent to [%F]) full date (ex. 2010-03-07) */
  / { D }  /** (equivalent to [%D]) date (ex. 03/07/10) */
  / { id } /** Format used in XHTML IDs of the widget (ex. 2010_03_07) */


WDatepicker = {{

  /**
   * {1 Configuration}
   */

  /**
   * The default configuration for a date widget. It can be used as a basis
   * on which to build a custom config. For example:
   * custom_config = { default_config with format={F} }
   */
  @both_implem default_config: WDatepicker.config = {
    inline          = false
    menus           = false
    prev_next       = (<>{"<"}</>, <>{">"}</>)

    first_wday      = { monday }
    format          = { D }
    year_range      = (1970, 2030)

    is_enabled      = _ -> true
    get_styler      = _ -> WStyler.empty
    links           = [("Today", Date.now())]

    global_style    = default_global_styler
    prev_next_style = (WStyler.empty, WStyler.empty)
    enabled_style   = WStyler.empty
    disabled_style  = WStyler.make_style(css {color: gray;})
    today_style     = WStyler.make_style(css {font-decoration: bold;})
    selected_style  = WStyler.make_style(css {border: solid;})

    on_open         = _ -> void
    on_close        = _ -> void
    open_class    = "wdate_open"
  }

  /**
   * Default CSS style used in the default config.
   */
  default_global_styler = WStyler.make_style(css {
    /*position   : absolute;*/
    /*float      : left;*/
    display    : none;
    z-index    : 999;
    background : white;
    border     : 1px solid;
  })

  /**
   * The default configuration modified to suit an inline widget.
   */
  default_inline_config: WDatepicker.config = { default_config with
    inline = true
    global_style = default_inline_styler
  }

  default_inline_styler = WStyler.make_style(css {
    background: white;
  })

  /**
   * {1 High-level interface}
   */

  /**
   * This function produce a full date widget (a clickable input field,
   * associated to a date picker calendar).
   *
   * @param config The widget configuration
   * @param id The widget identifier
   * @param init_date The initial date of the widget
   */
  edit(config: WDatepicker.config, onchange: Date.date -> void, id: string,
      init_date: Date.date): xhtml =
    /* Initialize the widget date */
    hidden_init = <input type="hidden" id={get_init_id(id)}
        value={to_formatted_string(config.format, init_date)} />
    /* Build the widget HTML */
    if config.inline then
      /* Build the calendar HTML */
      calendar = edit_inline(config, (_ -> void), onchange, id, init_date)
      <>
        {hidden_init}
        {calendar}
      </>
    else
      /* Adjust the calendar position to the input one */
      adjust_calendar_position(_evt) =
        input_sel       = #{get_input_id(id)}
        height_px    = Dom.get_height(input_sel)
        position_styler = WStyler.make_style(css {
            position: absolute;
            top: {height_px + 2}px;
          })
        calendar_styler = WStyler.merge([position_styler, config.global_style])
        WStyler.set_dom(calendar_styler, get_picker_id(id))
      /* Build the calendar HTML */
      calendar = edit_inline(config, adjust_calendar_position, onchange,
          id, init_date)
      input_date =
        <input id=#{get_input_id(id)} type="text"
               value={to_formatted_string(config.format, init_date)}
               onclick={
                 update_datepicker_mem((config, onchange, id, init_date, false))}
               onkeyup={
                 update_datepicker_mem((config, onchange, id, init_date, true))}
             />
      (<div style={css {position: relative; display: inline;}}>
        {hidden_init}
        {input_date}
        {calendar}
       </div>)

  /**
   * Shortcut to {!WDatepicker.edit} initialized with the default configuration.
   *
   * @param onchange An event handler to call when a new date is selected
   * @param id The HTML identifier of the widget
   * @param date The initial date
   * @return The HTML corresponding to the widget
   */
  edit_default(onchange, id, date): xhtml =
    edit(default_config, onchange, id, date)

  /**
   * Display a date following the format defined in the config.
   *
   * @param config The widget configuration
   * @param id The main ID identifying the widget
   * @param date The actual date to display
   * @return The HTML corresponding to the widget
   */
  show(config: WDatepicker.config, id: string, date: Date.date): xhtml =
    styler = build_date_styler(([
        (_ -> true, config.get_styler(date), none),
        (config.is_enabled, config.enabled_style, some(config.disabled_style)),
        (d -> eq_day(Date.now(), d), config.today_style, none),
      ], date))
    <span id=#{get_input_id(id)}>
      {to_formatted_string(config.format, date)}
    </span>
      |> WStyler.set(styler, _)

  /**
   * A shortcut to {!WDatepicker.show} initialized with the default configuration.
   */
  show_default(id, date) = show(default_config, id, date)

  update_datepicker((config: WDatepicker.config, onchange: Date.date -> void,
      id: string, init_date: Date.date, key_pressed: bool))
      : Dom.event -> void = event ->
    // Parse the input field and update the calendar
    input_date_opt() = parse(config, id)
    kc = Option.switch((new_date ->
        /*if not(eq_day(new_date, init_date)) then*/
          do Dom.transform([#{get_picker_id(id)} <-
              date_picker((config, onchange, id, new_date, new_date))])
          if key_pressed then
            kc = event.key_code
            kc = Option.lazy_default(
                   -> error("Wdate : Error on getting the keyCode"),
                  kc)
            is_edit_key = (kc >= 48 && kc <= 57) || List.mem(kc, [8,13,127])
            do if is_edit_key then onchange(new_date)
            kc
          else 0
      ), 0, input_date_opt())
      /*else 0), 0, input_date_opt())*/
    if not(is_open(config.open_class, id)) then
      do do_open(config.open_class, id)
      config.on_open(id)
    else if kc == 13 then
      do save_date(config, id)
      do hide_picker(config.open_class, id)
      config.on_close(id)
    else if kc == 27 then
      do do_close(config.open_class, id)
      _ = Dom.set_value(#{get_input_id(id)}, to_formatted_string(config.format, init_date))
      config.on_close(id)

  /**
   * {2 Imperative interface}
   */

  /**
   * Get a date from the date widget identified by [id]
   *
   * @param config The configuration of the widget
   * @param id The identifier of the widget
   * @return The current value of the widget if valid
   */
  parse(config: WDatepicker.config, id: string): option(Date.date) =
    get_input_id(id) |> Dom.select_id(_) |> Dom.get_value(_)
      |> of_formatted_string(config.format, _)

  /**
   * Shortcut to the {!WDatepicker.parse} function, initialized with the default
   * configuration.
   *
   * @param id The identifier of the widget
   * @return The current value of the widget if valid
   */
  parse_default(id) = parse(default_config, id)

  /**
   * Show the pop-up calendar of a date picker
   *
   * @param open_class Class of a widget when it's open
   * @param id The identifier of the widget
   */
  do_open(open_class: string, id: string): void =
    show_picker(open_class, id)

  /**
   * Hide the pop-up calendar of a date picker
   *
   * @param open_class Class of a widget when it's open
   * @param id The identifier of the widget
   */
  do_close(open_class: string,  id: string): void =
    do load_date(id)
    hide_picker(open_class, id)

  /**
   * Test whether or not the calendar of a date picker is shown
   *
   * @param open_class Class of a widget when it's open
   * @param id The identifier of the widget
   * @return Whether or not the datepicker is open
   */
  is_open(open_class: string, id: string): bool =
    Dom.has_class(#{get_input_id(id)}, open_class)

  /**
   * {1 Private functions aimed at internal use}
   *
   * Do not use them outside of the module.
   */

  /**
   *  {2 Build the calendar XHTML}
   */
  @private
  date_picker((config: WDatepicker.config, onchange: Date.date -> void,
      id: string, init_date: Date.date, selected_date: Date.date)): xhtml =
    offset = Date.Weekday.to_int(config.first_wday)
    <>
      <div>
        {calendar_caption(config, id,
            advance_month(config, onchange, id, init_date, selected_date, _),
            update_calendar(config, onchange, id, selected_date, _),
            init_date)}
      </div>
      <table>
        <thead>{calendar_header_mem(offset)}</thead>
        <tbody>
          {calendar_body(config, id,
              select_day(config, onchange, id, selected_date, _),
              init_date, selected_date)}
        </tbody>
      </table>
      {if config.links != [] then
        calendar_links(config, onchange, id, selected_date)
      else
        <></>}
    </>

  // TODO: some refactoring of the following (definitions at another place) 
  // functions may be useful
  @private
  advance_month(config: WDatepicker.config, onchange: Date.date -> void,
      id: string, init_date: Date.date, selected_date: Date.date, nmonths: int)
      : void =
    /* new_date = Date.advance(init_date, Duration.months(Float.of_int(nmonths)))
     replaced by a custom piece of code for now
     Date.advance seems not to work here (bug with February)
     TODO: put this line when Date.advance is fixed
      */
    init_date_hr = Date.to_human_readable(init_date)
    init_month_int = Date.Month.to_int(init_date_hr.month)
    new_month = Date.Month.of_int(mod((init_month_int + nmonths + 12), 12))
    new_year = init_date_hr.year - 1 + ((init_month_int + nmonths + 12) / 12)
    new_date = Date.of_human_readable(
      { init_date_hr with day=1 month=new_month year=new_year })
    Dom.transform([#{get_picker_id(id)} <-
      date_picker((config, onchange, id, new_date, selected_date))])

  @private
  update_calendar(config: WDatepicker.config, onchange: Date.date -> void,
      id: string, selected_date: Date.date, _: Dom.event)
      : void =
    new_month =
      Date.Month.of_int(Int.of_string(Dom.get_value(#{id ^ "_menu_month"})))
    new_year = Int.of_string(Dom.get_value(#{id ^ "_menu_year"}))
    new_date =
      epoch_hr = Date.to_human_readable(Date.epoch)
      Date.of_human_readable({ epoch_hr with month=new_month year=new_year })
    Dom.transform([#{get_picker_id(id)} <-
      date_picker((config, onchange, id, new_date, selected_date))])

  @private
  select_day(config: WDatepicker.config, onchange: Date.date -> void,
      id: string, init_date: Date.date, date_str: string)
      : void =
    new_date = of_formatted_string({id}, date_str)
    match new_date with
      | { none }  ->
        error("Error while parsing the selected date.")
      | { ~some } ->
        do if not(config.inline) then
          _ = Dom.set_value(#{get_input_id(id)}, to_formatted_string(config.format, some))
          do hide_picker(config.open_class, id)
          config.on_close(id)
        if not(eq_day(some, init_date)) then
          do Dom.transform([#{get_picker_id(id)} <-
            date_picker((config, onchange, id, init_date, some))])
          do save_date(config, id)
          onchange(some)

  @private
  calendar_links(config: WDatepicker.config, onchange: Date.date -> void,
      id: string, selected_date: Date.date): xhtml =
    date_link((lbl: string, d: Date.date), acc: xhtml): xhtml =
      acc <+> <a onclick={_event ->
        Dom.transform([#{get_picker_id(id)} <-
          date_picker((config, onchange, id, d, selected_date))])}>{lbl}</a>
    (<div id="{id}_links">
      {List.fold(date_link, config.links, <></>)}
    </div>)

  /**
   * Build the calendar part of the widget, can be used on its own to produce
   * an inline widget.
   */
  @private
  edit_inline(config: WDatepicker.config, onready_handler: (Dom.event -> void),
      onchange: Date.date -> void, id: string, init_date: Date.date)
      : xhtml =
    <div id=#{get_picker_id(id)} onready={onready_handler}>
      {date_picker((config, onchange, id, init_date, init_date))}
    </div>
      |> WStyler.add(config.global_style, _)

  /* IDs */
  @private get_input_id(id)  = id ^ "_input"
  @private get_picker_id(id) = id ^ "_picker"
  @private get_init_id(id) = id ^ "_init"

  /**
   *  {2 Additional date functions}
   */

  @private
  next_day(day: Date.date): Date.date =
    Date.shift_forward(day, Duration.days(1))

  /**
   * Test if two dates represent the same day
   */
  @private
  eq_day(d1: Date.date, d2: Date.date): bool =
   { year=y1 month=m1 day=n1 ... } = Date.to_human_readable(d1)
   { year=y2 month=m2 day=n2 ... } = Date.to_human_readable(d2)
   y1 == y2 && Date.Month.equals(m1, m2) && n1 == n2

  /**
   * The number of days from [wday1] to [wday2] (in range [1;6])
   */
  @private
  days_until_next(wday: Date.weekday, wday_next: Date.weekday): int =
    mod(Date.Weekday.to_int(wday_next) - Date.Weekday.to_int(wday) + 7, 7)

  /**
   * The date corresponding to the last [wday], counting from [start]
   */
  @private
  last_day(wday: Date.weekday, current: Date.date): Date.date =
    delta = days_until_next(wday, Date.to_human_readable(current).wday)
    Date.shift_backward(current, Duration.days(delta))

  /**
   * The date corresponding to the coming [wday], counting from [start]
   */
  @private
  coming_day(wday: Date.weekday, current: Date.date): Date.date =
    delta = days_until_next(Date.to_human_readable(current).wday, wday)
    Date.shift_forward(current, Duration.days(delta))

  /* 1st of the month corresponding to the given date */
  @private
  first_day_of_month(current: Date.date): Date.date =
    Date.of_human_readable({ Date.to_human_readable(current) with day = 1 })

  /* Find the last day of the month corresponding to a given date
   * (Heavily inspired by Tt's date.opa) */
  @private
  last_day_of_month(date: Date.date): Date.date =
    is_leap_year(year) =
      ((mod(year, 4) == 0) && (not(mod(year, 100) == 0))) || (mod(year, 400) == 0)
    date_hr = Date.to_human_readable(date)
    last_day = match date_hr.month with
      | {january}
      | {march}
      | {may}
      | {july}
      | {august}
      | {october}
      | {december} -> 31
      | {april}
      | {june}
      | {september}
      | {november} -> 30
      | {february} -> if is_leap_year(date_hr.year) then 29 else 28
    delta = last_day - date_hr.day
    Date.shift_forward(date, Duration.days(delta))
    /* TODO: put this line back when Date.of_human_readable is fixed
       Date.of_human_readable({ date_hr with day = last_day })
     */

  /* Date corresponding to the first day of the week */
  @private
  first_day_of_week(first_wday: Date.weekday, current_day: Date.date): Date.date =
    last_day(first_wday, current_day)

  /* Date corresponding to the last day of the week */
  @private
  last_day_of_week(first_wday: Date.weekday, current_day: Date.date): Date.date =
    last_wday = mod(Date.Weekday.to_int(first_wday) + 6, 7)
      |> Date.Weekday.of_int(_)
    coming_day(last_wday, current_day)

  /* Split a list of days in batch of 7, producing a list of weeks */
  @private
  split_weeks(lst: list((int, string, bool, WStyler.styler)))
      : list(list((int, string, bool, WStyler.styler))) =
    rec aux(l, acc) = match l with
      | [] -> acc
      | _  ->
        (l1, l2) = List.split_at(l, 7)
        aux(l2, l1 +> acc)
    List.rev(aux(lst, []))


  /*
   * Date parsing and printing functions
   *
   * NB: the following functions are a workaround to slow parsing of date format
   * strings. They should eventually be replaced by corresponding ones in the
   * Date module when speed becomes bearable.
   */

  /* Returns the format corresponding to the given */
/*  _string_of_format(format: WDatepicker.format): string = match format with*/
/*//     | { float } -> error("WDatepicker.str_of_format: can't convert \{ float \}")*/
/*    | { id }    -> "%Y_%m_%d"*/
/*    | { F }     -> "%F"*/
/*    | { D }     -> "%D"*/

  /* Simple formatter for a date */
  @private
  to_formatted_string(format: WDatepicker.format, date: Date.date): string =
    { ~year ~month ~day ... } = Date.to_human_readable(date)
    month_str = Int.to_string(Date.Month.to_int(month) + 1)
      |> String.padding_left("0", 2, _)
    day_str = Int.to_string(day) |> String.padding_left("0", 2, _)
    match format with
//       | { float }      -> String.of_float(@unwrap(date))
      | { id }         -> "{year}_{month_str}_{day_str}"
      | { F }          -> "{year}-{month_str}-{day_str}"
      | { D }          -> "{day_str}/{month_str}/{year}"

  /* Simple parser for a date */
  @private
  of_formatted_string(format: WDatepicker.format, date: string): option(Date.date) =
    mkdate((y, m, d): (int, int, int)): option(Date.date) =
      if m >= 1 && m <= 12 && d >= 1 && d <= 31 then
        epoch_hr = Date.to_human_readable(Date.epoch)
        { epoch_hr with year=y month=Date.Month.of_int(m - 1) day=d }
          |> Date.of_human_readable(_) |> some(_)
      else
        none
    nat = parser n=([0-9]+) -> Int.of_string(Text.to_string(n))
    date_parser = match format with
//       | { float } -> (parser f=Rule.float -> some(@wrap(f) : Date.date))
      | { id }    -> (parser y=nat "_" m=nat "_" d=nat -> mkdate((y,m,d)))
      | { F }     -> (parser y=nat "-" m=nat "-" d=nat -> mkdate((y, m, d)))
      | { D }     -> (parser d=nat "/" m=nat "/" y=nat -> mkdate((y, m, d)))
    Parser.parse(parser d=date_parser -> d | .* -> none, date)

  /* Retrieve stylers corresponding to a given date */
  @private
  build_date_styler((tests: list((Date.date -> bool, WStyler.styler, option(WStyler.styler))), d: Date.date))
      : WStyler.styler =
    aux((f, stl_true, stl_false_opt), acc) =
      if f(d) then stl_true +> acc
      else Option.switch(stl -> stl +> acc, acc, stl_false_opt)
    List.fold(aux, tests, [])
      |> WStyler.merge(_)

  /*
   * Calendar building functions.
   */


  /* Create a list of weeks, themselves being lists of tuples
   * (int, string, bool, list(string)) where the int stands for a day number,
   * the string for the full date, the bool indicates if this day has to be
   * enabled in the calendar, or not, and the list of string represents
   * classes associated with the day.
   *
   * @param config The widget config
   * @param id The ID of the widget
   * @param month Generate the calendar for this month (with surrounding days for complete weeks)
   * @param init_date The initial date for building the calendar of the month
   * @param selected_date The currently selected date
   */
  @private
  padded_month(config: WDatepicker.config, _id: string, init_date: Date.date,
    selected_date: Date.date)
      : list(list((int, string, bool, WStyler.styler))) =

    start_date = // Take the first day of month and complete the week
      first_day_of_month(init_date) |> first_day_of_week(config.first_wday, _)
    // Note: put this line back when last_day_of_month is fixed
    //   end_date = last_day_of_month(init_date) |> last_day_of_week
    end_date = // Take the last day of month and complete the week
      Date.shift_forward(start_date, Duration.days(7))
        |> last_day_of_month(_) |> last_day_of_week(config.first_wday, _)
    list_day_range(config, init_date, selected_date, start_date, end_date) |> split_weeks(_)

  @private
  is_enabled(config: WDatepicker.config, init_date: Date.date, d: Date.date) =
    config.is_enabled(d) && Date.to_human_readable(init_date).month
         == Date.to_human_readable(d).month

  @private
  get_styler(config: WDatepicker.config, today: Date.date, init_date: Date.date,
      selected_date: Date.date, d: Date.date) =
    build_date_styler(([
        (_ -> true, config.get_styler(d), none),
        (is_enabled(config, init_date, _), config.enabled_style, some(config.disabled_style)),
        (d -> eq_day(today, d), config.today_style, none),
        (d -> eq_day(selected_date, d), config.selected_style, none),
      ], d))

  /* Return a list of days between [init_date] and [end_date] and
   * represented by tuples of the form (day, date_str, is_enabled,
   * list(classes)).
   */
  @private
  list_day_range(config: WDatepicker.config, init_date: Date.date,
      selected_date: Date.date, start_date: Date.date, end_date: Date.date)
      : list((int, string, bool, WStyler.styler)) =
    today = Date.now()
    mktuple(d) =
    // FIXME: parsing of date format string is currently too slow for being usable
    //   (Date.to_human_readable(d).day, Date.to_formatted_string("%F", d), config.is_enabled(d))
      (Date.to_human_readable(d).day,
       to_formatted_string({id}, d),
       is_enabled(config, init_date, d), get_styler(config, today, init_date, selected_date, d))
    rec aux(d, acc: list((int, string, bool, WStyler.styler))) =
      if d == end_date then mktuple(d) +> acc
      else aux(next_day(d), mktuple(d) +> acc)
    List.rev(aux(start_date, []))

  /*
   * The following functions generate the XHTML components of a calendar.
   */

  /* Build the table header of a calendar (with days of the week) */
  @private
  calendar_header(offset: int) =
    mkth(acc: xhtml, n: int): xhtml =
      (acc <+> <th>{Date.Weekday.of_int(mod(n + offset, 7))
        |> Date.Weekday.to_string |> String.substring(0, 2, _)}</th>)
    <tr>
      {Int.fold(mkth, <></>, 7)}
    </tr>

  /* Build a td element corresponding to a single day */
  @private
  calendar_cell(id: string, handle: string -> void,
      (day, handle_args, enabled, stl): (int, string, bool, WStyler.styler)): xhtml =
    td =
      if enabled then
        <td id="{id}_{handle_args}" onclick={_ -> handle(handle_args)}>
          {day}
        </td>
      else
        <td id="{id}_{handle_args}">{day}</td>
    WStyler.set(stl, td)

  /* Build a tr element corresponding to a week (7 days) */
  @private
  calendar_row(id, handle, days: list((int, string, bool, WStyler.styler))): xhtml =
    <tr>{ List.map(calendar_cell(id, handle, _), days) }</tr>

  /**
   * Build the month menu items
   */
  @private
  options_month(selected_month_int: int): xhtml =
    aux(acc, i) =
      acc <+>
        m = Date.Month.of_int(i)
        m_str = Date.Month.to_string(m)
        if i == selected_month_int then //Note: much faster than == on months
          <option value="{i}" selected="selected">{m_str}</option>
        else
          <option value="{i}">{m_str}</option>
    Int.fold(aux, <></>, 12)


  /**
   * Build a drop down selection list for months
   */
  @private
  menu_month(id: string, update_calendar: Dom.event -> void,
      selected_month: Date.month): xhtml =
      selected_month_int = Date.Month.to_int(selected_month)
    <select id="{id}_menu_month" onchange={update_calendar}>
      {options_month_mem(selected_month_int)}
    </select>

  /**
   * Build a drop down selection list for years
   */
  @private
  menu_year(id: string, update_calendar: Dom.event -> void,
      first_year: int, last_year: int, selected_year: int): xhtml =
    <select id="{id}_menu_year" onchange={update_calendar}>
      {options_year_mem((first_year, last_year, selected_year))}
    </select>

  /**
   * Build the year menu items
   */
  @private
  options_year((first_year: int, last_year: int, selected_year: int)): xhtml =
    range = last_year - first_year + 1
    aux(acc, y) =
      acc <+>
        y = y + first_year
        if y == selected_year then
          <option value="{y}" selected="selected">{y}</option>
        else
          <option value="{y}">{y}</option>
    Int.fold(aux, <></>, range)

  /* Build the body of a calendar (day numbers as cells of a table) */
  @private
  calendar_body(config: WDatepicker.config, id: string,
      select_day: string -> void, init_date: Date.date,
      selected_date: Date.date)
      : xhtml =
    <>{ List.map(calendar_row(id, select_day,_), padded_month(config, id, init_date, selected_date)) }</>

  /* Build the caption of the calendar (month, year, navigation controls) */
  @private
  calendar_caption(config: WDatepicker.config, id: string,
      advance_month: int -> void, update_calendar: Dom.event -> void,
      current_day: Date.date)
      : xhtml =
    { ~year ~month ... } = Date.to_human_readable(current_day)
    (prev, next) = config.prev_next
    (prev_styler, next_styler) = config.prev_next_style
    (start_year, end_year) = config.year_range
    <>
      {<a onclick={_event -> advance_month(-1)}>{prev}</a>
        |> WStyler.set(prev_styler, _)}
      {if config.menus then
         <>
         {menu_month(id, update_calendar, month)}
         {menu_year(id, update_calendar,
           min(start_year, year), max(end_year, year), year)}
         </>
       else
         <>{"{Date.Month.to_string(month)} {String.of_int(year)}"}</>}
      {<a onclick={_event -> advance_month(1)}>{next}</a>
        |> WStyler.set(next_styler, _)}
    </>

  /*
   *  {2 DOM manipulation functions}
   */
  @private
  get_date(config: WDatepicker.config, id: string): option(Date.date) =
    get_input_id(id) |> Dom.select_id(_) |> Dom.get_value(_)
      |> of_formatted_string(config.format, _)

  @private
  hide_picker(open_class: string, id: string): void =
    do Dom.remove_class(#{get_input_id(id)}, open_class)
    _ = Dom.transition(#{get_picker_id(id)}, Dom.Effect.with_duration({millisec = 200}, Dom.Effect.hide()))
    void

  @private
  show_picker(open_class: string, id: string): void =
    do Dom.add_class(#{get_input_id(id)}, open_class)
    _ = Dom.transition(#{get_picker_id(id)}, Dom.Effect.with_duration({millisec = 200}, Dom.Effect.show()))
    void

  @private
  save_date(config: WDatepicker.config, id: string): void =
    date_opt = get_date(config, id)
    Option.switch((new_date ->
        _ = to_formatted_string(config.format, new_date)
          |> Dom.set_value(#{get_init_id(id)}, _)
        void),
      void, date_opt)

  @private
  load_date(id: string): void =
    _ = Dom.get_value(#{get_init_id(id)})
      |> Dom.set_value(#{get_input_id(id)}, _)
    void

  /**
   * {2 Memoization}
   */

  @private
  compare_month(d1: Date.date, d2: Date.date): Order.comparison =
    y1 = Date.get_year(d1)
    y2 = Date.get_year(d2)
    /* If the two dates are in the same year... */
    if Int.equals(y1, y2) then
      /* ...check if they are in the same month as well. */
      Int.compare(Date.Month.to_int(Date.get_month(d1)),
          Date.Month.to_int(Date.get_month(d2)))
    else
      /* ...or compare the years if they are different. */
      Int.compare(y1, y2)

  @private
  update_datepicker_cmp((_, _, id1, d1, _), (_, _, id2, d2, _))
      : Order.comparison =
    if String.equals(id1, id2) then
      compare_month(d1, d2)
    else
      String.compare(id1, id2)

  @private
  update_datepicker_order:
      order((WDatepicker.config, (Date.date -> void), string, Date.date, bool),
        Order.default) =
    Order.make_unsafe(update_datepicker_cmp)

  @private @both_implem
  update_datepicker_mem =
    Cache.make(Cache.Negociator.always_necessary(update_datepicker),
      {Cache.default_options with
        storage = {ordering = update_datepicker_order}
      }).get

  @private @both_implem calendar_header_mem = Cache.simple(calendar_header)
  @private @both_implem options_year_mem    = Cache.simple(options_year)
  @private @both_implem options_month_mem   = Cache.simple(options_month)
}}
