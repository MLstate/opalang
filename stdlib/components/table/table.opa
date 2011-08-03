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
 * @category WIDGET
 * @author Adrien Jonquet, 2010-2011
 */

import stdlib.widgets.core
import stdlib.widgets.select
import stdlib.interactions

// dummy code to keep the map balanced TO BE REPLACED BY NEW MAP IMPLEMENTATION

type BMap.intmap('a) = intmap('a)

BMap = {{

  size(map) = Map.size(map)

  exists(f, map) =  Map.exists(f, map)

  get_p(f,map) = match Map.retrieve(f,map) with
    | [e] -> some(e)
    | _ -> none
  end

  add(index, v, map) =
    if Int.equals(index, (size(map))) then
      Map.add(index, v, map)
    else
      map_l = Map.To.val_list(map)
      rec build_map(l, i, to_insert, acc) =
        match l with
        | [] -> acc
        | [hd|tl] ->
          if Int.equals(i, index) && to_insert then
            build_map(l, (i+1), false, Map.add(i, v, acc))
          else
            build_map(tl, (i+1), to_insert, Map.add(i, hd, acc))
        end
      build_map(map_l, 0, true, Map.empty)

  get(k, map) = Map.get(k, map)

  remove(k, map) =
    map = Map.remove(k, map)
    From.list_gen(Map.To.val_list(map), identity)

  empty = Map.empty

  To = {{
    val_list(map) = Map.To.val_list(map)
  }}

  From = {{

    assoc_list(l) = Map.From.assoc_list(l)

    list_gen(list:list('row), f:('row -> 'a)) =
      rec aux(i, l, map) =
        match l with
        | [] -> map
        | [td:'row|tl] -> aux((i+1), tl, add(i, f(td), map))
        end
      aux(0, list, empty)
  }}


}}

type CTable.Cell.config('a) = {
  on_open : string -> void
  on_close : string -> void
  on_change : string, 'a -> void
  style : WStyler.styler
  openable : bool
}

type CTable.Cell.widget('a) = {
  html : CTable.Cell.config('a), string, IEditable.content('a)-> xhtml
  do_open : CTable.Cell.config('a), string -> void
  do_close : CTable.Cell.config('a), string -> void
  parse : CTable.Cell.config('a), string -> IEditable.content('a)
  set_value : CTable.Cell.config('a), string, 'a -> void
  set_invalid_value : CTable.Cell.config('a), string, option('a) -> void
  clear_value : CTable.Cell.config('a), string -> void
}

type CTable.coordinates('row, 'col) = ('row, 'col)

type CTable.update('state) =
  { NoChange }
/ { NewState : 'state }
/ { Finish }


type CTable.style = {
  table : (string, WStyler.styler)
  td : (string, WStyler.styler)
}

type CTable.error('a, 'state, 'row, 'col) =
  { Index : (int, CTable.internal_msg('a, 'state, 'row, 'col)) }
/ { KeyRow : ('row, CTable.internal_msg('a, 'state, 'row, 'col)) }
/ { KeyCol : ('col, CTable.internal_msg('a, 'state, 'row, 'col)) }

type CTable.header_sort('row) = ('row, 'row -> Order.comparison)

type CTable.header_filter_search('row) = (string,'row -> bool)

type CTable.header_filter_select('row) = list((string,('row -> bool)))

type CTable.header_filter('row) =
  {search:CTable.header_filter_search('row)}
/ {select:CTable.header_filter_select('row)}

type CTable.header('a, 'state, 'row, 'col) =
  {title:string}
/ {custom:CTable.t('a, 'state, 'row, 'col) -> xhtml}
/ {title:string
   sort:option(CTable.header_sort('row))
   filter:option(CTable.header_filter('row))}

type CTable.config('a, 'state, 'row, 'col) = {
  cell_widget : CTable.coordinates('row, 'col), CTable.t('a, 'state, 'row, 'col) -> (CTable.Cell.widget('a), WStyler.styler, bool)
  style : CTable.style
  row_to_string : 'row -> string
  col_to_string : 'col -> string
  headers : option('col -> CTable.header('a, 'state, 'row, 'col))
  only_one_cell_open : bool
  max_number_of_lines_per_page : option(int)
  on_exists_open : string, CTable.coordinates('row, 'col) -> void
  on_forall_closed : string -> void
  prevent_empty_row_at_the_end : bool
  prevent_empty_col_at_the_end : bool
}

type CTable.data_writer('a, 'state, 'row, 'col) = {
  initial_state : 'state
  on_cell_change : 'state, CTable.coordinates('row, 'col), 'a -> CTable.update('state)
}

type CTable.msg('a, 'state, 'row, 'col) =
  { AddRow : ('row, list(('col, 'a))) }
/ { AddRows : list(('row, list(('col, 'a)))) }
/ { InsertRow : (int, 'row, list(('col, 'a))) }
/ { DeleteRow : 'row }
/ { DeleteRows : list('row) }
/ { ClearRow : ('row, list('col)) }
/ { SetRow : ('row, list(('col, 'a))) }
//
/ { AddColumn : ('col, list(('row, 'a))) }
/ { AddColumns : list(('col, list(('row, 'a)))) }
/ { InsertColumn : (int, 'col, list(('row, 'a)))}
/ { DeleteColumn : 'col }
/ { DeleteColumns : list('col) }
/ { ClearColumn : 'col }
/ { SetColumn : ('col, list(('row, 'a))) }
//
/ { SetValues : list((CTable.coordinates('row, 'col), 'a)) }
/ { ClearValues : (list('row), list('col)) }
/ { IterValues : (list('row), list('col), (IEditable.content('a) -> void)) }
//
/ { GetDisplay : CTable.display -> void }
/ { SetDisplay : CTable.display }
/ { ChangePage : int }
/ { Refresh }
//
/ { ScrollDown : int }
/ { ScrollUp : int }
/ { ScrollLeft : int }
/ { ScrollRight : int }
//
/ { SortRows : (('row, 'row -> Order.comparison),option(string)) }
//
/ { SetRowFilter : ('row -> bool,string) }
/ { UpdateRowFilter : ('row -> bool,string) }
/ { ClearRowFilter }
//
/ { SelectRow : 'row}
/ { SelectRows : list('row)}
/ { SelectAll}
/ { SelectClear}
/ { SelectDo : list('row) -> void}
//
/ { Terminate }


type CTable.internal_msg('a, 'state, 'row, 'col) =
  CTable.msg('a, 'state, 'row, 'col)
/ { Open : (CTable.coordinates('row, 'col), string) }
/ { Close : (CTable.coordinates('row, 'col), string) }
/ { Change : (CTable.coordinates('row, 'col), string, 'a) }
/ { GetSize : (int, int, int, int, int -> void)}

type CTable.callbacks('a, 'state, 'row, 'col) = {
  error : CTable.error('a, 'state, 'row, 'col) -> void
  data_writer : CTable.data_writer('a, 'state, 'row, 'col)
  request_value : 'row, 'col -> option('a)
  overrun_up : CTable.t('a, 'state, 'row, 'col), int, int -> void
  overrun_down : CTable.t('a, 'state, 'row, 'col), int, int -> void
  overrun_left : CTable.t('a, 'state, 'row, 'col), int, int -> void
  overrun_right : CTable.t('a, 'state, 'row, 'col), int, int -> void
  resize_table : CTable.t('a, 'state, 'row, 'col), CTable.display, int, int, int, int -> void
}

@abstract
type CTable.t('a, 'state, 'row, 'col) = channel(CTable.internal_msg('a, 'state, 'row, 'col))

type CTable.display = {
  top_row_index : int
  left_col_index : int
  row_header_size : int
  col_header_size : int
  row_page_size : int
  col_page_size : int
}

@private
type CTable.table_sort_state =
  {asc:string}
/ {des:string}
/ {off}

@private
type CTable.table_state('a, 'state, 'row, 'col) = {
  cur_opened_cell : option(CTable.coordinates('row, 'col))
  row_map : BMap.intmap(('row, bool, bool)) /* index map of row_key * is_hidden (filter option) */
  col_map : BMap.intmap('col)
  display : CTable.display
  display_bottom : int
  display_rows : int
  filter : list((string,('row -> bool)))
  sort : CTable.table_sort_state
}

CTable = {{

 /*
  * Ids generation
  */
  @private
  gen_id_aux(l)=String.concat("_",l)
  @private
  gen_cell_id(id, config, x, y, simple) =
    if simple
    then gen_id_aux([id,config.row_to_string(x)])
    else gen_id_aux([id,config.row_to_string(x),config.col_to_string(y)])
  @private
  gen_row_id(id, config, x) = gen_id_aux([id,config.row_to_string(x)])
  @private
  gen_col_header_id(id, config, col) = gen_id_aux([id,"header",config.col_to_string(col)])
  @private
  prefix_td(p) = "{p}_td"
  @private
  prefix_table(p) = "{p}_table"


 /*
  * About cells
  */

  @private
  make_cell_config(channel, coord, styler, openable) =
    on_open(channel, coord : CTable.coordinates('row, 'col), id : string):void =
      Session.send(channel, { Open = (coord,id) })
    on_close(channel, coord : CTable.coordinates('row, 'col), id : string):void =
      Session.send(channel, { Close = (coord,id) })
    on_change(channel, coord : CTable.coordinates('row, 'col), id : string, val:'a):void =
      Session.send(channel, { Change = (coord,id,val) })
    { on_open = on_open(channel, coord,_)
      on_close = on_close(channel, coord,_)
      on_change = on_change(channel, coord,_,_)
      style = styler
      openable = openable }

 /*
  * Html generation
  */
  @private
  gen_col_html(simple : bool, table_id : string, config : CTable.config('a, 'state, 'row, 'col), channel  : CTable.t('a, 'state, 'row, 'col), row:'row, col:'col, value_opt:option('a)) =
    (widget, styler, openable) = config.cell_widget((row, col), channel)
    cell_config = make_cell_config(channel, (row, col), styler, openable)
    cell_id = gen_cell_id(table_id, config, row, col, simple)
    (td_prefix, td_style) = config.style.td
    value = match value_opt with
      | {none} -> {missing_value}
      | {some=value} -> {~value}
      end
    html = widget.html(cell_config, cell_id, value)
    if simple
    then WStyler.add(td_style,html)
    else
      <td id="{cell_id}" style="display:none" class={[prefix_td(td_prefix)]}>
        {html}
      </td>
      |> WStyler.add(td_style,_)

  @private
  gen_row_html(simple : bool, table_id : string, config : CTable.config('a, 'state, 'row, 'col), channel : CTable.t('a, 'state, 'row, 'col), row:'row, cols:list(('col, 'a)), all_cols:list('col)) =
    row_id = gen_row_id(table_id, config, row)
    <tr id="{row_id}" class="table_row_hide" style="display:none">
      {List.map(col -> gen_col_html(simple, table_id, config, channel, row, col, List.assoc(col, cols)), all_cols)}
    </tr>

  @private
  gen_col_header(simple,id, config, channel, f, col)=
    header_col_id=gen_col_header_id(id,config,col)
    match f(col) with
      | ~{title} -> <th style="display:none" id="{header_col_id}">{title}</th>
      | ~{custom} ->
        if simple
        then custom(channel)
        else <th style="display:none" id="{header_col_id}">{custom}</th>
      | ~{title sort filter} ->
        id = Dom.fresh_id()
        sort_html = match sort with
          | {some=f} -> <a onclick={_ -> CTable.send(channel,{SortRows=(f,some(config.col_to_string(col)))})}>Δ</a>
          | {none} -> <></>
        end
        filter_html = match filter with
          | {some={~select}} ->
            on_select(s) =
              _ = match List.assoc(s, select) with
                | {~some} -> CTable.send(channel,{UpdateRowFilter=(some,id)})
                | {none} -> CTable.send(channel,{UpdateRowFilter=(_ -> false,id)})
              end
              true
            config = { WSelect.default_config_with_css("filter") with
              ~on_select
              stylers.icon= WStyler.make_class(["custom_triangle"])
              stylers.select=WStyler.make_class(["custom_select"])
            }
            item(filter)= {
              content = {key=filter title=filter; value= <>{filter}</>};
              state = {selected= (filter==title)};
              styler = WStyler.make_style(css {display: inline-block})
            }
            WSelect.html(config, id, List.map(item,[title | List.map(_.f1,select)]))
          | {some={~search}} ->
            id = "{id}_search"
            f(_)=
              s = Dom.get_value(#{id})
              if s == ""
              then
                CTable.send(channel,{UpdateRowFilter=(_ -> false,id)})
              else
                CTable.send(channel,{UpdateRowFilter=(search(s,_),id)})
            <>
              {title}
              <input style="position:absolute" id={id} type="text" onchange={f} />
            </>
          | {none} -> <>{title}</>
        end
        <th style="display:none" id="{header_col_id}">{sort_html} {filter_html}</th>
    end

 /*
  * Sow/Hide - Row/Cell
  */
  @private
  dom_show_cell(_dur, dom, simple) =
    if not(simple)
    then ignore(Dom.set_style_property_unsafe(dom,"display","table-cell"))

  @private
  dom_hide_cell(_dur, dom, _simple) = ignore(Dom.set_style_property_unsafe(dom,"display","none"))
  @private
  dom_show_row(_dur, dom) =
    _ = Dom.set_style_property_unsafe(dom,"display","table-row")
    _ = Dom.add_class(dom, "table_row_show")
    _ = Dom.remove_class(dom, "table_row_hide")
    void
  @private
  dom_hide_row(_dur, dom) =
    _ = Dom.set_style_property_unsafe(dom,"display","none")
    _ = Dom.remove_class(dom, "table_row_show")
    _ = Dom.add_class(dom, "table_row_hide")
    void
 /*
  * Get next/previous unfiltered row
  */
  @private
  next_unfiltered_index_aux(index, row_map, incr:int) =
    rec aux(i) =
      match BMap.get(i, row_map) with
        | {some=(_, {true}, _)} -> aux(i+incr)
        | {some=(_, {false}, _)} -> some(i)
        | {none} -> none
    aux(index)
  @private
  next_unfiltered_index(index, row_map) =
    next_unfiltered_index_aux(index, row_map, 1)
  @private
  prev_unfiltered_index(index, row_map) =
    next_unfiltered_index_aux(index, row_map, -1)

 /*
  * Check the existance of the map key in the row_map/col_map
  */
  @private
  check_row_key(row:'row,row_map) =
    BMap.exists((_, (k, _, _) -> k == row), row_map)
  @private
  check_col_key(col:'col,col_map) =
    BMap.exists((_, k -> k == col), col_map)

 /*
  * Request value
  */
  @private
  widget_value(simple, table_id, config, (row,col), channel, action) =
    (widget, styler, openable) = config.cell_widget((row, col), channel)
    cell_id = gen_cell_id(table_id, config, row, col, simple)
    cell_config = make_cell_config(channel, (row, col), styler, openable)
    match action with
      | {~value} ->
        widget.set_value(cell_config, cell_id, value)
      | {clear} -> widget.clear_value(cell_config, cell_id)
      | {~request} -> match widget.parse(cell_config, cell_id) with
          | {missing_value} ->
            match request(row, col) with
              | {none} -> void
              | {some=value} -> widget.set_value(cell_config, cell_id, value)
            end
          | _ -> void
        end
    end
 /*
  * other helpers
  */
  @private
  for_iter(i, max)(f:(int -> void)) = ignore(for(i,(i -> do f(i) (i+1)),_<=max))

  @private
  myselect(table_id,s) = Dom.select_raw("#{table_id}_table tbody tr{s}")

  @private
  refresh_parity(table_id,b)=
    even = if b then "even" else "odd"
    odd = if b then "odd" else "even"
    dom_even = myselect(table_id, ".table_row_show:even")
    dom_odd =  myselect(table_id, ".table_row_show:odd")
    _ =  Dom.add_class(dom_even, even)
    _ = Dom.remove_class(dom_even, odd)
    _ = Dom.add_class(dom_odd, odd)
    _ = Dom.remove_class(dom_odd, even)
    void

  /*
   * Message handling for the table session
   */
  @private
  table_on_message(simple, table_id, config, callbacks, state : CTable.table_state, msg, channel) =
    row_table_size = BMap.size(state.row_map)
    col_table_size = BMap.size(state.col_map)

    row_header_size = state.display.row_header_size
    col_header_size = state.display.col_header_size

    row_page_size = state.display.row_page_size
    col_page_size = state.display.col_page_size

    top = state.display.top_row_index
    bottom = state.display_bottom
    left = state.display.left_col_index
    right =
      r = left + state.display.col_page_size - 1
      if r >= col_table_size
      then (col_table_size - 1)
      else r

   /*
    * Get the index of a key
    */
    get_row_index(row:'row, row_map) =
      match BMap.get_p((_, (k, _, _) -> k == row), row_map) with
        | {some=(i,_)} -> some(i)
        | {none} -> none


   /*
    * Short cuts
    */
    gen_col_html(row:'row, col:'col, val:'a) =
      gen_col_html(simple, table_id, config, channel, row, col, val)

    gen_row_html(row:'row, cols:list(('col, 'b))) =
      gen_row_html(simple, table_id, config, channel, row, cols, BMap.To.val_list(state.col_map))

    myselect(s) = myselect(table_id,s)

    refresh_parity(b)=refresh_parity(table_id,b)
    /*
     * Call the callback request_value on all the cells of a given row
     */
    request_values(row) =
      for_iter(0, col_table_size-1)(i ->
        match BMap.get(i, state.col_map) with
          | {some=col} ->
            if ((i >= left) && (i <= right))
            then widget_value(simple, table_id, config, (row,col), channel, {request=callbacks.request_value})
            else
              cell_id = gen_cell_id(table_id, config, row, col, simple)
              dom_hide_cell(0, #{cell_id},simple)
          | {none} -> void
        )

   /*
    * Adjust top/bottom to avoid empty lines
    * return the new top/bottom and the nb of displayed rows
    */
    adjust_bottom(bottom,nb,max,row_map)=
      rec aux(bottom,nb) =
        if nb >= max
        then (bottom,nb)
        else
          match next_unfiltered_index(bottom+1, row_map) with
            | {some=b} -> aux(b,nb+1)
            | {none} -> (bottom,nb)
      aux(bottom,nb)

    adjust_top(top,nb,max,row_map)=
      rec aux(top,nb) =
        if nb >= max
        then (top,nb)
        else
          match prev_unfiltered_index(top-1, row_map) with
            | {some=b} -> aux(b,nb+1)
            | {none} -> (top,nb)
      aux(top,nb)

   /*
    * Scroll down/up
    */
    scroll_down(scroll,cur_nb,prevent)=
      //do Log.debug("scrolldown_bottom1",bottom)
      (bottom,nb) = adjust_bottom(bottom,cur_nb,min(row_page_size,cur_nb+scroll),state.row_map)
      scroll = scroll - (nb - cur_nb)
      //do Log.debug("scrolldown",scroll)
      //do Log.debug("scrolldown_bottom2",bottom)
      rec aux(top, bottom,scroll,nb) =
        if scroll==0
        then (top,bottom,scroll,nb)
        else
          match (next_unfiltered_index(bottom+1,state.row_map),next_unfiltered_index(top+1, state.row_map),prevent) with
            | ({some=b},{some=t},_) -> aux(t,b,scroll-1,nb)
            | ({none},{some=t},{false}) -> aux(t,bottom,scroll-1,nb-1)
            | _ -> (top,bottom,scroll,nb)
          end
      //do Log.debug("scrolldown_before","{top} -> {bottom}")
      (top, bottom,scroll,nb) =
        if top > bottom
        then (top,bottom,scroll,nb)
        else aux(top,bottom,scroll,nb)
      //do Log.debug("scrolldown_after","{top} -> {bottom}")
      (top, bottom,scroll,nb)

    scroll_up(scroll,cur_nb,prevent)=
      (top,nb) = adjust_top(top,cur_nb,min(row_page_size,cur_nb+scroll),state.row_map)
      scroll = scroll - (nb - cur_nb)
      rec aux(top, bottom,scroll,nb) =
        if scroll==0
        then (top,bottom,scroll,nb)
        else
          match (prev_unfiltered_index(bottom-1,state.row_map),prev_unfiltered_index(top-1, state.row_map),prevent) with
            | ({some=b},{some=t},_) -> aux(t,b,scroll-1,nb)
            | ({some=b},{none},{false}) -> aux(top,b,scroll-1,nb-1)
            | _ -> (top,bottom,scroll,nb)
          end
      //do Log.debug("scrollup_before","{top} -> {bottom}")
      (top, bottom,scroll,nb) =
        if top > bottom
        then (top,bottom,scroll,nb)
        else aux(top,bottom,scroll,nb)
      //do Log.debug("scrollup_after","{top} -> {bottom}")
      (top, bottom,scroll,nb)

   /*
    * Hide/Display unfiltered rows
    */
    hide_rows(row_map, top, bottom) =
      rec aux(top) =
       if top > bottom
       then void
       else
         match BMap.get(top, row_map) with
           | {some=(_, {true}, _)} -> aux(top+1)
           | {some=(row, {false}, _)} ->
              row_id = gen_row_id(table_id, config, row)
              do dom_hide_row(0, #{row_id})
              aux(top+1)
           | {none} -> void
      aux(top)

    show_rows(row_map, top, bottom) =
      rec aux(top) =
        if (top > bottom)
        then void
        else
          match BMap.get(top, row_map) with
            | {some=(_, {true}, _)} -> aux(top+1)
            | {some=(row, {false}, _)} ->
              row_id = gen_row_id(table_id, config, row)
              do dom_show_row(0, #{row_id})
              do request_values(row)
              aux(top+1)
            | {none} -> void
      aux(top)

    /*
     * Display (right - left) cols for all rows rows starting from left
     */
    show_cols(col_header_size, left, right, top, bottom) =
      do for_iter(top, bottom)(i ->
        (row, _, _) = BMap.get(i, state.row_map) ? @fail("should not happen") //OK
        for_iter(col_header_size, col_table_size-1)(j ->
            col = BMap.get(j, state.col_map) ? @fail("should not happen") //OK
            cell_id = gen_cell_id(table_id, config, row, col, simple)
            if (j >= left) && (j <= right) then
              do dom_show_cell(0, #{cell_id},simple)
              widget_value(simple, table_id, config, (row,col), channel, {request=callbacks.request_value})
            else
              dom_hide_cell(0, #{cell_id},simple)
        )
      )
      for_iter(col_header_size, col_table_size-1)(j ->
          col = BMap.get(j, state.col_map) ? @fail("should not happen") //OK
          cell_id =gen_col_header_id(table_id,config,col)
          if (j >= left) && (j <= right) then

            dom_show_cell(0, #{cell_id},simple)
          else
            dom_hide_cell(0, #{cell_id},simple)
      )

    /*
     * Update the display according to the filtered row map
     * and return the new bottom
     */
    filter_row_display(row_map) =
      rec aux(i, count, top, bottom) =
        if count >= state.display.row_page_size
        then (top,bottom, count)
        else
          match BMap.get(i, row_map) with
            | {some=(row, {true}, _)} ->
              row_id = gen_row_id(table_id, config, row)
              do dom_hide_row(0, #{row_id})
              aux(i+1, count, top, bottom)
            | {some=(row, {false}, _)} ->
              row_id = gen_row_id(table_id, config, row)
              do dom_show_row(0, #{row_id})
              do request_values(row)
              top = if Option.is_none(top) then some(i) else top
              aux(i+1, count+1, top, some(i))
            | {none} -> (top, bottom, count)
          end
      //do Log.debug("filter_row_display","top : {top}")

      (ntop, nbottom, count) = aux(top, 0, none, none)
      match (ntop,nbottom) with
        | ({some=ntop},{some=nbottom}) ->
          do hide_rows(state.row_map, top, ntop-1)
          do hide_rows(state.row_map, nbottom+1, bottom)
          (ntop, nbottom, count)
        | _ ->
          do hide_rows(state.row_map, top, bottom)
          (top, top-1, count)
      end

    update_row_display(new_row_map,sort_opt)=
      (new_top,new_bottom,display_rows) = filter_row_display(new_row_map)
      // the new top after filter
      //do Log.debug("filter","new_bottom {new_bottom}")
      //do Log.debug("filter","new_top {new_top}")
      // the possible top if the shown table became to small (according to the display)
      (top_possible,display_rows) =
        if config.prevent_empty_row_at_the_end
        then adjust_top(new_top,display_rows,state.display.row_page_size, new_row_map)
        else (new_top,display_rows)

      //do Log.debug("filter","top_possible {top_possible}")

      do if top_possible != new_top then // scroll
        show_rows(new_row_map, top_possible, new_top)
      do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
      // new_bottom = prev_unfiltered_index(new_bottom, new_row_map)
      //do Log.debug("update_row","display_rows : {display_rows}")
      { state with row_map = new_row_map
                           display.top_row_index = top_possible
                           display_bottom = new_bottom
                           ~display_rows
                           sort = sort_opt ? state.sort }

    /*
     * Apply a filter on the row_map and returns it
     */
    apply_filter(filter, i, max, row_map) =
      (list_filter,update_map) = match filter with
        | {empty} ->
          l = []
          f(row_map,row,_,i,s) = Map.add(i, (row, false,s), row_map)
          (l,f)
        | {set=(filter,key)} ->
          l=[(key,filter)]
          f(row_map,row,_,i,s)=
            if filter(row)
            then Map.add(i, (row, true,s), row_map)
            else Map.add(i, (row, false,s), row_map)
          (l,f)
        | {update=(filter,key)} ->
          (l,g) = match List.extract_p((x,_) -> x == key,state.filter) with
            | ({some=_},l) ->
              l = [(key,filter)| l]
              f(r,_)=List.fold((_,f),b -> b || f(r),l,false)
              (l,f)
            | ({none},_) ->
              l = [(key,filter)| state.filter]
              f(row,filtered)= filtered || filter(row)
              (l,f)
          end
          f(row_map,row,filtered,i,s) =
            if g(row,filtered)
            then Map.add(i, (row, true, s), row_map)
            else Map.add(i, (row, false, s), row_map)
          (l,f)
      end
      rec aux(i, row_map) =
        if (i > max) || (i > row_table_size)
        then row_map
        else match BMap.get(i, row_map) with
          | {some=(row, filtered, s)} ->
            map = update_map(row_map,row,filtered,i,s)
            aux(i+1,map)
          | {none} -> row_map
        end
      //do Log.debug("apply_filter","i : {i}, max : {max}")
      state = update_row_display(aux(i, row_map),none)
      {state with filter=list_filter}

    is_filtered(row) =
      List.exists((_, f) -> f(row), state.filter)
/*
      rec aux(fs,b) =
        if b
        then true
        else
          match fs with
            | [] -> b
            | [(_,f)|fs] -> aux(fs,f(row))
        end
      aux(state.filter,false)
*/

    /*
     * Add a rows to the row_map and display them if they are in the display scope
     * return new_bottom, nb_rows, row_map
     */
    add_row(row, cols, nb_rows, row_map) =
      row_html = gen_row_html(row, cols)
      do Dom.transform([#{"{table_id}_table tbody"} +<- row_html])
      // Check if the added row is in the display scope
      (bottom,nb_rows) =
        if nb_rows < row_page_size && not(is_filtered(row))
        then
          // Show the added row
          do request_values(row)
          // a new row added to the display -> bottom++
          (bottom + 1, nb_rows + 1)
        else
          // hide the row
          row_id = gen_row_id(table_id, config, row)
          do dom_hide_row(0, #{row_id})
          // bottom unchanged
          (bottom, nb_rows)
      // add the row to the row_map
      row_map = BMap.add(BMap.size(row_map), (row, false, false), row_map)
      (bottom,nb_rows, row_map)

    add_rows(rows, row_map, msg) =
      rec aux(rows, row_map, bottom, nb_rows) =
        match rows with
        | [] ->
          { set = { state with row_map = row_map display_bottom = bottom display_rows=nb_rows } }
        | [ (row, cols) | rows ] ->
           if check_row_key(row,row_map) then
             _ = callbacks.error({ KeyRow = (row, msg) })
             { unchanged }
           else
             (new_bottom, nb_rows, row_map) = add_row(row, cols, nb_rows, row_map)
             aux(rows, row_map, new_bottom, nb_rows)
        end
        r = aux(rows, row_map, state.display_bottom, state.display_rows)
        do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
        r
    /*
     * Add a cols to the col_map and display them if they are in the display scope
     */
    add_cols(cols, col_map,simple) =
      rec aux(cols, col_map) =
        match cols with
        | [] ->  { set = { state with col_map = col_map } }
        | [ (col, vals) | cols ] ->
          if check_col_key(col,col_map) then
            _ = callbacks.error({ KeyCol = (col, msg) })
            { unchanged }
          else
            do for_iter(0, row_table_size-1)(i ->
              (row, _, _) = BMap.get(i, state.row_map) ? @fail("should not happen") // OK
              val = List.assoc(row, vals)
              col_html = gen_col_html(row, col, val)
              row_id = gen_row_id(table_id, config, row)
              do Dom.transform([#{row_id} +<- col_html])
              // check if the added column is in the scope
              cell_id = gen_cell_id(table_id, config, row, col, simple)
              if col_page_size > (col_table_size - col_header_size)
              then dom_show_cell(0, #{cell_id},simple)
              else dom_hide_cell(0, #{cell_id},simple)
              )
            do match config.headers with
              | {some=f} ->
                 h_html = gen_col_header(simple,table_id, config, channel, f, col)
                 do Dom.transform([#{"{table_id}_table thead tr"} +<- h_html])
                 h_id = gen_col_header_id(table_id,config,col)
                 if col_page_size > (col_table_size - col_header_size)
                 then dom_show_cell(0, #{h_id}, simple)
                 else dom_hide_cell(0, #{h_id}, simple)
              | _ -> void
            // add the col to the col_map)
            col_map = BMap.add(col_table_size, col, col_map)
            aux(cols, col_map)
        end
      aux(cols, col_map)

    /*
     * Delete rows from the row_map and update the display
     */
    delete_rows(rows, row_map) =
      rec aux(rows, row_map, cur_top, cur_bottom, display_rows) =
        match rows with
        | [] -> { set = { state with row_map = row_map
                                     display = { state.display with top_row_index = cur_top }
                                     display_bottom = cur_bottom
                                     ~display_rows } }
        | [ row | rows ] ->
          if not(check_row_key(row,row_map)) then
            _ = callbacks.error({ KeyRow = (row, msg) })
            { unchanged }
          else
            row_id = gen_row_id(table_id, config, row)
            pos = get_row_index(row,state.row_map) ? @fail("should not happen") //OK
            // If the deleted row is filtered, move only the bottom index
            (new_top, new_bottom,minus) =
              if pos < top
              then (top-1,bottom-1,0)
              else if pos > bottom
              then (top,bottom,0)
              else //pos>=top && <=bottom // one bottom
                if Option.switch(_.f2, true,BMap.get(pos, row_map)) //filtered
                then (top,bottom-1,0)
                else match next_unfiltered_index(bottom+1, row_map) with
                  | {~some} -> //  move bottom + hide
                    (new_row, _, _) = BMap.get(some, row_map) ? @fail("should not happen") //OK
                    new_row_id = gen_row_id(table_id, config, new_row)
                    do dom_show_row(0, #{new_row_id})
                    //do Log.debug("delete","go down")
                    (top,bottom,0)
                  | {none} -> match prev_unfiltered_index(top-1, row_map) with
                    | {~some} ->
                    (new_row, _, _) = BMap.get(some, row_map) ? @fail("should not happen") //OK
                    new_row_id = gen_row_id(table_id, config, new_row)
                    do dom_show_row(0, #{new_row_id})
                    //do Log.debug("delete","go up")
                    (some,bottom-1,0)
                    | {none} -> match prev_unfiltered_index(bottom-1, row_map) with
                      | {~some} -> (top,some,1)
                      | {none} ->  (top,top-1,1)
                    end
                end
            do Dom.remove(#{row_id})
            display_rows = display_rows - minus
            //do Log.debug("display_rows(delete)", display_rows)
            row_map = BMap.remove(pos, row_map)
            aux(rows, row_map, new_top, new_bottom,display_rows)
          end
        r = aux(rows, row_map, top, bottom,state.display_rows)
        do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
        r
    /*
     * Delete cols from the col_map and update the display
     */
    delete_cols(cols, col_map, display,simple) =
      rec aux(cols, col_map, new_left) =
        match cols with
        | [] -> { set = { state with col_map = col_map display = { state.display with left_col_index = new_left } } }
        | [ col | cols ] ->
          if not(check_col_key(col,col_map)) then
            _ = callbacks.error({ KeyCol = (col, msg) })
            { unchanged }
          else
            // remove the col from the col_map
            rec aux_map(i, max, col, col_map) =
              if i > max
              then @fail("should not happen") //OK
              else if col == (BMap.get(i, col_map) ? @fail("should not happen")) //OK
              then (BMap.remove(i, state.col_map), i)
              else aux_map((i+1), max, col, col_map)

            (col_map, pos) = aux_map(0, col_table_size, col, state.col_map)

            do for_iter(0, (row_table_size - 1))(i ->
              (row, _, _) = BMap.get(i, state.row_map) ? @fail("should not happen") //OK
              cell_id = gen_cell_id(table_id, config, row, col, simple)
              h_id = gen_col_header_id(table_id,config,col)
              do Dom.remove(#{cell_id})
              do Dom.remove(#{h_id})
              // update the display
              if (pos >= left) && (pos <= right) then
                // if the deleted row was the bottom do nothing
                // else display the new col
                match BMap.get(right+1, state.col_map) with
                  | {some=new_col} ->
                    new_cell_id = gen_cell_id(table_id, config, row, new_col, simple)
                    h_id = gen_col_header_id(table_id,config,new_col)
                    do dom_show_cell(0, #{new_cell_id}, simple)
                    dom_show_cell(0, #{h_id},simple)
                  | _ -> void
            )
            // check if the inserted col was above the left position to move the index
            new_left = if pos < left then left - 1 else left
            aux(cols, col_map, new_left)
        end
      aux(cols, col_map, display.left_col_index)


    set_display(new_display,only_refresh) =
      // test if callbacks should be called
      (_callback_down, _scroll_down) =
        if (new_display.row_page_size + new_display.row_header_size) > row_table_size then
          (true, (new_display.row_page_size  + new_display.row_header_size) - row_table_size)
        else
          (false, 0)

      (_callback_right, _scroll_right) =
        if (new_display.col_page_size + new_display.col_header_size) > col_table_size then
          (true, (new_display.col_page_size + new_display.col_header_size) - col_table_size)
        else
          (false, 0)

       // if callback_down || callback_right then
       //   _ = callbacks.resize_table(channel, new_display, scroll_down, (row_table_size-1), scroll_right, (col_table_size-1))
       //   { unchanged }
       // else

        do if not(only_refresh) then hide_rows(state.row_map, top, bottom)
        newtop = match next_unfiltered_index(new_display.top_row_index, state.row_map) with
          | {some=i} -> i
          | {none} -> new_display.top_row_index
        rec aux(i,n,v)=
          if n == 0
          then (v,0)
          else
            match next_unfiltered_index(i,state.row_map) with
              | {some=_} -> aux(i+1,n-1,i)
              | {none} -> (v,n)
          end
        (new_bottom,minus) = aux(newtop,new_display.row_page_size,new_display.top_row_index)
        do show_rows(state.row_map, new_display.top_row_index, new_bottom)
        do show_cols(new_display.col_header_size, new_display.left_col_index, new_display.left_col_index + new_display.col_page_size - 1, new_display.top_row_index, new_bottom)
        do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
        display_rows = new_display.row_page_size - minus
        display = {
         top_row_index = newtop
         left_col_index = new_display.left_col_index
         row_header_size = new_display.row_header_size
         col_header_size = new_display.col_header_size
         row_page_size = new_display.row_page_size
         col_page_size = new_display.col_page_size
        }

        { set = { state with display = display
                              ~display_rows
                             display_bottom = new_bottom } }
    // _select_row(rows) =
    //   rec aux(i,map)=
    //     match BMap.get(i,map) with
    //       | {some=(r,f,_)} ->
    //         if List.mem(r,rows)
    //         then
    //           id_row = gen_row_id(table_id,config,r)
    //   	      do Dom.add_class(#{id_row},"selected")
    //           BMap.add(i,(r,f,true),map)
    //         else map
    //       | {none} -> map
    //   _map = aux(0,state.row_map)
    //   @todo

    // _unselect_row(rows) =
    //   rec aux(i,map)=
    //     match BMap.get(i,map) with
    //       | {some=(r,f,_)} ->
    //         if List.mem(r,rows)
    //         then
    //           id_row = gen_row_id(table_id,config,r)
    //   	      do Dom.remove_class(#{id_row},"selected")
    //           BMap.add(i,(r,f,false),map)
    //         else map
    //       | {none} -> map
    //   _map = aux(0,state.row_map)
    //   @todo
    //do Log.debug("top",top)
    //do Log.debug("bottom",bottom)
    //do Log.debug("nb", state.display_rows)
    match msg : CTable.internal_msg with

    // AddRow
    | { AddRow = row } ->   add_rows([row], state.row_map, msg)
    | { AddRows = rows } -> add_rows(rows,  state.row_map, msg)

    // InsertRow
    | { InsertRow = (pos, row, cols) } ->
      if check_row_key(row,state.row_map) then
        _ = callbacks.error({ KeyRow = (row, msg) })
        { unchanged }
      else if (pos >= row_table_size) || (pos < row_header_size) then
        _ = callbacks.error({ Index = (pos, msg) })
        { unchanged }
      else
        row_id_old =
          (r, _, _) = BMap.get(pos, state.row_map) ? @fail("should not happen") //OK
          gen_row_id(table_id, config, r)
        // insert the new row
        inserted_row = Dom.of_xhtml(gen_row_html(row, cols))
        _ = Dom.put_before(#{row_id_old}, inserted_row)
        row_id = gen_row_id(table_id, config, row)
        row_map = BMap.add(pos, (row, is_filtered(row), false), state.row_map)
        (new_top,new_bottom,nb_rows) =
          // Check if the new row is in the display scope
          if (pos >= top) && (pos <= bottom) && not(is_filtered(row)) then
            do request_values(row)
            do dom_show_row(0, #{row_id})
            // hide the moved row from the visible scope
            if state.display_rows < state.display.row_page_size
            then (top,bottom+1,state.display_rows+1)
            else
            (moved_row, _, _) = BMap.get(bottom+1, row_map) ? @fail("should not happen") //OK
            moved_row_id = gen_row_id(table_id, config, moved_row)
            do dom_hide_row(0, #{moved_row_id})
            new_bottom = prev_unfiltered_index(bottom, row_map) ? @fail("should not happen") //OK
            (top,new_bottom,state.display_rows)
          else
            do dom_hide_row(0, #{row_id})
            if pos < top
            then top = next_unfiltered_index(top, row_map) ? @fail("should not happen") //OK
                 bottom = next_unfiltered_index(bottom, row_map) ? @fail("should not happen") //OK
                 (top,bottom,state.display_rows)
            else (top,bottom,state.display_rows)
        do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
        { set = { state with display.top_row_index = new_top
                             display_rows = nb_rows
                             row_map = row_map
                             display_bottom = new_bottom } }

    // DeleteRow
    | { DeleteRow = row } ->   delete_rows([row], state.row_map)
    | { DeleteRows = rows } -> delete_rows(rows,  state.row_map)

    // ClearRow
    | { ClearRow = (row, cols) } ->
      if not(check_row_key(row,state.row_map)) then
        _ = callbacks.error({ KeyRow = (row, msg) })
        { unchanged }
      else
        do List.iter(col ->
          if List.exists((c -> c == col), cols) then
            widget_value(simple, table_id, config, (row,col), channel, {clear})
          else void
        , BMap.To.val_list(state.col_map))
        { unchanged }

    // SetRow
    | { SetRow = (row, cols) } ->
      if not(check_row_key(row,state.row_map)) then
         _ = callbacks.error({ KeyRow = (row, msg) })
         { unchanged }
      else
        do List.iter(col ->
         match List.assoc(col, cols) with
         | { none } -> void
         | { some=value } -> widget_value(simple, table_id, config, (row,col), channel, {~value})
         end
       , BMap.To.val_list(state.col_map))
       { unchanged }

    // AddColumn
    | { AddColumn = col } ->   add_cols([col], state.col_map,simple)
    | { AddColumns = cols } -> add_cols(cols,  state.col_map,simple)

    // InsertColumn
    | { InsertColumn = (pos, col, vals) } ->
      if check_col_key(col,state.col_map) || (pos >= col_table_size) || (pos < col_header_size) then
        _ = callbacks.error({ KeyCol = (col, msg) })
        { unchanged }
      else if (pos >= col_table_size) || (pos < col_header_size) then
        _ = callbacks.error({ Index = (pos, msg) })
        { unchanged }
      else
        do for_iter(0, row_table_size-1)(i ->
          (row, _, _) = BMap.get(i, state.row_map) ? @fail("should not happen") //OK
          old_cell = BMap.get(pos, state.col_map) ? @fail("should not happen") //OK
          old_cell_id = gen_cell_id(table_id, config, row, old_cell, simple)
          val = List.assoc(row, vals)
          col_html = gen_col_html(row, col, val)
          _ = Dom.put_before(#{old_cell_id}, col_html |> Dom.of_xhtml)
          // if col in display scope, hide the current right col
          if (pos >= left) && (pos <= right) then
            do widget_value(simple, table_id, config, (row,col), channel, {request=callbacks.request_value})
            moved_col = BMap.get(right, state.col_map) ? @fail("should not happen") //OK
            moved_cell_id = gen_cell_id(table_id, config, row, moved_col, simple)
            dom_hide_cell(0, #{moved_cell_id},simple)
          else dom_hide_cell(0, #{old_cell_id},simple)
        )
        old_cell = BMap.get(pos, state.col_map) ? @fail("should not happen") //OK
        old_cell_id = gen_col_header_id(table_id, config, old_cell)
        do match config.headers with
          | {some=f} ->
            h_html = gen_col_header(simple, table_id, config, channel, f, col)
            _ = Dom.put_before(#{old_cell_id}, h_html |> Dom.of_xhtml)
            h_id = gen_col_header_id(table_id,config,col)
            if (pos >= left) && (pos <= right) then
              moved_col = BMap.get(right, state.col_map) ? @fail("should not happen") //OK
              moved_cell_id = gen_col_header_id(table_id, config, moved_col)
              dom_hide_cell(0, #{moved_cell_id},simple)
            else dom_hide_cell(0, #{h_id},simple)
          | _ -> void
        // check if the inserted col was above the left position to move the index
        new_left = if pos < left then left + 1 else left
        // add the col to the col_map)
        col_map = BMap.add(pos, col, state.col_map)
        { set = { state with col_map = col_map  display = { state.display with left_col_index = new_left } } }

    // DeletColumn
    | { DeleteColumn = col } -> delete_cols([col], state.col_map, state.display,simple)

    // DeletColumns
    | { DeleteColumns = cols } -> delete_cols(cols, state.col_map, state.display,simple)

    // ClearColumn
    | { ClearColumn = col } ->
      if not(check_col_key(col,state.col_map)) then
        _ = callbacks.error({ KeyCol = (col, msg) })
        { unchanged }
      else
        do for_iter(row_header_size, (row_table_size - 1))(i ->
          (row, _, _) = BMap.get(i, state.row_map) ? @fail("should not happen") //OK
          widget_value(simple, table_id, config, (row,col), channel, {clear})
        )
        { unchanged }

    // SetColumn
    | { SetColumn = (col, rows) } ->
       if not(check_col_key(col,state.col_map)) then
         _ = callbacks.error({ KeyCol = (col, msg) })
         { unchanged }
      else
        do List.iter((row, _,_) ->
         match List.assoc(row, rows) with
         | { none } -> void
         | { some=value } -> widget_value(simple, table_id, config, (row,col), channel, {~value})

         end
        , BMap.To.val_list(state.row_map))
      { unchanged }

    // SetValues
    | { SetValues = cells } ->
      do List.iter((((row, col), value) -> widget_value(simple, table_id, config, (row,col), channel, {~value})), cells)
      { unchanged }

    // ClearValues
    | { ClearValues = (rows, cols) } ->
      do List.iter(row -> List.iter(col -> widget_value(simple, table_id, config, (row,col), channel, {clear}), cols), rows)
      { unchanged }

    // IterValues
    | { IterValues = (rows, cols, f) } ->
      do List.iter(row ->
        List.iter(col ->
          (widget, styler, openable) = config.cell_widget((row, col), channel)
          cell_config = make_cell_config(channel, (row, col), styler, openable)
          cell_id = gen_cell_id(table_id, config, row, col, simple)
          cur_val = widget.parse(cell_config, cell_id)
          f(cur_val)
        , cols)
      , rows)
      { unchanged }
    // Manipulate Display
    | { GetDisplay = f } -> do f(state.display) { unchanged }
    | { SetDisplay = new_display } -> set_display(new_display,false)
    | { Refresh } ->
      do  show_rows(state.row_map, state.display.top_row_index, state.display_bottom)
      do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
      { unchanged }

    // ScrollDown
    | { ScrollDown = scroll } ->
      (ntop,nbottom,_scroll,display_rows) = scroll_down(scroll,state.display_rows,config.prevent_empty_row_at_the_end)
      do show_rows(state.row_map,bottom,nbottom)
      do hide_rows(state.row_map,top,ntop-1)
      do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
      {set = { state with display = { state.display with top_row_index = ntop}
                          display_bottom = nbottom
                          ~display_rows } }

    // ScrollUp
    | { ScrollUp = scroll } ->
      (ntop,nbottom,_scroll,display_rows) = scroll_up(scroll,state.display_rows,config.prevent_empty_row_at_the_end)
      do show_rows(state.row_map,ntop,top)
      do hide_rows(state.row_map,nbottom+1,bottom)
      do refresh_parity(Dom.has_class(myselect(".table_row_show:first"),"even"))
      {set = { state with display = { state.display with top_row_index = ntop }
                          display_bottom = nbottom
                          ~display_rows
                          } }

    // ScrollRight
    | { ScrollRight = scroll } ->
      col_page_size =
        if col_page_size > (col_table_size - col_header_size) then
          (col_table_size - col_header_size)
        else col_page_size
      if Int.equals((left + col_page_size), col_table_size) then
        // call an overrun callback
        _ = callbacks.overrun_right(channel, scroll, col_table_size)
        { unchanged }
      else
        scroll =
          diff = scroll + left + col_page_size - col_table_size
          if diff > 0 then diff
          else scroll
        new_left = left + scroll
        new_right = new_left + col_page_size - 1
       do show_cols(state.display.col_header_size, new_left, new_right, top, bottom)
       { set = { state with display = { state.display with left_col_index = new_left } } }

    // ScrollLeft
    | { ScrollLeft = scroll } ->
      if Int.equals(left, col_header_size) then
        // call an overrun callback
        _ = callbacks.overrun_left(channel, scroll, col_header_size)
        { unchanged }
      else
        scroll =
          diff = left - scroll
          if diff < col_header_size then (col_header_size - diff)
          else scroll
        new_left = left - scroll
        new_right = new_left + col_page_size - 1
        do show_cols(state.display.col_header_size, new_left, new_right, top, bottom)
        { set = { state with display = { state.display with left_col_index = new_left } } }

    // SortRows
    | { SortRows = (compare_key,col_opt) } ->

      get(i, map) = BMap.get(i, map) ? @fail("should not happen") //OK

      switch(map, i1, i2) =
        if i1 != i2 then
          (i1, i2) = if i1 < i2 then (i1, i2) else (i2, i1)
          (row_1, b1,s1) = get(i1, map)
          (row_2, b2,s2) = get(i2, map)
          row_id_1 = gen_row_id(table_id, config, row_1)
          row_id_2 = gen_row_id(table_id, config, row_2)
          do if Int.equals((i2-1), i1) then
            _ = Dom.put_after(#{row_id_2}, #{row_id_1})
            void
          else
            (row_2_pred, _,_) = get(i2-1, map)
            row_id_2_pred = gen_row_id(table_id, config, row_2_pred)
            _ = Dom.put_after(#{row_id_1}, #{row_id_2})
            _ = Dom.put_after(#{row_id_2_pred}, #{row_id_1})
            void
          map = Map.add(i1, (row_2, b2, s2), map)
          Map.add(i2, (row_1, b1, s1), map)
        else map

      (updown,sort) = match col_opt : option(string) with
        | {none} -> (true,{off})
        | {some=col} ->
          match state.sort with
            | {off} -> (false,{asc=col})
            | {des=_} -> (false,{asc=col})
            | {asc=c} ->
              if c == col
              then
                 (true,{des=col})
              else (false,{asc=col})
          end
      end
      reverse =
        | {lt} -> {gt}
        | {gt} -> {lt}
        | {eq} -> {eq}
        | {neq}-> {neq}
      rec split(map, beg, end_, pivot) =
        pivot_val = (get(pivot, map)).f1
        map = switch(map, pivot, end_)
        rec aux(i, cpt, map) =
          if Int.equals(i, end_) then (map, cpt)
          else
            comp = compare_key((get(i, map)).f1, pivot_val)
            comp = if updown then reverse(comp) else comp
            match comp with
            | {lt} | {eq} ->
              map = switch(map, i, cpt)
              aux(i+1, cpt+1, map)
            | _ ->
              aux(i+1, cpt, map)
        (map, cpt) = aux(beg, beg, map)
        map = switch(map, end_, cpt)
        (map, cpt)

      rec quicksort(map) =
        rec aux(map, beg, end_) =
          if beg < end_ then
            pivot = beg
            (map, pos) = split(map, beg, end_, pivot)
            map = aux(map, beg, pos-1)
            aux(map, pos+1, end_)
          else map
        aux(map, state.display.row_header_size, BMap.size(map)-1)

      do hide_rows(state.row_map,top, bottom)
      new_row_map = quicksort(state.row_map)
      state = update_row_display(new_row_map,some(sort))
      {set=state}

    // SetRowFilters
    | { SetRowFilter = (filter,key) } ->
      state = apply_filter({set=(filter,key)}, row_header_size, (row_table_size-1), state.row_map)
      {set=state}

    // UpdateRowFilter
    | { UpdateRowFilter = (filter,key) } ->
      state = apply_filter({update=(filter,key)}, row_header_size, (row_table_size-1), state.row_map)
      {set=state}

    // ClearRowFilter
    | { ClearRowFilter } ->
      state = apply_filter({empty}, row_header_size, (row_table_size-1), state.row_map)
      {set=state}

    // Terminate
    | { Terminate } -> { stop }

    // Open
    | { Open = (coord, _) } ->
      // test if "one cell open at a time" mode is set to true
      if config.only_one_cell_open then
        state =
          match state.cur_opened_cell with
          | { none } -> { state with cur_opened_cell = { some = coord } }
          | { some = (row, col) } ->
            //tell this cell to close since a new one is opened
            (widget, styler, openable) = config.cell_widget((row, col), channel)
            cell_config = make_cell_config(channel, (row, col), styler, openable)
            _ = widget.do_close(cell_config, gen_cell_id(table_id, config, row, col, simple))
            { state with cur_opened_cell = { some = coord } }
          end
        { set = state }
      else
        { unchanged }

    // Close
    | { Close = (_, id) } ->
       if config.only_one_cell_open then
        state =
          match state.cur_opened_cell with
          | { none } -> state
          | { some = (row, col) } ->
            id_opened = gen_cell_id(table_id, config, row, col, simple)
            if String.equals(id, id_opened) then
              { state with cur_opened_cell = { none } }
            else state
          end
        { set = state }
      else
        { unchanged }

    // Change
    | { Change = (_, _, _) } ->
      { unchanged }

    | { ChangePage=n} ->
      rec aux(i, p, acc)=
        if p == 1
        then i
        else if acc == 0
        then aux(i,p-1,state.display.row_page_size)
        else match BMap.get(i, state.row_map) with
          | {some=(_,{false},_)} -> aux(i+1,p,acc-1)
          | {some=_} -> aux(i+1, p, acc)
          | {none} -> i
      i = aux(0,n,state.display.row_page_size)

      display = state.display
      set_display({ display with top_row_index=i},false)
    | { GetSize = f } ->
      rec aux(i, n, t)=
        t = if i == top then n else t
        match BMap.get(i, state.row_map) with
          | {some=(_,{false},_)} -> aux(i+1, n+1, t)
          | {some=_} -> aux(i+1, n, t)
          | {none} -> (n,t)
      (n,t) = aux(0,0,0)
      do f(row_table_size, state.display_rows, n, state.display.row_page_size, t)
      { unchanged }
    | { SelectRow=_ } -> { unchanged } //@todo
    | { SelectRows=_ } -> { unchanged } //@todo
    | { SelectAll } -> { unchanged } //@todo
    | { SelectClear } -> { unchanged } //@todo
    | { SelectDo=_ } -> { unchanged } //@todo
    end

  /*
   * init the session, return the table object and the xhtml
   */
  @private @client
  create_private(simple : bool,
         config:CTable.config('a, 'state, 'row, 'col),
         id:string,
         callbacks : CTable.callbacks('a, 'state, 'row, 'col),
         initial_rows:list('row),
         initial_cols : list('col),
         initial_values:list(('row, 'col, 'a)),
         display:CTable.display):CTable.t('a, 'state, 'row, 'col) =
    // state initialisation
    row_map : BMap.intmap(('row, bool, bool)) = BMap.From.list_gen(initial_rows,(x -> (x,false,false)))
    col_map : BMap.intmap('col) = BMap.From.list_gen(initial_cols,(x -> x))
    display_bottom = display.top_row_index + min(List.length(initial_rows),display.row_header_size + display.row_page_size) - 1
    display_rows = display_bottom
    table_init_state : CTable.table_state('a, 'state, 'row, 'col) = {
      ~row_map ~col_map ~display ~display_bottom ~display_rows
      cur_opened_cell={none} filter=[]  sort={off}}

    rec val channel = Session.make(table_init_state, table_on_message(simple,id, config, callbacks,_,_,channel))


    show_displayed_cols(_rows, cols, display) =
      do for_iter(display.top_row_index, display_bottom)(i ->
        (row, _, _) = BMap.get(i, row_map) ? @fail("should not happen") //OK
        for_iter(0, (List.length(cols)-1))(j ->
          col = BMap.get(j, col_map) ? @fail("should not happen") //OK
           if (j >= display.left_col_index) && (j <= (display.left_col_index + display.col_page_size))
           then
             cell_id = gen_cell_id(id, config, row, col, simple)
             do widget_value(simple, id, config, (row,col), channel, {request=callbacks.request_value})
             dom_show_cell(0, #{cell_id},simple)
           else
             void


         )
      )
      for_iter(0, (List.length(cols)-1))(j ->
          col = BMap.get(j, col_map) ? @fail("should not happen") //OK
           if (j >= display.left_col_index) && (j <= (display.left_col_index + display.col_page_size))
           then
             cell_id =gen_col_header_id(id,config,col)
             dom_show_cell(0, #{cell_id},simple)
           else void
         )

    show_displayed_rows(rows, display) =
      for_iter(0, (List.length(rows)-1))(i ->
         if (i >= display.top_row_index) && (i <= display.top_row_index + display.row_page_size - 1)
          then
           (row, _, _) = BMap.get(i, row_map) ? @fail("should not happen") //OK
           row_id = gen_row_id(id, config, row)
           dom_show_row(0, #{row_id})
      )

    header_html=
      match config.headers with
        | {some=f} ->
          els = List.map(col -> gen_col_header(simple, id, config, channel, f, col) ,initial_cols)
          <tr>{els}</tr>
        | {none} -> <></>
      end

    body_html =
      List.map((row : 'row) ->
        cols : list(('col, 'a)) = List.filter_map(col : 'col ->
          match List.find((r, c, _) -> (r == row) && (c == col), initial_values) with
          | { none } -> { none }
          | { some = (_, _, v) } -> { some = (col, v) }
          end,
          initial_cols)
        gen_row_html(simple : bool, id : string, config : CTable.config('a, 'state, 'row, 'col), channel : CTable.t('a, 'state, 'row, 'col), row:'row, cols:list(('col, 'a)), initial_cols:list('col))
      , initial_rows)

    (table_prefix, table_style) = config.style.table

    table = <table id="{id}_table" class="{prefix_table(table_prefix)}" >
              <thead>{header_html}</thead>
              <tbody>{body_html}</tbody>
            </table> |> WStyler.add(table_style, _)

    do Dom.transform([#{id} <- table])
    do show_displayed_rows(initial_rows, display)
    do show_displayed_cols(initial_rows, initial_cols, display)
    do refresh_parity(id,Dom.has_class(myselect(id,".table_row_show:first"),"even"))
    channel

  @client
  create_simple(config:CTable.config('a, 'state, 'row, 'col),
         id:string,
         callbacks : CTable.callbacks('a, 'state, 'row, 'col),
         initial_rows:list('row),
         initial_cols : list('col),
         initial_values:list(('row, 'col, 'a)),
         initial_display:CTable.display):CTable.t('a, 'state, 'row, 'col) =
         create_private(true, config, id, callbacks, initial_rows, initial_cols, initial_values, initial_display)

  @client
  create(config:CTable.config('a, 'state, 'row, 'col),
         id:string,
         callbacks : CTable.callbacks('a, 'state, 'row, 'col),
         initial_rows:list('row),
         initial_cols : list('col),
         initial_values:list(('row, 'col, 'a)),
         initial_display:CTable.display):CTable.t('a, 'state, 'row, 'col) =
         create_private(false, config, id, callbacks, initial_rows, initial_cols, initial_values, initial_display)
  /*
   * send messages to the table session to work with the table values
   */
  @both
  send(table : CTable.t('a, 'state, 'row, 'col), message : CTable.msg('a, 'state, 'row, 'col)) =
    Session.send(table,@opensums(message))

  default_data_writer = {
    initial_state = { Init }
    on_cell_change = _,_,_ -> { NoChange }
  }


  default_table_error(err:CTable.error('a, 'state, 'row, 'col)) =
    match err with
      | { Index = (_pos, _msg) } ->  Log.debug("CTable", "index error")
      | { KeyRow = (_key, _msg) } -> Log.debug("CTable", "key row error")
      | { KeyCol = (_key, _msg) } -> Log.debug("CTable", "key col error")
    end

  default_overrun_up(_, _, _) = void

  default_overrun_down(_, _, _) = void

  default_overrun_left(_, _, _) = void

  default_overrun_right(_, _, _) = void

  default_resize_table(_, _, _, _, _, _) = void

  default_on_exists_open(id:string, coord:CTable.coordinates('row, 'col)) :void =
       _ = (id, coord) void

  default_on_forall_close(id:string):void = _ = id void

  default_callback(request_value : ('row, 'col -> option('a))) : CTable.callbacks('a, 'state, 'row, 'col) = {
      error = default_table_error
      data_writer = default_data_writer
      request_value = request_value
      overrun_up = default_overrun_up
      overrun_down = default_overrun_down
      overrun_left = default_overrun_left
      overrun_right = default_overrun_right
      resize_table = default_resize_table
  }

  default_initial_display(rows : int, cols : int) : CTable.display ={
    top_row_index = 1
    left_col_index = 0
    row_header_size = 1 // first row is fixed
    col_header_size = 0 // no fixed col.
    row_page_size = rows
    col_page_size = cols
  }

  default_style : CTable.style = {
      table=("ctable",WStyler.make_class(["ctable_table"]))
      td=("ctable",WStyler.make_class(["ctable_td"]))
  }

  default_config(row_to_string : ('row -> string),
                 col_to_string : ('col -> string),
                 cell_widget : CTable.coordinates('b, 'c), CTable.t('a, 'state, 'row, 'col) -> (CTable.Cell.widget('a), WStyler.styler,bool)) : CTable.config('a, 'state, 'row, 'col) = {
    cell_widget = cell_widget
    style = default_style
    row_to_string = row_to_string
    col_to_string = col_to_string
    headers = none
    only_one_cell_open = true
    max_number_of_lines_per_page = {none} : option(int)
    on_exists_open = default_on_exists_open
    on_forall_closed = default_on_forall_close
    prevent_empty_row_at_the_end = true // TODO put false when prevent_empty_row is fixed
    prevent_empty_col_at_the_end = true
  }


  generate_page(table : CTable.t('a, 'state, 'row, 'col), id : string)=
    rec refresh(nb, nb_shown, nb_nofilter, nb_per_page, top) =
      goto(i)(_) =
        do Session.send(table,{ChangePage=i})
        Session.send(table,{GetSize=refresh})
      scroll(i)(_) =
        do if i >= 0
           then CTable.send(table,{ScrollDown=i})
           else CTable.send(table,{ScrollUp=-i})
        Session.send(table,{GetSize=refresh})
      page_size(_)=
        match Parser.int(Dom.get_value(#{"page_size{id}"})) with
          | {~some} ->
            f(d)=
              Session.send(table,{SetDisplay={d with row_page_size=some}})
            Session.send(table,{GetDisplay=f})
          | {none} -> void
        end
      current_page =  Float.to_int(Math.floor(Float.of_int(top)/Float.of_int(nb_per_page)))+1
      nb_pages = Float.to_int(Math.floor(Float.of_int(nb_nofilter)/Float.of_int(nb_per_page)))+1
      pmin = max(current_page-5,1)
      pmax = min(current_page+5,nb_pages)
      l = List.init(i ->i+pmin ,pmax-pmin+1)
      l = List.map(i ->
        <>
          {if not(i == pmin) then " - " else ""}
          {if i == current_page
           then <>{i}</>
           else <a onclick={goto(i)}>{i}</a>}
        </>
        , l)
      l = if pmin == 1 then l else [<><a onclick={goto(1)}>{1}</a> ... </>|l]
      l = if pmax == nb_pages then l else List.append(l,[<> ... <a onclick={goto(nb_pages)}>{nb_pages}</a></>])
      xhtml =
        <div id={id}>
          <button onclick={scroll(0 - nb_per_page)}>Prev</button>
          <button onclick={scroll(nb_per_page)}>Next</button>
          <span>{top+1} - {min(top+nb_shown,nb)}</span>
          <span> / {nb_nofilter} (total : {nb}) </span>
          <span>{l}</span>
          <span><input id="page_size{id}" type=text onchange={page_size} value={nb_per_page}/></span>
        </div>
      Dom.transform([#{id} <- xhtml])
    <div id={id} onready={_ -> Session.send(table,{GetSize=refresh})} />
}}
