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
 * @category Components
 * @author Hugo Heuzard, 2010-2011
 */

import stdlib.widgets.core
import stdlib.widgets.select
import stdlib.components.table
import stdlib.interactions.editable


type CSimpleTable.Cell.widget('a) = {
  html : string, IEditable.content('a) -> list(xhtml)
  set_value : string, 'a -> void
}

type CSimpleTable.callbacks('a, 'state, 'row) = {
  data_writer : CSimpleTable.data_writer('a, 'state, 'row)
  request_value : 'row -> option('a)
  overrun_up :    CSimpleTable.t('a, 'state, 'row), int, int -> void
  overrun_down :  CSimpleTable.t('a, 'state, 'row), int, int -> void
  resize_table :  CSimpleTable.t('a, 'state, 'row), CSimpleTable.display, int, int -> void
}

type CSimpleTable.msg('a, 'state, 'row) =
    { AddRow : ('row, option('a)) }
  / { AddRows : list(('row, option('a))) }
  / { InsertRow : (int, 'row, 'a) }
  / { DeleteRow : 'row }
  / { DeleteRows : list('row) }
  / { ClearRow : 'row }
  / { SetRow : ('row, 'a) }
//
  / { GetDisplay : CSimpleTable.display -> void }
  / { SetDisplay : CSimpleTable.display }
  / { Refresh }
//
  / { ScrollDown : int }
  / { ScrollUp : int }
//
  / { SortRows : (('row, 'row -> Order.comparison),option(string)) }
//
  / { SetRowFilter : ('row -> bool,string) }
  / { UpdateRowFilter : ('row -> bool,string) }
  / { ClearRowFilter }
//
  / { Terminate }

type CSimpleTable.style = {
  table : (string, WStyler.styler)
  td : (string, WStyler.styler)
}

type CSimpleTable.header_sort('row) = ('row, 'row -> Order.comparison)

type CSimpleTable.header_filter_search('row) = (string, 'row -> bool)

type CSimpleTable.header_filter_select('row) = list((string,('row -> bool)))

type CSimpleTable.header_filter('row) =
  {search:CSimpleTable.header_filter_search('row)}
/ {select:CSimpleTable.header_filter_select('row)}

type CSimpleTable.header('row) =
  {title:string}
/ {title:string
   sort:option(CSimpleTable.header_sort('row))
   filter:option(CSimpleTable.header_filter('row))}

type CSimpleTable.config('a, 'state, 'row) = {
  cell_widget : 'row, CSimpleTable.t('a, 'state, 'row) -> (CSimpleTable.Cell.widget('a), WStyler.styler, bool)
  style : CSimpleTable.style
  row_to_string : 'row -> string
  headers : list(CSimpleTable.header('row))
  only_one_cell_open : bool
  max_number_of_lines_per_page : option(int)
  on_exists_open : string, 'row -> void
  on_forall_closed : string -> void
  prevent_empty_row_at_the_end : bool
}

type CSimpleTable.data_writer('a, 'state, 'row) = {
  initial_state : 'state
  on_cell_change : 'state, 'row, 'a -> CSimpleTable.update('state)
}

type CSimpleTable.update('state) = CTable.update('state)

@abstract
type CSimpleTable.t('a, 'state, 'row) = CTable.t('a, 'state, 'row, CSimpleTable.col)

type CSimpleTable.display = {
  top_row_index : int
  row_page_size : int
}


@private
type CSimpleTable.col = {onecol}

CSimpleTable = {{

  @private
  onecol : CSimpleTable.col = {onecol}

  @client
  create(config:CSimpleTable.config('a, 'state, 'row),
         id:string,
         callbacks : CSimpleTable.callbacks('a, 'state, 'row),
         initial_rows:list('row),
         initial_values:list(('row,'a)),
         initial_display:CSimpleTable.display):CSimpleTable.t('a, 'state, 'row) =
    cellwidget((row,_),t) =
      (cell,style,b) = config.cell_widget(row,t)
      cell = {
        html = (_, id,e -> match e with
          | {value=_s} ->
             h = cell.html(id,e)
             do Dom.add_class(#{id},"valid")
             <>{List.map(s -> <td>{s}</td>,h)}</>
          | _ -> <>{List.map(s -> <td>{s}</td>,cell.html(id,e))}</>
        )
        do_open = (_,_ -> void)
        do_close = (_,_ -> void)
        parse = (_,id -> if Dom.has_class(Dom.select_id(id),"valid")
                         then {invalid_value=none}
                         else {missing_value});
        set_invalid_value = (_,_,_ -> void);
        clear_value = (_,id ->
          ignore(Dom.remove_class(Dom.select_id(id), "valid")));
        set_value = (_,id,e ->
          do Dom.add_class(#{id},"valid")
          cell.set_value(id,e))
      }
      (cell,style,b)
    gen_header_custom(channel)(kind)= match kind with
      | ~{title} -> <th style="diplay:none">{title}</th>
      | ~{title sort filter} ->
        id = Dom.fresh_id()
        sort_html = match sort with
          | {some=f} -> <a onclick={_ -> CSimpleTable.send(channel,{SortRows=(f,some(id))})}><span class="icon icon-triangle-n"></span></a>
          | {none} -> <></>
        end
        filter_html = match filter with
          | {some={~select}} ->
            on_select(s) =
              _ = match List.assoc(s, select) with
                | {~some} -> CSimpleTable.send(channel,{UpdateRowFilter=(some,id)})
                | {none} -> CSimpleTable.send(channel,{UpdateRowFilter=(_ -> false,id)})
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
                CSimpleTable.send(channel,{UpdateRowFilter=(_ -> false,id)})
              else
                CSimpleTable.send(channel,{UpdateRowFilter=(search(s,_),id)})
            <>
              {title}
              <input style="position:absolute" id={id} type="text" onchange={f} />
            </>
          | {none} -> <>{title}</>
        end
      <th>{sort_html} {filter_html}</th>
    end
    headers = if List.is_empty(config.headers)
              then none
              else some((_ -> {custom=(channel -> <>{List.map(gen_header_custom(channel),config.headers)}</>)}))
    col_to_string({onecol}) = "onecol"
    config : CTable.config('a, 'state, 'row, CSimpleTable.col) =
    { CTable.default_config(config.row_to_string, col_to_string, cellwidget) with
      style = config.style
      headers = headers
      only_one_cell_open = config.only_one_cell_open
      max_number_of_lines_per_page = config.max_number_of_lines_per_page
      on_exists_open = (s,(r,_) -> config.on_exists_open(s,r))
      on_forall_closed = config.on_forall_closed
      prevent_empty_row_at_the_end = config.prevent_empty_row_at_the_end
    }
    callbacks= {
      error = _ -> void
      data_writer = {
        initial_state=callbacks.data_writer.initial_state
        on_cell_change=s,(r,_),a -> callbacks.data_writer.on_cell_change(s,r,a)
      }
      request_value = row,_ -> callbacks.request_value(row)
      overrun_up = callbacks.overrun_up
      overrun_down = callbacks.overrun_down
      overrun_left = _,_,_ -> void
      overrun_right = _,_,_ -> void
      resize_table = t,d,a,b,_,_ ->
        d = {top_row_index=d.top_row_index row_page_size=d.row_page_size}
        callbacks.resize_table(t,d,a,b)
    }
    initial_rows=initial_rows
    initial_cols= [onecol]
    initial_values= List.map((r,v) -> (r,onecol,v),initial_values)
    initial_display= {
      top_row_index = initial_display.top_row_index
      left_col_index = 0
      row_header_size = 0
      col_header_size = 0
      row_page_size = initial_display.row_page_size
      col_page_size = 1
    }
    CTable.create_simple(config, id, callbacks, initial_rows, initial_cols, initial_values, initial_display)

  send(table : CSimpleTable.t('a, 'state, 'row), message : CSimpleTable.msg('a, 'state, 'row)) =
    m : CTable.msg('a, 'state, 'row, CSimpleTable.col) = match message with
        { AddRow=(row,v) } ->
          l = match v with
            | {none} -> []
            | {~some} -> [(onecol,some)]
          end
          {AddRow=(row, l)}
      | { AddRows=l} ->
        { AddRows=List.map((row,v) ->
          nl = match v with
            | {none} -> []
            | {~some} -> [(onecol,some)]
          end
          (row,nl),l)}
      | { InsertRow=(pos,row,v)} ->
        { InsertRow=(pos,row,[(onecol,v)])}
      | { ~DeleteRow } -> { ~DeleteRow }
      | { ~DeleteRows} -> { ~DeleteRows}
      | { ClearRow=row} -> {ClearRow=(row,[onecol])}
      | { SetRow=(row,v)} -> { SetRow=(row,[(onecol,v)])}
      | { GetDisplay=f} ->
        g(d)=
          f({
            top_row_index = d.top_row_index
            row_page_size = d.row_page_size
          })
        { GetDisplay=g}
      | { SetDisplay=d} ->
        d = {
          top_row_index = d.top_row_index
          row_page_size = d.row_page_size
          left_col_index = 0
          row_header_size = 0 // first row is fixed
          col_header_size = 0 // no fixed col.
          col_page_size = 1 }
        { SetDisplay = d}
      | { Refresh } -> { Refresh }
      | { ~ScrollDown} -> {~ScrollDown}
      | { ~ScrollUp} -> {~ScrollUp}
      | { SortRows=(sort,c)} -> { SortRows=(sort,c)}
      | { ~SetRowFilter } -> { ~SetRowFilter }
      | { ~UpdateRowFilter } -> { ~UpdateRowFilter }
      | { ClearRowFilter } -> { ClearRowFilter }
      | { Terminate } -> { Terminate }
    CTable.send(table,m)

  default_config(row_to_string : 'row -> string,
                cell_widget : 'row, CSimpleTable.t('a, 'state, 'row) -> (CSimpleTable.Cell.widget('a), WStyler.styler, bool)
                ) : CSimpleTable.config('a, 'state, 'row) = {
    ~cell_widget
    style=CTable.default_style
    ~row_to_string
    headers = []
    only_one_cell_open = false
    max_number_of_lines_per_page = none
    on_exists_open = (_,_ -> void)
    on_forall_closed = (_ -> void)
    prevent_empty_row_at_the_end = true
  }

  default_data_writer : CSimpleTable.data_writer('a, 'state, 'row) = {
    initial_state = { Init }
    on_cell_change = (_,_,_ -> { NoChange })
  }

  default_callbacks(request_value:('row -> option('a)))  : CSimpleTable.callbacks('a, 'state, 'row) = {
    data_writer=default_data_writer
    ~request_value
    overrun_up = (_,_,_ -> void)
    overrun_down = (_,_,_ -> void)
    resize_table = (_,_,_,_ -> void)
  }
  default_display(i : int) : CSimpleTable.display = {
    top_row_index=0
    row_page_size=i
  }

  generate_page(table : CSimpleTable.t('a,'b,'c), id : string)=
    CTable.generate_page(table <: CTable.t('a,'b,'c,CSimpleTable.col),id)
}}
