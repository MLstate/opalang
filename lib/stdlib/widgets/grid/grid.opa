/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * @category WIDGET
 * @author Adam Koprowski, 2011
 * @stability WIP
 */


import stdlib.widgets.core

// *********************************************************************************
/**
 * {1 Types}
**/
// *********************************************************************************

type WGrid.styling =
    {}
  / { css_props : Css.properties }
  / { styler : WStyler.styler; css_props : Css.properties }
  / { styler : WStyler.styler }

type WGrid.pos = { row : int; col : int }

type WGrid.size = { rows : int; cols : int }

type WGrid.cell('cell_content) =
    { occupied }
  / { cell : 'cell_content; size : WGrid.size }

type WGrid.t('cell_content) =
{
  size : WGrid.size
       // FIXME try with a better data-structure? two-dimensional array?
  cells : map(WGrid.pos, WGrid.cell('cell_content))
}

type WGrid.rendered_cell =
{
  xhtml : xhtml
  style : WGrid.styling
}

type WGrid.direction = {up} / {down} / {left} / {right}

type WGrid.edge = {top} / {bottom} / {left} / {right}

type WGrid.crop_type = WGrid.edge -> bool

// *********************************************************************************
/**
 * {1 Implementation}
**/
// *********************************************************************************

@both_implem WGrid = {{

  create(size : WGrid.size) : WGrid.t =
    { ~size cells=Map.empty }

  @private positions(at : WGrid.pos, size : WGrid.size) : list(WGrid.pos) =
    List.init(
      (row ->
        List.init(
          (col ->
            { row = at.row + row
              col = at.col + col
            }
          ),
          size.cols
        )
      ),
      size.rows
    ) |>
    List.flatten(_)

  @private apply_styling(xhtml : xhtml, styling : WGrid.styling) : xhtml =
    css_styler = WStyler.make_style
    style =
      match styling with
      | {} -> WStyler.empty
      | ~{css_props} -> css_styler(css_props)
      | ~{css_props styler} -> WStyler.merge([styler, css_styler(css_props)])
      | ~{styler} -> styler
    WStyler.add(style, xhtml)

  move(pos : WGrid.pos, dir : WGrid.direction) : WGrid.pos =
    match dir with
    | {up} -> {pos with row = pos.row - 1}
    | {down} -> {pos with row = pos.row + 1}
    | {left} -> {pos with col = pos.col - 1}
    | {right} -> {pos with col = pos.col + 1}

  pos_within_grid(pos : WGrid.pos, grid : WGrid.t) : bool =
    pos.row >= 0 && pos.col >= 0 && pos.row < grid.size.rows && pos.col < grid.size.cols

  cell_within_grid(pos : WGrid.pos, cell_size : WGrid.size, grid : WGrid.t) : bool =
    pos_within_grid({row = pos.row + cell_size.rows - 1
                    ;col = pos.col + cell_size.cols - 1
                    }, grid)

  put(grid : WGrid.t('cell_content), cell : 'cell_content, pos : WGrid.pos, size : WGrid.size) : option(WGrid.t('cell_content)) =
//    do Log.debug("Grid::put", "size={grid.size.rows}x{grid.size.cols}, pos={pos.row}x{pos.col}, size={size.rows}x{size.cols}")
    if size.cols <= 0 || size.rows <= 0 then // empty cell? no thank you
      none
    else if not(cell_within_grid(pos, size, grid)) then
      none
    else
      coords = positions(pos, size)
      make_occupied(pos, cells_opt) =
        match cells_opt with
        | {none} -> none
        | {some=cells} ->
            if Map.mem(pos, cells) then
              none
            else
              some(Map.add(pos, {occupied}, cells))
      match List.fold(make_occupied, coords, some(grid.cells)) with
      | {none} -> none
      | {some=cells} ->
          cells = Map.add(pos, ~{cell size}, cells)
          some({grid with ~cells})

  try_put_at(grid : WGrid.t('cell_content), cell : 'cell_content, pos : WGrid.pos, size : WGrid.size, slide : WGrid.direction) : option(WGrid.t('cell_content)) =
    if not(cell_within_grid(pos, size, grid)) then
      none
    else
      match put(grid, cell, pos, size) with
      | {some=new_grid} -> some(new_grid)
      | {none} -> try_put_at(grid, cell, move(pos, slide), size, slide)

  empty_crop : WGrid.crop_type = _dir -> false

  full_crop : WGrid.crop_type = _dir -> true

  crop_with(grid : WGrid.t('cell_content), crop_type : WGrid.crop_type) : WGrid.t('cell_content) =
    empty_grid = create({rows=0 cols=0})
    is_degenerated(grid) =
      grid.size.cols == 0 || grid.size.rows == 0
    rec perform_crop(grid, dir, pos_in_crop_region, size_update, pos_update) =
      single_crop(grid, f) =
        rebuild_grid(pos, cell, grid) = Map.add(pos_update(pos), cell, grid)
        new_grid =
          { size = size_update(grid.size)
          ; cells = Map.fold(rebuild_grid, grid.cells, Map.empty)
          }
        if (is_degenerated(new_grid)) then
          empty_grid
        else
          f(new_grid)
      if is_degenerated(grid) then
        empty_grid
      else if crop_type(dir) then
        check_crop_at(pos, _cell, can_crop) =
          can_crop && not(pos_in_crop_region(grid, pos))
        can_crop = Map.fold(check_crop_at, grid.cells, true)
        if can_crop then
          single_crop(grid,
            perform_crop(_, dir, pos_in_crop_region, size_update, pos_update))
        else
          grid
      else
        grid
    perform_crop(grid, {left},
      (_, pos -> pos.col == 0),
      (size -> {size with cols = size.cols-1}),
      (pos -> move(pos, {left}))
    ) |>
    perform_crop(_, {right},
      (grid, pos -> pos.col == grid.size.cols-1),
      (size -> {size with cols = size.cols-1}),
      (pos -> pos)
    ) |>
    perform_crop(_, {top},
      (_, pos -> pos.row == 0),
      (size -> {size with rows = size.rows-1}),
      (pos -> move(pos, {up}))
    ) |>
    perform_crop(_, {bottom},
      (grid, pos -> pos.row == grid.size.rows-1),
      (size -> {size with rows = size.rows-1}),
      (pos -> pos)
    )

  crop(grid : WGrid.t('cell_content)) : WGrid.t('cell_content) =
    crop_with(grid, full_crop)

  fill(grid : WGrid.t('cell_content), gen_cell : WGrid.pos -> 'cell_content) : WGrid.t('cell_content) =
    empty_grid = create(grid.size)
    coords = positions({row=0 col=0}, grid.size)
    fill_cell(pos, grid) =
      put(grid, gen_cell(pos), pos, {rows=1 cols=1}) ? error("[grid::fill] internal error")
    List.fold(fill_cell, coords, empty_grid)

  render(grid : WGrid.t('cell_content), render : 'cell_content -> WGrid.rendered_cell, empty_cell_style : WGrid.styling) : xhtml =
    render_cell(row)(acc, col) =
      this =
        match Map.get(~{row col}, grid.cells) with
        | {none} -> <td/> |> apply_styling(_, empty_cell_style)
        | {some={occupied}} -> <></>
        | {some=~{cell size}} ->
            rcell = render(cell)
            content = rcell.xhtml
            xhtml =
              match (size.rows, size.cols) with
              | (1, 1) -> <td>{content}</>
              | (1, j) -> <td colspan={j}>{content}</>
              | (i, 1) -> <td rowspan={i}>{content}</>
              | (i, j) -> <td rowspan={i} colspan={j}>{content}</>
            apply_styling(xhtml, rcell.style)
      <>{acc}{this}</>
    render_row(acc, row) =
      <>
        {acc}
        <tr>
          {Int.fold(render_cell(row), <></>, grid.size.cols)}
        </>
      </>;
    <table>
      {Int.fold(render_row, <></>, grid.size.rows)}
    </>

  to_debug_string(grid) =
    List.init(
      (row ->
        List.init(
          (col ->
            match Map.get(~{row col}, grid.cells) with
            | {none} -> " "
            | _ -> "X"
          ),
          grid.size.cols
        ) |> List.to_string_using("[", "]", "", _)
      ),
      grid.size.rows
    ) |> List.to_string_using("", "", " ", _)

}}
