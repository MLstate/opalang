/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A cloud widget to display weighted items.
 *
 * @category WIDGET
 * @author Guillem Rieu, 2010
 * @destination PUBLIC
 * @stability TRANSITIONAL
 */

import stdlib.web.client //type position
import stdlib.widgets.core

// TODO: unify list and greedy layouts orders
// TODO: document customization
// FIXME: different browser behaviour for greedy placement (Opera

/**
 * {1 About this module}
 *
 * This module renders a collection of weighted items into a cloud.
 * An item is usually a simple text tag, but could very well be any
 * XHTML chunk. You can choose between multiple cloud layouts (determining
 * the placement of items), the most common one being a list (which can be
 * sorted with an order).
 *
 * {1 Where should I start?}
 *
 * The most basic usage (with default configuration) produces a cloud
 * made of unsorted text tags. These tags appear aligned, with font sizes
 * and colors (actually red variations) depending on their weight.
 *
 * It can be obtained with the following code:
 *
 * {[
 * // These sample tags are of the type ((link_name, link_href), weight)
 * // Weights are arbitrary floats related to each other (higher is heavier)
 * tags = [
 *   (("web", "#"), 15.),
 *   (("opa", "#"), 6.),
 *   (("mlstate", "#"), 8.),
 *   ...
 * ]
 * ]}
 *
 * WCloud.html(WCloud.default_config, none, "wcloud_display", tags)
 *  |> WStyler.make("wcloud_display", _)
 *
 * This tag cloud only displays information, but no user action is possible.
 * If you want to catch onclick events occuring on a tag, you can use a callback
 * function this way:
 *
 * onclick_wcloud(((name, href), weight)) =
 *   jlog("onclick_wcloud(({name}, {href}), {weight})")
 *
 * WCloud.html(WCloud.default_config, some(onclick_wcloud), "wcloud_edit", tags)
 *  |> WStyler.make("wcloud_edit", _)
 *
 * In this example, tag elements are assumed to be tuples (name, href) but
 * they can be of any kind.
 *
 */


/**
 * {1 Types defined in this module}
 */

/**
 * A cloud item is a tuple associating an item to its weight
 */
type WCloud.item('item) = ('item, float)

/**
 * Cloud layouts are ways to display items in the cloud
 */
type WCloud.layout('item, 'label) =
    { list: order(WCloud.item('item), 'label) }
  / { greedy: order((WCloud.item('item), position), 'label) } /** WARNING: experimental */
  / { random } /** WARNING: experimental */
  / { custom: WCloud.item('item) -> WStyler.styler }

/**
 * Cloud settings that can be modified
 */
type WCloud.config('item, 'label) = {
    /** The layout to be used to place items and draw the cloud */
    layout: WCloud.layout('item, 'label)
    /** Fixed dimensions (some(width), some(height)) of the cloud. One or both
     * dimensions can be set to [none] in order to make it expandable in this
     * direction. */
    dimensions: (option(int), option(int))

    /** Give the style corresponding to an item */
    get_style: WCloud.item('item) -> WStyler.styler
    /** Give the XHTML representing an item */
    get_xhtml: WCloud.item('item) -> xhtml

    /** Style of the DIV surrounding tags */
    cloud_style: WStyler.styler
    /** Styles (normal and mouse over) of the LI elements of the cloud */
    item_styles: (WStyler.styler, WStyler.styler)
}

type WCloud.box = {left: int top: int width: int height: int}

do Random.random_init()

WCloud = {{
/**
 * {1 Configuration}
 */

  /**
   * Build a style corresponding to a weight from a list of styles
   * associated to intervals.
   *
   * @param styles A list of tuples associating a weight interval to a style
   *               in the form (low_boundary, high_boundary, css_style)
   * @param weight A weight between 0. and 1.
   * @return The style corresponding to the interval in which resides the given
   *         weight
   */
  style_of_weight(styles: list((float, float, Css.properties)), weight)
      : Css.properties =
    List.fold(((low: float, high: float, stl), acc ->
        if weight >= low && weight <= high then acc ++ stl else acc),
      styles, [])

  /**
   * Helper function to build a list of css colors associated to
   * weight intervals.
   *
   *
   * @param stl A function taking an int between 0 and 255 and returning
   *            a css style. For example:
   *            red_stl(step) = css {color: rgb(step, 0, 0)}
   * @param ncolors Number of different colors wanted in the pallet
   * @result A list of tuples assocating a weight interval to a style
   *         in the form (low_boundary, high_boundary, css_style)
   */
  pallet(stl: int -> Css.properties, ncolors: int)
    : list((float, float, Css.properties)) =
    weight_step = 1. / Float.of_int(ncolors)
    color_step = 256 / ncolors
    Int.fold((acc, i ->
        low = weight_step * Float.of_int(i)
        (low, low + weight_step, stl(color_step * i)) +> acc),
      [], ncolors)

  /**
   * A simple list layout (sort order is the original one)
   */
  unsorted_list_layout = {list=@nonexpansive(unsorted_order(false))}

  /**
   * A simple alphabetically sorted list layout (case insensitive)
   */
  alphabetical_list_layout = {list=@nonexpansive(alphabetical_order(false))}

  /**
   * A simple weight sorted list layout
   */
  weight_list_layout = {list=@nonexpansive(weight_order(false))}

  /**
   * A simple random list layout
   */
  random_list_layout = {list=random_order}

  /**
   * A compact layout using a greedy algorithm on item sizes
   */
  size_greedy_layout = {greedy=@nonexpansive(size_order(false))}

  /**
   * A random placement layout
   */
  random_layout = {random}


  /**
   * Default config produces a bare text tag cloud displayed as a list.
   */
  default_config : WCloud.config(_, void) = {

    layout = unsorted_list_layout
    dimensions = ({some=300}, none)

    /**
     * A default style with the font size and color depending on the
     * weight of the given item.
     */
    get_style = (_, weight: float) ->
      size = weight + 0.5 // Shift the font size interval by 0.5 points
      default_stl = (0., 1., css {
          font-size: {size * 125.}%;
          font-decoration: normal;
        })

      red_stl(step) = css {color: rgb(step, 0, 0)}
      style_of_weight(default_stl +> pallet(red_stl, 10), weight)
        |> WStyler.make_style

    /**
     * A default cloud element (a simple text tag of the form (name, href))
     */
    get_xhtml =
      ((name: string, _href: string), _) -> <>{name}</>

    /**
     * A default cloud style
     */
    cloud_style = WStyler.make_style(css {
        border: thin solid gray;
        padding: 5px;
        list-style: {{style=none style_position=none}};
        text-align: justify;
      })

    /**
     * Default item styles pair (normal, on_mouse_over)
     */
    item_styles = (
        WStyler.make_style(css {background: transparent;}),
        WStyler.make_style(css {background: #E6E6E6;})
      )
  }

/**
 * {1 High-level interface}
 */

  /**
   * Main display function of the tagcloud. If the optional callback function
   * is given, the items will be clickable.
   *
   * @param config The widget configuration
   * @param handles_opt Optional event handles to bind to tags
   * @param id The main ID of the widget (it should be the same as the one
   *           given to the wrapper created with [WCommon.make])
   * @return The cloud XHTML
   */
  html(config: WCloud.config('item, 'label),
      handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
      id: string, items: list(WCloud.item('item)))
      : xhtml =
    get_cloud(config, id, handles_opt, items)

/**
 * {1 Private functions}
 */

  /* IDs */
  @private
  get_list_id(id: string): string = id ^ "_list"

  @private
  get_item_id(id: string, index: int): string =
    id ^ "_item_" ^ String.of_int(index)


  /*
   * {2 Default orders on items}
   */

  /*
   * Identity order on items (no modification of the original elements order)
   */
  @private
  unsorted_order(reverse: bool): order(WCloud.item('item), 'label) =
    sign = if reverse then {lt} else {gt}//TODO: Order.reverse ?
    Order.make(_, _ -> sign)

  /*
   * An alphabetical order on default text tags (case insensitive)
   */
  @private
  alphabetical_order(reverse: bool): order(WCloud.item('item), 'label)  =
    default = Order.make(((i1, _), _), ((i2, _), _) -> Order.compare(i1, i2, String.order_ci))
    if reverse then Order.reverse(default) else default

  /*
   * A order for comparing two items weights
   */
  @private
  weight_order(reverse: bool): order(WCloud.item('item), 'label)  =
    default = Order.make((_, w1), (_, w2) -> Order.compare(w1, w2, Float.order))
    if reverse then Order.reverse(default) else default

  /*
   * A random order on items
   * <!> This is not an order
   */
  @private
  random_order: order(WCloud.item('item), 'label)  =
    @nonexpansive(Order.make(_, _ ->
      match Random.int(2) with
      | 0 -> {eq}
      | 1 -> {lt}
      | _ -> {gt}
    ))
    //TODO: check if this is useful

  /*
   * An order for comparing two item sizes (actually their areas)
   */
  @private
  size_order(reverse: bool): order((WCloud.item('item), position), 'label)  =
    default = Order.make((_, {x=w1 y=h1}), (_, {x=w2 y=h2}) ->
        Order.compare(w1 * h1, w2 * h2, Int.order))
    if reverse then Order.reverse(default) else default

  /*
   * Normalize item weights between 0. and 1.
   */
  @private
  normalize_weight(items: list(WCloud.item('item)))
      : list(WCloud.item('item)) =
    (wmin:float, wmax:float) =
      List.fold((_, w), (acc_min, acc_max) ->
          (min(acc_min, w), max(acc_max, w)),
        items, (0., 0.))

    List.map((i, w:float) -> (i, (w - wmin) / (wmax - wmin)), items)

  /*
   * Sort items with the given order.
   */
  @private
  sort_items = List.sort_with_order


  /*
   * {2 Layout building intermediate functions}
   */

  /*
   * Test whether or not a given position is inside a rectangle.
   */
  @private
  is_inside(~{x y}: position, ~{left top width height}: WCloud.box): bool =
    d1 = x - left
    d2 = y - top
    d1 >= 0 && d1 <= width && d2 >= 0 && d2 <= height

  /*
   * Test whether or not two rectangles intersect.
   */
  @private
  intersect(box1: WCloud.box, box2: WCloud.box): bool =
    is_inside({x=box2.left y=box2.top}, box1) ||
    is_inside({x=box2.left+box2.width y=box2.top}, box1) ||
    is_inside({x=box2.left y=box2.top+box2.height}, box1) ||
    is_inside({x=box2.left+box2.width y=box2.top+box2.height}, box1) ||
    is_inside({x=box1.left y=box1.top}, box2) ||
    is_inside({x=box1.left+box1.width y=box1.top}, box2) ||
    is_inside({x=box1.left y=box1.top+box1.height}, box2) ||
    is_inside({x=box1.left+box1.width y=box1.top+box1.height}, box2)


  /*
   * Build the XHTML corresponding to an item of the cloud.
   * The [pos_opt] parameter is only used when a particular layout is wanted.
   */
  @private
  get_item(config: WCloud.config, item_id: string,
      handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
      item: WCloud.item('item), pos_opt: option(WStyler.styler))
      : xhtml =
    Option.switch(pos ->
      common_stl = WStyler.make_style(css {
          display: inline;
          margin: 0px;
        })
      default_stl =
        WStyler.merge([common_stl, pos, config.item_styles.f1])
      mouseover_stl = Option.switch(_ ->
          WStyler.merge([common_stl, pos, config.item_styles.f2]),
          default_stl, handles_opt)
      item_xhtml = config.get_xhtml(item)
      // TODO : Test [add_binds]
      item_content = Option.switch(handles ->
          closed_handles = List.map((evt, hnd) -> (evt, hnd(item)), handles)
          <a style="text-decoration: none;">
            {item_xhtml}
          </a>
            |> WCore.add_binds(closed_handles, _),
          <span>{item_xhtml}</span>,
          handles_opt)
        |> WStyler.set(config.get_style(item), _)
      item_container =
        <li id={item_id}
          onmousemove={_ -> WStyler.set_dom(mouseover_stl, item_id)}
          onmouseout={_ -> WStyler.set_dom(default_stl, item_id)}>
          {item_content}
        </li>
      WStyler.set(default_stl, item_container),
      <></>, pos_opt)

  /*
   * {2 Layout building main functions}
   */

  /*
   * Build the XHTML corresponding to a list of items. The list is sorted
   * using the given order.
   */
  @private
  make_list_layout(config: WCloud.config, id: string,
      handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
      ord: order(WCloud.item('item), 'label), items: list(WCloud.item('item)))
      : (xhtml, ((string, position) -> option(position))) =
    ordered_items = sort_items(ord, items)
    items_xhtml = List.foldi((i, elt, acc ->
      acc <+> get_item(config, id ^ "_item_" ^ String.of_int(i),
          handles_opt, elt, some(WStyler.empty))), ordered_items, <></>)
    (items_xhtml, (_ -> none))


  /*
   * Build the XHTML of a cloud with items layed out following a greedy
   * algorithm aimed at minimizing the used space.
   *
   * WARNING: Use with care! This layout needs more work and is not robust
   * enough in the current state. Most notable problems are:
   * - different browser behaviours in the placement of items.
   * - the widget doesn't render the layout if it happens to be created in
   * an 'onready' event, this event colliding with the necessary redrawing
   * function of the layout (itself triggered within an onready event...)
   */
  @private
  make_greedy_layout(config: WCloud.config, id: string,
      handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
      ord: order((WCloud.item('item), position), 'label),
      items: list(WCloud.item('item)))
      : (xhtml, ((string, position) -> option(position))) =

    /* Raise an integer by a given factor.
     * (used to add more space between items) */
    edge_offset(val: int, fact: float): int =
      Float.of_int(val) * fact |> Int.of_float(_) |> Int.add(val, _)

    /* Retrieve item sizes and store them in an associative list of pairs
     * (item, item_size) */
    get_item_sizes(items: list(WCloud.item('item)))
        : list((WCloud.item('item), position)) =
      List.mapi((i, item ->
          item_id = get_item_id(id, i)
          (item, {x = Dom.get_width(#{item_id}) y=Dom.get_height(#{item_id})})),
        items)

    /* Compare the areas of two items (associated to their sizes) */
    _compare_item_sizes(
        (_, {x=w1 y=h1}: position),
        (_, {x=w2 y=h2}: position))
        : Order.comparison =
      compare_int(w1 * h1, w2 * h2)

    /* Compare the positions of two items in the cloud */
    compare_position(~{x=x1 y=y1}: position, ~{x=x2 y=y2}: position): Order.ordering =
      if x1 == x2 && y1 == y2 then {eq}
      else if y1 <= y2 then Int.ordering(y1, y2)
      else Int.ordering(x1, x2)

    /* Expand the area delimited by boundaries to make an item of size [size]
     * fit and return a tuple (new boundaries, list of new places, position of
     * size) */
    expand_x(max_pos: position, boundaries: position, size: position)
        : option((position, position, position)) =
      new_max_pos = {x=(max_pos.x + size.x + 1) y=max(max_pos.y, size.y)}
      new_boundaries =
          {x=max(boundaries.x, new_max_pos.x) y=max(boundaries.y, new_max_pos.y)}
      some((new_max_pos, new_boundaries, {x=(max_pos.x + 1) y=0}))

    /* Expand the area delimited by boundaries to make [size] fit
     * and return a tuple (new boundaries, list of new places, position of size) */
    expand_y(max_pos: position, boundaries: position, size: position)
        : option((position, position, position)) =
      new_max_pos = {x=max(max_pos.x, size.x) y=(max_pos.y + size.y + 1)}
      new_boundaries =
          {x=max(boundaries.x, new_max_pos.x) y=max(boundaries.y, new_max_pos.y)}
      some((new_max_pos, new_boundaries, {x=0 y=(max_pos.y + 1)}))

    /* Expand the cloud area either horizontally or vertically, depending on
     * the current width / height ratio */
    expand_xy(max_pos: position, boundaries: position, size: position)
        : option((position, position, position)) =
      // The 4/3 ratio is arbitrary (it determines the global shape of the
      // virtual rectangle wrapping the cloud.
      if (boundaries.x / boundaries.y) < (4/3) then
        expand_x(max_pos, boundaries, size)
      else
        expand_y(max_pos, boundaries, size)

    /* Discard less important items if there is no space left in the cloud,
     * instead of expanding it */
    discard_items(_, _, _) = none


    /* Insert a new "place" to the place list. Here, places are points of the
     * cloud area where an item can be insterted. They are sorted using
     * the [compare_position] function. */
    insert_place(new_place: position, places: list(position)): list(position) =
      rec aux(head, rest) = match rest with
        | [] -> List.rev(new_place +> head)
        | ~{hd tl} ->
          match compare_position(new_place, hd) with
            | {eq} -> List.rev(head) ++ rest
            | {gt} -> List.rev(head) ++ (new_place +> rest)
            | {lt} -> aux(hd +> head, tl)
          end
      aux([], places)

    /* Merge a unsorted list of places into a sorted one.  */
    fold_places(new_places: list(position), places: list(position))
        : list(position) =
      List.fold(insert_place, new_places, places)

    /* Return places corresponding to a cloud element. They are top-right
     * and bottom-left corners of the outer rectangle enclosing an item:
     *
     *                     +----------+ <- first place
     *                     |  item    |
     *     second place -> +----------+
     */
    get_places(origin: position, elt_size: position): list(position) = [
        {x=(origin.x + edge_offset(elt_size.x, 0.1)) y=origin.y},
        {x=origin.x y=(origin.y + edge_offset(elt_size.y, 0.5))},
      ]

    /* Search known places for the best free position to place an element of
     * given size, possibly expanding the cloud area if needed */
    best_place(expand_fun: (position, position, position -> option((position, position, position))),
        boxes, places: list(position), max_pos: position,
        boundaries: position, elt_size: position)
        : option((list(WCloud.box), list(position), position, position, position)) =
      area_box = {left=0 top=0 width=boundaries.x height=boundaries.y}
      rec aux(remaining_places, unproper_places) = match remaining_places with
        | [] ->
          // No proper place found, trying to expand the cloud area
          expand_opt = expand_fun(max_pos, boundaries, elt_size)
          Option.switch(((new_max_pos, new_boundaries, pos) ->
              new_box = {left=pos.x top=pos.y width=elt_size.x height=elt_size.y}
              new_places = get_places(pos, elt_size)
              some((new_box +> boxes,
                  fold_places(new_places, List.rev(unproper_places)),
                  new_max_pos, new_boundaries, pos))),
            none, expand_opt)
        | ~{hd tl} ->
          // Trying to place the element at the next place in the list
          new_box =
            {left=(hd.x + 1) top=(hd.y + 1) width=elt_size.x height=elt_size.y}
          if not(is_inside({
                x=(elt_size.x + hd.x + 1)
                y=(elt_size.y + hd.y + 1)
              }, area_box)) || List.exists(intersect(_, new_box), boxes) then
            // The element doesn't fit at this place, trying the next places
            aux(tl, hd +> unproper_places)
          else
            // We have found a proper place to set the element
            new_max_pos = {
                x=max(max_pos.x, (hd.x + elt_size.x + 1))
                y=max(max_pos.y, (hd.y + elt_size.y + 1))
              }
            new_places = get_places(hd, elt_size)
            some((new_box +> boxes,
                fold_places(new_places, List.rev(unproper_places) ++ tl),
                new_max_pos, boundaries, {x=(hd.x + 1) y=(hd.y + 1)}))
      aux(places, [])

    /* Build a layout with greedy placement of items: next element (in the
     * given order) in the list is placed at the first fitting place, or at the
     * end of the cloud if no place is found elsewhere. */
    greedy_positions(container_size: position, items: list(WCloud.item('item)))
        : (option(position), xhtml) =
      sorted_items = get_item_sizes(items)
        |> sort_items(ord, _) |> List.rev(_)
      // Selecting expand function to use depending on dimensions set in config
      expand_fun = match config.dimensions with
        | ({none}, {none}) -> expand_xy // Expand vertically and horizontally
        | ({none}, _) -> expand_x // Expand horizontally
        | (_, {none}) -> expand_y // Expand vertically
        | (_, _) -> discard_items // Don't expand, discard items instead
      get_place(i, (item, size), (boxes, places, max_pos, boundaries, accu)) =
        // Retrieving the next fitting place given a new item and a cloud state
        item_id = get_item_id(id, i)
        build_result((new_boxes, new_places, new_max_pos, new_boundaries, pos)) =
          // Building a new cloud state with a newly taken position
          pos_stl_opt = WStyler.make_style(css {
              margin: 0px;
              padding: 0px;
              position: absolute;
              left: {pos.x}px;
              top: {pos.y}px;
            }) |> some(_)
          (new_boxes, new_places, new_max_pos, new_boundaries,
              accu <+> get_item(config, item_id, handles_opt, item, pos_stl_opt))
        best_place_opt =
          best_place(expand_fun, boxes, places, max_pos, boundaries, size)
        Option.switch(build_result,
            (boxes, places, max_pos, boundaries, accu), best_place_opt)
      // Building the cloud itself, using preceding functions
      (_, _, final_size, _, result) =
        List.foldi(get_place, sorted_items,
            ([], [{x=0 y=0}], {x=0 y=0}, container_size, <></>))
      // Returning a tuple (new cloud size, XHTML of the cloud)
      (some(final_size), result)

    // TODO: factorize the draw function with the one of [random_layout]
    draw(container_size_opt: option(position), first_draw: bool)
        : (option(position), xhtml) =
      if first_draw then
        // Drawing a simple list layout while we don't know element sizes
        (none,
         make_list_layout(config, id, handles_opt, unsorted_order(false), items).f1)
      else
        // Redraw the real layout knowing element sizes (since the DOM has
        // already been displayed once)
        container_size = container_size_opt ? {x=300 y=150}
        greedy_positions(container_size, items)
    // Returning a tuple of the initial cloud content and the redraw function
    (draw(none, true).f2,
        ((_, container_size) ->
            (new_size, result) = draw(some(container_size), false)
            do Dom.transform([#{get_list_id(id)} <- result])
            new_size))

  /*
   * Build the XHTML of a cloud with items randomly placed in the cloud area.
   * If no free random place is found after 10 tries for an item, this item
   * is considered not fitting in the current area and is discarded.
   *
   * WARNING: Use with care! This layout needs more work and is not robust
   * enough in the current state. Most notable problems are:
   * - different browser behaviours in the placement of items.
   * - the widget doesn't render the layout if it happens to be created in
   * an 'onready' event, this event colliding with the necessary redrawing
   * function of the layout (itself triggered within an onready event...)
   */
  @private
  make_random_layout(config: WCloud.config, id: string,
    handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
    items: list(WCloud.item('item)))
    : (xhtml, ((string, position) -> option(position))) =

    /*
     * Get a random position in an area, for an element of given size
     */
    get_random_pos({x=xmax y=ymax}: position, {x=width y=height}: position)
        : position =
      x = Random.int(xmax - width)
      y = Random.int(ymax - height)
      ~{x y}

    /*
     * Try to find a free random position in the cloud layout.
     * The number of tries is fixed, and [none] is returned if no free
     * position is found.
     */
    get_free_random_pos(container_size: position, boxes, size: position) =
      rec aux(tries: int) =
        if tries > 0 then
          new_pos = get_random_pos(container_size, size)
          new_box = {left=new_pos.x top=new_pos.y width=size.x height=size.y}
          if List.exists(intersect(_, new_box), boxes) then
            aux(tries - 1)
          else
            (some(new_pos), new_box +> boxes)
        else
          (none, boxes)
      aux(10)

    /*
     * Build a layout with random positions of items. Each element is
     * placed randomly in the cloud area, with multiple tries until a free
     * space is found (the element being discarded if all tries fail).
     */
    randomize_positions(container_size: position, items): xhtml =
      get_free_random_pos =
        get_free_random_pos(container_size, _, _)
      get_place(i, item, (boxes, accu)) =
        // Retrieving a random fitting place given an item and taken places
        item_id = get_item_id(id, i)
        build_result(~{x y}) =
          pos_opt = WStyler.make_style(css {
              margin: 0px;
              padding: 0px;
              position: absolute;
              left: {x}px;
              top: {y}px;
            }) |> some(_)
          accu <+> get_item(config, item_id, handles_opt, item, pos_opt)
        (free_pos_opt, new_boxes) =
          dim = {x = Dom.get_width(#{item_id}) y=Dom.get_height(#{item_id})}
 //         ~{x=w y=h} = dim
          get_free_random_pos(boxes, dim)
        (new_boxes, Option.switch(build_result, accu, free_pos_opt))

      (_, result) = List.foldi(get_place, items, ([], <></>))
      result

    draw(container_size_opt: option(position), first_draw: bool): xhtml =
      if first_draw then
        // Drawing a simple list layout while we don't know element sizes
        make_list_layout(config, id, handles_opt, unsorted_order(false), items).f1
      else
        // Redraw the real layout knowing element sizes (since the DOM has
        // already been displayed once)
        container_size = container_size_opt ? {x=300 y=150}
        randomize_positions(container_size, items)
    // Returning a tuple of the initial cloud content and the redraw function
    (draw(none, true), ((_, container_size) ->
        do Dom.transform([#{get_list_id(id)} <- draw(some(container_size), false)])
        none))

  /*
   * {2 Cloud building functions}
   */

  /*
   * Select and return the cloud layout depending on the config setting
   */
  @private
  get_layout(config: WCloud.config, id: string,
      handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
      items: list(WCloud.item('item)))
      : (xhtml, ((string, position) -> option(position))) =
    match config.layout with
      | {list=ord}   -> make_list_layout(config, id, handles_opt, ord,  items)
      | {greedy=ord} -> make_greedy_layout(config, id, handles_opt, ord, items)
      | {random}     -> make_random_layout(config, id, handles_opt, items)
      | _            -> (<></>, (_ -> none))

  /* Return the XHTML corresponding to the cloud */
  @private
  get_cloud(config: WCloud.config, id: string,
      handles_opt: option(list((Dom.event.kind, (WCloud.item('item) -> (Dom.event -> void))))),
      items: list(WCloud.item('item))): xhtml =
    list_id = get_list_id(id)
    items = normalize_weight(items)
    (elements, redraw) = get_layout(config, id, handles_opt, items)
    default_css = css {position: relative;}
    switch_css(f, default_css, opt): Css.properties =
      Option.switch((val -> default_css ++ f(val)), default_css, opt)
    default_css = switch_css(w ->
        css {width: {w}px}, default_css, config.dimensions.f1)
    default_css = switch_css(h ->
        css {height: {h}px}, default_css, config.dimensions.f2)
    default_stl = WStyler.make_style(default_css)
    stl = WStyler.merge([default_stl, config.cloud_style])

    /* Retrieve real size of a cloud already displayed in the browser */
    get_cloud_size(): position = {
        x=Dom.get_width(#{list_id})
        y=Dom.get_height(#{list_id})
      }

    /* Set the size of a cloud already displayed in the browser */
    set_cloud_size(size: position) =
      do Dom.set_width(#{list_id}, size.x)
      do Dom.set_height(#{list_id}, size.y)
      void

    <div>
      <ul id={list_id}
          style={css {padding: 0px; margin: 0px;}}
          onready={_ ->
              // FIXME: 'onready' doesn't seem to be executed
              list_size = get_cloud_size()
              new_size = redraw((list_id, list_size))
              set_cloud_size(new_size ? list_size)}>
        {elements}
      </ul>
    </div>
      |> WStyler.set(stl, _)


}}
