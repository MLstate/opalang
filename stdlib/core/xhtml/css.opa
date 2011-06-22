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
/*
    @authors ?
**/

import stdlib.core.{web.core, map, color}

/**
 * Definition of the main OPA types for CSS.
 *
 * @category WEB
 * @destination PRIVATE?
 * @stability UNSTABLE -- NEEDS REWRITE
 */

/**
 * {1 About this module}
 *
 * This module provides the basic type definitions for the CSS (see {{:http://www.w3.org/TR/CSS2/} the full specification})
 * data structure, together with some handling functions.
 *
 * {1 Where do I start}
 *
 * The main part of this file is the definition of the CSS properties ({!Css.unary}). These
 * have a simple correspondance with css2, described below.
 *
 * This module is transitional and will soon be replaced by a better structured approach.
 *
 */

type Css.compiled_property = {name: string; value:string}

Css_private =
{{
  order = Order.make_unsafe(compare) : order(Css.entry, Css.order)
  Entry_map=Map_make(/*compare:Css.entry->Css.entry->int*/ order)
}}

@opacapi
__internal__add_css_entry(k:list, data:map(string,'b), m:ordered_map(Css.entry, map(string,'b), Css.order)) =
   match Css_private.Entry_map.get(k, m)
     | {~some} -> Css_private.Entry_map.add(k, StringMap.union(data, some), m)
     | {none}  -> Css_private.Entry_map.add(k, data, m)

Css_printer =
{{

  set(lst, old) =
    eq(old, new) = match (old, new) with
       // FIXME This is bound to be (get) wrong...
      | ({ background = _ }, { background = _ }) -> true
      | ({ border = _ }, { border = _ }) -> true
      | ({ border_collapse = _}, {border_collapse = _}) -> true
      | ({ border_spacing = _}, {border_spacing = _}) -> true
      | ({ color = _ }, { color = _ }) -> true
      | ({ cursor = _ }, { cursor = _ }) -> true
      | ({ direction = _ }, { direction = _ }) -> true
      | ({ display = _ }, { display = _ }) -> true
      | ({ float = _ }, { float = _ }) -> true
      | ({ font = _ }, { font = _ }) -> true
      | ({ font_family = _ }, { font_family = _ }) -> true
      | ({ font_size = _ }, { font_size = _ }) -> true
      | ({ font_decoration = _ }, { font_decoration = _ }) -> true
      | ({ height = _ }, { height = _ }) -> true
      | ({ left = _ }, { left = _ }) -> true
      | ({ letter_spacing = _ }, { letter_spacing = _ }) -> true
      | ({ line_height = _ }, { line_height = _ }) -> true
      | ({ list_style = _ }, { list_style = _ }) -> true
      | ({ margin = _ }, { margin = _ }) -> true
      | ({ overflow = _ }, { overflow = _ }) -> true
      | ({ padding = _ }, { padding = _ }) -> true
      | ({ position = _ }, { position = _ }) -> true
      | ({ table_layout = _ }, { table_layout = _ }) -> true
      | ({ text_align = _ }, { text_align = _ }) -> true
      | ({ top = _ }, { top = _ }) -> true
      | ({ visibility = _ }, { visibility = _ }) -> true
      | ({ width = _ }, { width = _ }) -> true
      | ({ white_space = _ }, { white_space = _ }) -> true
      | ({ z_index = _ }, { z_index = _ }) -> true
      | _ -> false
    rec bar(lst, prop, accu) =
      match lst with
        | [hd | tl] ->
          if eq(prop, hd) then (hd, List.append(tl, accu))
          else bar(tl, prop, List.cons(hd, accu))
        | [] -> (prop, accu)
    rec foo(add, old, accu) =
      match old : list with
        | [hd | tl] ->
          (prop, add) = bar(add, hd, [])
          foo(add, tl, List.cons(prop, accu))
        | [] -> List.append(accu, add)
    foo(lst, old, [])

  concat(x) = String.concat(" ", x)
  css_property(name, value)   = ~{ name value }
  css_properties(name, value) = [ css_property(name, value) ]
  property_of_option(s,o) =
    match o with
    | { none } -> []
    | { ~some } -> css_properties(s,some)

  to_xhtml_style(style: Css.properties): list(Css.compiled_property) =
    get_list_sep(get_alpha,sep,l) =
      List.foldl(( e,str -> str ^ get_alpha(e) ^ sep ),l,"")
    get_list(get_alpha,l) =
      get_list_sep(get_alpha," ",l)
    get_option(get_alpha,o) =
      Option.map(get_alpha,o) ? ""
    get_color(color) = Color.color_to_string(color)
    get_url(url:url) = "url(\"{Url.encode(url)}\")"
    get_z_index =
      | { none } -> "auto"
      | { ~some } -> "{some:int}"
    get_size(x:Css.size):string = match x
      | { ~cm } -> "{cm}cm"
      | { ~em } -> "{em}em"
      | { ~ex } -> "{ex}ex"
      | { ~inch } -> "{inch}in"
      | { ~mm } -> "{mm}mm"
      | { ~percent } -> "{percent}%"
      | { ~pc } -> "{pc}pc"
      | { ~pt } -> "{pt}pt"
      | { ~px } -> "{px}px"
    get_size_or_normal(x:Css.size_or_normal):string = match x
      | { normal } -> "normal"
      | { ~size } -> get_size(size)
    get_size_or_none(x:Css.size_or_none):string = match x
      | { none } -> "none"
      | { ~size } -> get_size(size)
    get_white_space =
      | { normal } -> "normal"
      | { pre } -> "pre"
      | { nowrap } -> "nowrap"
    get_align =
      | { left } -> "left"
      | { right } -> "right"
      | { center } -> "center"
      | { justify } -> "justify"
    get_style_position =
      | { inside } -> "inside"
      | { outside } -> "outside"
    get_case =
      | { upper_case } -> "upper"
      | { lower_case } -> "lower"
    get_list_style_def =
      | { ~image } -> get_url(image)
      | { disc } -> "disc"
      | { square } -> "square"
      | { decimal = { leading } } -> "decimal-leading-zero"
      | { decimal = { not_leading } } -> "decimal"
      | { ~roman } -> get_case(roman) ^ "-roman"
      | { ~latin } -> get_case(latin) ^ "-latin"
      | { greek } -> "lower-greek"
    get_list_style({ ~style_position ~style } as l) =
      if Css_build.no_list_style == l then
        none
      else
        some(concat([get_option(get_style_position,style_position),
                     get_option(get_list_style_def,style)
                    ]))
    get_visibility =
      | { visible } -> "visible"
      | { hidden } -> "hidden"
    get_direction =
       | { left_to_right } -> "ltr"
       | { right_to_left } -> "rtl"
    get_float =
      | { left } -> "left"
      | { right } -> "right"
      | { css_none } -> "none"
    get_position =
      | { absolute } -> "absolute"
      | { relative } -> "relative"
      | { fixed } -> "fixed"
      | { static } -> "static"
    get_cursor_resize =
      | { n } -> "n-resize"
      | { s } -> "s-resize"
      | { e } -> "e-resize"
      | { w } -> "w-resize"
      | { ne } -> "ne-resize"
      | { nw } -> "nw-resize"
      | { se } -> "se-resize"
      | { sw } -> "sw-resize"
    get_default_cursor =
      | { pointer } -> "pointer"
      | { auto } /* default */ -> "auto"
      | { default } -> "default"
      | { crosshair } -> "crosshair"
      | { progress } -> "progress"
      | { move } -> "move"
      | { text } -> "text"
      | { ~resize } -> get_cursor_resize(resize)
      | { help } -> "help"
      | { wait } -> "wait"
    get_cursor({ ~icons ~default}) =
      concat([get_list_sep(get_url,",",icons), get_default_cursor(default)])
    get_decoration =
      | { italic } -> "italic"
      | { bold } -> "bold"
      | { underline }
      | { overline }
      | { line_through } -> error("CSS::to_xhtml_style: Unexpected underline/overline/line-through in font")
      | { small_caps } -> "small_caps"
      | { normal } -> "normal"
    get_font_family =
      //quote(s) = "\"" ^ s ^ "\""
      //(s -> if List.exists(_ == ' ', String.to_list(s)) then quote(s) else s) @
      | { TimesNewRoman } -> "Cambria, \"Hoefler Text\", Utopia, \"Liberation Serif\", \"Nimbus Roman No9 L Regular\", Times, \"Times New Roman\", serif"
      | { Georgia } -> "Constantia, \"Lucida Bright\", Lucidabright, \"Lucida Serif\", Lucida, \"DejaVu Serif,\" \"Bitstream Vera Serif\", \"Liberation Serif\", Georgia, serif"
      | { Garamond } -> "\"Palatino Linotype\", Palatino, Palladio, \"URW Palladio L\", \"Book Antiqua\", Baskerville, \"Bookman Old Style\", \"Bitstream Charter\", \"Nimbus Roman No9 L\", Garamond, \"Apple Garamond\", \"ITC Garamond Narrow\", \"New Century Schoolbook\", \"Century Schoolbook\", \"Century Schoolbook L\", Georgia, serif"
      | { Helvetica } -> "Frutiger, \"Frutiger Linotype\", Univers, Calibri, \"Gill Sans\", \"Gill Sans MT\", \"Myriad Pro\", Myriad, \"DejaVu Sans Condensed\", \"Liberation Sans\", \"Nimbus Sans L\", Tahoma, Geneva, \"Helvetica Neue\", Helvetica, Arial, sans-serif"
      | { Verdana } -> "Corbel, \"Lucida Grande\", \"Lucida Sans Unicode\", \"Lucida Sans\", \"DejaVu Sans\", \"Bitstream Vera Sans\", \"Liberation Sans\", Verdana, \"Verdana Ref\", sans-serif"
      | { Trebuchet } -> "\"Segoe UI\", Candara, \"Bitstream Vera Sans\", \"DejaVu Sans\", \"Bitstream Vera Sans\", \"Trebuchet MS\", Verdana, \"Verdana Ref\", sans-serif"
      | { HeavyImpact } -> "Impact, Haettenschweiler, \"Franklin Gothic Bold\", Charcoal, \"Helvetica Inserat\", \"Bitstream Vera Sans Bold\", \"Arial Black\", sans-serif"
      | { Monospace } -> "Consolas, \"Andale Mono WT\", \"Andale Mono\", \"Lucida Console\", \"Lucida Sans Typewriter\", \"DejaVu Sans Mono\", \"Bitstream Vera Sans Mono\", \"Liberation Mono\", \"Nimbus Mono L\", Monaco, \"Courier New\", Courier, monospace"
    get_font({ ~size ~line_height ~family ~decoration}) =
      concat([get_list(get_decoration,decoration),
              get_size(size),
              "/",
              get_size_or_normal(line_height),
              get_font_family(family)])
    get_display =
      | { block } -> "block"
      | { inline } -> "inline"
      | { inline_block } -> "inline-block"
      | { css_none } -> "none"
    get_overflow =
      | { hidden } -> "hidden"
      | { visible } -> "visible"
      | { scroll } -> "scroll"
      | { auto } -> "auto"
    get_block_size(prefix, {~t ~l ~b ~r}) =
      f((local_prefix,v)) =
        Option.map(v -> css_property(prefix ^ "-" ^ local_prefix, get_size(v)), v)
      List.filter_map(f, [("top",t),("bottom",b),("left",l),("right",r)])
    get_border_style_elt =
      | { css_none } -> "none"
      | { hidden } -> "hidden"
      | { dotted } -> "dotted"
      | { dashed } -> "dashed"
      | { solid } -> "solid"
      | { double } -> "double"
      | { groove } -> "groove"
      | { ridge } -> "ridge"
      | { inset} -> "inset"
      | { outset } -> "outset"
    get_border_thickness =
      | { thin } -> "thin"
      | { medium } -> "medium"
      | { thick } -> "thick"
      | { ~size } -> get_size(size)
    get_vertical_align =
      | { middle } -> "middle"
      | { baseline } -> "baseline"
    get_border_type({~style ~width ~color} as v) =
      if v == Css_build.no_border_type then
        none
      else
        some(concat([get_option(get_border_style_elt,style),
                     get_option(get_border_thickness,width),
                     get_option(get_color,color),
                    ]))
    get_border_collapse =
      | { collapse } -> "collapse"
      | { separate } -> "separate"
      | { inherit } ->  "inherit"
    get_table_layout =
      | { auto } -> "auto"
      | { fixed } -> "fixed"
      | { inherit } ->  "inherit"
    get_repeat =
      | { x } -> "repeat-x"
      | { y } -> "repeat-y"
      | { css_none } -> "no-repeat"
      | { both } -> "repeat"
    get_background_position_y =
      | { center } -> "center"
      | { top    } -> "top"
      | { bottom } -> "bottom"
      | { ~size } -> get_size(size)
    get_background_position_x =
      | { center } -> "center"
      | { left   } -> "left"
      | { right  } -> "right"
      | { ~size } -> get_size(size)
    get_attached(_) =
      "fixed"
    get_background_position({~x ~y}) =
      "{get_background_position_y(y)} {get_background_position_x(x)}"
    get_background({~url ~position ~repeat ~color ~attached} as b) =
      if Css_build.no_background == b then
        none
      else
        some(concat([get_option(get_url,url),
                     get_option(get_background_position,position),
                     get_option(get_repeat,repeat),
                     get_option(get_color,color),
                     get_option(get_attached,attached),
                    ]))
    get_font_decoration(font_decoration) =
      is_text_dec(x) =
        match x : Css.decoration with
        | {overline} | {underline} | { line_through } -> {true}
        | _ -> {false}
      (over_under,others) = List.partition(is_text_dec,font_decoration)
      l1 =
        match List.unique_list_of(over_under) with
        | [] -> []
        | [hd] ->
          match hd with
          | { underline } -> css_properties("text-decoration","underline")
          | { overline } -> css_properties("text-decoration","overline")
          | { line_through } -> css_properties("text-decoration","line-through")
          | _ -> error("CSS::to_xhtml_string: Compiler bug")
          end
        | _ -> css_properties("text-decoration","underline overline")
      add_and_filter_out(name,v,l) = css_property(name,v) +> List.filter(x -> x.name != name, l)
      f(font_decoration,acc) =
        match font_decoration with
        | { italic } ->
        add_and_filter_out("font-style","italic",acc)
        | { bold } ->
        add_and_filter_out("font-weight","bold",acc)
        | { underline }
        | { overline }
        | { line_through } -> error("CSS::to_xhtml_string: Compiler bug")
        | { small_caps } ->
        add_and_filter_out("font-variant","small-caps",acc)
        | { normal } ->
          [css_property("font-style","normal"),
           css_property("font-weight","normal"),
           css_property("font-variant","normal"),
          ]
        end
      l2 = List.rev(List.foldl(f,others,[]))
      l1 ++ l2
    get_font_variant =
    | { normal } -> "normal"
    | { inherit } -> "inherit"
    | { small_caps} -> "small-caps"
    get_opacity(o : float) =
      oi = Int.of_float(o * 100.)
      [ css_property("opacity", "{o}")
      , css_property("filter", "alpha(opacity={oi})")
      , css_property("-ms-filter", "progid:DXImageTransform.Microsoft.Alpha(opacity={oi})")
      ]
    get_border_radius(border_radius : Css.size) =
      br = get_size(border_radius)
      [ css_property("border-radius", br)
      , css_property("-moz-morder-radius", br)
      , css_property("-webkit-border-radius", br)
      ]
    rec aux(style) =
      (match style: Css.unary with
        | { ~background } ->
          property_of_option("background",get_background(background))
        | { border = (border,border_type) } ->
            s = match border with
                | { all } -> "border"
                | { left } -> "border-left"
                | { right } -> "border-right"
                | { top } -> "border-top"
                | { bottom } -> "border-bottom"
            property_of_option(s, get_border_type(border_type))
        | { ~border_collapse } -> css_properties("border-collapse", get_border_collapse(border_collapse))
        | { ~border_radius } -> get_border_radius(border_radius)
        | { ~border_spacing } -> css_properties("border-spacing", get_size(border_spacing))
        | { ~bottom } -> css_properties("bottom", get_size(bottom))
        | { ~color } -> css_properties("color", get_color(color))
        | { ~cursor } -> css_properties("cursor", get_cursor(cursor))
        | { ~direction } -> css_properties("direction", get_direction(direction))
        | { ~display } -> css_properties("display", get_display(display))
        | { ~float } -> css_properties("float", get_float(float))
        | { ~font } ->
            // treating decoration a special way because we might have overline and underline, which are not part of a 'font' declaration in CSS
            css_properties("font", get_font({font with decoration=[]})) ++
              aux({font_decoration = font.decoration})
        | { ~font_family } -> css_properties("font-family", get_font_family(font_family))
        | { ~font_size } -> css_properties("font-size", get_size(font_size))
        | { ~font_decoration } -> get_font_decoration(font_decoration)
        | { ~font_variant } -> css_properties("font-variant", get_font_variant(font_variant))
        | { ~height } -> css_properties("height", get_size(height))
        | { ~left } -> css_properties("left", get_size(left))
        | { ~letter_spacing } -> css_properties("letter-spacing", get_size_or_normal(letter_spacing))
        | { ~line_height } -> css_properties("line-height", get_size(line_height))
        | { ~list_style } -> property_of_option("list-style",get_list_style(list_style))
        | { ~margin } -> get_block_size("margin",margin)
        | { ~max_height } -> css_properties("max-height", get_size_or_none(max_height))
        | { ~min_height } -> css_properties("min-height", get_size_or_none(min_height))
        | { ~max_width } -> css_properties("max-width", get_size_or_none(max_width))
        | { ~min_width } -> css_properties("min-width", get_size_or_none(min_width))
        | { ~overflow } -> css_properties("overflow", get_overflow(overflow))
        | { ~opacity } -> get_opacity(opacity)
        | { ~padding } -> get_block_size("padding",padding)
        | { ~position } -> css_properties("position", get_position(position))
        | { ~right } -> css_properties("right", get_size(right))
        | { ~table_layout } -> css_properties("table-layout", get_table_layout(table_layout))
        | { ~text_align } -> css_properties("text-align", get_align(text_align))
        | { ~top } -> css_properties("top", get_size(top))
        | { ~vertical_align } -> css_properties("vertical_align", get_vertical_align(vertical_align))
        | { ~visibility } -> css_properties("visibility", get_visibility(visibility))
        | { ~width } -> css_properties("width", get_size(width))
        | { ~white_space } -> css_properties("white-space", get_white_space(white_space))
        | { ~z_index } -> css_properties("z-index", get_z_index(z_index))
        | { ~not_typed } -> css_properties(not_typed.f1, not_typed.f2)
        | _  -> error("Css.to_xhtml_style::root"))
    List.flatten(List.map(aux,style))

}}

/**
 * The main type for css: describes CSS display properties.
 *
 * Includes many auxiliary types below
 */
type Css.unary =
    { background: Css.background }
  / { border: Css.border }
  / { border_collapse : Css.border_collapse }
  / { border_radius: Css.size }
  / { border_spacing: Css.size }
  / { bottom: Css.size }
  / { color: color }
  / { cursor: Css.cursor }
  / { direction: Css.direction }
  / { display: Css.display }
  / { float: Css.float }
  / { font: Css.font }
  / { font_family : Css.font_family }
  / { font_size: Css.size }
  / { font_decoration: list(Css.decoration) }
  / { font_variant: Css.font_variant }
  / { height: Css.size }
  / { left: Css.size }
  / { letter_spacing: Css.size_or_normal }
  / { line_height : Css.size }
  / { list_style: Css.list_style }
  / { margin: Css.block_size }
  / { max_height: Css.size_or_none }
  / { min_height: Css.size_or_none }
  / { max_width: Css.size_or_none }
  / { min_width: Css.size_or_none }
  / { opacity: float }
  / { overflow: Css.overflow }
  / { padding: Css.block_size }
  / { position: Css.position }
  / { right: Css.size }
  / { table_layout: Css.table_layout }
  / { text_align: Css.align }
  / { top: Css.size }
  / { visibility: Css.visibility }
  / { width: Css.size }
  / { white_space: Css.white_space }
  / { vertical_align: Css.vertical_align }
  / { z_index: option(int) }
  / { not_typed: (string,string) }
/*
  we show the correspondance between Css.unary and css2 definitions
         ------------------------------------------------
           css_unary                       css
         -------------------------------------------------
           background                 background
                      background-{color,attachment,image,position,repeat}
            border           border,border-{top,bottom,left,right}
            bottom                       bottom
            color                        color
            cursor                       cursor
           direction                   direction
            display                     display
             float                       float
             font                font,text-decoration
          font_family                 font-family
           font_size                   font-size
         font_decoration          font-variant,font-style,
                                font-weight,text-decoration
           height                       height
            left                         left
        letter_spacing               letter-spacing
          list_style             list-style,list-style-image
                              list-style-position,list-style-type
            margin           margin,margin-{top,bottom,left,right}
           overflow                    overflow
           padding          padding,padding-{top,bottom,left,right}
           position                    position
            right                       right
          text_align                  text-align
             top                          top
           visibility                  visibility
          white_space                 white-space
          z_index                       z-index

    The properties that are different in css and in css_unary can be done the following way:
    - background-color: blue -> background: blue
      (etc. for background-attachment, background-image, ...)

    - border-top: ... -> border({top},...)
      (etc.)

    - text-decoration: overline -> font_decoration: overline

    - font-weight: bold -> font_decoration: bold
      font-variant: small-caps -> font_decoration: small_caps
      font-style: italic -> font_decoration: italic

    - list-style-position: outside -> list-style: outside
      (etc. pour list-style-*)

    - margin-left: value -> margin(none,some(value),none,none)
      (etc. pour margin-*)

    - padding: works just like margin

    For concrete syntax examples, see opa/test/style.opa
*/

// TODO: once possible, inline as many "small" sum types here into the Css.unary type

// ** BORDER-COLLAPSE ****************
type Css.border_collapse =
    { inherit }
  / { separate }
  / { collapse }

// ** TABLE-LAYOUT ****************
type Css.table_layout =
    { auto }
  / { fixed }
  / { inherit }

// ** VERTICAL-ALIGN ****************
type Css.vertical_align =
    { baseline } /* default */
  / { middle }

// ** WHITE-SPACE ****************
type Css.white_space =
    { normal } /* default */
  / { pre }
  / { nowrap }

// ** TEXT ALIGNM ****************
type Css.align =
    { left } /* default, i guess */
  / { center }
  / { right }
  / { justify }

// ** LIST STYLE ****************#
type Css.style_position =
    { inside }
  / { outside } /* default */

type Css.list_style = // should not have (none,none) (but the case is treated by to_xhtml_style anyway)
    { style_position: option(Css.style_position)
      style : option(Css.list_style_def)
    }

type Css.list_style_def =
    { image: url }
  / { disc } /* default */
  / { square }
  / { decimal: leading_zero}
  / { roman: case }
  / { greek }
  / { latin: case }

type case = { lower_case } / { upper_case }
type leading_zero =
    { leading } /* 01 02 ... 10 */
  / { not_leading } /* 1 2 ... 10 */

// ** DIRECTION ******************
type Css.direction =
    { right_to_left }
  / { left_to_right }

// ** VISIBILITY ****************#
type Css.visibility =
    { visible } /* default */
  / { hidden }

// ** FLOAT **********************
type Css.float =
    { left }
  / { right }
  / { css_none } /* default */ /* not "none" to avoid conflict with the option type. Would be nicer to embed this sum into the Css.unary type once we can */

// ** POSITION ******************#
type Css.position =
    { absolute }
  / { relative }
  / { fixed }
  / { static } /* default */

// ** CURSOR ********************#
type Css.cursor_resize =
    { n }
  / { s }
  / { e }
  / { w }
  / { ne }
  / { nw }
  / { se }
  / { sw }

type Css.default_cursor =
    { pointer }
  / { auto } /* default */
  / { default }
  / { crosshair }
  / { progress }
  / { move }
  / { text }
  / { resize: Css.cursor_resize }
  / { help }
  / { wait }

type Css.cursor =
     { icons: list(url)
       default: Css.default_cursor
     }

// ** FONT **********************#
type Css.font =
     { size: Css.size
       line_height: Css.size_or_normal
       family: Css.font_family
       decoration: list(Css.decoration)
     }

/*
  you can't specify exact fonts, just stacks of fonts
  see get_font_family definition to see the content of each family
*/
type Css.font_family =
    { TimesNewRoman }
  / { Georgia }
  / { Garamond }
  / { Helvetica }
  / { Verdana }
  / { Trebuchet }
  / { HeavyImpact }
  / { Monospace }

type Css.font_variant =
    { normal }
  / { inherit }
  / { small_caps }

// ** DECORATION ****************#
type Css.decoration =
    { italic }
  / { bold }
  / { underline }
  / { overline }
  / { line_through }
  / { small_caps }
  / { normal }

// ** DISPLAY ********************
type Css.display =
    { block }
  / { inline }
  / { inline_block }
  / { css_none } /* not "none" to avoid conflict with the option type. Would be nicer to embed this sum into the Css.unary type once we can */

// ** OVERFLOW ******************#
type Css.overflow =
    { hidden }
  / { visible }
  / { scroll }
  / { auto }

// ** SIZE **********************#
@opacapi
type Css.size =
    { cm : float }
  / { em: float }
  / { ex: float }
  / { inch : float}
  / { mm: float }
  / { pc : float }
  / { percent: float }
  / { pt : float }
  / { px: int }

@opacapi
type Css.size_or_normal =
    { normal }
  / { size : Css.size }

@opacapi
type Css.size_or_none =
    { none }
  / { size : Css.size }

// ** BORDER_SIZE ****************
type Css.block_size = // none on every field is not supposed to happen, but even if it happens, to_xhtml_style is going to ignore it
    { t: option(Css.size) l: option(Css.size) b: option(Css.size) r: option(Css.size) }

// ** BORDER_TYPE ****************
type Css.border =
    tuple_2(border, Css.border_type)

type border =
    { all }
  / { left }
  / { right }
  / { top }
  / { bottom }

type Css.border_style_elt =
    { css_none } /* default */ /* not "none" to avoid conflict with the option type. Would be nicer to embed this sum into the Css.unary type once we can */
  / { hidden }
  / { dotted }
  / { dashed }
  / { solid }
  / { double }
  / { groove }
  / { ridge }
  / { inset }
  / { outset }

type Css.border_thickness =
    { thin }
  / { medium }
  / { thick }
  / { size: Css.size }

type Css.border_type =
    { style: option(Css.border_style_elt)
      width: option(Css.border_thickness)
      color: option(color) }

// ** BACKGROUND ****************#
@opacapi
type Css.background =
{ url: option(url)
  position: option(Css.background_position)
  repeat: option(Css.background_repeat)
  color: option(color)
  attached : option(void)
}

type Css.background_position =
{
  x: Css.background_position_x
  y: Css.background_position_y
}

type Css.background_repeat =
    { x }
  / { y }
  / { css_none } /* not "none" to avoid conflict with the option type. Would be nicer to embed this sum into the Css.unary type once we can */
  / { both } /* default */

type Css.background_position_y =
    { center }
  / { top } /* default */
  / { bottom }
  / { size: Css.size }

type Css.background_position_x =
    { center }
  / { left  } /* default */
  / { right }
  / { size: Css.size }


/**
 * The selector type describes the criterion that must be respected for some
 * style to be applied to an xhtml element.
 */
type Css.selector = list(Css.selector_path) /*a, p*/
type Css.selector_path = list(Css.selector_conjunction) /*a p*/
type Css.selector_conjunction = list(Css.selector_item)  /* a:onclick.classA */
@opacapi
type Css.event = {hover} / {active} / {visited} / {link} / {first_child} / {focus} // todo: complete this list // ADAM says: I think now only lang is missing (which is a bit more tricky as it takes argument)
@opacapi
type Css.selector_item =
  { tag : string } /
  { id: string } /
  { class : string } /
  { event : Css.event}

/**
 * An entry is described by, err... a list of disjunctions of conjunctions of items ?? (Louis)
 */
type Css.entry = list(list(list(Css.selector_item)))

type Css.properties = list(Css.unary)

// Bound from the opabsl
type css_properties = Css.properties

/**
 * Constructors for typed css
 *
 * This module defines all constructors to build values of the main [Css.unary] type.
 */

Css_build =
{{
/**
 * this two function should no be insinde the module,
 * but because of open module with local environ crash, they moved here
 */
  sum_option(o1,o2) =
    match (o1,o2) with
    | (_,{none}) -> o1
    | _ -> o2

  size_or_normal_of_option_size(v) =
    match v with
    | { none } -> { normal }
    | { ~some } -> { size = some }

  /**** background *****/
  /*
     -- background([background_repeat,background_image(string)])
     corresponds to
     -- background: repeat url(string)

     if you define a value several times, only the last one is kept
     -- background([background_color(Color.blue),background_color(Color.red)])
     is equivalent to
     -- background([background_color(Color.red)])
     (which means that
     -- background: blue red
     will be understood as
     -- background: red
  */
  no_background = { url = none position = none repeat = none color = none attached = none } : Css.background
  sum_background({url=url1 position=position1 repeat=repeat1 color=color1 attached=attached1},
                 {url=url2 position=position2 repeat=repeat2 color=color2 attached=attached2}) =
     { url = sum_option(url1,url2)
       position = sum_option(position1,position2)
       repeat = sum_option(repeat1,repeat2)
       color = sum_option(color1,color2)
       attached = sum_option(attached1,attached2)
     }
  rev_sum_background(b1,b2) = sum_background(b2,b1)
  background(l:list(Css.background)) = { background = List.foldl(rev_sum_background,l,no_background)} : Css.unary
  background_image(url) = { no_background with url = some(url)} : Css.background
  background_position(x,y) = { no_background with position = some({~x ~y})} : Css.background
  background_no_repeat = { no_background with repeat = some({css_none}) } : Css.background
  background_repeat = { no_background with repeat = some({both}) } : Css.background
  background_repeat_x = { no_background with repeat = some({x}) } : Css.background
  background_repeat_y = { no_background with repeat = some({y}) } : Css.background
  background_color(color) = { no_background with color = some(color)} : Css.background
  background_attached = { no_background with attached = some(void)} : Css.background

  /***** border ******/
  /*
    -- border-left: thick
    corresponds to
    -- border_left(border_thick)

    -- border-left: thick dotted
    corresponds to
    -- border_left(border_type([border_thick,border_dotted]))

   */
  rev_sum_border({style=style1 width=width1 color=color1},{style=style2 width=width2 color=color2}) =
    { style = sum_option(style2,style1)
      width = sum_option(width2,width1)
      color = sum_option(color2,color1)
    } : Css.border_type
  no_border_type = { style = none width = none color = none } : Css.border_type
  border_style(v) = { style = some(v) width = none color = none } : Css.border_type
  border_width(v) = { style = none width = some(v) color = none } : Css.border_type
  border_color(v) = { style = none width = none color = some(v) } : Css.border_type
  border_radius(v) = { border_radius = v } : Css.unary
  border_left(v) = { border = ({left}, v)} : Css.unary
  border_right(v) = { border = ({right}, v)} : Css.unary
  border_top(v) = { border = ({top}, v)} : Css.unary
  border_bottom(v) = { border = ({bottom}, v)} : Css.unary
  border_all(v) = { border = ({all}, v)} : Css.unary
  border_type(l) = List.foldl(rev_sum_border,l,no_border_type) : Css.border_type
  border_thin = border_width({thin})
  border_medium = border_width({medium})
  border_thick = border_width({thick})
  border_size(size) = border_width({~size})
  border_none = border_style({css_none})
  border_hidden = border_style({hidden})
  border_dotted = border_style({dotted})
  border_dashed = border_style({dashed})
  border_solid = border_style({solid})
  border_double = border_style({double})
  border_groove = border_style({groove})
  border_ridge = border_style({ridge})
  border_inset = border_style({inset})
  border_outset = border_style({outset})

  /*** border-collapse ****/
  border_collapse(v) = { border_collapse = v } : Css.unary

  /*** border-spacing ****/
  border_spacing(v) = { border_spacing = v } : Css.unary

  /**** bottom ****/
  bottom(s) = { bottom = s } : Css.unary

  /**** color ****/
  color(color) = { color= color }

  /**** cursor *****/
  /*
    -- cursor: image1, image2, move
    translates to
    -- cursor({icons=[image1,image2] default=cursor_move}
   */
  coerce_default_cursor(v) = v : Css.default_cursor
  cursor_pointer = coerce_default_cursor({pointer})
  cursor_auto = coerce_default_cursor({auto})
  cursor_default = coerce_default_cursor({default})
  cursor_crosshair = coerce_default_cursor({crosshair})
  cursor_progress = coerce_default_cursor({progress})
  cursor_move = coerce_default_cursor({move})
  cursor_text = coerce_default_cursor({text})
  cursor_resize_n = coerce_default_cursor({resize = {n}})
  cursor_resize_s = coerce_default_cursor({resize = {s}})
  cursor_resize_e = coerce_default_cursor({resize = {e}})
  cursor_resize_w = coerce_default_cursor({resize = {w}})
  cursor_resize_ne = coerce_default_cursor({resize = {ne}})
  cursor_resize_nw = coerce_default_cursor({resize = {nw}})
  cursor_resize_se = coerce_default_cursor({resize = {se}})
  cursor_resize_sw = coerce_default_cursor({resize = {sw}})
  cursor_help = coerce_default_cursor({help})
  cursor_wait = coerce_default_cursor({wait})
  cursor(v) = { cursor = v } : Css.unary
  cursor2(v1,v2) = cursor({icons=v1 default=v2})

  /**** direction ****/
  direction(v) = { direction = v } : Css.unary
  left_to_right = direction({ left_to_right })
  right_to_left = direction({ right_to_left })

  /**** display ***/
  display(v) = { display = v } : Css.unary
  display_block = display({block})
  display_inline = display({inline})
  display_inline_block = display({inline_block})
  display_none = display({css_none})

  /*** float ****/
  float(v) = { float = v } : Css.unary
  float_left = float({left})
  float_right = float({right})
  float_none = float({css_none})

  /**** font_decoration ****/
  font_decorations(v) = { font_decoration = v } : Css.unary
  font_decoration(v) = font_decorations([v])
  font_variant(v) = { font_variant = v } : Css.unary
  italic = font_decoration({ italic })
  bold = font_decoration({ bold })
  underline = font_decoration({ underline })
  overline = font_decoration({ overline })
  line_through = font_decoration({ line_through })
  small_caps = font_decoration({ small_caps })
  normal = font_decoration({ normal })

  /**** font_family ****/
  font_family(v) = { font_family = v } : Css.unary
  font_TimesNewRoman = font_family({TimesNewRoman})
  font_Georgia = font_family({Georgia})
  font_Garamond = font_family({Garamond})
  font_Helvetica = font_family({Helvetica})
  font_Verdana = font_family({Verdana})
  font_Trebuchet = font_family({Trebuchet})
  font_HeavyImpact = font_family({HeavyImpact})
  font_Monospace = font_family({Monospace})

  /**** font_size ****/
  font_size(s) = { font_size = s } : Css.unary

  /**** font ****/
  font(v) = { font = v } : Css.unary
  font2(decoration,size,lineo,family) =
    font({ size = size;
           line_height = size_or_normal_of_option_size(lineo);
           family = family;
           decoration = decoration
         })
  /**** height ****/
  height(size) = { height = size} : Css.unary
  max_height(size) = { max_height = size } : Css.unary
  min_height(size) = { min_height = size } : Css.unary

  /**** left ****/
  left(size) = { left = size} : Css.unary

  /*** letter_spacing ****/
  letter_spacing(size_normal) =
    { letter_spacing = size_normal } : Css.unary

  /**** line-height ****/
  line_height(size) = { line_height = size} : Css.unary

  /**** list_style ****/
  /*
    -- list-style: square outside
    -- list_style({style=square position={outside}})

    shortcuts:
    -- list-style: square
    -- list_square

    -- list-style: inside
    -- list_inside
  */
  list_style(v) = { list_style = v } : Css.unary
  list_style2(pos,style) = list_style({ style=style style_position=pos })
  list_style_position(v) = list_style2(some(v),none)
  list_style_style(style)= list_style2(none,some(style))
  list_style_inside = list_style_position({inside})
  list_style_outside = list_style_position({outside})
  no_list_style =
    { style_position = none;
      style = none
    } : Css.list_style
  list_image(url) = list_style_style({image = url})
  list_disc = list_style_style({disc})
  list_square = list_style_style({square})
  list_decimal = list_style_style({decimal = {not_leading}})
  list_decimal_leading = list_style_style({decimal = {leading}})
  list_lower_roman = list_style_style({roman = {lower_case}})
  list_upper_roman = list_style_style({roman = {upper_case}})
  list_lower_latin = list_style_style({latin = {lower_case}})
  list_upper_latin = list_style_style({latin = {upper_case}})
  list_upper_alpha = list_upper_latin
  list_lower_alpha = list_lower_latin
  list_greek = list_style_style({greek})
  list_lower_greek = list_greek

  /**** margin  ****/
  margin(t,l,b,r) = { margin = {~t ~l ~b ~r} } : Css.unary
  margin_all(s) = margin(some(s),some(s),some(s),some(s))
  margin_top(s) = margin(some(s),none,none,none)
  margin_bottom(s) = margin(none,none,some(s),none)
  margin_left(s) = margin(none,some(s),none,none)
  margin_right(s) = margin(none,none,none,some(s))

  /**** opacity ****/
  opacity(f) = { opacity = f }

  /**** overflow ****/
  overflow(v) = { overflow = v } : Css.unary
  overflow_hidden = overflow({hidden})
  overflow_visible = overflow({visible})
  overflow_scroll = overflow({scroll})
  overflow_auto = overflow({auto})

  /**** padding ****/
  padding(t,l,b,r) = { padding = {~t ~l ~b ~r} } : Css.unary
  padding_all(s) = padding(some(s),some(s),some(s),some(s))
  padding_top(s) = padding(some(s),none,none,none)
  padding_bottom(s) = padding(none,none,some(s),none)
  padding_left(s) = padding(none,some(s),none,none)
  padding_right(s) = padding(none,none,none,some(s))

  /**** position ****/
  position(v) = { position = v } : Css.unary
  position_fixed = position({fixed})
  position_static = position({static})
  position_absolute = position({absolute})
  position_relative = position({relative})


  /**** right ****/
  right(size) = { right = size } : Css.unary

  /*** table-layout ****/
  table_layout(v) = { table_layout = v } : Css.unary

  /*** text_align ****/
  text_align(v) = { text_align = v } : Css.unary
  align_left = text_align({left})
  align_right = text_align({right})
  align_center = text_align({center})
  align_justify = text_align({justify})

  /**** top ****/
  top(s) = { top = s } : Css.unary

  /**** whitespace ***/
  vertical_align(v) = { vertical_align = v } : Css.unary
  vertical_align_middle = vertical_align({middle})
  vertical_align_baseline = vertical_align({baseline})

  /**** visibility ****/
  visibility(v) = { visibility = v} : Css.unary
  visible = visibility({ visible })
  hidden = visibility({ hidden })

  /**** width ****/
  width(s) = { width = s } : Css.unary
  max_width(size) = { max_width = size } : Css.unary
  min_width(size) = { min_width = size } : Css.unary

  /**** whitespace ***/
  white_space(v) = { white_space = v} : Css.unary
  white_space_pre = white_space({pre})
  white_space_normal = white_space({normal})
  white_space_nowrap = white_space( {nowrap})

  /*** index ***/
  z_index(o) = { z_index = o } : Css.unary
  z_index_n(n) = z_index(some(n))
  z_index_auto = z_index(none)
}}

/* ---------------- < utils for CSS > --------------- */
map_css_size(f) =
  | { ~cm } -> {cm = f(cm)}
  | { ~em } -> {em = f(em)}
  | { ~ex } -> {ex = f(ex)}
  | { ~inch } -> {inch = f(inch)}
  | { ~mm } -> {mm = f(mm)}
  | { ~pc } -> {pc = f(pc)}
  | { ~percent } -> {percent = f(percent)}
  | { ~pt } -> {pt = f(pt)}
  | { ~px } -> {px = Float.to_int(f(Int.to_float(px)))}


/* --------------------- < CSS > -------------------- */

type Css.prop_name = string

@opacapi
type Css.percentage = int

@opacapi
type Css.length =
    { nounitF : float } /
    { nounitI : float } / // but this is an int ;-) !
    { em : float } /
    { ex : float } /
    { px : float } /
    { inch : float } /
    { cm : float } /
    { mm : float } /
    { pt : float } /
    { pc : float }


px(x) = {length={px=x}:Css.length}:Css.prop_value_item


length_to_css_string(l) =
    f(x:float) = "{x}"
    i(x:float) = "{int_of_float(x)}";
    match l : Css.length with
      | { ex=x } -> "{i(x)}ex"
      | { em=x } -> "{f(x)}em"
      | { cm=x } -> "{f(x)}cm"
      | { mm=x } -> "{i(x)}mm"
      | { px=x } -> "{i(x)}px"
      | { pt=x } -> "{i(x)}pt"
      | { pc=x } -> "{i(x)}pc"
      | { inch=x   } ->"{f(x)}inch"
      | { nounitF=x } -> "{f(x)}"
      | { nounitI=x } -> "{i(x)}"


type Css.url = string

@opacapi
type Css.prop_value_item =
    { property : string } /
    { percentage : Css.percentage } /
    { length : Css.length } /
    { color : color } /
    { url : url }


type Css.prop_value = list(Css.prop_value_item)

type Css.props = map(Css.prop_name, Css.prop_value)

type Css.type = map(Css.selector, Css.props)

type Css.def = list(Css.type)

@abstract type Css.order = void

type Css.declaration = list(ordered_map(Css.entry, Css.props, Css.order))

empty_css = [] :  Css.declaration

css_map_merge(submerge, map2, map1) =
        Css_private.Entry_map.fold(
        (k2, v2, map1 -> match Css_private.Entry_map.get(k2, map1) with
                                |{~some} -> Css_private.Entry_map.add(k2, submerge(some, v2), map1)
                                |{none}  -> Css_private.Entry_map.add(k2, v2, map1)
        ), map2, map1)

/*
construct_map_css_def(css_def) = List.map( construct_map_css_type , css_def )
construct_map_css_type(
*/

generate_css_def(css_def:Css.declaration) =
    css_type =
      match css_def
      | [] -> Css_private.Entry_map.empty
      | [hd | tl] -> List.foldl(css_map_merge(css_props_merge,_,_), tl , hd)
    ;
    generate_css_type("", css_type)

generate_css_type(acc, css_type) =
    Css_private.Entry_map.fold(generate_paths_props, css_type, acc)

generate_paths_props(paths, props, acc:string) =
    "{acc}{generate_paths(paths)} \{{ generate_props(props, "") }\}\n"

generate_paths(paths) = String.concat(",", List.map(generate_path, paths))

generate_path(path)   = String.concat(" ", List.map(generate_selector, path))

generate_selector(sel)= String.concat("", List.map(generate_selector_item, sel))

generate_selector_item(sel) =
    match sel : Css.selector_item with
      | {~tag} -> "{tag}"
      | {~id} -> "#{id}"
      | {class=c} -> ".{c}"
      | {event={hover}} -> ":hover"
      | {event={active}} -> ":active"
      | {event={visited}} -> ":visited"
      | {event={link}} -> ":link"
      | {event={first_child}} -> ":first-child"
      | {event={focus}} -> ":focus"

generate_props(props:Css.props, acc:string) =
    add_prop(prop_name, prop_value, acc)="{acc}{prop_name} : {generate_prop_value(prop_value)};"
    Map.fold(add_prop, props, acc)

generate_prop_value(prop_value) =
    List.foldl( (it,acc-> acc^generate_prop_value_item(it)^" ") , prop_value, "")

generate_prop_value_item(item) =
    match item : Css.prop_value_item with
      | {~property}   -> property
      | {~percentage} -> "{percentage}%"
      | {~length}     -> length_to_css_string(length)
      | {~color}         -> Color.color_to_string(color)
      | {~url} -> "url('{Url.encode(url)}')"
      | _ -> error("BAD VALUE_ITEM")

css_props_merge(m1, m2) = Map.union(m1, m2)
