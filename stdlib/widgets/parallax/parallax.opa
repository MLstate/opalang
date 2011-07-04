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
 * A configurable parallax widget.
 *
 * @author Jessica Castejon, 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

package stdlib.widgets.parallax

import stdlib.widgets.core

/*
 * {1 About this module}
 *
 * Parallax widget
 * Inspired by https://github.com/500 and
 * http://webdev.stephband.info/parallax_demos.html
 *
 */

type WParallax.dim = {px_x: int; px_y: int}

type WParallax.percent = {x: float; y: float}

type WParallax.offset = {px: WParallax.dim}/{percent: WParallax.percent}

type WParallax.src_type = {img: string} / {text: string}

/**
 *An element of the parallax
 */
type WParallax.src = {
     content: WParallax.src_type
     id: string
     depth: int
     offset: WParallax.offset
     content_style : WStyler.styler
}

type WParallax.display = {vertical} / {horizontal} / {both}

/**
 *The configuration of a widget
 */
type WParallax.config = {
     display: WParallax.display
     width: int
     height: int
     global_style: WStyler.styler
}

WParallax = {{

/**
 *Default configuration of a widget
 */
  default_config : WParallax.config = {
    display = {both}
    width = 500
    height = 200
    global_style = WStyler.empty
  }

/**
 *Main display function of the parallax
 *
 *@param config The widget configuration
 *@param sources The elements configuration list
 *@return The HTML corresponding to the widget
 */
  html(config: WParallax.config, sources: list(WParallax.src)): xhtml =
    parallax = <div id=#parallax style="positon:absolute overflow:hidden" onready={_ -> configure(config,sources)}></div>
    WStyler.add(config.global_style,parallax)

/*
 *Function that calculates a dimension(width of height) of a container inside
 *the parallax, depending on its depth
 *
 *@param depth Depth of the element
 *@param dimension Dimension (width or height) of the parallax
 *@return The calculated dimension
 */
  @private
  get_dimension(depth: int, dimension: int): int=
    d = if depth>0
        then float_of_int(depth)
        else float_of_int((-1)*depth)
    dim = float_of_int(dimension)
    percent = dim / 20.
    adjustment = d * percent
    int_of_float(dim + adjustment)

/*
 *Sorts the elements depending on their depth
 *
 *@param sources The elements configuration list
 *@return The sorted list
 */
  @private
  sort_sources(sources: list(WParallax.src)): list(WParallax.src) =
    rec division(list) = (match list with
      |[] -> ([],[])
      |[x] -> ([x],[])
      |[x|[y|t]] -> (l1,l2)=division(t)
                     ([x|l1],[y|l2])
    )
    rec fusion(l1,l2) = (match (l1,l2) with
      |(x,[]) -> x
      |([],x) -> x
      |([x|t1],[y|t2]) -> if (x.depth)<(y.depth)
                        then [y|[x|fusion(t1,t2)]]
                        else [x|[y|fusion(t1,t2)]]
    )
    match sources with
    |[] -> []
    |[x] -> [x]
    |l -> (l1,l2)=division(l)
          fusion(sort_sources(l1),sort_sources(l2))

/*
 *Function that gives the dimensions of a container inside the parallax,
 *depending on its depth and the config (horizontal, vertical or both)
 *
 *@param config The widget configuration
 *@param source The element configuration
 *@return The calculated dimensions
 */
  @private
  get_container_dimensions(config: WParallax.config, source: WParallax.src): WParallax.dim =
    (w,h) = match config.display with
      |{horizontal} -> (get_dimension(source.depth,config.width), config.height)
      |{vertical} -> (config.width, get_dimension(source.depth,config.height))
      | _ -> (get_dimension(source.depth,config.width),get_dimension(source.depth,config.height))
    {px_x=w;px_y=h}

/*
 *Function that calculates the margins of an element of the parallax
 *
 *@param config The widget configuration
 *@param source The element configuration
 *@return The margins of the element
 */
  @private
  get_margin(config: WParallax.config, source: WParallax.src): WParallax.dim=
    match source.offset with
      |~{px} -> px
      |~{percent} -> offset_x = (percent.x / 2.) + 50.
                     offset_y = (percent.y / 2.) + 50.
                     dimensions = get_container_dimensions(config, source)
                     w = float_of_int(dimensions.px_x)
                     h = float_of_int(dimensions.px_y)
                     margin_left = int_of_float((w * offset_x) / 100.)
                     margin_top = int_of_float((h * offset_y) / 100.)
                     {px_x=margin_left;px_y=margin_top}

/*
 *Give the adjustment for an element
 *
 *@param pos The position of the mouse on the page
 *@param off The offset of the parallax on the page
 *@param dim The dimension of the parallax
 *@return The adjustment (in percent)
 */
  @private
  get_adjustment(pos: int, off: int, dim: int): float =
    p = float_of_int(pos)
    o = float_of_int(off)
    d = float_of_int(dim)
    offset = p - o
    half = d / 2.
    (offset - half) / half

/*
 *Add the style to an element of the parallax
 *
 *@param config The widget configuration
 *@param source The element configuration
 */
  @private
  configure_content(config: WParallax.config, source: WParallax.src): void =
    margin = get_margin(config, source)
    top = margin.px_y
    left = margin.px_x
    padding={t={none};
    l={none};
    b={none};
    r={none}}
    margin={t={some={px=top}};
    l={some={px=left}};
    b={none};
    r={none}}
    style = [{position={absolute}},{padding=padding},{margin=margin}]
    dom_src = Dom.select_id("{source.id}_content")
    Dom.set_style(dom_src,style)

/*
 *Add the style to the container of an element of the parallax
 *
 *@param config The widget configuration
 *@param source The element configuration
 */
  @private
  configure_container(config: WParallax.config, source: WParallax.src): void =
    dimensions = get_container_dimensions(config, source)
    w = dimensions.px_x
    h = dimensions.px_y
    margin_top = (config.height - h)/2
    margin_left = (config.width - w)/2
    margin={t={some={px=margin_top}};
    l={some={px=margin_left}};
    b={none};
    r={none}}
    style = [{position={absolute}},{width={px=w}},{height={px=h}},{margin=margin}]
    dom_container = Dom.select_id(source.id)
    Dom.set_style(dom_container,style)

/*
 *Add the style to the parallax and the HTML of the elements
 *
 *@param config The widget configuration
 *@param sources The elements configuration list
 */
  @private
  configure(config: WParallax.config, sources: list(WParallax.src)): void =
    list_sources = sort_sources(sources)
    dom_parallax = Dom.select_id("parallax")
    do Dom.set_style(dom_parallax,[{position={relative}},{overflow={hidden}},{width={px=config.width}},{height={px=config.height}}])
    do Dom.transform([#parallax <- get_content(config,list_sources)])
    _ = Dom.bind(dom_parallax,{mousemove},handle(config,list_sources,_))
    void


/*
 *Function that generates the HTML correspondig to a elements list
 *
 *@param config The widget configuration
 *@param sources The elements configuration list
 *@return The HTML corresponding to the elements list
 */
  @private
  get_content(config: WParallax.config, sources: list(WParallax.src)): xhtml=
    add_src(source)=
      src = match source.content with
        |~{img} -> <img id="{source.id}_content" src={img} alt="" onready={_->configure_content(config,source)}/>
        |~{text} -> <div id="{source.id}_content" onready={_->configure_content(config,source)}>{text}</div>
      src = WStyler.add(source.content_style,src)
      <div id={source.id} onready={_->configure_container(config,source)}>
      {src}
      </div>
    list_src = List.map(add_src,sources)
    <>{list_src}</>

/*
 *Function that applies the changes to the elements of the parallax
 *
 *@param config The widget configuration
 *@param sources The elements configuration list
 *@param event The event information
 */
  @private
  handle(config: WParallax.config, sources: list(WParallax.src), event: Dom.event): void =
    dimensions = event.mouse_position_on_page
    offset = Dom.get_offset(Dom.select_id("parallax"))
    percent_x = get_adjustment(dimensions.x_px,offset.x_px,config.width)
    percent_y = get_adjustment(dimensions.y_px,offset.y_px,config.height)
    adjust(source) = (
      (w,h) = match config.display with
        |{horizontal} -> (get_dimension(source.depth,config.width), config.height)
        |{vertical} -> (config.width, get_dimension(source.depth,config.height))
        | _ -> (get_dimension(source.depth,config.width),get_dimension(source.depth,config.height))
      margin_top = float_of_int(config.height - h) / 2.
      margin_left = float_of_int(config.width - w) / 2.
      adj_x = int_of_float(percent_x * margin_left)
      adj_y = int_of_float(percent_y * margin_top)
      src = Dom.select_id("{source.id}")
      style = match config.display with
        |{horizontal} -> if source.depth>0
                         then [{left={px=adj_x}}]
                         else (
                              if source.depth<0
                              then [{left={px=-adj_x}}]
                              else []
                              )
        |{vertical} -> if source.depth>0
                       then [{top={px=adj_y}}]
                       else (
                            if source.depth<0
                            then [{top={px=-adj_y}}]
                            else []
                            )
        |_ -> if source.depth>0
              then [{left={px=adj_x}},{top={px=adj_y}}]
              else (
                   if source.depth<0
                   then [{left={px=-adj_x}},{top={px=-adj_y}}]
                   else []
                   )
      Dom.set_style(src,style)
    )
    List.iter(adjust,sources)


}}
