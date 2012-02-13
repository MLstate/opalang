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
 * Low-level interaction with the user-interface
 *
 * @author Hugo Heuzard, 2011
 * @target PUBLIC
 * @stability EXPERIMENTAL
 */

import stdlib.core.{web.core}
import-plugin browser_canvas

/**
 * {1 About this module}
 *
 * This module defines client side manipulation of canvas element (html5).
 *
 * {1 where should i start?}
 *
 * {1 what if i need more?}
 */

/**
 * {1 Types defined in this module}
 */

@abstract
type Canvas.context = canvas_element
type canvas_element = external
type Canvas.canvas = external
type Canvas.gradient = external
type Canvas.pattern= external
type Canvas.style =
  {color: color}
/ {gradient : Canvas.gradient}
/ {pattern : Canvas.pattern}
/ {unsafe: string}

type Canvas.linecap =
  {butt}
/ {round}
/ {square}

type Canvas.linejoin =
  {bevel}
/ {round}
/ {miter}

type Canvas.imagedata =
  {width:int
   height:int
   }

type Canvas.image =
  {image : Image.image}
/ {canvas : Canvas.canvas}
/ {video : Video.video}

type Canvas.repeat =
  {repeat}
/ {repeat_x}
/ {repeat_y}
/ {no_repeat}

type Image.image = external
type Video.video = external
type Image.data = external

type Canvas.textalign = 
  {align_start}
/ {align_end}
/ {align_left}
/ {align_right}
/ {align_center}

type Canvas.textbaseline = 
  {top}
/ {hanging}
/ {middle}
/ {alphabetic}
/ {ideographic}
/ {bottom}

/**
 * {1 Interface}
 */

@client
Canvas = {{

  /**
   * {2 Canvas}
  **/

  create_with(id : string, width : Css.size, height : Css.size,
              error_msg : xhtml) : xhtml =
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
    <canvas id="{id}"
            width="{get_size(width)}"
            height="{get_size(height)}"
            style="width:{width};height:{height}">
      {error_msg}
    </canvas>

  create(id : string, width: Css.size, height : Css.size) : xhtml =
    err_msg =
      <p>
        You cannot see this image because your browser is not fully
        compatible with the latest web standard, HTML5.
        Please consider upgrading or switching to a more modern browser.
      </p> ;
     create_with(id, width, height, err_msg)

  get(dom : dom) : option(Canvas.canvas) =
    %% BslCanvas.get %%(Dom.of_selection(dom))

  to_data_url(canvas : Canvas.canvas, t : string) : option(string) =
    %% BslCanvas.to_data_url %%(canvas, t)

  to_data_url_png(canvas :  Canvas.canvas) : option(string) =
    to_data_url(canvas, "image/png")

  /**
   * {2 Context}
  **/

  get_context(canvas : Canvas.canvas, t : string) : option(Canvas.context) =
    %% BslCanvas.get_context %%(canvas, t)

  get_context_2d(canvas : Canvas.canvas) : option(Canvas.context) =
    get_context(canvas,"2d")

  /**
   * {2 Canvas state}
  **/

  save(context : Canvas.context) : void =
    %% BslCanvas.save %%(context)

  restore(context : Canvas.context) : void =
    %% BslCanvas.restore %%(context)


  /**
   * {2 Transformations}
  **/

  /**
   * The scale(x, y) method add the scaling transformation described by the arguments to the transformation matrix.
   * The x argument represents the scale factor in the horizontal direction and the y argument represents the scale
   * factor in the vertical direction.
   */
  scale(context : Canvas.context, x : float, y : float) : void =
    %% BslCanvas.scale %%(context, x, y)

  /**
   * The rotate(angle) method add the rotation transformation described by the argument to the transformation matrix.
   * The angle argument represents a clockwise rotation angle expressed in radians.
   */
  rotate(context : Canvas.context, r : float) : void =
    %% BslCanvas.rotate %%(context, r)

  /**
   * The translate(x, y) method add the translation transformation described by the arguments to the transformation matrix.
   * The x argument represents the translation distance in the horizontal direction and the y argument represents the translation
   * distance in the vertical direction. The arguments are in coordinate space units.
   */
  translate(context : Canvas.context, x : int, y : int) : void =
    %% BslCanvas.translate %%(context, x, y)

  /**
   * The transform(a, b, c, d, e, f) method replace the current transformation matrix with the result of
   *  multiplying the current transformation matrix with the matrix described by: [a,c,e|b,d,f|0,0,1]
   */
  transform(context : Canvas.context, a : int, b : int, c : int, d : int, e : int, f : int) : void =
    %% BslCanvas.transform %%(context, a, b, c, d, e, f)

  /**
   * The setTransform(a, b, c, d, e, f) method must reset the current transform to the identity matrix,
   * and then invoke the transform(a, b, c, d, e, f) method with the same arguments.
   */
  set_transform(context : Canvas.context, a : int, b : int, c : int, d : int, e : int, f : int) : void =
    %% BslCanvas.set_transform %%(context, a, b, c, d, e, f)


  /**
   * {2 Compositing}
  **/

  set_global_alpha(context : Canvas.context, alpha : float) : void =
    %% BslCanvas.set_global_alpha %%(context, alpha)

  get_global_alpha(context : Canvas.context) : float =
    %% BslCanvas.get_global_alpha %%(context)

  //Todo globalCompositeOperation

  /**
   * {2 Colors and Styles}
  **/

  set_fill_style(context : Canvas.context, style : Canvas.style) : void =
    match style with
      | {~color} -> %% BslCanvas.set_fill_style_color %%(context, Color.color_to_string(color))
      | {~unsafe} -> %% BslCanvas.set_fill_style_color %%(context, unsafe)
      | {~gradient} -> %% BslCanvas.set_fill_style_gradient %%(context, gradient)
      | {~pattern} -> %% BslCanvas.set_fill_style_pattern %%(context, pattern)

  get_fill_style(context : Canvas.context) : Canvas.style =
    match %% BslCanvas.get_fill_style %%(context) with
      {~unsafe} -> (match Color.of_string(unsafe) with
                     | {~some} -> {color=some}
                     | {none} -> {~unsafe})
      x -> x

  set_stroke_style(context : Canvas.context, style : Canvas.style) : void =
    match style with
      | {~color} -> %% BslCanvas.set_stroke_style_color %%(context, Color.color_to_string(color))
      | {~unsafe} -> %% BslCanvas.set_fill_style_color %%(context, unsafe)
      | {~gradient} -> %% BslCanvas.set_stroke_style_gradient %%(context, gradient)
      | {~pattern} -> %% BslCanvas.set_stroke_style_pattern %%(context, pattern)

  get_stroke_style(context : Canvas.context) : Canvas.style =
    match %% BslCanvas.get_stroke_style %%(context) with
      {~unsafe} -> (match Color.of_string(unsafe) with
                     | {~some} -> {color=some}
                     | {none} -> {~unsafe})
      x -> x

  /**
   * {2 Gradient}
  **/

  create_linear_gradient(context : Canvas.context, x0 : int, y0 : int, x1 : int, y1 :int ) : Canvas.gradient =
    %% BslCanvas.create_linear_gradient %%(context,x0,y0,x1,y1)

  create_radial_gradient(context : Canvas.context, x0 : int, y0 : int, r0 : int, x1 : int, y1 : int, r1 : int) : Canvas.gradient =
    %% BslCanvas.create_radial_gradient %%(context,x0,y0,r0,x1,y1,r1)

  add_color_stop(gradient : Canvas.gradient, offset : float, color : color ) : void =
    %% BslCanvas.add_color_stop %%(gradient,offset,Color.color_to_string(color))


  /**
   * {2 Pattern}
  **/

   create_pattern(context : Canvas.context, image : Canvas.image, repeat : Canvas.repeat) : Canvas.pattern =
     repeat_to_string(r) =
       match r with
         | {repeat} -> "repeat"
         | {repeat_x} -> "repeat-x"
         | {repeat_y} -> "repeat-y"
         | {no_repeat} -> "no-repeat"
       end
     r = repeat_to_string(repeat)
     match image with
       | {~image} -> %% BslCanvas.create_pattern_i %%(context,image,r)
       | {~video} -> %% BslCanvas.create_pattern_v %%(context,video,r)
       | {~canvas} -> %% BslCanvas.create_pattern_c %%(context,canvas,r)
     end

  /**
   * {2 Line width / caps / joins}
  **/

  set_line_width(context : Canvas.context, size : float) : void =
    %% BslCanvas.set_line_width %%(context, size)

  get_line_width(context : Canvas.context) : float =
    %% BslCanvas.get_line_width %%(context)

  set_line_cap(context : Canvas.context, cap : Canvas.linecap ) : void =
    s = match cap with
          | {butt} -> "butt"
          | {round} -> "round"
          | {square} -> "square"
        end
    %% BslCanvas.set_line_cap %%(context, s)
  get_line_cap(context : Canvas.context) : Canvas.linecap =
    match %% BslCanvas.get_line_cap %%(context) with
      | "butt" -> {butt}
      | "round" -> {round}
      | "square" -> {square}
      | _ -> error("Other value should not be returned as it should be ignored by the browser")
    end

  set_line_join(context : Canvas.context, join : Canvas.linejoin ) : void =
    s = match join with
          | {bevel} -> "bevel"
          | {round} -> "round"
          | {miter} -> "miter"
        end
    %% BslCanvas.set_line_join %%(context, s)
  get_line_join(context : Canvas.context) : Canvas.linejoin =
    match %% BslCanvas.get_line_join %%(context) with
      | "bevel" -> {bevel}
      | "round" -> {round}
      | "miter" -> {miter}
      | _ -> error("Other value should not be returned as it should be ignored by the browser")
    end

  set_miter_limit(context : Canvas.context, limit : float) : void =
    %% BslCanvas.set_miter_limit %%(context, limit)

  get_miter_limit(context : Canvas.context) : float =
    %% BslCanvas.get_miter_limit %%(context)

  /**
   * {2 Shadows}
  **/

  set_shadow_color(context : Canvas.context, color : color) : void =
    %% BslCanvas.set_shadow_color %%(context,Color.color_to_string(color))

  get_shadow_color(context : Canvas.context) : color =
    match Color.of_string(%% BslCanvas.get_shadow_color %%(context)) with
      | {~some} -> some
      | {none} -> error("should not append")
    end

  set_shadow_offset_x(context : Canvas.context, x : int) : void =
    %% BslCanvas.set_shadow_offset_x %%(context,x)

  get_shadow_offset_x(context : Canvas.context) : int =
    %% BslCanvas.get_shadow_offset_x %%(context)

  set_shadow_offset_y(context : Canvas.context, y : int) : void =
    %% BslCanvas.set_shadow_offset_y %%(context,y)

  get_shadow_offset_y(context : Canvas.context) : int =
    %% BslCanvas.get_shadow_offset_y %%(context)

  set_shadow_offset(context : Canvas.context, x : int, y : int) : void =
    do set_shadow_offset_x(context, x)
    do set_shadow_offset_y(context, y)
    void

  get_shadow_offset(context : Canvas.context) : (int,int) =
    (get_shadow_offset_x(context),get_shadow_offset_y(context))

  set_shadow_blur(context : Canvas.context, blur : int) : void =
    %% BslCanvas.set_shadow_blur %%(context, blur)

  get_shadow_blur(context : Canvas.context) : int =
    %% BslCanvas.get_shadow_blur %%(context)

  /**
   * {2 Simple shapes}
  **/

  clear_rect(context : Canvas.context, x : int, y : int, w : int, h : int) : void =
    %% BslCanvas.clear_rect %%(context,x,y,w,h)

  fill_rect(context : Canvas.context, x : int, y : int, w : int, h : int) : void =
    %% BslCanvas.fill_rect %%(context,x,y,w,h)

  stroke_rect(context : Canvas.context, x : int, y : int, w : int, h : int) : void =
    %% BslCanvas.stroke_rect %%(context,x,y,w,h)


  /**
   * {2 Complex shapes}
  **/

  begin_path(context : Canvas.context) : void =
    %% BslCanvas.begin_path %%(context)

  close_path(context : Canvas.context) : void =
    %% BslCanvas.close_path %%(context)

  move_to(context : Canvas.context, x : int, y : int) : void =
    %% BslCanvas.move_to %%(context, x, y)

  line_to(context : Canvas.context, x : int, y : int) : void =
    %% BslCanvas.line_to %%(context, x, y)

  quadratic_curve_to(context : Canvas.context, cpx : int, cpy : int, x : int, y : int) : void =
    %% BslCanvas.quadratic_curve_to %%(context,cpx,cpy,x,y)

  bezier_curve_to(context : Canvas.context, cp1x : int, cp1y : int, cp2x : int, cp2y : int, x : int, y : int) : void =
    %% BslCanvas.bezier_curve_to %%(context,cp1x,cp1y,cp2x,cp2y,x,y)

  arc_to(context : Canvas.context, x1 : int, y1 : int, x2 : int, y2 : int, r : int) : void =
    %% BslCanvas.arc_to %%(context,x1,y1,x2,y2,r)

  /**
   * arc
   * @param context : Canvas.context
   * @param centerX : int 
   * @param centerY : int
   * @param radius : int
   * @param startingAngle : float
   * @param endinAngle : float
   * @param counterclockwise : bool
   */
  arc(context : Canvas.context, centerX : int, centerY : int, radius : int, startingAngle : float, endingAngle : float, counterclockwise : bool) : void =
    %% BslCanvas.arc %%(context, centerX, centerY, radius, startingAngle, endingAngle, counterclockwise)

  rect(context : Canvas.context, x : int, y : int, w : int, h : int) : void =
    %% BslCanvas.rect %%(context,x,y,w,h)

  fill(context : Canvas.context) : void =
    %% BslCanvas.fill %%(context)

  stroke(context : Canvas.context) : void =
    %% BslCanvas.stroke %%(context)

  clip(context : Canvas.context) : void =
    %% BslCanvas.clip %%(context)

  is_point_in_path(context : Canvas.context, x : int, y : int) : bool =
    %% BslCanvas.is_point_in_path %%(context,x,y)

  /**
   * {2 Focus management}
  **/

  // TODO

  /**
   * {2 Text}
  **/

  // TODO: type font
  set_font(context:Canvas.context, font:string) : void =
    %% BslCanvas.set_font %%(context,font)

  set_text_align(context:Canvas.context, align:Canvas.textalign) : void =
    align_text = match align with
      | {align_start}  -> "start"
      | {align_end}    -> "end"
      | {align_left}   -> "left"
      | {align_right}  -> "right"
      | {align_center} -> "center"
    %% BslCanvas.set_text_align %%(context,align_text)

  set_text_baseline(context:Canvas.context, baseline:Canvas.textbaseline) : void =
    baseline_text = match baseline with
      | {top}         -> "top"
      | {hanging}     -> "hanging"
      | {middle}      -> "middle"
      | {alphabetic}  -> "alphabetic"
      | {ideographic} -> "ideographic"
      | {bottom}      -> "bottom"
    %% BslCanvas.set_text_baseline %%(context,baseline_text)

  stroke_text(context:Canvas.context, text:string, x:int, y:int) : void =
    %% BslCanvas.stroke_text %%(context,text,x,y)

  fill_text(context:Canvas.context, text:string, x:int, y:int) : void =
    %% BslCanvas.fill_text %%(context,text,x,y)

  measure_text(context:Canvas.context, text:string) : int =
    %% BslCanvas.measure_text %%(context,text)

  /**
   * {2 Drawing images}
  **/

   draw_image(context : Canvas.context, image : Canvas.image, x: int, y : int) : void =
     match image with
       | {image=image} -> %% BslCanvas.draw_image_i %%(context, image, x, y)
       | {video=image} -> %% BslCanvas.draw_image_v %%(context, image, x, y)
       | {canvas=image} -> %% BslCanvas.draw_image_c %%(context, image, x, y)
     end
   draw_image_with_dimensions(context : Canvas.context, image : Canvas.image, x: int, y : int, w : int, h : int) : void =
     match image with
       | {image=image} -> %% BslCanvas.draw_image_di %%(context, image, x, y, w, h)
       | {video=image} -> %% BslCanvas.draw_image_dv %%(context, image, x, y, w, h)
       | {canvas=image} -> %% BslCanvas.draw_image_dc %%(context, image, x, y, w, h)
     end

   draw_image_full(context : Canvas.context,
                  image : Canvas.image,
                  sx: int, sy : int, sw : int, sh : int,
                  dx : int , dy : int, dw : int, dh : int) : void =
     match image with
       | {image=image} -> %% BslCanvas.draw_image_fi %%(context, image, sx, sy, sw, sh, dx, dy, dw, dh)
       | {video=image} -> %% BslCanvas.draw_image_fv %%(context, image, sx, sy, sw, sh, dx, dy, dw, dh)
       | {canvas=image} -> %% BslCanvas.draw_image_fc %%(context, image, sx, sy, sw, sh, dx, dy, dw, dh)
     end

  /**
   * {2 Pixel manipulation}
  **/

  // TODO
  // put_image_data(context : Canvas.context, data : Image.data, x : int, y : int) : void =
  //   %% BslCanvas.put_image_data %%(context, data, x, y)


  /**
   * {2 Other}
  **/

  // create_image(data : string) : Canvas.image =
  //   {image= %% BslCanvas.create_image %%(data) }

  get_image(dom : dom) : option(Canvas.image) =
    Option.map(image -> {~image},%% BslCanvas.get_image %%(Dom.of_selection(dom)))

}}
