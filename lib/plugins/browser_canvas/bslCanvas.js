/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/** @externType Dom.private.element */
/** @externType Canvas.context */
/** @externType Canvas.pattern */
/** @externType Canvas.gradient */
/** @externType Image.data */
/** @externType Image.image */
/** @externType Canvas.canvas */
/** @externType Video.video */
/** @opaType Canvas.style */

// Canvas

/**
 * @register {Dom.private.element -> opa[option(Canvas.canvas)]}
 */
function get(canvas) {
  if(canvas && canvas[0] && canvas[0].getContext) {
    return js_some(canvas[0]);
  }
  return js_none;
}

/**
 * @register {Canvas.canvas, string -> opa[option(string)]}
 */
function to_data_url(canvas, name) {
    if(canvas.toDataURL && canvas.toDataURL(name))
    {
        return js_some(canvas.toDataURL(name))
    }
    return js_none
}

/**
 * @register {Canvas.canvas -> int}
 */
function get_width(canvas) {
    return canvas.width;
}

/**
 * @register {Canvas.canvas -> int}
 */
function get_height(canvas) {
    return canvas.height;
}

// Context

/**
 * @register {Canvas.canvas, string -> opa[option(Canvas.context)]}
 */
function get_context(canvas, name) {
    if(canvas.getContext && canvas.getContext(name))
    {
        return js_some(canvas.getContext(name))
    }
    return js_none
}

// State
/**
 * @register {Canvas.context -> void}
 */
function save(context) {
    context.save()
}

/**
 * @register {Canvas.context -> void}
 */
function restore(context) {
    context.restore()
}



  // transformations (default transform is the identity matrix)
/**
 * @register {Canvas.context, float, float -> void}
 */
function scale(context, x, y) {
    context.scale(x, y)
}

/**
 * @register {Canvas.context, float -> void}
 */
function rotate(context, x) {
    context.rotate(x)
}

/**
 * @register {Canvas.context, int, int -> void}
 */
function translate(context, x, y) {
    context.translate(x,y)
}


/**
 * @register {Canvas.context, int, int, int, int, int, int -> void}
 */
function transform(context, a, b, c, d, e, f) {
    context.transform(a, b, c, d, e, f)
}

/**
 * @register {Canvas.context, int, int, int, int, int, int -> void}
 */
function set_transform(context, a, b, c, d, e, f) {
    context.setTransform(a, b, c, d, e, f)
}

// compositing
/**
 * @register {Canvas.context, float -> void}
 */
function set_global_alpha(context, a) {
    context.globalAlpha = a
}

/**
 * @register {Canvas.context -> float}
 */
function get_global_alpha(context) {
    return context.globalAlpha
}

// colors and styles

/**
 * @register {Canvas.context, string -> void}
 */
function set_stroke_style_color(context, c) {
    context.strokeStyle = c
}

/**
 * @register {Canvas.context, Canvas.gradient -> void}
 */
function set_stroke_style_gradient(context, c) {
    context.strokeStyle = c
}

/**
 * @register {Canvas.context, Canvas.pattern -> void}
 */
function set_stroke_style_pattern(context, c) {
    context.strokeStyle = c
}

/**
 * @register {Canvas.context, string -> void}
 */
function set_fill_style_color(context, c) {
    context.fillStyle = c
}

/**
 * @register {Canvas.context, Canvas.gradient -> void}
 */
function set_fill_style_gradient(context, c) {
    context.fillStyle = c
}

/**
 * @register {Canvas.context, Canvas.pattern -> void}
 */
function set_fill_style_pattern(context, c) {
    context.fillStyle = c
}

function canvas_get_style_getter(c){
    if(c instanceof CanvasGradient){
        var cmsg = empty_constructor()
        cmsg = add_field(cmsg,static_field_of_name('gradient'),c)
        cmsg = make_record(cmsg)
    }
    else if(c instanceof CanvasPattern){
        var cmsg = empty_constructor()
        cmsg = add_field(cmsg,static_field_of_name('pattern'),c)
        cmsg = make_record(cmsg)
    }
    else if((c instanceof String) || (typeof(c) == "string")){
        var cmsg = empty_constructor()
        cmsg = add_field(cmsg,static_field_of_name('unsafe'),c)
        cmsg = make_record(cmsg)
    }
    else{
        var cmsg = empty_constructor()
        cmsg = add_field(cmsg,static_field_of_name('unsafe'),c)
        cmsg = make_record(cmsg)
    }
    return cmsg
}


/**
 * @register {Canvas.context -> opa[Canvas.style]}
 */
function get_stroke_style(context) {
    return canvas_get_style_getter(context.strokeStyle)
}

/**
 * @register {Canvas.context -> opa[Canvas.style]}
 */
function get_fill_style(context) {
    return canvas_get_style_getter(context.fillStyle)
}

//Gradient

/**
 * @register {Canvas.gradient, float, string -> void}
 */
function add_color_stop(gradient,offset,color) {
    gradient.addColorStop(offset, color)
}

/**
 * @register {Canvas.context, int, int, int, int -> Canvas.gradient}
 */
function create_linear_gradient(context,x0,y0,x1,y1) {
    return context.createLinearGradient(x0, y0, x1, y1)
}

/**
 * @register {Canvas.context, int, int, int, int, int, int -> Canvas.gradient}
 */
function create_radial_gradient (context,x0,y0,r0,x1,y1,r1) {
    return context.createRadialGradient(x0, y0, r0, x1, y1, r1)
}

//pattern

/**
 * @register {Canvas.context, Image.image, string -> Canvas.pattern} create_pattern_i bslcanvas_create_pattern
 */
/**
 * @register {Canvas.context, Video.video, string -> Canvas.pattern} create_pattern_v bslcanvas_create_pattern
 */
/**
 * @register {Canvas.context, Canvas.canvas, string -> Canvas.pattern} create_pattern_c bslcanvas_create_pattern
 */
function bslcanvas_create_pattern(context, image, repeat)
{
    context.createPattern(image, repeat)
}

// line caps/joins
/**
 * @register {Canvas.context -> float}
 */
function get_line_width(context) {
    return context.lineWidth
}

/**
 * @register {Canvas.context, float -> void}
 */
function set_line_width(context,size) {

    context.lineWidth = size
}

/**
 * @register {Canvas.context, string -> void}
 */
function set_line_cap(context, cap) {
    context.lineCap = cap
}

/**
 * @register {Canvas.context -> string}
 */
function get_line_cap(context) {
    return context.lineCap
}

/**
 * @register {Canvas.context, string -> void}
 */
function set_line_join(context, join) {
    context.lineJoin = join
}

/**
 * @register {Canvas.context -> string}
 */
function get_line_join(context) {
    return context.lineJoin
}

/**
 * @register {Canvas.context, float -> void}
 */
function set_miter_limit(context, limit) {
    context.miterLimit = limit
}

/**
 * @register {Canvas.context -> float}
 */
function get_miter_limit(context) {
    return context.miterLimit
}
// shadows

/**
 * @register {Canvas.context, string -> void}
 */
function set_shadow_color(context,color) {
    context.shadowColor=color
}

/**
 * @register {Canvas.context ->  string}
 */
function get_shadow_color(context) {
    return context.shadowColor
}

/**
 * @register {Canvas.context, int -> void}
 */
function set_shadow_offset_x(context,offset) {
    context.shadowOffsetX=offset
}

/**
 * @register {Canvas.context ->  int}
 */
function get_shadow_offset_x(context) {
    return context.shadowOffsetX
}

/**
 * @register {Canvas.context, int -> void}
 */
function set_shadow_offset_y(context,offset) {
    context.shadowOffsetY=offset
}

/**
 * @register {Canvas.context -> int}
 */
function get_shadow_offset_y(context) {
    return context.shadowOffsetY
}

/**
 * @register {Canvas.context, int -> void}
 */
function set_shadow_blur(context, blur) {
    context.shadowBlur=blur
}

/**
 * @register {Canvas.context -> int}
 */
function get_shadow_blur(context) {
    return context.shadowBlur
}

// rects
/**
 * @register {Canvas.context, int ,int, int, int -> void}
 */
function clear_rect(context,x,y,w,h) {
    context.clearRect(x, y, w, h)
}

/**
 * @register {Canvas.context, int ,int, int, int -> void}
 */
function fill_rect(context,x,y,w,h) {
    context.fillRect(x, y, w, h)
}

/**
 * @register {Canvas.context, int ,int, int, int -> void}
 */
function stroke_rect(context,x,y,w,h) {
    context.strokeRect(x, y, w, h)
}

// path API
/**
 * @register {Canvas.context -> void}
 */
function begin_path(context) {
    context.beginPath()
}


/**
 * @register {Canvas.context -> void}
 */
function close_path(context) {
    context.closePath()
}

/**
 * @register {Canvas.context, int, int -> void}
 */
function move_to(context, x, y) {
    context.moveTo(x, y)
}

/**
 * @register {Canvas.context, int, int -> void}
 */
function line_to(context, x, y) {
    context.lineTo(x, y)
}

/**
 * @register {Canvas.context, int, int, int, int -> void}
 */
function quadratic_curve_to(context, cpx, cpy, x, y) {
    context.quadraticCurveTo(cpx, cpy, x, y)
}

/**
 * @register {Canvas.context, int, int, int, int, int, int -> void}
 */
function bezier_curve_to(context, cp1x, cp1y, cp2x, cp2y, x, y) {
    context.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y)
}

/**
 * @register {Canvas.context, int, int, int, int, int -> void}
 */
function arc_to(context, x1, y1, x2, y2, radius) {
    context.arcTo( x1, y1, x2, y2, radius)
}

/**
 * @register {Canvas.context, int, int, int, float, float, bool -> void}
 */
function arc(context, centerX, centerY, radius, startingAngle, endingAngle, counterclockwise) {
    context.arc(centerX, centerY, radius, startingAngle,
        endingAngle, counterclockwise);
}

/**
 * @register {Canvas.context, int, int, int, int -> void}
 */
function rect(context, x, y, w, h) {
    context.rect( x, y, w, h)
}

// ##register arc: Canvas.context, int, int, int, int -> void
// ##args(context, x, y, w, h)
// {
//     context.rect( x, y, w, h)
// }

/**
 * @register {Canvas.context -> void}
 */
function fill(context) {
    context.fill()
}

/**
 * @register {Canvas.context -> void}
 */
function stroke(context) {
    context.stroke()
}

/**
 * @register {Canvas.context -> void}
 */
function clip(context) {
    context.clip()
}

/**
 * @register {Canvas.context, int, int -> bool}
 */
function is_point_in_path (context, x, y) {
    return context.isPointInPath(x, y)
}

// focus management
//todo

// text

/**
 * @register {Canvas.context, string -> void}
 */
function set_font(context, fontText) {
    context.font = fontText
}

/**
 * @register {Canvas.context, string -> void}
 */
function set_text_align(context, alignText) {
    context.textAlign = alignText
}

/**
 * @register {Canvas.context, string -> void}
 */
function set_text_baseline(context, baselineText) {
    context.textBaseline = baselineText
}

/**
 * @register {Canvas.context, string, int, int -> void}
 */
function stroke_text(context, text, x, y) {
    context.strokeText(text, x, y)
}

/**
 * @register {Canvas.context, string, int, int -> void}
 */
function fill_text(context, text, x, y) {
    context.fillText(text, x, y)
}

/**
 * @register {Canvas.context, string -> int}
 */
function measure_text(context, text) {
    return context.measureText(text).width
}

// drawing images

/**
 * @register {string -> Image.image}
 */
function create_image (data) {
  var img = new Image();
  img.src = data;
  return img
}

/**
 * @register {Canvas.context, Image.image, int, int -> void} draw_image_i bslcanvas_draw_image
 */
/**
 * @register {Canvas.context, Canvas.canvas, int, int -> void} draw_image_c bslcanvas_draw_image
 */
/**
 * @register {Canvas.context, Video.video, int, int -> void} draw_image_v bslcanvas_draw_image
 */
function bslcanvas_draw_image(context, image, x, y)
{
    return context.drawImage(image, x ,y)
}

/**
 * @register {Canvas.context, Image.image, int, int, int, int -> void} draw_image_di bslcanvas_draw_image_d
 */
/**
 * @register {Canvas.context, Canvas.canvas, int, int, int, int -> void} draw_image_dc bslcanvas_draw_image_d
 */
/**
 * @register {Canvas.context, Video.video, int, int, int, int -> void} draw_image_dv bslcanvas_draw_image_d
 */
function bslcanvas_draw_image_d(context, image, x, y, w, h)
{
    return context.drawImage(image, x ,y, w ,h)
}

/**
 * @register {Canvas.context, Image.image, int, int, int, int, int, int, int, int -> void} draw_image_fi bslcanvas_draw_image_f
 */
/**
 * @register {Canvas.context, Canvas.canvas, int, int, int, int, int, int, int, int -> void} draw_image_fc bslcanvas_draw_image_f
 */
/**
 * @register {Canvas.context, Video.video, int, int, int, int, int, int, int, int -> void} draw_image_fv bslcanvas_draw_image_f
 */
function bslcanvas_draw_image_f(context, image, sx, sy, sw, sh, dx, dy, dw, dh)
{
    return context.drawImage(image, sx, sy, sw, sh, dx, dy, dw, dh)
}

// pixel manipulation

/**
 * @register {Canvas.context, Image.data, int, int -> void}
 */
function put_image_data(context, data, x, y) {
    context.putImageData(data, x ,y)
}

//other


/**
 * @register {Dom.private.element -> opa[option(Image.image)]}
 */
function get_image (dom) {
    if (dom && dom[0] && (dom[0].tagName.toLowerCase() == "img") && dom[0].complete){
        return js_some(dom[0])
    }
    return js_none
}
