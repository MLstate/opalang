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
 * Colors management
 */

package stdlib.color
import stdlib.core.parser

/**
 * {1 About this module}
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type color = Color.color

type Color.color = { r : int ; g : int ; b : int ; a : int}

type Color.color_hsv = {h:float s:float v:float}

/**
 * {1 Interface}
 */

Color = {{

  // from http://mjijackson.com/2008/02/rgb-to-hsl-and-rgb-to-hsv-color-model-conversion-algorithms-in-javascript
  to_hsv(c : Color.color) : Color.color_hsv =
    (r, g, b) = (Float.of_int(c.r), Float.of_int(c.g), Float.of_int(c.b));
    r = r/255.; g = g/255.; b = b/255.;
    max = max(r, max(g, b)); min = min(r, min(g, b));
    (h, _s, v) = (max, max, max);

    d = max - min;
    s = if (max == 0.) then 0. else d / max;

    h =
      if(max == min)
      then
        0. // achromatic
      else
        h = if (max == r)
            then (g - b) / d + (if (g < b) then  6. else 0.)
            else if (max == g)
            then (b - r) / d + 2.
            else if (max == b)
            then (r - g) / d + 4.
            else h
        h / 6.
    {~h ~s ~v} : Color.color_hsv;


  of_hsv({h=h s=s v=v} : Color.color_hsv) : Color.color =
    if ( s == 0. )                       //HSV from 0 to 1
    then
      vi = Int.of_float(v * 255.)
      {r=vi g=vi b=vi a= 255}
    else
      vh = h * 6.
      vh = if ( vh == 6. ) then 0. else vh      //H must be < 1
      vi =Float.floor( vh )             //Or ... var_i = floor( var_h )
      v1 = v * ( 1. - s )
      v2 = v * ( 1. - s * ( vh - vi ) )
      v3 = v * ( 1. - s * ( 1. - ( vh - vi ) ) )
      (r,g,b) = match Int.of_float(vi) with
        | 0 -> (v,v3,v1)
        | 1 -> (v2,v,v1)
        | 2 -> (v1,v,v3)
        | 3 -> (v1,v2,v)
        | 4 -> (v3,v1,v)
        | _ -> (v,v1,v2)
      end
      {r = Int.of_float(r * 255.)                 //RGB results from 0 to 255
       g = Int.of_float(g * 255.)
       b = Int.of_float(b * 255.)
       a = 255}


  @private
  color_value(x : int) : int =
    if x < 0 then 0
    else if x > 255 then 255
    else x

  color_to_string(c:color) =
    match c.a with
      | 0 -> "transparent"
      | 255 -> "rgb({color_value(c.r)},{color_value(c.g)},{color_value(c.b)})"
      | _ ->  "rgba({color_value(c.r)},{color_value(c.g)},{color_value(c.b)},{color_value(c.a)/255})"


  of_string : string -> option(color) =
    Parser.try_parse(color_parser, _)

   // FIXME: this duplicates functionality of opalang/syntax/css.trx

  color_parser = parser
    | r = color_hexa_parser -> r
    | r = color_rgb_parser -> r
    | r = color_name_parser -> r
  @private
  color_hexa_parser =
    hc1 = parser v1=Rule.hexadecimal -> v1 * 17 // hint: 16 + 1
    hc2 = parser v1=Rule.hexadecimal v2=Rule.hexadecimal -> v1*16 + v2
    parser
    | "#" r=hc2 g=hc2 b=hc2 a=hc2 -> rgba(r,g,b,a)
    | "#" r=hc2 g=hc2 b=hc2 -> rgb(r,g,b)
    | "#" r=hc1 g=hc1 b=hc1 a=hc1 -> rgba(r,g,b,a)
    | "#" r=hc1 g=hc1 b=hc1 -> rgb(r,g,b)
  @private
  color_rgb_parser = parser
    | "rgb(" Rule.ws r = Rule.byte Rule.ws "," Rule.ws g = Rule.byte Rule.ws "," Rule.ws b = Rule.byte Rule.ws ")" -> rgb(r,g,b)
    | "rgba(" Rule.ws r = Rule.byte Rule.ws "," Rule.ws g = Rule.byte Rule.ws "," Rule.ws b = Rule.byte Rule.ws "," Rule.ws a = Rule.float_0_1 Rule.ws ")" -> rgba(r,g,b,Float.to_int(a*255.))
  @private
  color_name_parser = parser
    | "yellowgreen" -> yellowgreen
    | "yellow" -> yellow
    | "whitesmoke" -> whitesmoke
    | "white" -> white
    | "wheat" -> wheat
    | "violet" -> violet
    | "turquoise" -> turquoise
    | "transparent" -> transparent
    | "tomato" -> tomato
    | "thistle" -> thistle
    | "teal" -> teal
    | "tan" -> tan
    | "steelblue" -> steelblue
    | "springgreen" -> springgreen
    | "snow" -> snow
    | "slategray" -> slategray
    | "slateblue" -> slateblue
    | "skyblue" -> skyblue
    | "silver" -> silver
    | "sienna" -> sienna
    | "seashell" -> seashell
    | "seagreen" -> seagreen
    | "sandybrown" -> sandybrown
    | "salmon" -> salmon
    | "saddlebrown" -> saddlebrown
    | "royalblue" -> royalblue
    | "rosybrown" -> rosybrown
    | "red" -> red
    | "purple" -> purple
    | "powderblue" -> powderblue
    | "plum" -> plum
    | "pink" -> pink
    | "peru" -> peru
    | "peachpuff" -> peachpuff
    | "papayawhip" -> papayawhip
    | "palevioletred" -> palevioletred
    | "paleturquoise" -> paleturquoise
    | "palegreen" -> palegreen
    | "palegoldenrod" -> palegoldenrod
    | "orchid" -> orchid
    | "orangered" -> orangered
    | "orange" -> orange
    | "olivedrab" -> olivedrab
    | "olive" -> olive
    | "oldlace" -> oldlace
    | "navy" -> navy
    | "navajowhite" -> navajowhite
    | "moccasin" -> moccasin
    | "mistyrose" -> mistyrose
    | "mintcream" -> mintcream
    | "midnightblue" -> midnightblue
    | "mediumvioletred" -> mediumvioletred
    | "mediumturquoise" -> mediumturquoise
    | "mediumspringgreen" -> mediumspringgreen
    | "mediumslateblue" -> mediumslateblue
    | "mediumseagreen" -> mediumseagreen
    | "mediumpurple" -> mediumpurple
    | "mediumorchid" -> mediumorchid
    | "mediumblue" -> mediumblue
    | "mediumaquamarine" -> mediumaquamarine
    | "maroon" -> maroon
    | "magenta" -> magenta
    | "linen" -> linen
    | "limegreen" -> limegreen
    | "lime" -> lime
    | "lightyellow" -> lightyellow
    | "lightsteelblue" -> lightsteelblue
    | "lightslategray" -> lightslategray
    | "lightskyblue" -> lightskyblue
    | "lightseagreen" -> lightseagreen
    | "lightsalmon" -> lightsalmon
    | "lightpink" -> lightpink
    | "lightgreen" -> lightgreen
    | "lightgrey" -> lightgrey
    | "lightgoldenrodyellow" -> lightgoldenrodyellow
    | "lightcyan" -> lightcyan
    | "lightcoral" -> lightcoral
    | "lightblue" -> lightblue
    | "lemonchiffon" -> lemonchiffon
    | "lawngreen" -> lawngreen
    | "lavenderblush" -> lavenderblush
    | "lavender" -> lavender
    | "khaki" -> khaki
    | "ivory" -> ivory
    | "indigo" -> indigo
    | "indianred" -> indianred
    | "hotpink" -> hotpink
    | "honeydew" -> honeydew
    | "greenyellow" -> greenyellow
    | "green" -> green
    | "gray" -> gray
    | "goldenrod" -> goldenrod
    | "gold" -> gold
    | "ghostwhite" -> ghostwhite
    | "gainsboro" -> gainsboro
    | "fuchsia" -> fuchsia
    | "forestgreen" -> forestgreen
    | "floralwhite" -> floralwhite
    | "firebrick" -> firebrick
    | "dodgerblue" -> dodgerblue
    | "dimgray" -> dimgray
    | "deepskyblue" -> deepskyblue
    | "deeppink" -> deeppink
    | "darkviolet" -> darkviolet
    | "darkturquoise" -> darkturquoise
    | "darkslategray" -> darkslategray
    | "darkslateblue" -> darkslateblue
    | "darkseagreen" -> darkseagreen
    | "darksalmon" -> darksalmon
    | "darkred" -> darkred
    | "darkorchid" -> darkorchid
    | "darkorange" -> darkorange
    | "darkolivegreen" -> darkolivegreen
    | "darkmagenta" -> darkmagenta
    | "darkkhaki" -> darkkhaki
    | "darkgreen" -> darkgreen
    | "darkgray" -> darkgray
    | "darkgoldenrod" -> darkgoldenrod
    | "darkcyan" -> darkcyan
    | "darkblue" -> darkblue
    | "cyan" -> cyan
    | "crimson" -> crimson
    | "cornsilk" -> cornsilk
    | "cornflowerblue" -> cornflowerblue
    | "coral" -> coral
    | "chocolate" -> chocolate
    | "chartreuse" -> chartreuse
    | "cadetblue" -> cadetblue
    | "burlywood" -> burlywood
    | "brown" -> brown
    | "blueviolet" -> blueviolet
    | "blue" -> blue
    | "blanchedalmond" -> blanchedalmond
    | "black" -> black
    | "bisque" -> bisque
    | "beige" -> beige
    | "azure" -> azure
    | "aquamarine" -> aquamarine
    | "aqua" -> aqua
    | "antiquewhite" -> antiquewhite
    | "aliceblue" -> aliceblue

  rgba(r, g, b, a) : color =
    r = color_value(r)
    g = color_value(g)
    b = color_value(b)
    a = color_value(a)
    {~r ~g ~b ~a}

  rgb(r, g, b) : color=
    rgba(r, g, b, 255)

  r(c:color) = c.r
  g(c:color) = c.g
  b(c:color) = c.b
  a(c:color) = c.a

  set_r(c, v) = {c:color with r = color_value(v)}
  set_g(c, v) = {c:color with g = color_value(v)}
  set_b(c, v) = {c:color with b = color_value(v)}
  set_a(c, v) = {c:color with a = color_value(v)}

  transparent = rgba(0,0,0,0)

  aliceblue            = rgb(240, 248, 255)
  antiquewhite         = rgb(250, 235, 215)
  aqua                 = rgb(  0, 255, 255)
  aquamarine           = rgb(127, 255, 212)
  azure                = rgb(240, 255, 255)
  beige                = rgb(245, 245, 220)
  bisque               = rgb(255, 228, 196)
  black                = rgb(  0,   0,   0)
  blanchedalmond       = rgb(255, 235, 205)
  blue                 = rgb(  0,   0, 255)
  blueviolet           = rgb(138,  43, 226)
  brown                = rgb(165,  42,  42)
  burlywood            = rgb(222, 184, 135)
  cadetblue            = rgb( 95, 158, 160)
  chartreuse           = rgb(127, 255,   0)
  chocolate            = rgb(210, 105,  30)
  coral                = rgb(255, 127,  80)
  cornflowerblue       = rgb(100, 149, 237)
  cornsilk             = rgb(255, 248, 220)
  crimson              = rgb(220,  20,  60)
  cyan                 = rgb(  0, 255, 255)
  darkblue             = rgb(  0,   0, 139)
  darkcyan             = rgb(  0, 139, 139)
  darkgoldenrod        = rgb(184, 134,  11)
  darkgray             = rgb(169, 169, 169)
  darkgreen            = rgb(  0, 100,   0)
  darkkhaki            = rgb(189, 183, 107)
  darkmagenta          = rgb(139,   0, 139)
  darkolivegreen       = rgb( 85, 107,  47)
  darkorange           = rgb(255, 140,   0)
  darkorchid           = rgb(153,  50, 204)
  darkred              = rgb(139,   0,   0)
  darksalmon           = rgb(233, 150, 122)
  darkseagreen         = rgb(143, 188, 143)
  darkslateblue        = rgb( 72,  61, 139)
  darkslategray        = rgb( 47,  79,  79)
  darkturquoise        = rgb(  0, 206, 209)
  darkviolet           = rgb(148,   0, 211)
  deeppink             = rgb(255,  20, 147)
  deepskyblue          = rgb(  0, 191, 255)
  dimgray              = rgb(105, 105, 105)
  dodgerblue           = rgb( 30, 144, 255)
  firebrick            = rgb(178,  34,  34)
  floralwhite          = rgb(255, 250, 240)
  forestgreen          = rgb( 34, 139,  34)
  fuchsia              = rgb(255,   0, 255)
  gainsboro            = rgb(220, 220, 220)
  ghostwhite           = rgb(248, 248, 255)
  gold                 = rgb(255, 215,   0)
  goldenrod            = rgb(218, 165,  32)
  gray                 = rgb(128, 128, 128)
  green                = rgb(  0, 128,   0)
  greenyellow          = rgb(173, 255,  47)
  honeydew             = rgb(240, 255, 240)
  hotpink              = rgb(255, 105, 180)
  indianred            = rgb(205,  92,  92)
  indigo               = rgb( 75,   0, 130)
  ivory                = rgb(255, 255, 240)
  khaki                = rgb(240, 230, 140)
  lavender             = rgb(230, 230, 250)
  lavenderblush        = rgb(255, 240, 245)
  lawngreen            = rgb(124, 252,   0)
  lemonchiffon         = rgb(255, 250, 205)
  lightblue            = rgb(173, 216, 230)
  lightcoral           = rgb(240, 128, 128)
  lightcyan            = rgb(224, 255, 255)
  lightgoldenrodyellow = rgb(250, 250, 210)
  lightgrey            = rgb(211, 211, 211)
  lightgreen           = rgb(144, 238, 144)
  lightpink            = rgb(255, 182, 193)
  lightsalmon          = rgb(255, 160, 122)
  lightseagreen        = rgb( 32, 178, 170)
  lightskyblue         = rgb(135, 206, 250)
  lightslategray       = rgb(119, 136, 153)
  lightsteelblue       = rgb(176, 196, 222)
  lightyellow          = rgb(255, 255, 224)
  lime                 = rgb(  0, 255,   0)
  limegreen            = rgb( 50, 205,  50)
  linen                = rgb(250, 240, 230)
  magenta              = rgb(255,   0, 255)
  maroon               = rgb(128,   0,   0)
  mediumaquamarine     = rgb(102, 205, 170)
  mediumblue           = rgb(  0,   0, 205)
  mediumorchid         = rgb(186,  85, 211)
  mediumpurple         = rgb(147, 112, 216)
  mediumseagreen       = rgb( 60, 179, 113)
  mediumslateblue      = rgb(123, 104, 238)
  mediumspringgreen    = rgb(  0, 250, 154)
  mediumturquoise      = rgb( 72, 209, 204)
  mediumvioletred      = rgb(199,  21, 133)
  midnightblue         = rgb( 25,  25, 112)
  mintcream            = rgb(245, 255, 250)
  mistyrose            = rgb(255, 228, 225)
  moccasin             = rgb(255, 228, 181)
  navajowhite          = rgb(255, 222, 173)
  navy                 = rgb(  0,   0, 128)
  oldlace              = rgb(253, 245, 230)
  olive                = rgb(128, 128,   0)
  olivedrab            = rgb(107, 142,  35)
  orange               = rgb(255, 165,   0)
  orangered            = rgb(255,  69,   0)
  orchid               = rgb(218, 112, 214)
  palegoldenrod        = rgb(238, 232, 170)
  palegreen            = rgb(152, 251, 152)
  paleturquoise        = rgb(175, 238, 238)
  palevioletred        = rgb(216, 112, 147)
  papayawhip           = rgb(255, 239, 213)
  peachpuff            = rgb(255, 218, 185)
  peru                 = rgb(205, 133,  63)
  pink                 = rgb(255, 192, 203)
  plum                 = rgb(221, 160, 221)
  powderblue           = rgb(176, 224, 230)
  purple               = rgb(128,   0, 128)
  red                  = rgb(255,   0,   0)
  rosybrown            = rgb(188, 143, 143)
  royalblue            = rgb( 65, 105, 225)
  saddlebrown          = rgb(139,  69,  19)
  salmon               = rgb(250, 128, 114)
  sandybrown           = rgb(244, 164,  96)
  seagreen             = rgb( 46, 139,  87)
  seashell             = rgb(255, 245, 238)
  sienna               = rgb(160,  82,  45)
  silver               = rgb(192, 192, 192)
  skyblue              = rgb(135, 206, 235)
  slateblue            = rgb(106,  90, 205)
  slategray            = rgb(112, 128, 144)
  snow                 = rgb(255, 250, 250)
  springgreen          = rgb(  0, 255, 127)
  steelblue            = rgb( 70, 130, 180)
  tan                  = rgb(210, 180, 140)
  teal                 = rgb(  0, 128, 128)
  thistle              = rgb(216, 191, 216)
  tomato               = rgb(255,  99,  71)
  turquoise            = rgb( 64, 224, 208)
  violet               = rgb(238, 130, 238)
  wheat                = rgb(245, 222, 179)
  white                = rgb(255, 255, 255)
  whitesmoke           = rgb(245, 245, 245)
  yellow               = rgb(255, 255,   0)
  yellowgreen          = rgb(154, 205,  50)





}}
