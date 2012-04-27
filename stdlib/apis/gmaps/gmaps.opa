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
 * Google Maps modules
 *
 * This file provides a way to easily integrate static or javascript map int a page.
 * It also allow (reverse-)geocoding. An API key is needed for geocoding and javascript
 * API, you can get one here: http://code.google.com/apis/maps/signup.html
 *
 * Important :
 *   Any application using this must respect Google policy, available here:
 *   http://code.google.com/apis/maps/terms.html
 *
 * @category web
 * @author Nicolas Glondu, 2010
 * @destination
 * - private: Gmaps_private
 * - public: Gmaps_static, Gmaps_geocoding, Gmaps
 * @stability Work in progress
 */

import stdlib.web.client

/**
 * Static maps API
 *
 * {1 About this module}
 *
 * Helps to build the src location for a given map. This method has the advantage of
 * working without Javascript. Limited to 1000 diffent pictures per viewer (IP) per day.
 *
 * {1 Where should I start?}
 *
 * This module contains only one function and some types. You should start to get
 * familiar with the various types available. The function tranforms a size and a list
 * of parameter into the url of a map. You should use the html tag "img" to display it.
 * The simplest only needs a center and a zoom level. The center can be either coordinates
 * formatted as "latitude,longitude" (ex: "48.8259695,2.3846613") or an address or the name
 * of a place (ex: "Eiffel tower"). A static map can't be higher or wider than 640 pixels.
 * If you ask a map outside these bounds, it will be limited to those.
 *
 * {1 What if I need more?}
 *
 * Most of the static maps can be built with the various parameters. You should look
 * at the existing types to find what ou are searching.
 */

/**
 * Format of the picture
 *
 * jpg and jpg_baseline provide the smallest image size but are compressed while
 * other formats are not. jpg should be used when data size matters, for instance
 * mobile Internet.
 */
type Gmaps_static.format = {png} / {png32} / {gif} / {jpg} / {jpg_baseline}

/**
 * Type of the map
 */
type Gmaps_static.type = {roadmap} / {satellite} / {hybrid} / {terrain}

/**
 * Type of the style of markers
 *
 * There are actually two subsets which can't be mixed:
 * Size/Color/Label and Icon/Noshadow
 */
type Gmaps_static.markerstyle =
     { Size: string }
   / { Color: string }
   / { Label: string }
   / { Icon: string } /** url encoded with '|' double-encoded */
   / { Noshadow }

/**
 * Type of markers
 *
 * Represents a group of markers with the same style.
 */
type Gmaps_static.markers = {
     markerstyle: list(Gmaps_static.markerstyle);
     locations: list(string)
}

/**
 * Type of the style of paths
 */
type Gmaps_static.pathstyle = { Weight: int } / { Color: string } / { Fillcolor: string }

/**
 * Type of paths
 */
type Gmaps_static.path = {
     pathstyle: list(Gmaps_static.pathstyle);
     locations: list(string)
}

/**
 * Parameters of the static map of the user
 *
 * To be valid, a map needs either a center and a zoom or a at least one marker or a path.
 */
type Gmaps_static.param =
     { Center: string }
   / { Zoom: int }
   / { Format: Gmaps_static.format }
   / { Maptype: Gmaps_static.type }
   / { Mobile } /* true if mentionned, default is false */
   / { Language: string }
   / { Markers: Gmaps_static.markers }
   / { Path: Gmaps_static.path }
   / { Visible: list(string)}
   / { Sensor } /* true if mentionned, default is false */

/**
 * Static maps module
 */
Gmaps_static = {{

/**
 * Builds the url corresponding to given map parameters
 *
 * This function is the only function of Google static maps module.
 *
 * @param map_param Definition of wanted map as a list of Gmaps_static.param.
 */
  build_url(size:(int, int), map_params) =
    base_url = "http://maps.google.com/maps/api/staticmap?"
    add(a, b) = List.cons(a, b)
    merge(a, b) =
      if a == "" then b
      else "{a}|{b}"
    auxlist(l) =
      if l == [] then ""
      else
        aux(el, acc) = "{acc}|{el}"
        List.fold(aux, List.tail(l), List.head(l))

    auxformat(p, acc) = match p : Gmaps_static.format
      { png } -> add("format=png", acc)
      { png32 } -> add("format=png32", acc)
      { gif } -> add("format=gif", acc)
      { jpg } -> add("format=jpg", acc)
      { jpg_baseline } -> add("format=jpg-baseline", acc)

    auxtype(p, acc) = match p : Gmaps_static.type
      { roadmap } -> add("maptype=roadmap", acc)
      { satellite } -> add("maptype=satellite", acc)
      { hybrid } -> add("maptype=hybrid", acc)
      { terrain } -> add("maptype=terrain", acc)

    auxmarkerstyle(p, acc) = match p : Gmaps_static.markerstyle
      { ~Size } -> add("size:{Size}", acc)
      { ~Color } -> add("color:{Color}", acc)
      { ~Label } -> add("label:{Label}", acc)
      { ~Icon } -> add("icon:{Icon}", acc)
      { Noshadow } -> add("shadow:false", acc)

    auxpathstyle(p, acc) = match p : Gmaps_static.pathstyle
      { ~Weight } -> add("weight:{Weight}", acc)
      { ~Color } -> add("color:{Color}", acc)
      { ~Fillcolor } -> add("weight:{Fillcolor}", acc)

    aux(p, acc) = match p : Gmaps_static.param
      { ~Center } -> add("center={Center}", acc)
      { ~Zoom } -> add("zoom={Zoom}", acc)
      { ~Format } -> auxformat(Format, acc)
      { ~Maptype } -> auxtype(Maptype, acc)
      { Mobile } -> add("mobile=true", acc)
      { ~Language } -> add("language={Language}", acc)
      { ~Markers } ->
        s1 = auxlist(List.fold(auxmarkerstyle, Markers.markerstyle, []))
        s2 = auxlist(Markers.locations)
        add("markers={merge(s1,s2)}", acc)
      { ~Path } ->
        s1 = auxlist(List.fold(auxpathstyle, Path.pathstyle, []))
        s2 = auxlist(Path.locations)
        add("path={merge(s1,s2)}", acc)
      { ~Visible } -> add("visible={auxlist(Visible)}", acc)
      _ -> acc

    sensor = List.mem({Sensor}, map_params)
    txtparams = List.fold(aux, map_params, [])
    txtparams = add("size={size.f1}x{size.f2}&sensor={sensor}", txtparams)
    txtparams = List.rev(txtparams)

    finaux(l) =
      aux(el : string, acc : string) = "{acc}&{el}"
      List.fold(aux, List.tail(l), List.head(l))
    "{base_url}{finaux(txtparams)}"

}}

/* ------------------------------------------------------------------------------------------------------------
   ----8<----8<----8<----8<----8<----8<----8<----8<---- - ---->8---->8---->8----->8---->8---->8---->8---->8----
   ------------------------------------------------------------------------------------------------------------ */

/**
 * Gmaps geocoding API
 *
 * {1 About this module}
 *
 * This module performs geocoding requests. Geocoding means finding the latitude and the longitude
 * of a point of Earth from its address. This function also works as a reverse geocoder:
 * an address can be found from its latitude and longitude.
 * Note : This API is limited to 15 000 requests in 24 hours so frequent requests should
 * be cached. Requests should not be sent 'too fast' (no more precision) - IP-based limitations.
 *
 * All the functions of this this module return the same kind of result. The end user can
 * choose the format of the result in all functions. Google recommends json because it
 * is the lightest so the less bandwith consuming. See
 * http://code.google.com/intl/fr/apis/maps/documentation/geocoding/ for explanation
 * on the content of the result.
 *
 * {1 Where should I start?}
 *
 * The simplest function is Gmaps_geocoding.request. A basic test is to try with an address you
 * know, the format output_json and a function that simply displays the result. You can also try
 * to pass the coordinates obtained back to test the reverse geocoder.
 *
 * {1 What if I need more?}
 *
 * The other functions add biases to your search. The results will tend to give result in the
 * United States by default. For instance "Toledo" will return a result in Ohio. If you specify
 * the country Spain (ccTLD "es"), "Toledo" will give a return a result in Spain.
 */

/**
 * Format of the output
 */
type Gmaps_geocoding.output_format =
     { output_json }
   / { output_csv }
   / { output_xml }

Gmaps_geocoding = {{

/**
 * Full Google Maps geocoding request
 *
 * This function makes a geocoding request to Google servers and returns the result
 * to given function.
 * Note: The parameters ll, spn and gl work as biases. They define where result \should\
 * but not necesarily \must\ be. The result can still be outside these bounds.
 *
 * @param query The geocoding or reverse geocoding request (either an address or coordinates formatted as latitude,longitude)
 * @param key Your API key. If you don't have one yet, go to http://code.google.com/apis/maps/signup.html
 * @param output The format you want for the response: xml, kml, csv or json.
 * @param gl The country code (ccTLD) of the country where the result should be
 * @param ll The center of the area in which the result should be (requires spn)
 * @param spn The span of the area in which the result should be (requires ll)
 * @param final_fun The function which will handle the result. It must take a string (the raw text result) and return a Resource.
 */
  request_fullbias(query, key, output, gl, ll, spn, final_fun) =

    sensor = false /* Since it is browser-based, I think it will stay false */

    output_format_to_string(output) =
      match output:Gmaps_geocoding.output_format
      { output_json } -> "json"
      { output_csv }  -> "csv"
      { output_xml }  -> "xml"

    uri_query =
      List.filter((_, s) -> s != "",
        [("q", query), ("key", key), ("sensor", "{sensor}"),
         ("output", output_format_to_string(output)),
         ("ll", ll), ("spn", spn), ("gl", gl) ])

    uri =
      { Uri.default_absolute with
        domain = "maps.google.com";
        path = ["maps", "geo"] ;
        query = uri_query } <: Uri.uri

    res = match WebClient.Get.try_get(uri) with
      | {success = { ~content ... }} -> content
      | _ -> "error"
    final_fun(res)

/**
 * Basic Google Maps geocoding request
 *
 * Similar to request_fullbias but no bias is added to this request. Which make
 * the function more convenient to use.
 *
 * @param query The geocoding or reverse geocoding request (either an address or coordinates formatted as latitude,longitude)
 * @param key Your API key. If you don't have one yet, go to http://code.google.com/apis/maps/signup.html
 * @param output The format you want for the response: xml, kml, csv or json.
 * @param final_fun The function which will handle the result. It must take a string (the raw text result) and return a Resource.
 */
  request(query, key, output, final_fun) =
    request_fullbias(query, key, output, "", "", "", final_fun)

/**
 * Google Maps geocoding request with country bias
 *
 * Similar to request_fullbias but only country bias is added to this request. Which make
 * the function more convenient to use.
 *
 * @param query The geocoding or reverse geocoding request (either an address or coordinates formatted as latitude,longitude)
 * @param key Your API key. If you don't have one yet, go to http://code.google.com/apis/maps/signup.html
 * @param output The format you want for the response: xml, kml, csv or json.
 * @param gl The country code (ccTLD) of the country where the result should be
 * @param final_fun The function which will handle the result. It must take a string (the raw text result) and return a Resource.
 */
  request_countrybias(query, key, output, gl, final_fun) =
    request_fullbias(query, key, output, gl, "", "", final_fun)

}}

/* ------------------------------------------------------------------------------------------------------------
   ----8<----8<----8<----8<----8<----8<----8<----8<---- - ---->8---->8---->8----->8---->8---->8---->8---->8----
   ------------------------------------------------------------------------------------------------------------ */

/**
 * Google Maps Javascript API
 *
 * {1 About this module}
 *
 * This module converts a given Gmaps.map object into the html/javascript needed
 * to load the map in a new div with the given id.
 *
 *
 * {1 Where should I start?}
 *
 * This module has three functions to use according to the number of maps on the page.
 * You should start with Gmaps.single_map_elt which takes as input your API key (you can
 * start with an empty key for tests), an id and a map. Since a map is a complex object,
 * can use Gmaps.default_map and modify its fields according to your needs. This (and the
 * two others) returns xhtml so you should directly add it into your page:
 * <div>{Gmaps.single_map_elt( "", "my_map", Gmaps.default_map )}</div> is a simple map
 * example.
 *
 * {1 What if I need more?}
 *
 * You should look at the sctructure of the types available. A high level of customization
 * is quite hard to build but the result is generally nice.
 */

/**
 * Type of latitudes and longitudes
 */
type Gmaps.LatLng = { lat : float ; lng : float }

/**
 * Type of sizes
 */
type Gmaps.size = { x : int; y : int }

/**
 * Various types of controls
 */
type Gmaps.control =
    { LargeMapControl3D }
  / { LargeMapControl }
  / { SmallMapControl }
  / { SmallZoomControl3D }
  / { SmallZoomControl }
  / { ScaleControl }
  / { MapTypeControl }
  / { MenuMapTypeControl }
  / { OverviewMapControl }
  / { NavLabelControl }

/**
 * Position of a control
 *
 * The size given is the offset from given border.
 */
type Gmaps.controlPosition =
    { TOP_RIGHT    : option(Gmaps.size) }
  / { TOP_LEFT     : option(Gmaps.size) }
  / { BOTTOM_RIGHT : option(Gmaps.size) }
  / { BOTTOM_LEFT  : option(Gmaps.size) }

/**
 * Available map types
 *
 * The names are explicit. The last types are groupment of types.
 */
type Gmaps.map_type =
    { NORMAL } / { SATELLITE } / { AERIAL } / { HYBRID } / { AERIAL_HYBRID } / { PHYSICAL }
  / { MAPMAKER_NORMAL } / { MAPMAKER_HYBRID }
  / { MOON_ELEVATION } / { MOON_VISIBLE }
  / { MARS_ELEVATION } / { MARS_VISIBLE } / { MARS_INFRARED }
  / { SKY_VISIBLE }
  / { SATELLITE_3D } /** Requires a specific google plugin */
  / { DEFAULT_TYPES }  /** Groups normal, satellite and hybrid */
  / { MAPMAKER_TYPES  } /** Groups mapmaker_normal, satellite and mapmaker_hybrid */
  / { MOON_TYPES } /** Groups moon_elevation and moon visible */
  / { MARS_TYPES } /** Groups mars_elevation, mars_visible and mars_infrared */
  / { SKY_TYPES } /** Equivalent to sky_visible */

/**
 * Properties of icons
 *
 * An Icon is a list of changes starting from the classic Google Maps icon
 * (the red marker).
 */
type Gmaps.icon_property =
    { image : string } /** Url of the new picture */
  / { shadow : string } /** Url of the new shadow */
  / { iconSize : Gmaps.size } /** Size of the new picture */
  / { shadowSize : Gmaps.size } /** Size of the new shadow */
  / { iconAnchor : (int, int) } /** Coordinates of the anchor of the picture from top left corner */
  / { infoWindowAnchor : (int, int) } /** Coordinates of the anchor of the shadow from top left corner */
  / { maxHeight : float } /** Elevation of the icon when dragged */
  / { dragCrossImage : string } /** Url of the cross displayed under the picture while dragging */
  / { dragCrossSize : Gmaps.size } /** Size of the drag cross */
  / { dragCrossAnchor : (int, int) } /** Coordinates of the anchor of the drag cross from top left corner */

/**
 * Options of Gmaps markers
 *
 * One option should be used only once per marker. It is, for instance, useless to put two titles
 * for one marker and the result of such may vary. Some options are implicitely linked. it is
 * useless for a marker to be bouncy if it is not draggable or a draggable marker will be clickable
 * even if you try to set clickable to false.
 */
type Gmaps.marker_option =
    { dragCrossMove : bool } /** Lightly changes the display of the marked when dragged, default is false. */
  / { title : string } /** Title of the marker. Displayed as a classic img title: when the mouse is over it. */
  / { clickable : bool } /** If false, the marker will be inert and take less ressource. Default is true. */
  / { draggable : bool } /** If true, the marker will be draggable. Default is false. */
  / { bouncy : bool } /** If false, the marker will not bounce when release after having been dragged. Default is true. */
  / { bounceGravity : float } /** The gravity for the bounces. Default is 1.0.*/
  / { autopan : bool } /** If false, the map will not automatically scroll if you drag the marker near a border. Default is true */
  / { icon : list(Gmaps.icon_property) }
  / { specific : string } /** Specific option. Utility depends on the tool used with the map. */

/**
 * Type of a marker
 *
 * A marker is a pin showing a specific point on the map. An info window can be attached to it.
 * The info window will display additional information when the marker is clicked.
 */
type Gmaps.marker =
   { position    : Gmaps.LatLng
   ; info_window : option(string)
   ; options     : list(Gmaps.marker_option)
   }

/**
 * Options of Gmaps polylines
 */
type Gmaps.polyline_option =
   { clickable : bool } / { geodesic : bool }
/* / { mouseOutTolerance : int } (useless because no events (yet?)))*/

/**
 * Type of a polyline
 *
 * A polyline is a group of lines between various geographic points. On long distances, since the
 * map is flattened to be displayed, straight lines do not represent reality. If the option "geodesic"
 * is added and set to true, the polyline will be built as geodesic (segment of a "great circle").
 */
type Gmaps.polyline =
   { points  : list(Gmaps.LatLng)
   ; color   : string
   ; weight  : float
   ; opacity : float
   ; options : list(Gmaps.polyline_option)
   }

/**
 * Type of a polygon
 *
 * A polygon is a closed polyline. The part "inside" (the smallest part limited by the polygon)
 * can be colored. It is recommended to really close a polygon. This means that the first and the
 * last points of the list should be the same. Polygons cannot be built with geodesic.
 */
type Gmaps.polygon =
   { points      : list(Gmaps.LatLng)
   ; color       : string
   ; weight      : float
   ; opacity     : float
   ; fillcolor   : string
   ; fillopacity : float
   ; options     : list(Gmaps.polyline_option)
   }

/**
 * Type of a screen overlay
 *
 * A screen overlay is a picture displayed like a control, at a fix position of the screen.
 */
type Gmaps.screen_overlay =
   { img_url    : string
   ; screenXY   : (int, int)
   ; overlayXY  : (int, int)
   ; screenSize : (int, int)
   }

/**
 * Type of a ground overlay
 *
 * A ground overlay is a picture displayed on the ground as a new tile of the map.
 */
type Gmaps.ground_overlay =
   { img_url : string
   ; boundSW : Gmaps.LatLng
   ; boundNE : Gmaps.LatLng
   }

/**
 * Type of overlays
 *
 * The purpose of this type is to group all the defined overlays in one type.
 * map_center is a specific marker which will be placed at the center of the map.
 * Note: map_center cannot be tracked with the Marker_tracker tool and will alway be
 * displayed with the Marker_manager. It can be used with the Marker_clusterer.
 */
type Gmaps.overlay =
    { marker         : Gmaps.marker }
  / { polyline       : Gmaps.polyline }
  / { polygon        : Gmaps.polygon }
  / { layer          : string }
  / { screen_overlay : Gmaps.screen_overlay }
  / { ground_overlay : Gmaps.ground_overlay }
  / { map_center }

/**
 * Google Maps tools
 *
 * Tools are additonal (and often open source) modules of Google Maps. Modules add
 * possibilities to Google Maps not necessary in a general use.
 * Modules are here associated with the marker_option named /specific/.
 * Actually, only one module can be used at once.
 */
type Gmaps.tool =
    { No_tool } /** No tool, default behaviour. */
  / { Marker_clusterer } /** Marker clusterer tool. If used, markers close to each others will be grouped in one big marker indicating the number of markers in the area. /specific/ is not used for this module, all markers use it if activated.*/
  / { Marker_manager } /** Marker manager tool. This tool allow to set a zoom level (or a set of levels) at which each marker will be visible. The marker manager also improves performances when a lot of markers are "outside" the current viewport. /specific/ must be formatted as "{minZoomLevel},{maxzoomlevel}" or "{minZoomLevel}". The markers with no or incorrect /specific/ are displayed normally. */
  / { Marker_tracker } /** Marker tracker tool. This tool adds an simbol at the edge of the viewport to find more easily specified markers. A bit buggy [April 2010] if you cross the Earth, but useful at a country or a continent scale. /specific/ must be a color (encoded in Hexadecimal like '#00E0E0') to enable this tool for a marker. */

/**
 * A Google Map map
 *
 * The main object which groups others in a structured map. If well built, passing this object to
 * the Gmaps.map_elt function with a valid api key will create the map wanted by the user.
 * A default map Gmaps.default_map if defined to make the map creation easier. The user only
 * has to chenge the parameters he wants.
 */
type Gmaps.map =
  { center           : Gmaps.LatLng /** Center of the map */
  ; zoom             : int /** The zoom level of the map */
  ; size             : option(Gmaps.size) /** The size of the map. The map takes the size of the container if none. */
  ; dragging_enabled : bool /** Determines if the map can be dragged */
  ; controls         : list((Gmaps.control, option(Gmaps.controlPosition))) /** The controls for the map. */
  ; overlays         : list(Gmaps.overlay) /** Various overlays for the map. See individual overlay definitions for help. */
  ; maptypes         : list(Gmaps.map_type) /** The map types activated for the map. If none specified, the classic ones (map, satellite, hybrid) are set. */
  ; tool             : Gmaps.tool /** Special tools for the map. */
  }

Gmaps_private = {{
  _json_generic(delim:string, o:string, c:string, l:list(string)) =
    if l == [] then ""
    else
      aux(elt:string, acc:string) = "{acc},{delim}{elt}{delim}"
      sub_res = List.fold(aux, List.tail(l), "{delim}{List.head(l)}{delim}")
      "{o}{sub_res}{c}"

/*
 * ---------- basic ----------
 */

  _latlng_to_string(ll:Gmaps.LatLng) = "new GLatLng({ll.lat}, {ll.lng})"

  _addSizeOpt(size) =
    match size: option(Gmaps.size)
    { none } -> ""
    { ~some } -> "new GSize({some.x},{some.y})"

/*
 * ---------- icons ----------
 */

  _icon_property_to_string(icon_name, i) =
    match i:Gmaps.icon_property
    { image=v }            -> icon_name^".image=\"{v}\";"
    { shadow=v }           -> icon_name^".shadow=\"{v}\";"
    { iconSize=v }         -> icon_name^".iconSize={_addSizeOpt({some=v})};"
    { shadowSize=v }       -> icon_name^".shadowSize={_addSizeOpt({some=v})};"
    { iconAnchor=v }       -> icon_name^".iconAnchor=new GPoint({v.f1},{v.f2});"
    { infoWindowAnchor=v } -> icon_name^".infoWindowAnchor=new GPoint({v.f1},{v.f2});"
    { maxHeight=v }        -> icon_name^".maxHeight={v};"
    { dragCrossImage=v }   -> icon_name^".dragCrossImage=\"{v}\";"
    { dragCrossSize=v }    -> icon_name^".dragCrossSize={_addSizeOpt({some=v})};"
    { dragCrossAnchor=v }  -> icon_name^".dragCrossAnchor=new GPoint({v.f1},{v.f2});"

  _icon_to_string(icon_name, properties) =
    properties
    |> List.map(_icon_property_to_string(icon_name, _), _)
    |> (l -> List.fold( (elt, acc -> acc^elt), l, "new GIcon(G_DEFAULT_ICON);"))

/*
 * ---------- overlays ----------
 */

  _marker_option_to_string(js_tmp:string, o) =
    match o:Gmaps.marker_option
    { dragCrossMove=v } -> "dragCrossMove:{v}"
    { title=v }         -> "title:\"{String.replace("\"","\\\"",v)}\""
    { clickable=v }     -> "clickable:{v}"
    { draggable=v }     -> "draggable:{v}"
    { bouncy=v }        -> "bouncy:{v}"
    { bounceGravity=v } -> "bounceGravity:{v}"
    { autopan=v }       -> "autopan:{v}"
    { specific=v }      -> v
    { icon=v }          -> "icon:{_icon_to_string(js_tmp,v)}"

  _marker_to_string(js_tmp, m:Gmaps.marker) =
    icon_def = m.options
      |> List.filter((o -> match o:Gmaps.marker_option
                         { icon=_ } -> true
                         _ -> false ), _)
      |> List.map((o -> match o:Gmaps.marker_option
                         { ~icon } -> icon
                         _ -> [] ), _)
      |> (l -> if l == [] then ""
               else
                 List.head(l) /* There should be only one icon set per marker, if multiple, the first is taken */
                 |> _icon_to_string(js_tmp, _)
                 |> (s -> "{s} {js_tmp}=")
      )
    options_text = m.options
      |> List.filter((o -> match o:Gmaps.marker_option
                         { specific=_ } -> false
                         _ -> true ), _)
      |> List.map(_marker_option_to_string(js_tmp, _), _)
      |> _json_generic("", ", \{", "}", _)
    more_text = match m.info_window:option(string)
    | { none }  -> ""
    | { ~some } -> ";GEvent.addListener({js_tmp}, 'click', function()\{ this.openInfoWindowHtml(\"{String.replace("\"","\\\"",some)}\");})"
    "{icon_def}new GMarker({_latlng_to_string(m.position)}{options_text}){more_text}"

  _marker_get_specific(js_tmp, m:Gmaps.marker) =
    m.options
    |> List.filter((o -> match o:Gmaps.marker_option
                         { specific=_ } -> true
                         _ -> false ), _)
    |> List.map(_marker_option_to_string(js_tmp, _), _)
    |> (l -> if l == [] then "" else List.head(l))

  _polyline_option_to_string(o) =
    match o:Gmaps.polyline_option
    { clickable=v } -> "clickable:{v}"
    { geodesic=v }  -> "geodesic:{v}"

  _polyline_to_string(p:Gmaps.polyline) =
    points_text = if p.points == [] then "[]"
      else List.map(_latlng_to_string, p.points) |> _json_generic("", "[", "]", _)
    options_text = p.options
      |> List.map(_polyline_option_to_string, _)
      |> _json_generic("", ", \{", "}", _)
    "new GPolyline({points_text}, '{p.color}', {p.weight}, {p.opacity}{options_text})"

  _polygon_to_string(p:Gmaps.polygon) =
    points_text = if p.points == [] then "[]"
      else List.map(_latlng_to_string, p.points) |> _json_generic("", "[", "]", _)
    options_text = p.options
      |> List.map(_polyline_option_to_string, _)
      |> _json_generic("", ", \{", "}", _)
    "new GPolygon({points_text}, '{p.color}', {p.weight}, {p.opacity}, '{p.fillcolor}', {p.fillopacity}{options_text})"

  _screen_overlay_to_string(s:Gmaps.screen_overlay) =
    "new GScreenOverlay('{s.img_url}', new GScreenPoint(s.screenXY.f1, s.screenXY.f2), new GScreenPoint(s.overlayXY.f1, s.overlayXY.f2), new GScreenSize(s.screenSize.f1, s.screenSize.f2))"

  _ground_overlay_to_string(g:Gmaps.ground_overlay) =
    "new GGroundOverlay('{g.img_url}', new GLatLngBounds({_latlng_to_string(g.boundSW)}, {_latlng_to_string(g.boundNE)}))"

  _map_center_to_string(js_obj:string) = "new GMarker({js_obj}.getCenter())"

  _overlay_to_string(js_obj:string, js_tmp, o) =
    match o:Gmaps.overlay
    { ~marker }         -> _marker_to_string(js_tmp, marker)
    { ~polyline }       -> _polyline_to_string(polyline)
    { ~polygon }        -> _polygon_to_string(polygon)
    { ~layer }          -> "new GLayer('{layer}')"
    { ~screen_overlay } -> _screen_overlay_to_string(screen_overlay)
    { ~ground_overlay } -> _ground_overlay_to_string(ground_overlay)
    { map_center }      -> _map_center_to_string(js_obj)

  _add_overlays(js_obj, js_tmp, fun, os) =
    os
    |> List.map(fun(js_obj, js_tmp, _), _)
    |> (l -> List.fold((x,y -> x^y), l, ""))

  _add_overlay_normal(js_obj:string, js_tmp, o) =
    "{js_tmp}={_overlay_to_string(js_obj, js_tmp, o)}; {js_obj}.addOverlay({js_tmp}); "

/*
 * ---------- map types ----------
 */

  _map_type_to_string(t) =
    f(x:string) = "G_{x}_MAP"
    match t:Gmaps.map_type
    { NORMAL }          -> f("NORMAL")
    { SATELLITE }       -> f("SATELLITE")
    { AERIAL }          -> f("AERIAL")
    { HYBRID }          -> f("HYBRID")
    { AERIAL_HYBRID }   -> f("AERIAL_HYBRID")
    { PHYSICAL }        -> f("PHYSICAL")
    { MAPMAKER_NORMAL } -> f("MAPMAKER_NORMAL")
    { MAPMAKER_HYBRID } -> f("MAPMAKER_HYBRID")
    { MOON_ELEVATION }  -> f("MOON_ELEVATION")
    { MOON_VISIBLE }    -> f("MOON_VISIBLE")
    { MARS_ELEVATION }  -> f("MARS_ELEVATION")
    { MARS_VISIBLE }    -> f("MARS_VISIBLE")
    { MARS_INFRARED }   -> f("MARS_INFRARED")
    { SKY_VISIBLE }     -> f("SKY_VISIBLE")
    { SATELLITE_3D }    -> f("SATELLITE_3D")
    { DEFAULT_TYPES }   -> "G_DEFAULT_MAP_TYPES"
    { MAPMAKER_TYPES  } -> "G_MAPMAKER_MAP_TYPES"
    { MOON_TYPES }      -> "G_MOON_MAP_TYPES"
    { MARS_TYPES }      -> "G_MARS_MAP_TYPES"
    { SKY_TYPES }       -> "G_SKY_MAP_TYPES"

  _map_type_expand(t) =
    match t:Gmaps.map_type
    { DEFAULT_TYPES }   -> [{NORMAL}, {SATELLITE}, {HYBRID}]
    { MAPMAKER_TYPES  } -> [{MAPMAKER_NORMAL}, {SATELLITE}, {MAPMAKER_HYBRID}]
    { MOON_TYPES }      -> [{MOON_ELEVATION}, {MOON_VISIBLE}]
    { MARS_TYPES }      -> [{MARS_ELEVATION}, {MARS_VISIBLE}, {MARS_INFRARED}]
    { SKY_TYPES }       -> [{SKY_VISIBLE}]
    x                   -> [x]

  _addMapTypes(types) =
    if types == [] then ""
    else types
      |> List.map(_map_type_expand, _)
      |> List.flatten(_)
      |> List.map(_map_type_to_string, _)
      |> _json_generic("", "[", "]", _)

/*
 * ---------- controls ----------
 */

  _control_to_string(c) =
    match c:Gmaps.control
    { LargeMapControl3D }  -> "GLargeMapControl3D"
    { LargeMapControl }    -> "GLargeMapControl"
    { SmallMapControl }    -> "GSmallMapControl"
    { SmallZoomControl3D } -> "GSmallZoomControl3D"
    { SmallZoomControl }   -> "GSmallZoomControl"
    { ScaleControl }       -> "GScaleControl"
    { MapTypeControl }     -> "GMapTypeControl"
    { MenuMapTypeControl } -> "GMenuMapTypeControl"
    { OverviewMapControl } -> "GOverviewMapControl"
    { NavLabelControl }    -> "GNavLabelControl"

  _addPosOpt(s) =
    auxSize(s) =
      res = _addSizeOpt(s)
      if (res == "") then res
      else ", {res}"
    match s : option(Gmaps.controlPosition)
    { none }  -> ""
    { ~some } -> ( match some : Gmaps.controlPosition
      { ~TOP_RIGHT }    -> "new GControlPosition(G_ANCHOR_TOP_RIGHT{auxSize(TOP_RIGHT)})"
      { ~TOP_LEFT }     -> "new GControlPosition(G_ANCHOR_TOP_LEFT{auxSize(TOP_LEFT)})"
      { ~BOTTOM_RIGHT } -> "new GControlPosition(G_ANCHOR_BOTTOM_RIGHT{auxSize(BOTTOM_RIGHT)})"
      { ~BOTTOM_LEFT }  -> "new GControlPosition(G_ANCHOR_BOTTOM_LEFT{auxSize(BOTTOM_LEFT)})"
    )

  _add_controls(js_obj:string, controls) =
    auxPos(s) =
      res = _addPosOpt(s)
      if res == "" then res
      else ", {res}"
    aux(c, acc) = "{acc}{js_obj}.addControl(new {_control_to_string(c.f1)}(){auxPos(c.f2)});\n"
    List.fold(aux, controls, "")

/*
 * ---------- tools ----------
 */

  _specific_check(s:string, parsefun) =
    Parser.try_parse(parsefun, s) ? false

  _add_overlay_clusterer(js_obj:string, js_tmp, o) =
    aux(s:string) = "{js_tmp}={s};markers.push({js_tmp});"
    match o:Gmaps.overlay
    { ~marker }    -> aux(_marker_to_string(js_tmp, marker))
    { map_center } -> aux(_map_center_to_string(js_obj))
    _              -> _add_overlay_normal(js_obj, js_tmp, o);

  _manager_specific_parser =
    blanck = parser [ ] -> void
    parser
    | blanck* [0-9]+ blanck* -> true
    | blanck* [0-9]+ blanck* "," blanck* [0-9]+ blanck* -> true
    | .* -> false

  _add_overlay_manager(js_obj:string, js_tmp, o) =
    match o:Gmaps.overlay
    { ~marker } ->
      specific = _marker_get_specific(js_tmp, marker)
      marker_js = _marker_to_string(js_tmp, marker)
      if specific == "" || (_specific_check(specific, _manager_specific_parser) == false) then
        "{js_tmp}={marker_js};{js_obj}.addOverlay({js_tmp}); "
      else
        "{js_tmp}={marker_js};mgr.addMarker({js_tmp}, {specific}); "
    _           -> _add_overlay_normal(js_obj, js_tmp, o);

  _tracker_specific_parser =
    blanck = parser [ ] -> void
    hex = parser [0-9a-fA-F] -> void
    parser
    | blanck* "#" hex hex hex hex hex hex blanck* -> true
    | .* -> false

  _add_overlay_tracker(js_obj:string, js_tmp, o) =
    match o:Gmaps.overlay
    { ~marker } ->
      specific = _marker_get_specific(js_tmp, marker)
      marker_js = _marker_to_string(js_tmp, marker)
      if specific == "" then
        "{js_tmp}={marker_js};{js_obj}.addOverlay({js_tmp}); "
      else if (_specific_check(specific, _tracker_specific_parser)) then
        "{js_tmp}={marker_js}; {js_obj}.addOverlay({js_tmp}); new MarkerTracker({js_tmp}, {js_obj}, \{color:'{specific}'}); "
      else
        "{js_tmp}={marker_js}; {js_obj}.addOverlay({js_tmp}); new MarkerTracker({js_tmp}, {js_obj}); "
    _           -> _add_overlay_normal(js_obj, js_tmp, o);

  _tool_to_source(t) =
    base = "http://gmaps-utility-library.googlecode.com/svn/trunk/"
    match t:Gmaps.tool
    { No_tool }          -> ""
    { Marker_clusterer } -> base^"markerclusterer/1.0/src/markerclusterer_packed.js"
    { Marker_manager }   -> base^"markermanager/1.1/src/markermanager_packed.js"
    { Marker_tracker }   -> base^"markertracker/1.0/src/markertracker_packed.js"

  _tool_before_overlay(js_obj:string, t) =
    match t:Gmaps.tool
    { Marker_clusterer } -> "var markers = []; "
    { Marker_manager }   -> "var mgr = new MarkerManager({js_obj}); "
    _                    -> ""

  _tool_to_overlay_fun(t) =
    match t:Gmaps.tool
    { Marker_clusterer } -> _add_overlay_clusterer
    { Marker_manager }   -> _add_overlay_manager
    { Marker_tracker }   -> _add_overlay_tracker
    _                    -> _add_overlay_normal

  _tool_after_overlay(js_obj:string, t) =
    match t:Gmaps.tool
    { Marker_clusterer } -> "new MarkerClusterer({js_obj}, markers); "
    _                    -> ""

/*
 * ---------- final formatting ----------
 */
  _lauch_options(p:Gmaps.map) =
    size = _addSizeOpt(p.size)
    types = _addMapTypes(p.maptypes)
    [] |> (l -> if size == "" then l else List.cons("size: {size}", l))
       |> (l -> if types == "" then l else List.cons("mapTypes : {types}", l))
       |> _json_generic("", ", \{", "}", _)

  _build_id(name) =
    name
    |>  String.replace(" ", "_", _)
    |>  String.replace(".", "_", _)

/**
 * Generates the javascript code corresponding to given map
 */
  _load_map(first, other_ids, key:string, container:string, p:Gmaps.map) =
    js_obj = _build_id(container)
    js_tmp = js_obj^"tmp"

    load_text = if first == false then ""
    else
      other_loading = other_ids
        |> List.map(_build_id, _)
        |> List.map((x -> "GmapsOpa_{x}_loadTool();"), _)
        |> (l -> List.fold((el,acc -> acc^el), l, ""))
      "function GmapsOpa_{js_obj}_loadScript() \{
       var s = document.createElement('script'); s.type = 'text/javascript';
       s.src = 'http://maps.google.com/maps?file=api&v=2&async=2&callback={
         if p.tool=={No_tool} then "GmapsOpa_{js_obj}_loadMap" else "GmapsOpa_{js_obj}_loadTool"
       }&key={key}'; document.body.appendChild(s);}

       function what_to_load() \{ GmapsOpa_{js_obj}_loadScript(); {other_loading} };
       if (typeof jQuery != 'undefined')
         \{ $('#{js_obj}').ready(what_to_load) }
       else
         \{ window.onload=what_to_load };"

  "var {js_obj};
    function GmapsOpa_{js_obj}_loadMap() \{
      {js_obj} = new GMap2(document.getElementById('{container}'){_lauch_options(p)});
      {js_obj}.setCenter({_latlng_to_string(p.center)}, {p.zoom});
      {if p.dragging_enabled then "" else "{js_obj}.disableDragging();"}
      {_add_controls(js_obj, p.controls)} {_tool_before_overlay(js_obj, p.tool)}
      var {js_tmp};{_add_overlays( js_obj, js_tmp, _tool_to_overlay_fun(p.tool), p.overlays)}
      {_tool_after_overlay(js_obj, p.tool)}
    }

    function GmapsOpa_try_{js_obj}_loadMap() \{
      try \{ GmapsOpa_{js_obj}_loadMap(); }
      catch (e) \{ setTimeout(GmapsOpa_try_{js_obj}_loadMap, 200); }
    }

    function GmapsOpa_{js_obj}_loadTool() \{
      { if p.tool=={No_tool} then "GmapsOpa_try_{js_obj}_loadMap()"
        else
          "var scripts=document.getElementsByTagName('script'), add=true, i,
           src=\"{_tool_to_source(p.tool)}\";
           for(i in scripts) if (scripts[i].src == src) add=false;
           if(add) \{
             var s=document.createElement('script'); s.type='text/javascript'; s.src=src;
             document.body.appendChild(s); setTimeout(GmapsOpa_try_{js_obj}_loadMap, 200);
           } else GmapsOpa_try_{js_obj}_loadMap();"
       }
    }
    {load_text}"

/**
 * Bridge between the public and the private part of this API.
 */
  generic_map_elt(first, ids, key, id, map) =
    <div id={id}></div>
    <script type="text/javascript">
      {Xhtml.of_string_unsafe("//<![CDATA[\n{_load_map(first, ids, key, id, map)}\n//]]>")}
    </script>

}}

Gmaps = {{

  default_map =
  { center = {lat=48.8259695; lng=2.3846613}:Gmaps.LatLng /** Mlstate offices in April 2010 */
  ; zoom = 14
  ; size = { some = { x = 600; y = 400 }:Gmaps.size }
  ; dragging_enabled = true
  ; controls = []
  ; overlays = []
  ; maptypes = []
  ; tool = { No_tool }
  }:Gmaps.map

/* Functions for end user */

/**
 * Adds a map in the page
 *
 * This function should be used only if there is only one map on the page.
 * If multiple mas are added with this function, only the last one will load
 * because of javascript limitations.
 *
 * @param key The API key provided by Google (http://code.google.com/apis/maps/signup.html). If incorrect, a javascript alert will pop when the map loads but it still will work.
 * @param id id of the div which will be created for the map. Each id must be unique in a valid xhtml page.
 * @param map A Gmaps.map object representing the map you want to add. Since this object is complex, it is recommended to take Gmaps.default_map and to modify its parameters.
 */
  single_map_elt(key, id, map) = Gmaps_private.generic_map_elt(true, [], key, id, map)

/**
 * Adds a map in the page
 *
 * This function should be used if there is more than one map on the page. All
 * ids given to this function must have a map added in (when the function
 * is called or later on the page).
 * The other maps should be added with Gmaps.more_map_elt.
 * Note: This function must be called once, but can be placed before,
 * after or in the middle of the calls of Gmaps.more_map_elt.
 *
 * @param key The API key provided by Google (http://code.google.com/apis/maps/signup.html). If incorrect, a javascript alert will pop when the map loads but it still will work.
 * @param id The id of the div which will be created for the map. Each id must be unique in a valid xhtml page.
 * @param ids List of the ids where the other maps will be added.
 * @param map A Gmaps.map object representing the map you want to add. Since this object is complex, it is recommended to take Gmaps.default_map and to modify its parameters.
 */
  map_elt(key, id, ids, map) = Gmaps_private.generic_map_elt(true, ids, key, id, map)

/**
 * Adds a map in the page
 *
 * This function should be used only if there is more than one map on the page.
 * It creates a div with given id in which will be added a map. If the id given to this
 * function is not passed to a Gmaps.map_elt call, the map will not load.
 *
 * @param key The API key provided by Google (http://code.google.com/apis/maps/signup.html). If incorrect, a javascript alert will pop when the map loads but it still will work.
 * @param id id of the div which will be created for the map. Each id must be unique in a valid xhtml page.
 * @param map A Gmaps.map object representing the map you want to add. Since this object is complex, it is recommended to take Gmaps.default_map and to modify its parameters.
 */
  more_map_elt(key, id, map) = Gmaps_private.generic_map_elt(false, [], key, id, map)

}}
