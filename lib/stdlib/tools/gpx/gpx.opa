 /** Measure of a distance in space */
type Gpx.distance = { meters : float }

 /** Geographic latitude */
type Gpx.latitude = float

 /** Geographic longitude */
type Gpx.longitude = float

 /** Geographical location in space, consisting of longitude and latitude */
type Gpx.location = { latitude : Gpx.latitude; longitude : Gpx.longitude }

 /** Bounding box, specified by upper-left (ul) and bottom-down (bd) corners */
type Gpx.bounds = { ul : Gpx.location; bd : Gpx.location }

 /** Single GPS measurement consisting of the time at which it was taken,
     latitude, longitude and optional elevation */
type Gpx.entry = { time: Date.date
                 ; location : Gpx.location
                 ; elevation : option(Gpx.distance)
                 }

type Gpx.trace = { started_at : Date.date
                 ; bounds : option(Gpx.bounds)
                 ; trace : list(Gpx.entry)
                 }

Gpx = {{

  @private gpx_date_scanner = Date.generate_scanner("%Y-%m-%dT%H:%M:%SZ")

  @private xmlns = "http://www.topografix.com/GPX/1/1"

  @private float = Rule.float
  @private xml_string = xml_parser s=(.*) -> Text.to_string(s)
  @private xml_float = xml_parser f={xml_string} -> Float.of_string(f)

  @private time =
    xml_parser <time xmlns={xmlns}>time={xml_string}</>
      -> Date.of_formatted_string(gpx_date_scanner, time) ? error("Wrong date: {time}")

  @private metadata =
    bounds = xml_parser <bounds xmlns={xmlns} minlat={float} minlon={float}
      maxlat={float} maxlon={float} />
      ->
      minlat = String.to_float("{minlat}")
      minlon = String.to_float("{minlon}")
      maxlat = String.to_float("{maxlat}")
      maxlon = String.to_float("{maxlon}")
      { ul = { latitude=minlat; longitude=minlon }
      ; bd = { latitude=maxlat; longitude=maxlon }
      }
    xml_parser
      <metadata xmlns={xmlns}>
        (<name xmlns={xmlns}>_*</> -> void)?
        (<desc xmlns={xmlns}>_*</> -> void)?
        (<author xmlns={xmlns}>_*</> -> void)?
        (<copyright xmlns={xmlns}>_*</> -> void)?
        (<link xmlns={xmlns}>_*</> -> void)*
        time={time}
        (<keywords xmlns={xmlns}>_*</> -> void)?
        bounds={bounds}?
        (<extensions xmlns={xmlns}>_*</> -> void)?
      </>
      -> ~{time bounds}

  @private trkseg =
    elevation =
      xml_parser <ele xmlns={xmlns} >elevation={xml_float}</>
        -> {meters=elevation}
    trk_pt = xml_parser
      <trkpt xmlns={xmlns} lat={float} lon={float}>
        elevation={elevation}?
        time={time}
      </>
      ->
      latitude  = String.to_float("{lat}")
      longitude = String.to_float("{lon}")
      location : Gpx.location = ~{ latitude; longitude }
      ~{ location elevation time }
    xml_parser
      <trkseg xmlns={xmlns}>
        route={trk_pt}*
      </>
      -> route

  @private trk =
    xml_parser
      <trk xmlns={xmlns}>
        (<name xmlns={xmlns}>_*</> -> void)?
        (<cmt xmlns={xmlns}>_*</> -> void)?
        (<desc xmlns={xmlns}>_*</> -> void)?
        (<src xmlns={xmlns}>_*</> -> void)?
        (<link xmlns={xmlns}>_*</> -> void)*
        (<number xmlns={xmlns}>_*</> -> void)?
        (<type xmlns={xmlns}>_*</> -> void)?
        (<extensions xmlns={xmlns}>_*</> -> void)?
        trksegs={trkseg}*
      </>
      -> List.flatten(trksegs)

   // TODO: convert from UTC to local
   /**
    * An XML parser for GPX data
    */
  gpx_parser =
    xml_parser
      <gpx xmlns={xmlns}>
        metadata={metadata}
        (<wpt xmlns={xmlns}>_*</> -> void)*
        (<rte xmlns={xmlns}>_*</> -> void)*
        trk={trk}
        (<extensions xmlns={xmlns}>_*</> -> void)?
      </>
    ->
      { started_at=metadata.time
      ; bounds=metadata.bounds
      ; trace=trk
      } : Gpx.trace

}}
