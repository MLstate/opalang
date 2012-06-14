
type Pack.private.options = void

PackOptions = {{

  @private CL = CommandLine

  default_pack_opts = void

  args = {
    title = "Pack options"
    init = default_pack_opts
    anonymous = []
    parsers = [
      CL.switch(["--pack-debug"],
                "Enable debug output"
               )(p -> do ServerReference.set(Pack.debug,true) p),

      CL.case(["--default-endian"],
              [("little",Pack.littleEndian),
               ("big",   Pack.bigEndian)],
              "Default endian", "little, big"
             )(endian,p -> do ServerReference.set(Pack.defaultEndian,endian) p),

      CL.case(["--default-ints"],
              [("signed",  Pack.signedInts),
               ("unsigned",Pack.unsignedInts)],
              "Default ints", "signed, unsigned"
             )(signed,p -> do ServerReference.set(Pack.defaultInts,signed) p),

      CL.case(["--default-size"],
              [("byte",    Pack.sizeByte),
               ("short",   Pack.sizeShort),
               ("long",    Pack.sizeLong),
               ("longlong",Pack.sizeLonglong)],
              "Default size","byte, short, long, longlong"
             )(size,p -> do ServerReference.set(Pack.defaultSize,size) p),
    ]
  } : CommandLine.family(Pack.private.options)

  pack_opts = CL.filter(args)

}}

