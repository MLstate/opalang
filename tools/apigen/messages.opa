/* This file creates binary protocol handler code, it generates pack/unpack
 * functions and send/receive functions which use them.
 * Currently uses the old codegen, will be updated to use the new one in the future.
 */

function field_size_number(field_size fs) {
  match (fs) {
    case {byte}: "8";
    case {short}: "16";
    case {long}: "32";
    case {longlong}: "64";
  }
}

function field_size_code(field_size fs) {
  match (fs) {
    case {byte}: "\{B}";
    case {short}: "\{S}";
    case {long}: "\{L}";
    case {longlong}: "\{Ll}";
  }
}

function field_type_proforma_type(config, field_type ft) {
  match (ft) {
    case {char}: "\"_\"";
    case {unsigned:_, ...}: "0";
    case {int:_, ...}: "0";
    case {cstring}: "\"\"";
    case ~{binary:_, ...}: "Binary.create(0)";
    case {list:_, size:_, le:_}: "[]";
    case {ntlist:_}: "[]";
    case ~{tyname}:
      match (lookup_message(config, tyname)) {
      case {some:msg}:
        pts = message_types(config, msg.fields)
        proformas =
          match (pts) {
          case []: "";
          case [ty]: field_type_proforma_type(config,ty);
          default: List.to_string_using("(",")",",",List.map(field_type_proforma_type(config,_),pts));
          }
        "pack_{tyname}({proformas})"
      case {none}: "";
      }
  }
}

function field_type_proforma(config config, field_type ft) {
  match (ft) {
    case {char}: record([("Char","\"_\"")]);
    case {unsigned:{byte},...}: record([("Byte","0"),("signed","false")]);
    case {unsigned:{short},~le}: record([("Short","0"),("signed","false"),("le","{le}")]);
    case {unsigned:{long},~le}: record([("Long","0"),("signed","false"),("le","{le}")]);
    case {unsigned:{longlong},~le}: record([("Longlong","0"),("signed","false"),("le","{le}")]);
    case {int:{byte},...}: record([("Byte","0")]);
    case {int:{short},~le}: record([("Short","0"),("le","{le}")]);
    case {int:{long},~le}: record([("Long","0"),("le","{le}")]);
    case {int:{longlong},~le}: record([("Longlong","0"),("le","{le}")]);
    case {cstring}: record([("Cstring","\"\"")]);
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        record([("Binary","Binary.create(0)"),("size",field_size_code(binary))])
      else
        record([("FixedBinary","({size},Binary.create(0))")]);
    case ~{binary:_, fixed:{to_end}}:
      record([("Binary","Binary.create(0)"),("size","\{none}")])
    case ~{list, size:_, le:_}: record([("List","([{field_type_proforma(config,list)}],[])")]);
    case ~{ntlist}: record([("List","([{field_type_proforma(config,ntlist)}],[])"),("null","\{B}")]);
    case ~{tyname}:
      match (lookup_message(config, tyname)) {
      case {some:msg}:
        pts = message_types(config, msg.fields)
        proformas =
          match (pts) {
          case []: "";
          case [ty]: field_type_proforma_type(config,ty);
          default: List.to_string_using("(",")",",",List.map(field_type_proforma_type(config,_),pts));
          }
        record([("Pack","pack_{tyname}({proformas})")]);
      case {none}: "";
      }
  }
}

function field_type_to_data(field_type ft) {
  match (ft) {
    case {char}: func("c", "[{record([("Char","c")])}]");
    case {unsigned:{byte},...}: func("b", "[{record([("Byte","b"),("signed","false")])}]");
    case {unsigned:{short},~le}: func("s", "[{record([("Short","s"),("signed","false"),("le","{le}")])}]");
    case {unsigned:{long},~le}: func("l", "[{record([("Long","l"),("signed","false"),("le","{le}")])}]");
    case {unsigned:{longlong},~le}: func("ll", "[{record([("Longlong","ll"),("signed","false"),("le","{le}")])}]");
    case {int:{byte},...}: func("b", "[{record([("Byte","b")])}]");
    case {int:{short},~le}: func("s", "[{record([("Short","s"),("le","{le}")])}]");
    case {int:{long},~le}: func("l", "[{record([("Long","l"),("le","{le}")])}]");
    case {int:{longlong},~le}: func("ll", "[{record([("Longlong","ll"),("le","{le}")])}]");
    case {cstring}: func("cs", "[{record([("Cstring","cs")])}]");
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        func("b", "[{record([("Binary","b"),("size",field_size_code(binary))])}]")
      else
        func("b", "[{record([("FixedBinary","({size},b)")])}]");
    case ~{binary:_, fixed:{to_end}}:
      func("b", "[{record([("Binary","b"),("no_prefix","")])}]")
    case {list:_, size:_, le:_}: @fail("No nested lists")
    case {ntlist:_}: @fail("No nested lists")
    case ~{tyname}: func("v", "[{record([("Pack","pack_{tyname}(v)")])}]");
  }
}

function field_type_record(config config, field_type ft, string val) {
  match (ft) {
    case {char}: record([("Char",val)]);
    case {unsigned:{byte},...}: record([("Byte",val),("signed","false")]);
    case {unsigned:{short},~le}: record([("Short",val),("signed","false"),("le","{le}")]);
    case {unsigned:{long},~le}: record([("Long",val),("signed","false"),("le","{le}")]);
    case {unsigned:{longlong},~le}: record([("Longlong",val),("signed","false"),("le","{le}")]);
    case {int:{byte},...}: record([("Byte",val)]);
    case {int:{short},~le}: record([("Short",val),("le","{le}")]);
    case {int:{long},~le}: record([("Long",val),("le","{le}")]);
    case {int:{longlong},~le}: record([("Longlong",val),("le","{le}")]);
    case {cstring}: record([("Cstring",val)]);
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        record([("Binary",val),("size",field_size_code(binary))])
      else
        record([("FixedBinary","({size},{val})")]);
    case ~{binary:_, fixed:{to_end}}:
        record([("Binary",val),("no_prefix","")])
    case ~{list, size, le}:
      record([("List","([{field_type_proforma(config,list)}],List.map({field_type_to_data(list)},{val}))"),
              ("size",field_size_code(size)),("le","{le}")]);
    case ~{ntlist}:
      record([("List","([{field_type_proforma(config,ntlist)}],List.map({field_type_to_data(ntlist)},{val}))"),
              ("null","\{B}")]);
    case ~{tyname}:
      record([("Pack","pack_{tyname}({val})")]);
  }
}

function pack_field(config config, field field, string lenval) {
  match (field) {
    case {const:~{typ, val}}: field_type_record(config,typ,val);
    case {param:~{name, typ, ...}}: field_type_record(config,typ,name);
    case ~{length, offset}:
      if (lenval == "")
        record([("Reference","length")])
      else
        offset =
          match (offset) {
          case {some:offset}: if (offset < 0) "{offset}" else "+{offset}";
          case {none}: "";
          }
        field_type_record(config,{int:length, le:get_le(config)},"{lenval}{offset}")
    case {~pack}:
      match (lookup_message(config, pack)) {
      case {some:msg}:
        params = String.concat(",",message_names(config, msg.fields))
        record([("Pack","pack_{msg.name}({params})")]);
      case {none}: "";
      }
  }
}

function string doc_string(field_param param, string forb) {
  if (forb == "B")
    "@param {param.name} ({string_of_field_type(param.typ)}): {Option.get(param.doc)}"
  else
    "@return {param.name} ({string_of_field_type(param.typ)}): {Option.get(param.doc)}"
}

function output_packs(config config) {
  List.iter(function (msg) {
    match (msg.fields) {
    case []: void;
    case fields:
      //jlog("fields:{fields}");
      params =
        match (message_names(config, fields)) {
        case []: "";
        case [name]: name;
        case names: List.to_string_using("(",")",",",names);
        }
      length = get_length(fields)
      match ((msg.docs,list(field_param) message_docs(config, fields))) {
      case ([],[]): void;
      case (_,params):
        println(List.to_string_using("/**\n * ","\n */","\n * ",List.append(msg.docs,List.map(doc_string(_,"B"),params))));
      }
      print(func_start("  ","pack_{msg.name}",params,""))
      match (length) {
        case {some:length}: println("    length = ServerReference.create([{pack_field(config,length,"0")}])\n    data = ")
        case {none}: void;
      }
      println("    [")
      println(String.concat(",\n",List.map(function (field) { "     "^pack_field(config,field,"") },fields)))
      println("    ]")
      match (length) {
        case {some:length}:
          f = "ServerReference.set(length,[{pack_field(config,length,"Pack.Encode.packlen(data)")}])"
          println("    {dofun(f)}\n    data")
        case {none}: void;
      }
      print(func_end("  "))
    }
  },List.filter(is_message_frontend, config.messages))
}

function get_ip(bool single, bool noip) {
  if (single)
    if (noip) "" else "(input)"
  else
    if (noip) "_" else "input"
}

function field_type_unser_func(config config, field_type ft, string _val, bool noip) { // TODO: verify val for consts
  ip = get_ip(_, noip)
  match (ft) {
    case {char}: "U.char{ip(true)}";
    case {unsigned:{byte},...}: "U.uoctet{ip(true)}";
    case {unsigned:{short},~le}: "U.ushort_{string_of_le(le)}{ip(true)}";
    case {unsigned:{long},~le}: "U.ulong_{string_of_le(le)}{ip(true)}";
    case {unsigned:{longlong},~le}: "U.ulonglong_{string_of_le(le)}{ip(true)}";
    case {int:{byte},...}: "U.octet{ip(true)}";
    case {int:{short},~le}: "U.short_{string_of_le(le)}{ip(true)}";
    case {int:{long},~le}: "U.long_{string_of_le(le)}{ip(true)}";
    case {int:{longlong},~le}: "U.longlong_{string_of_le(le)}{ip(true)}";
    case {cstring}: "U.cstring{ip(true)}";
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        "U.binary({pack_of_le(get_le(config))}, {field_size_code(binary)}, {ip(false)})"
      else
        "U.fixed_binary({ip(false)},{size})";
    case ~{binary:_, fixed:{to_end}}:
        "U.binary_no_prefix({ip(false)})";
    case ~{list, size, le}:
      "U.list({field_type_unser_func(config,list,"",noip)}, {pack_of_le(le)}, {field_size_code(size)}, {ip(false)})";
    case ~{ntlist}: "U.ntlist({field_type_unser_func(config,ntlist,"",noip)}, \{B}, {ip(false)})";
    case ~{tyname}: "unpack_{tyname}{ip(true)}";
  }
}

function unpack_field(config config, field field, bool noip) {
  match (field) {
  case {const:~{typ, val}}: field_type_unser_func(config,typ,val,noip);
  case {param:~{name, typ, ...}}: field_type_unser_func(config,typ,name,noip);
  case ~{length, offset}: field_type_unser_func(config,{int:length, le:get_le(config)},"/*!!!offset={offset}!!!*/",noip)
  case {~pack}:
    match (lookup_message(config, pack)) {
    case {some:msg}: "unpack_{msg.name}{get_ip(false,noip)}";
    case {none}: "/*!!!missing message {pack}!!!*/";
    }
  }
}

function unpack_pat(config config, field field) {
  match (field) {
  case {const:~{typ:_, val}}: val;
  case {param:~{name, typ:_, ...}}: name;
  case ~{length:_, offset:_}: "_";
  case {~pack}:
    match (lookup_message(config, pack)) {
    case {some:msg}: unpack_pats(config, msg.fields)
    case {none}: "/*!!!missing message {pack}!!!*/";
    }
  }
}

function unpack_pats(config config, fields fields) {
  match (fields) {
  case []: "\{}";
  case [field]: unpack_pat(config, field);
  case fields: List.to_string_using("(",")",",",List.map(unpack_pat(config,_),fields));
  }
}

function output_unpacks(config config) {
  List.iter(function (msg) {
    match (msg.fields) {
    case []:
      if (msg.docs != []) println(List.to_string_using("/**\n * ","\n */","\n * ",msg.docs))
      print(func_start("  ","unpack_{msg.name}",typed("Pack.input","input"),""))
      println("    {record([("success","(input,\{})")])}")
      print(func_end("  "))
    case fields:
      docs = List.append(msg.docs,List.map(doc_string(_,"F"),message_docs(config, fields)))
      if (docs != []) println(List.to_string_using("/**\n * ","\n */","\n * ",docs))
      print(func_start("  ","unpack_{msg.name}",typed("Pack.input","input"),""))
      mas =
        match (fields) {
        case []: "\{}";
        case [field]: unpack_field(config, field, false);
        case fields:
          arity = List.length(fields)
          List.to_string_using("U.tuple{arity}(input,",")",",",List.map(unpack_field(config,_,true),fields));
        }
      p1 = unpack_pats(config, fields)
      e1 =
        match (message_names(config, List.filter(is_param,fields))) {
        case []: "\{}";
        case [name]: name
        case names: List.to_string_using("(",")",",",names);
        }
      pes =
        List.flatten([[(record([("success","(input,{p1})")]),record([("success","(input,{e1})")]))],
                      if (List.exists(is_const,fields))
                        [(record([("success","_")]),record([("failure","\"Bad constant in unpack_{msg.name}\"")]))]
                      else
                        [],
                      [(record([("~failure","")]),record([("~failure","")]))]])
      print(mtch(false,"    ",mas,pes))
      print(func_end("  "))
    }
  },List.filter(is_message_backend, config.messages))
}

function string code_doc_string(code code, string forb) {
  if (forb == "B")
    "@param {code.message_name} ({String.trim(code.code)}): {Option.get(code.doc)}"
  else
    "@return {code.message_name} ({String.trim(code.code)}): {Option.get(code.doc)}"
}

function output_uncodes(config config) {
  List.iter(function (apicoded coded) {
    match (coded.codes) {
    case []: void;
    case _:
      code_docs = List.filter_map(function (code) { if (Option.is_some(code.doc)) {some:code} else none },coded.codes)
      docs = List.append(coded.docs,List.map(code_doc_string(_,"F"),code_docs))
      if (docs != []) println(List.to_string_using("/**\n * ","\n */","\n * ",docs))
      print(func_start("  ","unpack_{coded.name}",typed("Pack.input","input"),""))
      mas = field_type_unser_func(config, coded.code_type, "", false)
      pes =
        List.append(
          List.map(function (code code) {
                     mas = "unpack_{code.message_name}(input)"
                     rexp = record([("{code.message_name}","~")])
                     pes = [(record([("success","(input,{code.message_name})")]),
                             record([("success","(input,{rexp})")])),
                            (record([("~failure","")]),record([("~failure","")]))]
                     (record([("success","(input,{String.trim(code.code)})")]),
                      mtch(true,"      ",mas,pes))
                   },coded.codes),
          [(record([("success","(_,code)")]),record([("failure","\"Bad code \{code} in unpack_{coded.name}\"")])),
           (record([("~failure","")]),record([("~failure","")]))])
      print(mtch(false,"    ",mas,pes))
      print(func_end("  "))
    }
  },List.filter(is_coded_backend, config.coded))
}

function output_connection(config config) {
  print("  /** Default connection for this driver */\n")
  print("  default_host = {get_parameter(config,"default_host","(\"localhost\",8080)")}\n")
  major_version = get_int_parameter(config,"major_version",-1)
  if (major_version != -1) print("  default_major_version = {major_version}\n")
  minor_version = get_int_parameter(config,"minor_version",-1)
  if (minor_version != -1) print("  default_minor_version = {minor_version}\n\n")
  print("  /** Build the connection module */\n")
  print("  Conn = ApilibConnection(default_host)\n\n")
  print("  /** Open connection object (doesn't connect until read/write received) */\n")
  print(func_start("  ","connect",typed("string","name"),"Apigen.outcome(ApigenLib.connection)"))
  print("    conn = Conn.init(\"{get_parameter(config,"connection_name","default")}\",name)\n")
  length =
    List.flatten([if (get_parameter(config,"length.offset","") != "")
                    [recel("offset","{get_int_parameter(config,"length.offset",-1)}")] else [],
                  if (get_parameter(config,"length.le","") != "")
                    [recel("le","{get_bool_parameter(config,"length.le",false)}")] else [],
                  if (get_parameter(config,"length.signed","") != "")
                    [recel("signed","{get_bool_parameter(config,"length.signed",false)}")] else [],
                  if (get_parameter(config,"length.size","") != "")
                    [recel("size","\{{get_parameter(config,"length.size","L")}}")] else []
                 ])
  if (List.length(length) > 0) {
    print("    conn = Conn.custom_read_packet(conn, Conn.read_packet_prefixed(\{Conn.default_length with\n")
    print("      {String.concat("{recsep()}\n      ",length)}")
    print("\n    }))\n")
  }
  print("    Conn.connect(conn, default_host)")
  print(func_end("  "))
  print("\n  /** Close connection (closes all open connections in pool) */")
  print(func_start("\n  ","close",typed("Apigen.outcome(ApigenLib.connection)","c"),"Apigen.outcome(ApigenLib.connection)"))
  print(mtch(false,"    ","c",[
               (record([("success","c")]),record([("success","Conn.close(c)")])),
               (record([("failure","~")]),record([("failure","~")]))
             ]))
  print(func_end("  "))
}

function mcname(morc) {
  match (morc) {
  case {~coded}: coded.name;
  case {~message}: message.name;
  case {none}: "name missing";
  }
}

function mcparams(morc) {
  match (morc) {
  case {coded:_}: ("","");
  case {~message}: if (List.length(List.filter(is_param,message.fields)) == 0) ("","") else (",","params");
  case {none}: ("","missing message");
  }
}

function output_sndrcv(config config) {
  List.iter(function (sr) {
    match ((sr.snd,sr.rcv)) {
    case ("",""): void;
    case (snd,""):
      match (lookup_message_or_coded(config, snd)) {
      case {none}: void;
      case snd:
        Option.iter(function (doc) { println("  /** {doc} */") },sr.doc)
        (comma,params) = mcparams(snd)
        name = mcname(snd)
        if (params == "") {
          print("  {prvt()} packed_{name} =\n")
          println(mtch(false,"    ","Pack.Encode.pack(Pg.pack_{name}())",[
            (record([("success","binary")]),"binary"),
            (record([("failure","~")]),"@fail(\"Failed to pre-pack message {name} \{failure}\")")
          ]))
          print(func_start("  ",sr.name,"{typed("Apigen.outcome(ApigenLib.connection)","conn")}{comma}{params}",""))
          if (get_int_parameter(config,"debug",0) > 0) print("    {dofun("jlog(\"{sr.name}\")")}\n")
          print("    Pg.Conn.snd(conn,packed_{name})\n")
          print(func_end("  "))
        } else {
          print(func_start("  ",sr.name,"{typed("Apigen.outcome(ApigenLib.connection)","conn")}{comma}{params}",""))
          if (get_int_parameter(config,"debug",0) > 0) print("    {dofun("jlog(\"{sr.name}\")")}\n")
          print("    data = Pg.pack_{name}({params})\n")
          print(mtch(false,"    ","Pack.Encode.pack(data)",[
            (record([("success","binary")]),"Pg.Conn.snd(conn,binary)"),
            (record([("failure","~")]),record([("failure",record([("pack","failure")]))]))
          ]))
          print(func_end("  "))
        }
      }
    case ("",rcv):
      match (lookup_message_or_coded(config, rcv)) {
      case {none}: void;
      case rcv:
        name = mcname(rcv)
        Option.iter(function (doc) { println("  /** {doc} */") },sr.doc)
        print(func_start("  ",sr.name,"{typed("Apigen.outcome(ApigenLib.connection)","conn")}",""))
        dbg =
          if (get_int_parameter(config,"debug",0) > 0)
            "\n      "^dofun("jlog(\"{name}: reply=\\n\{bindump(binary)}\")")
          else ""
        mch =
          mtch(true,"      ","Pg.unpack_{name}({record([("binary","~"),("pos","0")])})",[
            (record([("success","reply")]),
             if (get_bool_parameter(config, "check_all_input_consumed", false))
               ite(true,"        ",
                   "reply.f1.pos == Binary.length(binary)",
                   record([("success","reply.f2")]),
                   record([("failure",record([("pack","\"Unused input (\{reply.f1} bytes)\"")]))]))
             else
               record([("success","reply.f2")])),
            (record([("failure","~")]),record([("failure",record([("pack","failure")]))]))
          ])
        print(mtch(false,"    ","Pg.Conn.rcv(conn)",[
          (record([("success","binary")]),dbg^mch),
          (record([("failure","~")]),record([("failure","~")]))
        ]))
        print(func_end("  "))
      }
    case (snd,rcv):
      match ((lookup_message_or_coded(config, snd),lookup_message_or_coded(config, rcv))) {
      case ({none},_): void;
      case (_,{none}): void;
      case (snd,rcv):
        Option.iter(function (doc) { println("  /** {doc} */") },sr.doc)
        print(func_start("  ",sr.name,"{typed("Apigen.outcome(ApigenLib.connection)","conn")},params",""))
        print("    data = Pg.pack_{mcname(snd)}(params)\n")
        print(mtch(false,"    ","Pack.Encode.pack(data)",[
          (record([("success","binary")]),
           mtch(true,"      ","Pg.Conn.sndrcv(conn,binary)",[
             (record([("success","binary")]),
              mtch(true,"        ","Pg.unpack_{mcname(rcv)}({record([("binary","~"),("pos","0")])})",[
                  (record([("success","reply")]),
                   if (get_bool_parameter(config, "check_all_input_consumed", false))
                     ite(true,"        ",
                         "reply.f1.pos == Binary.length(binary)",
                         record([("success","reply.f2")]),
                         record([("failure",record([("pack","\"Unused input (\{reply.f1} bytes)\"")]))]))
                   else
                     record([("success","reply.f2")])),
                  (record([("failure","~")]),record([("failure",record([("pack","failure")]))]))
              ])),
             (record([("failure","~")]),record([("failure","~")]))
           ])),
          (record([("failure","~")]),record([("failure",record([("pack","failure")]))]))
        ]))
        print(func_end("  "))
      }
    }
  },config.sndrcv)
}

function output_messages(config config) {
  output_packs(config)
  output_unpacks(config)
  output_uncodes(config)
  if (get_bool_parameter(config,"connection",false)) output_connection(config)
  output_sndrcv(config)
}

