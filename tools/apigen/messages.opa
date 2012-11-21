/* This file creates binary protocol handler code, it generates pack/unpack
 * functions and send/receive functions which use them.
 * Currently uses the old codegen, will be updated to use the new one in the future.
 */

function exp efield_size_code(field_size fs) {
  match (fs) {
    case {byte}: erecsimple("B");
    case {short}: erecsimple("S");
    case {long}: erecsimple("L");
    case {longlong}: erecsimple("Ll");
  }
}

function proformas(config, pts) {
  match (pts) {
  case []: eempty;
  case [ty]: field_type_proforma_type(config,ty);
  default: etup(List.map(field_type_proforma_type(config,_),pts));
  }
}

function exp field_type_proforma_type(config, field_type ft) {
  match (ft) {
    case {char}: estr("_");
    case {unsigned:_, ...}: eint(0);
    case {int:_, ...}: eint(0);
    case {cstring}: estr("");
    case ~{binary:_, ...}: efa(eid("Binary.create"),[eint(0)]);
    case {list:_, size:_, le:_}: evb("[]");
    case {ntlist:_}: evb("[]");
    case ~{tyname}:
      match (lookup_message(config, tyname)) {
      case {some:msg}: efa(eid("pack_{tyname}"),[proformas(config,message_types(config, msg.fields))])
      case {none}: eempty;
      }
  }
}

function exp field_type_proforma(config config, field_type ft) {
  match (ft) {
    case {char}: erecs([("Char","\"_\"")]);
    case {unsigned:{byte},...}: erecs([("Byte","0"),("signed","false")]);
    case {unsigned:{short},~le}: erecs([("Short","0"),("signed","false"),("le","{le}")]);
    case {unsigned:{long},~le}: erecs([("Long","0"),("signed","false"),("le","{le}")]);
    case {unsigned:{longlong},~le}: erecs([("Longlong","0"),("signed","false"),("le","{le}")]);
    case {int:{byte},...}: erecs([("Byte","0")]);
    case {int:{short},~le}: erecs([("Short","0"),("le","{le}")]);
    case {int:{long},~le}: erecs([("Long","0"),("le","{le}")]);
    case {int:{longlong},~le}: erecs([("Longlong","0"),("le","{le}")]);
    case {cstring}: erecs([("Cstring","\"\"")]);
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        erecr([("Binary",{v:"Binary.create(0)"}),("size",{e:efield_size_code(binary)})])
      else
        erecs([("FixedBinary","({size},Binary.create(0))")]);
    case ~{binary:_, fixed:{to_end}}:
      erecs([("Binary","Binary.create(0)"),("size","\{none}")])
    case ~{list, size:_, le:_}: erecs([("List","([{field_type_proforma(config,list)}],[])")]);
    case ~{ntlist}: erecs([("List","([{field_type_proforma(config,ntlist)}],[])"),("null","\{B}")]);
    case ~{tyname}:
      match (lookup_message(config, tyname)) {
      case {some:msg}: erecr([("Pack",{e:efa(eid("pack_{tyname}"),[proformas(config,message_types(config, msg.fields))])})]);
      case {none}: eempty;
      }
  }
}

function exp field_type_to_data(field_type ft) {
  match (ft) {
    case {char}:
      efn([pid("c")], [sexp(elst([erec([eex("Char",eid("c"))])]))]);
    case {unsigned:{byte},...}:
      efn([pid("b")], [sexp(elst([erec([eex("Byte",eid("b")),eex("signed",efalse)])]))]);
    case {unsigned:{short},~le}:
      efn([pid("s")], [sexp(elst([erec([eex("Short",eid("s")),eex("signed",efalse),eex("le",ebool(le))])]))]);
    case {unsigned:{long},~le}:
      efn([pid("l")], [sexp(elst([erec([eex("Long",eid("l")),eex("signed",efalse),eex("le",ebool(le))])]))]);
    case {unsigned:{longlong},~le}:
      efn([pid("ll")], [sexp(elst([erec([eex("Longlong",eid("ll")),eex("signed",efalse),eex("le",ebool(le))])]))]);
    case {int:{byte},...}:
      efn([pid("b")], [sexp(elst([erec([eex("Byte",eid("b"))])]))]);
    case {int:{short},~le}:
      efn([pid("s")], [sexp(elst([erec([eex("Short",eid("s")),eex("le",ebool(le))])]))]);
    case {int:{long},~le}:
      efn([pid("l")], [sexp(elst([erec([eex("Long",eid("l")),eex("le",ebool(le))])]))]);
    case {int:{longlong},~le}:
      efn([pid("ll")], [sexp(elst([erec([eex("Longlong",eid("ll")),eex("le",ebool(le))])]))]);
    case {cstring}:
      efn([pid("cs")], [sexp(elst([erec([eex("Cstring",eid("cs"))])]))]);
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        efn([pid("b")], [sexp(elst([erec([eex("Binary",eid("b")),eex("size",efield_size_code(binary))])]))])
      else
        efn([pid("b")], [sexp(elst([erec([eex("FixedBinary",evb("({size},b)"))])]))]);
    case ~{binary:_, fixed:{to_end}}:
      efn([pid("b")], [sexp(elst([erec([eex("Binary",eid("b")),epl("no_prefix")])]))]);
    case {list:_, size:_, le:_}: @fail("No nested lists")
    case {ntlist:_}: @fail("No nested lists")
    case ~{tyname}:
      efn([pid("v")], [sexp(elst([erec([eex("Pack",eid("pack_{tyname}(v)"))])]))]);
  }
}

function exp field_type_record(config config, field_type ft, string val) {
  val = evb(val)
  match (ft) {
    case {char}: erec([eex("Char",val)]);
    case {unsigned:{byte},...}: erec([eex("Byte",val),eex("signed",efalse)]);
    case {unsigned:{short},~le}: erec([eex("Short",val),eex("signed",efalse),eex("le",evb("{le}"))]);
    case {unsigned:{long},~le}: erec([eex("Long",val),eex("signed",efalse),eex("le",evb("{le}"))]);
    case {unsigned:{longlong},~le}: erec([eex("Longlong",val),eex("signed",efalse),eex("le",evb("{le}"))]);
    case {int:{byte},...}: erec([eex("Byte",val)]);
    case {int:{short},~le}: erec([eex("Short",val),eex("le",evb("{le}"))]);
    case {int:{long},~le}: erec([eex("Long",val),eex("le",evb("{le}"))]);
    case {int:{longlong},~le}: erec([eex("Longlong",val),eex("le",evb("{le}"))]);
    case {cstring}: erec([eex("Cstring",val)]);
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        erec([eex("Binary",val),eex("size",efield_size_code(binary))])
      else
        erec([eex("FixedBinary",etup([evb("{size}"),val]))]);
    case ~{binary:_, fixed:{to_end}}:
        erec([eex("Binary",val),epl("no_prefix")])
    case ~{list, size, le}:
      erec([eex("List",etup([elst([field_type_proforma(config,list)]),
                             efa(eid("List.map"),[field_type_to_data(list),val])])),
            eex("size",efield_size_code(size)),eex("le",evb("{le}"))]);
    case ~{ntlist}:
      erec([eex("List",etup([elst([field_type_proforma(config,ntlist)]),
                             efa(eid("List.map"),[field_type_to_data(ntlist),val])])),
            eex("null",evb("\{B}"))]);
    case ~{tyname}:
      erec([eex("Pack",efa(eid("pack_{tyname}"),[val]))]);
  }
}

function pack_field(config config, field field, string lenval) {
  match (field) {
    case {const:~{typ, val}}: field_type_record(config,typ,val);
    case {param:~{name, typ, ...}}: field_type_record(config,typ,name);
    case ~{length, offset}:
      if (lenval == "")
        erec([eex("Reference",eid("length"))])
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
        params = [etup(List.map(eid,message_names(config, msg.fields)))]
        erec([eex("Pack",efa(eid("pack_{msg.name}"),params))]);
      case {none}: eempty;
      }
  }
}

function string doc_string(field_param param, string forb) {
  if (forb == "B")
    "@param {param.name} ({string_of_field_type(param.typ)}): {Option.get(param.doc)}"
  else
    "@return {param.name} ({string_of_field_type(param.typ)}): {Option.get(param.doc)}"
}

function block gen_packs(config config) {
  List.map(function (msg) {
    match (msg.fields) {
    case []: sempty;
    case fields:
      length = get_length(fields)
      params = [ptup(List.map(pid,message_names(config, fields)))]
      docs =
        match ((msg.docs,list(field_param) message_docs(config, fields))) {
        case ([],[]): sempty;
        case (_,params): sdocs(List.append(msg.docs,List.map(doc_string(_,"B"),params)));
        }
      lst = elst(List.map(function (field) { pack_field(config,field,"") },fields))
      func =
        sfn("pack_{msg.name}",params,empty_t,
            if (Option.is_some(length))
               [asgn(pid("length"),efa(eid("ServerReference.create"),[elst([pack_field(config,Option.get(length),"0")])])),
                asgn(pid("data"),lst),
                sexp(edo(efa(eid("ServerReference.set"),
                             [eid("length"),elst([pack_field(config,Option.get(length),"Pack.Encode.packlen(data)")])]))),
                sexp(eid("data"))]
             else [sexp(lst)])
      sblock([docs,func])
    }
  },List.filter(is_message_frontend, config.messages))
}

function exp get_ip(string fn, list(exp) preargs, list(exp) postargs, bool single, bool noip) {
  if (single)
    if (noip)
      eid(fn)
    else
      efa(eid(fn),List.flatten([preargs,[eid("input")],postargs]))
  else
    if (noip)
      efa(eid(fn),List.flatten([preargs,[ewild],postargs]))
    else
      efa(eid(fn),List.flatten([preargs,[eid("input")],postargs]))
}

function exp field_type_unser_func(config config, field_type ft, string _val, bool noip) { // TODO: verify val for consts
  ip = get_ip(_, _, _, _, noip)
  match (ft) {
    case {char}: ip("U.char",[],[],true);
    case {unsigned:{byte},...}: ip("U.uoctet",[],[],true);
    case {unsigned:{short},~le}: ip("U.ushort_{string_of_le(le)}",[],[],true);
    case {unsigned:{long},~le}: ip("U.ulong_{string_of_le(le)}",[],[],true);
    case {unsigned:{longlong},~le}: ip("U.ulonglong_{string_of_le(le)}",[],[],true);
    case {int:{byte},...}: ip("U.octet",[],[],true);
    case {int:{short},~le}: ip("U.short_{string_of_le(le)}",[],[],true);
    case {int:{long},~le}: ip("U.long_{string_of_le(le)}",[],[],true);
    case {int:{longlong},~le}: ip("U.longlong_{string_of_le(le)}",[],[],true);
    case {cstring}: ip("U.cstring",[],[],true);
    case ~{binary, fixed:{~size}}:
      if (size <= 0)
        ip("U.binary",[eid(pack_of_le(get_le(config))), efield_size_code(binary)], [], false)
      else
        ip("U.fixed_binary",[],[eint(size)],false);
    case ~{binary:_, fixed:{to_end}}:
        ip("U.binary_no_prefix",[],[],false);
    case ~{list, size, le}:
      ip("U.list",[field_type_unser_func(config,list,"",noip), eid(pack_of_le(le)), efield_size_code(size)], [], false);
    case ~{ntlist}: ip("U.ntlist",[field_type_unser_func(config,ntlist,"",noip), evb("\{B}")], [], false);
    case ~{tyname}: ip("unpack_{tyname}",[],[],true);
  }
}

function exp unpack_field(config config, field field, bool noip) {
  match (field) {
  case {const:~{typ, val}}: field_type_unser_func(config,typ,val,noip);
  case {param:~{name, typ, ...}}: field_type_unser_func(config,typ,name,noip);
  case ~{length, offset}: field_type_unser_func(config,{int:length, le:get_le(config)},"/*!!!offset={offset}!!!*/",noip)
  case {~pack}:
    match (lookup_message(config, pack)) {
    case {some:msg}: get_ip("unpack_{msg.name}",[],[],false,noip);
    case {none}: evb("/*!!!missing message {pack}!!!*/");
    }
  }
}

function pat unpack_pat(config config, field field) {
  match (field) {
  case {const:~{typ:_, val}}: pvb(val);
  case {param:~{name, typ:_, ...}}: pid(name);
  case ~{length:_, offset:_}: pwild;
  case {~pack}:
    match (lookup_message(config, pack)) {
    case {some:msg}: unpack_pats(config, msg.fields)
    case {none}: pvb("/*!!!missing message {pack}!!!*/");
    }
  }
}

function pat unpack_pats(config config, fields fields) {
  match (fields) {
  case []: pvoid;
  case [field]: unpack_pat(config, field);
  case fields: ptup(List.map(unpack_pat(config,_),fields));
  }
}

function block gen_unpacks(config config) {
  List.map(function (msg) {
    match (msg.fields) {
    case []:
      docs = sdocs(msg.docs)
      func =
        sfn("unpack_{msg.name}",[ptid("input",tyname("Pack.input"))],empty_t,
            [sexp(esuccess(etup([eid("input"),evoid])))])
      sblock([docs,func])
    case fields:
      docs =
        match ((msg.docs,list(field_param) message_docs(config, fields))) {
        case ([],[]): sempty;
        case (_,params): sdocs(List.append(msg.docs,List.map(doc_string(_,"F"),params)));
        }
      mas =
        match (fields) {
        case []: evoid;
        case [field]: unpack_field(config, field, false);
        case fields:
          arity = List.length(fields)
          efa(eid("U.tuple{arity}"),[eid("input")|List.map(unpack_field(config,_,true),fields)]);
        }
      p1 = unpack_pats(config, fields)
      e1 =
        match (message_names(config, List.filter(is_param,fields))) {
        case []: evoid;
        case [name]: eid(name);
        case names: etup(List.map(eid,names));
        }
      pes =
        List.flatten([[(psuccess(ptup([pid("input"),p1])),
                        esuccess(etup([eid("input"),e1])))],
                      if (List.exists(is_const,fields))
                        [succw2fail(evb("\"Bad constant in unpack_{msg.name}\""))]
                      else
                        [],
                      [failt2failt]
                     ])
      func =
        sfn("unpack_{msg.name}",[ptid("input",tyname("Pack.input"))],empty_t,
            [sexp(emtch(mas,pes))])
      sblock([docs,func])
    }
  },List.filter(is_message_backend, config.messages))
}

function string code_doc_string(code code, string forb) {
  if (forb == "B")
    "@param {code.message_name} ({String.trim(code.code)}): {Option.get(code.doc)}"
  else
    "@return {code.message_name} ({String.trim(code.code)}): {Option.get(code.doc)}"
}

function block gen_uncodes(config config) {
  List.map(function (apicoded coded) {
    match (coded.codes) {
    case []: sempty;
    case _:
      code_docs = List.filter_map(function (code) { if (Option.is_some(code.doc)) {some:code} else none },coded.codes)
      docs =
        match (List.append(coded.docs,List.map(code_doc_string(_,"F"),code_docs))) {
        case []: sempty;
        case docs: sdocs(docs);
        }
      mas = field_type_unser_func(config, coded.code_type, "", false)
      pes =
        List.flatten([
          List.map(function (code code) {
                     (psuccess(ptup([pid("input"),pvb(String.trim(code.code))])),
                      emtch(efa(eid("unpack_{code.message_name}"),[eid("input")]),
                            [(psuccess(ptup([pid("input"),pid(code.message_name)])),
                              esuccess(etup([eid("input"),erectsimple("{code.message_name}")]))),
                             failt2failt]))
                   },coded.codes),
          [succ2fail(ptup([pwild,pid("code")]),estr("Bad code \{code} in unpack_{coded.name}")),
           failt2failt]])
      func = sfn("unpack_{coded.name}",[ptid("input",tyname("Pack.input"))],empty_t,[sexp(emtch(mas,pes))])
      sblock([docs, func])
    }
  },List.filter(is_coded_backend, config.coded))
}

function block gen_connection(config config) {
  major_version = get_int_parameter(config,"major_version",-1)
  minor_version = get_int_parameter(config,"minor_version",-1)
  length =
    List.flatten([if (get_parameter(config,"length.offset","") != "")
                    [eex("offset",eint(get_int_parameter(config,"length.offset",-1)))] else [],
                  if (get_parameter(config,"length.le","") != "")
                    [eex("le",ebool(get_bool_parameter(config,"length.le",false)))] else [],
                  if (get_parameter(config,"length.signed","") != "")
                    [eex("signed",ebool(get_bool_parameter(config,"length.signed",false)))] else [],
                  if (get_parameter(config,"length.size","") != "")
                    [eex("size",erecsimple(get_parameter(config,"length.size","L")))] else []
                 ])
  [sblnk,
   sdoc("Default connection for this driver"),sblnk,
   asgn(pid("default_host"),evb(get_parameter(config,"default_host","(\"localhost\",8080)"))),
   if (major_version != -1) asgn(pid("default_major_version"),eint(major_version)) else sempty,
   if (minor_version != -1) asgn(pid("default_minor_version"),eint(minor_version)) else sempty,sblnk,
   sdoc("Build the connection module"),sblnk,
   asgn(pid("Conn"),efa(eid("ApilibConnection"),[eid("default_host")])),sblnk,
   sdoc("Open connection object (doesn't connect until read/write received)"),sblnk,
   sfn("connect",[ptid("name",string_t),ptid("secure",typopt(tyname("SSL.secure_type")))],
                 tyname("Apigen.outcome(ApigenLib.connection)"),
       [asgn(pid("conn"),efa(eid("Conn.init"),[estr(get_parameter(config,"connection_name","default")),
                                               eid("name"),eid("secure")])),
        if (List.length(length) > 0) {
          asgn(pid("conn"),efa(eid("Conn.custom_read_packet"),
                               [eid("conn"),
                                efa(eid("Conn.read_packet_prefixed"),
                                    [ewrec(eid("Conn.default_length"),length)])]))
        } else sempty,
        sexp(efa(eid("Conn.connect"),[eid("conn"),eid("default_host")]))]),sblnk,
   sdoc("Close connection (closes all open connections in pool)"),sblnk,
   sfn("close",[ptid("c",tyname("Apigen.outcome(ApigenLib.connection)"))],tyname("Apigen.outcome(ApigenLib.connection)"),
       [sexp(emtch(eid("c"),[succ2succ(pid("c"),efa(eid("Conn.close"),[eid("c")])),
                             failt2failt]))])]
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
  case {coded:_}: [];
  case {~message}: if (List.length(List.filter(is_param,message.fields)) == 0) [] else [pid("params")];
  case {none}: [pid("missing message")];
  }
}

function block gen_sndrcv(config config) {
  ptypconn = ptid("conn",tyname("Apigen.outcome(ApigenLib.connection)"))
  List.map(function (sr) {
    match ((sr.snd,sr.rcv)) {
    case ("",""): sempty;
    case (snd,""):
      match (lookup_message_or_coded(config, snd)) {
      case {none}: sempty;
      case snd:
        doc = match (sr.doc) { case {some:doc}: sdoc(doc); case {none}: sempty; }
        params = mcparams(snd)
        name = mcname(snd)
        func =
          if (params == []) {
            [spvt,
             asgn(pid("packed_{name}"),
                  emtch(efa(eid("Pack.Encode.pack"),[efa(eid("Pg.pack_{name}"),[])]),
                        [(psuccess(pid("binary")),eid("binary")),
                         (pfailuret,evb("@fail(\"Failed to pre-pack message {name} \{failure}\")"))])),
             sfn(sr.name,List.append([ptypconn],params),empty_t,
                 [if (get_int_parameter(config,"debug",0) > 0) sexp(edo(evb("jlog(\"{sr.name}\")"))) else sempty,
                  sexp(efa(eid("Pg.Conn.snd"),[eid("conn"),eid("packed_{name}")]))])
            ]
          } else {
            [
             sfn(sr.name,List.append([ptypconn],params),empty_t,
                 [if (get_int_parameter(config,"debug",0) > 0) sexp(edo(evb("jlog(\"{sr.name}\")"))) else sempty,
                  sexp(emtch(efa(eid("Pack.Encode.pack"),[efa(eid("Pg.pack_{name}"),[eid("params")])]),
                        [(psuccess(pid("binary")),efa(eid("Pg.Conn.snd"),[eid("conn"),eid("binary")])),
                         (pfailuret,efailure(erecr([("pack",{e:eid("failure")})])))]))
                 ])
            ]
          }
        sblock(List.append([doc],func))
      }
    case ("",rcv):
      match (lookup_message_or_coded(config, rcv)) {
      case {none}: sempty;
      case rcv:
        name = mcname(rcv)
        doc = match (sr.doc) { case {some:doc}: sdoc(doc); case {none}: sempty; }
        func =
          sfn(sr.name,[ptypconn],empty_t,
              [smtch(efa(eid("Pg.Conn.rcv"),[eid("conn")]),
                     [(psuccess(pid("binary")),
                       eblk([if (get_int_parameter(config,"debug",0) > 0)
                               sexp(edo(evb("jlog(\"{name}: reply=\\n\{bindump(binary)}\")")))
                             else sempty,
                             smtch(efa(eid("Pg.unpack_{name}"),[erecs([("binary","~"),("pos","0")])]),
                                   [(psuccess(pid("reply")),
                                     if (get_bool_parameter(config, "check_all_input_consumed", false))
                                       eite(evb("reply.f1.pos == Binary.length(binary)"),
                                            esuccess(eid("reply.f2")),
                                            efailure(erecr([("pack",{e:estr("Unused input (\{reply.f1} bytes)")})])))
                                     else
                                       esuccess(eid("reply.f2"))),
                                    (pfailuret,efailure(erecr([("pack",{e:eid("failure")})])))])])),
                      failt2failt])])
        sblock([doc,func])
      }
    case (snd,rcv):
      match ((lookup_message_or_coded(config, snd),lookup_message_or_coded(config, rcv))) {
      case ({none},_): sempty;
      case (_,{none}): sempty;
      case (snd,rcv):
        doc = match (sr.doc) { case {some:doc}: sdoc(doc); case {none}: sempty; }
        func =
          sfn(sr.name,[ptid("conn",tyname("Apigen.outcome(ApigenLib.connection)")),pid("params")],empty_t,
              [asgn(pid("data"),efa(eid("Pg.pack_{mcname(snd)}"),[eid("params")])),
               smtch(efa(eid("Pack.Encode.pack"),[eid("data")]),
                     [(psuccess(pid("binary")),
                       emtch(efa(eid("Pg.Conn.sndrcv"),[eid("conn"),eid("binary")]),
                             [(psuccess(pid("binary")),
                               emtch(efa(eid("Pg.unpack_{mcname(rcv)}"),[erecr([("binary",{t}),("pos",{e:eint(0)})])]),
                                     [(psuccess(pid("reply")),
                                      if (get_bool_parameter(config, "check_all_input_consumed", false))
                                        eite(evb("reply.f1.pos == Binary.length(binary)"),
                                             esuccess(eid("reply.f2")),
                                             efailure(erecr([("pack",{e:estr("Unused input (\{reply.f1} bytes)")})])))
                                      else
                                        esuccess(eid("reply.f2")))]))]))])])
        sblock([doc,func])
      }
    }
  },config.sndrcv)
}

function block gen_messages(config config) {
  List.flatten([gen_packs(config),
                gen_unpacks(config),
                gen_uncodes(config),
                if (get_bool_parameter(config,"connection",false)) gen_connection(config) else [],
                gen_sndrcv(config)])
}

