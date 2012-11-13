
function dbg(where) {
  parser {
    Rule.debug_parse_string(function (s) {
      dots = if (String.length(s) > 20) { "..." } else ""
      s = String.sub(0,Int.min(20,String.length(s)),s)
      s = String.escape_non_utf8_special(s)
      jlog("{where}: {s}{dots}")
    }): void
  }
}

kw_apiname = parser { case ("APINAME"|"apiname"): void }
kw_path = parser { case ("PATH"|"path"): void }
kw_error_field = parser { case ("ERROR_FIELD"|"error_field"): void }
kw_message = parser { case ("MESSAGE"|"message"): void }
kw_coded = parser { case ("CODED"|"coded"): void }
kw_snd = parser { case ("SEND"|"SND"): void }
kw_rcv = parser { case ("RECEIVE"|"RCV"): void }
kw_sndrcv = parser { case ("SENDRECEIVE"|"SNDRCV"): void }
kw_extcode = parser { case ("EXTCODE"|"extcode"): void }
kw_parameter = parser { case ("PARAMETER"|"parameter"): void }
kw_type = parser { case ("TYPE"|"type"): void }
kw_call = parser { case ("CALL"|"call"): void }
kw_parse = parser { case ("PARSE"|"parse"): void }
kw_end = parser { case ("END"|"end"): void }
kw_opt = parser { case ("OPT"|"opt"): void }
kw_copyright = parser { case ("COPYRIGHT"|"copyright"): void }

function to_eol(name) {
  parser {
    case name Rule.ws val=Rule.full_line: val;
  }
}

verb = parser {
  case "GET": {GET}
  case "POST": {POST}
}

typ = parser {
  case "unknown": {unknown}
  case "int": {int}
  case "string": {string}
  case "bool": {bool}
  case "float": {float}
  case tyname=([A-Za-z0-9._]+): {tyname:Text.to_string(tyname)}
  case "list" Rule.ws "(" Rule.ws ~typ Rule.ws ")": typ {list:typ}
}

id = parser {
  case id=([\-A-Za-z0-9_]+ "."? [\-A-Za-z0-9_]*): Text.to_string(id);
}

ws_or_eol = parser {
  case Rule.ws: void;
  case Rule.eol: void;
}

return_type = parser {
  case "OBJECT": return_type {OBJECT};
  case "LIST": return_type {LIST};
  case "BOOL": return_type {BOOL};
  case "INT": return_type {INT};
}

field_size = parser {
  case ("8"|"byte"|"B"): field_size {byte};
  case ("16"|"short"|"S"): field_size {short};
  case ("32"|"long"|"L"): field_size {long};
  case ("64"|"longlong"|"Ll"): field_size {longlong};
}

le = parser {
  case ("little"|"le"|"l"|"littleendian"|"little_endian"): true;
  case ("big"|"be"|"b"|"bigendian"|"big_endian"): false;
}

size_or_fixed = parser {
  case /*dbg("size")  */ size=field_size: (field_size, fixed) (size,{size:0});
  case /*dbg("fixed") */ "fixed" Rule.ws size=Rule.natural: (field_size, fixed) ({long},{~size});
  case /*dbg("to_end")*/ "to_end": (field_size, fixed) ({long},{to_end});
}

function field_type(config config) {
  parser {
  case /*dbg("char")    */ "char": field_type {char};
  case /*dbg("unsigned")*/ "unsigned" unsigned=field_size Rule.ws ~le?: field_type ~{unsigned, le:le?get_le(config)};
  case /*dbg("int")     */ "int" int=field_size Rule.ws ~le?: field_type {~int, le:le?get_le(config)};
  case /*dbg("cstring") */ "cstring": field_type {cstring};
  case /*dbg("binary")  */ "binary" Rule.ws sf=size_or_fixed: field_type {binary:sf.f1, fixed:sf.f2};
  case /*dbg("list")    */ "list" size=field_size Rule.ws list=field_type(config) Rule.ws ~le?:
    field_type ~{list, size, le:le?get_le(config)};
  case /*dbg("ntlist")  */ "ntlist" Rule.ws ntlist=field_type(config): field_type ~{ntlist};
  case /*dbg("tyname")  */ tyname=id: field_type ~{tyname};
  }
}

offset = parser {
  case "offset" Rule.ws offset=Rule.integer: offset;
}

docstart = parser {
  case ("/**"): void;
}

doc = parser {
  case doc=to_eol(docstart): doc;
}

function field(config config) {
  parser {
  case /*dbg("const") */ "const" Rule.ws typ=field_type(config) Rule.ws val=Rule.full_line: field {const:~{typ, val}};
  case /*dbg("param") */ "param" Rule.ws typ=field_type(config) Rule.ws name=id Rule.ws ~doc?: field {param:~{name, typ, doc}};
  case /*dbg("length")*/ "length" length=field_size Rule.ws ~offset? Rule.ws: field ~{length, offset};
  case /*dbg("pack")  */ "pack" Rule.ws pack=id: field {~pack};
  }
}

f_b = parser {
  case ("F"|"Frontend"|"Server"|"S"): f_b {F};
  case ("B"|"Backend"|"Client"|"C"): f_b {B};
}

fb = parser {
  case fb=f_b*: fb fb;
}

message_line = parser {
  case kw_message Rule.ws ~fb Rule.ws message=Rule.full_line: ~{message, fb}
}

function coded_line(config config) {
  parser {
  case kw_coded Rule.ws ~fb Rule.ws name=id Rule.ws code_type=field_type(config): ~{code_type, fb, name}
  }
}

commstart = parser { case ("//"|"#"): void; }

nondoc = parser { case txt=((!docstart .)*): Text.to_string(txt) }

code = parser {
  case "code" Rule.ws message_name=id Rule.ws code=nondoc Rule.ws ~doc?: code ~{code, message_name, doc};
}

sndrcv = parser {
  case kw_sndrcv Rule.strict_ws name=id Rule.strict_ws snd=id Rule.strict_ws rcv=id Rule.ws ~doc?: ~{name, snd, rcv, doc};
  case kw_snd Rule.strict_ws name=id Rule.strict_ws snd=id Rule.ws ~doc?: ~{name, snd, rcv:"", doc};
  case kw_rcv Rule.strict_ws name=id Rule.strict_ws rcv=id Rule.ws ~doc?: ~{name, snd:"", rcv, doc};
}

nonws = parser { case txt=((!Rule.ws .)*) Rule.ws?: Text.to_string(txt) }

function line_parser(config config) {
  parser {
    case /*dbg("COMMENT")       */ comment=to_eol(commstart) Rule.ws: {~comment}
    case /*dbg("PARAMETER")     */ kw_parameter Rule.ws parameter=id Rule.ws value=Rule.full_line Rule.ws: ~{parameter, value}
    case /*dbg("DOC")           */ doc=to_eol(docstart) Rule.ws: {~doc}
    case /*dbg("COPYRIGHT")     */ copyright=to_eol(kw_copyright) Rule.ws: {~copyright}
    case /*dbg("APINAME")       */ apiname=to_eol(kw_apiname) Rule.ws: {~apiname}
    case /*dbg("TYPE")          */ kw_type Rule.ws typename=id Rule.ws: {~typename}
    case /*dbg("MESSAGE")       */ ~message_line: message_line
    case /*dbg("CODED")         */ ~coded_line(config): coded_line
    case /*dbg("CODE")          */ ~code: {~code}
    case /*dbg("SNDRCV")        */ ~sndrcv: {~sndrcv}
    case /*dbg("PATH")          */ path=to_eol(kw_path) Rule.ws: {~path}
    case /*dbg("ERROR_FIELD")   */ error_field=to_eol(kw_error_field) Rule.ws: {~error_field}
    case /*dbg("CALL")          */ kw_call Rule.ws name=id Rule.ws ~verb Rule.ws: ~{verb, name}
    case /*dbg("PARSE")         */ kw_parse Rule.ws ~return_type Rule.ws ~id?: {parse:~{return_type, tyname:id?""}}
    case /*dbg("FIELD")         */ ~field(config) Rule.ws: {~field}
    case /*dbg("opt? name type")*/ opt=(kw_opt Rule.ws)? name=id Rule.ws typ=typ: ~{optional:Option.is_some(opt), name, typ};
    case /*dbg("EXTCODE")       */ kw_extcode: {extcode};
    case /*dbg("END")           */ kw_end: {END};
    case /*dbg("dead")          */ dead=(.*): ~{dead}
  }
}

function l1l((config -> list('a)) getc, (config, list('a) -> config) setc, mode new_mode)(
             config config, mode old_mode, ('a -> 'a) update) {
  match (getc(config)) {
  case [e|es]: (new_mode,setc(config,[update(e)|es]))
  default: (old_mode, config);
  }
}

l1l_defs = l1l(function (config) { config.defs },function (config, defs) { {config with ~defs} }, {def})
l1l_messages = l1l(function (config) { config.messages },function (config, messages) { {config with ~messages} }, {messages})
l1l_types = l1l(function (config) { config.types },function (config, types) { {config with ~types} }, {types})
l1l_coded = l1l(function (config) { config.coded },function (config, coded) { {config with ~coded} }, {coded})

function add_end(e, l) {
  match (l) {
  case []: [e];
  case [h|t]: [h|add_end(e,t)];
  }
}

function get_line(line, (mode mode,config config)) {
  //jlog("get_line({mode}): {line}")
  if (mode == {extcode}) {
    match (Parser.try_parse(parser { case "END": void; },line)) {
    case {some:_}: ({start},config);
    case {none}: ({extcode},{config with extcode:[line|config.extcode]});
    }
  } else {
    prs = Parser.try_parse(line_parser(config),line)
    //jlog("prs: {prs}")
    res =
    match ((mode,prs)) {
    case (_,{some:{comment:_}}): (mode,config);
    case (_,{some:~{parameter, value}}): (mode,{config with parameters:StringMap.add(parameter,value,config.parameters)});
    case (_,{some:{END}}): ({start},config);
    case (_,{some:{~copyright}}): (mode, {config with copyright:add_end(copyright,config.copyright)});
    case (_,{some:{~doc}}):
      match (mode) {
      case {start}: (mode, {config with docs:add_end(doc,config.docs)});
      case {messages}: l1l_messages(config, mode, function (msg) { {msg with docs:add_end(doc,msg.docs)} });
      case {coded}: l1l_coded(config, mode, function (codes) { {codes with docs:add_end(doc,codes.docs)} });
      default: (mode,config);
      };
    case ({start},{some:{extcode}}):
      ({extcode}, config);
    case ({start},{some:~{apiname}}):
      ({start},{config with name:apiname});
    case ({start},{some:~{typename}}):
      ({types},{config with types:[~{null_type with name:typename}|config.types]});
    case ({start},{some:~{message, fb}}):
      ({messages},{config with messages:[~{null_message with name:message, fb}|config.messages]});
    case ({start},{some:~{code_type, fb, name}}):
      ({coded},{config with coded:[~{null_coded with code_type, name, fb}|config.coded]});
    case ({start},{some:~{sndrcv}}):
      ({start},{config with sndrcv:add_end(sndrcv,config.sndrcv)});
    case ({start},{some:~{verb, name}}):
      ({def},{config with defs:[~{null_def with name, verb}|config.defs]});
    case ({def},{some:~{path}}):
      l1l_defs(config, mode, function (def) { {def with ~path} });
    case ({def},{some:~{error_field}}):
      l1l_defs(config, mode, function (def) { {def with ~error_field} });
    case ({def},{some:~{optional, name, typ}}):
      l1l_defs(config, mode, function (def) { {def with params:[~{optional, name, typ}]} });
    case ({messages},{some:~{field}}):
      l1l_messages(config, mode, function (msg) { {msg with fields:[field|msg.fields]} });
    case ({coded},{some:~{code}}):
      l1l_coded(config, mode, function (codes) { {codes with codes:add_end(code,codes.codes)} });
    case ({def},{some:{parse:~{return_type, tyname}}}):
      match (config.defs) {
      case [def|defs]:
        match (def.result) {
        case {some:_}: @fail("multiple return types")
        case {none}: ({def},{config with defs:[{def with result:{some:{parse:~{return_type, tyname}}}}|defs]});
        }
      default: (mode, config);
      };
    case ({types},{some:~{optional, name, typ}}):
      l1l_types(config, mode, function (ty) { {ty with params:[~{optional, name, typ}|ty.params]} });
    case (_,{some:~{dead}}):
      jlog("dead: {dead}"); (mode,config);
    default:
      jlog("default: {line}"); (mode,config);
    }
    //jlog("get_line: res={res}")
    res
  }
}

function config parseConfig(string str) {
  lines = String.explode("\n",str)
  List.fold(get_line,lines,({start},null_config)).f2
}

function read_config() {
  config_file = config_file.get()
  match (File.read_opt(config_file)) {
  case {some:bin}:
    config = parseConfig(string_of_binary(bin))
    // Ridiculous, fix the parser...
    config = {config with defs:List.rev(config.defs), types:List.rev(config.types), messages:List.rev(config.messages)}
    config = {config with defs:List.map(function (def) { {def with params:List.rev(def.params)} },config.defs)}
    config = {config with types:List.map(function (typ) { {typ with params:List.rev(typ.params)} },config.types)}
    config = {config with messages:List.map(function (msg) { {msg with fields:List.rev(msg.fields)} },config.messages)}
    config
  case {none}: @fail("Can't read file {config_file}")
  }
}

