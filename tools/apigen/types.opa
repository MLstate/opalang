
type style = {js_like} or {classic}

type verb = {GET} or {POST}

function string_of_verb(verb verb) {
  match (verb) {
    case {GET}: "GET";
    case {POST}: "POST";
  }
}

type typ0('a) = {unknown} or {int} or {string} or {bool} or {float} or {string tyname} or {'a list}
type typ = typ0(typ)

function string_of_typ(config config, typ typ) {
  match (typ) {
    case {unknown}: "RPC.Json.json";
    case {int}: "int";
    case {string}: "string";
    case {bool}: "bool";
    case {float}: "float";
    case {~tyname}: "{config.name}.{tyname}";
    case {~list}: "list({string_of_typ(config, list)})";
  }
}

function dflt_typ_of_typ(typ typ, bool opt) {
  function option(v) { if (opt) { "option({v})" } else v }
  match (typ) {
    case {unknown}: @fail("dflt_typ_of_typ: unknown")
    case {int}: option("int");
    case {string}: option("string");
    case {bool}: option("bool");
    case {float}: option("float");
    case {~tyname}: @fail("dflt_typ_of_typ: {tyname}")
    case {~list}: @fail("dflt_typ_of_typ: list {list}")
  }
}

type param = {
  bool optional,
  string name,
  typ typ
}

type return_type = {OBJECT} or {LIST} or {BOOL} or {INT}

type result =
    {{ return_type return_type, string tyname } parse}

type apidef = {
  string name,
  string path,
  string error_field,
  verb verb,
  list(param) params,
  option(result) result
}

apidef null_def = {name:"", path:"/", error_field:"", verb:{GET}, params:[], result:none}

type apitype = {
  string name,
  list(param) params
}

apitype null_type = {name:"", params:[]}

type field_size = {byte} or {short} or {long} or {longlong}

function string_of_field_size(field_size fs) {
  match (fs) {
  case {byte}: "byte";
  case {short}: "short";
  case {long}: "long";
  case {longlong}: "longlong";
  }
}

function pack_of_le(lb) {
  match (lb) {
  case {true}: "Pack.littleEndian";
  case {false}: "Pack.bigEndian";
  }
}

function string_of_le(le) {
  match (le) {
  case {true}: "le";
  case {false}: "be";
  }
}

type fixed = {int size} or {to_end}

type field_type0('a) =
    {char}
 or {field_size unsigned, bool le}
 or {field_size int, bool le}
 or {cstring}
 or {field_size binary, fixed fixed}
 or {'a list, field_size size, bool le}
 or {'a ntlist}
 or {string tyname}

type field_type = field_type0(field_type)

function string_of_field_type(field_type ft) {
  match (ft) {
  case {char}: "char";
  case ~{unsigned, ...}: "unsigned {string_of_field_size(unsigned)}";
  case {~int, ...}: "{string_of_field_size(int)}";
  case {cstring}: "string";
  case {binary:_, ...}: "binary";
  case {~list, ...}: "{string_of_field_type(list)} list";
  case {~ntlist}: "{string_of_field_type(ntlist)} list";
  case {~tyname}: tyname;
  }
}

type field_param = {
  string name,
  field_type typ,
  option(string) doc
}

type field =
    {{field_type typ, string val} const}
 or {field_param param}
 or {field_size length, option(int) offset}
 or {string pack}

type fields = list(field)

type f_b = {F} or {B}
type fb = list(f_b)

type apimessage = {
  string name,
  fb fb,
  fields fields,
  list(string) docs
}

apimessage null_message = {name:"", fb:[], fields:[], docs:[]}

type code = {
  string code,
  string message_name,
  option(string) doc
}

type apicoded = {
  string name,
  fb fb,
  field_type code_type,
  list(code) codes,
  list(string) docs
}

apicoded null_coded = {name:"", fb:[], code_type:{cstring}, codes:[], docs:[]}

type apisndrcv = {
  string name,
  string snd,
  string rcv,
  option(string) doc
}

type config = {
  string name,
  list(apidef) defs,
  list(apitype) types,
  list(apimessage) messages,
  list(apicoded) coded,
  list(apisndrcv) sndrcv,
  list(string) extcode,
  list(string) docs,
  list(string) copyright,
  stringmap(string) parameters
}

type mode = {start} or {def} or {parse} or {types} or {messages} or {coded} or {sndrcv} or {extcode}

config null_config = {
  name:"default",
  defs:[],
  types:[],
  messages:[],
  coded:[],
  sndrcv:[],
  extcode:[],
  docs:[],
  copyright:[],
  parameters:StringMap.empty
}

function get_parameter(config config, string name, string default_value) {
  List.fold(function (name,value) {
    match (StringMap.get(name, config.parameters)) {
    case {some:value}: value;
    case {none}: value;
    }
  },[String.lowercase(name),String.uppercase(name),String.capitalize(name)],default_value)
}

function get_parsed_parameter(config config, string name, default_value, parse) {
  match (Parser.try_parse(parser { case val=parse: val; },get_parameter(config, name, "invalid parameter"))) {
  case {some:val}: val;
  case {none}: default_value;
  }
}

function get_converted_parameter(config config, string name, 'a default_value, (string -> 'a) of_string) {
  match (get_parameter(config, name, "invalid parameter")) {
  case "invalid parameter": default_value;
  case valstr: of_string(valstr);
  }
}

get_bool_parameter = get_parsed_parameter(_, _, _, Rule.bool)
get_int_parameter = get_parsed_parameter(_, _, _, Rule.integer)

function get_le(config config) { get_bool_parameter(config,"endianness",false) }
function get_oauth(config config) { get_bool_parameter(config,"oauth",false) }

function lookup_message(config config, string name) { List.find(function (msg) { msg.name == name },config.messages) }

function lookup_coded(config config, string name) { List.find(function (coded) { coded.name == name },config.coded) }

function lookup_message_or_coded(config config, string name) {
  match (lookup_message(config, name)) {
  case {some:message}: {~message};
  case {none}:
    match (lookup_coded(config, name)) {
    case {some:coded}: {~coded};
    case {none}: {none};
    }
  }
}

function is_param(field field) {
  match (field) {
  case {param:_}: true
  default: false
  }
}

function is_const(field field) {
  match (field) {
  case {const:_}: true
  default: false
  }
}

function message_vals(config config, fields fields, f) {
  List.fold_backwards(function (field, acc) {
                        match (field) {
                        case {~pack}:
                          match (lookup_message(config, pack)) {
                          case {some:msg}: List.append(message_vals(config, msg.fields, f),acc);
                          case {none}: acc;
                          }
                        default: f(field,acc);
                        }
                      },fields,[])
}

message_names =
  message_vals(_,_,function (field field,acc) { match (field) { case {param:~{name, ...}}: [name|acc]; default: acc; } })
message_types =
  message_vals(_,_,function (field field,acc) { match (field) { case {param:~{typ, ...}}: [typ|acc]; default: acc; } })
message_docs =
  message_vals(_,_,function (field field,acc) { match (field) {
                                                case {~param}: if (Option.is_some(param.doc)) [param|acc] else acc;
                                                default: acc; } })

function get_length(fields fields) {
  List.fold(function (field field, length_opt) {
              match (length_opt) {
              case {some:_}: length_opt;
              case {none}:
                match (field) {
                case ~{length, offset}: {some:~{length, offset}};
                default: length_opt;
                }
              }
            },fields,{none})
}

function param_name(field field) {
  match (field) {
  case {param:~{name, ...}}: name;
  default: "bad name";
  }
}

function is_message_backend(apimessage message) {
  List.exists(function (fb) { match (fb) { case {B}: true; case {F}: false } },message.fb)
}

function is_message_frontend(apimessage message) {
  List.exists(function (fb) { match (fb) { case {B}: false; case {F}: true } },message.fb)
}

function is_coded_backend(apicoded coded) {
  List.exists(function (fb) { match (fb) { case {B}: true; case {F}: false } },coded.fb)
}

