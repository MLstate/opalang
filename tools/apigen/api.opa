/*
This file is out of date and incomplete, it generates twitter/facebook style apis
but has been neglected in favour of the binary protocol generator in messages.opa.
It will be updated and finished at some time in the future.
*/

function opt_of_typ(param param) {
  opt = if (param.optional) { "opt" } else "req"
  function simple(string l) { "      \{{l}{opt}:(\"{param.name}\",options.{param.name})}" }
  match (param.typ) {
    case {unknown}: "      \{j{opt}:(\"{param.name}\",function (j) \{ j },options.{param.name})}";
    case {int}: simple("i");
    case {string}: simple("s");
    case {bool}: simple("b");
    case {float}: simple("f");
    case {~tyname}: "      \{c{opt}:(\"{param.name}\",string_of_{tyname},options.{param.name})}";
    case {~list}: "      \{l{opt}:(\"{param.name}\",string_of_{list},options.{param.name})}"; // TODO
  }
}

function output_param_gen(config config) {
  List.iter(function (def) {
    println("  function {def.name}_options({config.name}.{def.name}_options options) \{")
    println("    ApigenLib.params([")
    print(String.concat(",\n",List.map(opt_of_typ,def.params)))
    println("\n    ])")
    println("  \}\n")
  },config.defs)
}

function output_param_default(config config) {
  List.iter(function (def) {
    println("  {config.name}.{def.name}_options {def.name}_default = \{")
    print(String.concat(",\n",List.map(function (param) {
      "      {param.name} : {dflt_of_typ(param.typ,"",param.optional)}"
    },def.params)))
    println("\n  \}\n")
  },config.defs)
}

function getname(typ typ) {
  match (typ) {
  case {unknown}: "function (j) \{ j }";
  case {int}: "ApigenLib.get_json_int";
  case {string}: "ApigenLib.get_json_string";
  case {bool}: "ApigenLib.get_json_bool";
  case {float}: "ApigenLib.get_json_float";
  case {~tyname}: "get_{tyname}";
  case {list:_}: @fail("recursive list type");
  }
}

function get_get(param param) {
  match (param.typ) {
  case {unknown}: "ApigenLib.get_unknown(\"{param.name}\", map)";
  case {int}: "ApigenLib.get_int(\"{param.name}\", map)";
  case {string}: "ApigenLib.get_string(\"{param.name}\", map)";
  case {bool}: "ApigenLib.get_bool(\"{param.name}\", map, false)";
  case {float}: "ApigenLib.get_float(\"{param.name}\", map)";
  case {~tyname}: "ApigenLib.get_obj(\"{param.name}\", map, get_{tyname})";
  case {~list}: "ApigenLib.get_list(\"{param.name}\", map, {getname(list)})";
  }
}

function output_get_types(config config) {
  List.iter(function (ty) {
    match (ty.params) {
    case []: void;
    case params:
      println("  function get_{ty.name}(json) \{")
      println("    map = JsonOpa.record_fields(json) ? Map.empty")
      println("    \{success:{config.name}.{ty.name} \{")
      println(String.concat(",\n",List.map(function (param) {
          "      {param.name} : {get_get(param)}"
              },params)))
      println("    }}")
      println("  }\n")
    }
  },config.types)
}

function output_builds(config config) {
  List.iter(function (def) {
    match (def.result) {
    case {none}: void;
    case {some:{parse:~{return_type:{BOOL}, tyname:_}}}: void;
    case {some:{parse:~{return_type:{INT}, tyname:_}}}: void;
    case {some:{parse:~{return_type:{OBJECT}, tyname}}}:
      println("  function build_{tyname}(WebClient.success res) \{")
      if (def.error_field != "") {
        println("    ApigenLib.check_errors(res.content, \"{def.error_field}\", \{OBJECT}, get_{tyname})")
      } else {
        println("    json = API_libs_private.parse_json(res.content)")
        println("    get_{tyname}(json)")
      }
      println("  }\n")
    case {some:{parse:~{return_type:{LIST}, tyname}}}:
      println("  function build_{tyname}(WebClient.success res) \{")
      if (def.error_field != "") {
        println("    ApigenLib.check_errors(res.content, \"{def.error_field}\", \{LIST}, ApigenLib.get_raw_list(_, get_{tyname}))")
      } else {
        println("    json = API_libs_private.parse_json(res.content)")
        println("    ApigenLib.get_raw_list(json, get_{tyname})")
      }
      println("  }\n")
    }
  },config.defs)
}

function output_calls(config config) {
  List.iter(function (def) {
    credentials = if (get_oauth(config)) { ", credentials" } else ""
    verb = string_of_verb(def.verb)
    content = if (def.verb == {POST}) ", content" else ""
    println("  function {def.name}({config.name}.{def.name}_options options{content}{credentials}) \{")
    println("    path = \"{def.path}\"")
    println("    options = {def.name}_options(options)")
    match (def.result) {
    case {none}: void;
    case {some:{~parse}}:
      get =
        match (parse.return_type) {
        case {BOOL}: "ApigenLib.build_bool(_,\"{def.error_field}\")"
        case {INT}: "ApigenLib.build_int(_,\"{def.error_field}\")"
        case {OBJECT}|{LIST}: "build_{parse.tyname}"
        }
      match (parse.return_type) {
      case {BOOL}:
        if (get_oauth(config)) {
          println("    ApigenOauth(params).{verb}(endpoint, path, options{content}, credentials, {get})")
        } else
          println("    ApigenLib.{verb}(endpoint, path, options{content}, {get})")
      case {INT}:
        if (get_oauth(config)) {
          println("    ApigenOauth(params).{verb}(endpoint, path, options{content}, credentials, {get})")
        } else
          println("    ApigenLib.{verb}(endpoint, path, options{content}, {get})")
      case {OBJECT}|{LIST}:
        if (get_oauth(config)) {
          println("    ApigenOauth(params).{verb}(endpoint, path, options{content}, credentials, {get})")
        } else
          println("    ApigenLib.{verb}(endpoint, path, options{content}, {get})")
      }
    }
    println("  }\n")
  },config.defs)
}

function output_api(config config) {
  output_param_default(config)
  output_param_gen(config)
  output_get_types(config)
  output_builds(config)
  output_calls(config)
}

