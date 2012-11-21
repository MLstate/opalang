/*
This file is incomplete, it generates twitter/facebook style apis
but has been neglected in favour of the binary protocol generator in messages.opa.
It will be updated and finished at some time in the future.
*/

function exp opt_of_typ(param param) {
  opt = if (param.optional) { "opt" } else "req"
  function simple(string l) {
    erec([eex("{l}{opt}",etup([evb("\"{param.name}\""),evb("options.{param.name}")]))])
  }
  match (param.typ) {
    case {unknown}:
      erec([eex("\{j{opt}",etup([evb("\"{param.name}\""),efn([pid("j")],[svb("j")]),evb("options.{param.name}")]))]);
    case {int}: simple("i");
    case {string}: simple("s");
    case {bool}: simple("b");
    case {float}: simple("f");
    case {~tyname}:
      erec([eex("\{j{opt}",etup([evb("\"{param.name}\""),evb("string_of_{tyname}"),evb("options.{param.name}")]))]);
    case {~list}:
      erec([eex("\{j{opt}",etup([evb("\"{param.name}\""),evb("string_of_{list}"),evb("options.{param.name}")]))]);
  }
}

function gen_param_gen(config config) {
  List.map(function (def) {
    sfn("{def.name}_options",[ptid("options",tyname("{config.name}.{def.name}_options"))],empty_t,
        [sexp(efa(eid("ApigenLib.params"),[elst(List.map(opt_of_typ,def.params))]))])
  },config.defs)
}

function exp dflt_of_typ(typ typ, string dflt, bool opt) {
  function option(v) { if (opt) { if (dflt == "") enone else esome(evb(dflt)) } else if (dflt == "") evb(v) else evb(dflt) }
  match (typ) {
    case {unknown}: @fail("dflt_of_typ: unknown")
    case {int}:    option("0");
    case {string}: option("\"\"");
    case {bool}:   option("false");
    case {float}:  option("0.0");
    case {~tyname}: @fail("dflt_of_typ: {tyname}")
    case {~list}: @fail("dflt_of_typ: list {list}")
  }
}

function gen_param_default(config config) {
  List.map(function (def) {
    asgn(ptid("{def.name}_default",tyname("{config.name}.{def.name}_options")),
         erec(List.map(function (param) { eex(param.name,dflt_of_typ(param.typ,"",param.optional)) },def.params)))
  },config.defs)
}

function exp getname(typ typ) {
  match (typ) {
  case {unknown}: efn([pid("j")],[svb("j")]);
  case {int}: evb("ApigenLib.get_json_int");
  case {string}: evb("ApigenLib.get_json_string");
  case {bool}: evb("ApigenLib.get_json_bool");
  case {float}: evb("ApigenLib.get_json_float");
  case {~tyname}: evb("get_{tyname}");
  case {list:_}: @fail("recursive list type");
  }
}

function exp get_get(param param) {
  match (param.typ) {
  case {unknown}: efa(eid("ApigenLib.get_unknown"),[eid(param.name),eid("map")]);
  case {int}: efa(eid("ApigenLib.get_int"),[eid(param.name),eid("map")]);
  case {string}: efa(eid("ApigenLib.get_string"),[eid(param.name),eid("map")]);
  case {bool}: efa(eid("ApigenLib.get_bool"),[eid(param.name),eid("map")]);
  case {float}: efa(eid("ApigenLib.get_float"),[eid(param.name),eid("map")]);
  case {~tyname}: efa(eid("ApigenLib.get_obj"),[eid(param.name),eid("map"),eid("get_{tyname}")]);
  case {~list}: efa(eid("ApigenLib.get_list"),[eid(param.name),eid("map"),getname(list)]);
  }
}

function gen_get_types(config config) {
  List.map(function (ty) {
    match (ty.params) {
    case []: sempty;
    case params:
      sfn("get_{ty.name}",[pid("json")],empty_t,
          [svb("map = JsonOpa.record_fields(json) ? Map.empty"),
           sexp(esuccess(typedexp(erec(List.map(function (param) { eex("{param.name}",get_get(param)) },params)),
                                  tyname("{config.name}.{ty.name}")))
               )
          ]);
    }
  },config.types)
}

function gen_builds(config config) {
  List.map(function (def) {
    match (def.result) {
    case {none}: sempty;
    case {some:{parse:~{return_type:{BOOL}, tyname:_}}}: sempty;
    case {some:{parse:~{return_type:{INT}, tyname:_}}}: sempty;
    case {some:{parse:~{return_type:{OBJECT}, tyname:tn}}}:
      sfn("build_{tn}",[ptid("res",tyname("WebClient.success"))],empty_t,
          if (def.error_field != "") {
            [sexp(efa(eid("ApigenLib.check_errors"),
                      [eid("res.content"),evb("\"{def.error_field}\""),erecsimple("OBJECT"),eid("get_{tn}")]))]
          } else {
            [svb("json = API_libs_private.parse_json(res.content)"),
             sexp(efa(eid("get_{tn}"),[eid("json")]))]
          });
    case {some:{parse:~{return_type:{LIST}, tyname:tn}}}:
      sfn("build_{tn}",[ptid("res",tyname("WebClient.success"))],empty_t,
          if (def.error_field != "") {
            [sexp(efa(eid("ApigenLib.check_errors"),
                      [eid("res.content"),evb("\"{def.error_field}\""),erecsimple("LIST"),
                       eid("ApigenLib.get_raw_list(_, get_{tn})")]))]
          } else {
            [svb("json = API_libs_private.parse_json(res.content)"),
             sexp(efa(eid("ApigenLib.get_raw_list"),[eid("json"),eid("get_{tn}")]))]
          });
    }
  },config.defs)
}

function gen_calls(config config) {
  List.map(function (def) {
    match (def.result) {
    case {none}: sempty;
    case {some:{~parse}}:
      verb = string_of_verb(def.verb)
      args = [ptid("options",tyname("{config.name}.{def.name}_options"))]
      args = if (get_oauth(config)) List.append(args,[pid("credentials")]) else args
      args = if (def.verb == {POST}) List.append(args,[pid("content")]) else args
      get =
        match (parse.return_type) {
        case {BOOL}: efa(eid("ApigenLib.build_bool"),[ewild,evb("\"{def.error_field}\"")]);
        case {INT}: efa(eid("ApigenLib.build_int"),[ewild,evb("\"{def.error_field}\"")]);
        case {OBJECT}|{LIST}: eid("build_{parse.tyname}")
        }
      fn = if (get_oauth(config)) eid("ApigenOauth(params).{verb}") else eid("ApigenLib.{verb}")
      fargs = List.flatten([[eid("endpoint"),eid("path"),eid("options")],
                            if (def.verb == {POST}) [eid("content")] else [],
                            [eid("credentials"),get]])
      sfn(def.name,args,empty_t,
          [svb("path = \"{def.path}\""),
           svb("options = {def.name}_options(options)"),
           sexp(efa(fn,fargs))])
    }
  },config.defs)
}

function block gen_api(config config) {
  List.flatten([gen_param_default(config),
                gen_param_gen(config),
                gen_get_types(config),
                gen_builds(config),
                gen_calls(config)])
}

