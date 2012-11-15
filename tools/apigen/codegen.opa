
style = Mutable.make(style {classic})

function styled(js, cl) { match (style.get()) { case {js_like}: js; case {classic}: cl; } }

function typed(string typ, string v) {
  match (style.get()) {
  case {js_like}: "{typ} {v}";
  case {classic}: "{v}:{typ}";
  }
}

function recel(string name, string val) {
  if (val == "")
    name
  else if (val == "~")
    "~{name}"
  else
    match (style.get()) {
    case {js_like}: "{name}:{val}";
    case {classic}: "{name}={val}";
    }
}

function recsep() { styled(", ","; ") }

function record(els) { "\{{String.concat(recsep(),List.map(function ((name,val)) { recel(name,val) },els))}}" }

function indent_lines(string indent, list(string) ls) { String.concat("\n",List.map(function (l) { "{indent}  {l}" },ls)) }

function func(args, body) {
  match (style.get()) {
  case {js_like}: "function ({args}) \{{body}}";
  case {classic}: "({args} -> {body})";
  }
}

function func_start(indent, name, args, return_type) {
  match (style.get()) {
  case {js_like}:
    return_type = if (return_type == "") "" else return_type^" "
    "{indent}function {return_type}{name}({args}) \{\n";
  case {classic}:
    return_type = if (return_type == "") "" else ": {return_type} "
    "{indent}{name}({args}) {return_type}=\n";
  }
}

function func_end(indent) {
  match (style.get()) {
  case {js_like}: "{indent}}\n\n";
  case {classic}: "\n";
  }
}

function mod_start(indent, name, args) {
  args = if (args == "") "" else "(args)"
  match (style.get()) {
  case {js_like}: "{indent}module {name}{args} \{\n\n";
  case {classic}: "{indent}{name}{args} = \{\{\n\n";
  }
}

function mod_end(indent) {
  match (style.get()) {
  case {js_like}: "{indent}}\n\n";
  case {classic}: "{indent}}}\n\n";
  }
}

function dofun(f) {
  match (style.get()) {
  case {js_like}: "{f};";
  case {classic}: "do {f}";
  }
}

function mtch(nested,indent,e,pes) {
  nested_ = if (nested) "\n" else ""
  end_ = if (nested) "{indent}end" else ""
  match (style.get()) {
  case {js_like}: List.to_string_using("{nested_}{indent}match ({e}) \{\n{indent}","\n{indent}}","\n{indent}",
                                       List.map(function ((p,e)) { "case {p}: {e};" },pes));
  case {classic}: List.to_string_using("{nested_}{indent}match {e} with\n{indent}","\n{end_}","\n{indent}",
                                       List.map(function ((p,e)) { "| {p} -> {e}" },pes));
  }
}

function ite(split,indent,i,t,e) {
  split = if (split) "\n{indent}" else " "
  els =
    if (e == "")
      styled("","else void")
    else
      match (style.get()) {
      case {js_like}: " else \{{split} {e}{split}}";
      case {classic}: "else {e}";
      }
  match (style.get()) {
  case {js_like}: "{split}if ({i}) \{{split} {t}{split}}{els}";
  case {classic}: "{split}if {i}{split}then {t}{split}{els}";
  }
}

function prvt() { styled("private","@private") }
