import stdlib.io.file
import stdlib.system

type po_line = 
    {string file}
 or {string txt}
 or {int num}
 or {string msgid}
 or {string msgstr}
 or {string cmnt}
 or {eos}

type po_record =
    {string cmnt}
 or {string file, string txt, int num, string msgid, string msgstr}

type tr_line = 
    {string pkg}
 or {string imprt}
 or {string file}
 or {string txt}
 or {int num}
 or {string code, string args}
 or {string lang, string trans}
 or {string dflt}
 or {eos}

type tr_record =
    {string pkg}
 or {string imprt}
 or {string file, string txt, int num, string code, string args, list({string lang, string trans}) trans, string dflt}

type Params.t = {
  bool debug,
  bool clear_translation,
  bool strict,
  string root_dir,
  list(string) create_lang,
  list(string) packages
}

//done = Mutable.make(stringset StringSet.empty)
//function add_done(string id) { jlog("add_done: {id}"); done.set(StringSet.add(id, done.get())) }
//function is_done(string id) { StringSet.mem(id, done.get()) }

module Params {

  private Params.t default_params = {
    debug:false, clear_translation:false, strict:false, create_lang:[], packages:[], root_dir:"lang"
  }

  private CommandLine.family(Params.t) params_family = {
    title : "Params",
    init : default_params,
    anonymous : [{filter      : "*",
                  description : "Other parameters",
                  parse       : function (state) { parser { result=Rule.consume -> {state with packages:result +> state.packages} } }
                 }],
    parsers : [
      { CommandLine.default_parser with
        names : ["--debug"],
        description : "Enable debug output",
        param_doc : "<bool>",
        on_param : function(state) { parser { debug=Rule.bool ->
          state = { state with debug : debug }
          {no_params : state}
        } } },
      { CommandLine.default_parser with
        names : ["--root-dir"],
        description : "Root directory (default: \"lang\")",
        param_doc : "<string>",
        on_param : function(state) { parser { root_dir=(.*) ->
          state = { state with root_dir : Text.to_string(root_dir) }
          {no_params : state}
        } } },
      { CommandLine.default_parser with
        names : ["--clear-translation"],
        description : "Don't retain non-default values from the original opa translation file",
        param_doc : "<bool>",
        on_param : function(state) { parser { clear_translation=Rule.bool ->
          state = { state with clear_translation : clear_translation }
          {no_params : state}
        } } },
      { CommandLine.default_parser with
        names : ["--strict"],
        description : "Strictly match file names and string numbers",
        param_doc : "<bool>",
        on_param : function(state) { parser { strict=Rule.bool ->
          state = { state with strict : strict }
          {no_params : state}
        } } },
      { CommandLine.default_parser with
        names : ["--create-lang"],
        description : "Create new language PO file",
        param_doc : "<lang>",
        on_param : function(state) { parser { create_lang=Rule.ident ->
          state = { state with create_lang : [create_lang|state.create_lang] }
          {no_params : state}
        } } }
    ]
  }

  protected params = CommandLine.filter(params_family)

}

module ParsePo {

  function string tt(list(text) t) { Text.to_string(Text.ltconcat(t)) }

  po_line = parser {
    case "# Template for " file=(!"\n" .)* "\n":
      if (Params.params.debug) jlog("file: {tt(file)}")
      po_line {file:tt(file)};
    case "# \"" txt=(!"\"" .)* "\"":
      if (Params.params.debug) jlog("txt: {tt(txt)}")
      po_line {txt:tt(txt)};
    case "# string, " num=Rule.natural:
      if (Params.params.debug) jlog("num: {num}")
      po_line {~num};
    case "msgid \"" msgid=(!"\"" .)* "\"":
      if (Params.params.debug) jlog("msgid: {tt(msgid)}")
      po_line {msgid:tt(msgid)};
    case "msgstr \"" msgstr=(!"\"" .)* "\"":
      if (Params.params.debug) jlog("msgstr: {tt(msgstr)}")
      po_line {msgstr:tt(msgstr)};
    case "\"" cmnt=(!"\"" .)* "\"":
      if (Params.params.debug) jlog("cmnt: {tt(cmnt)}")
      po_line {cmnt:tt(cmnt)};
    case Rule.eos:
      if (Params.params.debug) jlog("eos")
      po_line {eos};
  }

  po_parser = parser {
    case Rule.ws l=Rule.parse_list(po_line, Rule.ws): list(po_line) l;
  }

  tr_line = parser {
    case "package " pkg=(!"\n" .)* "\n":
      if (Params.params.debug) jlog("pkg: {tt(pkg)}")
      {pkg:tt(pkg)};
    case "import " imprt=(!"\n" .)* "\n":
      if (Params.params.debug) jlog("imprt: {tt(imprt)}")
      {imprt:tt(imprt)};
    case "// Template for " file=(!"\n" .)* "\n":
      if (Params.params.debug) jlog("file: {tt(file)}")
      {file:tt(file)};
    case "// \"" txt=(!"\"" .)* "\"":
      if (Params.params.debug) jlog("txt: {tt(txt)}")
      {txt:tt(txt)};
    case "// string, " num=Rule.natural:
      if (Params.params.debug) jlog("num: {num}")
      {~num};
    case "__i18n_" code=([a-z0-9])+ "(" args=(!")" .)* ")= match I18n.lang()\n":
      if (Params.params.debug) jlog("code: {tt(code)} args:{tt(args)}")
      {code:tt(code), args:tt(args)};
    case Rule.ws "\"" lang=(!"\"" .)+ "\"" Rule.ws "->" Rule.ws "\"" trans=(!"\"" .)+ "\"":
      if (Params.params.debug) jlog("{tt(lang)} -> \"{tt(trans)}\"")
      {lang:tt(lang), trans:tt(trans)};
    case Rule.ws "_" Rule.ws "->" Rule.ws "\"" dflt=(!"\"" .)* "\"":
      if (Params.params.debug) jlog("dflt: {tt(dflt)}")
      {dflt:tt(dflt)};
    case Rule.eos:
      if (Params.params.debug) jlog("eos")
      {eos};
  }

  tr_parser = parser {
    case Rule.ws l=Rule.parse_list(tr_line, Rule.ws): list(tr_line) l;
  }

  function stringmap(po_record) process_po(list(po_line) lines) {
    recursive function aux(lines,n) {
      match (lines) {
      case []
      case [{eos}]
      case [{eos},{eos}]: StringMap.empty;
      case [{msgid:""},{msgstr:""}|lines]: aux(lines,n);
      case [{~cmnt}|lines]: StringMap.add("{n}",{~cmnt},aux(lines,n+1));
      case [{~file},{~txt},{~num},{~msgid},{~msgstr}|lines]: StringMap.add(msgid,~{file,txt,num,msgid,msgstr},aux(lines,n));
      case [_|lines]: aux(lines,n);
      }
    }
    aux(lines,0)
  }

  function list(tr_record) process_tr(list(tr_line) lines) {
    recursive function aux(lines) {
      match (lines) {
      case []
      case [{eos}]
      case [{eos},{eos}]: [];
      case [{~pkg}|lines]: [{~pkg}|aux(lines)];
      case [{~imprt}|lines]: [{~imprt}|aux(lines)];
      case [{~file},{~txt},{~num},~{code, args}|lines]:
        //jlog("is_done({code}): {if (is_done(code)) "yes" else "no"}")
        recursive function aux2(lines, transs) {
          match (lines) {
          case [~{lang, trans}|lines]: aux2(lines,[~{lang, trans}|transs]);
          case [~{dflt}|lines]:
            //add_done(code)
            [~{file,txt,num,code,args,trans:transs,dflt}|aux(lines)];
          default: aux(lines);
          }
        }
        /*if (is_done(code)) aux(lines) else*/ aux2(lines, []);
      case [_|lines]: aux(lines);
      }
    }
    aux(lines)
  }

  function string generate_translation(tr_records, langmap) {
    strs =
      List.fold(function (tr, acc) {
                  match (tr) {
                  case ~{pkg}: ["package {pkg}\n"|acc];
                  case ~{imprt}: ["import {imprt}\n"|acc];
                  case {file:_,txt:_,num:_,code:_,args:_,trans:_,dflt:""}: acc;
                  case ~{file,txt,num,code,args,trans,dflt}:
                    trs = StringMap.fold(function (lang, map, trs) {
                                           function nopo() {
                                             match (List.find(function ({lang:l, trans:_}) { l == lang },trans)) {
                                             case {some:{lang:_, ~trans}}: [(lang,trans)|trs];
                                             case {none}: trs;
                                             }
                                           }
                                           function po_ok(po_record po_record) {
                                             match (po_record) {
                                             case ~{file:f, num:n, msgstr:s, ...}:
                                               if (not(Params.params.strict) || (f == file && n == num)) {some:s} else {none};
                                             default: {none};
                                             }
                                           }
                                           match (StringMap.get(txt, map)) {
                                           case {some:po_record po_record}:
                                             match (po_ok(po_record)) {
                                             case {some:msgstr}:
                                               if (Params.params.debug) jlog("{lang} -> {msgstr}")
                                               [(lang,msgstr)|trs]
                                             case {none}:
                                               match (po_record) {
                                               case {file:f, num:n, ...}:
                                                 if (f != file) jlog("{lang} PO:{f} OP:{file} \"{txt}\"")
                                                 if (n != num) jlog("{lang} PO:{n} OP:{num} \"{txt}\"")
                                               default: void;
                                               }
                                               nopo()
                                             }
                                           case {none}: nopo();
                                           }
                                         },langmap,[])
                    dflt = match (List.find(function ((lang,_)) { lang == "dflt" },trs)) {
                           case {some:(_,dflt)}:
                             if (Params.params.debug) jlog("Setting default from PO file \"{dflt}\"")
                             dflt;
                           case {none}: dflt;
                           }
                    translation =
                      String.concat("\n",List.filter_map(function ((lang,tr)) {
                                                           if (tr != dflt) {some:"  \"{lang}\" -> \"{tr}\""} else {none}
                                                         },trs))
["
// Template for {file}
// \"{txt}\"
// string, {num}
__i18n_{code}({args})= match I18n.lang()
{translation}
  _   -> \"{dflt}\"
"|acc];
                  }
                },tr_records,[])
    res = String.concat("",List.rev(strs))
    //if (Params.params.debug) jlog("{res}")
    res
  }

  function update_langmap(lang, file, txt, num, msgid, msgstr, langmap) {
    map =
      match (StringMap.get(lang, langmap)) {
      case {some:map}:
        match (StringMap.get(msgid, map)) {
        case {some:po_record msg}:
          match (msg) {
          case ~{file, txt, num, msgid, msgstr:existing_msgstr}:
            msgstr = if (lang == "dflt") msgstr else existing_msgstr
            //jlog("update_langmap(updating): %g{lang}%d mapping %c{msgid}%d to %m{msgstr}%d")
            StringMap.add(msgid, po_record ~{file, txt, num, msgid, msgstr}, map); // we never overwrite a translation
          case {cmnt:_}:
            //jlog("update_langmap(comment): %g{lang}%d %c{cmnt}%d")
            map;
          }
        case {none}:
          //jlog("update_langmap(creating): %g{lang}%d mapping %c{msgid}%d to %m{msgstr}%d")
          StringMap.add(msgid, ~{file, txt, num, msgid, msgstr}, map);
        }
      case {none}:
        //jlog("update_langmap(new language): %g{lang}%d creating %c{msgid}%d to %m{msgstr}%d")
        StringMap.add(msgid, ~{file, txt, num, msgid, msgstr}, StringMap.empty);
      }
    StringMap.add(lang, map, langmap)
  }

  function update_po(tr_records, langmap) {
    List.fold(function (tr_record, langmap) {
                match (tr_record) {
                case ~{pkg:_}: langmap;
                case ~{imprt:_}: langmap;
                case ~{file,txt,num,code:_,args:_,trans,dflt}:
                  langmap = {
                    //if (dflt != "") {
                      if (Params.params.debug) jlog("update_po: dflt \"{txt}\" -> \"{dflt}\"")
                      langmap = List.fold(function (l, langmap) {
                                            update_langmap(l, file, txt, num, txt, dflt, langmap)
                                          },Params.params.create_lang,langmap)
                      update_langmap("dflt", file, txt, num, txt, dflt, langmap)
                    //} else langmap
                  }
                  List.fold(function (~{lang,trans}, langmap) {
                              if (Params.params.debug) jlog("update_po: {lang} \"{txt}\" -> \"{trans}\"")
                              update_langmap(lang, file, txt, num, txt, trans, langmap)
                            },trans,langmap);
                }
              },tr_records,langmap)
  }

  function write_file(dir, filename, translation) {
    if (File.exists(filename)) {
      idx = Date.in_milliseconds(Date.now())
      File.write("{dir}/tmp{idx}.opa",Binary.of_binary(translation))
      backupfilename = "{filename}.{idx}"
      resp = System.exec("mv {filename} {backupfilename}","")
      if (resp != "") {
        jlog("{resp}")
        System.exit(1)
      } else {
        jlog("Backup file {backupfilename}")
        resp = System.exec("mv {dir}/tmp{idx}.opa {filename}","")
        if (resp != "") {
          jlog("{resp}")
          System.exit(1)
        } else
          jlog("Updated file {filename}")
      }
    } else {
      File.write(filename,Binary.of_binary(translation))
      jlog("Created file {filename}")
    }
  }

  function generate_po(langmap) {
    StringMap.fold(function (lang, po_records, po_texts) {
                     //jlog("generate_po({lang}): po_records={po_records}")
                     texts =
                       StringMap.fold(function (_, po_record, texts) {
                                        match (po_record) {
                                        case ~{file, txt, num, msgid, msgstr}:
text = "
# Template for {file}
# \"{txt}\"
# string, {num}
msgid \"{msgid}\"
msgstr \"{msgstr}\"
"
                                          [text|texts]
                                        case {cmnt:_}: texts;
                                        }
                                      },po_records,[])
                     comments =
                       StringMap.fold(function (_, po_record, texts) {
                                        match (po_record) {
                                        case {file:_, txt:_, num:_, msgid:_, msgstr:_}: texts;
                                        case ~{cmnt}: ["\"{cmnt}\"\n"|texts];
                                        }
                                      },po_records,[])
                     [(lang,"msgid \"\"\nmsgstr \"\"\n"^String.concat("",comments)^String.concat("",texts))|po_texts]
                   },langmap,[])
  }

  function get_po_filename(dir, lang, pkg) {
    ("{dir}/locale/{lang}",
     "{dir}/locale/{lang}/{pkg}.translation.{lang}.po")
  }

  function write_po_file(dir, pkg, (lang,text)) {
    (po_dir,po_file) = get_po_filename(dir, lang, pkg)
    if (File.exists(po_dir) || File.mkdir(po_dir)) {
      write_file(dir, po_file, text)
    } else {
      jlog("Can't create PO output directory {po_dir}")
      System.exit(1)
    }
  }

  function clean_translation(tr_record) {
    List.map(function (tr) {
               match (tr) {
               case ~{pkg}: ~{pkg};
               case ~{imprt}: ~{imprt};
               case ~{file,txt,num,code,args,trans:_,dflt}: ~{file,txt,num,code,args,trans:[],dflt};
               }
             },tr_record)
  }

  function do_package(dir, pkg) {

    files =
      match (File.readdir("{dir}/locale")) {
      case {success:files}: files
      case {~failure}:
        jlog("Can't read locale directory {failure}")
        System.exit(1)
      }

    langmap =
      LowLevelArray.fold(function (lang, acc) {
                           if (Params.params.debug) jlog("{lang}")
                           (_,file) = get_po_filename(dir, lang, pkg)
                           match (File.read_opt(file)) {
                           case {some:content}:
                             match (Parser.try_parse(po_parser, Binary.to_binary(content))) {
                             case {some:pfile}:
                               jlog("Parsed file {file}")
                               po_record = process_po(pfile)
                               //if (Params.params.debug) jlog("{po_record}")
                               StringMap.add(lang, po_record, acc);
                             case {none}: jlog("PO file {file} doesn't parse"); acc;
                             }
                           case {none}: jlog("Can't read file {file}"); acc;
                           }
                         },files,StringMap.empty)
    //if (Params.params.debug) jlog("langmap={langmap}")

    trfilename = "{dir}/{pkg}.translation.opa"

    tr_record =
      match (File.read_opt(trfilename)) {
      case {some:content}:
        match (Parser.try_parse(tr_parser, Binary.to_binary(content))) {
        case {some:trfile}:
          jlog("Parsed file {trfilename}")
          tr_record = process_tr(trfile)
          //if (Params.params.debug) jlog("{tr_record}")
          if (Params.params.clear_translation) clean_translation(tr_record) else tr_record
        case {none}:
          jlog("Original translation file {trfilename} doesn't parse");
          System.exit(1)
        }
      case {none}:
        jlog("Can't read original translation file {trfilename}")
        System.exit(1)
      }

    translation = generate_translation(tr_record, langmap)
    write_file(dir, trfilename, translation)

    updated_langmap = update_po(tr_record, langmap)
    //if (Params.params.debug) jlog("updated_langmap={updated_langmap}")
    po_files = generate_po(updated_langmap)
    List.iter(write_po_file(dir, pkg, _),po_files)

  }

  function run() {
    if (List.length(Params.params.packages) == 0) {
      jlog("No packages given")
      System.exit(1)
    }
    List.iter(function (pkg) { do_package(Params.params.root_dir, pkg) },Params.params.packages)
  }

}

_x = Scheduler.push(ParsePo.run)
