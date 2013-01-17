/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
//TODO: the namespaces on decode are actually wrong.  CHECK!!!

import stdlib.core.map
import stdlib.core.xhtml

type WBXml.publicid = {int id} or {string str}

type WBXml.header = {
  int version,             /** WBXml version  */
  WBXml.publicid publicid, /** XML document public identifier */
  int charset              /** IANA-assigned MIB number for the XML document character encoding */
}

type WBXml.context = {
  int debug,           /** Debug flag */
  bool use_st,         /** Use string table */
  binary buf,          /** The wbxml being built */
  int pos,             /** Position during parsing */
  WBXml.header header, /** Header read during parsing */
  int tag_cp,          /** Current tag code page */
  string tag_ns,       /** Current tag namespace value */
  int att_cp,          /** Current attribute code page */
  string att_ns,       /** Current attribute namespace value */
  int nsti,            /** Next string table index */
  list(string) default_namespaces,          /** Stack of default namespaces */
  stringmap(list(string)) named_namespaces, /** Stack of namespace names */
  stringmap(int) st,                        /** String table */
  intmap(string) rst,                       /** Reverse string table */
  intmap(string) namespace_map,             /** Map from code page to namespace */
  stringmap(int) namespace_rmap,            /** Map from namespace to code page */
  intmap(string) tag_code_space,            /** Map from code page/tag code to tag names */
  ordered_map((int,string),int,Order.default) reverse_tag_code_space, /** Map from code page/tag name to tag codes */
  intmap((string,string)) attrstart_token_map, /** Map from code page/attribute start code to attribute names/value prefix */
  ordered_map((int,string),list((int,string)),Order.default) attrstart_reverse_token_map, /** Reverse attrstart map */
  intmap(string) attrvalue_token_map, /** Map from code page/attrvalue code to attrvalue text */
  ordered_map((int,string),int,Order.default) attrvalue_reverse_token_map /** Map from code page/attrvalue to attrvalue codes */
}

module WBXml {

  private (binary -> string) bindump = %% BslPervasives.bindump %%

  /** IANA MIB enums for charset (see http://www.iana.org/assignments/character-sets/character-sets.xml) */
  MIBE_US_ASCI     = 3
  MIBE_ISO_8859_1  = 4
  MIBE_ISO_8859_15 = 111
  MIBE_UTF8        = 106

  /** WBXml specificiation codes */
  SWITCH_PAGE = 0x00
  END         = 0x01
  ENTITY      = 0x02
  STR_I       = 0x03
  LITERAL     = 0x04
  EXT_I_0     = 0x40
  EXT_I_1     = 0x41
  EXT_I_2     = 0x42
  PI          = 0x43
  LITERAL_C   = 0x44
  EXT_T_0     = 0x80
  EXT_T_1     = 0x81
  EXT_T_2     = 0x82
  STR_T       = 0x83
  LITERAL_A   = 0x84
  EXT_0       = 0xC0
  EXT_1       = 0xC1
  EXT_2       = 0xC2
  OPAQUE      = 0xC3
  LITERAL_AC  = 0xC4

  /* Build maps between tags and strings.
   * Complicated by the fact that attrStart tags can include a prefix string for the attrValue.
   */

  private function Order.ordering order_is((int,string) (i1,s1), (int,string) (i2,s2)) {
    match (Int.ordering(i1,i2)) {
    case {eq}: String.ordering(s1,s2);
    case ord: ord;
    }
  }
  private isMap = Map_make(order((int,string),Order.default) Order.make(order_is))

  private function map_from_code_space(code_space) {
    List.fold(function ((cp, codes), dcm) {
                List.fold(function ((code, tag), dcm) {
                            IntMap.add(cp * 0x100 + code, tag, dcm)
                          },codes,dcm)
              },code_space,IntMap.empty)
  }

  function get_tag_from_code(ctxt, cp, code) { IntMap.get(cp * 0x100 + code, ctxt.tag_code_space) }
  function get_attrvalue_from_code(ctxt, cp, code) { IntMap.get(cp * 0x100 + code, ctxt.attrvalue_token_map) }

  private function reverse_map_from_code_space(code_space) {
    List.fold(function ((cp, codes), dcm) {
                List.fold(function ((code, tag), dcm) {
                            isMap.add((cp,tag), code, dcm)
                          },codes,dcm)
              },code_space,isMap.empty)
  }

  function get_code_from_tag(ctxt, cp, tag) { isMap.get((cp,tag), ctxt.reverse_tag_code_space) }
  function get_code_from_attrvalue(ctxt, cp, attrvalue) { isMap.get((cp,attrvalue), ctxt.attrvalue_reverse_token_map) }

  private function get_attrstart_token_map(attrstart) {
    List.fold(function ((cp, codes), asm) {
                List.fold(function ((tag, prefix_codes), asm) {
                            List.fold(function ((code, prefix), asm) {
                                        if (code < 128) IntMap.add(cp * 0x100 + code, (tag,prefix), asm) else asm
                                      },prefix_codes,asm)
                          },codes,asm)
              },attrstart,IntMap.empty)
  }

  function get_attrstart_from_code(ctxt, cp, code) { IntMap.get(cp * 0x100 + code, ctxt.attrstart_token_map) }

  private function get_attrstart_token_reverse_map(attrstart) {
    List.fold(function ((cp, codes), asm) {
                List.fold(function ((tag, prefix_codes), asm) {
                            isMap.add((cp,tag), prefix_codes, asm)
                          },codes,asm)
              },attrstart,isMap.empty)
  }

  function get_prefix_list_from_attrstart(ctxt, cp, tag) { isMap.get((cp,tag), ctxt.attrstart_reverse_token_map) }

  /** Default header.
   * This is put into the generated binaries but is updated during a decode.
   * The public id will always be 0x01 (unknown id value) and the charset
   * doesn't mean much to us so we just set it to UTF-8.
   * The WBXml version number may get bumped one day.
   */
  WBXml.header default_header = {version:0x03, publicid:{id:0x01}, charset:MIBE_UTF8}

  /**
   * Create a context for WBXml encode/decode.
   * This is complicated since we can only encode/decode in the context of
   * the tag and attribute code spaces.
   * Here we build maps and reverse maps to be used during encode and decode.
   */
  function WBXml.context init_context(header, namespaces, tag_code_space, attrstart, attrvalue) {
    {
      debug:0, use_st:false,
      buf:Binary.create(0), pos:0,
      ~header,
      tag_cp:0, tag_ns:"", att_cp:0, att_ns:"", nsti:0, st:StringMap.empty, rst:IntMap.empty,
      default_namespaces:[], named_namespaces:StringMap.empty,
      namespace_map:List.fold(function ((code, tag), nm) { IntMap.add(code, tag, nm) },namespaces,IntMap.empty),
      namespace_rmap:List.fold(function ((code, tag), nm) { StringMap.add(tag, code, nm) },namespaces,StringMap.empty),
      tag_code_space:map_from_code_space(tag_code_space),
      reverse_tag_code_space:reverse_map_from_code_space(tag_code_space),
      attrstart_token_map:get_attrstart_token_map(attrstart),
      attrstart_reverse_token_map:get_attrstart_token_reverse_map(attrstart),
      attrvalue_token_map:map_from_code_space(attrvalue),
      attrvalue_reverse_token_map:reverse_map_from_code_space(attrvalue)
    }
  }

  /* Some debug stuff */

  private function bp(bool tf) { Ansi.print({yellow},"{tf}") }

  private function tp(string tag) { Ansi.print({blue},tag) }

  private function pcode(int code) { Ansi.print({red},"0x{String.pad_left("0",2,Int.to_hex(code))}") }

  private function pns(string namespace) { Ansi.print({cyan},namespace) }

  private function sanitize(string str) { Ansi.print({magenta},String.replace("\n","\\n",str)) }

  private function debug(ctxt, level, what, message) {
    if (ctxt.debug >= level) {
      Ansi.color color =
        match (level) {
        case 0: {black};
        case 1: {cyan};
        case 2: {green};
        case 3: {yellow};
        case 4: {magenta};
        default: {red};
        }
      jlog("{Ansi.print(color,what)}: {message}")
    }
  }

  /* Start of encode section */

  function add_to_string_table(ctxt, string text) {
    match (StringMap.get(text,ctxt.st)) {
    case {some:_}: ctxt;
    case {none}:
      debug(ctxt,2,"WBXml.add_to_string_table","\"{sanitize(text)}\"")
      {ctxt with
         st:StringMap.add(text,ctxt.nsti,ctxt.st),
         rst:IntMap.add(ctxt.nsti,text,ctxt.rst),
         nsti:ctxt.nsti+1};
    }
  }

  function cstring(ctxt, str) {
    Binary.add_string(ctxt.buf,str)
    Binary.add_uint8(ctxt.buf,0x00)
    ctxt
  }

  function start(ctxt) {
    Binary.add_uint8(ctxt.buf,ctxt.header.version)
    ctxt =
      match (ctxt.header.publicid) {
      case {~id}: Binary.add_uint8(ctxt.buf,id); ctxt;
      case {~str}:
        ctxt = add_to_string_table(ctxt, str)
        Binary.add_uint8(ctxt.buf,0)
        Binary.add_binary(ctxt.buf,MBInt.of_int(Option.get(StringMap.get(str, ctxt.st))))
        ctxt;
      }
    Binary.add_binary(ctxt.buf,MBInt.of_int(ctxt.header.charset))
    if (StringMap.is_empty(ctxt.st))
      Binary.add_uint8(ctxt.buf,0x00)
    else {
      stlen = StringMap.fold(function (str, _, stlen) { stlen + String.length(str) + 1 },ctxt.st,0)
      Binary.add_binary(ctxt.buf,MBInt.of_int(stlen))
      strs = StringMap.fold(function (str, idx, strs) { [(idx,str)|strs] },ctxt.st,[])
      strs = List.sort_by(function ((idx,_)) { idx },strs)
      List.iter(function ((_,str)) { ignore(cstring(ctxt, str)) },strs)
    }
    ctxt
  }

  private function ctxt_cp(ctxt, tagatt) {
    current_cp =
      match (tagatt) {
        case {tag}: ctxt.tag_cp;
        case {attrstart}: ctxt.att_cp;
        case {attrvalue}: ctxt.att_cp;
      }
    match (ctxt.default_namespaces) {
    case []: current_cp;
    case [dflt|_]:
      match (StringMap.get(dflt,ctxt.namespace_rmap)) {
      case {some:cp}:
        debug(ctxt,2,"WBXml.encode","default namespace {pns(dflt)} in map code page = {pcode(cp)}")
        cp;
      default: current_cp;
      }
    }
  }

  private function get_cp(ctxt, tagatt, ns) { // TODO: direct map from nns to codepage
    if (ns == "")
      ctxt_cp(ctxt, tagatt)
    else
      match (StringMap.get(ns, ctxt.namespace_rmap)) {
      case {some:cp}:
        debug(ctxt,2,"WBXml.encode","namespace {pns(ns)} in map code page = {pcode(cp)}")
        cp;
      default:
        match (StringMap.get(ns, ctxt.named_namespaces)) {
        case {some:[nns|_]}:
          debug(ctxt,2,"WBXml.encode","namespace {pns(ns)} in maps to namespace {pns(nns)}")
          match (StringMap.get(nns, ctxt.namespace_rmap)) {
          case {some:cp}:
            debug(ctxt,2,"WBXml.encode","namespace {pns(nns)} in map code page = {pcode(cp)}")
            cp;
          default: ctxt_cp(ctxt, tagatt);
          }
        default: ctxt_cp(ctxt, tagatt);
        }
      }
  }

  private function get_mapping(tagatt, ctxt, ta, namespace) { // TODO: return namespace error
    cp = get_cp(ctxt, tagatt, namespace)
    match (match (tagatt) {
           case {tag}: ({tag:get_code_from_tag(ctxt, cp, ta)});
           case {attrstart}: {attrstart:get_prefix_list_from_attrstart(ctxt, cp, ta)};
           case {attrvalue}: {attrvalue:get_code_from_attrvalue(ctxt, cp, ta)};
           }) {
    case {tag:{some:code}}: {some:{tag:(cp, code)}};
    case {attrstart:{some:pfxlst}}: {some:{attrstart:(cp,pfxlst)}};
    case {attrvalue:{some:code}}: {some:{attrvalue:(cp,code)}};
    default: none;
    }
  }

  function str_i(ctxt, string content) {
    if (content == "")
      ctxt
    else {
      debug(ctxt,2,"WBXml.encode","STR_I \"{sanitize(content)}\"")
      Binary.add_uint8(ctxt.buf,STR_I)
      cstring(ctxt, content)
    }
  }

  function str_t(ctxt, int idx) {
    debug(ctxt,2,"WBXml.encode","STR_T {idx} for \"{sanitize(Option.get(IntMap.get(idx,ctxt.rst)))}\"")
    Binary.add_uint8(ctxt.buf,STR_T)
    Binary.add_binary(ctxt.buf,MBInt.of_int(idx))
    ctxt
  }

  function opaque(ctxt, string opaque) {
    len = String.length(opaque)
    debug(ctxt,2,"WBXml.encode","OPAQUE {Ansi.print({red},"{len}")} bytes")
    Binary.add_uint8(ctxt.buf,OPAQUE)
    Binary.add_binary(ctxt.buf,MBInt.of_int(len))
    Binary.add_string(ctxt.buf,opaque)
    ctxt
  }

  function literal(ctxt, int idx, atts, content) {
    literal = LITERAL
    literal = if (List.length(atts) > 0) Bitwise.lor(literal,0x80) else literal
    literal = if (List.length(content) > 0) Bitwise.lor(literal,0x40) else literal
    debug(ctxt,2,"WBXml.encode","LITERAL {pcode(literal)} for {Option.get(IntMap.get(idx,ctxt.rst))}")
    Binary.add_uint8(ctxt.buf,literal)
    Binary.add_binary(ctxt.buf,MBInt.of_int(idx))
    ctxt
  }

  function end_tag(what,ctxt) {
    debug(ctxt,2,"WBXml.encode","END({what})")
    Binary.add_uint8(ctxt.buf, END)
    ctxt
  }

  function switch_page(ctxt, tagatt, int cp) {
    ns = match (IntMap.get(cp,ctxt.namespace_map)) { case {some:ns}: ns; case {none}: ""; }
    (at_cp,set) =
      match (tagatt) {
      case {tag}: (ctxt.tag_cp,function (ctxt,cp) { {ctxt with tag_cp:cp, tag_ns:ns} });
      case {att}: (ctxt.att_cp,function (ctxt,cp) { {ctxt with att_cp:cp, att_ns:ns} });
      }
    if (at_cp != cp) {
      debug(ctxt,2,"WBXml.encode","SWITCH_PAGE {pcode(cp)}")
      Binary.add_uint8(ctxt.buf,SWITCH_PAGE);
      Binary.add_uint8(ctxt.buf,cp);
      set(ctxt,cp)
    } else ctxt
  }

  function Outcome_fold(f, l, acc) {
    List.fold(function (e, acc) {
                match (acc) {
                case {success:acc}: f(e, acc);
                case {~failure}: {~failure};
                }
              },l,{success:acc})
  }

  function attributes(ctxt, namespace, atts) {
    if (atts != []) {
      ctxt = List.fold(function (att, ctxt) {
        match
          (match (get_mapping({attrstart}, ctxt, att.name, namespace)) {
           case {some:{attrstart:(cp,pfxlst)}}:
             ctxt = switch_page(ctxt, {att}, cp)
             match (List.find(function ((_,prefix)) { String.has_prefix(prefix,att.value) },pfxlst)) {
             case {some:(code,prefix)}:
               debug(ctxt,2,"WBXml.encode","ATTRSTART tag={att.name} code={pcode(code)} prefix=\"{sanitize(prefix)}\"")
               Binary.add_uint8(ctxt.buf, code); (ctxt,prefix,true)
             case {none}:
               match (StringMap.get(att.name,ctxt.st)) {
               case {some:idx}:
                 debug(ctxt,2,"WBXml.encode","ATTRSTART tag={att.name} idx={idx} noprefix")
                 ignore(literal(ctxt,idx,[],[])); (ctxt,"",true);
               case {none}: (ctxt,"",false);
               }
             }
           default:
             match (StringMap.get(att.name,ctxt.st)) {
             case {some:idx}:
               debug(ctxt,2,"WBXml.encode","ATTRSTART tag={att.name} idx={idx}")
               ignore(literal(ctxt,idx,[],[])); (ctxt,"",true);
             case {none}: (ctxt,"",false);
             }
           }) {
        case (ctxt,prefix,{true}):
          plen = String.length(prefix)
          value = if (prefix == "") att.value else String.sub(plen,String.length(att.value)-plen,att.value)
          match (get_mapping({attrvalue}, ctxt, value, namespace)) {
          case {some:{attrvalue:(cp,code)}}:
            if (code >= 128) {
              ctxt = switch_page(ctxt, {att}, cp)
              debug(ctxt,2,"WBXml.encode","ATTRVALUE tag={value} code={pcode(code)}")
              Binary.add_uint8(ctxt.buf, code)
              ctxt
            } else
              str_i(ctxt, value);
          default:
            str_i(ctxt, value);
          }
        case (ctxt,_,{false}): ctxt;
        }},atts,ctxt)
      {success:end_tag("attributes",ctxt)}
    } else {success:ctxt}
  }

  private function remove_trailing_colon(string str) {
    if (String.has_suffix(":",str)) Option.default("",String.get_prefix(String.length(str)-1,str)) else str
  }

  private function push_namespace(ctxt, nsxml) {
//jlog("push_namespace: {Ansi.print({yellow},"{nsxml}")}")
    if (nsxml == [])
      ctxt
    else {
      List.fold(function (binding, ctxt) {
                  match (binding) {
                  case ~{default:dflt}:
                    dflt = remove_trailing_colon(dflt)
                    debug(ctxt,2,"WBXml.encode","push default -> {Ansi.print({yellow},dflt)}");
                    {ctxt with default_namespaces:[dflt|ctxt.default_namespaces]};
                  case ~{name, uri}:
                    uri = remove_trailing_colon(uri)
                    debug(ctxt,2,"WBXml.encode","push {Ansi.print({yellow},"{name} -> {uri}")}");
                    nns = Option.default([],StringMap.get(name,ctxt.named_namespaces))
                    nns = [uri|nns]
                    {ctxt with named_namespaces:StringMap.add(name,nns,ctxt.named_namespaces)};
                  }
                },nsxml,ctxt)
    }
  }

  private function pop_list(list('a) l) { match (l) { case []: []; case [_|t]: t; } }

  private function pop_namespace(ctxt, nsxml) {
//jlog("pop_namespace: {Ansi.print({yellow},"{nsxml}")}")
    if (nsxml == [])
      ctxt
    else {
      List.fold(function (binding, ctxt) {
                  match (binding) {
                  case ~{default:dflt}:
                    debug(ctxt,2,"WBXml.encode","pop default {Ansi.print({yellow},dflt)}");
                    {ctxt with default_namespaces:pop_list(ctxt.default_namespaces)};
                  case ~{name, uri}:
                    uri = remove_trailing_colon(uri)
                    debug(ctxt,2,"WBXml.encode","pop {Ansi.print({yellow},"{name} -> {uri}")}");
                    nns = Option.default([],StringMap.get(name,ctxt.named_namespaces))
                    nns = pop_list(nns)
                    {ctxt with named_namespaces:StringMap.add(name,nns,ctxt.named_namespaces)};
                  }
                },nsxml,ctxt)
    }
  }

  function start_tag(ctxt, tag, namespace, nsxml, atts, content) {
    debug(ctxt,2,"WBXml.encode","tag {tp(tag)}")
//jlog("namespace:{Ansi.print({cyan},namespace)}  nsxml:{Ansi.print({cyan},"{nsxml}")}  atts:{atts}")
    ctxt = push_namespace(ctxt, nsxml)
    match (get_mapping({tag}, ctxt, tag, namespace)) {
    case {some:{tag:(cp,code)}}:
      ctxt = switch_page(ctxt, {tag}, cp)
      code = if (List.length(atts) > 0) Bitwise.lor(code,0x80) else code
      code = if (List.length(content) > 0) Bitwise.lor(code,0x40) else code
      debug(ctxt,2,"WBXml.encode","{pcode(code)} for {tp(tag)}")
      Binary.add_uint8(ctxt.buf,code)
      attributes(ctxt, namespace, atts);
    default:
      match (StringMap.get(tag,ctxt.st)) {
      case {some:idx}:
        ctxt = literal(ctxt, idx, atts, content)
        attributes(ctxt, namespace, atts);
      case {none}:
        {failure:"Can't encode tag {tp(tag)}"};
      }
    }
  }

  function of_xmlns0(ctxt, xmlns xmlns) {
//jlog("of_xmlns0: xmlns={Xmlns.to_string(xmlns)}\n{String.replace("\n","\\n","{xmlns}")}")
    match (xmlns) {
      case {text:""}: {success:ctxt};
      case {~text}: // *MUST NOT* have null characters
        if (String.contains(text,"\000"))
          {failure:"String contains null \"{sanitize(text)}\""}
        else
          match (StringMap.get(text, ctxt.st)) {
          case {some:idx}: {success:str_t(ctxt, idx)};
          case {none}: {success:str_i(ctxt, text)};
          }
      case {args:[], content:[], ~namespace, specific_attributes:_, xmlns:nsxml, ~tag}:
        match (start_tag(ctxt, tag, namespace, nsxml, [], [])) {
        case {success:ctxt}:
          ctxt = pop_namespace(ctxt, nsxml)
          {success:ctxt};
        case {~failure}: {~failure};
        }
      case {args:[], content:[{~text}], namespace:_, specific_attributes:_, xmlns:[], tag:"Opaque"}:
        {success:opaque(ctxt, text)};
      case {~args, content:[{fragment:content}], ~namespace, specific_attributes:_, xmlns:nsxml, ~tag}
      case {~args, ~content, ~namespace, specific_attributes:_, xmlns:nsxml, ~tag}:
        match (start_tag(ctxt, tag, namespace, nsxml, args, content)) {
        case {success:ctxt}:
          match (List.fold(function (xmlns, ctxt) {
                             match (ctxt) {
                             case {success:ctxt}: of_xmlns0(ctxt, xmlns);
                             case {~failure}: {~failure};
                           }},content,{success:ctxt})) {
          case {success:ctxt}:
            ctxt = if (content != []) end_tag(tag,ctxt) else ctxt
            ctxt = pop_namespace(ctxt, nsxml)
            {success:ctxt};
          case {~failure}: {~failure};
          }
        case {~failure}: {~failure};
        }
      case {~fragment}: Outcome_fold(function (xmlns, ctxt) { of_xmlns0(ctxt, xmlns) },fragment,ctxt);
      case {content_unsafe:_}: {failure:"content_unsafe"};
      case {xml_dialect:_}: {failure:"xml_dialect"};
    }
  }

  /* Encoding pre-pass.  We need to preload the string table. */

  function string_table_of_attributes(ctxt, namespace, atts) {
    List.fold(function (att, ctxt) {
                match (get_mapping({attrstart}, ctxt, att.name, namespace)) {
                case {some:_}: ctxt;
                case {none}: add_to_string_table(ctxt, att.name);
                }
              },atts,ctxt)
  }

  function string_table_of_xmlns(ctxt, xmlns xmlns) {
    match (xmlns) {
      case {text:""}: ctxt;
      case {~text}: add_to_string_table(ctxt, text);
      case {content_unsafe:_}: ctxt;
      case {fragment:_}: ctxt;
      case {xml_dialect:_}: ctxt;
      case {~args, ~content, ~namespace, specific_attributes:_, xmlns:nsxml, ~tag}:
        ctxt = push_namespace(ctxt, nsxml)
        ctxt =
          match (get_mapping({tag}, ctxt, tag, namespace)) {
          case {none}: add_to_string_table(ctxt, tag);
          case {some:_}: ctxt;
          }
        ctxt = string_table_of_attributes(ctxt, namespace, args)
        ctxt = pop_namespace(ctxt, nsxml)
        List.fold(function (xmlns, ctxt) { string_table_of_xmlns(ctxt, xmlns) },content,ctxt)
    }
  }

  /* Main exported routine, encode an XML value as WBXml */

  function of_xmlns(WBXml.context ctxt, xmlns xmlns, int hint) {
    debug(ctxt,1,"WBXml.of_xmlns","xmlns=\n{Ansi.print({cyan},Xmlns.to_string(xmlns))}")
    ctxt = { ctxt with buf:Binary.create(hint), tag_cp:0, tag_ns:"", att_cp:0, att_ns:"", nsti:0, st:StringMap.empty }
    ctxt = if (ctxt.use_st) string_table_of_xmlns(ctxt, xmlns) else ctxt
    ctxt = start(ctxt)
    match (of_xmlns0(ctxt, xmlns)) {
    case {success:ctxt}:
      debug(ctxt,1,"WBXml.of_xmlns","wbxml=\n{Ansi.print({green},bindump(ctxt.buf))}")
      {success:ctxt};
    case {~failure}:
      debug(ctxt,1,"WBXml.of_xmlns","failure=\"{Ansi.print({red},failure)}\"")
      {~failure};
    }
  }

  /* Start of decode section */

  private function where(ctxt, from) {
    b = Binary.get_binary(ctxt.buf,ctxt.pos,Int.min(Binary.length(ctxt.buf)-ctxt.pos,16))
    dump = String.trim(bindump(b))
    str = Ansi.print({green},String.sub(5,String.length(dump)-5,dump))
    debug(ctxt, 3, "WBXml {from}", str)
  }

  private function get_strtab(ctxt, strtablen) {
    len = ctxt.pos + strtablen
    recursive function aux(last, i) {
      if (last == len)
        {success:(last,[])}
      else if (last > len)
        {failure:"WBXml.get_strtab: strings ran over string table length"}
      else {
        if (Binary.get_uint8(ctxt.buf, last+i) == 0) {
          str = Binary.get_string(ctxt.buf, last, i)
          match (aux(last+i+1, 0)) {
          case {success:(pos,strs)}: {success:(pos,[str|strs])};
          case {~failure}: {~failure};
          }
        } else aux(last, i+1)
      }
    }
    match (aux(ctxt.pos,0)) {
    case {success:(pos,strs)}:
      st = List.foldi(function (i, str, st) { StringMap.add(str, i, st) },strs,StringMap.empty)
      rst = List.foldi(function (i, str, rst) { IntMap.add(i, str, rst) },strs,IntMap.empty)
      {success:~{ctxt with pos, st, rst}}
    case {~failure}: {~failure};
    }
  }

  function get_publicid(ctxt) {
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:0}:
      match (MBInt.to_int_posr(ctxt.buf,ctxt.pos+1)) {
      case {success:(pos,idx)}: {success:({ctxt with ~pos},{~idx})}
      case {~failure}: {~failure};
      }
    case {success:_}:
      match (MBInt.to_int_posr(ctxt.buf,ctxt.pos)) {
      case {success:(pos,id)}: {success:({ctxt with ~pos},{~id})}
      case {~failure}: {~failure};
      }
    case {~failure}: {~failure};
    }
  }

  function get_header(ctxt) {
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:version}:
      ctxt = {ctxt with pos:ctxt.pos+1}
      match (get_publicid(ctxt)) {
      case {success:(ctxt,publicid)}:
        match (MBInt.to_int_posr(ctxt.buf,ctxt.pos)) {
        case {success:(pos,charset)}:
          match (MBInt.to_int_posr(ctxt.buf,pos)) {
          case {success:(pos,strtablen)}:
            if (Binary.length(ctxt.buf) - pos > strtablen) {
              match (get_strtab({ctxt with ~pos}, strtablen)) {
              case {success:ctxt}:
                match (publicid) {
                case {~id}: {success:{ctxt with header:~{version, publicid:{~id}, charset}}};
                case {~idx}:
                  match (IntMap.get(idx, ctxt.rst)) {
                  case {some:str}: {success:{ctxt with header:~{version, publicid:{~str}, charset}}};
                  case {none}: {failure:"Public ID string not in string table"};
                  }
                }
              case {~failure}: {~failure};
              }
            } else
              {failure:"WBXml.get_header: not enough data for string table"};
          case {~failure}: {~failure};
          }
        case {~failure}: {~failure};
        }
      case {~failure}: {~failure};
      }
    case {~failure}: {~failure};
    }
  }

  function get_str_i(ctxt) {
    recursive function aux(i) {
      match (Binary.get_uint8r(ctxt.buf,ctxt.pos+i)) {
      case {success:0}: {success:({ctxt with pos:ctxt.pos+i+1},Binary.get_string(ctxt.buf,ctxt.pos,i))};
      case {success:_}: aux(i+1);
      case {~failure}: {~failure};
      }
    }
    aux(0)
  }

  function get_str_t(ctxt) {
    match (MBInt.to_int_posr(ctxt.buf,ctxt.pos)) {
    case {success:(pos,index)}:
      match (IntMap.get(index,ctxt.rst)) {
      case {some:str}: {success:({ctxt with ~pos},str)};
      case {none}: {failure:"String not found in string table"};
      }
    case {~failure}: {~failure};
    }
  }

  function get_opaque(ctxt) {
    match (MBInt.to_int_posr(ctxt.buf,ctxt.pos)) {
    case {success:(pos,length)}:
      match (Binary.get_binaryr(ctxt.buf,pos,length)) {
      case {success:bin}:
        str = %%bslBinary.to_encoding%%(bin,"binary")
        debug(ctxt, 2, "WBXml.decode", "got opaque {Ansi.print({red},Int.to_string(String.length(str)))} bytes")
        {success:({ctxt with pos:pos+length},str)};
      case {~failure}: {~failure};
      }
    case {~failure}: {~failure};
    }
  }

  function get_entity(ctxt) {
    match (MBInt.to_int_posr(ctxt.buf,ctxt.pos)) {
    case {success:(pos,unicode)}: {success:({ctxt with ~pos},Text.to_string(Text.from_character(unicode)))};
    case {~failure}: {~failure};
    }
  }

  private function tagattrstr(tagattr) { match (tagattr) { case {tag}: "tag"; case {attr}: "attribute"; }
  }

  function get_switch_page(ctxt, tagattr, (WBXml.context -> outcome(_,string)) goto) {
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:cp}:
      ns = match (IntMap.get(cp,ctxt.namespace_map)) { case {some:ns}: ns; case {none}: ""; }
      ctxt =
        match (tagattr) {
        case {tag}: {ctxt with tag_cp:cp, tag_ns:ns};
        case {attr}: {ctxt with att_cp:cp, tag_ns:ns};
        }
      debug(ctxt, 2, "WBXml.decode", "Switch to {tagattrstr(tagattr)} codepage {pcode(cp)} ({pns(ns)})")
      goto({ctxt with pos:ctxt.pos+1});
    case {~failure}: {~failure};
    }
  }

  function get_text(tag,what) {
    match (what) {
    case {success:(ctxt,text)}:
      debug(ctxt, 2, "WBXml.decode", "got text \"{sanitize(text)}\"")
      match (get_content(tag,ctxt)) {
      case {success:(ctxt,content)}: {success:(ctxt,[{~text}|content])};
      case {~failure}: {~failure};
      }
    case {~failure}: {~failure};
    }
  }

  function outcome((WBXml.context,list(xmlns)),string) get_content(tag,ctxt) {
    where(ctxt,"content({tp(tag)})")
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:code}:
      debug(ctxt, 4, "code", "{pcode(code)}")
      match (code) {
      case /*SWITCH_PAGE*/0x00: get_switch_page({ctxt with pos:ctxt.pos+1}, {tag}, get_content(tag,_));
      case /*ENTITY*/0x02: get_text(tag,get_entity({ctxt with pos:ctxt.pos+1}));
      case /*STR_I*/0x03: get_text(tag,get_str_i({ctxt with pos:ctxt.pos+1}));
      case /*STR_T*/0x83: get_text(tag,get_str_t({ctxt with pos:ctxt.pos+1}));
      case /*OPAQUE*/0xC3: get_text(tag,get_opaque({ctxt with pos:ctxt.pos+1}));
      case /*END*/0x01:
        debug(ctxt, 2, "WBXml.decode", "got END(content)")
        {success:({ctxt with pos:ctxt.pos+1},[])};
      default:
        match (get_tag(ctxt)) {
        case {success:(ctxt,element)}:
          match (get_content(tag,ctxt)) {
          case {success:(ctxt,content)}: {success:(ctxt,[element|content])};
          case {~failure}: {~failure};
          }
        case {~failure}: {~failure};
        }
      }
    case {~failure}: {~failure};
    }
  }

  function get_avalue(what) {
    match (what) {
    case {success:(ctxt,value)}:
      debug(ctxt, 2, "WBXml.decode", "got attrvalue \"{sanitize(value)}\"")
      match (get_attrvalue(ctxt)) {
      case {success:(ctxt,values)}: {success:(ctxt,[value|values])};
      case {~failure}: {~failure};
      }
    case {~failure}: {~failure};
    }
  }

  function outcome((WBXml.context,list(string)),string) get_attrvalue(ctxt) {
    where(ctxt,"attrvalue")
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:code}:
      debug(ctxt, 4, "code", "{pcode(code)}")
      match (code) {
      case /*SWITCH_PAGE*/0x00: get_switch_page(ctxt, {attr}, get_attrvalue);
      case /*ENTITY*/0x02: get_avalue(get_entity({ctxt with pos:ctxt.pos+1}));
      case /*STR_I*/0x03: get_avalue(get_str_i({ctxt with pos:ctxt.pos+1}));
      case /*STR_T*/0x83: get_avalue(get_str_t({ctxt with pos:ctxt.pos+1}));
      case /*LITERAL*/0x04 // attrValue never has LITERAL
      case /*LITERAL_C*/0x44
      case /*LITERAL_A*/0x84
      case /*LITERAL_AC*/0xc4
      case /*END*/0x01:
        debug(ctxt, 2, "WBXml.decode", "got END(attrValue)")
        {success:(ctxt,[])};
      default:
        if (code <= 128) {
          debug(ctxt, 2, "WBXml.decode", "got END(attrValue)")
          {success:(ctxt,[])};
        } else
          match (get_attrvalue_from_code(ctxt, ctxt.att_cp, code)) {
          case {some:str}: get_avalue({success:({ctxt with pos:ctxt.pos+1},str)});
          case {none}: {failure:"Unknown code(attrValue): 0x{Int.to_hex(code)}"};
          }
      }
    case {~failure}: {~failure};
    }
  }

  function get_astart(ctxt, name, prefix) {
    pfx = if (prefix == "") "" else "=\"{sanitize(prefix)}...\""
    debug(ctxt, 2, "WBXml.decode", "got attrstart {Ansi.print({blue},name)}{pfx}")
    match (get_attrvalue(ctxt)) {
    case {success:(ctxt,values)}:
      match (get_attrstart(ctxt)) {
      case {success:(ctxt,attributes)}:
        value = String.concat("",[prefix|values])
        {success:(ctxt,[{~name, namespace:"", ~value}|attributes])};
      case {~failure}: {~failure};
      }
    case {~failure}: {~failure};
    }
  }

  function outcome((WBXml.context,list(Xml.attribute)),string) get_attrstart(ctxt) {
    where(ctxt,"attrstart")
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:code}:
      debug(ctxt, 4, "code","{pcode(code)}")
      match (code) {
      case /*SWITCH_PAGE*/0x00: get_switch_page(ctxt, {attr}, get_attrstart);
      case /*LITERAL*/0x04
      case /*LITERAL_C*/0x44
      case /*LITERAL_A*/0x84
      case /*LITERAL_AC*/0xc4:
        match (MBInt.to_int_posr(ctxt.buf,ctxt.pos+1)) {
        case {success:(pos,index)}:
          ctxt = {ctxt with ~pos}
          match (IntMap.get(index,ctxt.rst)) {
          case {some:name}: get_astart(ctxt, name, "");
          case {none}: {failure:"Literal attribute start not in string table"};
          }
        case {~failure}: {~failure};
        }
      case /*END*/0x01:
        debug(ctxt, 2, "WBXml.decode", "got END(attrStart)")
        {success:({ctxt with pos:ctxt.pos+1},[])};
      default:
        if (code < 128)
          match (get_attrstart_from_code(ctxt, ctxt.att_cp, code)) {
          case {some:(name,prefix)}: get_astart({ctxt with pos:ctxt.pos+1}, name, prefix);
          case {none}: {failure:"Unknown code(attrStart): 0x{Int.to_hex(code)}"};
          }
        else
          {failure:"Unknown code(attrStart): 0x{Int.to_hex(code)}"};
      }
    case {~failure}: {~failure};
    }
  }

  function make_tag(ctxt, tag, args, content) {
    debug(ctxt, 2, "WBXml.decode", "make tag {Ansi.print({blue},tag)}")
    {success:(ctxt,{~args, ~content, namespace:ctxt.tag_ns, specific_attributes:none, xmlns:[], ~tag})}
  }

  function make_content(ctxt, tag, atts, has_content) {
    if (has_content)
      match (get_content(tag,ctxt)) {
      case {success:(ctxt,content)}: make_tag(ctxt, tag, atts, content);
      case {~failure}: {~failure};
      }
    else make_tag(ctxt, tag, atts, [])
  }

  function got_tag(ctxt, tag, has_atts, has_content) {
    debug(ctxt, 2, "WBXml.decode", "tag {Ansi.print({blue},tag)} attributes:{bp(has_atts)} content:{bp(has_content)}")
    if (has_atts)
      match (get_attrstart(ctxt)) {
      case {success:(ctxt,atts)}: make_content(ctxt, tag, atts, has_content);
      case {~failure}: {~failure};
      }
    else make_content(ctxt, tag, [], has_content)
  }

  function get_tag(ctxt) {
    where(ctxt,"tag")
    match (Binary.get_uint8r(ctxt.buf,ctxt.pos)) {
    case {success:stag}:
      ctxt = {ctxt with pos:ctxt.pos+1}
      debug(ctxt, 4, "stag","{pcode(stag)}")
      has_atts = Bitwise.land(stag,0x80) != 0
      has_content = Bitwise.land(stag,0x40) != 0
      stag = Bitwise.land(stag,0x3f)
      match (stag) {
      case /*SWITCH_PAGE*/0x00: get_switch_page(ctxt, {tag}, get_tag);
      case /*LITERAL*/0x04:
        match (MBInt.to_int_posr(ctxt.buf,ctxt.pos)) {
        case {success:(pos,index)}:
          ctxt = {ctxt with ~pos}
          match (IntMap.get(index,ctxt.rst)) {
          case {some:tag}: got_tag(ctxt, tag, has_atts, has_content);
          case {none}: {failure:"Literal tag not in string table"};
          }
        case {~failure}: {~failure};
        }
      default:
        match (get_tag_from_code(ctxt, ctxt.tag_cp, stag)) {
        case {some:tag}: got_tag(ctxt, tag, has_atts, has_content);
        case {none}: {failure:"Tag code not in code space {pcode(stag)}"};
        }
      }
    case {~failure}: {~failure};
    }
  }

  /* Main exported decode routine. */

  function to_xmlns(WBXml.context ctxt, binary buf) {
    if (Binary.length(buf) == 0) {
      debug(ctxt,1,"WBXml.to_xmlns","xmlns=(Empty)")
      {success:(ctxt,{none})}
    } else {
      ctxt = {ctxt with ~buf, pos:0}
      debug(ctxt,1,"WBXml.to_xmlns","wbxml=\n{Ansi.print({green},String.trim(bindump(ctxt.buf)))}")
      match
        (match (get_header(ctxt)) {
         case {success:ctxt}: get_tag(ctxt);
         case {~failure}: {~failure};
         }) {
      case {success:(ctxt,xmlns)}:
        debug(ctxt,1,"WBXml.to_xmlns","xmlns=\n{Ansi.print({cyan},Xmlns.to_string(xmlns))}")
        {success:(ctxt,{~xmlns})};
      case {~failure}:
        debug(ctxt,1,"WBXml.to_xmlns","failure=\"{Ansi.print({red},failure)}\"")
        {~failure};
      }
    }
  }

}

