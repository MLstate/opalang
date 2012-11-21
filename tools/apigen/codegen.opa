/**
 * Code generator.
 *
 * The sole purpose of this file is currently to allow the generation of
 * Opa code in either classic or js-like syntax.  It probably has no future
 * for this reason, especially if classic syntax is ever deprecated and
 * also since stdlib can now accept js-like syntax files.
 *
 * It produces a very simple subset of Opa, just enough to generate
 * automatic code from simple descriptions.  Note, however, that it
 * it is intended to generate readable code and so contains layout
 * information and comments/documentation etc.  It is not intended
 * to be a faithful representation of Opa code, for example, base
 * types (int, bool, string, etc.) are all handled as simple strings.
 * It is also possible to generate illegal code.
 *
 * Currently, the layout algorithm is a trivial write-once making decisions
 * locally within the code.  This may be replaced, in future with a more
 * optimal polynomial-time algorithm.
 *
 * This file provides a type to store an abstracted AST for an Opa program,
 * a simple layout algorithm which generates a text representation of the program
 * and a set of simple abbreviations for constructing the AST, for example:
 * erecs builds a record expression from a list of string pairs,
 * eg. [("a","1"),("b","~"),("c","")] -> {a:1,~b,c}.
 *
 * Verbatim entries can be both: evb("f(x,y)") or split by style:
 * evs("function (x) { x }","(x -> x)").
 */

/* Types */

type styled_code = {string js, string cl} or {string bth}

type cmnt = string
type cmnts = list(string)
type doc = string
type docs = list(string)

type ctyp =
    {empty}
 or {string tyname}
 or {string tyname, list(ctyp) args}
 or {cols cols}

type cols = list(row)

type row = list(rowel)

type rowel = (string,ctyp)

type id = {string id, ctyp typ} or {string id}

type fnargs = list(pat)

type stmnt =
    {empty} // outputs nothing
 or {blank} // outputs empty line
 or {cmnt cmnt}
 or {cmnts cmnts}
 or {doc doc}
 or {docs docs}
 or {styled_code verbatim}
 or {id id}
 or {pat pat, exp exp}
 or {exp exp}
 or {modfunc func}
 or {dec dec}
 or {block block}

type block = list(stmnt)

type ilfunc = {fnargs args, block body}

type exp =
    {empty}
 or {wild}
 or {id id}
 or {erecord rcrd}
 or {tilde, erecord rcrd}
 or {exp wth, erecord rcrd}
 or {list(exp) tup}
 or {list(exp) lst}
 or {mtch mtch}
 or {(exp,exp,exp) ite}
 or {ilfunc ilfunc}
 or {exp fn, list(exp) args}
 or {exp doexp}
 or {exp exp, ctyp typ}
 or {block block}
 or {styled_code verbatim}

type rexp =
    {plain}
 or {tilde}
 or {exp exp}
type erecel = (string,rexp)
type erecord = list(erecel)

type mtch = {exp exp, pes pes}

type pat =
    {id id}
 or {precord rcrd}
 or {tilde, precord rcrd}
 or {list(pat) tup}
 or {list(pat) lst}
 or {wild}
 or {styled_code verbatim}

type ppat =
    {plain}
 or {tilde}
 or {wild}
 or {pat pat}
type precel = (string,ppat)
type precord = list(precel)

type pe = (pat,exp)

type pes = list(pe)

type typdef = {string name, ctyp typ}

type modfunc = {string name, fnargs args, ctyp return_type, block body}

type dec0('a) =
    {blank}
 or {cmnt cmnt}
 or {cmnts cmnts}
 or {doc doc}
 or {docs docs}
 or {typdef typdef}
 or {modfunc mod}
 or {modfunc func}
 or {exp exp}
 or {stmnt stmnt}
 or {styled_code verbatim}
type dec = dec0(dec)

type opa = list(dec)

type context = {
  style style,
  text indent,
  bool nested,
  int start,
  int width
}

/* Code to handle text */

tx = Text.cons
lt = Text.ltconcat
l = Text.length
function ltt(list(string) ss) { List.map(tx,ss) }
function ls(list(string) ss) { lt(ltt(ss)) }
function tc(text sep,list(text) tt) {
  recursive function aux(tt) {
    match (tt) {
    case []: [];
    case [t]: [t];
    case [t|tt]: [t,sep|aux(tt)];
    }
  }
  lt(aux(tt))
}
function tc2(context c,text sep,list(text) tt) {
  indentlen = l(c.indent)
  seplen = l(sep)
  recursive function aux(pos,tt) {
    match (tt) {
    case []: [];
    case [t]: [t];
    case [t|tt]:
      tlen = l(t)
      if (pos+tlen+seplen >= c.width)
        [t_cr,c.indent,t,sep|aux(indentlen+tlen+seplen,tt)]
      else
        [t,sep|aux(pos+tlen+seplen,tt)];
    }
  }
  lt(aux(c.start,tt))
}
function sc(text sep,list(string) ss) { tc(sep,ltt(ss)) }
function sps(int n) { tx(String.repeat(n," ")) }

/* Pre-defined text strings */

t_type = tx("type")
t_module = tx("module")
t_function = tx("function")
t_b = tx("\{")
t_e = tx("}")
t_bb = tx("\{\{")
t_ee = tx("}}")
t_sp = tx(" ")
t_sp2 = tx("  ")
t_cr = tx("\n")
t_empty = tx("")
t_tab = tx("\t")
t_com = tx(",")
t_col = tx(":")
t_sem = tx(";")
t_eq = tx("=")
t_ob = tx("(")
t_cb = tx(")")
t_os = tx("[")
t_cs = tx("]")
t_match = tx("match")
t_with = tx("with")
t_case = tx("case")
t_pipe = tx("|")
t_rar = tx("->")
t_us = tx("_")
t_end = tx("end")
t_do = tx("do")
t_ds = tx("/**")
t_cms = tx("/*")
t_cde = tx("*/")
t_ss = tx("//")
t_st = tx("*")
t_or = tx("or")
t_sl = tx("/")
t_til = tx("~")
t_if = tx("if")
t_then = tx("then")
t_else = tx("else")

// For debugging the layout
//mark = Mutable.make(0)
//function t_mark(s) { mark.set(mark.get()+1); tx(s^Int.to_string(mark.get())) }

/* Context manipulation */

function context iin(context c, int n) { {c with indent:lt([c.indent,sps(n)])} }
function context ii(context c) { iin(c,2) }
function context ie(context c) { {c with indent:t_empty} }
function context is(context c, int n) { {c with indent:sps(n)} }
function context ns(context c) { {c with nested:true} }
function context scs(context c,int start) { {c with ~start} }
function context ics(context c,int delta) { {c with start:c.start+delta} }

function context init_context(style style) { {~style, indent:sps(0), nested:false, start:0, width:130} }

/* The code formatter */

function styled(context c, js, cl) { match (c.style) { case {js_like}: js; case {classic}: cl; } }

function roweltt(context c,rowel rowel) {
  match (rowel) {
  case (name,typ):
    t_name = tx(name)
    match (c.style) {
    case {js_like}: lt([ctyptt(c,typ),t_sp,t_name]);
    case {classic}: lt([t_name,t_col,ctyptt(c,typ)]);
    }
  }
}

function rowtt(context c,row row) {
  match (row) {
  case []: lt([t_b,t_e]);
  case [rowel]: lt([t_b,roweltt(c,rowel),t_e]);
  default: lt([t_b,t_cr,ii(c).indent,
               tc(lt([styled(c,t_com,t_sem),t_cr,ii(c).indent]),List.map(roweltt(c,_),row)),
               t_cr,c.indent,t_e])
  }
}

function colstt(context c,cols cols) {
  match (cols) {
  case []: t_empty;
  case [col]: tc(lt([t_sp,styled(c,t_or,t_sl),t_sp]),List.map(rowtt(c,_),[col]));
  default:
    sep = styled(c,t_or,t_sl)
    lt([t_cr,ii(c).indent,sps(l(sep)+1),tc(lt([t_cr,ii(c).indent,sep,t_sp]),List.map(rowtt(c,_),cols))]);
  }
}

function tyargstt(context c,list(ctyp) args) { lt([t_ob,tc(t_com,List.map(ctyptt(c,_),args)),t_cb]) }

function ctyptt(context c,ctyp ctyp) {
  match (ctyp) {
  case {empty}: t_empty;
  case ~{tyname}: tx(tyname);
  case ~{tyname, args}: lt([tx(tyname),tyargstt(c,args)]);
  case ~{cols}: colstt(c,cols);
  }
}

function int typ_length(ctyp ctyp) {
  match (ctyp) {
  case {empty}: 0;
  case ~{tyname}: String.length(tyname);
  case ~{tyname,args:[]}: String.length(tyname);
  case ~{tyname,args}: String.length(tyname)+1+List.length(args)+List.fold(function (arg,len) {len+typ_length(arg)},args,0);
  case ~{cols:_}: 4;//TODO
  }
}

function idtt(context c,id id) {
  match (id) {
  case ~{id, typ}:
    match (c.style) {
    case {js_like}: lt([ctyptt(ie(c),typ),t_sp,tx(id)]);
    case {classic}: lt([tx(id),t_col,ctyptt(ie(c),typ)]);
    }
  case ~{id}: tx(id);
  }
}

function id_length(context c,id id) {
  match (id) {
  case ~{id, typ}:
    match (c.style) {
    case {js_like}: typ_length(typ)+1+String.length(id);
    case {classic}: String.length(id)+1+typ_length(typ);
    }
  case ~{id}: String.length(id);
  }
}

function ereceltt(context c,erecel erecel) {
  name = tx(erecel.f1)
  match (erecel.f2) {
  case {plain}: name;
  case {tilde}: lt([t_til,name]);
  case ~{exp}: lt([name,styled(c,t_col,t_eq),exptt(ics(c,l(name)+1),exp)]);
  }
}

function erecordtt(context c,bool tilde,option(exp) wth,erecord erecord) {
  wth =
    match ((erecord,wth)) {
    case ([],{some:exp}): exptt(c,exp);
    case ([_],{some:exp}): lt([exptt(c,exp),t_sp,t_with,t_sp]);
    case (_,{some:exp}): lt([exptt(c,exp),t_sp,t_with,t_cr,ii(c).indent,t_sp2]);
    case default: t_empty;
    }
  match (erecord) {
  case []: lt([t_b,wth,t_e]);
  case [erecel]: lt([t_tilde(tilde),wth,ereceltt(c,erecel),t_e]);
  default:
    tillen = l(t_tilde(tilde))
    wthlen = l(wth)
    c2 = scs(c,l(c.indent)+tillen+wthlen)
    lt([t_tilde(tilde),wth,tc2(c2,lt([styled(c,t_com,t_sem),t_sp]),List.map(ereceltt(c,_),erecord)),t_e])
  }
}

function preceltt(context c,precel precel) {
  name = tx(precel.f1)
  match (precel.f2) {
  case {plain}: name;
  case {tilde}: lt([t_til,name]);
  case {wild}: lt([name,styled(c,t_col,t_eq),t_us]);
  case ~{pat}: lt([name,styled(c,t_col,t_eq),pattt(c,pat)]);
  }
}

function t_tilde(bool tilde) { if (tilde) lt([t_til,t_b]) else t_b }

function precordtt(context c,bool tilde,precord precord) {
  lt([t_tilde(tilde),tc2(c,lt([styled(c,t_com,t_sem)]),List.map(preceltt(c,_),precord)),t_e])
}

function styledtt(context c,styled_code styled) {
  match (styled) {
  case ~{js, cl}:
    match (c.style) {
    case {js_like}: tx(js);
    case {classic}: tx(cl);
    }
  case ~{bth}: tx(bth);
  }
}

function pseqtt(context c, list(pat) pats, start, finish) {
  match (pats) {
  case []: lt([start,finish]);
  case [pat]: lt([start,pattt(c,pat),finish]);
  default: lt([start,tc2(c,t_com,List.map(pattt(c,_),pats)),finish]);
  }
}

plsttt = pseqtt(_, _, t_os, t_cs)

function ptuptt(context c, list(pat) pats) {
  match (pats) {
  case []: t_empty;
  case [pat]: pattt(c,pat);
  default: lt([t_ob,tc2(c,t_com,List.map(pattt(c,_),pats)),t_cb]);
  }
}

function pattt(context c,pat pat) {
  match (pat) {
  case ~{id}: idtt(c,id);
  case ~{rcrd}: precordtt(c,false,rcrd);
  case {tilde, ~rcrd}: precordtt(c,true,rcrd);
  case ~{tup}: ptuptt(c,tup);
  case ~{lst}: plsttt(c,lst);
  case {wild}: t_us;
  case ~{verbatim}: styledtt(c,verbatim);
  }
}

function pett(context c, (pat,exp) (pat,exp)) {
  l2 = styled(c,2,4) // ": " or " -> "
  t_pat = pattt(c,pat); pl = l(t_pat)
  t_exp = exptt(ii(c),exp); el = l(t_exp)
  t_sep = if (c.start+pl+l2+el > c.width) lt([t_cr,ii(c).indent]) else t_sp
  match (c.style) {
  case {js_like}:
    lt([t_case,t_sp,t_pat,t_col,t_sep,t_exp]);
  case {classic}:
    lt([t_pipe,t_sp,t_pat,t_sp,t_rar,t_sep,t_exp]);
  }
}

function matchtt(context c, mtch mtch) {
  c2 = ns(c)
  sep = lt([t_cr,c.indent])
  match (mtch) {//TODO: match nested
  case ~{exp, pes}:
    match (c.style) {
    case {js_like}: lt([if (c.nested) sep else t_empty,
                        t_match,t_sp,t_ob,exptt(c2,exp),t_cb,t_sp,t_b,t_cr,
                        c.indent,tc(sep,List.map(pett(ii(c2),_),pes)),t_cr,
                        c.indent,t_e
                       ]);
    case {classic}: lt([if (c.nested) sep else t_empty,
                        t_match,t_sp,exptt(c2,exp),t_sp,t_with,t_cr,
                        c.indent,tc(sep,List.map(pett(ii(c2),_),pes)),t_cr,
                        c.indent,t_end,
                       ]);
    }
  }
}

function itett(context c, (exp,exp,exp) ite) {
  match (c.style) {//nested
  case {js_like}: lt([t_cr,
                      c.indent,t_if,t_sp,t_ob,exptt(c,ite.f1),t_cb,t_sp,t_b,t_cr,
                      ii(c).indent,exptt(c,ite.f2),t_cr,
                      c.indent,t_e,t_sp,t_else,t_sp,t_b,t_cr,
                      ii(c).indent,exptt(c,ite.f3),t_cr,
                      c.indent,t_e
                     ]);
  case {classic}: lt([t_cr,
                      c.indent,t_if,t_sp,exptt(c,ite.f1),t_cr,
                      c.indent,t_then,t_cr,
                      ii(c).indent,exptt(c,ite.f2),t_cr,
                      c.indent,t_else,t_cr,
                      ii(c).indent,exptt(c,ite.f3)
                     ]);
  }
}

function doexptt(context c, exp exp) {
  match (c.style) {
  case {js_like}: lt([exptt(c,exp),t_sem]);
  case {classic}: lt([t_do,t_sp,exptt(c,exp)]);
  }
}

function ilfunctt(context c,ilfunc ilfunc) {
  match (ilfunc) {
  case ~{args, body}:
    match (c.style) {
    case {js_like}: lt([t_function,t_sp,fnargstt(c,args),t_sp,t_b,bodytt(c,body),t_e]);
    case {classic}: lt([t_ob,fnargstt(c,args),t_sp,t_rar,t_sp,bodytt(c,body),t_cb]);
    }
  }
}

function enot_empty(exp exp) { exp != {empty} }

function eseqtt(context c, list(exp) exps, start, finish) {
  match (exps) {
  case []: lt([start,finish]);
  case [exp]: lt([start,exptt(c,exp),finish]);
  default: lt([start,tc2(c,t_com,List.map(exptt(c,_),List.filter(enot_empty,exps))),finish]);
  }
}

elsttt = eseqtt(_, _, t_os, t_cs)
eargstt = eseqtt(_, _, t_ob, t_cb)

function etuptt(context c, list(exp) exps) {
  match (exps) {
  case []: lt([t_ob,t_cb]);
  case [exp]: exptt(c,exp);
  default: lt([t_ob,tc2(c,t_com,List.map(exptt(c,_),List.filter(enot_empty,exps))),t_cb]);
  }
}

function fnapptt(context c, exp fn, list(exp) args) {
  lt([exptt(c,fn),eargstt(c,args)])
}

function typedexptt(context c, exp exp, ctyp typ) {
  match (c.style) {
  case {js_like}: lt([ctyptt(ie(c),typ),t_sp,exptt(c,exp)]);
  case {classic}: lt([exptt(c,exp),t_col,ctyptt(ie(c),typ)]);
  }
}

function exptt(context c, exp exp) {
  match (exp) { 
  case {empty}: t_empty;
  case {wild}: t_us;
  case ~{id}: idtt(c,id);
  case ~{rcrd}: erecordtt(c,false,none,rcrd);
  case {tilde, ~rcrd}: erecordtt(c,true,none,rcrd);
  case ~{wth, rcrd}: erecordtt(c,true,{some:wth},rcrd);
  case ~{tup}: etuptt(c,tup);
  case ~{lst}: elsttt(c,lst);
  case ~{mtch}: matchtt(c,mtch);
  case ~{ite}: itett(c,ite);
  case ~{ilfunc}: ilfunctt(c,ilfunc);
  case ~{fn, args}: fnapptt(c,fn,args);
  case ~{doexp}: doexptt(c,doexp);
  case ~{block}: blocktt(c,block);
  case ~{exp, typ}: typedexptt(c,exp,typ);
  case ~{verbatim}: styledtt(c,verbatim);
  }
}

function stmnttt(context c, stmnt stmnt) {
  match (stmnt) {
  case {empty}: t_empty;
  case {blank}: t_empty;
  case ~{cmnt}: cmnttt(ie(c),cmnt);
  case ~{cmnts}: cmntstt(ie(c),c,cmnts);
  case ~{doc}: doctt(ie(c),doc);
  case ~{docs}: docstt(ie(c),c,docs);
  case ~{verbatim}: styledtt(c,verbatim);
  case ~{id}: idtt(c,id);
  case ~{pat, exp}: lt([pattt(c,pat),t_sp,t_eq,t_sp,exptt(ii(c),exp)]);
  case ~{exp}: exptt(c, exp);
  case ~{func}: functt(c,func);
  case ~{dec}: lt([dectt(ii(c),dec)]);
  case ~{block}: blocktt(c,block);
  }
}

function cmnttt(context c, cmnt cmnt) {
  lt([c.indent,t_ss,t_sp,tx(cmnt)])
}

function cmntstt(context c1, context c, cmnts cmnts) {
  lt([c1.indent,t_cms,sc(lt([t_cr,c.indent]),cmnts),t_cr,c.indent,t_cde])
}

function doctt(context c, doc doc) {
  lt([c.indent,t_ds,t_sp,tx(doc),t_sp,t_cde])
}

function docstt(context c1, context c, docs docs) {
  lt([c1.indent,t_ds,t_sp,sc(lt([t_cr,c.indent,t_sp,t_st,t_sp]),docs),t_cr,c.indent,t_sp,t_cde])
}

function snot_empty(stmnt stmnt) { stmnt != {empty} }

function blocktt(context c, block block) {
  lt([t_cr,c.indent,tc(lt([t_cr,c.indent]),List.map(stmnttt(c,_),List.filter(snot_empty,block)))])
}

function bodytt(context c,block block) {
  lt([tc(styled(c,lt([t_sem,t_sp]),t_sp),List.map(stmnttt(is(c,1),_),List.filter(snot_empty,block)))])
}

function fnargstt(context c,fnargs args) {
  lt([t_ob,tc(t_com,List.map(pattt(c,_),args)),t_cb]);
}

function modargstt(context c,fnargs args) {
  match (args) {
  case []: t_empty;
  case [pat]: lt([t_ob,pattt(c,pat),t_cb]);
  default: lt([t_ob,tc(t_com,List.map(pattt(c,_),args)),t_cb]);
  }
}

function rttt(context c,ctyp return_type) {
  if (return_type == {empty})
    t_empty
  else
    match (c.style) {
    case {js_like}: lt([ctyptt(ie(c),return_type),t_sp]);
    case {classic}: lt([t_sp,t_col,t_sp,ctyptt(ie(c),return_type)]);
    }
}

function typdeftt(context c,typdef typdef) {
  match (typdef) {
  case ~{name, typ}:
    match (c.style) {
    case {js_like}: lt([c.indent,t_type,t_sp,tx(name),t_sp,t_eq,t_sp,ctyptt(c,typ)]);
    case {classic}: lt([c.indent,t_type,t_sp,tx(name),t_sp,t_eq,t_sp,ctyptt(c,typ)]);
    }
  }
}

function functt(context c,modfunc func) {
  match (func) {
  case ~{name, args, return_type, body}:
    match (c.style) {
    case {js_like}: lt([t_function,t_sp,rttt(c,return_type),tx(name),fnargstt(c,args),t_sp,t_b,t_cr,
                        blocktt(ii(c),body),
                        c.indent,t_e,t_cr]);
    case {classic}: lt([tx(name),fnargstt(c,args),rttt(c,return_type),t_sp,t_eq,t_sp,
                        blocktt(ii(c),body)]);
    }
  }
}

function modtt(context c, modfunc mod) {
  match (mod) {
  case ~{name, args, return_type:_, body}:
    match (c.style) {
    case {js_like}: lt([c.indent,t_module,t_sp,tx(name),modargstt(c,args),t_sp,t_b,t_cr,
                        blocktt(ii(c),body),
                        t_cr,t_cr,c.indent,t_e,t_cr]);
    case {classic}: lt([c.indent,tx(name),modargstt(c,args),t_sp,t_eq,t_sp,t_bb,t_cr,
                        blocktt(ii(c),body),
                        t_cr,t_cr,c.indent,t_ee,t_cr]);
    }
  }
}

function dectt(context c, dec dec) {
  match (dec) {
  case {blank}: t_empty;
  case ~{cmnt}: cmnttt(ie(c),cmnt);
  case ~{cmnts}: cmntstt(ie(c),c,cmnts);
  case ~{doc}: doctt(c,doc);
  case ~{docs}: docstt(c,c,docs);
  case ~{mod}: modtt(c,mod);
  case ~{typdef}: typdeftt(c,typdef);
  case ~{func}: functt(c,func);
  case ~{exp}: exptt(c,exp);
  case ~{stmnt}: stmnttt(c,stmnt);
  case ~{verbatim}: styledtt(c,verbatim);
  }
}

function opatt(context c, opa opa) { tc(t_cr,List.map(dectt(c,_),opa)) }

/* Construction of the code type */

// Spread code out with blank lines (not recursively)
function list('a) space(list('a) l,'a blnk) {
  recursive function aux(l) {
    match (l) {
    case []: [blnk];
    case [e]: [e,blnk];
    case [e|l]: [e,blnk|aux(l)];
    }
  }
  aux(l)
}
dspace = (opa -> opa) space(_,dblnk)
sspace = (block -> block) space(_,sblnk)

ctyp empty_t = {empty}
ctyp string_t = {tyname:"string"}
ctyp int_t = {tyname:"int"}
ctyp bool_t = {tyname:"bool"}
ctyp float_t = {tyname:"float"}

exp eempty = {empty}
function exp eid(id) { {id:~{id}} }
function exp etid(id, typ) { {id:~{id, typ}} }
function exp edo(doexp) { ~{doexp} }
function exp eblk(block) { ~{block} }
function exp typedexp(exp, typ) { ~{exp, typ} }
function exp emtch(exp, pes) { {mtch:~{exp, pes}} }
function exp eite(expi, expt, expe) { {ite:(expi,expt,expe)} }
function exp efn(args, body) { {ilfunc:~{args, body}} }
function exp efa(fn, args) { ~{fn, args} }
function exp evs(js,cl) { {verbatim:~{js,cl}} }
function exp evb(s) { {verbatim:{bth:s}} }
function exp etup(tup) { ~{tup} }
function exp elst(lst) { ~{lst} }
function exp erec(rcrd) { ~{rcrd} }
function exp etrec(rcrd) { {tilde, ~rcrd} }
function exp ewrec(wth, rcrd) { ~{wth, rcrd} }
function erecel epl(name) { (name,{plain}) }
function erecel etl(name) { (name,{tilde}) }
function erecel eex(name,exp) { (name,{~exp}) }
function exp erecsimple(name) { erec([epl(name)]) }
function exp erectsimple(name) { erec([etl(name)]) }
function exp erecs(list((string,string)) flds) {
  erec(List.map(function ((n,v)) {
                  match (v) {
                  case "": epl(n);
                  case "~": etl(n);
                  default: eex(n,evb(v));
                  }
                },flds))
}
type ervdef = {l} or {t} or {string v} or {exp e}
function exp erecr(list((string,ervdef)) flds) {
  erec(List.map(function ((n,v)) {
                  match (v) {
                  case {l}: epl(n);
                  case {t}: etl(n);
                  case ~{v}: eex(n,evb(v));
                  case ~{e}: eex(n,e);
                  }
                },flds))
}
exp etrue = evb("true")
exp efalse = evb("false")
function exp esome(exp) { erec([("some",~{exp})]) }
function exp esuccess(exp) { erec([("success",~{exp})]) }
function exp efailure(exp) { erec([("failure",~{exp})]) }
exp efailuret = erec([etl("failure")]) // {~failure}
exp enone = evb("none")
exp evoid = evb("\{}")
exp ewild = evb("_")
function estr(string s) { evb("\"{s}\"") }
function eint(int i) { evb(Int.to_string(i)) }
function ebool(bool b) { evb(Bool.to_string(b)) }

function pat pid(id) { {id:~{id}} }
function pat ptid(id, typ) { {id:~{id, typ}} }
function pat pvb(s) { {verbatim:{bth:s}} }
function pat ptup(tup) { ~{tup} }
function pat plst(lst) { ~{lst} }
function pat prec(rcrd) { ~{rcrd} }
function pat ptrec(rcrd) { {tilde, ~rcrd} }
function precel ppl(name) { (name,{plain}) } // {name}
function precel ptl(name) { (name,{tilde}) } // {~name}
function precel pwl(name) { (name,{wild}) } // {name:_}
function precel pex(name,pat) { (name,{~pat}) } // {name:pat}
function pat precsimple(name) { prec([ppl(name)]) }
function pat prectsimple(name) { prec([ptl(name)]) }
function pat precs(list((string,string)) flds) {
  prec(List.map(function ((n,v)) {
                  match (v) {
                  case "": ppl(n);
                  case "~": ptl(n);
                  default: pex(n,pvb(v));
                  }
                },flds))
}
type prvdef = {l} or {t} or {string v} or {pat p}
function pat precr(list((string,prvdef)) flds) {
  prec(List.map(function ((n,v)) {
                  match (v) {
                  case {l}: ppl(n);
                  case {t}: ptl(n);
                  case ~{v}: pex(n,pvb(v));
                  case ~{p}: pex(n,p);
                  }
                },flds))
}
pat ptrue = prec([ppl("true")])
pat pfalse = prec([ppl("false")])
pat pnone = prec([ppl("none")])
function pat psome(pat) { prec([("some",~{pat})]) }
function pat psuccess(pat) { prec([("success",~{pat})]) } // {success:pat}
pat psuccesst = prec([ptl("success")]) // {~success}
pat psuccessw = prec([pwl("success")]) // {success:_}
function pat pfailure(pat) { prec([("failure",~{pat})]) } // {failure:pat}
pat pfailuret = prec([ptl("failure")]) // {~failure}
pat pfailurew = prec([pwl("failure")]) // {failure:_}
pat pvoid = pvb("\{}")
pat pwild = pvb("_")

function pe succ2succ(pat p,exp e) { (psuccess(p),esuccess(e)) }
function pe succ2fail(pat p,exp e) { (psuccess(p),efailure(e)) }
function pe succw2fail(exp e) { (psuccessw,efailure(e)) }
function pe fail2fail(pat p,exp e) { (pfailure(p),efailure(e)) }
pe failt2failt = (pfailuret,efailuret)

function ctyp tyname(tyname) { ~{tyname} }
function ctyp typargs(tyname, args) { ~{tyname, args} }
function ctyp typopt(arg) { ~{tyname:"option", args:[arg]} }
function ctyp ctyp(cols) { ~{cols} }

stmnt sempty = {empty}
stmnt sblnk = {blank}
function stmnt scmnt(cmnt) { ~{cmnt} }
function stmnt scmnts(cmnts) { ~{cmnts} }
function stmnt sdoc(doc) { ~{doc} }
function stmnt sdocs(docs) { ~{docs} }
function stmnt asgn(pat, exp) { ~{pat, exp} }
function stmnt sexp(exp) { ~{exp} }
function stmnt smtch(exp, pes) { sexp(emtch(exp,pes)) }
function stmnt sfn(name, args, return_type, body) { {func:~{name, args, return_type, body}} }
function stmnt sid(string id) { {id:~{id}} }
function stmnt svs(js,cl) { {verbatim:~{js,cl}} }
function stmnt svb(bth) { {verbatim:~{bth}} }
function stmnt sblock(block) { ~{block} }
stmnt spvt = svs("private","@private")

dec dblnk = {blank}
function dec dvs(js,cl) { {verbatim:~{js,cl}} }
function dec dvb(s) { {verbatim:{bth:s}} }
function dec dcmnt(cmnt) { ~{cmnt} }
function dec dcmnts(cmnts) { ~{cmnts} }
function dec ddoc(doc) { ~{doc} }
function dec ddocs(docs) { ~{docs} }
function dec typdef(name, typ) { {typdef:~{name, typ}} }
function dec dfn(name, args, return_type, body) { {func:~{name, args, return_type, body}} }
function dec dmod(name, args, body) { {mod:~{name, args, return_type:empty_t, body}} }
function dec dexp(exp) { ~{exp} }
function dec dstmnt(stmnt) { ~{stmnt} }
dec dpvt = dvs("private","@private")

