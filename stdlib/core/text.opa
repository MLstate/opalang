/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
    @authors Matthieu Guffroy, Frédéric Ye, Corentin Gallet
*/

/*
        TODO Critical for speed : integrate Utf-8 length in Text node, remove all possible call to Text_node.length
             Currently : concat and insert have inacceptable worst case complexity
*/


/*
** Text.
** The `camomile part' is desactivated. If you want Camomile back, uncomment
** it.
** Tip : C-s camo
*/

/**
 * {1 Iterators}
 *
**/

/**
 * Iterator.
 *
 * <!> To serialize this value, we need to traduce the position,
 * because on the server, [pos] is the index in bytes, where in Javascript,
 * it is the character position (native support of utf8)
 * This is implemented in [opaserialize]
**/
@opacapi
@abstract type itextrator = {
  txt : string ;
  pos : int ;
}

Itextrator = {{

/**
 * Test if the 2 iterators are the same, at the same position.
**/
equals(it1:itextrator, it2:itextrator) =
  (it1.pos == it2.pos) && (it1.txt == it2.txt)

/**
 * Give the number of characters eaten by the proud iterator.
**/
pos(it:itextrator) =
  it.pos

/**
 * Returns the complete txt used by the iterators
 * <!> Not for casual user
**/
txt(it:itextrator) = it.txt

/**
 * Return the next itextrator
 * FIXME: refactor 
 * FIXME: used as wild insertion by [surfaceAstTrx], without opacapi
**/
next(it:itextrator) : option((itextrator, Unicode.character)) =
  txt = it.txt
  pos = it.pos
  if pos >= String.length(txt) then
    none
  else
    c = Cactutf.look(txt, pos)
    new_pos = Cactutf.next(txt, pos)
    new_it = {
      txt = txt ;
      pos = new_pos ;
    }
    some((new_it, c))

/**
 * Build a new itextrator from a string
**/
make(txt:string) : itextrator =
  pos = 0
  ~{ pos txt }

/**
 * Give the length of the text inside the iterator.
 * FIXME: slow
**/
length(it:itextrator) =
  Cactutf.length(it.txt)

/**
 * WARNING, private for TRX and serialization
 * Not for casual users
**/
forward(it : itextrator, bytes : int) : itextrator =
  txt = it.txt
  pos = it.pos + bytes
  ~{ pos txt }
}}


/**
 * {1 Texts}
**/

/**
 * Represent a text (an ordered collection of strings) as a `rope'.
 * The left part is a RA list of AVLs.
 * The right part is a reversed RA list of AVLs.
**/
@opacapi
@abstract type text =
   { textralist1 : list(textavl) ; textralist2 : list(textavl) }
 / { string : string }

@abstract type utf8cacttext = {textralist1:utf8cacttextralist; textralist2:utf8cacttextralist} / {string:string}

Text =
Textralist = Utf8cact_textralist
param_smerge = 50
{{

/*
** Textavl : can call directly the AVL module instead of using Textralist to do so.
*/

/**
 * Do a concatenation of two Texts.
**/
concat(t1, t2) =
  match t1 : text with
  | {string = string1} ->
      (match t2:text with
       | {string = string2} ->
           /* TODO , should use approximate length here */
           if (Textavl.node_length(string1) + Textavl.node_length(string2) > param_smerge) then
              {textralist1 = [Textavl.singleton(string1)]:textralist;
               textralist2 = [Textavl.singleton(string2)]:textralist}:text
           else
              {textralist1 = [Textavl.singleton(string1 ^ string2)]:textralist;
               textralist2 = []:textralist}:text
       | {textralist1 = l1; textralist2 = l2} ->
           {textralist1 = Textralist.insert_str_to_the_left(l1, string1, param_smerge):textralist;
            textralist2 = l2:textralist}:text
      )
  | {textralist1 = listA; textralist2 = listB} ->
      match t2 : text with
      | {string = string3} ->
          {textralist1 = listA;
           textralist2 = Textralist.insert_str_to_the_left_rev(listB, string3, param_smerge):textralist}:text
      | {textralist1 = listC; textralist2 = listD} ->
          sizeA = Textralist.length(listA);
          sizeB = Textralist.length(listB);
          sizeC = Textralist.length(listC);
          sizeD = Textralist.length(listD);
          cas1diff = Int.abs((sizeA + sizeB) - (sizeC + sizeD));
          cas2diff = Int.abs((sizeA + sizeB + sizeC) - (sizeD));
          cas3diff = Int.abs((sizeA) - (sizeB + sizeC + sizeD));
          if ((cas1diff <= cas2diff) && (cas1diff <= cas3diff)) then
             {textralist1 = Textralist.raregroup(List.append(listA, Textralist.reverse(listB)));
              textralist2 = Textralist.raregroup_rev(List.append(listD, Textralist.reverse(listC)))}:text
          else if ((cas2diff <= cas1diff) && (cas2diff <= cas3diff)) then
             {textralist1 = Textralist.raregroup(List.append(listA, List.append(Textralist.reverse(listB), listC)));
              textralist2 = listD}:text
          else
             {textralist1 = listA;
              textralist2 = Textralist.raregroup_rev(List.append(listD, List.append(Textralist.reverse(listC), listB)))}:text

/**
 * Build a little Text from a string "hello".
**/
cons(s) =
  {string = s}:text

/**
 * Return the number of characters in the Text.
**/
length(t) =
  match t:text with
  | {~string} ->
      Textavl.node_length(string)
  | {textralist1 = l1; textralist2 = l2} ->
      Textralist.length(l1) + Textralist.length(l2)

/**
 * Return all the strings inside the Text.
**/
@stringifier(text) to_string(txt) =
  rec aux(f, l) =
    match l with
    | [] -> [] : list(string)
    | [hd | tl] -> List.append(f(hd), aux(f, tl))
  match txt : text with
  | {~string} ->
      string
  | {textralist1 = l1; textralist2 = l2} ->
      src= List.append(aux(Textavl.to_list, l1), aux(Textavl.to_list, List.rev(l2)))
      String.flatten(src)

/**
 * Insert a standard string "hello" at the left (beginning) of the text.
**/
insert_left(txt, str) =
  concat(cons(str), txt)

/**
 * Insert a standard string "hello" at the right (ending) of the text.
**/
insert_right(txt, str) =
  concat(txt, cons(str))

/**
 * Return a sub-text of the text.
 * If the range is invalid, the range is corrected to nearest valid range
 * Complexity for invalid range is linear.
**/
sub(txt, start, len) =
 match txt:text with
     | {~string} -> {string = Textavl.node_sub(string, start, len)}:text
     | {textralist1 = l1; textralist2 = l2} ->
          lenleft = Textralist.length(l1)
          lenright = Textralist.length(l2)
          length=lenleft+lenright
          start=min(max(0, start),length)
          len=min(max(0,len),length-start+1)
            res=if (lenleft < start) then
                 {textralist1 = [] : textralist;
                  textralist2 = Textralist.raregroup_rev(Textralist.sub_rev(l2, lenright + lenleft - start - len, len))}:text
            else if (start + len < lenleft) then
                 {textralist1 = Textralist.raregroup(Textralist.sub(l1, start, len));
                  textralist2 = [] : textralist}:text
            else
                 {textralist1 = Textralist.raregroup(Textralist.sub(l1, start, lenleft - start));
                  textralist2 = Textralist.raregroup_rev(Textralist.sub_rev(l2, lenright + lenleft - start - len, start + len - lenleft))}:text
            res


/**
 * Return an optional sub-text of the text.
 *
 *  TODO : propagate error as option to remove the very slow front check
**/
sub_opt(txt, start, len) =
  match txt:text with
     | {~string} ->
          match Textavl.node_sub_opt(string, start, len):option
                {some=s}->some({string = s}:text)
                _      ->none
          end
     | {textralist1 = l1; textralist2 = l2} ->
          lenleft = Textralist.length(l1)
          lenright = Textralist.length(l2)
          length=lenleft+lenright
          if (start < 0) || (len < 0) || (length < start + len) then none
          else
            res=if (lenleft < start) then
                 {textralist1 = [] : textralist;
                  textralist2 = Textralist.raregroup_rev(Textralist.sub_rev(l2, lenright + lenleft - start - len, len))}:text
            else if (start + len < lenleft) then
                 {textralist1 = Textralist.raregroup(Textralist.sub(l1, start, len));
                  textralist2 = [] : textralist}:text
            else
                 {textralist1 = Textralist.raregroup(Textralist.sub(l1, start, lenleft - start));
                  textralist2 = Textralist.raregroup_rev(Textralist.sub_rev(l2, lenright + lenleft - start - len, start + len - lenleft))}:text
           some(res)

/**
 * Returns a part of a given text cut from a given position till the end
 *
 * @param txt a text to be manipulated
 * @param start starting position of the cut (0-based)
 * @return a text containing the content of [txt] from [start] position
 * till the end
 */
from(txt, start) =
  sub(txt, start, length(txt) - start)

/**
 * insert()
 * Insert the second text into the first at the given position.
**/
insert(txt1, txt2, pos) =
  lentxt1 = length(txt1);
  if ((pos > lentxt1) || (pos < 0)) then
     {string = "[Argument error in Text.insert]"}:text
  else if (pos == 0) then
     concat(txt2, txt1)
  else if (pos == lentxt1) then
     concat(txt1, txt2)
  else
      match (txt1:text, txt2:text) with
      | ({string = s1}, {string = s2}) ->                                               /* Case 1 : string < string.*/
          {textralist1 = [Textavl.split_insert(s1, s2, pos, lentxt1)]:textralist;
           textralist2 = [] : textralist}:text
      | ({string = s1}, {textralist1 = r1; textralist2 = r2}) ->                        /* Case 2 : string < list.*/
           {textralist1 = Textralist.insert_str_to_the_left(r1,
                                                            Textavl.stringsub(s1, 0, pos),
                                                            param_smerge):textralist;
            textralist2 = Textralist.insert_str_to_the_left_rev(r2,
                                                            Textavl.stringsub(s1, pos, Textavl.node_length(s1) - pos),
                                                            param_smerge):textralist}:text
      | ({textralist1 = r1; textralist2 = r2}, {string = s2}) ->                        /* Case 3 : list < string.*/
           lenr1 = Textralist.length(r1);
           if (pos <= lenr1) then
              {textralist1 = Textralist.insert_aux_str_into_avls(r1, s2, pos):textralist;
               textralist2 = r2}:text
           else
              {textralist1 = r1;
               textralist2 = Textralist.insert_aux_str_into_avls_rev(r2,
                                                                     s2,
                                                                     Textralist.length(r2) - pos + lenr1):textralist}:text
      | ({textralist1 = r1; textralist2 = r2}, {textralist1 = r3; textralist2 = r4}) -> /* Case 4 list < list.*/
           lenr1 = Textralist.length(r1);
           if (pos <= lenr1) then
              {textralist1 = Textralist.raregroup(Textralist.insert_aux_avls_into_avls(r1,
                                                                                       List.append(r3, Textralist.reverse(r4)),
                                                                                       pos)):textralist;
               textralist2 = r2}:text
           else
              {textralist1 = r1;
               textralist2 = Textralist.raregroup_rev(Textralist.insert_aux_avls_into_avls_rev(r2,
                                                                                           List.append(r3, Textralist.reverse(r4)),
                                                                                           Textralist.length(r2) - pos + lenr1)):textralist}:text

/**
 * Compare two texts.
 * Needed by MagicCompare.
 *
 * FIXME:
 * historically, naive implementation., using itstat flatten + 'next' utilisation
 * Since the itstart flatten the text, there is no efficient itextrator for text.
 *
 * Now: since we use the flattening, better is to make that point explicit in the code.
 * which uses a string compare after a potentially huge string allocation
**/
compare(t1:text, t2:text) =
  if t1 === t2 then {eq}
  else
    s1 = to_string(t1)
    s2 = to_string(t2)
    String.compare(s1, s2)

/**
 * Return an uppercased Text.
**/
uppercase(txt) =
  match txt:text with
  | {~string} ->
      {string = Textavl.node_uppercase(string)}:text
  | {textralist1 = r1; textralist2 = r2} ->
      {textralist1 = Textralist.uppercase(r1); textralist2 = Textralist.uppercase(r2)}:text

/**
 * Return a lowercased Text.
**/
lowercase(txt) =
  match txt:text with
  | {~string} ->
      {string = Textavl.node_lowercase(string)}:text
  | {textralist1 = r1; textralist2 = r2} ->
      {textralist1 = Textralist.lowercase(r1); textralist2 = Textralist.lowercase(r2)}:text

/**
 * Return the character at the position `pos'.
**/
get(txt, pos) =
  match txt:text with
  | {~string} ->
      Textavl.node_get(string, pos):Unicode.character
  | {textralist1 = r1; textralist2 = r2} ->
      lenr1 = Textralist.length(r1);
      if (pos <= lenr1) then
         Textralist.get(r1, pos):Unicode.character
      else
         Textralist.get(r2, Textralist.length(r2) - pos + lenr1):Unicode.character

/**
 * Build a text from a(n) Unicode number.
**/
from_character(c:Unicode.character) =
  Textavl = Textralist.Internal_module
  {string = Textavl.node_cons(c)}:text


/**
 * {2 Iterators section}
 *
 * Beware, Mortal.
 * Here stands the Dreadful Land of the Mighty `Itextrators'.
 * Thou should be careful.
**/

/**
 * Build a new iterator.
 * Note: historically, Itextrator was using text for their internal implementation.
 * Now, it uses a to_string, and build the Itextrator from the string.
 * FIXME: potentially sloow if the text is not built from a string.
 * use rather [Itextrator.make(string)]
**/
itstart(t : text) : itextrator =
  ts = to_string(t)
  Itextrator.make(ts)

/**
 * Return the subtext between 2 iterators.
**/
itsub(it1 : itextrator, it2 : itextrator) : text =
  (it1, it2) = if it1.pos < it2.pos then (it1, it2) else (it2, it1)
  cons(String.substring(it1.pos, it2.pos - it1.pos, it1.txt))

/**
 * {2 For parsers}
 *
 * Lists of Text (useful when working with parsers
**/

/**
 * Merge all the characters from the list into a single text.
**/
lcconcat(l) =
  rec aux(li, accu) =
    match (li : list) with
    | [] -> accu
    | [hd | tl] -> aux(tl, concat(accu, from_character(hd)))
  aux(l, cons(""))

/**
 * Merge some texts into a single one.
**/
ltconcat(l) =
  rec aux(li, accu) =
    match (li : list) with
    | [] -> accu
    | [hd | tl] -> aux(tl, concat(accu, hd))
  aux(l, cons(""))

/*----------------------
| Internationalization |
----------------------*/
/*
** whereareyoufrom()
** Return a string of the language of the character.
*/
/*
    Deleted, because of the Unicode classes.
*/

/**
 * {2 Information, debug}
**/

/**
 * Study the Text.
 * For debug only.
**/
check_text(t) =
  match t:text with
  | {~string} ->
      "String of {Textavl.node_length(string):int}"
  | {textralist1 = l1; textralist2 = l2} ->
      Textralist.check_text(l1, 0) ^ " ||| " ^ Textralist.check_text(l2, 0)

/**
 * Return a string of a Dot representation of the Text.
**/
dot_graph(txt) =
  "digraph MyPrettyTextRAList \{\n" ^
  match txt:text with
  | {~string} ->
      "\"Root:{string}\" [shape=box];\n}\n"
  | {textralist1 = r1; textralist2 = r2} ->
      "Root [shape=box];\n"
      ^ "left [shape=egg];\n"
      ^ "right [shape=egg];\n"
      ^ "Root -> left;\n"
      ^ "Root -> right;\n"
      ^ Textralist.dot_graph(r1, 1, "left", "octagon")
      ^ Textralist.dot_graph(r2, 1, "right", "house")
      ^ "}\n"

/**
 * Return a string with a description of the module.
**/
version() =
   Textavl = Textralist.Internal_module
  "Module `text', inner functions working with "
  ^ Textavl.textavl_description
  ^ ", minimum merge {param_smerge}."


}}

/*---------------------------------------------------------------------------*/

/*-----*\
| ASCII |
\*-----*/

/*
** Ascii_text : old style text.
** No support of accents or foreign characters.
** Fast and take little space.
** Can be used for english websites.
*/
/*Ascii_text = Make_text(Ascii_textralist, 50);*/

/*---------------------------------------------------------------------------*/

/*----*\
| UTF8 |
\*----*/

/*
** Utf8_text : Unicode text support.
** International, but slower and take more memory.
** Utf8camo_text : Camomile (Ocaml library, have to be linked, 30 Mo).
** Utf8euca_text : Eucalyptutf (OPA file. Slow, but no bypasses at all).
** Utf8cact_text : Cactutf (Ocaml file + cmx, as fast as Camomille).
*/
/*Utf8camo_text = Make_text(Utf8camo_textralist, 50);
Utf8euca_text = Make_text(Utf8euca_textralist, 50);
Utf8cact_text = Make_text(Utf8cact_textralist, 50);*/

/*---------------------------------------------------------------------------*/

/*--------*\
| STANDARD |
\*--------*/

/*
** Text : standard text module for OPA.
** Pick the one you want.
*/
//Text = Utf8cact_text;

textToString              = Text.to_string
