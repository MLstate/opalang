/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
    @authors Matthieu Guffroy, Frédéric Ye, Corentin Gallet
**/

/**
 * {1 About this module}
 *
 * RA lists of AVLs.
 * `RA' stands for "Random Access".
 *
 * {1 Where should I start?}
 *
 * The lists are ascending, but only one element for two ; the second is always a single leaf-tree.
 * So 1-1-1-2-1-3-1-7 is okay.
 *
 * They can't be two elements h>1, they have to be merged.
 * 1-1-4-1-4 -> 1-1-5
 *
 * And no h=1 element at the end of a list with elements h>1 :
 * 1-2-1-3-6-1 is not okay, since the last tree will never be merged.
 *
 * 4 h=1 elements must be merged :
 * 1-1-1-1 -> 1-2.
 *
 * Beware : some RA lists are reversed here.
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

type textralist = list(textavl)

/* disabled :
type Textralist('a,'b,'c,'d) =
{{
  Internal_module : Textavl('a,'b,'c,'d)
  check_text : textralist -> int -> string
  dot_graph : textralist -> int -> string -> string -> string
  empty : textralist
  get : textralist -> int -> Unicode.character
  head : textralist -> textavl
  insert_aux_avls_into_avls : textralist -> textralist -> int -> textralist
  insert_aux_avls_into_avls_rev : textralist -> textralist -> int -> textralist
  insert_aux_str_into_avls : textralist -> string -> int -> textralist
  insert_aux_str_into_avls_rev : textralist -> string -> int -> textralist
  insert_str_to_the_left : textralist -> string -> int -> textralist
  insert_str_to_the_left_rev : textralist -> string -> int -> textralist
  length : textralist -> int
  lowercase : textralist -> textralist
  next : textralist -> textralist
  raconcat : textavl -> textralist -> textavl
  raregroup : textralist -> textralist
  raregroup_rev : textralist -> textralist
  reverse : textralist -> textralist
  rstart : textralist -> (Unicode.character, string, textralist, list(textavl))
  start : textralist -> (Unicode.character, string, textralist, list(textavl))
  sub : textralist -> int -> int -> textralist
  sub_rev : textralist -> int -> int -> textralist
  to_list : textralist -> list(string)
  to_string : textralist -> string
  uppercase : textralist -> textralist
  //mapc : textralist -> (Unicode.character -> Unicode.character) -> textralist
  //next_node : seq('data) -> seq('data)
}}
*/

/*type asciitextralist = textralist
type utf8camotextralist = textralist(utf8camotextavl)
type utf8eucatextralist = textralist*/
type utf8cacttextralist = textralist

/**
 * {1 Interface}
 */

Make_textralist/*(Textavl:Textavl)*/ =
//Textavl = Utf8cact_textavl
{{

/*
** Internal_module : Allow the module Text to directly use Textavl.
*/
Internal_module = Textavl;

/*
** empty
** Build an empty list.
*/
empty = [] : textralist

/*
** raregroup()
** Return a valid RA-list.
*/
raregroup(lavl) =
  rec raregroup_aux(lavl, n) =
    match lavl : textralist with
    | [] ->
        [] : textralist
    | [hd1 | tl1] ->
        (hei1 = Textavl.sizetr(hd1);
         match tl1 : textralist with
         | [] ->
             lavl : textralist
         | [hd2 | tl2] ->
             (hei2 = Textavl.sizetr(hd2);
              match tl2:textralist with
              | [] ->
                  if ((hei1 > 1) && (hei2 > 1)) then
                     [Textavl.merge_avl_nn(hd1, hd2)]:textralist
                     /* Case 1 : two trees. Merge the two.*/
                  else if ((hei2 == 1) && (n >= 3)) then
                     [Textavl.merge_avl_nn(hd1, hd2)]:textralist
                     /* Case 2 : ending h=1 element. Merging.*/
                  else
                     lavl
                     /* Case 3 : okay.*/
              | [ hd3 | tl3] ->
                  hei3 = Textavl.sizetr(hd3);
                  if ((hei1 > 1) && (hei2 == 1) && (hei3 > hei1)) then
                     List.cons(hd1, raregroup_aux(tl1, 10)):textralist
                     /* Case 4 : tree. Okay.*/
                  else if ((hei1 == 1) && (hei2 > 1) && (hei3 == 1)) then
                     List.cons(hd1, raregroup_aux(tl1, n + 1)):textralist
                     /* Case 5 : h=1 element. Okay.*/
                  else if ((hei1 == 1) && (hei2 == 1) && (n == 1)) then
                     List.cons(hd1, raregroup_aux(tl1, n + 1)):textralist
                     /* Case 6 : First h=1 elements. Okay.*/
                  else if ((hei1 > 1) && (hei2 == 1) && (hei3 == hei1)) then
                     raregroup_aux(List.cons(Textavl.create_node(hd1, Textavl.value(hd2), hd3), tl3):textralist, 10)
                     /* Case 7 : merging two trees of the same height.*/
                  else if ((hei1 == 1) && (hei2 == 1) && (hei3 == 1) && (n == 2)) then
                     raregroup_aux(List.cons(Textavl.create_node(hd1, Textavl.value(hd2), hd3), tl3):textralist, 10)
                     /* Case 8 : Merging the h=1 elements of the start.*/
                  else if ((hei1 > 1) && (hei2 == 1)) then
                     raregroup_aux(List.cons(Textavl.merge_avl_nn(Textavl.merge_avl_nn(hd1, hd2), hd3), tl3):textralist, 10)
                     /* Case 9 : Merging the 3 elements.*/
                  else if (hei1 > 1) then
                     raregroup_aux(List.cons(Textavl.merge_avl_nn(hd1, hd2), tl2):textralist, 10)
                     /* Case 10 : Merging the first and the second elements.*/
                  else
                     List.cons(hd1, raregroup_aux(tl1, n + 1)):textralist
                     /* Case 11 : one more step forward.*/
             )
        )
  raregroup_aux(lavl, 1)

/*
** raregroup_rev()
** Reversed version of raregroup().
*/
raregroup_rev(lavl) =
  rec raregroup_aux_rev(lavl, n) =
    match lavl:textralist with
    | [] -> [] : textralist
    | [hd1 | tl1] ->
        (hei1 = Textavl.sizetr(hd1);
        match tl1:textralist with
        | [] -> lavl : textralist
        | [hd2 | tl2] ->
             (hei2 = Textavl.sizetr(hd2);
              match tl2:textralist with
              | [] ->
                  if ((hei1 > 1) && (hei2 > 1)) then
                     [Textavl.merge_avl_nn(hd2, hd1)]:textralist
                     /* Case 1 : two trees. Merge the two.*/
                  else if ((hei2 == 1) && (n >= 3)) then
                     [Textavl.merge_avl_nn(hd2, hd1)]:textralist
                     /* Case 2 : ending h=1 element. Merging.*/
                  else
                     lavl
                     /* Case 3 : okay.*/
              | [hd3 | tl3] ->
                  hei3 = Textavl.sizetr(hd3);
                  if ((hei1 > 1) && (hei2 == 1) && (hei3 > hei1)) then
                     List.cons(hd1, raregroup_aux_rev(tl1, 10)):textralist
                     /* Case 4 : tree. Okay.*/
                  else if ((hei1 == 1) && (hei2 > 1) && (hei3 == 1)) then
                     List.cons(hd1, raregroup_aux_rev(tl1, n + 1)):textralist
                     /* Case 5 : h=1 element. Okay.*/
                  else if ((hei1 == 1) && (hei2 == 1) && (n == 1)) then
                     List.cons(hd1, raregroup_aux_rev(tl1, n + 1)):textralist
                     /* Case 6 : First h=1 elements. Okay.*/
                  else if ((hei1 > 1) && (hei2 == 1) && (hei3 == hei1)) then
                     raregroup_aux_rev(List.cons(Textavl.create_node(hd3, Textavl.value(hd2), hd1), tl3):textralist, 10)
                     /* Case 7 : merging two trees of the same height.*/
                  else if ((hei1 == 1) && (hei2 == 1) && (hei3 == 1) && (n == 2)) then
                     raregroup_aux_rev(List.cons(Textavl.create_node(hd3, Textavl.value(hd2), hd1), tl3):textralist, 10)
                     /* Case 8 : Merging the h=1 elements of the start.*/
                  else if ((hei1 > 1) && (hei2 == 1)) then
                     raregroup_aux_rev(List.cons(Textavl.merge_avl_nn(Textavl.merge_avl_nn(hd3, hd2), hd1), tl3):textralist, 10)
                     /* Case 9 : Merging the 3 elements.*/
                  else if (hei1 > 1) then
                     raregroup_aux_rev(List.cons(Textavl.merge_avl_nn(hd2, hd1), tl2):textralist, 10)
                     /* Case 10 : Merging the first and the second elements.*/
                  else
                     List.cons(hd1, raregroup_aux_rev(tl1, n + 1)):textralist
                     /* Case 11 : one more step forward.*/
             )
        )
  raregroup_aux_rev(lavl, 1)

/*
** reverse()
** Return a reversed list.
*/
reverse(lavl) =
  List.rev(lavl)

/*
** raconcat()
** Merge all the AVLs of a list into one only AVL.
*/
raconcat(tree, lavl) =
  match lavl : textralist with
  | [] -> tree
  | [hd | tl] -> raconcat(Textavl.merge_avl_nn(tree, hd), tl)

/*
** head()
** Return the first AVL of the list.
*/
head(lavl) =
  match lavl : textralist with
  | [] -> Textavl.empty : textavl
  | [hd | _] -> hd

/*
** next()
** Return the next AVL.
*/
next(lavl) =
  match lavl : textralist with
  | [] -> []
  | [_ | tl] -> tl

/*
** insert_str_to_the_left()
** Insert a new element of height 1 at the beginning of the list.
** A `right' version is no longer useful.
*/
insert_str_to_the_left(lavl, str, smerge) =
  match lavl:textralist with
  | [] ->
      [Textavl.singleton(str)]:textralist
  | [hd | tl] ->
      if (Textavl.node_length(str) + Textavl.left_leaf_size(hd, 0) <= smerge) then
         List.cons(Textavl.concat_left(hd, str), tl):textralist
      else
         raregroup(List.cons(Textavl.singleton(str), lavl))

/*
** insert_str_to_the_left()
** Reverse version.
*/
insert_str_to_the_left_rev(lavl, str, smerge) =
  match lavl:textralist with
  | [] ->
      [Textavl.singleton(str)]:textralist
  | [hd | tl] ->
      if (Textavl.node_length(str) + Textavl.left_leaf_size(hd, 0) <= smerge) then
         List.cons(Textavl.concat_right(hd, str), tl):textralist
      else
         raregroup_rev(List.cons(Textavl.singleton(str), lavl))

/*
** length()
** Return the number of Unicode.characters stored in the list.
*/
length(l) =
  match l : textralist with
  | [] -> 0
  | [hd | tl] -> Textavl.sizetxt(hd) + length(tl)

/*
** to_string()
** Return a string of all the contents of the list.
** Useless now, since we're using to_list.
*/
to_string(l) =
  match l : textralist with
  | [] -> ""
  | [hd | tl] -> Textavl.to_string(hd) ^ to_string(tl)

/*
** to_list()
** Return a list of all the elements of the list.
*/
to_list(l) =
  List.flatten(List.map(Textavl.to_list, l))

/*
** sub()
** Return a sub-list.
*/
sub(l, start, len) =
  match l:textralist with
  | [] -> [] : textralist
  | [hd | tl] ->
      hdlen = Textavl.sizetxt(hd);
      if (start >= hdlen) then
         sub(tl, start - hdlen, len)
      else if (hdlen >= start + len) then
         [Textavl.sub_avl(hd, start, len)]:textralist
      else
         List.cons(Textavl.sub_avl(hd, start, hdlen - start),
                   sub(tl, 0, len - hdlen + start)):textralist

/*
** sub_rev()
** Return a sub-list.
*/
sub_rev(l, start, len) =
  match l:textralist with
  | [] -> [] : textralist
  | [hd | tl] ->
      hdlen = Textavl.sizetxt(hd);
      if (start >= hdlen) then
         sub_rev(tl, start - hdlen, len)
      else if (hdlen >= start + len) then
         [Textavl.sub_avl(hd, hdlen - len - start, len)]:textralist
      else
         List.cons(Textavl.sub_avl(hd, 0, hdlen - start),
                   sub_rev(tl, 0, len - hdlen + start)):textralist
/*
** insert_aux_str_into_avls()
** Insert a string into a list of AVLs.
*/
insert_aux_str_into_avls(l, s, pos) =
  match l:textralist with
  | [] ->
      [Textavl.singleton(s):textavl]:textralist
  | [hd | tl] ->
      lenhd = Textavl.sizetxt(hd);
      if (lenhd >= pos) then
         List.cons(Textavl.insert_str_into_avl(hd, s, pos), tl):textralist
      else
         List.cons(hd, insert_aux_str_into_avls(tl, s, pos - lenhd):textralist):textralist

/*
** insert_aux_str_into_avls_rev()
** Reversed version of insert_aux_str_into_avls().
*/
insert_aux_str_into_avls_rev(l, s, pos) =
  match l:textralist with
  | [] ->
      [Textavl.singleton(s):textavl]:textralist
  | [hd | tl] ->
      lenhd = Textavl.sizetxt(hd);
      if (lenhd >= pos) then
         List.cons(Textavl.insert_str_into_avl(hd, s, lenhd - pos), tl):textralist
      else
         List.cons(hd, insert_aux_str_into_avls_rev(tl, s, pos - lenhd):textralist):textralist

/*
** insert_aux_avls_into_avls()
** Split a list in two, and insert a list inside.
*/
insert_aux_avls_into_avls(l1, l2, pos) =
  match l1:textralist with
  | [] ->
      l2:textralist
  | [hd | tl] ->
      lenhd = Textavl.sizetxt(hd);
      if (lenhd == pos) then /* Shortcut.*/
         List.cons(hd, List.append(l2, tl)):textralist
      else if (lenhd >= pos) then
         (lefthd, righthd) = Textavl.split(hd, pos);
         List.cons(lefthd, List.append(l2, List.cons(righthd, tl))):textralist
      else
         List.cons(hd, insert_aux_avls_into_avls(tl, l2, pos - lenhd)):textralist

/*
** insert_aux_avls_into_avls_rev()
** Reversed version of insert_aux_avls_into_avls().
*/
insert_aux_avls_into_avls_rev(l1, l2, pos) =
  match l1:textralist with
  | [] ->
      l2:textralist
  | [hd | tl] ->
      lenhd = Textavl.sizetxt(hd);
      if (lenhd == pos) then
         [raconcat(hd, List.append(l2, tl))]:textralist
      else if (lenhd >= pos) then
         (lefthd, righthd) = Textavl.split(hd, lenhd - pos);
         [raconcat(lefthd, List.append(l2, List.cons(righthd, tl)))]:textralist
      else
         List.cons(hd, insert_aux_avls_into_avls_rev(tl, l2, pos - lenhd)):textralist

/*
** check_text()
** Show the height of the AVLs in the list.
** Useful for debug only.
*/
check_text(l, n) =
  match l:textralist with
  | [] ->
      "Sons : {n}"
  | [hd | tl] ->
      "H:{Textavl.sizetr(hd):int}; " ^ check_text(tl, n + 1);

/*
** dot_graph()
** Return a string in the Dot syntax showing the structure of the list and its AVLs.
*/
dot_graph(txt, number:int, side:string, shape:string) =
 match txt:textralist with
  | [] ->
      "\"Empty {side}\" [shape={shape}];\n{side} -> \"Empty {side}\";\n"
  | [hd | tl] ->
      Textavl.dot_avlgraph(hd, "Tree {side} {number} - ", "{side}", " [shape={shape}]") ^ dot_graph(tl, number + 1, side, shape)

/*
** uppercase()
** Uppercase the content of the list.
*/
uppercase(l) =
  match l : textralist with
  | [] -> [] : textralist
  | [hd | tl] ->
      List.cons(Textavl.uppercase(hd), uppercase(tl)):textralist

/*
** lowercase()
** Lowercase the content of the list.
*/
lowercase(l) =
  match l : textralist with
  | [] -> [] : textralist
  | [hd | tl] ->
      List.cons(Textavl.lowercase(hd), lowercase(tl)):textralist

/*
** get()
** Return the Unicode.character.
*/
get(lavl, pos) =
  match lavl : textralist with
  | [] -> error("[In Textralist.get()]")
  | [hd | tl] ->
      lenhd = Textavl.sizetxt(hd);
      if (lenhd >= pos) then Textavl.get(hd, pos)
      else get(tl, pos - lenhd)

/*
** mapc()
** Apply a function to the Unicode.characters of the trees of the list.
*/
/*
mapc(l, f) =
  match l:textralist with
  | [] -> [] : textralist
  | [hd | tl] ->
      List.cons(Textavl.mapc(hd, f), mapc(tl, f)):textralist
*/

/*
** start()
** Build a new iterator.
*/
start(l) =
  match l:list(textavl) with
  | [] -> (0, "", [], [])
  | [hd | tl] ->
      (a, b, c) = Textavl.start(hd);
      (a, b, tl, c)

/*
** rstart()
** Reversed version of start().
*/
rstart(l) =
  rec rstart_aux(l, mem, old) =
    match l:list(textavl) with
    | [] ->
        (a, b, c) = Textavl.rstart(old);
        (a, b, next(mem), c)
    | [hd|tl] ->
        rstart_aux(tl, List.cons(hd, mem), hd)
  match l:list(textavl) with
  | [] ->
      (0, "", [], [])
  | [_|_] ->
      rstart_aux(l, [], Textavl.empty)

}} /* disabled for : Textralist */

/*
** Ascii_textralist : RA lists of Ascii-nodes AVLs.
*/
//Ascii_textralist = Make_textralist(Ascii_textavl);

/*
** Utf8_textralist : RA lists of UTF8-nodes AVLs.
** Camomile, Eucalyptutf, Cactutf.
*/
/*Utf8camo_textralist = Make_textralist(Utf8camo_textavl);*/
//Utf8euca_textralist = Make_textralist(Utf8euca_textavl);
Utf8cact_textralist = Make_textralist//(Utf8cact_textavl);
