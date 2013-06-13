/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
    @authors Matthieu Guffroy, Frédéric Ye, Corentin Gallet
*/

/**
 * {1 About this module}
 *
 * a [textavl] is the inner data structure of a texralist (which is itself
 * a inner data structure of a text).
 *
 * Some lines may be commented, because of the deactivation of mapc(), and
 * because we don't need Camomile anymore.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * textavl
 * It's a AVL tree.
**/
@abstract type textavl =
   {empty}
 /
   {
     left : textavl ;
     value : string ;
     right : textavl ;
     sizetree : int ; /* height of the tree, not the size! */
     sizetext : int ;
   }

type stringtextavl = textavl

type utf8eucatextavl = textavl

type utf8cacttextavl = textavl

/**
 * {1 Interface}
 */

Textavl =
       paramfun_length=Cactutf.length
       paramfun_sub=Cactutf.sub
       paramfun_sub_opt=Cactutf.sub_opt
       paramfun_upper=Cactutf.uppercase
       paramfun_lower=Cactutf.lowercase
       paramfun_get=Cactutf.get
       paramfun_nextchar=Cactutf.next
       paramfun_look=Cactutf.look
       paramfun_cons=Cactutf.cons
       description="Cactutf"


{{

/**
 * {2 Inner functions}
 *
 * FIXME: probably private functions.
**/

/*
** Export of inner functions, so the module's parameters can be used by Textralist and Text.
*/

node_length =
  paramfun_length
node_sub =
  paramfun_sub
node_sub_opt =
  paramfun_sub_opt
node_uppercase =
  paramfun_upper
node_lowercase =
  paramfun_lower
node_get =
  paramfun_get
node_nextchar =
  paramfun_nextchar
node_look =
  paramfun_look
node_cons =
  paramfun_cons
textavl_description =
  description

/**
 * {2 Texavl API}
 *
 * FIXME: probably private functions.
**/


/**
 * Build an empty leaf.
**/
empty =
  {empty}:textavl

/**
 * Return the value of the current node.
 * <!> Returns a dummy string if the textavl is empty, does not fail
**/
value(s) =
  match s : textavl with
  | {empty} ->
      "[ERROR into Textavl.value]"
  | {~value; ...} ->
      value

/**
 * Return the height of the tree.
 * FIXME: probably private functions.
**/
sizetr(s) =
  match s : textavl with
  | {empty} ->
      0
  | {~sizetree; ...} ->
      sizetree

/**
 * Return the number of characters of the tree.
**/
sizetxt(s) =
  match s : textavl with
  | { empty} ->
      0
  | { ~sizetext ; ... } ->
      sizetext

/**
 * Build a node.
 * Beware that using this could build no-AVL trees.
 * Use create_node() instead if you're not sure.
**/
node(l, v, r, str, stxt) =
  {left = l; value = v; right = r; sizetree = str; sizetext = stxt} : textavl

/**
 * Works like the standard String.sub() function.
**/
stringsub(s:string, start:int, len:int): string = node_sub(s, start, len)

/**
 * Return a sub-tree of the main tree.
 * L:left ; M:middle (the current node aka root); R:right; FULL:the full sub-tree.
**/
sub_avl(t:textavl, start, len) =
  match t:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=_; sizetext=stxt} ->
      if ((start == 0) && (len == stxt)) then          /* Case 1 : (full LMR)*/
         t
      else
         lenl = sizetxt(l);
         lenv = node_length(v);
         if (lenl > start) then                        /* Using the left node.*/
            (if (lenl >= start + len) then             /* Case 2 : only the left node (L)*/
                sub_avl(l, start, len)
             else if (lenl + lenv >= start + len) then /* Case 3 : left and root. (LM)*/
                uberbalance(create_node(sub_avl(l, start, lenl - start), stringsub(v, 0, len - lenl + start), {empty}))
             else                                      /* Case 4 : left, root and right. (LMR)*/
                uberbalance(create_node(sub_avl(l, start, lenl - start), v, sub_avl(r, 0, len - lenv - (lenl - start))))
            )                                          /* Not using the left node.*/
         else
            if (start < lenl + lenv) then              /* Using the root.*/
               (if (start + len <= lenl + lenv) then   /* Case 5 : only a part of the root. (M)*/
                   singleton(stringsub(v, start - lenl, len))
                else                                   /* Case 6 : root and right. (MR)*/
                   uberbalance(create_node({empty}, stringsub(v, start - lenl, lenv - start + lenl), sub_avl(r, 0, len - lenv - lenl + start)))
               )
            else                                       /* Case 7 : Not using the left nor the root, so right only (R)*/
               sub_avl(r, start - lenl - lenv, len)

/**
 * Create a node.
**/
create_node(l,v,r) =
  strl = sizetr(l);
  strr = sizetr(r);
  stxtl = sizetxt(l:textavl);
  stxtr = sizetxt(r:textavl);
  stxtv = node_length(v);
  stxt = stxtl + stxtr + stxtv;
  str = if (strl > strr) then (strl + 1) else (strr + 1);
  node(l,v,r,str,stxt)

/**
 * Return the left son of the node, or [{empty}] if the tree is empty
**/
left_son(avl) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {~left; ...} ->
      left:textavl

/**
 * Return the right son of the node, or [{empty}] if the tree is empty
**/
right_son(avl) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {~right; ...} ->
      right

/**
 * Balance the AVL tree.
 *
 * FIXME: probably not exported, internal function for asserting invariants
**/
balance(avl) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      leftb = sizetr(l);
      rightb = sizetr(r);
      if (Int.abs(rightb - leftb) > 1) then                         /* Unbalanced*/
         if (rightb - leftb <= -2) then                                /* Unbalancing factor : -2 (Left).*/
            if (sizetr(right_son(l)) - sizetr(left_son(l)) <= -1) then /* Unbalanced left son : -1.*/
               create_node(left_son(l), value(l), create_node(right_son(l), v, r))
            else                                                       /* Unbalanced left son : +1.*/
               create_node(create_node(left_son(l), value(l), left_son(right_son(l))),
                           value(right_son(l)),
                           create_node(right_son(right_son(l)), v, r))
         else                                                          /* Unbalancing factor : +2 (Right).*/
            if (sizetr(right_son(l)) - sizetr(left_son(l)) <= -1) then /* Unbalanced right son : -1.*/
               create_node(create_node(l, v, left_son(left_son(r))),
                           value(left_son(r)),
                           create_node(right_son(left_son(r)), value(r), right_son(r)))
            else                                                       /* Unbalanced right son : +1.*/
               create_node(create_node(l, v, left_son(r)), value(r), right_son(r))
      else                                                             /* Balanced.*/
         avl

/**
 * An heavy version of balance().
 * Use it when there can be more than a factor of +2/-2.
 * That's it, when you used no-AVL operations on an AVL tree.
 *
 * FIXME: like {!Texavl.balance} probably private
**/
uberbalance(avl) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      leftb = sizetr(l);
      rightb = sizetr(r);
      if (Int.abs(rightb - leftb) > 1) then                         /* Unbalanced.*/
         if (rightb - leftb <= -2) then                                /* Unbalancing factor : -2 (Left).*/
            if (sizetr(right_son(l)) - sizetr(left_son(l)) <= -1) then /* Unbalanced left son : -1.*/
               uberbalance(create_node(uberbalance(left_son(l)),
                                       value(l),
                                       uberbalance(create_node(right_son(l), v, r))))
            else                                                       /* Unbalanced left son : +1.*/
               uberbalance(create_node(uberbalance(create_node(left_son(l), value(l), left_son(right_son(l)))),
                                       value(right_son(l)),
                                       uberbalance(create_node(right_son(right_son(l)), v, r))))
         else                                                          /* Unbalancing factor : +2 (Right).*/
            if (sizetr(right_son(l)) - sizetr(left_son(l)) <= -1) then /* Unbalanced right son : -1.*/
               uberbalance(create_node(uberbalance(create_node(l, v, left_son(left_son(r)))),
                                       value(left_son(r)),
                                       uberbalance(create_node(right_son(left_son(r)), value(r), right_son(r)))))
            else                                                       /* Unbalanced right son : +1.*/
               uberbalance(create_node(uberbalance(create_node(l, v, left_son(r))),
                                       value(r),
                                       uberbalance(right_son(r))))
      else                                                             /* Balanced.*/
         avl

/**
 * Verify the validation of the AVL.
 * Debug-only function.
**/
check_avl(avl, text) =
  match avl:textavl with
  | {empty} ->
      ""
  | {left=l; value=v; right=r; sizetree=str; sizetext=stxt} ->
      if (str != max(sizetr(l), sizetr(r)) + 1) then
         "ERROR : Tree size corrupted:{v}:" ^ text
      else if (stxt != (sizetxt(l) + sizetxt(r) + node_length(v))) then
         "ERROR : Text size corrupted:{v}:" ^ text
      else if (Int.abs(sizetr(r) - sizetr(l)) > 1) then
         "ERROR : Unbalanced tree:{v}:" ^ text
      else if (stxt == 0) then
         "ERROR : string of size 0:{v}:" ^ text
      else
          check_avl(l, text) ^ check_avl(r, text)

/**
 * Build a leaf.
**/
singleton(el) =
  node({empty}, el, {empty}, 1, node_length(el))

/**
 * Return the content of the tree.
 * Maybe useless.
**/
to_string(avl) =
  match avl:textavl with
  | {empty} -> ""
  | {left=l; value=v; right=r; ...} ->
      to_string(l) ^ v ^ to_string(r)

/**
 * Return the tree as a list.
**/
to_list(avl) =
  match avl:textavl with
  | {empty} -> []
  | {left=l; value=v; right=r; ...} ->
    List.append(to_list(l), List.cons(v, to_list(r)))

/**
 * Create a little AVL. It's used at the concatenation of two text-strings.
 * Why to the left ? Why not !
**/
build_pair(s1, s2) =
  create_node(singleton(s1), s2, {empty})

/**
 * Basic version of {!Textaval.smart_merge_avl_nn} (see below).
**/
merge_avl_nn(avl1, avl2) =
  rec merge_avl_nn_aux(avl1, avl2) =
    match avl1:textavl with
    | {empty} ->
        avl2:textavl
    | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
        uberbalance(create_node(l, v, merge_avl_nn_aux(r, avl2))):textavl
   match (avl1,avl2) with
   | ({empty}, b) ->
       b:textavl
   | (a, {empty}) ->
       a:textavl
   | (a, b) ->
       merge_avl_nn_aux(a, b)

/**
 * Merge a string with a node.
**/
split_insert(s1, s2, pos, lens1) =
  create_node(singleton(stringsub(s1, 0, pos)),
              s2,
              singleton(stringsub(s1, pos, lens1 - pos)))

/**
 * Insert a string into a normal AVL at the position pos.
**/
insert_str_into_avl(avl, s, pos) =
  match avl:textavl with
  | {empty} ->
      singleton(s)
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      lenl = sizetxt(l);
      lenv = node_length(v);
      if (pos < lenl) then
         create_node(insert_str_into_avl(l, s, pos), v, r)
      else if (lenl + lenv < pos) then
         create_node(l, v, insert_str_into_avl(r, s, pos - lenl - lenv))
      else
         create_node(l, stringsub(v, 0, pos - lenl) ^ s ^ stringsub(v, pos - lenl, lenv - pos + lenl), r)

/**
 * Return two AVLs from one.
**/
split(avl, pos) =
  match avl:textavl with
  | {empty} ->
      ({empty}:textavl, {empty}:textavl)
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      lenl = sizetxt(l);
      lenv = node_length(v);
      if (pos < lenl) then
         (a, b) = split(l, pos);
         (a, balance(create_node(b, v, r)))
      else if (lenl + lenv < pos) then
         (a, b) = split(r, pos - lenl - lenv);
         (balance(create_node(l, v, a)), b)
      else
         (if ((lenv - pos + lenl) == 0) then
             (balance(create_node(l, stringsub(v, 0, pos - lenl), {empty})),
              r)
          else
             (balance(create_node(l, stringsub(v, 0, pos - lenl), {empty})),
              balance(create_node({empty}, stringsub(v, pos - lenl, lenv - pos + lenl), r)))
         )

/**
 * Merge two AVLs into one.
**/
smart_merge_nn(avl1, avl2) =
  hei1 = sizetr(avl1);
  hei2 = sizetr(avl2);
  if (hei1 == 0) then
     avl2
  else if (hei2 == 0) then
     avl1
  else if (hei1 != hei2) then
     merge_avl_nn(avl1, avl2)
  else
     (root, avl1m) = smart_merge_nn_aux_left(avl1);
     create_node(avl1m, root, avl2)

/**
 * Sub-function of smart_merge_nn().
**/
smart_merge_nn_aux_left(avl) =
  match avl:textavl with
  | {empty} ->
      ("", {empty}:textavl)
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      if (r == {empty}) then
         (v, l)
      else
         (a, b) = smart_merge_nn_aux_left(r);
         (a, balance(create_node(l, v, b)))

/**
 * Return a string in the Dot format.
**/
dot_avlgraph(set, root, father, special_string) =
                  match set:textavl with
                  | {empty} ->
                      "\"" ^ father ^ "\" -> \"" ^ root ^ ":EMPTY\";\n"
                  | {left=l; value=v; right=r; sizetree=str; sizetext=stxt} ->
                      named = root ^ " : " ^ v ^ " (sizetree={str} sizetxt={stxt})";
                      dot_avlgraph(l, root ^ "L", named, "")
                      ^ "\"" ^ named ^ "\" " ^ special_string ^ ";\n"
                      ^ "\"" ^ father ^ "\" -> \"" ^ named ^ "\";\n"
                      ^ dot_avlgraph(r, root ^ "R", named, "")

/**
 * Return an AVL in uppercase ("aAa" -> "AAA").
**/
uppercase(avl) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=str; sizetext=stxt} ->
      node(uppercase(l), node_uppercase(v), uppercase(r), str, stxt)

/**
 * Return an AVL in lowercase ("aAa" -> "aaa").
**/
lowercase(avl) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=str; sizetext=stxt} ->
      node(lowercase(l), node_lowercase(v), lowercase(r), str, stxt)

/**
 * Return the length of the left-bottom leaf.
**/
left_leaf_size(avl, old) =
  match avl:textavl with
  | {empty} ->
      old
  | {left=l; value=_; right=_; sizetree=_; sizetext=stxt} ->
      left_leaf_size(l, stxt)

/**
 * Directly concatenate a string and the left leaf.
**/
concat_left(avl, st) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      if (l == {empty}) then
         create_node(l, st ^ v, r)
      else
         create_node(concat_left(l, st), v, r)

/**
 * Directly concatenate a string and the right leaf.
**/
concat_right(avl, st) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      if (r == {empty}) then
         create_node(l, v ^ st, r)
      else
         create_node(l, v, concat_right(r, st))

/**
 * Return the character.
**/
get(avl, pos) =
  match avl:textavl with
  | {empty} ->
      error("[Error in Textavl.get]")
  | {left=l; value=v; right=r; sizetree=_; sizetext=_} ->
      lenl = sizetxt(l);
      lenv = node_length(v);
      if (pos < lenl) then
         get(l, pos)
      else if (lenl + lenv < pos) then
         get(r, pos - lenl - lenv)
      else
         node_get(v, pos - lenl):Unicode.character

/*
 * Apply a function to a character.
**/
/*
mapc(avl, f) =
  match avl:textavl with
  | {empty} ->
      {empty}:textavl
  | {left=l; value=v; right=r; sizetree=str; sizetext=stxt} ->
      node(mapc(l, f), paramfun_applyc(v, f), mapc(r, f), str, stxt);
*/

/**
** start()
** Return a set of variables to build a new iterator for this tree.
*/
start(avl) =
  rec start_aux(avl, c, s, heap) =
    match avl:textavl with
    | {empty} ->
        (c, s, heap)
    | {left=l; value=v; right=_; sizetree=_; sizetext=_} ->
        if (l == {empty}) then
           (node_get(v, 0), v, List.cons(avl, heap))
        else
           start_aux(l, c, s, List.cons(avl, heap))
  start_aux(avl, 0, "", [])

/**
 * Reversed version of start().
**/
rstart(avl) =
  rec rstart_aux(avl, c, s, heap) =
    match avl:textavl with
    | {empty} ->
        (c, s, heap)
    | {left=_; value=v; right=r; sizetree=_; sizetext=_} ->
        if (r == {empty}) then
           (node_get(v, 0), v, List.cons(avl, heap))
        else
           rstart_aux(r, c, s, List.cons(avl, heap))
  rstart_aux(avl, 0, "", [])

}}

// -----------------------------------------------------------------

/* ---------
   | ASCII |
   - ------- */

/*
 * [ascii_stringget(a, b)]
 * returns the ascii value (later utf-8 value) of the [b-th] char of the string [a]
**/
// @private ascii_stringget(a, b) =
//   if b < 0 || b >= String.length(a) then int_of_char(' ')
//   else int_of_first_char(String.get(b, a))

/*
** ascii_apply_mapc()
*/
/*
ascii_apply_mapc(s, f) =
  rec ascii_apply_mapc_aux(s, f, n) =
    if (n == 0) then
       ""
    else if (n == 1) then
       Char.to_string(Char.unsafe_chr(f(ascii_stringget(s, 0))))
    else
       ascii_apply_mapc_aux(s, f, n - 1) ^ Char.to_string(Char.unsafe_chr(f(ascii_stringget(s, n - 1))))
  ascii_apply_mapc_aux(s, f , String.length(s))
*/

/*
** Ascii_textavl : AVL trees with standard strings.
*/
/*Ascii_textavl = Make_textavl(String.length,
                             a, b, c -> Option.some(String.sub(c, b, a)),
                             String.to_upper,
                             String.to_lower,
                             ascii_stringget,
                             /*ascii_apply_mapc,*/
                             (s, n -> (n + 1)),
                             ascii_stringget,
                             (a -> String.of_ascii_val(a)),
                             "US-ASCII")*/

// -----------------------------------------------------------------

/* --------
   | UTF8 |
   -------- */

/*
** utf8_apply_mapc()
*/
/*
utf8_apply_mapc(s, f) =
  rec utf8_apply_mapc_aux(s, f, n) =
    if (n == 0) then
       ""
    else if (n == 1) then
       Unicode_utf8.cons((f(Unicode_utf8.get(s, 0))))
    else
       utf8_apply_mapc_aux(s, f, n - 1) ^ Unicode_utf8.cons((f(Unicode_utf8.get(s, n - 1))))
  utf8_apply_mapc_aux(s, f, Unicode_utf8.last(s))
*/

/*
** Utf8_txtavl : AVL trees with unicode strings.
** Support international languages.
*/

/*
** Camomile.
*/
/*
Utf8camo_textavl = Make_textavl(Unicode_utf8.length,
                                Unicode_utf8.sub,
                                Unicode_utf8.uppercase,
                                Unicode_utf8.lowercase,
                                Unicode_utf8.get,
                                /*utf8_apply_mapc,*/
                                Unicode_utf8.next,
                                Unicode_utf8.look)
*/

/*
** Eucalyptutf.
*/
/*Utf8euca_textavl = Make_textavl(Eucalyptutf.length,
                                Eucalyptutf.sub,
                                Eucalyptutf.uppercase,
                                Eucalyptutf.lowercase,
                                Eucalyptutf.get,
                                Eucalyptutf.next,
                                Eucalyptutf.look,
                                Eucalyptutf.cons,
                                "Eucalyptutf")
*/
/*
** Cactutf.
*/
/* Utf8cact_textavl = Make_textavl(Cactutf.length,
                                Cactutf.sub,
                                Cactutf.uppercase,
                                Cactutf.lowercase,
                                Cactutf.get,
                                Cactutf.next,
                                Cactutf.look,
                                Cactutf.cons,
                                "Cactutf")
                                */

// -----------------------------------------------------------------
