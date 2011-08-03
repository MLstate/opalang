(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   This pass replaces every call to a module field (module.field) by its toplevel alias.
   It first collects every toplevel declaration (corresponding to the module fields)
   and stores it in an environment

   Then it replaces every module.field by its corresponding toplevel identifier

   Example :

   {[
   toplevel_empty = []
   toplevel_add (x, y, m) = (x,y)::m
   toplevel_find (x, m) = List.assoc (x, m)
   rec toplevel_remove ((x,y),m) =
     match m with
       [] -> []
     | hd::tl when hd <> (x,y) -> hd::(toplevel_remove ((x,y), tl))
     | hd::tl -> toplevel_remove ((x,y) tl)

   (* The module *)
   map = \{\{
     empty = toplevel_empty;
     add = toplevel_add;
     find = toplevel_find;
     remove = toplevel_remove;
   \}\}

   my_map = map.empty
   my_map = map.add (0, 'a', my_map)
   my_map = map.remove ((0,'a'), my_map)
   ]}

   The last part of the code is rewritten in

   {[
   my_map = toplevel_empty
   my_map = toplevel_add (0, 'a', my_map)
   my_map = toplevel_remove ((0,'a'), my_map)
   ]}

   @author Esther Baruk
*)

val process_code :
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
