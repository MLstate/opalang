(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   This module implements a pass to lighten deep record expressions

   @author Esther Baruk
*)

(**
   What this pass does :
   it goes deep into a record expression and when the maximum depth is reached,
   expressions contained in the record are lifted into let-bindings which are
   inserted just before the record expression.

   Example :
   {[
   r =
     { hd =
           { hd =
                 { hd =
                       { hd = { hd = "foo" ; tl = { nil = { } } };
                         tl = { hd = "bar" ; tl = { nil = { } } }
                       };
                   tl = { nil = { } }
                };
             tl = { nil = { } }
           };
       tl = { nil = { } }
     }
   ]}

   is rewritten in

   {[
   r =
     v0_r = { }
     v1_r = { nil = v0_r }
     v2_r = "foo"
     v3_r = { hd = v2_r ; tl = v1_r }
     v4_r = { }
     v5_r = { nil = v4_r }
     v6_r = "bar"
     v7_r = { hd = v6_r ; tl = v5_r }
     v8_r = { hd = v3_r ; tl = v7_r }
     v9_r = { }
     v10_r = { nil = v9_r }
     { hd =
         { hd = { hd = v8_r ; tl = v10_r };
           tl = { nil = { } }
         };
       tl = { nil = { } }
     }
   ]}
*)

(** The function that does the modification on the code *)
val process_code :
  typed:bool ->
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code
