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

val warning_set : WarningClass.Set.t

val process_code : QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code -> QmlTypes.gamma * QmlAst.annotmap * QmlAst.code * IdentSet.t

(*
   The transformation related to closure serialization is as follows:

   --- Source code
   f(x:list) =
     g(y) = (x,y)
     (g,g(1))

   --- After the early lambda lifting
   g(x,y) = (x,y)
   f(x) =
     (@partial_apply(g(x)),g(x,1))

   --- After instrumentation by this pass
   g(x,y) = (x,y)
   g'(x) = @partial_apply(g(x),@typeof(x))
   f(x) =
     (g'(x),g(x,1))

   --- After ei
   g(x,y) = (x,y)
   g'(ty,x) = @partial_apply(g(x),list(ty))
   f(ty,x) =
    (g'(ty,x),g(x,1))

   --- After closures, assuming no cps:
   g(x,y) = (x,y)
   clos_g = %%closure_create%%(g,2,true)
   g'(ty,x) = %%closure_apply_env_with_ty%%(clos_g,@llarray(x),@llarray(list(ty)))
   // no need to create a closure for g', it is the code that creates the closures
   f(ty,x) =
     (g'(ty,x),g(x,1))
   clos_f = %%closure_create%%(f,2,true)


   And the more complicated case, where the lifted function is recursive through its closure:
   --- Source code
   id(x) = x
   f(x:list) =
     rec g(y) = if true then x else id(g)(y)
     g

   --- After early ll
   id(x) = x
   rec g(x,y) = if true then x else id(@partial_apply(g(x)))(y)
   f(x) =
     @partial_apply(g(x))

   --- After instrumentation
   id(x) = x
   rec g(x,y) = if true then x else id(g'(x))(y)
   and g'(x) = @partial_apply(g(x),OpaType.ty,@typeof(x)) // tricky case, we must anticipate
                                                          // the addition of a typevar by ei
   f(x) = g'(x)

   --- After ei
   id(x) = x
   rec g(ty,x,y) = if true then x else id(g'(ty,x))(y)
   and g'(ty,x) = @partial_apply(g(ty,x),OpaType.ty,list(ty))
   f(ty,x) = g'(ty,x)

   --- After closures:
   id(x) = x
   clos_id(x) = %%closure_create%%(id,1,true)
   clos_g = %% closure_create_no_function%%(3,true)
   rec g(ty,x,y) = if true then x else @clos_apply(id(g'(ty,x)),y)
   and g'(ty,x) = %%closure_apply_env_with_ty%%(clos_g,@llarray(ty,x),@llarray(OpaType.ty,list(ty)))
   f(ty,x) = g'(ty,x)
*)
