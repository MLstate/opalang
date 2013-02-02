/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package stdlib.apis.mongo

/**
 * MongoDB binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * [MongoView] provides a way of handling field selects in queries while
 * retaining some run-time type-safety.
 *
 * We do this by allowing the return type to be different
 * from, but a sub-type of (in the sense of field exclusion), the parent collection's type.
 * We can't enforce type-safety at compile time but we do insert runtime type-checks into the
 * view, both at creation time and at query time.  If you are 100% sure that your
 * types are correct, you can eliminate the run-time type-check by simply using
 * the MongoCollection module "unsafe" operations.
 *
 * The fields are selected by building a [fields] value using the [MongoCollection.Fields] module.
 * The result has to be cast to the return type which is derived from the collection
 * type with the required fields included/excluded.
 *
 * There are two modes of operation depending upon the [is_opa] parameter when the
 * view is created:
 *
 * If [is_opa] is [false] then the return type has to be the included/excluded sub-type
 * but with all fields turned into Bson.register values,
 * for example, collection type [\{a:int; b:string\}] and field selector [\{b:1\}] result in
 * the result type: [\{b:Bson.register(string)\}].  This is checked at runtime.
 *
 * Note that, for this case, we are obliged to turn all the fields into [Bson.register] types because
 * MongoDB will return a record with missing fields for documents which match the query
 * but which do not have all of the fields selected.
 *
 * If [is_opa] is [true] then the return type does not need the [Bson.register] transformation
 * but note that any values returned which have missing fields will be ignored.
 *
 * Once the view has been created, you simply substitute the [MongoView] query functions for the
 * [MongoCollection] functions.
 *
 * [MongoForeign] is a simple effort at managing foreign keys in client space.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/**
 * This module is work in progress.
 **/

@abstract
type Mongo.view('a,'b) = {
  coll: Mongo.collection('a);
  vty: OpaType.ty; // type of the view collection
  is_opa: bool; // if true, we assume an OPA type and ignore incomplete documents
}

@abstract
type Mongo.foreign('a,'b,'c,'d,'e) = {
  primary: Mongo.view('a,'b); // the parent view
  foreign: Mongo.view('c,'d); // the foreign view
  pkey: string;
  fkey: string;
}

MongoView = {{

  @private make_reg(fld) = {fld with ty={TyName_args=[fld.ty]; TyName_ident="Bson.register"}}

  @private
  type_from_fields(pty:OpaType.ty, fields:Mongo.fields): OpaType.ty =
    if not(MongoCollection.Fields.validate(fields))
    then
      msg = "Fields failed to validate"
      do Log.error("View.type_from_fields",msg)
      @fail(msg)
    else
      tst =
        match List.unique_list_of(List.map((e -> Bson.int_of_value(e.value)),List.filter((e -> e.name != "_id"),fields))) with
        | [{some=num}] -> (match num with 0 -> not | _ -> (tf -> tf))
        | _ ->
          msg = "Bad fields value {fields}"
          do Log.error("View.type_from_fields",msg)
          @fail(msg)
      dfields = List.map((e -> String.explode(".",e.name)),fields)
      MongoTypeSelect.filter_field(pty, (fs -> tst(List.mem(fs,dfields))))

  @private
  verify_type_match(ty1:OpaType.ty, ty2:OpaType.ty, from:string, msg:string): void =
    //do println("ty1={OpaType.to_pretty(MongoTypeSelect.name_type(ty1))}")
    //do println("ty2={OpaType.to_pretty(MongoTypeSelect.name_type(ty2))}")
    // We can't use the fancy caching in compare_ty since our altered types mess with the caching
    if not(MongoTypeSelect.naive_type_compare(ty1, ty2))
    then
      msg = "{msg} {OpaType.to_pretty(ty1)} and {OpaType.to_pretty(ty2)}"
      do Log.error(from, msg)
      @fail(msg)
    else void

  create(c:Mongo.collection('collection), vfields:Mongo.fields, is_opa:bool): Mongo.view('collection,'view) =
    coll = MongoCollection.fields(c, {some=vfields})
    pty = @typeval('collection)
    //do verify_type_match(pty, coll.ty, "View.create","Attempt to create view from non-matching parent type")
    fvty = type_from_fields(pty, vfields)
    vty = if is_opa then fvty else MongoTypeSelect.map_field(fvty, make_reg)
    cvty = @typeval('view)
    //do println("pty={OpaType.to_pretty(pty)}")
    //do println("fvty={OpaType.to_pretty(fvty)}")
    //do println("vty={OpaType.to_pretty(vty)}")
    //do println("cvty={OpaType.to_pretty(cvty)}")
    do verify_type_match(vty, cvty, "View.create","Attempt to create view with incompatible view types")
    { ~coll; ~vty; ~is_opa; }

  of_collection(c:Mongo.collection('collection), is_opa:bool): Mongo.view('collection,'collection) =
    { coll=c; vty=@typeval('collection); ~is_opa; }

  @private
  runtime_view_type_check(v:Mongo.view('value,'view), from:string): void =
    //do verify_type_match(@typeval('value), v.coll.ty, from, "Collection type does not match view type")
    do verify_type_match(@typeval('view), v.vty, from, "View type does not match result type")
    void

  find_one(v:Mongo.view('value,'view), select:Mongo.select('value)): outcome('view,Mongo.failure) =
    do runtime_view_type_check(v, "View.find_one")
    MongoCollection.find_one_unsafe(v.coll, select, v.is_opa)

  query(v:Mongo.view('value,'view), select:Mongo.select('value)): outcome(Mongo.collection_cursor('view),Mongo.failure) =
    do runtime_view_type_check(v, "View.query")
    MongoCollection.query_unsafe(v.coll, select, v.is_opa)

  find_all(v:Mongo.view('value,'view), select:Mongo.select('value)): outcome(list('view),Mongo.failure) =
    do runtime_view_type_check(v, "View.find_all")
    MongoCollection.find_all_unsafe(v.coll, select, v.is_opa)

}}

MongoForeign = {{

  @private ML = MongoLog

  create(primary:Mongo.view('ps,'pr), foreign:Mongo.view('fs,'fr), pkey:string, fkey:string)
       : Mongo.foreign('ps,'pr,'fs,'fr,('pr,Bson.register('fr))) =
    pty = @typeval('ps)
    pkt = MongoTypeSelect.find_label_in_row(pty,pkey)
    fatal(msg) =
      do Log.error("Foreign.create","Can't find primary key {pkey} in type {OpaType.to_pretty(pty)}")
      @fail(msg)
    do if not(Option.is_some(pkt))
       then fatal("Can't find primary key {pkey} in type {OpaType.to_pretty(pty)}")
    fty = @typeval('fs)
    fkt = MongoTypeSelect.find_label_in_row(fty,fkey)
    do if not(Option.is_some(fkt))
       then fatal("Can't find foreign key {fkey} in type {OpaType.to_pretty(fty)}")
    do if not(MongoTypeSelect.naive_type_compare((Option.get(pkt)).ty,(Option.get(fkt)).ty))
       then fatal("Mismatching primary {OpaType.to_pretty(pty)} and foreign {OpaType.to_pretty(fty)}")
    { ~primary; ~foreign; ~pkey; ~fkey }

  find_one(f:Mongo.foreign('ps,'pr,'fs,'fr,'view), select:Mongo.select('ps)): outcome('view,Mongo.failure) =
    match MongoCollection.find_one_doc(f.primary.coll, select) with
    | {success=pdoc} ->
       (match Bson.bson_to_opa(pdoc, @typeval('pr)) with
        | {some=pv} ->
           pv = (Magic.id(pv):'pr)
           (match Bson.dot_element(pdoc,f.pkey) with
            | {some=e} ->
               //do println("Foreign.find_one: e={Bson.to_pretty([e])}")
               (match MongoCollection.find_one_doc(f.foreign.coll, ([{e with name=f.fkey}]:Mongo.select('fr))) with
                | {success=fdoc} ->
                   //do println("Foreign.find_one: fdoc={Bson.to_pretty(fdoc)}")
                   (match Bson.bson_to_opa(fdoc, @typeval('fr)) with
                    | {some=fv} -> {success=(pv,{present=Magic.id(fv):'fr})}
                    | {none} -> {failure={Error="Foreign.find_one: Bson to OPA conversion error for foreign value"}})
                | {failure=_} -> {success=(pv,{absent})})
            | {none} -> {failure={Error="Foreign.find_one: Can't find primary key {f.pkey}"}})
        | {none} -> {failure={Error="Foreign.find_one: Bson to OPA conversion error for primary value"}})
    | {~failure} -> {~failure}

}}

// End of file view.opa
