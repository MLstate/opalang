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

import stdlib.core.map
import stdlib.system

type path = external

type MongoDb.key = { IntKey: int } / { StringKey: string } / { AbstractKey: Bson.document } // <- for map(bool,int) etc.
type MongoDb.path = list(MongoDb.key)

/* We need access to this to do the embedding properly */
type MongoDb.node = { name:string; typ:string; id:int; node:MongoDb.key; zubnodes:MongoDb.schema; }
type MongoDb.schema = list(MongoDb.node)

type tm = ordered_map(OpaType.ty,int,Order.default)
type rm = ordered_map(int, bool, String.order)
type dm = ordered_map(MongoDb.path, OpaType.ty, Order.default)

Schema = {{

  TypeMap = Map_make(Compare.order_ty)

  order_path(pth1, pth2): Order.ordering =
    match (pth1, pth2) with
    | ([],[]) -> {eq}
    | ([_|_],[]) -> {gt}
    | ([],[_|_]) -> {lt}
    | ([k1|r1],[k2|r2]) ->
      (match (k1,k2) with
       | ({IntKey=i1},{IntKey=i2}) ->
         if i1 == i2
         then order_path(r1,r2)
         else Int.ordering(i1, i2)
       | ({StringKey=s1},{StringKey=s2}) ->
         if s1 == s2
         then order_path(r1,r2)
         else String.ordering(s1, s2)
       | ({AbstractKey=a1},{AbstractKey=a2}) ->
         if a1 == a2
         then order_path(r1,r2)
         else Order.compare(a1, a2, Order.default)
       | ({AbstractKey=_},_) -> {lt}
       | (_,{AbstractKey=_}) -> {gt}
       | ({StringKey=_},_) -> {lt}
       | (_,{StringKey=_}) -> {gt}
       | ({IntKey=_},_) -> {lt}
       | (_,{IntKey=_}) -> {gt})

  DbtyMap = Map_make(((Order.make(order_path):order(MongoDb.path,Order.default))))

  get_path = (%% Mongolink.get_path %% : ref_path('a) -> MongoDb.path)

  empty_schema = ([]:MongoDb.schema)

  id = Mutable.make(0)

  getid() =
    myid = id.get()
    do id.set(succ(myid))
    myid

  dbkey = Mutable.make({IntKey=0})

  incpe(pe) =
    match pe with
    | {IntKey=n} -> {IntKey=(n+1)}
    | _ -> pe

  getdbkey() =
    mykey = dbkey.get()
    do dbkey.set(incpe(mykey))
    mykey

  schema = Mutable.make(empty_schema)

  getschema() = schema.get()

  sort_schema() =
    s = schema.get()
    s = List.sort_by((n -> n.name),s)
    s = List.mapi((i, n -> {n with node={IntKey=i}}),s)
    schema.set(s)

  dm = Mutable.make((DbtyMap.empty:dm))

  strip_ref(ty:OpaType.ty): OpaType.ty =
    match ty with
    | {TyName_args=[ty]; TyName_ident="ref_path"} -> ty
    | {TyName_args=[ty]; TyName_ident="val_path"} -> ty
    | {TyName_args=[_,ty]; TyName_ident="path_t"} -> ty
    | ty -> /*do println("ty={ty}")*/ ty

  @private
  bsn() =
    [{node={IntKey=0}; name=""; id=getid(); typ="RECORD";
      zubnodes=[{node={IntKey=0}; name="false"; id=getid(); typ="RECORD"; zubnodes=[]}]},
     {node={IntKey=1}; name=""; id=getid(); typ="RECORD";
      zubnodes=[{node={IntKey=0}; name="true"; id=getid(); typ="RECORD"; zubnodes=[]}]}]

  @private
  lst(node,typ,zubnodes) =
    lid = getid()
    [{~node; name="*"; id=lid; typ="SUM";
      zubnodes=[{node={IntKey=0}; name=""; id=getid(); typ="RECORD";
                zubnodes=[{node={IntKey=0}; name="hd"; id=getid(); ~typ; ~zubnodes},
                          {node={IntKey=1}; name="tl"; id=lid; typ="link"; zubnodes=[]}
                         ]},
                {node={IntKey=1}; name=""; id=getid(); typ="RECORD";
                zubnodes=[{node={IntKey=0}; name="nil"; id=getid(); typ="RECORD"; zubnodes=[]}
                         ]}
               ]}
    ]

  @private
  osn(typ,zubnodes) =
    [{node={IntKey=0}; name=""; id=getid(); typ="RECORD";
      zubnodes=[{node={IntKey=0}; name="none"; id=getid(); typ="RECORD"; zubnodes=[]}]},
     {node={IntKey=1}; name=""; id=getid(); typ="RECORD";
      zubnodes=[{node={IntKey=0}; name="some"; id=getid(); ~typ; ~zubnodes}]}]

  ptm(tm:tm): string = TypeMap.fold((ty:OpaType.ty, id, s -> ("{OpaType.to_pretty(ty)} -> {id}\n"^s)),tm,"")
  prm(rm:rm): string = IntMap.fold((id:int, isrec, s -> ("{id} -> {isrec}\n"^s)),rm,"")

  schema_of_type(dbname:string, ty:OpaType.ty): MongoDb.schema =

    norec = (IntMap.empty:rm)

    rec aux(depth:int, name:string, ty:OpaType.ty, n:MongoDb.key, tm:tm): (MongoDb.key, MongoDb.schema, rm) =
      //do println("schema_of_type: name={name}  ty={OpaType.to_pretty(ty)} n={n}")
      do if depth > 100 then @fail else void
      myid = getid()
      //do println("myid={myid}")
      ret(n,schema,rm) =
        //do println("ret: n={n}  schema={schema}")
        //do println("rm({myid})={prm(rm)}")
        match IntMap.get(myid-1,rm) with
        | {some=({true}:bool)} ->
          //do println("recursion point at {myid}")
          (match schema with
           | [node] ->
             (n,
              ([{node=node.node; name=node.name; id=node.id; typ="RECURSIVE";
                 zubnodes=([{node=n; name="*"; id=myid-1; typ=node.typ;
                             zubnodes=node.zubnodes}]:MongoDb.schema)}]:MongoDb.schema),
              IntMap.remove(myid-1,rm))
           | _ -> @fail)
        | _ ->
          (n,schema,rm)
      //do println("getting id_opt: ty={OpaType.to_pretty(ty)} tm={ptm(tm)}")
      //do println("id_opt={TypeMap.get(ty,tm)}")
      match TypeMap.get(ty,tm) with
      | {some=id} ->
         //do println("recursive at {id}")
         ret(incpe(n),[{node=n; ~name; ~id; typ="link"; zubnodes=[]}],IntMap.singleton(id,{true}))
      | {none} ->
        (tm = if depth == 1 then TypeMap.add(ty,myid,tm) else tm
         //do println("depth={depth} tm={ptm(tm)}")
         match ty with
         | {TyName_args=[]; TyName_ident="void"} -> ret(incpe(n),[{node=n; ~name; id=myid; typ="RECORD"; zubnodes=[]}],norec)
         | {TyConst={TyInt={}}} -> ret(incpe(n),[{node=n; ~name; id=myid; typ="int"; zubnodes=[]}],norec)
         | {TyConst={TyString={}}} -> ret(incpe(n),[{node=n; ~name; id=myid; typ="text"; zubnodes=[]}],norec)
         | {TyConst={TyFloat={}}} -> ret(incpe(n),[{node=n; ~name; id=myid; typ="float"; zubnodes=[]}],norec)
         | {TyName_args=[]; TyName_ident="bool"} -> ret(incpe(n),[{node=n; ~name; id=myid; typ="SUM"; zubnodes=bsn()}],norec)
         | {TyName_args=[ty1]; TyName_ident="intmap"} ->
            (_,st,rm) = aux(depth+1,"",ty1,{StringKey="int"},tm)
            ret(incpe(n),[{node=n; ~name; id=myid; typ="SET"; zubnodes=st}],rm)
         | {TyName_args=[ty1]; TyName_ident="stringmap"} ->
            (_,st,rm) = aux(depth+1,"",ty1,{StringKey="string"},tm)
            ret(incpe(n),[{node=n; ~name; id=myid; typ="SET"; zubnodes=st}],rm)
         | {TyName_args=[ty1,ty2]; TyName_ident="map"} ->
            (_,st,rm) = aux(depth+1,"",ty2,{StringKey="{OpaType.to_pretty(ty1)}"},tm)
            ret(incpe(n),[{node=n; ~name; id=myid; typ="SET"; zubnodes=st}],rm)
         | {TyRecord_row=row}
         | {TyRecord_row=row; TyRecord_rowvar=_} ->
           //do println("row={OpaType.to_pretty(ty)}")
           (_,s,rm) =
             List.fold_left(((n, s, rm1), r ->
                             (nn,ss,rm2) = aux(depth+1,r.label,r.ty,n,tm)
                             rm = IntMap.union(rm1,rm2)
                             (nn,List.flatten([s,ss]),rm)),({IntKey=0}, empty_schema, norec),row)
           ret(incpe(n),[{node=n; ~name; id=myid; typ="RECORD"; zubnodes=s}],rm)
         | {TySum_col = col}
         | {TySum_col = col; TySum_colvar = _} ->
           (_,s,rm) =
             List.fold_left(((n, s, rm1), c ->
                             myid = getid()
                             (_,ss,rm2) =
                               List.fold_left(((n, s, rm1), r ->
                                               (nn,ss,rm2) = aux(depth+1,r.label,r.ty,n,tm)
                                               rm = IntMap.union(rm1,rm2)
                                               (nn,List.flatten([s,ss]),rm)),
                                              ({IntKey=0}, empty_schema, norec), c)
                             rm = IntMap.union(rm1,rm2)
                             ss = [{node=n; name=""; id=myid; typ="RECORD"; zubnodes=ss}]
                             (incpe(n),List.flatten([s,ss]),rm)),
                            ({IntKey=0}, empty_schema, norec), col)
           ret(incpe(n),[{node=n; ~name; id=myid; typ="SUM"; zubnodes=s}],rm)
         | {TyName_args=[lty]; TyName_ident="list"} ->
           (_,st,rm) = aux(depth+1,"",lty,n,tm)
           (match st with
            | [node] ->
              ret(incpe(n),[{node=n; ~name; id=myid; typ="RECURSIVE"; zubnodes=lst(node.node,node.typ,node.zubnodes)}],rm)
            | _ -> @fail)
         | {TyName_args = tys; TyName_ident = tyid} ->
           aux(depth+1,name, OpaType.type_of_name(tyid, tys), n, tm)
         | _ -> @fail("Schema.schema_of_type: Can't deconstruct {/*OpaType.to_pretty*/(ty)}"))

   (_,schema,_) = aux(1,dbname,ty,getdbkey(),TypeMap.empty)
   schema

  dbpath(pth:MongoDb.path): MongoDb.path = List.take(2,pth)

  add_db_to_schema(dbname:string, path:ref_path('a)): void =
    pth = dbpath(get_path(path))
    ty = strip_ref(@typeof(path))
    mydm = dm.get()
    do dm.set(DbtyMap.add(pth,ty,mydm))
    //do println("add_db_to_schema: pth={pth} ty={OpaType.to_pretty(ty)}")
    old_schema = schema.get()
    schema.set(List.flatten([schema_of_type(dbname,ty), old_schema]))

  get_db_type(pth:MongoDb.path): OpaType.ty =
    match DbtyMap.get(dbpath(pth),dm.get()) with
    | {some=ty} ->
      //do println("get_db_type: pth={pth} ty={OpaType.to_pretty(ty)}")
      ty
    | {none} ->
      //do println("get_db_type: pth={pth} @fail")
      @fail

  find_node(key:MongoDb.key, schema:MongoDb.schema): MongoDb.node =
    match List.find((n -> n.node == key),schema) with
    | {some=node} -> node
    | {none} -> @fail("Schema.find_node: can't find key {key} in schema {schema}")

  find_db_schema(path:ref_path('a), schema:MongoDb.schema): (MongoDb.schema, MongoDb.path) =
    match get_path(path) with
    | [{IntKey=1} | [dbkey | l]] ->
        node = find_node(dbkey, schema)
        //do println("find_db_schema: dbkey={dbkey} l={l} node={node}")
        (node.zubnodes, l)
    | _ -> @fail("Schema.find_db_schema: can't find db schema")

  string_of_node0(node:MongoDb.node, tab:string): string =
    nn = match node.node with | {IntKey=i} -> "{i}" | {StringKey=s} -> "{s}" | {AbstractKey=a} -> "{Bson.string_of_bson(a)}"
    id =
      if node.name == "*"
      then "*"
      else
        if node.name == ""
        then "{nn}"
        else "{nn}-{node.name}"
    ty = "({node.id}-{node.typ})"
    sn = if node.zubnodes == [] then "" else "\n{tab}{string_of_schema0(node.zubnodes,(tab^" "))}"
    "[{id}]-{ty}{sn}"

  string_of_node(node:MongoDb.node): string = string_of_node0(node, "")

  string_of_schema0(schema:MongoDb.schema, tab:string): string =
    "["^(String.concat(",\n{tab}",List.map((n -> string_of_node0(n,tab^"  ")),schema)))^"]"

  string_of_schema(schema:MongoDb.schema): string = string_of_schema0(schema, "  ")

}}

MongoDb = {{

  keyname = "key"
  idxname = "index"
  valname = "value"

  path_to_string = (%% Mongolink.path_to_string %% : ref_path('a) -> string)
  path_to_bson = (%% Mongolink.path_to_bson %% : ref_path('a) -> Bson.document)
  path_to_mongo = (%% Mongolink.path_to_mongo %% : ref_path('a) -> (string, string, string))
  path_length = (%% Mongolink.path_length %% : ref_path('a) -> int)

  /* Temporary, we need to extract this from some new db syntax:
   * db[mongo:localhost:27017] /path/xyz
   * or something like that.
   */
  mongo =
    mongo = Mongo.open(50*1024,"www.localhost.local",27017)
    do System.at_exit( ->
                        do println("closing mongo")
                        Mongo.close(mongo))
    mongo

  rec opa_to_bson(key:string, v:'a, ty_opt:option(OpaType.ty)): Bson.document =

    rec rec_to_bson(v:'a, fields:OpaType.fields): Bson.document =
      List.flatten(OpaValue.Record.fold_with_fields(
                                     (field, tyfield, value, bson ->
                                       name = OpaValue.Record.name_of_field_unsafe(field)
                                       //do println("  tyfield={OpaType.to_pretty(tyfield)} name={name}")
                                       res = opa_to_document(name, value, tyfield)
                                       //do println("rec_to_bson({name}): res={Bson.string_of_bson(res)}")
                                       [res | bson]
                                      ),
                                     v, fields, []))

    and list_to_bson(key:string, v:'a, ty:OpaType.ty): Bson.document =
      //do println("list_to_bson\n  key={key}\n  ty={OpaType.to_pretty(ty)})")
      doc = List.flatten(List.fold_index((i, v, acc ->
                                           (doc = opa_to_document("{i}", v, ty)
                                            //do println("list_to_bson: doc={/*Bson.string_of_bson*/(doc)}")
                                            ((doc +> acc):list(Bson.document)))), @unsafe_cast(v), []))
      res = [H.arr(key,doc)]
      //do println("list_to_bson: res = {Bson.string_of_bson(res)}")
      res

    and opa_to_document(key:string, v:'a, ty:OpaType.ty): Bson.document =
      //do println("opa_to_document: key={key} ty={ty}")
      ret(_n,bson) = /*do println("ret{n}({key},{Bson.string_of_bson(bson)})")*/ (bson:Bson.document)
      match ty with
      | {TyName_args=[]; TyName_ident="void"} -> ret(1,[H.null(key)])
      | {TyConst={TyInt={}}} -> ret(2,[H.i64(key,(@unsafe_cast(v):int))])
      | {TyConst={TyString={}}} -> ret(3,[H.str(key,(@unsafe_cast(v):string))])
      | {TyConst={TyFloat={}}} -> ret(4,[H.dbl(key,(@unsafe_cast(v):float))])
      | {TyName_args=[]; TyName_ident="bool"} -> ret(5,[H.bool(key,(@unsafe_cast(v):bool))])
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
        //do println("opa_to_document: row={OpaType.to_pretty(ty)}")
        (match row with
         | [] -> [H.null(key)]
         | [{label=name; ty=ty}] ->
           if OpaType.is_void(ty)
           then ret(6,[H.doc(key,[H.null(name)])])
           else ret(7,[H.doc(key,rec_to_bson(v, row))])
         | _ ->
           ret(8,[H.doc(key,rec_to_bson(v, row))])) // Check this!!! (doesn't work with dbMongo)
           //ret(8,rec_to_bson(v, row))) // <-- is this right???
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        if List.mem([{label="false"; ty={TyRecord_row=[]}}],col) // <-- ? ! :-(
        then ret(9,[H.bool(key,(@unsafe_cast(v):bool))])
        else
          //do println("opa_to_document: col={OpaType.to_pretty(ty)}")
          fields = OpaType.fields_of_fields_list(v, col).f1
          //do println("fields={fields}")
          ret(10,[H.doc(key,rec_to_bson(v, fields))])
      | {TyName_args=[{TyName_args=[]; TyName_ident="Bson.element"}]; TyName_ident="list"}
      | {TyName_ident="Bson.document"; TyName_args=_} ->
        ret(12,[H.doc(key,@unsafe_cast(v))])
      | {TyName_args=[lty]; TyName_ident="list"} ->
        ret(11,list_to_bson(key, @unsafe_cast(v), lty))
      | {TyName_args = tys; TyName_ident = tyid} ->
        opa_to_document(key, v, OpaType.type_of_name(tyid, tys))
      | _ -> @fail("MongoDb.opa_to_bson: unknown value {v} of type {/*OpaType.to_pretty*/(ty)}")

    ty = match ty_opt with {some=ty} -> ty | {none} -> @typeof(v)
    opa_to_document(key, v, ty)

  rec bson_to_opa(bson:Bson.document, ty:OpaType.ty, valname:string): option('a) =
 
    //do println("bson_to_opa:\n  bson={Bson.string_of_bson(bson)}\n  ty={OpaType.to_pretty(ty)}\n  valname={valname}")

    error(str, v) =
      do Log.error("MongoDb.bson_to_opa", str)
      v

    rec element_to_rec(doc:Bson.document, fields:OpaType.fields): option('a) =
      match fields with
      | [{label=name; ty=ty}] ->
        if OpaType.is_void(ty)
        then
          match OpaValue.Record.field_of_name(name) with
          | {none} -> {none}
          | {some=field} -> {some=@unsafe_cast(OpaValue.Record.make_simple_record(field))}
          end
        else
          element_to_rec2(doc,fields)
      | _ ->
        element_to_rec2(doc,fields)

    /* Note: we don't need to sort the fields because MongoDB provides sorted fields, either
     * because it sorts them in standard lexicographic order or because it simply preserves
     * the order of the fields it receives.  It seems to work in any case.
     */
    and element_to_rec2(doc:Bson.document, fields:OpaType.fields): option('a) =
      res =
        List.foldr(
          (element, (acc, fields, err) ->
            name = Bson.key(element)
            //do println("element_to_rec2:\n  element={Bson.string_of_element(element)}\n  name={name}\n  fields={fields}")
            if err
              then (acc, [], err)
            else
              match fields with
              | [] -> error("Type of field {name} not found", (acc, [], true))
              | [hd | tl] ->
                do if hd.label != name then @fail("MongoDb.bson_to_opa: name mismatch \"{hd.label}\" vs. \"{name}\"")
                (match internal_document_to_opa(name, doc, hd.ty) with
                 | {none} ->
                   error("MongoDb.bson_to_opa: Failed with field {name}, document {doc} and type {OpaType.to_pretty(hd.ty)}",
                         (acc, [], true))
                 | {some=value} ->
                   (match OpaValue.Record.field_of_name(name) with
                    | {none} -> error("Missing field {name}", (acc, [], true))
                    | {some=field} -> (OpaValue.Record.add_field(acc, field, value), tl, err)))),
          List.rev(Bson.sort_document(doc)), (OpaValue.Record.empty_constructor(), fields, false))
      if res.f3
      then
        do Log.error("MongoDb.bson_to_opa: Failed with fields {OpaType.to_pretty_fields(fields)}", doc)
        {none}
      else
        {some=@unsafe_cast(OpaValue.Record.make_record(res.f1))}

    and element_to_opa(element:Bson.element, ty:OpaType.ty): option('a) =
      match ty with
      | {TyName_args=[({TyName_args=[]; TyName_ident="Bson.element"}:OpaType.ty)]; TyName_ident="list"}
      | {TyName_args=_; TyName_ident="Bson.document"} ->
        (match element with
         | {value={Document=doc} ...} -> {some=@unsafe_cast(doc)}
         | element -> @fail("MongoDb.bson_to_opa: expected Bson.document, got {element}"))
      | {TyName_args=[]; TyName_ident="void"} ->
        (match element with
         | {value={Null=_} ...} -> {some=@unsafe_cast(void)}
         | element -> @fail("MongoDb.bson_to_opa: expected void, got {element}"))
      | {TyConst={TyInt={}}} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(if tf then 1 else 0)}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(i)}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(i)}
         | {value={Double=d} ...} -> {some=@unsafe_cast(Float.to_int(d))}
         | {value={String=s} ...} -> {some=@unsafe_cast(Int.of_string(s))}
         | element -> @fail("MongoDb.bson_to_opa: expected int, got {element}"))
      | {TyConst={TyString={}}} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(Bool.to_string(tf))}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(Int.to_string(i))}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(Int.to_string(i))}
         | {value={Double=d} ...} -> {some=@unsafe_cast(Float.to_string(d))}
         | {value={String=s} ...} -> {some=@unsafe_cast(s)}
         | element -> @fail("MongoDb.bson_to_opa: expected string, got {element}"))
      | {TyConst={TyFloat={}}} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(if tf then 1.0 else 0.0)}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(Float.of_int(i))}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(Float.of_int(i))}
         | {value={Double=d} ...} -> {some=@unsafe_cast(d)}
         | {value={String=s} ...} -> {some=@unsafe_cast(Float.of_string(s))}
         | element -> @fail("MongoDb.bson_to_opa: expected float, got {element}"))
      | {TyName_args=[]; TyName_ident="bool"} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(tf)}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(i != 0)}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(i != 0)}
         | {value={Double=d} ...} -> {some=@unsafe_cast(d != 0.0)}
         | {value={String="true"} ...} -> {some=@unsafe_cast(true)}
         | {value={String="false"} ...} -> {some=@unsafe_cast(false)}
         | element -> @fail("MongoDb.bson_to_opa: expected bool, got {element}"))
      | {TyName_args = [ty]; TyName_ident = "option"} ->
        (match element with
         | {name=_key; value={Document=doc}} ->
           //do println("ty={OpaType.to_pretty(ty)} key={_key} doc={doc}")
           (match Bson.find_elements(doc,["some","none"]) with
            | {some=("some",element)} ->
              (match element_to_opa(element, ty) with
              | {some=v} -> {some=@unsafe_cast({some=v})}
              | {none} -> {none})
            | {some=("none",_)} -> {some=@unsafe_cast({none})}
            | _ -> {none})
         | element -> @fail("MongoDb.bson_to_opa: expected option, got {element}"))
      | {TyName_args = [ty]; TyName_ident = "list"} ->
        (match element with
         | {name=_key; value={Array=doc}} ->
           //do println("ty={OpaType.to_pretty(ty)} key={_key} doc={doc}")
           len = List.length(doc) - 1
           l = List.fold_index(
                 (i, element, l ->
                    do if "{len-i}" != Bson.key(element)
                       then @fail("MongoDb.bson_to_opa: Array to list index mismatch {doc}")
                    match element_to_opa(element, ty) with
                    | {some=v} -> [v | l]
                    | {none} -> @fail("MongoDb.bson_to_opa: failed for list element {element} type {OpaType.to_pretty(ty)}")),
                 doc,[])
           {some=@unsafe_cast(l)}
         | element -> @fail("MongoDb.bson_to_opa: expected list, got {element}"))
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
        //do println("MongoDb.bson_to_opa: row={OpaType.to_pretty(ty)}")
        (match element with
         | {value={Document=doc} ...} ->
           doc = Bson.remove_id(doc)
           //do println("  doc={doc}\n  keys={Bson.keys(doc)}")
           element_to_rec(doc, row)
         | _ -> element_to_rec([element],row))
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        //do println("element={element} col={OpaType.to_pretty(ty)}")
        (ltyfield, doc) =
          (match element with
           | {value={Document=doc} ...}->
             (List.sort(Bson.keys(doc)), doc) // <-- We might get away with List.rev here???
           | _ ->
             ([Bson.key(element)], [element]))
        //do println("doc={doc} keys={Bson.keys(doc)}")
        (match OpaSerialize.fields_of_fields_list2(ltyfield, col) with
         | {some=fields} ->
           //do println("fields={fields}")
           element_to_rec(doc, fields)
         | {none} -> @fail("Fields ({OpaType.to_pretty_lfields(col)}) not found in sum type ({List.to_string(ltyfield)})"))
      | {TyName_args = tys; TyName_ident = tyid} ->
        element_to_opa(element, OpaType.type_of_name(tyid, tys))
      | _ -> @fail("MongoDb.bson_to_opa: unknown type {OpaType.to_pretty(ty)}")

    and internal_document_to_opa(key:string, doc:Bson.document, ty:OpaType.ty): option('a) =
      match Bson.find_element(doc,key) with
      | {some=element} -> element_to_opa(element, ty)
      | {none} -> {none}

    bson_noid = Bson.remove_id(bson)
    match bson_noid with
    | [element] ->
       //do println("  element={Bson.string_of_element(element)}\n  ty={OpaType.to_pretty(ty)}")
       element_to_opa(element, ty)
    | bson ->
       (match Bson.find_element(bson,valname) with
        | {some=element} -> element_to_opa(element, ty)
        | {none} -> element_to_opa(H.doc(valname,bson_noid), ty)) // assume bare record

  index(indices:list('a)): Bson.document =
    List.flatten(List.mapi((n, index -> opa_to_bson((idxname^(Int.to_string(n))), index, {none})),indices))

  find_field(row:OpaType.fields, name:string): option(OpaType.field) =
    List.find((r -> match r with | {~label; ty=_} -> label == name),row)

  db_coll(pth:MongoDb.path): (string, string) =
    match pth with
    | [] -> @fail("MongoDb.db_coll: empty path")
    | [{IntKey=i}|[]] -> ("db_{i}", "collection")
    | [{IntKey=i}|[{IntKey=j}|[]]] -> ("db_{i}_{j}", "collection")
    | [{IntKey=i}|[{IntKey=j}|[{IntKey=k}|_]]] -> ("db_{i}_{j}", "c_{k}")
    | _ -> @fail("MongoDb.db_coll: bad path {pth}")

  real_type(path:ref_path('b)): (Bson.document,OpaType.ty,OpaType.ty,string,string) =
    (schema, pth) = Schema.find_db_schema(path, Schema.getschema())
    //do println("\nreal_type: pth={pth}")
    pthlen = List.length(pth)
    full_path = Schema.get_path(path)
    (dbname, collname) = db_coll(full_path)
    //do println("real_type: full_path={full_path}")
    rec aux(schema:MongoDb.schema, idx:int, pth:MongoDb.path, ty:OpaType.ty, ty2:OpaType.ty, select:Bson.document) =
      //do println("pth={pth}\nty={OpaType.to_pretty(ty)}\nty2={OpaType.to_pretty(ty2)}")
      //do println("schema:\n  {Schema.string_of_schema(schema)}")
      if pth == []
      then (select,ty,ty2)
      else
        match ty with
          | {TyName_args=[]; TyName_ident="void"}
          | {TyConst={TyInt={}}}
          | {TyConst={TyString={}}}
          | {TyConst={TyFloat={}}}
          | {TyName_args=[]; TyName_ident="bool"} ->
            @fail("MongoDb.real_type: Can't deconstruct {OpaType.to_pretty(ty)}")
          | {TyName_args=[{TyConst={TyInt={}}},ty]; TyName_ident="map"}
          | {TyName_args=[ty]; TyName_ident="intmap"} ->
            //do println("  hd(pth)={List.head(pth)}")
            (match List.head(pth) with
             | {IntKey=i} ->
               (match schema with
                | [node] ->
                  (select,ty,ty2) = aux(node.zubnodes,idx+1,List.tail(pth),ty,ty,select)
                  ([H.i64("index{idx}",i)|select],ty,ty2)
                | _ -> @fail("MongoDb.real_type: bad schema"))
             | k -> @fail("MongoDb.real_type: index missing for intmap {k}"))
          | {TyName_args=[{TyConst={TyString={}}},ty]; TyName_ident="map"}
          | {TyName_args=[ty]; TyName_ident="stringmap"} ->
            //do println("  hd(pth)={List.head(pth)}")
            (match List.head(pth) with
             | {StringKey=s} ->
               (match schema with
                | [node] ->
                  (select,ty,ty2) = aux(node.zubnodes,idx+1,List.tail(pth),ty,ty,select)
                  ([H.str("index{idx}",s)|select],ty,ty2)
                | _ -> @fail("MongoDb.real_type: bad schema"))
             | k -> @fail("MongoDb.real_type: index missing for stringmap {k}"))
          /* We need to wait for the new syntax to use this...
          | {TyName_args=[tya,ty]; TyName_ident="map"} ->
            //do println("  hd(pth)={List.head(pth)}")
            (match List.head(pth) with
             | {AbstractKey=a} ->
               (match schema with
                | [node] ->
                  (select,ty,ty2) = aux(node.zubnodes,idx+1,List.tail(pth),ty,ty,select)
                  ([opa_to_bson("index{idx}",Magic.id(a),{none})|select],ty,ty2)
                | _ -> @fail("MongoDb.real_type: bad schema"))
             | k -> @fail("MongoDb.real_type: index missing for stringmap {k}"))
          */
          | {TyRecord_row=row}
          | {TyRecord_row=row; TyRecord_rowvar=_} ->
            //do println("\nrow={OpaType.to_pretty(ty)}")
            //do println("  head(pth)={List.head(pth)}")
            node = Schema.find_node(List.head(pth),schema)
            //do println("  node.name={node.name}")
            //do println("  subnode(row)=\n    {Schema.string_of_node(node)}")
            (match find_field(row,node.name) with
             | {some={~label; ty=ty2}} ->
               ty3 = {TyRecord_row=[{~label; ty=ty2}]}
               //do println("{if List.length(pth) <= 1 then "*" else "-"} k={node.name}")
               //do println("  pth={pth}\n  ty={OpaType.to_pretty(ty)}")
               //do println("  ty2={OpaType.to_pretty(ty2)}\n  ty3={OpaType.to_pretty(ty3)}")
               (select,ty,ty2) = aux(node.zubnodes,idx,List.tail(pth),ty2,ty,select)
               select = if select == [] && pthlen <= 1 then [H.str("key",label)] else select
               if List.length(pth) <= 1
               then (select,ty3,ty2)
               else ([H.str("key",label)|select],ty,ty2)
             | {none} -> @fail)
          | {TySum_col = _col}
          | {TySum_col = _col; TySum_colvar = _} ->
            ([H.str("key","SUM")],ty,ty2) // <-- TODO: Check if this is right.
          | {TyName_args=[{TyName_args=[]; TyName_ident="Bson.element"}]; TyName_ident="list"}
          | {TyName_args=_; TyName_ident="Bson.document"} ->
            (select,ty,ty2)
          | {TyName_args=[_]; TyName_ident="list"} ->
            (select,ty,ty2)
          | {TyName_args = tys; TyName_ident = tyid} ->
            aux(schema, idx, pth, OpaType.type_of_name(tyid, tys), OpaType.type_of_name(tyid, tys), select)
          | _ -> @fail("MongoDb.real_type: unknown type {OpaType.to_pretty(ty)}")

    ty = Schema.get_db_type(full_path)
    if pth == []
    then ([H.str(keyname,"key")],ty,ty,dbname,collname)
    else (select,ty,ty2) = aux(schema,0,pth,ty,ty,[])
         select = if select == [] then [H.str(keyname,"key")] else select
         (select,ty,ty2,dbname,collname)

  unfactor_types(ty:OpaType.ty): Bson.document =
    //do println("unfactor_types:\n  ty={ty}")
    ret(doc) = /*do println("unfactor_types: result={Bson.string_of_bson(doc)}")*/ doc
    match ty with
    | {TyRecord_row=row}
    | {TyRecord_row=row; TyRecord_rowvar=_} -> ret(List.flatten(List.map((r -> [H.i32(r.label,1)]),row)))
    | _ -> ret([H.i32(valname,1)])

  read(path:ref_path('a)): 'a =
    (select,ty,_,db,collection) = real_type(path)
    ns = "{db}.{collection}"
    /*do println("read:\n  ns={ns}\n  select={select}")
    do println("  ty={OpaType.to_pretty(ty)}")
    do println("  ty2={OpaType.to_pretty(ty2)}")
    do println("  ty==ty2: {ty==ty2}")*/
    (match @typeof(path) with
     | {TyName_args=[pty]; TyName_ident="ref_path"}
     | {TyName_args=[pty]; TyName_ident="val_path"} ->
       //key =
       //  match Bson.find_element(select,"key") with
       //  | {some={name="key"; value={String=key}}} -> key
       //  | _ -> @fail
       //do println("MongoDb.read: {ns}({key}) pty={OpaType.to_pretty(pty)}")
       (match Cursor.find_one(mongo,ns,select,{some=unfactor_types(ty)}) with
        | {success=doc} ->
          //do println("doc={doc}")
          (match bson_to_opa(doc, pty, valname) with
           | {some=v} -> v
           | {none} -> @fail("MongoDb.read: not found"))
        | {~failure} -> @fail("MongoDb.read: error from MongoDB: {failure}"))
     | ty -> @fail("MongoDb.read: unknown db value {path} of path type {OpaType.to_pretty(ty)}"))

  factor_types(ty:OpaType.ty, ty2:OpaType.ty, v:'a): Bson.document =
    v_ty = @typeof(v)
    if ty == ty2
    then opa_to_bson(valname,v,{some=v_ty})
    else
      match ty with
      | {TyRecord_row=[~{label; ty=_}]}
      | {TyRecord_row=[~{label; ty=_}]; TyRecord_rowvar=_} -> opa_to_bson(label,v,{some=v_ty})
      | {TyName_args=[_lty]; TyName_ident="list"} -> opa_to_bson(valname,v,{some=v_ty})
      | _ -> @fail("MongoDb.factor_types:  mis-matching types {OpaType.to_pretty(ty)} and {OpaType.to_pretty(v_ty)}")

  write(path:ref_path('a), v:'a) =
    (select,ty,ty2,db,collection) = real_type(path)
    ns = "{db}.{collection}"
    /*do println("v={v}\n  ns={ns}\n  select={select}")
    do println("  ty={OpaType.to_pretty(ty)}")
    do println("  ty2={OpaType.to_pretty(ty2)}")
    do println("  typeof(v)={OpaType.to_pretty(@typeof(v))}")
    do println("  ty==ty2: {ty==ty2}")*/
    update = [H.doc("$set",factor_types(ty,ty2,v))]
    //do println("MongoDb.write: update={Bson.string_of_bson(update)}")
    key =
      match Bson.find_element(select,"key") with
      | {some={name="key"; value={String=key}}} -> key
      | _ -> @fail
    do println("MongoDb.write: {db}.{collection}({key})")
    do if Mongo.update(mongo,Mongo.UpsertBit,ns,select,update)
       then println("err={Bson.string_of_result(Cursor.last_error(mongo, db))}")
       else println("update failure")
    void

  /*`<-`(t,a) = write(t,a)*/

  create_index(path:ref_path('a), index:Bson.document, flags:int): Mongo.result =
    (db, collection, _) = path_to_mongo(path)
    ns = "db{db}.c{collection}"
    if Indexes.create_index(mongo,ns,index,flags)
    then Cursor.last_error(mongo, db)
    else {failure={Error="command send failure"}}

  create_standard_index(path:ref_path('a)): Mongo.result =
    (select,_,_,_,_) = real_type(path)
    index = List.map((e -> H.i32(Bson.key(e),1)),select)
    create_index(path,index,(Indexes.SparseBit+Indexes.UniqueBit))

}}

/* Test code */

/*
type rtype = { rtInt:int } / { rtString:string } / { rtFloat:float } / { rtNull:void }
type stype = { stInt:int; stString:string; stFloat:float; stNull:void }
type abc = {a:int; b:int; c:int} / {d:int; e:int; f:int} / {g:int; h:int; i:int}
type rct = {x:int; y:rct} / {z:void}

db /path/i: int
db /path/s: string
db /path/f: float
db /path/b: bool = {false}
db /path/ot: option(int)
db /path/nt: option(int)
db /path/rt: rtype = { rtNull=void }
db /path/st: stype = { stInt=0; stString=""; stFloat=0.0; stNull=void }
db /path/lt: list(int)
db /path/tt: (string, int)
db /bd: Bson.document = [H.null("null")]
db /bd/hd = H.null("null")
db /bd/hd/value = { Null }
db /bd/hd/value/Boolean = { false }
db /path2/p/v: void
db /path2/p/i: int
db /i0: int
db /ism/ism : map(int, string) // Warning: at top level you get an index of collections!
db /ssm/ssm : map(string, string)
//db /bsm/bsm : map(bool, string) // We can't put arbitrary values in the index through dbGen
db /db3/im: intmap({ a: int; b: string })
db /db4/nm: intmap(intmap({ a: int; b: string }))
db /db5/sm: stringmap({ a: int; b: string; c: bool })
db /db5/sm[_]/c = { false }
db /rct: rct

// Things still to try...
// db /opages/pages[_] = Page.empty
// db /z[{a;b}] : { a : int; b : string; c : int }

_ =
  do println("dbMongo")
  do Schema.add_db_to_schema("db3",@/db3)
  do Schema.add_db_to_schema("db4",@/db4)
  do Schema.add_db_to_schema("db5",@/db5)
  do Schema.add_db_to_schema("i0",@/i0)
  do Schema.add_db_to_schema("path",@/path)
  do Schema.add_db_to_schema("path2",@/path2)
  do Schema.add_db_to_schema("rct",@/rct)
  do Schema.add_db_to_schema("bd",@/bd)
  do Schema.add_db_to_schema("ism",@/ism)
  do Schema.add_db_to_schema("ssm",@/ssm)
  //do Schema.add_db_to_schema("bsm",@/bsm)
  do Schema.sort_schema()
  do println("schema=\n{Schema.string_of_schema(Schema.getschema())}")
  do MongoDb.write(@/i0,420)
  do MongoDb.write(@/path/i,42)
  do MongoDb.write(@/path/s,"forty two")
  do MongoDb.write(@/path/f,42.24)
  do MongoDb.write(@/path/b,true)
  do MongoDb.write(@/path/ot,{some=4242})
  do MongoDb.write(@/path/nt,{none})
  do MongoDb.write(@/path/rt,{rtInt=4242})
  do MongoDb.write(@/path/st,{stInt=424242; stString="forty two forty two"; stFloat=42.42; stNull=void})
  do MongoDb.write(@/path/tt,("ghi",789))
  do MongoDb.write(@/path/lt,[42,43])
  do MongoDb.write(@/path2/p/v,void)
  do MongoDb.write(@/path2/p/i,4224)
  do MongoDb.write(@/rct,{z})
  do MongoDb.write(@/rct,{x=1; y={x=2; y={x=3; y={z}}}})
  do MongoDb.write(@/bd,([H.i32("int32",2424)]:Bson.document))
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/ism/ism[0]))}")
  do MongoDb.write(@/ism/ism[3],"three")
  do MongoDb.write(@/ism/ism[5],"five")
  do MongoDb.write(@/ism/ism[7],"seven")
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/ssm/ssm[""]))}")
  do MongoDb.write(@/ssm/ssm["cat"],"moggy")
  do MongoDb.write(@/ssm/ssm["rat"],"roland")
  do MongoDb.write(@/ssm/ssm["hat"],"fedora")
  //do MongoDb.write(Magic.id(@/bsm/bsm[(Magic.id({true}):map(bool,string))]),"true")
  //do MongoDb.write(@/bsm/bsm[OpaSerialize.String.serialize({false})],"false")
  //do MongoDb.write(@/db3/im,IntMap.empty)
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/db3/im[0]/a))}")
  do MongoDb.write(@/db3/im[12],{a=123; b="abc"})
  do MongoDb.write(@/db3/im[12]/a,456)
  do MongoDb.write(@/db3/im[12]/b,"def")
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/db4/nm[0][0]/a))}")
  do MongoDb.write(@/db4/nm[12][34],{a=123; b="abc"})
  do MongoDb.write(@/db4/nm[12][34]/a,456)
  do MongoDb.write(@/db4/nm[12][34]/b,"def")
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/db5/sm[""]/a))}")
  do MongoDb.write(@/db5/sm["parasaurolophus"],{a=76500000; b="cyrtocristatus"; c={false}})
  do MongoDb.write(@/db5/sm["parasaurolophus"]/a,73000000)
  do MongoDb.write(@/db5/sm["parasaurolophus"]/c,{true})
  do println("i0={(MongoDb.read(@/i0):int)}")
  do println("i={(MongoDb.read(@/path/i):int)}")
  do MongoDb.write(@/path/i,43)
  do println("i={(MongoDb.read(@/path/i):int)}")
  do println("s={(MongoDb.read(@/path/s):string)}")
  do println("f={(MongoDb.read(@/path/f):float)}")
  do println("b={(MongoDb.read(@/path/b):bool)}")
  do println("ot={(MongoDb.read(@/path/ot):option(int))}")
  do println("nt={(MongoDb.read(@/path/nt):option(int))}")
  do println("rt={(MongoDb.read(@/path/rt):rtype)}")
  do println("st={(MongoDb.read(@/path/st):stype)}")
  do println("tt={(MongoDb.read(@/path/tt):(string,int))}")
  do println("lt={(MongoDb.read(@/path/lt):list(int))}")
  do println("p/v={(MongoDb.read(@/path2/p/v):void)}")
  do println("p/i={(MongoDb.read(@/path2/p/i):int)}")
  do println("rct={(MongoDb.read(@/rct):rct)}")
  do println("bd={(MongoDb.read(@/bd):Bson.document)}")
  do println("ism[3]={(MongoDb.read(@/ism/ism[3]):string)}")
  do println("ism[5]={(MongoDb.read(@/ism/ism[5]):string)}")
  do println("ism[7]={(MongoDb.read(@/ism/ism[7]):string)}")
  do println("ssm[cat]={(MongoDb.read(@/ssm/ssm["cat"]):string)}")
  do println("ssm[rat]={(MongoDb.read(@/ssm/ssm["rat"]):string)}")
  do println("ssm[hat]={(MongoDb.read(@/ssm/ssm["hat"]):string)}")
  //do println("bsm[true]={(MongoDb.read(@/bsm/bsm[{true}]):string)}")
  //do println("bsm[false]={(MongoDb.read(@/bsm/bsm[{false}]):string)}")
  do println("db3/im[12]={(MongoDb.read(@/db3/im[12]):{a:int; b:string})}")
  do println("db3/im[12]/a={(MongoDb.read(@/db3/im[12]/a):int)}")
  do println("db3/im[12]/b={(MongoDb.read(@/db3/im[12]/b):string)}")
  do println("db4/nm[12][34]={(MongoDb.read(@/db4/nm[12][34]):{a:int; b:string})}")
  do println("db4/nm[12][34]/a={(MongoDb.read(@/db4/nm[12][34]/a):int)}")
  do println("db4/nm[12][34]/b={(MongoDb.read(@/db4/nm[12][34]/b):string)}")
  do println("db5/sm[\"parasaurolophus\"]={(MongoDb.read(@/db5/sm["parasaurolophus"]):{a:int; b:string; c:bool})}")
  do println("db5/sm[\"parasaurolophus\"]/a={(MongoDb.read(@/db5/sm["parasaurolophus"]/a):int)}")
  do println("db5/sm[\"parasaurolophus\"]/c={(MongoDb.read(@/db5/sm["parasaurolophus"]/c):bool)}")
  void
*/
