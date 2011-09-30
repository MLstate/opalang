import stdlib.system

type path = external

type MongoDb.key = { IntKey: int } / { StringKey: string }
type MongoDb.path = list(MongoDb.key)

/* We need access to this to do the embedding properly */
type MongoDb.node = { name:string; typ:string; id:int; node:MongoDb.key; zubnodes:MongoDb.schema; }
type MongoDb.schema = list(MongoDb.node)

type sm = ordered_map(string, int, String.order)
type rm = ordered_map(int, bool, String.order)

Schema = {{

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
    | {StringKey=_s} -> pe//@fail("StringKey({s}) inc")

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

  strip_ref(ty:OpaType.ty): OpaType.ty =
    match ty with
    | {TyName_args=[ty]; TyName_ident="ref_path"} -> ty
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

  ptm(tm:sm): string = StringMap.fold((ty, id, s -> ("{ty} -> {id}\n"^s)),tm,"")
  prm(rm:rm): string = IntMap.fold((id, isrec, s -> ("{id} -> {isrec}\n"^s)),rm,"")

  schema_of_type(ty:OpaType.ty): MongoDb.schema =

    norec = (IntMap.empty:rm)

    rec aux(depth:int, name:string, ty:OpaType.ty, n:MongoDb.key, tm:sm): (MongoDb.key, MongoDb.schema, rm) =
      //do println("schema_of_type: name={name}  ty={OpaType.to_pretty(ty)} n={n}")
      do if depth > 100 then @fail else void
      myid = getid()
      tykey = OpaType.to_string(ty)
      ret(n,schema,rm) =
        //do println("ret: n={n}  schema={schema}")
        //do println("rm({myid})={prm(rm)}")
        match IntMap.get(myid-1,rm) with
        | {some=({true}:bool)} ->
          do println("recursion point at {myid}")
          (match schema with
           | [node] ->
             (n,
              ([{node=node.node; name=node.name; id=node.id; typ="RECURSIVE";
                 zubnodes=([{node=n; name="*"; id=myid-1; typ=node.typ;
                             zubnodes=node.zubnodes}]:MongoDb.schema)}]:MongoDb.schema),
              IntMap.remove(myid-1,rm))
             //(n,schema,rm)
           | _ -> @fail)
        | _ ->
          (n,schema,rm)
      match StringMap.get(tykey,tm) with
      | {some=id} ->
         do println("recursive at {id}")
         ret(incpe(n),[{node=n; ~name; ~id; typ="link"; zubnodes=[]}],IntMap.singleton(id,{true}))
      | {none} ->
        (tm = StringMap.add(tykey,myid,tm)
         //do println("tm={ptm(tm)}")
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
         | {TyRecord_row=row}
         | {TyRecord_row=row; TyRecord_rowvar=_} ->
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

   dbkey = getdbkey()
   dbnum = match dbkey with | {IntKey=n} -> n | _ -> @fail
   (_,schema,_) = aux(1,"db_1_{dbnum}",strip_ref(ty),dbkey,StringMap.empty)
   schema

  add_db_to_schema(path:ref_path('a)): void =
    old_schema = schema.get()
    schema.set(List.flatten([schema_of_type(@typeof(path)), old_schema]))

  find_node0(key:MongoDb.key, schema:MongoDb.schema): MongoDb.node =
    match List.find((n -> n.node == key),schema) with
    | {some=node} -> node
    | {none} -> @fail("Schema.find_node: can't find key {key} in schema {schema}")

  find_node(key:MongoDb.key, schema:MongoDb.schema): MongoDb.node =
    match schema with
    | [node] ->
      // skip recursion points
      if node.name == "*"
      then find_node(key,node.zubnodes)
      else find_node0(key, schema)
    | _ -> find_node0(key, schema)

  find_db_schema(path:ref_path('a), schema:MongoDb.schema): (MongoDb.schema, MongoDb.path) =
    match get_path(path) with
    | [{IntKey=1} | [dbkey | l]] ->
        node = find_node(dbkey, schema)
        //do println("find_db_schema: dbkey={dbkey} l={l} node={node}")
        (node.zubnodes, l)
    | _ -> @fail("Schema.find_db_schema: can't find db schema")

  string_of_node(node:MongoDb.node, tab:string): string =
    nn = match node.node with | {IntKey=i} -> "{i}" | {StringKey=s} -> "{s}"
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

  string_of_schema0(schema:MongoDb.schema, tab:string): string =
    "["^(String.concat(",\n{tab}",List.map((n -> string_of_node(n,tab^"  ")),schema)))^"]"

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

    rec rec_to_bson(_key:string, v:'a, fields:OpaType.fields): Bson.document =
      List.flatten(OpaValue.Record.fold_with_fields(
                                     (field, tyfield, value, bson ->
                                       name = OpaValue.Record.name_of_field_unsafe(field)
                                       res = opa_to_document(name, value, tyfield)
                                       [res | bson]),
                                     v, fields, []))

    and list_to_bson(key:string, v:'a, ty:OpaType.ty): Bson.document =
      doc = List.flatten(List.fold_index((i, v, acc -> (opa_to_document("{i}", v, ty)) +> acc), @unsafe_cast(v), []))
      [{Array = (key,doc)}]

    and opa_to_document(key:string, v:'a, ty:OpaType.ty): Bson.document =
      //do println("opa_to_document: key={key} ty={ty}")
      match ty with
      | {TyName_args=[]; TyName_ident="void"} -> [{Null=(key,void)}]
      | {TyConst={TyInt={}}} -> [{Int64=(key,(@unsafe_cast(v):int))}]
      | {TyConst={TyString={}}} -> [{String=(key,(@unsafe_cast(v):string))}]
      | {TyConst={TyFloat={}}} -> [{Double=(key,(@unsafe_cast(v):float))}]
      | {TyName_args=[]; TyName_ident="bool"} -> [{Boolean=(key,(@unsafe_cast(v):bool))}]
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
        (match row with
         | [] -> [{Null=(key,void)}]
         | [{label=name; ty=ty}] ->
           if OpaType.is_void(ty)
           then [{Document=(key,[{Null=(name,void)}])}]
           else rec_to_bson(key, v, row)
         | _ ->
           rec_to_bson(key, v, row))
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        if List.mem([{label="false"; ty={TyRecord_row=[]}}],col) // <-- ? ! :-(
        then [{Boolean=(key,(@unsafe_cast(v):bool))}]
        else rec_to_bson(key, v, OpaType.fields_of_fields_list(v, col).f1)
      | {TyName_args=[lty]; TyName_ident="list"} ->
        list_to_bson(key, @unsafe_cast(v), lty)
      | {TyName_ident = "Bson.document"; TyName_args = _} ->
        [{Document=(key,@unsafe_cast(v))}]
      | {TyName_args = tys; TyName_ident = tyid} ->
        opa_to_document(key, v, OpaType.type_of_name(tyid, tys))
      | _ -> @fail("MongoDb.opa_to_bson: unknown value {v} of type {/*OpaType.to_pretty*/(ty)}")

    ty = match ty_opt with {some=ty} -> ty | {none} -> @typeof(v)
    opa_to_document(key, v, ty)

  rec bson_to_opa(bson:Bson.document, ty:OpaType.ty): option('a) =

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
          doc, (OpaValue.Record.empty_constructor(), fields, false))
      if res.f3
      then
        do Log.error("MongoDb.bson_to_opa: Failed with fields {OpaType.to_pretty_fields(fields)}", doc)
        {none}
      else
        {some=@unsafe_cast(OpaValue.Record.make_record(res.f1))}

    and element_to_opa(element:Bson.element, ty:OpaType.ty): option('a) =
      match ty with
      | {TyName_ident = "Bson.document"; TyName_args = _} ->
        (match element with
         | {Document=(_,doc)} -> {some=@unsafe_cast(doc)}
         | element -> @fail("MongoDb.bson_to_opa: expected Bson.document, got {element}"))
      | {TyName_args=[]; TyName_ident="void"} ->
        (match element with
         | {Null=(_,{})} -> {some=@unsafe_cast(void)}
         | element -> @fail("MongoDb.bson_to_opa: expected void, got {element}"))
      | {TyConst={TyInt={}}} ->
        (match element with
         | {Boolean=(_,tf)} -> {some=@unsafe_cast(if tf then 1 else 0)}
         | {Int32=(_,i)} -> {some=@unsafe_cast(i)}
         | {Int64=(_,i)} -> {some=@unsafe_cast(i)}
         | {Double=(_,d)} -> {some=@unsafe_cast(Float.to_int(d))}
         | {String=(_,s)} -> {some=@unsafe_cast(Int.of_string(s))}
         | element -> @fail("MongoDb.bson_to_opa: expected int, got {element}"))
      | {TyConst={TyString={}}} ->
        (match element with
         | {Boolean=(_,tf)} -> {some=@unsafe_cast(Bool.to_string(tf))}
         | {Int32=(_,i)} -> {some=@unsafe_cast(Int.to_string(i))}
         | {Int64=(_,i)} -> {some=@unsafe_cast(Int.to_string(i))}
         | {Double=(_,d)} -> {some=@unsafe_cast(Float.to_string(d))}
         | {String=(_,s)} -> {some=@unsafe_cast(s)}
         | element -> @fail("MongoDb.bson_to_opa: expected string, got {element}"))
      | {TyConst={TyFloat={}}} ->
        (match element with
         | {Boolean=(_,tf)} -> {some=@unsafe_cast(if tf then 1.0 else 0.0)}
         | {Int32=(_,i)} -> {some=@unsafe_cast(Float.of_int(i))}
         | {Int64=(_,i)} -> {some=@unsafe_cast(Float.of_int(i))}
         | {Double=(_,d)} -> {some=@unsafe_cast(d)}
         | {String=(_,s)} -> {some=@unsafe_cast(Float.of_string(s))}
         | element -> @fail("MongoDb.bson_to_opa: expected float, got {element}"))
      | {TyName_args=[]; TyName_ident="bool"} ->
        (match element with
         | {Boolean=(_,tf)} -> {some=@unsafe_cast(tf)}
         | {Int32=(_,i)} -> {some=@unsafe_cast(i != 0)}
         | {Int64=(_,i)} -> {some=@unsafe_cast(i != 0)}
         | {Double=(_,d)} -> {some=@unsafe_cast(d != 0.0)}
         | {String=(_,"true")} -> {some=@unsafe_cast(true)}
         | {String=(_,"false")} -> {some=@unsafe_cast(false)}
         | element -> @fail("MongoDb.bson_to_opa: expected bool, got {element}"))
      | {TyName_args = [ty]; TyName_ident = "option"} ->
        (match element with
         | {Document=(_key,doc)} ->
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
         | {Array=(_key,doc)} ->
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
        (match element with
         | {Document=(_key,doc)} ->
           //do println("key={_key} doc={doc} keys={Bson.keys(doc)}")
           element_to_rec(doc, row)
         | _ -> @fail("MongoDb.bson_to_opa: expected record, got {element}"))
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        //do println("element={element} col={col}")
        (match element with
         | {Document=(_key,doc)} ->
           //do println("key={_key} doc={doc} keys={Bson.keys(doc)}")
           ltyfield = Bson.keys(doc)
           (match OpaSerialize.fields_of_fields_list2(ltyfield, col) with
            | {some=fields} ->
              /*do println("fields={fields}")*/
              element_to_rec(doc, fields)
            | {none} -> @fail("Fields ({OpaType.to_pretty_lfields(col)}) not found in sum type ({List.to_string(ltyfield)})"))
         | _ -> @fail("MongoDb.bson_to_opa: expected record, got {element}"))
      | {TyName_args = tys; TyName_ident = tyid} ->
        element_to_opa(element, OpaType.type_of_name(tyid, tys))
      | _ -> @fail("MongoDb.bson_to_opa: unknown type {OpaType.to_pretty(ty)}")

    and internal_document_to_opa(key:string, doc:Bson.document, ty:OpaType.ty): option('a) =
      match Bson.find_element(doc,key) with
      | {some=element} -> element_to_opa(element, ty)
      | {none} -> {none}

    match Bson.find_element(bson,valname) with
    | {some=element} -> element_to_opa(element, ty)
    | {none} -> {none}

  index(indices:list('a)): Bson.document =
    List.flatten(List.mapi((n, index -> opa_to_bson((idxname^(Int.to_string(n))), index, {none})),indices))

  get_select(key:string, indices:Bson.document): Bson.document =
    match indices with
    | [] -> [{String=(keyname,key)}]
    | _ -> List.flatten([[{String=(keyname,key)}], indices])

  find_field(row:OpaType.fields, name:string): option(OpaType.field) =
    List.find((r -> match r with | {~label; ty=_} -> label == name),row)

  db_coll(pth:MongoDb.path): (string, string) =
    match pth with
    | [] -> @fail("MongoDb.db_coll: empty path")
    | [{IntKey=i}|[]] -> ("db_{i}", "collection")
    | [{IntKey=i}|[{IntKey=j}|[]]] -> ("db_{i}_{j}", "collection")
    | [{IntKey=i}|[{IntKey=j}|[{IntKey=k}|_]]] -> ("db_{i}_{j}", "c_{k}")
    | _ -> @fail("MongoDb.db_coll: bad path {pth}")

  real_type(dbpath:ref_path('a), path:ref_path('b)): (Bson.document,OpaType.ty,OpaType.ty,string,string) =
    (schema, pth) = Schema.find_db_schema(path, Schema.getschema())
    pthlen = List.length(pth)
    full_path = Schema.get_path(path)
    (dbname, collname) = db_coll(full_path)
    rec aux(schema:MongoDb.schema, idx:int, pth:MongoDb.path, ty:OpaType.ty, ty2:OpaType.ty, select:Bson.document) =
      //do println("pth={pth}\nty={OpaType.to_pretty(ty)}\nty2={OpaType.to_pretty(ty2)}")
      //do println("schema: {schema}")
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
          | {TyName_args=[ty]; TyName_ident="intmap"} ->
            //do println("  hd(pth)={List.head(pth)}")
            (match List.head(pth) with
             | {IntKey=i} ->
               (match schema with
                | [node] ->
                  (select,ty,ty2) = aux(node.zubnodes,idx+1,List.tail(pth),ty,ty,select)
                  ([{Int64=("index{idx}",i)}|select],ty,ty2)
                | _ -> @fail("MongoDb.real_type: bad schema"))
             | k -> @fail("MongoDb.real_type: index missing for intmap {k}"))
          | {TyName_args=[ty]; TyName_ident="stringmap"} ->
            //do println("  hd(pth)={List.head(pth)}")
            (match List.head(pth) with
             | {StringKey=s} ->
               (match schema with
                | [node] ->
                  (select,ty,ty2) = aux(node.zubnodes,idx+1,List.tail(pth),ty,ty,select)
                  ([{String=("index{idx}",s)}|select],ty,ty2)
                | _ -> @fail("MongoDb.real_type: bad schema"))
             | k -> @fail("MongoDb.real_type: index missing for stringmap {k}"))
          | {TyRecord_row=row}
          | {TyRecord_row=row; TyRecord_rowvar=_} ->
            //do println("  row={OpaType.to_pretty(ty)}")
            //do println("head(pth)={List.head(pth)}")
            node = Schema.find_node(List.head(pth),schema)
            //do println("  subnode={node}")
            (match find_field(row,node.name) with
             | {some={~label; ty=ty2}} ->
               ty3 = {TyRecord_row=[{~label; ty=ty2}]}
               //do println("{if List.length(pth) <= 1 then "*" else "-"} k={node.name}")
               //do println("  pth={pth}\n  ty={OpaType.to_pretty(ty)}")
               //do println("  ty2={OpaType.to_pretty(ty2)}\n  ty3={OpaType.to_pretty(ty3)}")
               (select,ty,ty2) = aux(node.zubnodes,idx,List.tail(pth),ty2,ty,select)
               select = if select == [] && pthlen <= 1 then [{String=("key",label)}] else select
               if List.length(pth) <= 1
               then (select,ty3,ty2)
               else ([{String=("key",label)}|select],ty,ty2)
             | {none} -> @fail)
          | {TySum_col = col}
          | {TySum_col = col; TySum_colvar = _} ->
            // TODO: Recursive types
            // TODO: Sum types
            // UNFINISHED{{
            do println("  col={OpaType.to_pretty(ty)}")
            do println("head(pth)={List.head(pth)}")
            node = Schema.find_node(List.head(pth),schema)
            do println("  subnode={node}")
            s =
            List.fold_left((s, row ->
                             (ss,_ty,_ty2) = aux(schema,idx,pth,{TyRecord_row=row},{TyRecord_row=row},[])
                             List.flatten([s, ss])
                           ),[],col)
            do println("s={s}")
            @fail//rec_to_bson(key, v, OpaType.fields_of_fields_list(v, col).f1)
            // }}UNFINISHED
          | {TyName_args=[_lty]; TyName_ident="list"} ->
            //do println("list:\n  pth={pth}\n  ty={OpaType.to_pretty(ty)}\n  ty2={OpaType.to_pretty(ty2)}")
            (select,ty,ty2)
          // TODO: Write through of Bson.document (again)
/*
          | {TyName_ident = "Bson.document"; TyName_args = _} ->
            [{Document=(key,@unsafe_cast(v))}]
*/
          | {TyName_args = tys; TyName_ident = tyid} ->
            aux(schema, idx, pth, OpaType.type_of_name(tyid, tys), ty2, select)
          | _ -> @fail("MongoDb.real_type: unknown type {OpaType.to_pretty(ty)}")

    ty = Schema.strip_ref(@typeof(dbpath))
    if pth == []
    then ([{String=(keyname,"key")}],ty,ty,dbname,collname)
    else (select,ty,ty2) = aux(schema,0,pth,ty,ty,[])
         (select,ty,ty2,dbname,collname)

  factor_types(ty0:OpaType.ty, v:'a): Bson.document =
    v_ty = @typeof(v)
    match ty0 with
    | {TyRecord_row=[~{label; ty=_}]}
    | {TyRecord_row=[~{label; ty=_}]; TyRecord_rowvar=_} ->
      /*do if v_ty != ty
         then @fail("MongoDb.factor_types:  mis-matching types {OpaType.to_pretty(ty)} and {OpaType.to_pretty(v_ty)}")*/
      opa_to_bson(label,v,{some=v_ty})
    | {TyName_args=[_lty]; TyName_ident="list"} ->
      opa_to_bson(valname,v,{some=v_ty})
    | _ -> @fail("MongoDb.factor_types:  mis-matching types {OpaType.to_pretty(ty0)} and {OpaType.to_pretty(v_ty)}")

  write(dbpath:ref_path('c), path:ref_path('a), v:'a) =
    (select,ty,ty2,db,collection) = real_type(dbpath,path)
    ns = "{db}.{collection}"
    /*do println("v={v}\n  ns={ns}\n  select={select}")
    do println("  ty={OpaType.to_pretty(ty)}")
    do println("  ty2={OpaType.to_pretty(ty2)}")
    do println("  typeof(v)={OpaType.to_pretty(@typeof(v))}")
    do println("  ty==ty2: {ty==ty2}")*/
    update =
      [{Document=("$set",(if ty == ty2
                          then opa_to_bson(valname,v,{none})
                          else factor_types(ty,v)))}]
    //do println("MongoDb.write: update={update}")
    key =
      match Bson.find_element(select,"key") with
      | {some={String=("key",key)}} -> key
      | _ -> @fail
    do println("MongoDb.write: {db}.{collection}({key})")
    do if Mongo.update(mongo,Mongo._Upsert,ns,select,update)
       then println("err={Bson.string_of_result(Cursor.last_error(mongo, db))}")
       else println("update failure")
    void

  /*`<-`(d,t,a) = write(d,t,a)*/

  // TODO: update read to match write
  read_(path:ref_path('a), indices:Bson.document): 'a =
    (db, collection, key) = path_to_mongo(path)
    ns = "db{db}.c{collection}"
    (match @typeof(path) with
     | {TyName_args=[ty]; TyName_ident="ref_path"}
     | {TyName_args=[ty]; TyName_ident="val_path"} ->
       do println("MongoDb.read: {ns}({key}) ty={OpaType.to_pretty(ty)}")
       select = get_select(key, indices)
       (match Cursor.find_one(mongo,ns,select,{some=[{Int32=(valname,1)}]}) with
        | {success=doc} ->
          //do println("doc={doc}")
          (match bson_to_opa(doc, ty) with
           | {some=v} -> v
           | {none} -> @fail("MongoDb.read: not found"))
        | {~failure} -> @fail("MongoDb.read: error from MongoDB: {failure}"))
     | ty -> @fail("MongoDb.read: unknown db value {path} of path type {OpaType.to_pretty(ty)}"))

  readm(path:ref_path(map('a, 'b)), i:list('a)) =
    //do println("path={@typeof(path)}  i={@typeof(i)}")
    read_(@unsafe_cast(path),index(i))

  read(path:ref_path('a)) =
    read_(path, [])

  create_index(path:ref_path('a), index:Bson.document, flags:int): Mongo.result =
    (db, collection, _) = path_to_mongo(path)
    ns = "db{db}.c{collection}"
    if Indexes.create_index(mongo,ns,index,flags)
    then Cursor.last_error(mongo, db)
    else {failure={Error="command send failure"}}

  create_standard_index(path:ref_path('a)): Mongo.result =
    create_index(path,[{Int32=(idxname,1)}],(Indexes._Sparse+Indexes._Unique))

  show_path(str:string, path:ref_path('a), ty:OpaType.ty): void =
    (db, collection, key) = path_to_mongo(path)
    len = path_length(path)
    pth = Schema.get_path(path)
    do println("{str}:\nlen={len}\npth={pth}\ndb={db}\ncollection={collection}\nkey={key}\nty={OpaType.to_pretty(ty)}\n")
    void

}}

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
/*db /path/bd: Bson.document = [{Null=("null",void)}]
db /path/bd/hd = { Null = ("null",void) }
db /path/bd/hd/Boolean/f2 = { false }*/
db /path2/p/v: void
db /path2/p/i: int
db /i0: int
//db /ism : map(int, string)
db /ssm : map(string, string)
//db /bsm : map(bool, string) /* Works but generates warnings */
db /db3/im: intmap({ a: int; b: string })
db /db4/nm: intmap(intmap({ a: int; b: string }))
db /db5/sm: stringmap({ a: int; b: string; c: bool })
db /db5/sm[_]/c = { false }
db /rct: rct

/*
[{id = 8; name = b; node = {IntKey = 0}; typ = SUM;
  zubnodes = [{id = 9; name = ; node = {IntKey = 0}; typ = RECORD;
               zubnodes = [{id = 10; name = false; node = {IntKey = 0}; typ = RECORD; zubnodes = []}]},
                           {id = 11; name = ; node = {IntKey = 1}; typ = RECORD;
               zubnodes = [{id = 12; name = true; node = {IntKey = 0}; typ = RECORD; zubnodes = []}]}]},
 {id = 13; name = f; node = {IntKey = 1}; typ = float; zubnodes = []},
 {id = 14; name = i; node = {IntKey = 2}; typ = int; zubnodes = []},
 {id = 16; name = *; node = {IntKey = 3}; typ = SUM;
  zubnodes = [{id = 17; name = ; node = {IntKey = 0}; typ = RECORD;
               zubnodes = [{id = 18; name = hd; node = {IntKey = 0}; typ = int; zubnodes = []},
                           {id = 16; name = tl; node = {IntKey = 1}; typ = link; zubnodes = []}]},
                           {id = 19; name = ; node = {IntKey = 1}; typ = RECORD;
                            zubnodes = [{id = 20; name = nil; node = {IntKey = 0}; typ = RECORD; zubnodes = []}]}]},
 {id = 21; name = nt; node = {IntKey = 3}; typ = SUM;
  zubnodes = [{id = 22; name = ; node = {IntKey = 0}; typ = RECORD;
               zubnodes = [{id = 23; name = none; node = {IntKey = 0}; typ = RECORD; zubnodes = []}]},
                           {id = 24; name = ; node = {IntKey = 1}; typ = RECORD;
                            zubnodes = [{id = 25; name = some; node = {IntKey = 0}; typ = int; zubnodes = []}]}]},
 {id = 26; name = ot; node = {IntKey = 3}; typ = SUM;
  zubnodes = [{id = 27; name = ; node = {IntKey = 0}; typ = RECORD;
               zubnodes = [{id = 28; name = none; node = {IntKey = 0}; typ = RECORD; zubnodes = []}]},
                           {id = 29; name = ; node = {IntKey = 1}; typ = RECORD;
                            zubnodes = [{id = 30; name = some; node = {IntKey = 0}; typ = int; zubnodes = []}]}]},
 {id = 31; name = rt; node = {IntKey = 3}; typ = SUM;
  zubnodes = [{id = 32; name = ; node = {IntKey = 0}; typ = RECORD;
               zubnodes = [{id = 33; name = rtFloat; node = {IntKey = 0}; typ = float; zubnodes = []}]},
                           {id = 34; name = ; node = {IntKey = 1}; typ = RECORD;
                            zubnodes = [{id = 35; name = rtInt; node = {IntKey = 0}; typ = int; zubnodes = []}]},
                           {id = 36; name = ; node = {IntKey = 2}; typ = RECORD;
                            zubnodes = [{id = 37; name = rtNull; node = {IntKey = 0}; typ = RECORD; zubnodes = []}]},
                           {id = 38; name = ; node = {IntKey = 3}; typ = RECORD;
                            zubnodes = [{id = 39; name = rtString; node = {IntKey = 0}; typ = text; zubnodes = []}]}]},
 {id = 40; name = s; node = {IntKey = 3}; typ = text; zubnodes = []},
 {id = 41; name = st; node = {IntKey = 4}; typ = RECORD;
  zubnodes = [{id = 42; name = stFloat; node = {IntKey = 0}; typ = float; zubnodes = []},
              {id = 43; name = stInt; node = {IntKey = 1}; typ = int; zubnodes = []},
              {id = 44; name = stNull; node = {IntKey = 2}; typ = RECORD; zubnodes = []},
              {id = 45; name = stString; node = {IntKey = 3}; typ = text; zubnodes = []}]},
 {id = 46; name = tt; node = {IntKey = 4}; typ = RECORD;
  zubnodes = [{id = 47; name = f1; node = {IntKey = 0}; typ = text; zubnodes = []},
              {id = 48; name = f2; node = {IntKey = 1}; typ = int; zubnodes = []}]}]
*/

/*
bsn(n) =
  [{node={IntKey=0}; name=""; id=(n+2); typ="RECORD";
    zubnodes=[{node={IntKey=0}; name="false"; id=(n+3); typ="RECORD"; zubnodes=[]}]},
   {node={IntKey=1}; name=""; id=n; typ="RECORD";
    zubnodes=[{node={IntKey=0}; name="true"; id=(n+1); typ="RECORD"; zubnodes=[]}]}]

osn(n,t) =
  [{node={IntKey=0}; name=""; id=(n+2); typ="RECORD";
    zubnodes=[{node={IntKey=0}; name="none"; id=(n+3); typ="RECORD"; zubnodes=[]}]},
   {node={IntKey=1}; name=""; id=n; typ="RECORD";
    zubnodes=[{node={IntKey=0}; name="some"; id=(n+1); typ=t; zubnodes=[]}]}]

schema=([{node={IntKey=0}; name="i0"; id=45; typ="int"; zubnodes=[]},

         {node={IntKey=5}; name="db3"; id=62; typ="RECORD";
          zubnodes=[{node={IntKey=0}; name="im"; id=63; typ="SET";
                     zubnodes=[{node={StringKey="int"}; name=""; id=64; typ="RECORD";
                                zubnodes=[{node={IntKey=0}; name="a"; id=66; typ="int"; zubnodes=[]},
                                          {node={IntKey=1}; name="b"; id=65; typ="text"; zubnodes=[]}
                                         ]}
                              ]}
                   ]},

         {node={IntKey=7}; name="db4"; id=56; typ="RECORD";
          zubnodes=[{node={IntKey=0}; name="nm"; id=57; typ="SET";
                     zubnodes=[{node={StringKey="int"}; name=""; id=58; typ="SET";
                                zubnodes=[{node={StringKey="int"}; name=""; id=59; typ="RECORD";
                                           zubnodes=[{node={IntKey=0}; name="a"; id=61; typ="int"; zubnodes=[]},
                                                     {node={IntKey=1}; name="b"; id=60; typ="text"; zubnodes=[]}
                                                    ]}
                                         ]}
                              ]}
                   ]},

         {node={IntKey=10}; name="db5"; id=46; typ="RECORD";
          zubnodes=[{node={IntKey=0}; name="sm"; id=47; typ="SET";
                     zubnodes=[{node={StringKey="string"}; name=""; id=48; typ="RECORD";
                                zubnodes=[{node={IntKey=0}; name="a"; id=55; typ="int"; zubnodes=[]},
                                          {node={IntKey=1}; name="b"; id=54; typ="text"; zubnodes=[]},
                                          {node={IntKey=2}; name="c"; id=49; typ="SUM"; zubnodes=bsn(52)}
                                         ]}
                              ]}
                   ]},
         {node={IntKey=2}; name="path"; id=7; typ="RECORD";
          zubnodes=[{node={IntKey=0}; name="b"; id=38; typ="SUM"; zubnodes=bsn(41)},
                    {node={IntKey=2}; name="f"; id=37; typ="float"; zubnodes=[]},
                    {node={IntKey=3}; name="i"; id=36; typ="int"; zubnodes=[]},
                    {node={IntKey=14}; name="lt"; id=36; typ="RECURSIVE";
                     zubnodes=[{node={IntKey=-1}; name="*"; id=37; typ="SUM";
                                zubnodes=[{node={IntKey=0}; name=""; id=40; typ="RECORD";
                                          zubnodes=[{node={IntKey=0}; name="hd"; id=41; typ="int"; zubnodes=[]},
                                                    {node={IntKey=1}; name="tl"; id=37; typ="link"; zubnodes=[]}
                                                   ]},
                                          {node={IntKey=1}; name=""; id=38; typ="RECORD";
                                          zubnodes=[{node={IntKey=0}; name="nil"; id=39; typ="RECORD"; zubnodes=[]}
                                                   ]}
                                         ]}
                              ]},
                    {node={IntKey=5}; name="nt"; id=31; typ="SUM"; zubnodes=osn(32,"int")},
                    {node={IntKey=6}; name="ot"; id=26; typ="SUM"; zubnodes=osn(27,"int")},
                    {node={IntKey=7}; name="rt"; id=17; typ="SUM";
                     zubnodes=[{node={IntKey=0}; name=""; id=24; typ="RECORD";
                                zubnodes=[{node={IntKey=0}; name="rtFloat"; id=25; typ="float"; zubnodes=[]}]},
                               {node={IntKey=1}; name=""; id=22; typ="RECORD";
                                zubnodes=[{node={IntKey=0}; name="rtInt"; id=23; typ="int"; zubnodes=[]}]},
                               {node={IntKey=2}; name=""; id=20; typ="RECORD";
                                zubnodes=[{node={IntKey=0}; name="rtNull"; id=21; typ="RECORD"; zubnodes=[]}]},
                               {node={IntKey=3}; name=""; id=18; typ="RECORD";
                                zubnodes=[{node={IntKey=0}; name="rtString"; id=19; typ="text"; zubnodes=[]}]}
                              ]},
                    {node={IntKey=8}; name="s"; id=16; typ="text"; zubnodes=[]},
                    {node={IntKey=9}; name="st"; id=11; typ="RECORD";
                     zubnodes=[{node={IntKey=0}; name="stFloat"; id=15; typ="float"; zubnodes=[]},
                               {node={IntKey=1}; name="stInt"; id=14; typ="int"; zubnodes=[]},
                               {node={IntKey=2}; name="stNull"; id=13; typ="RECORD"; zubnodes=[]},
                               {node={IntKey=3}; name="stString"; id=12; typ="text"; zubnodes=[]}
                              ]},
                    {node={IntKey=10}; name="tt"; id=8; typ="RECORD";
                     zubnodes=[{node={IntKey=0}; name="f1"; id=10; typ="text"; zubnodes=[]},
                               {node={IntKey=1}; name="f2"; id=9; typ="int"; zubnodes=[]}
                              ]}
                   ]},
         {node={IntKey=3}; name="path2"; id=3; typ="RECORD";
          zubnodes=[{node={IntKey=0}; name="p"; id=4; typ="RECORD";
                     zubnodes=[{node={IntKey=0}; name="i"; id=6; typ="int"; zubnodes=[]},
                               {node={IntKey=1}; name="v"; id=5; typ="RECORD"; zubnodes=[]}
                              ]}
                   ]}
        ]:MongoDb.schema)
*/

// Alphabetical order
/*
+-[0-db3]-(74-RECORD)-[0-im]-(75-SET)-[int]-(76-RECORD)-+-[0-a]-(78-int)
|                                                       `-[1-b]-(77-text)
|-[1-db4]-(68-RECORD)-[0-nm]-(69-SET)-[int]-(70-SET)-[int]-(71-RECORD)-+-[0-a]-(73-int)
|                                                                      `-[1-b]-(72-text)
|-[2-db5]-(58-RECORD)-[0-sm]-(59-SET)-[string]-(60-RECORD)-+-[0-a]-(67-int)
|                                                          |-[1-b]-(66-text)
|                                                          `-[2-c]-(61-SUM)-+-[0]-(64-RECORD)-[0-false]-(65-RECORD)
|                                                                           `-[1]-(62-RECORD)-[0-true]-(63-RECORD)
|-[3-i0]-(57-int)
|-[4-ism]-(55-SET)-[int]-(56-text)
|-[5-path]-(13-RECORD)-+-[0-b]-(50-SUM)-+-[0]-(53-RECORD)-[0-false]-(54-RECORD)
|                      |                `-[1]-(51-RECORD)-[0-true]-(52-RECORD)
|                      |-[1-f]-(49-float)
|                      |-[2-i]-(48-int)
|                      |-[3-lt]-(42-RECURSIVE)-[*]-(43-SUM)-+-[0]-(46-RECORD)-+-[0-hd]-(47-int)
|                      |                                    |                 `-[1-tl]-->{43}
|                      |                                    `-[1]-(44-RECORD)-[0-nil]-(45-RECORD)
|                      |-[4-nt]-(37-SUM)-+-[0]-(40-RECORD)-[0-none]-(41-RECORD)
|                      |                 `-[1]-(38-RECORD)-[0-some]-(39-int)
|                      |-[5-ot]-(32-SUM)-+-[0]-(35-RECORD)-[0-none]-(36-RECORD)
|                      |                 `-[1]-(33-RECORD)-[0-some]-(34-int)
|                      |-[6-rt]-(23-SUM)-+-[0]-(30-RECORD)-[0-rtFloat]-(31-float)
|                      |                 |-[1]-(28-RECORD)-[0-rtInt]-(29-int)
|                      |                 |-[2]-(26-RECORD)-[0-rtNull]-(27-RECORD)
|                      |                 `-[3]-(24-RECORD)-[0-rtString]-(25-text)
|                      |-[7-s]-(22-text)
|                      |-[8-st]-(17-RECORD)-+-[0-stFloat]-(21-float)
|                      |                    |-[1-stInt]-(20-int)
|                      |                    |-[2-stNull]-(19-RECORD)
|                      |                    `-[3-stString]-(18-text)
|                      `-[9-tt]-(14-RECORD)-+-[0-f1]-(16-text)
|                                           `-[1-f2]-(15-int)
|-[6-path2]-(9-RECORD)-[0-p]-(10-RECORD)-+-[0-i]-(12-int)
|                                        `-[1-v]-(11-RECORD)
|-[7-rct]-(3-RECURSIVE)-[*]-(4-SUM)-+-[0]-(7-RECORD)-+-[0-x]-(8-int)
|                                   |                `-[1-y]-->{4}
|                                   `-[1]-(5-RECORD)-[0-z]-(6-RECORD)
`-[8-ssm]-(1-SET)-[string]-(2-text)
*/

/*
db /maps/im : intmap(int)
db /db3/im: intmap({ a: int; b: string })
db /opages/pages[_] = Page.empty
db /test/ii : map(int, int)
db /test/si : map(string, int)
db /test/ss : stringmap(string)
db /z[{a;b}] : { a : int; b : string; c : int }
db /wiki: stringmap(Template.default_content)
db /benchs/nobels : stringmap(stringmap(stringmap(string)))
db /session_data : intmap(map(string,string))
*/

/*
tstrt(name,dbpath,path) =
  (select,ty,ty2,dbname,collname) = MongoDb.real_type(dbpath,path)
  do println("{name}:\n  select={Bson.string_of_bson(select)}\n  ty={OpaType.to_pretty(ty)}\n  ty2={OpaType.to_pretty(ty2)}")
  do println("  dbname={dbname}\n  collname={collname}\n")
  void
*/

rctval = {x=1; y={x=2; y={x=3; y={z}}}}

_ =
  do println("dbMongo")
  //v = [1,2,3,4,5,6,7]
  //v = rctval
  //do println("opa_to_bson({v})={MongoDb.opa_to_bson(MongoDb.valname,v,{none})}")
  /*do /db3/im[12] <- { a=123; b="abc" }
  do println("/db3/im[12]={/db3/im[12]}")
  do println("ty(int)={@typeof(/db3/im[12])}")*/
/*
  do MongoDb.show_path("@/i0",@/i0,@typeof(/i0))
  do MongoDb.show_path("@/path/i",@/path/i,@typeof(/path/i))
  do MongoDb.show_path("@/path2/p/v",@/path2/p/v,@typeof(/path2/p/v))
  do MongoDb.show_path("@/ism",@/ism,@typeof(/ism))
  do MongoDb.show_path("@/ism[3]",@/ism[3],@typeof(/ism[3]))
  do MongoDb.show_path("@/path",@/path,@typeof(/path))
  do MongoDb.show_path("@/db3",@/db3,@typeof(/db3))
  do MongoDb.show_path("@/db3/im",@/db3/im,@typeof(/db3/im))
  do MongoDb.show_path("@/db3/im[12]",@/db3/im[12],@typeof(/db3/im[12]))
  do MongoDb.show_path("@/db3/im[12]/a",@/db3/im[12]/a,@typeof(/db3/im[12]/a))
  do MongoDb.show_path("@/db3/im[12]/b",@/db3/im[12]/b,@typeof(/db3/im[12]/b))
  do MongoDb.show_path("@/db4/nm",@/db4/nm,@typeof(/db4/nm))
  do MongoDb.show_path("@/db4/nm[1]",@/db4/nm[1],@typeof(/db4/nm[1]))
  do MongoDb.show_path("@/db4/nm[1][2]",@/db4/nm[1][2],@typeof(/db4/nm[1][2]))
  do MongoDb.show_path("@/db4/nm[1][2]/a",@/db4/nm[1][2]/a,@typeof(/db4/nm[1][2]/a))
  do MongoDb.show_path("@/db4/nm[1][2]/b",@/db4/nm[1][2]/b,@typeof(/db4/nm[1][2]/b))
  do tstrt("@/db3/im[12]/a",@/db3,@/db3/im[12]/a)
  do tstrt("@/db3/im[12]",@/db3,@/db3/im[12])
  do tstrt("@/db4/nm[12][34]/b",@/db4,@/db4/nm[12][34]/b)
  do tstrt("@/db4/nm[12][34]",@/db4,@/db4/nm[12][34])
  do tstrt("@/db5/sm[\"here\"]/c",@/db5,@/db5/sm["here"]/c)
  do tstrt("@/db5/sm[\"here\"]",@/db5,@/db5/sm["here"])
  do tstrt("@/path/i",@/path,@/path/i)
  do tstrt("@/path2/p/v",@/path2,@/path2/p/v)
*/
  do Schema.add_db_to_schema(@/db3)
  do Schema.add_db_to_schema(@/db4)
  do Schema.add_db_to_schema(@/db5)
  do Schema.add_db_to_schema(@/i0)
  //do Schema.add_db_to_schema(@/ism)
  do Schema.add_db_to_schema(@/path)
  do Schema.add_db_to_schema(@/path2)
  do Schema.add_db_to_schema(@/rct)
  //do Schema.add_db_to_schema(@/ssm)
  do Schema.sort_schema()
  do println("schema=\n{Schema.string_of_schema(Schema.getschema())}")
  do MongoDb.write(@/i0,@/i0,420)
  //do MongoDb.write(@/db3,@/db3/im,IntMap.empty)
  do MongoDb.write(@/db3,@/db3/im[12],{a=123; b="abc"})
  do MongoDb.write(@/db3,@/db3/im[12]/a,456)
  do MongoDb.write(@/db3,@/db3/im[12]/b,"def")
  do MongoDb.write(@/db4,@/db4/nm[12][34],{a=123; b="abc"})
  do MongoDb.write(@/db4,@/db4/nm[12][34]/a,456)
  do MongoDb.write(@/db4,@/db4/nm[12][34]/b,"def")
  do MongoDb.write(@/db5,@/db5/sm["parasaurolophus"],{a=76500000; b="cyrtocristatus"; c={false}})
  do MongoDb.write(@/db5,@/db5/sm["parasaurolophus"]/a,73000000)
  do MongoDb.write(@/db5,@/db5/sm["parasaurolophus"]/c,{true})
  do MongoDb.write(@/path,@/path/i,42)
  do MongoDb.write(@/path,@/path/s,"forty two")
  do MongoDb.write(@/path,@/path/f,42.24)
  do MongoDb.write(@/path,@/path/b,true)
  do MongoDb.write(@/path,@/path/ot,{some=4242})
  do MongoDb.write(@/path,@/path/nt,{none})
  do MongoDb.write(@/path,@/path/rt,{rtInt=4242})
  do MongoDb.write(@/path,@/path/st,{stInt=424242; stString="forty two forty two"; stFloat=42.42; stNull=void})
  do MongoDb.write(@/path,@/path/tt,("ghi",789))
  do MongoDb.write(@/path,@/path/lt,[42,43])
  //do MongoDb.write(@/path/bd,([{Int32=("int32",2424)}]:Bson.document))
  do MongoDb.write(@/path2,@/path2/p/v,void)
  do MongoDb.write(@/path2,@/path2/p/i,4224)
  //do MongoDb.write(@/rct,@/rct,rctval)
/*
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/ism))}")
  do MongoDb.write(@/ism[3],"three")
  do MongoDb.write(@/ism[5],"five")
  do MongoDb.write(@/ism[7],"seven")
  do MongoDb.writem(@/ism,[3],"three")
  do MongoDb.writem(@/ism,[5],"five")
  do MongoDb.writem(@/ism,[7],"seven")
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/ssm))}")
  do MongoDb.writem(@/ssm,["cat"],"moggy")
  do MongoDb.writem(@/ssm,["rat"],"roland")
  do MongoDb.writem(@/ssm,["hat"],"fedora")
  do MongoDb.writem(@/bsm,[{true}],"true")
  do MongoDb.writem(@/bsm,[{false}],"false")
  do MongoDb.write(@/db3/im[12]/b,"im")
  do println("i0={(MongoDb.read(@/i0):int)}")
  do println("i={(MongoDb.read(@/path/i):int)}")
  do println("s={(MongoDb.read(@/path/s):string)}")
  do println("f={(MongoDb.read(@/path/f):float)}")
  do println("b={(MongoDb.read(@/path/b):bool)}")
  do println("ot={(MongoDb.read(@/path/ot):option(int))}")
  do println("nt={(MongoDb.read(@/path/nt):option(int))}")
  do println("rt={(MongoDb.read(@/path/rt):rtype)}")
  do println("st={(MongoDb.read(@/path/st):stype)}")
  do println("lt={(MongoDb.read(@/path/lt):list(int))}")
  do println("p/v={(MongoDb.read(@/path2/p/v):void)}")
  do println("p/i={(MongoDb.read(@/path2/p/i):int)}")
  do println("bd={(MongoDb.read(@/path/bd):Bson.document)}")
  do MongoDb.write(@/path/i,43)
  do println("i={(MongoDb.read(@/path/i):int)}")
  do println("ism[3]={(MongoDb.readm(@/ism,[3]):string)}")
  do println("ism[5]={(MongoDb.readm(@/ism,[5]):string)}")
  do println("ism[7]={(MongoDb.readm(@/ism,[7]):string)}")
  do println("ssm[cat]={(MongoDb.readm(@/ssm,["cat"]):string)}")
  do println("ssm[rat]={(MongoDb.readm(@/ssm,["rat"]):string)}")
  do println("ssm[hat]={(MongoDb.readm(@/ssm,["hat"]):string)}")
  do println("bsm[true]={(MongoDb.readm(@/bsm,[{true}]):string)}")
  do println("bsm[false]={(MongoDb.readm(@/bsm,[{false}]):string)}")
  do println("db3/im[12]/b={(MongoDb.read(@/db3/im[12]/b):string)}")
*/
  void
*/
