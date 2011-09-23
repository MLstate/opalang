import stdlib.system

type path = external

MongoDb = {{

  keyname = "key"
  idxname = "index"
  valname = "value"

  path_to_string = (%% Mongolink.path_to_string %% : ref_path('a) -> string)
  path_to_bson = (%% Mongolink.path_to_bson %% : ref_path('a) -> Bson.document)
  path_to_mongo = (%% Mongolink.path_to_mongo %% : ref_path('a) -> (string, string, string))

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

    rec rec_to_bson(key:string, v:'a, fields:OpaType.fields): Bson.document =
      [{Document=(key,(List.flatten(OpaValue.Record.fold_with_fields(
                                     (field, tyfield, value, bson ->
                                       name = OpaValue.Record.name_of_field_unsafe(field)
                                       res = opa_to_document(name, value, tyfield)
                                       [res | bson]),
                                     v, fields, []))))}]

    and list_to_bson(key:string, v:'a, ty:OpaType.ty): Bson.document =
      doc = List.flatten(List.fold_index((i, v, acc -> (opa_to_document("{i}", v, ty)) +> acc), @unsafe_cast(v), []))
      [{Array = (key,doc)}]

    and opa_to_document(key:string, v:'a, ty:OpaType.ty): Bson.document =
      match ty with
      | {TyName_args=[]; TyName_ident="void"} -> [{Null=(key,void)}]
      | {TyConst={TyInt={}}} -> [{Int64=(key,(@unsafe_cast(v):int))}]
      | {TyConst={TyString={}}} -> [{String=(key,(@unsafe_cast(v):string))}]
      | {TyConst={TyFloat={}}} -> [{Double=(key,(@unsafe_cast(v):float))}]
      | {TyName_args=[]; TyName_ident="bool"} -> [{Boolean=(key,(@unsafe_cast(v):bool))}]
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
        (match row with
         | [{label=name; ty=ty}] ->
           if OpaType.is_void(ty)
           then [{Document=(key,[{Null=(name,void)}])}]
           else rec_to_bson(key, v, row)
         | _ ->
           rec_to_bson(key, v, row))
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        rec_to_bson(key, v, OpaType.fields_of_fields_list(v, col).f1)
      | {TyName_args=[lty]; TyName_ident="list"} ->
        list_to_bson(key, @unsafe_cast(v), lty)
      | {TyName_ident = "Bson.document"; TyName_args = _} ->
        [{Document=(key,@unsafe_cast(v))}]
      | _ -> @fail("MongoDb.opa_to_bson: unknown value {v} of type {OpaType.to_pretty(ty)}")

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
         | element -> @fail("MongoDb.bson_to_opa: expected float, got {element}"))
      | {TyName_args=[]; TyName_ident="bool"} ->
        (match element with
         | {Boolean=(_,tf)} -> {some=@unsafe_cast(tf)}
         | {Int32=(_,i)} -> {some=@unsafe_cast(i != 0)}
         | {Int64=(_,i)} -> {some=@unsafe_cast(i != 0)}
         | {Double=(_,d)} -> {some=@unsafe_cast(d != 0.0)}
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
              //do println("fields={fields}")
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

  get_select(key:string, index_opt:option(Bson.document)): Bson.document =
    match index_opt with
    | {some=index} -> List.flatten([[{String=(keyname,key)}], index])
    | {none} -> [{String=(keyname,key)}]

  write_(path:ref_path('a), index_opt:option(Bson.document), v:'a) =
    match path_to_mongo(path) with
    | (db, collection, key) ->
      ns = "db{db}.c{collection}"
      select = get_select(key, index_opt)
      update = List.flatten([select, opa_to_bson(valname,v,{none})])
      //do println("MongoDb.write: update={update}")
      do println("MongoDb.write: db{db}.c{collection}({key})")
      do if Mongo.update(mongo,Mongo._Upsert,ns,select,update)
         then println("err={Bson.string_of_result(Cursor.last_error(mongo, db))}")
         else println("update failure")
      void

  index(i:'a): Bson.document = opa_to_bson(idxname, i, {none})

  writem(path:ref_path(map('a, 'b)), i:'a, v:'b) =
    //do println("path={@typeof(path)}  i={@typeof(i)}  typeof={@typeof(v)}")
    write_(@unsafe_cast(path),{some=index(i)},v)

  write(path:ref_path('a), v:'a) =
    write_(path, {none}, v)

  `<-`(d,a) = write(d,a)

  read_(path:ref_path('a), index_opt:option(Bson.document)): 'a =
    match path_to_mongo(path) with
    | (db, collection, key) ->
      ns = "db{db}.c{collection}"
      (match @typeof(path) with
       | {TyName_args=[ty]; TyName_ident="ref_path"}
       | {TyName_args=[ty]; TyName_ident="val_path"} ->
         do println("MongoDb.read: {ns}({key}) ty={OpaType.to_pretty(ty)}")
         select = get_select(key, index_opt)
         (match Cursor.find_one(mongo,ns,select,{some=[{Int32=(valname,1)}]}) with
          | {success=doc} ->
            //do println("doc={doc}")
            (match bson_to_opa(doc, ty) with
             | {some=v} -> v
             | {none} -> @fail("MongoDb.read: not found"))
          | {~failure} -> @fail("MongoDb.read: error from MongoDB: {failure}"))
       | ty -> @fail("MongoDb.read: unknown db value {path} of path type {OpaType.to_pretty(ty)}"))

  readm(path:ref_path(map('a, 'b)), i:'a) =
    //do println("path={@typeof(path)}  i={@typeof(i)}")
    read_(@unsafe_cast(path),{some=index(i)})

  read(path:ref_path('a)) =
    read_(path, {none})

  create_index(path:ref_path('a), index:Bson.document, flags:int): Mongo.result =
    match path_to_mongo(path) with
    | (db, collection, _) ->
      ns = "db{db}.c{collection}"
      if Indexes.create_index(mongo,ns,index,flags)
      then Cursor.last_error(mongo, db)
      else {failure={Error="command send failure"}}

  create_standard_index(path:ref_path('a)): Mongo.result =
    create_index(path,[{Int32=(idxname,1)}],(Indexes._Sparse+Indexes._Unique))

}}

/*
type rtype = { rtInt:int } / { rtString:string } / { rtFloat:float } / { rtNull:void }
type stype = { stInt:int; stString:string; stFloat:float; stNull:void }

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
db /path/bd: Bson.document = [{Null=("null",void)}]
db /path/bd/hd = { Null = ("null",void) }
db /path/bd/hd/Boolean/f2 = { false }
db /path2/p/v: void
db /path2/p/i: int
db /i0: int
db /ism : map(int, string)
db /ssm : map(string, string)
//db /bsm : map(bool, string) /* Works but generates warnings */

/* Some things from opa code to try... */
//db /maps/im : intmap(int)
//db /db3/mymap: intmap({ a: int; b: string })
//db /opages/pages[_] = Page.empty
//db /test/ii : map(int, int)
//db /test/si : map(string, int)
//db /test/ss : stringmap(string)
//db /z[{a;b}] : { a : int; b : string; c : int }
//db /wiki: stringmap(Template.default_content)
//db /benchs/nobels : stringmap(stringmap(stringmap(string)))
//db /session_data : intmap(map(string,string))

_ =
  do println("dbMongo")
  do MongoDb.write(@/i0,420)
  do MongoDb.write(@/path/i,42)
  do MongoDb.write(@/path/s,"forty two")
  do MongoDb.write(@/path/f,42.0)
  do MongoDb.write(@/path/b,true)
  do MongoDb.write(@/path/ot,{some=4242})
  do MongoDb.write(@/path/nt,{none})
  do MongoDb.write(@/path/rt,{rtInt=4242})
  do MongoDb.write(@/path/st,{stInt=424242; stString="forty two forty two"; stFloat=42.42; stNull=void})
  do MongoDb.write(@/path/lt,[42,43])
  do MongoDb.write(@/path/bd,([{Int32=("int32",2424)}]:Bson.document))
  do MongoDb.write(@/path2/p/v,void)
  do MongoDb.write(@/path2/p/i,4224)
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/ism))}")
  do MongoDb.writem(@/ism,3,"three")
  do MongoDb.writem(@/ism,5,"five")
  do MongoDb.writem(@/ism,7,"seven")
  do println("index: {Bson.string_of_result(MongoDb.create_standard_index(@/ssm))}")
  do MongoDb.writem(@/ssm,"cat","moggy")
  do MongoDb.writem(@/ssm,"rat","roland")
  do MongoDb.writem(@/ssm,"hat","fedora")
  //do MongoDb.writem(@/bsm,{true},"true")
  //do MongoDb.writem(@/bsm,{false},"false")
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
  do println("ism[3]={(MongoDb.readm(@/ism,3):string)}")
  do println("ism[5]={(MongoDb.readm(@/ism,5):string)}")
  do println("ism[7]={(MongoDb.readm(@/ism,7):string)}")
  do println("ssm[cat]={(MongoDb.readm(@/ssm,"cat"):string)}")
  do println("ssm[rat]={(MongoDb.readm(@/ssm,"rat"):string)}")
  do println("ssm[hat]={(MongoDb.readm(@/ssm,"hat"):string)}")
  //do println("bsm[true]={(MongoDb.readm(@/bsm,{true}):string)}")
  //do println("bsm[false]={(MongoDb.readm(@/bsm,{false}):string)}")
  void
*/
