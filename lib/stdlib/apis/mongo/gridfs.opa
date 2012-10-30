/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

abstract
type GridFS.t = {
    Mongo.db    db,
    string      namespace,
    string      ns,
    string      key,
}

abstract
type GridFS.file = {
    Mongo.reply reply,
    GridFS.t    grid,
}

private
type GridFS.chunk = {
    Bson.value files_id,
    int n,
    binary data,
}

/**
 * GridFS is a storage for large files in MongoDb.
 *
 * @destination public
 * @stabilization work in progress
 **/
module GridFS{

    private
    upsert_flags = Bitwise.lor(0, MongoCommon.UpsertBit)

    private
    function updateerr(Mongo.db db, flags, ns, selector, update, upsert){
        reply = MongoDriver.updatee(db, flags, ns, selector, update)
        match(reply){
        case {none} : {failure : {Error : "Network error"}}
        case {some : reply} :
            match(MongoCommon.reply_document(reply, 0)){
            case {none} : {failure : {Error : "Protocol Error (1)"}}
            case {some : doc} :
                match(Bson.find_float(doc, "ok")){
                case {none} : {failure : {Error : "Protocol Error (2)"}}
                case {some : ok} :
                    if(ok != 1.0){
                        {failure : {Error : "MongoDb GetLastError : {doc}" }}
                    } else {
                        match(Bson.find_element(doc, "err")){
                        case {none} : {success}
                        case {some : {value : {String : str} ...}} :
                            {failure : {Error : "DbGen/Mongo: {str}"}}
                        case {some : {value : {Null} ...}} :
                            if(not(upsert)){
                                match(Bson.find_bool(doc, "updatedExisting")){
                                case {some:{true}} : {failure : {Error : "Update error"}}
                                case {some:{false}} : {success}
                                case {none} : {failure : {Error : "Protocol Error (4)"}}
                                }
                            } else {
                                {success};
                            }
                        case {some : err} :
                            {failure : {Error : "DbGen/Mongo: Protocol Error (3) {err}"}}
                        }
                    }
                }
            }
        }
    }

    private
    module Chunk{

        function chunks_selector(id){
            [{name : "files_id", value : id}]
        }

        function chunks_ns(grid){
            "{grid.namespace}.chunks"
        }

        function write(GridFS.t grid,  Bson.value id, GridFS.chunk chunk){
            updateerr(
                grid.db, upsert_flags, chunks_ns(grid), [
                    {name:"files_id", value:id},
                    {name:"n", value:{Int64:chunk.n}},
                ], [
                    {name:"files_id", value:chunk.files_id},
                    {name:"n", value:{Int64:chunk.n}},
                    {name:"data", value:{Binary:chunk.data}}
                ], true
            )
        }

        function writes(GridFS.t grid, binary data, Bson.value id, options){
            length = Binary.length(data)
            recursive function aux(n){
                start = n * options.chunk_size
                if(start > length) {success}
                else {
                    chunk_length = min((n+1) * options.chunk_size, length) - start
                    data = Binary.get_binary(data, start, chunk_length)
                    match(write(grid, id, ~{n, data, files_id:options.files_id})){
                    case {success} : aux(n+1)
                    case ~{failure} : ~{failure}
                    }
                }
            }
            aux(0)
        }

        function read(GridFS.t grid, Bson.value id){
            query = chunks_selector(id)
            selector = [
                {name : "$query", value : {Document : query}},
                {name : "$orderby", value : {Document : [
                    {name:"n", value:{Int32 : 1}}
                ]}},
            ]
            match(MongoDriver.query(grid.db, 0, chunks_ns(grid), 0, 0, selector, none)){
            case {none} : {failure : {Error : "Connection error"}}
            case {some:reply} : {success : GridFS.file ~{reply, grid}}
            }
        }

        function delete(GridFS.t grid, Bson.value id){
            MongoDriver.deletee(grid.db, 0, chunks_ns(grid), chunks_selector(id))
        }
    }

    private
    module File{

        function files_selector(id){
            [{name:"_id", value:id}]
        }

        function files_ns(grid){
            "{grid.namespace}.files"
        }

        function build(GridFS.t grid, Bson.value id, options){
            match(MongoCommands.run_command_ll(
                grid.db, grid.db.name, [
                    {name:"filemd5", value:id},
                    {name:"root", value:{String:grid.ns}}
                ])){
            case {success : bson} :
                match(Bson.find_element(bson, "md5")){
                case {some : md5} :
                    {success : (Bson.document [
                        {name:"_id", value:id},
                        {name:"length", value:{Int64:options.length}},
                        {name:"chunkSize", value:{Int64:options.chunk_size}},
                        {name:"uploadDate", value:{Date:Date.now()}},
                        md5
                    ] ++ Bson.document options.doc) }
                case _ : {failure : {Error : "Unexpected result of 'filemd5' commands: {bson}"}}
                }
            case {failure:_} as e -> e
            }
        }

        function save(GridFS.t grid, Bson.value id, options){
            match(build(grid, id, options)){
            case {failure:_} as e : e
            case {success:bson} :
                selector = [
                    {name : "_id", value : id}
                ]
                updateerr(grid.db, upsert_flags, files_ns(grid), selector, bson, true)
            }
        }

        function read(GridFS.t grid, Bson.value id){
            query = files_selector(id)
            match(MongoDriver.query(grid.db, 0, files_ns(grid), 0, 0, query, none)){
            case {none} : {failure : {Error : "Connection error"}}
            case {some:reply} :
                match(MongoCommon.reply_document(reply, 0)){
                case {none} : {failure : {Error : "No document"}}
                case {some:document} : {success : document}
                }
            }
        }

        function delete(GridFS.t grid, Bson.value id){
            MongoDriver.deletee(grid.db, 0, files_ns(grid), files_selector(id))
        }
    }

    /**
     * Create a Grid.
     * @param db The databases which host the grid.
     * @param namespace The namespace where the grid is stored
     */
    function GridFS.t make(Mongo.db db, string namespace){
        ns = namespace
        namespace = "{db.name}.{namespace}"
        grid = ~{db, namespace, ns, key:"_id"}
        Log.notice("GridFS", "Before")
        result = MongoDriver.create_index(db, Chunk.chunks_ns(grid), [
            {name:"files_id", value:{Int32:1}},
            {name:"n", value:{Int32:1}},
        ], Bitwise.lor(0, MongoCommon.UniqueBit))
        Log.notice("GridFS", "After")
        if(result){
            Log.notice("GridFS", "Indexes was successfully created")
        } else {
            Log.error("GridFS", "Cannot create indexes")
        }
        grid
    }

    /**
     * Write [data] to the [grid].
     * @param grid The grid where the file is stored
     * @param data Content of the file
     * @param id The file identifier
     */
    function write(GridFS.t grid, binary data, Bson.value id, options){
        chunk_options = {chunk_size:options.chunk_size, files_id:id}
        match(Chunk.writes(grid, data, id, chunk_options)){
        case {failure:_} as e : e
        case {success} :
            Log.debug("GridFS", "Chunks are successfully saved")
            file_options = {chunk_options extend
                            length : Binary.length(data),
                            doc : options.doc}
            File.save(grid, id, file_options)
        }
    }

    /**
     * Get binary data stored to the [grid]
     * @param grid The grid where the file is stored
     * @param id The file identifier
     */
    function read(GridFS.t grid, Bson.value id){
        Chunk.read(grid, id)
    }

    /**
     * Delete from the [grid] the file identified by [id].
     * @param grid The grid where the file is stored
     * @param id The file identifier
     */
    function delete(GridFS.t grid, Bson.value id){
        _ = Chunk.delete(grid, id)
        _ = File.delete(grid, id)
        void
    }

    /**
     * Returns a binary iterator from a gridfs file.
     * @param file A gridfs file
     * @return A binary iterator on the file content
     */
    function iter(binary) iterator(GridFS.file file){
        recursive function aux(n, size, file){
            if (n < size){
                match(MongoCommon.reply_document(file.reply, n)){
                case {none} : @fail("Unexpected error: can't retreive document {n}/{size}")
                case {some:doc} :
                    match(Bson.find_value(doc, "data")){
                    case {some : {Binary : bin}} :
                        function next(){ aux(n+1, size, file) }
                        {some : (bin, ~{next})}
                    case _ : @fail("Unexpected error: bad formatted data")
                    }
                }
            } else {
                cursor = MongoCommon.reply_cursorID(file.reply)
                if(MongoCommon.is_null_cursorID(cursor)){ none; }
                else {
                    match(MongoDriver.get_more(file.grid.db, Chunk.chunks_ns(file.grid), 0, cursor)){
                    case {none} : @fail("Can't get {n}th chunk")
                    case {some:reply} :
                        aux(0, MongoCommon.reply_numberReturned(reply),
                            {file with ~reply})
                    }
                }
            }
        }
        {
            function next(){
                aux(0, MongoCommon.reply_numberReturned(file.reply), file)
            }
        }
    }

    /**
     * Returns the binary content from a gridfs file.
     * @param file A gridfs file
     * @return The binary content of the file
     */
    function to_binary(GridFS.file file){
        bin = Binary.create(1024)
        Iter.iter(function(chunk){
            Binary.add_binary(bin, chunk)
        }, iterator(file))
        bin
    }
}


