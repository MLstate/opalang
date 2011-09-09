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

import stdlib.io.socket
import stdlib.core.{rpc.core}

type mongo_buf = external
type cursorID = external
type reply = external

type mongo = {
     conn : Socket.connection;
     mbuf : mongo_buf
  }

//@both <-- ??? why doesn't this work ???
Mongo = {{

  /* Flags */

  /* OP_INSERT */
  _ContinueOnError  = 0x00000001

  /* OP_UPDATE */
  _Upsert           = 0x00000001
  _MultiUpdate      = 0x00000002

  /* OP_QUERY */
  _TailableCursor   = 0x00000002
  _SlaveOk          = 0x00000004
  _OplogReplay      = 0x00000008
  _NoCursorTimeout  = 0x00000010
  _AwaitData        = 0x00000020
  _Exhaust          = 0x00000040
  _Partial          = 0x00000080

  /* OP_DELETE */
  _SingleRemove     = 0x00000001

  /* OP_REPLY */
  _CursorNotFound   = 0x00000001
  _QueryFailure     = 0x00000002
  _ShardConfigStale = 0x00000003
  _AwaitCapable     = 0x00000004

    /** Allocate new buffer of given size **/
  @private create_ = (%% BslMongo.Mongo.create %%: int -> mongo_buf)

  /** Build OP_INSERT message in buffer **/
  @private insert_ = (%% BslMongo.Mongo.insert %%: mongo_buf, int, string, 'a -> void)

  /** Build OP_UPDATE message in buffer **/
  @private update_ = (%% BslMongo.Mongo.update %%: mongo_buf, int, string, 'a, 'a -> void)

  /** Build OP_QUERY message in buffer **/
  @private query_ = (%% BslMongo.Mongo.query %%: mongo_buf, int, string, int, int, 'a, option('a) -> void)

  /** Build OP_GET_MORE message in buffer **/
  @private get_more_ = (%% BslMongo.Mongo.get_more %%: mongo_buf, string, int, cursorID -> void)

  /** Build OP_DELETE message in buffer **/
  @private delete_ = (%% BslMongo.Mongo.delete %%: mongo_buf, int, string, 'a -> void)

  /** Build OP_KILL_CURSORS message in buffer **/
  @private kill_cursors_ = (%% BslMongo.Mongo.kill_cursors %%: mongo_buf, list(cursorID) -> void)

  /** Build OP_MSG message in buffer **/
  @private msg_ = (%% BslMongo.Mongo.msg %%: mongo_buf, string -> void)

  /** Copies string out of buffer. **/
  @private get_ = (%% BslMongo.Mongo.get %%: mongo_buf -> string)

  /** Access the raw string and length **/
  @private export_ = (%% BslMongo.Mongo.export %%: mongo_buf -> (string, int))

  /** Clear out any data in the buffer, leave buffer allocated **/
  @private clear_ = (%% BslMongo.Mongo.clear %%: mongo_buf -> void)

  /** Reset the buffer, unallocate storage **/
  @private reset_ = (%% BslMongo.Mongo.reset %%: mongo_buf -> void)

  /**
   * Specialised read, read until the size equals the (little endian)
   * 4-byte int at the start of the reply.
   **/
  // Unable to type bypass
  //  bslmongo_read_mongo.
  // Sigh.
  //@private read_mongo = (%% BslMongo.read_mongo %%: Socket.connection, int -> reply)

  /**
   *  Create new mongo object:
   *    - Open connection to mongo server at addr:port
   *    - Allocate buffer of given size
   **/
  open(bufsize,addr,port): mongo =
    { conn = Socket.connect(addr,port);
      mbuf = create_(bufsize)
    }

  @private
  send_no_reply(m,name): bool =
    match export_(m.mbuf) with
    | (str, len) ->
      s = String.substring(0,len,str)
      do println("{name}: s=\n{Bson.dump(10)(s)}")
      cnt = Socket.write_len(m.conn,s,len)
      do println("cnt={cnt} len={len}")
      (cnt==len)

  @private
  send_with_reply(m,name): option(string) =
    match export_(m.mbuf) with
    | (str, len) ->
      s = String.substring(0,len,str)
      do println("{name}: s=\n{Bson.dump(10)(s)}")
      cnt = Socket.write_len(m.conn,s,len)
      do println("cnt={cnt} len={len}")
      if (cnt==len)
      then {some=Socket.read(m.conn)}
      else {none}

  /**
   *  Send OP_INSERT with given collection name:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  insert(m,flags,ns,documents): bool =
    do insert_(m.mbuf,flags,ns,documents)
    send_no_reply(m,"insert")

  /**
   *  Send OP_UPDATE with given collection name:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  update(m,flags,ns,selector,update): bool =
    do update_(m.mbuf,flags,ns,selector,update)
    send_no_reply(m,"update")

  /**
   *  Send OP_QUERY and get reply:
   **/
  query(m,flags,ns,numberToSkip,numberToReturn,query,returnFieldSelector_opt): option(string) =
    do query_(m.mbuf,0,flags,ns,numberToSkip,numberToReturn,query,returnFieldSelector_opt)
    send_with_reply(m,"query")

  /**
   *  Send OP_GETMORE and get reply:
   **/
  getmore(m,ns,numberToReturn,cursorID): option(string) =
    do get_more_(m.mbuf,0,ns,numberToReturn,cursorID)
    send_with_reply(m,"getmore")

  /**
   *  Send OP_DELETE:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  delete(m,flags,ns,selector): bool =
    do delete_(m.mbuf,flags,ns,selector)
    send_no_reply(m,"delete")

  /**
   *  Send OP_KILL_CURSORS:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  kill_cursors(m,cursors): bool =
    do kill_cursors_(m.mbuf,cursors)
    send_no_reply(m,"kill_cursors")

  /**
   *  Send OP_MSG:
   *    - no reply expected
   *    - returns a bool indicating success or failure
   **/
  msg(m,msg): bool =
    do msg_(m.mbuf,msg)
    send_no_reply(m,"msg")

  /**
   *  Close mongo connection and deallocate buffer.
   **/
  close(m) =
    do Socket.close(m.conn)
    do reset_(m.mbuf)
    void

}}

