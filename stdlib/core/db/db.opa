package stdlib.core.db

/**
 * {1 About this module}
 *
 * {1 Where do I start?}
 *
 * {1 What if I need more?}
 *
 */

/**
 * {1 Types defined in this module}
 */

@abstract type Db.t('a) = 'a

@abstract type Db.path('kind, 'data, 'engine) = {
  id      : string /* Used for debugging stuff */;
  read    : -> 'data
  exists  : -> bool
  more    : 'kind
  engine  : 'engine
}

type Db.builder('kind, 'data, 'engine) = {
  id      : string /* Used for debugging stuff */;
  read    : -> 'data
  exists  : -> bool
  more    : 'kind
  engine  : 'engine
}

@opacapi type Db.val_path('data, 'engine) = Db.path(void, 'data, 'engine)

type Db.ref_kind('data) = {
  remove : -> void
  write : 'data -> bool
}

@opacapi type Db.ref_path('data, 'engine) = Db.path(Db.ref_kind('data), 'data, 'engine)

Db = {{

  /**
   * {1 Read access}
   */

  /**
   * Checks existence of a path, i.e. a data is present at the given path.
   */
  exists(path:Db.path(_,_,_)) = path.exists()

  /**
   * Reads the data on the given path.
   */
  read(path:Db.path(_,_,_)) = path.read()

  /**
   * {1 Write access}
   */

  /**
   * Write data at the given path in the database.
   */
  write(path:Db.ref_path('data, _), data:'data) =
    match path.more.write(data) with
    | {false} -> Log.error("Database", "An error occurs while writing at path '{path.id}'")
    | {true} -> void

  /**
   * Operator for write path.
   * @example [Db.write(path, data)] is equivalent to [path <- data].
   */
  `<-`(d,a) = write(d,a)

  /**
   * Removes the data held at a path.
   */
  remove(path:Db.ref_path(_, _)):void = path.more.remove()

  /**
   * {1 Utils}
   */

  /**
   * Turns a reference-path into a value-path
   *
   * @example [get_val(@/path)] is equivalent to [!/path]
   */
  get_val(path:Db.ref_path('data, 'engine)):Db.val_path('data, 'engine) =
    {path with more=void}

  get_engine(path:Db.path(_,_,'engine)):'engine = path.engine

  build(builder:Db.builder('a, 'b, 'c)):Db.path('a, 'b, 'c) = builder

}}

`<-` = Db.`<-`
