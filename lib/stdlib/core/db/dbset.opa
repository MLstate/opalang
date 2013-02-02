
package stdlib.core.db

@abstract type DbSet.t('a, 'engine) = {
  iter : iter('a);
  engine : 'engine;
}

@opacapi type dbset('a, 'engine) = DbSet.t('a, 'engine)

DbSet = {{
  @package build(iter, engine):dbset = ~{iter engine}

  fold(init, dbset:dbset(_, _))(f) =
      Iter.fold(acc, e -> f(e, acc), dbset.iter, init)

  iterator(dbset:dbset('a, _)):iter('a) = dbset.iter

  @stringifier(dbset('a, _)) to_string(dbset) =
    tx = Text.cons("Database Set:\n")
    (_, tx) = fold((0, tx), dbset)((i, tx), a ->
      tx = Text.insert_right(tx, "  {i}")
        |> Text.insert_right(_, " : ")
        |> Text.insert_right(_, "{a}")
        |> Text.insert_right(_, ",\n")
      (i+1, tx)
    )
    Text.to_string(tx)

  engine(dbset:dbset('a, 'engine)):'engine = dbset.engine

  map(f : 'a -> 'b, dbset:dbset('a, 'engine)) : dbset('b, 'engine) =
    {iter = Iter.map(f, dbset.iter); engine = dbset.engine}

}}

@opacapi DbSet_genbuild = DbSet.build
@opacapi DbSet_map = DbSet.map
