import stdlib.core.iter
package stdlib.core.db

@abstract type DbSet.t('a, 'engine) = {
  iter : iter('a);
  engine : 'engine;
}

@opacapi type dbset('a, 'engine) = DbSet.t('a, 'engine)

DbSet = {{
  @package build(iter, engine) = ~{iter engine}

  fold(init, dbset:dbset(_, _))(f) =
      Iter.fold(acc, e -> f(e, acc), dbset.iter, init)

  iterator(dbset:dbset('a, _)):iter('a) = dbset.iter

  @stringifier(dbset('a, _)) to_string(fa, _, dbset) =
    tx = Text.cons("Database Set:\n")
    (_, tx) = fold((0, tx), dbset)((i, tx), a ->
      tx = Text.insert_right(tx, "  {i}")
        |> Text.insert_right(_, " : ")
        |> Text.insert_right(_, fa(a))
        |> Text.insert_right(_, ",\n")
      (i+1, tx)
    )
    Text.to_string(tx)

}}

@opacapi DbSet_genbuild = DbSet.build
