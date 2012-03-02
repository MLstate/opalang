import stdlib.core.iter
package stdlib.core.db

@abstract type DbSet.t('a, 'engine) = {
  iter : iter('a);
  engine : 'engine;
}

@opacapi type dbset('a, 'engine) = DbSet.t('a, 'engine)

DbSet = {{
    fold(init, dbset:dbset(_, _))(f) =
        Iter.fold(acc, e -> f(e, acc), dbset.iter, init)

    iterator(dbset:dbset('a, _)):iter('a) = dbset.iter
}}
