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
#include "nessie.h"
#include "whirlpool-T.h"
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#define Context_val(v) ((struct NESSIEstruct *) String_val(v))

CAMLprim value caml_whirlpool_init(value unit)
{
  value ctx = alloc_string(sizeof(struct NESSIEstruct));
  NESSIEinit(Context_val(ctx));
  return ctx;
}

CAMLprim value caml_whirlpool_update(value ctx, value src, value ofs, value len)
{
  /* /!\ len * 8 because whirlpool takes the length in bits not in bytes */
  NESSIEadd(&Byte_u(src, Long_val(ofs)), Long_val(len)*8, Context_val(ctx));
  return Val_unit;
}

CAMLprim value caml_whirlpool_final(value ctx)
{
  CAMLparam1(ctx);
  CAMLlocal1(res);

  res = alloc_string(64);
  NESSIEfinalize(Context_val(ctx), &Byte_u(res, 0));
  CAMLreturn(res);
}

