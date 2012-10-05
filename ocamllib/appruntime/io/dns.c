/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa.  If not, see <http://www.gnu.org/licenses/>.
*/
#include <stdio.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>


#include "mlstate_platform.h"

#ifdef MLSTATE_WINDOWS

#include <Windows.h>
#include <Windns.h>

CAMLprim value get_mx_dns(value v_domain)
{
  CAMLparam1(v_domain);
  CAMLlocal2(v_res, v_tuple);

  PDNS_RECORD pDns = NULL;
  if (DnsQuery_A(String_val(v_domain),
		 DNS_TYPE_MX,
		 DNS_QUERY_STANDARD,
		 NULL,
		 &pDns,
		 NULL) == NOERROR)
    {
      PDNS_RECORD cur = pDns;
      int count = 0;
      int i = 0;

      for (cur = pDns; cur != NULL; cur = cur->pNext)
	if (cur->wType == DNS_TYPE_MX) count++;

      v_res = caml_alloc(count, 0);
      for (cur = pDns; cur != NULL; cur = cur->pNext)
	if (cur->wType == DNS_TYPE_MX)
	  {
	    value str = caml_copy_string(cur->Data.Mx.pNameExchange);
	    v_tuple = caml_alloc_small(2, 0);
	    printf("dns: %s\n", cur->Data.Mx.pNameExchange);
	    Field(v_tuple, 0) = str;
	    Field(v_tuple, 1) = Val_int(cur->Data.Mx.wPreference);
	    Store_field(v_res, i++, v_tuple);
	  }

      DnsRecordListFree(pDns, DnsFreeRecordList);
      CAMLreturn(v_res);
    }

  caml_failwith("No MX record found");
  CAMLreturn(Val_unit);
}
#else

value error_cygwin(char* mess){
  printf("Iocp : error_unix_or_cywin :%s",mess);
  exit(1);
  return Val_unit;
}

CAMLprim value get_mx_dns(value v_domain){ return error_cygwin("Dns : get_mx_dns"); }
#endif
