/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/

#include <caml/memory.h>

#if (defined(__APPLE__) && defined(__MACH__)) /* MAC */

#include <mach/task.h>
#include <mach/mach.h>

// http://blog.kuriositaet.de/?p=257
int get_mem (unsigned int *rss)
{
    struct task_basic_info t_info;
    mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;

    if (KERN_SUCCESS != task_info(mach_task_self(),
       TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count))
    {
        return -1;
    }
    *rss = t_info.resident_size;
    return 0;
}

#else /* not MAC */

#include <stdio.h>
#include <unistd.h>

int get_mem (unsigned int *rss)
{
    FILE* f = fopen("/proc/self/statm", "r");
    unsigned int res = -1;
    long page_size = getpagesize();
    if (!f) return -1;
    if (fscanf(f, "%d\n", &res) != 1) return -1;
    *rss = res*page_size;
    fclose(f);
    return 0;
}

#endif

long usage() {
  unsigned int rss = -1;
  get_mem(&rss);
  return rss;
}

value get_memory_usage() {
  CAMLparam0();
  CAMLreturn(Val_long(usage()));
}
