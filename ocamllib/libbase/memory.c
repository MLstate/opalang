/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
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

#elif (defined(__FreeBSD__) || defined(__FreeBSD_kernel__)) /* FreeBSD */

#include <kvm.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/user.h>

int get_mem (unsigned int *rss)
{
    kvm_t *kv;
    int cnt,pagesize,pid;

    pagesize = getpagesize();
    pid = getpid();
    kv = kvm_open(NULL, "/dev/null", NULL, O_RDONLY, NULL);

    if (kv != NULL) {
        struct kinfo_proc *kproc;
        kproc = kvm_getprocs(kv, KERN_PROC_PID, pid, &cnt);
        if (kproc && cnt > 0)
            *rss = kproc->ki_rssize * pagesize;
        else *rss = 0;
        kvm_close(kv);
        return 0;
    } else return -1;
}

#else /* not MAC and FreeBSD */

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
