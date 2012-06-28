#include <stdio.h>
#include <sys/time.h>
#include <string.h>
#include <unistd.h>

unsigned long long int rdtsc(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
}

int freq()
{
    struct timezone tz;
    struct timeval tvstart, tvstop;
    unsigned long long int cycles[2];
    unsigned long microseconds;
    int mhz;

    memset(&tz, 0, sizeof(tz));

    gettimeofday(&tvstart, &tz);
    cycles[0] = rdtsc();
    gettimeofday(&tvstart, &tz);

    usleep(250000);

    gettimeofday(&tvstop, &tz);
    cycles[1] = rdtsc();
    gettimeofday(&tvstop, &tz);

    microseconds = ((tvstop.tv_sec-tvstart.tv_sec)*1000000) + (tvstop.tv_usec-tvstart.tv_usec);

    mhz = (int) (cycles[1]-cycles[0]) / microseconds;

    //        printf("%i MHz\n",mhz);

    return mhz;
}

// OCAML binding
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/compatibility.h>
#include <caml/fail.h>

value freq_get()
{
    CAMLparam0();
    CAMLreturn(Val_int(freq()));
}
