#include <stdio.h>
#include <stddef.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/compatibility.h>
#include <caml/fail.h>
#include <caml/threads.h>

#ifdef __GNUC__
#define INLINE static __inline__
#else
#define INLINE static
#endif

#define UNTESTEDENDIAN (-1)
#define LITTLEENDIAN 0
#define BIGENDIAN 1
#define UNKNOWNENDIAN (-2)

int endian = UNTESTEDENDIAN;

#define CHV(x,i) ((x>>(byte_size*(sizeof(x)-i-1)))&mask)

void test_endian(void)
{
  char c, ci=0x11;
  int byte_size;
  int x, xi=0x11;
  unsigned int mask, i;

  if (endian != UNTESTEDENDIAN) return;
  c=1; byte_size=0;
  do { c=c<<1; byte_size++; } while(c!=0);
  for (mask=0, i=1; i<=(unsigned)byte_size; i++) mask= (mask<<1)|1;
  for (c=ci,i=0; i<sizeof(x); c+=ci,i++) ((char *)&x)[i]= c;
  if ((CHV(x,0) == (int)(sizeof(x)*xi)) && (CHV(x,(sizeof(x)-1)) == (int)xi)) {
    endian = LITTLEENDIAN;
  } else if ((CHV(x,(sizeof(x)-1)) == (int)(sizeof(x)*xi)) && (CHV(x,0) == (int)xi)) {
    endian = BIGENDIAN;
  } else {
    endian = UNKNOWNENDIAN;
  }
}

INLINE void swap64( void *outp, const void *inp )
{
  const char *in = (const char *)inp;
  char *out = (char *)outp;

  out[0] = in[7]; out[1] = in[6]; out[2] = in[5]; out[3] = in[4];
  out[4] = in[3]; out[5] = in[2]; out[6] = in[1]; out[7] = in[0];
}

INLINE void swap32( void *outp, const void *inp )
{
  const char *in = (const char *)inp;
  char *out = (char *)outp;

  out[0] = in[3]; out[1] = in[2]; out[2] = in[1]; out[3] = in[0];
}

void __i_le_32(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  int i = Int_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_le_32(b,_p,_i); break;
  case LITTLEENDIAN: memcpy(String_val(b)+p, (char *)&i, 4); break;
  case BIGENDIAN: swap32(String_val(b)+p, (char *)&i); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_be_32(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  int i = Int_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_be_32(b,_p,_i); break;
  case BIGENDIAN: memcpy(String_val(b)+p, (char *)&i, 4); break;
  case LITTLEENDIAN: swap32(String_val(b)+p, (char *)&i); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_le_32l(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  int i = Int32_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_le_32l(b,_p,_i); break;
  case LITTLEENDIAN: memcpy(String_val(b)+p, (char *)&i, 4); break;
  case BIGENDIAN: swap32(String_val(b)+p, (char *)&i); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_be_32l(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  int i = Int32_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_le_32l(b,_p,_i); break;
  case LITTLEENDIAN: swap32(String_val(b)+p, (char *)&i); break;
  case BIGENDIAN: memcpy(String_val(b)+p, (char *)&i, 4); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_le_64(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  long i = Long_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_le_64(b,_p,_i); break;
  case LITTLEENDIAN: memcpy(String_val(b)+p, (char *)&i, 8); break;
  case BIGENDIAN: swap64(String_val(b)+p, (char *)&i); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_be_64(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  long i = Long_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_be_64(b,_p,_i); break;
  case BIGENDIAN: memcpy(String_val(b)+p, (char *)&i, 8); break;
  case LITTLEENDIAN: swap64(String_val(b)+p, (char *)&i); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_le_64L(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  long i = Int64_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_le_64L(b,_p,_i); break;
  case LITTLEENDIAN: memcpy(String_val(b)+p, (char *)&i, 8); break;
  case BIGENDIAN: swap64(String_val(b)+p, (char *)&i); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

void __i_be_64L(value b, value _p, value _i)
{
  CAMLparam3(b, _p, _i);
  int p = Int_val(_p);
  long i = Int64_val(_i);

  switch (endian) {
  case UNTESTEDENDIAN: test_endian(); __i_le_64L(b,_p,_i); break;
  case LITTLEENDIAN: swap64(String_val(b)+p, (char *)&i); break;
  case BIGENDIAN: memcpy(String_val(b)+p, (char *)&i, 8); break;
  default: caml_failwith("Unknown endianness"); break;
  }
  CAMLreturn0;
}

/*
int main(argc, argv)
int argc; char *argv[];
{
  test_endian();
  switch (endian) {
  case UNTESTEDENDIAN: printf("untested endian\n"); break;
  case LITTLEENDIAN: printf("little endian\n"); break;
  case BIGENDIAN: printf("big endian\n"); break;
  default: printf("unknown endian\n"); break;
  }
  return(0);
}
*/

