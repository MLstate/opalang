#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/intext.h>

#include <iconv.h>
#include <string.h>
#include <errno.h>

struct mliconv_t {
	iconv_t handle;
	char *tocode;
	char *fromcode;
};

static inline struct mliconv_t *mliconv_val(value data)
{
	return (struct mliconv_t *)(Data_custom_val(data));
}

static void mliconv_finalize(value r);
static int mliconv_compare(value left, value right);
static long mliconv_hash(value data);
static void mliconv_serialize(value v, unsigned long * wsize_32, unsigned long * wsize_64);
static unsigned long mliconv_deserialize(void * dst);

static struct custom_operations iconv_ops = {
	"jp.halfmoon.panathenaia.iconv",
	mliconv_finalize,
	mliconv_compare,
	mliconv_hash,
	mliconv_serialize,
	mliconv_deserialize};

static void mliconv_finalize(value data)
{
	CAMLparam1(data);
	struct mliconv_t *internal = mliconv_val(data);
	iconv_close(internal->handle);
	free(internal->tocode);
	free(internal->fromcode);
	CAMLreturn0;
}

static int mliconv_compare(value left, value right)
{
	CAMLparam2(left, right);
	struct mliconv_t *left_internal = mliconv_val(left);
	struct mliconv_t *right_internal = mliconv_val(right);
	int result = strcmp(left_internal->tocode, right_internal->tocode);
	if(result == 0){
		result = strcmp(left_internal->fromcode, right_internal->fromcode);
	}
	CAMLreturn(result);
}

static long mliconv_hash(value data)
{
	CAMLparam1(data);
	struct mliconv_t *internal = mliconv_val(data);
	long result = (strlen(internal->tocode) << 4) + strlen(internal->fromcode);
	CAMLreturn(result);
}

static void mliconv_serialize(value v, unsigned long * wsize_32, unsigned long * wsize_64)
{
	CAMLparam1(v);
	*wsize_32 = 4 * 3;
	*wsize_64 = 8 * 3;
	struct mliconv_t *internal = mliconv_val(v);
	size_t to_len = strlen(internal->tocode);
	serialize_int_4(to_len);
	serialize_block_1(internal->tocode, to_len);
	size_t from_len = strlen(internal->fromcode);
	serialize_int_4(from_len);
	serialize_block_1(internal->fromcode, from_len);
	CAMLreturn0;
}

static unsigned long mliconv_deserialize(void * dst)
{
	CAMLparam0();
	size_t to_len = deserialize_uint_4();
	char *tocode = malloc(to_len + 1);
	deserialize_block_1(tocode, to_len);
	size_t from_len = deserialize_uint_4();
	char *fromcode = malloc(from_len + 1);
	deserialize_block_1(fromcode, from_len);
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(strcat(strcat(strcpy(message, "failed iconv_open to "), tocode), " from "), fromcode);;
		free(tocode);
		free(fromcode);
		caml_failwith(message);
	}
	struct mliconv_t *internal = (struct mliconv_t *)dst;
	internal->handle = handle;
	internal->tocode = tocode;
	internal->fromcode = fromcode;
	CAMLreturn(sizeof(struct mliconv_t));
}

CAMLprim value mliconv_open(value tocodev, value fromcodev)
{
	CAMLparam2(tocodev, fromcodev);
	CAMLlocal1(result);
	const char* tocode = String_val(tocodev);
	size_t to_len = caml_string_length(tocodev);
	const char* fromcode = String_val(fromcodev);
	size_t from_len = caml_string_length(fromcodev);
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(strcat(strcat(strcpy(message, "failed iconv_open to "), tocode), " from "), fromcode);;
		caml_failwith(message);
	}
	result = alloc_custom(&iconv_ops, sizeof(struct mliconv_t), 0, 1);
	struct mliconv_t *internal = mliconv_val(result);
	internal->handle = handle;
#ifdef _LIBICONV_H
	internal->tocode = strdup(iconv_canonicalize(tocode));
	internal->fromcode = strdup(iconv_canonicalize(fromcode));
#else	/* iconv_canonicalize does not exist in gconv */
	internal->tocode = strdup(tocode);
	internal->fromcode = strdup(fromcode);
#endif
	CAMLreturn(result);
}

CAMLprim value mliconv_convert(value conv, value source)
{
	CAMLparam2(conv, source);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
#if (defined(__FreeBSD__) || defined(__FreeBSD_kernel__))
 	const
#endif
	char *s = String_val(source);
	size_t s_len = caml_string_length(source);
	size_t d_len = s_len * 6;
	char *d = malloc(d_len);
	char *d_current = d;
	while(s_len > 0){
		if(iconv(internal->handle, &s, &s_len, &d_current, &d_len) == (size_t)-1){
			int e = errno;
			if(e == EILSEQ || e == EINVAL){
				*d_current = '?';
				++ d_current;
				-- d_len;
				++ s;
				-- s_len;
			}else{
				free(d);
				caml_failwith("failed iconv");
			}
		}
	}
	size_t result_len = d_current - d;
	result = caml_alloc_string(result_len);
	memcpy(String_val(result), d, result_len);
	free(d);
	CAMLreturn(result);
}

CAMLprim value mliconv_tocode(value conv)
{
	CAMLparam1(conv);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
	result = caml_copy_string(internal->tocode);
	CAMLreturn(result);
}

CAMLprim value mliconv_fromcode(value conv)
{
	CAMLparam1(conv);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
	result = caml_copy_string(internal->fromcode);
	CAMLreturn(result);
}
