/*
    Copyright © 2011 MLstate

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
/*
  @author Frederic Ye
**/

/* Warning: The following code is highly inspired by Ocaml-ssl binding
   -> TODO: Check Ocaml-ssl licence */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <openssl/ssl.h>
#include <openssl/pem.h>
#include <openssl/err.h>
#include <openssl/crypto.h>
#include <openssl/bn.h>
#include <openssl/engine.h>

// SSL
/* Some definitions from Ocaml-SSL */
#define Cert_val(v) (*((X509**)Data_custom_val(v)))
#define RSA_val(v) (*((RSA**)Data_custom_val(v)))
#define Ctx_val(v) (*((SSL_CTX**)Data_custom_val(v)))
#define SSL_val(v) (*((SSL**)Data_custom_val(v)))
#define ONELINE_NAME(X) X509_NAME_oneline(X, 0, 0)

#include "../libbase/mlstate_platform.h"

#ifdef MLSTATE_WINDOWS
#include <io.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#include <string.h>
#include <assert.h>


CAMLprim value ocaml_ssl_ext_init(void) {
  /* ERR_load_crypto_strings(); */
  OpenSSL_add_all_digests();
  OpenSSL_add_all_ciphers();
  OpenSSL_add_all_algorithms();
  /* ENGINE_load_builtin_engines(); */
  return Val_unit;
}

static int s_server_session_id_context = 1; /* anything will do */
CAMLprim value ocaml_ssl_ext_ctx_set_session_id_context(value context)
{
  CAMLparam1(context);
  SSL_CTX *ctx = Ctx_val(context);

  caml_enter_blocking_section();
  SSL_CTX_set_session_id_context(ctx,(void*)&s_server_session_id_context,
				 sizeof s_server_session_id_context);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ssl_ext_ctx_set_options(value context)
{
  CAMLparam1(context);
  long ans;
  SSL_CTX *ctx = Ctx_val(context);

  caml_enter_blocking_section();
  ans = SSL_CTX_set_options(ctx, SSL_OP_ALL | SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION);
  caml_leave_blocking_section();

  CAMLreturn(Val_long(ans));
}

CAMLprim value ocaml_ssl_ext_ctx_set_accept_renegotiate(value context)
{
  CAMLparam1(context);
  long ans;
  SSL_CTX *ctx = Ctx_val(context);

  caml_enter_blocking_section();
  ans = SSL_CTX_sess_accept_renegotiate(ctx);
  caml_leave_blocking_section();

  CAMLreturn(Val_long(ans));
}

static int no_client_verify_callback(int, X509_STORE_CTX *);

CAMLprim value ocaml_ssl_ext_get_no_client_verify_callback_ptr(value unit)
{
  return (value)no_client_verify_callback;
}

CAMLprim value ocaml_ssl_ext_get_signature(value certificate)
{
  CAMLparam1(certificate);
  X509 *cert = Cert_val(certificate);
  unsigned char *signature ; //= cert->signature->data;

  caml_enter_blocking_section();
  signature = cert->signature->data;

  caml_leave_blocking_section();
  if (signature == NULL) caml_raise_not_found ();

  CAMLreturn(caml_copy_string(String_val(signature)));
}

CAMLprim value ocaml_ssl_ext_get_hash(value certificate)
{
  CAMLparam1(certificate);
  X509 *cert = Cert_val(certificate);
  unsigned char *hash ; //= cert->sha1_hash;

  caml_enter_blocking_section();
  hash = cert->sha1_hash;
  caml_leave_blocking_section();
  if (hash == NULL) caml_raise_not_found ();

  CAMLreturn(caml_copy_string(String_val(hash)));
}

CAMLprim value ocaml_ssl_ext_compute_digest(value certificate, value dname)
{
  CAMLparam2(certificate, dname);
  X509 *cert = Cert_val(certificate);
  char *digest_name = String_val(dname);
  unsigned int n;
  unsigned char digest[64];

  const EVP_MD *fdig = EVP_get_digestbyname(digest_name);

  caml_enter_blocking_section();
  // @see evp.h #define EVP_MAX_MD_SIZE 64 /* longest known is SHA512 */

  X509_digest(cert,fdig,digest,&n);
  caml_leave_blocking_section();
  if (digest == NULL) caml_raise_not_found ();
  
  CAMLreturn(caml_copy_string(String_val(digest)));
}

static int no_client_verify_callback(int ok, X509_STORE_CTX *ctx)
{
  char *subject, *issuer;
  unsigned char *signature;
  char *xs;

  xs = (char *)X509_STORE_CTX_get_current_cert(ctx);

  subject = issuer = NULL;
  signature = NULL;

  /* First thing is to have a meaningful name for the current
   * certificate that is being verified ... and if we cannot
   * determine that then something is seriously wrong!
   */
   subject=(char*)ONELINE_NAME(X509_get_subject_name((X509*)xs)); 
   if (subject == NULL) 
   { 
     ERR_print_errors_fp(stderr); 
     ok = 0; 
     goto return_time; 
   } 
   issuer = (char*)ONELINE_NAME(X509_get_issuer_name((X509*)xs)); 
   if (issuer == NULL) 
   { 
     ERR_print_errors_fp(stderr); 
     ok = 0; 
     goto return_time; 
   } 
  signature = (((X509*)xs)->signature)->data;
  if (signature == NULL) {
    ERR_print_errors_fp(stderr);
    ok = 0;
    goto return_time;    
  }

  ok = 1;

return_time:

  /* Clean up things. */
  if (subject)
    free(subject);
  if (issuer)
    free(issuer);

  return ok;
}

void print_ssl_error(err) {
  switch (err) {
  case SSL_ERROR_NONE: printf("SSL_ERROR_NONE\n"); break;
  case SSL_ERROR_ZERO_RETURN: printf("SSL_ERROR_ZERO_RETURN\n"); break;
  case SSL_ERROR_WANT_READ: printf("SSL_ERROR_WANT_READ\n"); break;
  case SSL_ERROR_WANT_WRITE: printf("SSL_ERROR_WANT_WRITE\n"); break;
  case SSL_ERROR_WANT_CONNECT: printf("SSL_ERROR_WANT_CONNECT\n"); break;
  case SSL_ERROR_WANT_ACCEPT: printf("SSL_ERROR_WANT_ACCEPT\n"); break;
  case SSL_ERROR_WANT_X509_LOOKUP: printf("SSL_ERROR_WANT_X509_LOOKUP\n"); break;
  case SSL_ERROR_SYSCALL: printf("SSL_ERROR_SYSCALL\n"); break;
  case SSL_ERROR_SSL: printf("SSL_ERROR_SSL\n"); break;
  default: printf("...\n"); break;
  }
}

CAMLprim value ocaml_ssl_ext_renegotiate(value socket)
{
  CAMLparam1(socket);
  SSL *ssl = SSL_val(socket);
  int ret;
  caml_enter_blocking_section();
  ret = SSL_renegotiate(ssl);
  caml_leave_blocking_section();
  if(ret <= 0)
    caml_raise_constant(*caml_named_value("ssl_ext_exn_renegotiation_error"));
  CAMLreturn(Val_unit);
}


/* Very basic implementation of a certificate chain check:
   Must be greatly improved!!! */
CAMLprim value ocaml_ssl_ext_verify_chain(value certificate, value caf)
{
  CAMLparam2(certificate, caf);
  char *cafile = String_val(caf);
  X509 *cert = Cert_val(certificate);
  int i = 0;
  X509_STORE *ctx=X509_STORE_new();
  X509_LOOKUP *lookup=X509_STORE_add_lookup(ctx,X509_LOOKUP_file());
  X509_STORE_CTX *csc=X509_STORE_CTX_new();
  
/*   printf("CAfile %s\n", cafile); */
/*   printf("Certificate: "); */
/*   X509_NAME_print_ex_fp(stdout, */
/* 			X509_get_subject_name(cert), */
/* 			0, XN_FLAG_ONELINE); */
/*   printf("\n"); */

 
  if (ctx == NULL) printf("ctx error\n");

  OpenSSL_add_all_algorithms();

  if (lookup == NULL) printf("lookup error\n");
  if (!X509_LOOKUP_load_file(lookup,cafile,X509_FILETYPE_PEM)) {
    printf("error load file\n");
  }

  if (csc == NULL) printf("csc error\n");

  X509_STORE_set_flags(ctx, 0);
  if (!X509_STORE_CTX_init(csc,ctx,cert,NULL)) {
    printf("store init error\n");
  }

  i = X509_verify_cert(csc);

  X509_STORE_CTX_free(csc);
  X509_STORE_free(ctx);

  CAMLreturn(Val_int(i));
}

CAMLprim value ocaml_ssl_ext_do_handshake(value socket)
{
  CAMLparam1(socket);
  int ret;
  int err;
  SSL *ssl = SSL_val(socket);

  caml_enter_blocking_section();
  ret = SSL_do_handshake(ssl);
  err = SSL_get_error(ssl, ret);
  caml_leave_blocking_section();
  if (err != SSL_ERROR_NONE)
    caml_raise_with_arg(*caml_named_value("ssl_ext_exn_handshake_error"), Val_int(err));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ssl_ext_set_verify(value socket, value vmode, value vcallback)
{
  CAMLparam3(socket, vmode, vcallback);
  SSL *ssl = SSL_val(socket);
  int mode = 0;
  value mode_tl = vmode;
  int (*callback) (int, X509_STORE_CTX*) = NULL;

  if (Is_long(vmode))
    mode = SSL_VERIFY_NONE;

  while (Is_block(mode_tl))
  {
    switch(Int_val(Field(mode_tl, 0)))
    {
      case 0:
        mode |= SSL_VERIFY_PEER;
        break;

      case 1:
        mode |= SSL_VERIFY_FAIL_IF_NO_PEER_CERT | SSL_VERIFY_PEER;
        break;

      case 2:
        mode |= SSL_VERIFY_CLIENT_ONCE | SSL_VERIFY_PEER;
        break;

      default:
        caml_invalid_argument("mode");
    }

    mode_tl = Field(mode_tl, 1);
  }

  if (Is_block(vcallback))
    callback = (int(*) (int, X509_STORE_CTX*))Field(vcallback, 0);

  caml_enter_blocking_section();
  SSL_set_verify(ssl, mode, callback);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ssl_ext_set_verify_depth(value socket, value vdepth)
{
  SSL *ssl = SSL_val(socket);
  int depth = Int_val(vdepth);

  if (depth < 0)
    caml_invalid_argument("depth");

  caml_enter_blocking_section();
  SSL_set_verify_depth(ssl, depth);
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value ocaml_ssl_ext_set_accept_state(value socket)
{
  SSL *ssl = SSL_val(socket);

  caml_enter_blocking_section();
  SSL_set_accept_state(ssl);
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value ocaml_ssl_ext_set_state(value socket, value state)
{
  SSL *ssl = SSL_val(socket);
  int st = Int_val(state);

  caml_enter_blocking_section();
  ssl->state=st;
  caml_leave_blocking_section();

  return Val_unit;
}

CAMLprim value ocaml_ssl_ext_get_verify_mode(value socket)
{
  CAMLparam1(socket);
  int ans;
  SSL *ssl = SSL_val(socket);

  caml_enter_blocking_section();
  ans = SSL_get_verify_mode(ssl);
  caml_leave_blocking_section();

  CAMLreturn(Val_int(ans));
}

CAMLprim value ocaml_ssl_ext_get_state(value socket)
{
  CAMLparam1(socket);
  int ans;
  SSL *ssl = SSL_val(socket);

  caml_enter_blocking_section();
  ans = SSL_get_state(ssl);
  caml_leave_blocking_section();

  CAMLreturn(Val_int(ans));
}

CAMLprim value ocaml_ssl_ext_clear(value socket)
{
  CAMLparam1(socket);
  int ans;
  SSL *ssl = SSL_val(socket);

  caml_enter_blocking_section();
  ans = SSL_clear(ssl);
  caml_leave_blocking_section();

  CAMLreturn(Val_int(ans));
}

CAMLprim value ocaml_ssl_ext_num_renegotiations(value socket)
{
  CAMLparam1(socket);
  long ans;
  SSL *ssl = SSL_val(socket);

  caml_enter_blocking_section();
  ans = SSL_num_renegotiations(ssl);
  caml_leave_blocking_section();

  CAMLreturn(Val_long(ans));
}

// RSA

#define RSA_val(v) (*((RSA**)Data_custom_val(v)))

/* Convert a BIGNUM into a string */
char *bn_to_hex(const BIGNUM *bn)
{
  char *res = "";
  caml_enter_blocking_section();
  if (bn != NULL)
    res = BN_bn2hex(bn);
  caml_leave_blocking_section();
  return res;
}

CAMLprim value ocaml_ssl_ext_rsa_read_privkey(value vfilename)
{
  value block;
  char *filename = String_val(vfilename);
  RSA *rsa = NULL;
  FILE *fh = NULL;

  if((fh = fopen(filename, "r")) == NULL)
    caml_raise_constant(*caml_named_value("ssl_ext_exn_rsa_error"));

  caml_enter_blocking_section();
  if((PEM_read_RSAPrivateKey(fh, &rsa, PEM_def_callback, NULL)) == NULL)
  {
    fclose(fh);
    caml_leave_blocking_section();
    caml_raise_constant(*caml_named_value("ssl_ext_exn_rsa_error"));
  }
  fclose(fh);
  caml_leave_blocking_section();

  block = caml_alloc(sizeof(RSA*), 0);
  RSA_val(block) = rsa;
  return block;
}

CAMLprim value ocaml_ssl_ext_rsa_read_pubkey(value vfilename)
{
  value block;
  char *filename = String_val(vfilename);
  RSA *rsa = NULL;
  FILE *fh = NULL;

  if((fh = fopen(filename, "r")) == NULL)
    caml_raise_constant(*caml_named_value("ssl_ext_exn_rsa_error"));

  caml_enter_blocking_section();
  if((PEM_read_RSA_PUBKEY(fh, &rsa, PEM_def_callback, NULL)) == NULL)
  {
    fclose(fh);
    caml_leave_blocking_section();
    caml_raise_constant(*caml_named_value("ssl_ext_exn_rsa_error"));
  }
  fclose(fh);
  caml_leave_blocking_section();

  block = caml_alloc(sizeof(RSA*), 0);
  RSA_val(block) = rsa;
  return block;
}

CAMLprim value ocaml_ssl_ext_rsa_get_size(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  int size = 0;
  caml_enter_blocking_section();
  size = RSA_size(rsa);
  caml_leave_blocking_section();
  CAMLreturn(Val_int(size));
}

CAMLprim value ocaml_ssl_ext_rsa_get_n(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->n))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_e(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->e))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_d(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->d))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_p(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->p))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_q(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->q))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_dp(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->dmp1))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_dq(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->dmq1))));
}

CAMLprim value ocaml_ssl_ext_rsa_get_qinv(value key)
{
  CAMLparam1(key);
  RSA *rsa = RSA_val(key);
  CAMLreturn(caml_copy_string(String_val(bn_to_hex(rsa->iqmp))));
}

