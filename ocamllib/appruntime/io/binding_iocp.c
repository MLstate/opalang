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
/*
  @author Laurent Le Brun
**/

/**
    socket : unit -> Unix.file_descr = "iocp_ml_socket"
    async_wait : int -> int = "wait"
    async_accept : Unix.file_descr -> int = "iocp_ml_accept"
    async_init : unit -> unit = "async_init"
    async_read : Unix.file_descr -> int -> int = "iocp_ml_read"
    async_write : Unix.file_descr -> string -> int -> int = "iocp_ml_write"

    get_socket : unit -> Unix.file_descr = "get_socket"
    get_buffer : unit -> string = "get_buffer"

*/



#include <stdio.h>
#include <stdlib.h>

#include "mlstate_platform.h"

#ifdef MLSTATE_WINDOWS
#include <Winsock2.h>
#include <windows.h>
#include <mswsock.h>
#endif

#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/compatibility.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

//#include "iocp.h"

#ifdef MLSTATE_WINDOWS
// For some unknown reason, this macro was not available on my Windows
#define my_offsetof(s,m)   (size_t)&(((s *)0)->m)

enum kind {Read, Write, Listen};

struct io_context
{
  OVERLAPPED ovl;
  int id;
  int kind;
  char *buf;
  SOCKET sock;
};

typedef struct io_context io_context;

struct connection_context
{
  HANDLE file;
  SOCKET sock;

  WSABUF readbuf;
  WSABUF sendbuf;

  struct io_context readctx;
  struct io_context sendctx;
};

#define BUFFER_SIZE 4096


void read_finished(DWORD error, DWORD transferred,
                   struct io_context *ioctx);
void send_finished(DWORD error, DWORD transferred,
                   struct io_context *ioctx);
void begin_send(struct connection_context *ctx);
int async_read(SOCKET s, int len);
int async_write(SOCKET so, char *str, int len);





typedef struct sockaddr_in sockaddr_in;
typedef struct hostent hostent;

// The handle to the completion port
HANDLE iocp;

// Pointer to the system accept function
LPFN_ACCEPTEX lpfnAcceptEx = NULL;

// Warning: This is NOT thread-safe
int count = 1;
io_context *g_last_context = NULL;


io_context* make_overlap(enum kind k)
{
  io_context *ctx = malloc(sizeof(io_context));
  memset(ctx, 0, sizeof(*ctx));
  ctx->id = count++;
  ctx->kind = k;
  return ctx;
}


void debug(char* toto, ...){}

#define DEBUG_IOCP

#ifdef DEBUG_IOCP
#define DEBUGMESS debug
#else
#define DEBUGMESS printf
#endif

CAMLprim value iocp_ml_wait(value vtime)
{
  CAMLparam1(vtime);
  CAMLlocal1(res);
  int time = Long_val(vtime);
  OVERLAPPED *ovl;
  ULONG_PTR completionkey;
  DWORD transferred;
  BOOL ret;
  int event;
  value v_ev;

  if (time < 0) time = INFINITE;
  if (iocp == NULL) printf("iocp not initialized\n");
  if (iocp == INVALID_HANDLE_VALUE) printf("iocp badly initialized\n");
  debug("wait...\n");
  ret = GetQueuedCompletionStatus(iocp, &transferred,
				  &completionkey, &ovl, time);

  if(ret)
    {
      struct io_context *ctx = (struct io_context*)ovl;
      g_last_context = ctx;
      if (ctx->kind == Listen) {
	event = 0;
	/* async_read(ctx->sock, 20); */
	/* async_write(ctx->sock, "Bienvenue sur cat!\n", 0); */
      }
      else if (ctx->kind == Read) {
	event = 0;
	DEBUGMESS("recv buf: `%s'\n", ctx->buf);
	ctx->buf[transferred] = '\0';
	/* async_read(ctx->sock, 20); */
	/* async_write(ctx->sock, "Tu as écrit : ", 0); */
	/* async_write(ctx->sock, ctx->buf, transferred); */
      }
      else if (ctx->kind == Write) {
	event = 1;
	DEBUGMESS("Write end.\n");
      }else{
        event = -1;
	printf("Unknown kind: %d?!\n", ctx->kind);
      }
      res = Val_int(ctx->id);
    }
  else if(ovl)
    {
      DWORD err = GetLastError();
      struct io_context *ctx = (struct io_context*)ovl;
      event = -1;
      printf("IO failed on %d\n", ctx->id);
      res = Val_int(-1);
    }
  else
    {
      DWORD err = GetLastError();
      printf("Wait error %d\n", err);
      event = -1;
      res = Val_int(-1);
      // error out
    }
  v_ev = caml_alloc_small(2, 0);
  Field(v_ev, 0) = /* Val_int */(res);
  Field(v_ev, 1) = Val_int(event);
  CAMLreturn(v_ev);
}

CAMLprim value iocp_ml_get_socket(value _)
{
  CAMLparam1(_);
  CAMLlocal1(ret);

  if (g_last_context == NULL) printf("context is null\n");
  if (g_last_context->kind != Listen)
    printf("Bad context, expect Listen, got %d\n", g_last_context->kind);
  ret = win_alloc_socket(g_last_context->sock);
  free(g_last_context);
  g_last_context = NULL;
  CAMLreturn(ret);
  /* return ret; */
}

CAMLprim value iocp_ml_get_buffer(value _)
{
  CAMLparam1(_);
  CAMLlocal1(ret);

  if (g_last_context == NULL) printf("context is null\n");
  if (g_last_context->kind != Read)
    printf("Bad context, expect Read, got %d\n", g_last_context->kind);

  ret = caml_copy_string(g_last_context->buf);
  free(g_last_context->buf);
  free(g_last_context);
  g_last_context = NULL;
  CAMLreturn(ret);
}

int async_write(SOCKET so, char *str, int len)
{
  int ret;
  WSABUF buf;
  io_context *ctx = make_overlap(Write);

  CreateIoCompletionPort((HANDLE)so, iocp, 1, 0);
  buf.buf = str;
  if (len == 0) len = strlen(str);
  buf.len = len;

  //  memset(&ovl, 0, sizeof(ovl));
  ret = WSASend(so, &buf, 1, NULL, 0, (OVERLAPPED*)ctx, NULL);

  if(ret)
    {
      int writeerr = WSAGetLastError();

      if (writeerr == WSAENOTSOCK)
	printf("THIS IS NOT A SOCK%$#!\n");
      else if(writeerr != WSA_IO_PENDING)
	printf("error sending (%d)\n", writeerr);
      else
	DEBUGMESS("ok, pending\n");
    }
  else
    DEBUGMESS("write success!\n");

  return ctx->id;
}

int async_write_new(SOCKET s, char *str, int len)
{
  char *iobuf = malloc(sizeof(char) * len); // remove the stupid [UNIX_BUFFER_SIZE];
  int ret;
  io_context *ctx = make_overlap(Write);
  WSABUF buf;

  iobuf[0] = 'W';
  iobuf[1] = '\0';
  buf.buf = iobuf;
  buf.len = len;
  ctx->buf = iobuf;
  ctx->sock = s;

  CreateIoCompletionPort((HANDLE)s, iocp, 1, 0);
  ret = WriteFile((HANDLE)s, iobuf, len, NULL, &ctx->ovl);

  if(ret)
    {
      int writeerr = GetLastError();

      if (writeerr == ERROR_IO_PENDING)
	printf("Begin read error %d\n", writeerr);
      else
	printf("write pending\n");
    }
  else
    DEBUGMESS("write success!\n");

  return ctx->id;
}


/* Create an overlapped socket */
SOCKET async_socket(void)
{
  SOCKET s = WSASocket(AF_INET, SOCK_STREAM, 0,
		       NULL, 0,  WSA_FLAG_OVERLAPPED);

  if (s == INVALID_SOCKET)
    printf("ERROR: Invalid socket!\n");

  CreateIoCompletionPort((HANDLE)s, iocp, (u_long)0, 0);

  return s;
}

CAMLprim value iocp_ml_socket(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(res);
  res = win_alloc_socket(async_socket());
  CAMLreturn(res);
}

CAMLprim value iocp_ml_async_init(value unit)
{
  printf("IOCP INITIALIZED\n");
  iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
}

int async_accept(SOCKET ListenSocket)
{
  SOCKET AcceptSocket;
  char lpOutputBuf[1024];
  //  WSAOVERLAPPED olOverlap;
  io_context *ctx = make_overlap(Listen);
  DWORD dwBytes;
  GUID GuidAcceptEx = WSAID_ACCEPTEX;

  //----------------------------------------
  // Create an accepting socket
  /* AcceptSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP); */
  AcceptSocket = WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP,
			   NULL, 0,  WSA_FLAG_OVERLAPPED);

  if (AcceptSocket == INVALID_SOCKET) {
    printf("Error creating accept socket.\n");
    WSACleanup();
    return -1;
  }

  //----------------------------------------
  // Empty our overlapped structure and accept connections.
  memset((OVERLAPPED*)ctx, 0, sizeof(OVERLAPPED));


  WSAIoctl(ListenSocket,
    SIO_GET_EXTENSION_FUNCTION_POINTER,
    &GuidAcceptEx,
    sizeof(GuidAcceptEx),
    &lpfnAcceptEx,
    sizeof(lpfnAcceptEx),
    &dwBytes,
    NULL,
    NULL);


  lpfnAcceptEx(ListenSocket,
    AcceptSocket,
    lpOutputBuf,
    0,
    sizeof(sockaddr_in) + 16,
    sizeof(sockaddr_in) + 16,
    &dwBytes,
    (OVERLAPPED*)ctx);

  //----------------------------------------
  // Associate the accept socket with the completion port
  CreateIoCompletionPort((HANDLE)AcceptSocket, iocp, (u_long)0, 0);
  ctx->sock = AcceptSocket;
  return ctx->id;
}

value iocp_ml_accept(value ListenSocket)
{
  CAMLparam1(ListenSocket);
  CAMLlocal1(res);
  SOCKET so = Socket_val(ListenSocket);
  res = Val_int(async_accept(so));
  CAMLreturn(res);
}

int async_read(SOCKET s, int len)
{
  char *iobuf = malloc(sizeof(char) * len); // remove the stupid [UNIX_BUFFER_SIZE];
  int ret;
  io_context *ctx = make_overlap(Read);
  WSABUF buf;

  iobuf[0] = 'W';
  iobuf[1] = '\0';
  buf.buf = iobuf;
  buf.len = len;
  ctx->buf = iobuf;
  ctx->sock = s;
  CreateIoCompletionPort((HANDLE)s, iocp, 1, 0);
  ret = ReadFile((HANDLE)s, iobuf, len, NULL, &ctx->ovl);

  if(!ret)
    {
      DWORD readerr = GetLastError();

      if(readerr != ERROR_IO_PENDING)
	printf("Begin read error %d\n", readerr);
      else
	printf("read pending\n");
    }
  else DEBUGMESS("read: no error\n");

  return ctx->id;
}

CAMLprim value iocp_ml_read(value fd, value vlen)
{
  CAMLparam2(fd, vlen);
  CAMLlocal1(res);
  intnat len = Long_val(vlen);
  SOCKET s = Socket_val(fd);
  assert(Descr_kind_val(fd) == KIND_SOCKET);
  res = Val_int(async_read(s, len));

  CAMLreturn(res);
}

CAMLprim value iocp_ml_write(value fd, value vbuf, value vlen)
{
  CAMLparam3(fd, vbuf, vlen);
  CAMLlocal1(res);
  intnat len = Long_val(vlen);
  SOCKET s = Socket_val(fd);
  char *buf = String_val(vbuf);
  assert(Descr_kind_val(fd) == KIND_SOCKET);
  res =  Val_int(async_write(s, buf, len));
  CAMLreturn(res);
  /* async_write(s, buf, len); */
  /* return Val_unit; */
}
#else
//#if defined( MLSATE_CYGWIN ) || defined( MLSATE_UNIX )

value ep_error_cygwin(char* mess){
  printf("Iocp : error_unix_or_cywin :%s",mess);
  exit(1);
  return ( (value)0 );
}

CAMLprim value iocp_ml_socket(){ return ep_error_cygwin("Iocp.ml_socket");}
CAMLprim value iocp_ml_wait(value int_){ return ep_error_cygwin("Iocp.ml_wait");}
CAMLprim value iocp_ml_accept(value fd){ return ep_error_cygwin("Iocp.ml_accept");}
CAMLprim value iocp_ml_async_init(){ return ep_error_cygwin("Iocp.ml_async_init");}
CAMLprim value iocp_ml_async_read(value fd, value _0, value _1){ return ep_error_cygwin("Iocp.ml_async_read");}
CAMLprim value iocp_ml_get_socket(value _){ return ep_error_cygwin("Iocp.ml_getsocket");}
CAMLprim value iocp_ml_get_buffer(value _){ return ep_error_cygwin("Iocp.ml_getsocket");}

#endif
