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
/*
  @author Laurent Le Brun
  @author Stephan Hadinger
**/

// TODO: check errors in AcceptEx for aborted connections
// TODO: after accept, update origin address

#include <stdio.h>
#include <stdlib.h>

#include "libbase/mlstate_platform.h"

#ifdef WIN32
#include <Winsock2.h>
#include <mswsock.h>

#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/compatibility.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#define SIMPLE_EXPORT(name)                             \
  CAMLprim value io_##name()				\
  { return Val_int(name); }			        \
  CAMLprim value io_has_##name(value flag)		\
  { return Val_bool((Int_val(flag) & flag) == flag); }

//#define DEBUG_IOCP

#ifdef DEBUG_IOCP
#define DEBUGMESS printf
#else
#define DEBUGMESS quiet
#endif
void quiet(char* toto, ...){}

enum kind {IOCP_READ, IOCP_WRITE, IOCP_ACCEPT, IOCP_CONNECT, IOCP_ERR};

typedef struct io_context
{
  WSAOVERLAPPED ovl;
  int id;
  int kind;
  value fd;		// ocaml internal Unix.file_descr
  char *buf;
  int   buf_size;
  SOCKET socket;
  SOCKET listen_socket;				// so that AcceptEx retains the listening socket context
  struct sockaddr_in senderAddr;	// holds the sender's addr - primarily for UDP
  int senderAddrSize;
} io_context;


io_context* make_context(value fd, enum kind k, int buf_size)
{
static intnat g_count = 1;

  io_context *ctx = malloc(sizeof(io_context));
  memset(ctx, 0, sizeof(*ctx));
  ctx->senderAddrSize = sizeof(ctx->senderAddr);
  ctx->id = g_count++;
  ctx->kind = k;
  if (buf_size > 0) {
    ctx->buf = malloc(buf_size);
	ctx->buf_size = buf_size;
  }
  ctx->fd = fd;
  if (fd)
	caml_register_global_root(&ctx->fd);		// register global pointer to file_descr
  return ctx;
}

void free_context(io_context* ctx) {
  DEBUGMESS("free_overlap %p\n", (void*)ctx);
  if (ctx->fd)
    caml_remove_global_root(&ctx->fd);			// unregister global
  if (ctx != NULL) {
    if (ctx->buf != NULL) {
      free(ctx->buf);
    }
  free(ctx);
  }
}

// Warning: This is NOT thread-safe, but don't care. opalang is not threaded.
io_context *_g_last_context = NULL;

void set_last_context(io_context *ctx) {
  if (_g_last_context != NULL) {
    free_context(_g_last_context);
  }
  _g_last_context = ctx;
}

io_context* get_last_context() {
  return _g_last_context;
}

void clear_last_context() {
  set_last_context(NULL);
}

#define fail_err(where, what)  { \
  win32_maperr(what);          \
  uerror(where, Nothing); }

/* ************************************************************ */
/* Simply convert the fd address to an ocaml int, for printing  */
/* ************************************************************ */
value fd_to_int(value file_desc) {
CAMLparam1(file_desc);
intnat res = -1;

  if (file_desc)
    res = (intnat) Socket_val(file_desc);
  CAMLreturn(Val_long(res));
}


/* ************************************************************ */
/* Init once the iocp port at first call                        */
/* ************************************************************ */
HANDLE get_or_init_iocp() {
// The handle to the completion port
static HANDLE g_iocp = NULL;  

  if (g_iocp == NULL) {
    g_iocp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0);
    DEBUGMESS("Initializing IOCP 0x%p\n", (void*) g_iocp);
    if (g_iocp == NULL)
      fail_err("CreateIoCompletionPort", GetLastError());
  }
  return g_iocp;
}

/* ************************************************************ */
/* Explicitly clears the last context to free buffers memory    */
/* ************************************************************ */
CAMLprim value iocp_ml_clear_last_context() {
  clear_last_context();
  return Val_unit;
}

/* ************************************************************ */
/* Retrieves the buffer after a READ completion                 */
/* ************************************************************ */
CAMLprim value iocp_ml_get_last_buffer(value fd)
{
  CAMLparam1(fd);
  CAMLlocal1(buffer);
  io_context *ctx;

  ctx = get_last_context();
  if (ctx == NULL) {                DEBUGMESS("get_last_buffer: context is null\n");
  } else {
    if (ctx->fd != fd)				DEBUGMESS("get_last_buffer: bad fd\n");
    if (ctx->kind != IOCP_READ)     DEBUGMESS("get_last_buffer: Bad context, expected IOCP_READ\n");
    if (ctx->buf == NULL)           DEBUGMESS("get_last_buffer: buffer is null\n");
  }

  if ((ctx != NULL) && (ctx->fd == fd) && (ctx->buf != NULL) && (ctx->buf_size > 0)) {
    DEBUGMESS("reading buffer: adr=%p len=%d\n", (void*) ctx->buf, (int) ctx->buf_size);
    buffer = caml_alloc_string(ctx->buf_size);
	memcpy(String_val(buffer), ctx->buf, ctx->buf_size);
  } else {
    DEBUGMESS("reading buffer: empty buffer");
    buffer = caml_copy_string("");	// empty string
  }
  CAMLreturn(buffer);
}

/* ************************************************************ */
/* Retrieves the last sender address (only for UDP)             */
/* ************************************************************ */
CAMLprim value iocp_ml_get_socket_addr(value fd)
{
  CAMLparam1(fd);
  CAMLlocal1(addr);
  io_context *ctx;

  ctx = get_last_context();
  if (ctx == NULL) {                DEBUGMESS("get_last_buffer: context is null\n");
  } else {
    if (ctx->fd != fd)				DEBUGMESS("get_last_buffer: bad fd\n");
    if (ctx->kind != IOCP_READ)     DEBUGMESS("get_last_buffer: Bad context, expected IOCP_READ\n");
  }

  if ((ctx != NULL) && (ctx->fd == fd)) {
    addr = alloc_sockaddr( (union sock_addr_union *) &ctx->senderAddr, ctx->senderAddrSize, -1);
  } else {
    caml_failwith("iocp_ml_get_socket_addr: no addr available");
  }
  CAMLreturn(addr);
}



/* ************************************************************ */
/* Wait for an IO Completion event                              */
/* ************************************************************ */
CAMLprim value iocp_ml_wait(value vtime)
{
  CAMLparam1(vtime);
  CAMLlocal2(v_res, v_ev);
  intnat time = Long_val(vtime);
  io_context *ctx;
  ULONG_PTR completionkey;
  DWORD transferred;
  HANDLE iocp;
  intnat res_fd;
  int res_op;

  DEBUGMESS("wait(%Ii)...\n", time);
  if (time < 0) time = INFINITE;
  iocp = get_or_init_iocp();
  if (GetQueuedCompletionStatus(iocp, &transferred, &completionkey, (OVERLAPPED**) &ctx, time)) {
  	set_last_context(ctx);		// freeing is automatic
    res_fd = ctx->fd;
	res_op = ctx->kind;
    if (ctx->kind == IOCP_ACCEPT) {
	    DEBUGMESS("wait: ACCEPT received\n");
		// update the accepting socket with the context of the listening socket
		if (setsockopt(ctx->socket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, (char *)&ctx->listen_socket, sizeof(ctx->listen_socket))) {
		  fail_err("iocp_ml_wait/setsockopt(SO_UPDATE_ACCEPT_CONTEXT)", WSAGetLastError());
	    }
		// now getpeername() will work normally, so update the sender addr
		if (getpeername(ctx->socket, (struct sockaddr *) &ctx->senderAddr, &ctx->senderAddrSize)) {
		  fail_err("iocp_ml_wait/getpeername", WSAGetLastError());
	    }
		DEBUGMESS("addr(sin_family)=%d\n", ctx->senderAddr.sin_family);
		DEBUGMESS("addr(port)=%d\n", ctx->senderAddr.sin_port);
		DEBUGMESS("addr(ip)=%d.%d.%d.%d\n", ctx->senderAddr.sin_addr.S_un.S_un_b.s_b1, ctx->senderAddr.sin_addr.S_un.S_un_b.s_b2,
											ctx->senderAddr.sin_addr.S_un.S_un_b.s_b3, ctx->senderAddr.sin_addr.S_un.S_un_b.s_b4);
      } else if (ctx->kind == IOCP_CONNECT) {
	    DEBUGMESS("wait: CONNECT received\n");
      } else if (ctx->kind == IOCP_READ) {
	    DEBUGMESS("wait: READ received, fd=%d transferred=%d\n", (int) ctx->fd, (int) transferred);
		ctx->buf_size = transferred;		// update the real buffer size
      } else if (ctx->kind == IOCP_WRITE) {
	    DEBUGMESS("wait: WRITE received, transferred=%d\n", (int) transferred);
		ctx->buf_size = transferred;		// update the real buffer size
      } else {
	    DEBUGMESS("Unknown kind: %d?!\n", ctx->kind);
      }
	  
	v_res = caml_alloc_small(1, 0);
	v_ev = caml_alloc_small(2, 0);
	Field(v_ev, 0) = res_fd;
	Field(v_ev, 1) = Val_int(res_op); // v_flags replaced with ev->events
	Store_field(v_res, 0, v_ev);
    }
  else if (ctx) {
	set_last_context(ctx);		// freeing is automatic
    res_fd = ctx->fd;
	res_op = IOCP_ERR;
    DWORD err = GetLastError();
    DEBUGMESS("IO failed on %p, err=%lu\n", (void*)ctx->fd, err);
	set_last_context(NULL);		// freeing is automatic
	ctx = NULL;
	
	v_res = caml_alloc_small(1, 0);
	v_ev = caml_alloc_small(2, 0);
	Field(v_ev, 0) = res_fd;
	Field(v_ev, 1) = Val_int(res_op); // v_flags replaced with ev->events
	Store_field(v_res, 0, v_ev);
  } else {
    DWORD err = GetLastError();
	if (err == WAIT_TIMEOUT) {
	  DEBUGMESS("GetQueuedCompletionStatus timeout");
	  res_fd = 0;
	  res_op = IOCP_ERR;
	  v_res = caml_alloc(0, 0);
	} else {
	  fail_err("GetQueuedCompletionStatus", err);
	}
  }

  CAMLreturn(v_res);
}

/* ************************************************************ */
/* Trigger an async write on a socket                           */
/* ************************************************************ */
CAMLprim value iocp_ml_write(value fd, value vbuf, value vlen, value vaddr)
{
  CAMLparam4(fd, vbuf, vlen, vaddr);
  CAMLlocal1(res);
  intnat len = Long_val(vlen);
  intnat vbuf_len = caml_string_length(vbuf);
  SOCKET socket = Socket_val(fd);
  union sock_addr_union addr;
  socklen_param_type addr_len;
  io_context *ctx;
  DWORD flags = 0;
  WSABUF wsabuf;
  int wsasend_err;
  
  DEBUGMESS("async_write(fd=%d, len=%d)\n", (int) fd, (int) len);
  assert(Descr_kind_val(fd) == KIND_SOCKET);
  if ((len <= 0) || (len > vbuf_len))							// if len is negative, use the string length
    len = vbuf_len;

  ctx = make_context(fd, IOCP_WRITE, len);
  ctx->socket = socket;
  char *vbuf_str = String_val(vbuf);
  memcpy(ctx->buf, vbuf_str, len);			// make a copy of the buffer in our own structure to avoid ocaml to free it too soon
  
  wsabuf.buf = ctx->buf;
  wsabuf.len = ctx->buf_size;
  
  DEBUGMESS("vaddr=%d\n", (int) vaddr);
  if (vaddr != Val_int(0)) {
    get_sockaddr(vaddr, &addr, &addr_len);
    wsasend_err = WSASendTo(socket,
              &wsabuf,			// wsabuf is allowed to be allocated on stack
              1,				// only one buffer
              NULL,				// don't accept sync completion, force iocp notification
              flags,
			  (struct sockaddr *) &addr,
			  addr_len,
              &ctx->ovl,
              NULL);
  } else {
    wsasend_err = WSASend(socket,
              &wsabuf,			// wsabuf is allowed to be allocated on stack
              1,				// only one buffer
              NULL,				// don't accept sync completion, force iocp notification
              flags,
              &ctx->ovl,
              NULL);
  }
  
  if (wsasend_err) {
    DWORD err = WSAGetLastError();
	if (err == WSA_IO_PENDING) {
	  // OK, the async read is pending
	  DEBUGMESS("WSASend WSA_IO_PENDING fd=%d\n", (int) fd);
	} else {
	  fail_err("WSASend", err);
	}
  } else {
    DEBUGMESS("WSASend: WSASend sync success, this is not normal!!!\n");
  }
  
  CAMLreturn(Val_unit);
}


/* ************************************************************ */
/* Trigger an async read on a socket                            */
/* ************************************************************ */
CAMLprim value iocp_ml_read(value fd, value vlen, value vudp)
{
  CAMLparam3(fd, vlen, vudp);
  CAMLlocal1(res);
  intnat len = Long_val(vlen);
  SOCKET socket = Socket_val(fd);
  int udp = Bool_val(vudp);
  io_context *ctx;
  //HANDLE iocp;
  DWORD flags = 0;
  WSABUF wsabuf;
  int wsarecv_err;
  
  DEBUGMESS("async_read(fd=%d, len=%d)\n", (int) fd, (int) len);
  assert(Descr_kind_val(fd) == KIND_SOCKET);

  ctx = make_context(fd, IOCP_READ, len);
  ctx->socket = socket;
  
  wsabuf.buf = ctx->buf;
  wsabuf.len = ctx->buf_size;
  
  if (udp) {
    wsarecv_err = WSARecvFrom(socket,
              &wsabuf,
              1,				// only one buffer
              NULL,				// don't accept sync completion, force iocp notification
              &flags,
			  (SOCKADDR*) &ctx->senderAddr,	// catch receiver's address
			  &ctx->senderAddrSize,
              &ctx->ovl,
              NULL);			// no completion callback
  } else {
    wsarecv_err = WSARecv(socket,
              &wsabuf,
              1,				// only one buffer
              NULL,				// don't accept sync completion, force iocp notification
              &flags,
              &ctx->ovl,
              NULL);			// no completion callback
  }
  
  if (wsarecv_err) {
    DWORD err = WSAGetLastError();
	if (err == WSA_IO_PENDING) {
	  // OK, the async read is pending
	  DEBUGMESS("WSARecv WSA_IO_PENDING fd=%d\n", (int) fd);
	} else {
	  DEBUGMESS("WSARecv WSAGetLastError=%d\n", (int) err);
	  fail_err("WSARecv", err);
	}
  } else {
    // a buffer is already available, but we simply ignore it
	// it will anyways trigger an io completion event at the next polling
    DEBUGMESS("WARNING: WSARecv sync success, this is not normal!!!\n");
  }

  CAMLreturn(Val_unit);
}

/* ************************************************************ */
/* Associate a socket to the iocp port                          */
/* ************************************************************ */
CAMLprim value iocp_associate_iocp(value vfd)
{
CAMLparam1(vfd);
SOCKET socket = Socket_val(vfd);
  
  HANDLE iocp = get_or_init_iocp();
  DEBUGMESS("iocp_associate_iocp>CreateIoCompletionPort, socket=%p, iocp=%p\n", (void*) socket, (void*) iocp);
  if (CreateIoCompletionPort((HANDLE)socket, iocp, 1, 0) == NULL) {		// XXX why 1 ?
    DEBUGMESS("iocp_associate_iocp>CreateIoCompletionPort err=%d\n", (int) GetLastError());
	fail_err("CreateIoCompletionPort", GetLastError());
  }

  CAMLreturn(Val_unit);
}


/* ************************************************************ */
/* Async accept                                                 */
/* ************************************************************ */
value iocp_ml_accept(value listen_file_descr)
{
CAMLparam1(listen_file_descr);
CAMLlocal1(accept_file_descr);
SOCKET listen_socket = Socket_val(listen_file_descr);
SOCKET accept_socket = (SOCKET) NULL;
io_context *ctx = NULL;
DWORD dwBytes;
// dummy variable needed to pass a valid pointer for an unsued variable (AcceptEx)
static DWORD g_dummy;

LPFN_ACCEPTEX lpfnAcceptEx;
GUID GuidAcceptEx = WSAID_ACCEPTEX;
  
  DEBUGMESS("iocp_ml_accept file_descr=%d\n", (int) listen_file_descr);
  //----------------------------------------
  // Create an accepting socket
  accept_socket = WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, NULL, 0,  WSA_FLAG_OVERLAPPED);
  DEBUGMESS("iocp_ml_accept: allocated socket 0x%p\n", (void*) accept_socket);

  if (accept_socket == INVALID_SOCKET) {
    fail_err("WSASocket", WSAGetLastError());
  }

  //----------------------------------------
  // Empty our overlapped structure and accept connections.
  // Load the AcceptEx function
  WSAIoctl(listen_socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
					   &GuidAcceptEx, sizeof(GuidAcceptEx),
					   &lpfnAcceptEx, sizeof(lpfnAcceptEx),
					   &dwBytes,
					   NULL,
					   NULL
					   );
    
  ctx = make_context(listen_file_descr, IOCP_ACCEPT, (sizeof(SOCKADDR_IN) + 16) * 2);	// will update target fd later
  ctx->socket = accept_socket;
  ctx->listen_socket = listen_socket;
  if (!lpfnAcceptEx(listen_socket, 
					accept_socket,
					ctx->buf,
					0,
					sizeof(SOCKADDR_IN) + 16, 
					sizeof(SOCKADDR_IN) + 16, 
					&g_dummy, 					// need to pass a valid pointer to a dummy unused variable, 
												// see: http://support.microsoft.com/default.aspx?scid=kb;en-us;Q192800
					&ctx->ovl)) {
    DWORD err = WSAGetLastError();
	if (err != WSA_IO_PENDING) {
      clear_last_context();		// freeing is automatic
	  fail_err("AcceptEx", WSAGetLastError());
	}
	if (err)
	  DEBUGMESS("AcceptEx: WSA_IO_PENDING\n");
	else
	  DEBUGMESS("AcceptEx: NO_ERR");
  }

  // Associate the newly allocated socket to the iocp port
  HANDLE iocp = get_or_init_iocp();
  DEBUGMESS("Before CreateIoCompletionPort, socket=%p, iocp=%p\n", (void*) accept_socket, (void*) iocp);
  if (CreateIoCompletionPort((HANDLE)accept_socket, iocp, 1, 0) == NULL) {		// XXX why 1 ?
	fail_err("CreateIoCompletionPort", GetLastError());
  }
  
  accept_file_descr = win_alloc_socket(accept_socket);
  DEBUGMESS("iocp_ml_accept -> %Id\n", (intnat)accept_file_descr);
  ctx->fd = accept_file_descr;		// fd updated to the target one, no need to re-register global pointer
  CAMLreturn(accept_file_descr);
}

/* ************************************************************ */
/* Async connect                                                */
/* ************************************************************ */
value iocp_ml_connect(value connect_file_descr, value vaddr)
{
CAMLparam2(connect_file_descr, vaddr);
SOCKET connect_socket = Socket_val(connect_file_descr);
union sock_addr_union addr;
socklen_param_type addr_len;
io_context *ctx = NULL;
DWORD dwBytes;
// dummy variable needed to pass a valid pointer for an unsued variable (AcceptEx)
static DWORD g_dummy;

LPFN_CONNECTEX lpfnConnectEx;
GUID GuidConnectEx = WSAID_CONNECTEX;
  
  get_sockaddr(vaddr, &addr, &addr_len);
  struct sockaddr_in *debug_addr = (struct sockaddr_in*) &addr;
  DEBUGMESS("iocp_ml_connect ip=%d.%d.%d.%d ", debug_addr->sin_addr.S_un.S_un_b.s_b1, debug_addr->sin_addr.S_un.S_un_b.s_b2,
							   debug_addr->sin_addr.S_un.S_un_b.s_b3, debug_addr->sin_addr.S_un.S_un_b.s_b4);
  DEBUGMESS("port=%d\n", debug_addr->sin_port);

  // first bind the socket to default address
  struct sockaddr_in connect_orig_addr;
  connect_orig_addr.sin_family = AF_INET;
  connect_orig_addr.sin_addr.S_un.S_addr = INADDR_ANY;
  connect_orig_addr.sin_port = 0;
  if (bind(connect_socket, (struct sockaddr *) &connect_orig_addr, sizeof(connect_orig_addr))) {
    fail_err("bind", GetLastError());
  }
  
  //----------------------------------------
  // Empty our overlapped structure and accept connections.
  // Load the AcceptEx function
  WSAIoctl(connect_socket, SIO_GET_EXTENSION_FUNCTION_POINTER,
					   &GuidConnectEx, sizeof(GuidConnectEx),
					   &lpfnConnectEx, sizeof(lpfnConnectEx),
					   &dwBytes,
					   NULL,
					   NULL
					   );
    
//  connect_file_descr = win_alloc_socket(connect_socket);
  ctx = make_context(connect_file_descr, IOCP_CONNECT, 0);	// don't need any buffer here
  ctx->socket = connect_socket;
  DEBUGMESS("lpfnConnectEx=%p\n", (void*) lpfnConnectEx);
  if (!lpfnConnectEx(connect_socket,
                     (struct sockaddr *) &addr,
					 addr_len,
					 NULL,			// don't send any data yet
					 0,				// size of data = 0
					 &g_dummy,		// need a pointer here, unused
					 &ctx->ovl)) {
    DWORD err = WSAGetLastError();
	if (err != WSA_IO_PENDING) {
      clear_last_context();		// freeing is automatic
	  DEBUGMESS("ConnectEx err=%d\n", (int)err);
	  fail_err("ConnectEx", err);
	}
	if (err)
	  DEBUGMESS("ConnectEx: WSA_IO_PENDING\n");
	else
	  DEBUGMESS("ConnectEx: NO_ERR");
  }

  DEBUGMESS("iocp_ml_connect -> %Id\n", (intnat)connect_file_descr);
  CAMLreturn(Val_unit);
}


SIMPLE_EXPORT(IOCP_READ)
SIMPLE_EXPORT(IOCP_WRITE)
SIMPLE_EXPORT(IOCP_ACCEPT)
SIMPLE_EXPORT(IOCP_CONNECT)
SIMPLE_EXPORT(IOCP_ERR)


#else
//#if defined( MLSATE_CYGWIN ) || defined( MLSATE_UNIX )

#include <caml/mlvalues.h>

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

#endif
