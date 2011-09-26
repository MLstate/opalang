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
  @author Laurent Le Brun (epoll)
  @author Cedric Soulas (kqueue)
**/

#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/compatibility.h>
#include <caml/fail.h>
#include <caml/threads.h>

#include "libbase/mlstate_platform.h"

#define EXPORT(name,v)					\
  CAMLprim value ep_##name()                            \
  { return Val_int(v); }                                \
  CAMLprim value ep_has_##name(value flag)              \
  { return Val_bool((Int_val(flag) & flag) == flag); }

#define SIMPLE_EXPORT(name)                             \
  CAMLprim value ep_##name()				\
  { return Val_int(name); }			        \
  CAMLprim value ep_has_##name(value flag)		\
  { return Val_bool((Int_val(flag) & flag) == flag); }

#define DUMMY_EXPORT(name,str)					      \
  CAMLprim value ep_##name(){return error_epoll_cygwin("ep",str); }	\
  CAMLprim value ep_has_##name(value flag){return error_epoll_cygwin("ep_has",str); }


CAMLprim value error_msg()
{
	CAMLparam0();
	const char *error = strerror(errno);
	errno = 0;
	CAMLreturn (caml_copy_string(error));
}

CAMLprim value error_value()
{
	CAMLparam0();
	CAMLreturn (Val_int(errno));
}

#ifdef MLSTATE_UNIX

#include <unistd.h>

/* same unix standard (unistd) close for kqueue and epoll */
value ep_close(value fd)
{
    CAMLparam1(fd);
    CAMLreturn(Val_int(close(fd)));
}

#if (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
#define USE_KQUEUE
#endif

#ifdef USE_KQUEUE

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

CAMLprim value on_mac(){
    CAMLparam0();
    CAMLreturn(Val_true);
}

char* string_of_op(int op){
	switch (op) {
		case 0:
			return("NULL");
			break;
		case EV_ADD | EV_ENABLE:
			return("ADD");
			break;
		case EV_DISABLE:
			return("DISABLE");
			break;
		case EV_DELETE:
			return("DELETE");
			break;
		default:
			return("UNKNOWN");
	}
}

char* string_of_flags(int flags){
	switch (flags) {
		case 0:
			return("NULL");
			break;
		case EVFILT_READ:
			return("READ");
			break;
		case EVFILT_WRITE:
			return("WRITE");
			break;
		default:
			return("UNKNOWN");
	}
}

value ep_create(value size)
{
  CAMLparam1(size);
  CAMLreturn(Val_int(kqueue()));
}

//value epoll_op(value v_epfd, value v_fd, value v_flags, int op)
value ep_ctl(value epfd, int op, value fd, int flags)
{
	CAMLparam2(epfd, fd);
	int ret;
	struct kevent ev_monitored;
	struct kevent ev_check_error;
	struct timespec nullt = {0, 0};

	EV_SET(&ev_monitored, Int_val(fd), flags, op, 0, 0, 0);
	ret = kevent(Int_val(epfd), &ev_monitored, 1, &ev_check_error, 1, &nullt);
	if(ret==-1) printf("Net error");
	/** TO DEBUG:
	    if ((op != EV_DISABLE) && (ev_check_error.flags & EV_ERROR)) { //It may have error with DISABLE as explained below.
	    fprintf(stderr, "%s with %s '%s %d'\n", strerror(ev_check_error.data), string_of_op(op), string_of_flags(flags), Int_val(fd));
	    //exit(EXIT_FAILURE);
	    }
	**/
	CAMLreturn(Val_int(ret));
}

value ep_add(value epfd, value fd, value v_flags)
{
	return ep_ctl(epfd, EV_ADD | EV_ENABLE, fd, Int_val(v_flags));
}

// [WARNING] Mac OS specific:
// Unlike the Linux version, this function delete a precise filter from a filedescripor
// and not the filedescriptor
value ep_del(value epfd, value fd, value v_flags)
{
    return ep_ctl(epfd, EV_DELETE, fd, Int_val(v_flags));
}

// [WARNING] Mac OS specific:
// Unlike the Linux version, this function do nothing
// You have to use ep_del in conjunction with ep_add
// The reason is that kqueue filters aren't bitmasks and there is no "modify" operation
value ep_mod(value epfd, value fd, value v_flags)
{
    return(-1);
}

value ep_wait(value v_epfd, value v_maxevents, value v_timeout)
{
  CAMLparam3(v_epfd, v_maxevents, v_timeout);
  CAMLlocal2(v_res, v_flags);
  int maxevents = Int_val(v_maxevents);
  struct kevent *evs;
  int nb;

  if (maxevents <= 0) caml_invalid_argument("kqueue wait with maxevents <= 0");

  /* evs = caml_stat_alloc(maxevents); */
  evs = malloc(maxevents * sizeof (struct kevent));

  int t = Int_val(v_timeout);
  struct timespec *ptout;

  if (t<0) {
    ptout = NULL;
  } else {
    time_t sec = t/1000;
    long nano = (t-sec*1000)*1000000;
    struct timespec tout = {sec, nano};
    ptout = &tout;
  }

  /* fflush(stdout); */
  nb = kevent(Int_val(v_epfd), NULL, 0, evs, maxevents, ptout);

  if (nb < 0) {
    caml_stat_free(evs);
    int err = errno;
    errno = 0;
    /* fprintf(stderr, "kqueue error -1 with WAIT\n"); */
    caml_failwith(strerror(err));
  }

  v_res = caml_alloc(nb, 0);

  /* FIXME? */
  while (--nb >= 0) {
    value v_ev;
    struct kevent *ev = &evs[nb];
    if (ev->flags & EV_ERROR) {
	fprintf(stderr, "kqueue error: \"%s\"\n", strerror(ev->data));
	exit(EXIT_FAILURE);
    } else {
	//v_flags = caml_copy_int32(ev->fflags); //WHY THIS ??
	v_ev = caml_alloc_small(2, 0);
	Field(v_ev, 0) = Val_int(ev->ident);
	Field(v_ev, 1) = Val_int(ev->filter); // filter like EVFILT_READ/WRITE
	Store_field(v_res, nb, v_ev);
    }

  }

  free(evs);
  /* caml_stat_free(evs); */

  CAMLreturn(v_res);
}

// kqueue & epoll equivalent
EXPORT(EPOLLIN, EVFILT_READ);  //EXPORT(EPOLLIN);
EXPORT(EPOLLOUT, EVFILT_WRITE); //EXPORT(EPOLLOUT);
//TODO EXPORT(EPOLLPRI);
EXPORT(EPOLLERR, EV_ERROR);
EXPORT(EPOLLHUP, EV_ERROR); // TODO with a hang up like event
//TODO //EXPORT(EPOLLET);
//TODO //EXPORT(EPOLLONESHOT);

#else /* not USE_KQUEUE */

#include <sys/epoll.h>

CAMLprim value on_mac(){
    CAMLparam0();
    CAMLreturn(Val_false);
}

value ep_create(value size)
{
	CAMLparam1(size);
	CAMLreturn(Val_int(epoll_create(size)));
}

/* value epoll_op(value v_epfd, value v_fd, value v_flags, int op) */
value ep_ctl(value epfd, int op, value fd, value flags)
{
	int ret;
	struct epoll_event ev;
	CAMLparam3(epfd, fd, flags);
	ev.events = Int_val(flags);
	ev.data.fd = Int_val(fd);
	ret = epoll_ctl(Int_val(epfd), op, Int_val(fd), &ev);
	CAMLreturn(Val_int(ret));
}

value ep_add(value epfd, value fd, value flags)
{
	return ep_ctl(epfd, EPOLL_CTL_ADD, fd, flags);
}

// [WARNING] Linux specific:
// This function remove the filedescriptor from the epoll descriptor
// Unlike the Mac OS version, the third argument is unused
value ep_del(value epfd, value fd, value unused)
{
	return ep_ctl(epfd, EPOLL_CTL_DEL, fd, 0);
}

// [WARNING] Linux specific:
// Unlike the Mac OS version, this function change the bitmask filter for a filedescriptor
value ep_mod(value epfd, value fd, value flags)
{
	return ep_ctl(epfd, EPOLL_CTL_MOD, fd, flags);
}

value ep_wait(value v_epfd, value v_maxevents, value v_timeout)
{
	CAMLparam3(v_epfd, v_maxevents, v_timeout);
	CAMLlocal2(v_res, v_flags);
	int maxevents = Int_val(v_maxevents);
	struct epoll_event *evs;
	int nb;

	if (maxevents <= 0) caml_invalid_argument("epoll_wait: maxevents <= 0");

	/* evs = caml_stat_alloc(maxevents); */
	evs = malloc(maxevents * sizeof (struct epoll_event));

	caml_release_runtime_system();
	nb = epoll_wait(Int_val(v_epfd), evs, maxevents, Int_val(v_timeout));
	caml_acquire_runtime_system();

	if (nb == -1) {
		caml_stat_free(evs);
		int err = errno;
		errno = 0;
		caml_failwith(strerror(err));
	}

	v_res = caml_alloc(nb, 0);

	/* FIXME? */
	while (--nb >= 0) {
		value v_ev;
		struct epoll_event *ev = &evs[nb];
		//v_flags = caml_copy_int32(ev->events); WHY THIS ??
		v_ev = caml_alloc_small(2, 0);
		Field(v_ev, 0) = Val_int(ev->data.fd);
		Field(v_ev, 1) = Val_int(ev->events); // v_flags replaced with ev->events
		Store_field(v_res, nb, v_ev);
	}

	free(evs);
	/* caml_stat_free(evs); */

	CAMLreturn(v_res);
}

SIMPLE_EXPORT(EPOLLIN)
SIMPLE_EXPORT(EPOLLOUT)
SIMPLE_EXPORT(EPOLLHUP)
SIMPLE_EXPORT(EPOLLERR)
/*SIMPLE_EXPORT(EPOLLPRI)
SIMPLE_EXPORT(EPOLLET)
SIMPLE_EXPORT(EPOLLONESHOT)*/

#endif // USE_KQUEUE FALSE BRANCH

#endif  // MLSTATE_UNIX

#ifdef MLSTATE_CYGWIN
/*
  THIS IS A DUMMY IMPLEMENTATION FOR DEBUGGING THE COMPILATION PROCESS ONLY,
  AND HAVING A RESTRICTED PORT ON CYGWIN
*/

#define EPOLL_MESS_ERROR "error_cygwin:%s:%s"
#ifdef MLSTATE_WINDOWS
#define EPOLL_MESS_ERROR "error_windows:%s:%s"
#endif

value error_epoll_cygwin(char* mess,char* fun){
  printf(EPOLL_MESS_ERROR,mess,fun);
  exit(1);
  return ( (value)0 );
}


/* value ep_create(value size){return error_epoll_cygwin("ep_create","");}; */
/* value ep_add(value epfd, value fd, value flags){return error_epoll_cygwin("ep_add","");}; */
/* value ep_del(value epfd, value fd, value flags){return error_epoll_cygwin("ep_del","");}; */
/* value ep_mod(value epfd, value fd, value flags){return error_epoll_cygwin("ep_mod","");}; */
/* value ep_wait(value v_epfd, value v_maxevents, value v_timeout){return error_epoll_cygwin("ep_wait","");}; */
/* value ep_close(value fd){return error_epoll_cygwin("ep_close","");}; */

DUMMY_EXPORT(EPOLLIN,"epollin")
DUMMY_EXPORT(EPOLLOUT,"epollout")
DUMMY_EXPORT(EPOLLERR, "epollerr")
DUMMY_EXPORT(EPOLLHUP, "epollhup")
/*DUMMY_EXPORT(EPOLLPRI)
DUMMY_EXPORT(EPOLLET)
DUMMY_EXPORT(EPOLLONESHOT)*/

#endif
