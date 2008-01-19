/* OCaml bindings for libvirt.
 * (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
 * http://libvirt.org/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* Please read libvirt/README file. */

static char *Optstring_val (value strv);
typedef value (*Val_ptr_t) (void *);
static value Val_opt (void *ptr, Val_ptr_t Val_ptr);
/*static value option_default (value option, value deflt);*/
static void _raise_virterror (virConnectPtr conn, const char *fn) Noreturn;
static void not_supported (const char *fn) Noreturn;
static value Val_virterror (virErrorPtr err);

/* Use this around synchronous libvirt API calls to release the OCaml
 * lock, allowing other threads to run simultaneously.  'code' must not
 * perform any caml_* calls, run any OCaml code, or raise any exception.
 * http://web.archive.org/web/20030521020915/http://caml.inria.fr/archives/200106/msg00199.html
 */
#define NONBLOCKING(code)			\
  do {						\
    caml_enter_blocking_section ();		\
    code;					\
    caml_leave_blocking_section ();		\
  } while (0)

/* Check error condition from a libvirt function, and automatically raise
 * an exception if one is found.
 */
#define CHECK_ERROR(cond, conn, fn) \
  do { if (cond) _raise_virterror (conn, fn); } while (0)

/* For more about weak symbols, see:
 * http://kolpackov.net/pipermail/notes/2004-March/000006.html
 * We are using this to do runtime detection of library functions
 * so that if we dynamically link with an older version of
 * libvirt than we were compiled against, it won't fail (provided
 * libvirt >= 0.2.1 - we don't support anything older).
 */
#ifdef __GNUC__
#ifdef linux
#if (__GNUC__ == 3 && __GNUC_MINOR__ >= 3) || (__GNUC__ > 3)
#define HAVE_WEAK_SYMBOLS 1
#endif
#endif
#endif

#ifdef HAVE_WEAK_SYMBOLS
#define WEAK_SYMBOL_CHECK(sym)				\
  do { if (!sym) not_supported(#sym); } while (0)
#else
#define WEAK_SYMBOL_CHECK(sym)
#endif /* HAVE_WEAK_SYMBOLS */

/*----------------------------------------------------------------------*/

/* Some notes about the use of custom blocks to store virConnectPtr,
 * virDomainPtr and virNetworkPtr.
 *------------------------------------------------------------------
 *
 * Libvirt does some tricky reference counting to keep track of
 * virConnectPtr's, virDomainPtr's and virNetworkPtr's.
 *
 * There is only one function which can return a virConnectPtr
 * (virConnectOpen*) and that allocates a new one each time.
 *
 * virDomainPtr/virNetworkPtr's on the other hand can be returned
 * repeatedly (for the same underlying domain/network), and we must
 * keep track of each one and explicitly free it with virDomainFree
 * or virNetworkFree.  If we lose track of one then the reference
 * counting in libvirt will keep it open.  We therefore wrap these
 * in a custom block with a finalizer function.
 *
 * We also have to allow the user to explicitly free them, in
 * which case we set the pointer inside the custom block to NULL.
 * The finalizer notices this and doesn't free the object.
 *
 * Domains and networks "belong to" a connection.  We have to avoid
 * the situation like this:
 *
 *   let conn = Connect.open ... in
 *   let dom = Domain.lookup_by_id conn 0 in
 *   (* conn goes out of scope and is garbage collected *)
 *   printf "dom name = %s\n" (Domain.get_name dom)
 *
 * The reason is that when conn is garbage collected, virConnectClose
 * is called and any subsequent operations on dom will fail (in fact
 * will probably segfault).  To stop this from happening, the OCaml
 * wrappers store domains (and networks) as explicit (dom, conn)
 * pairs.
 *
 * Further complication with virterror / exceptions: Virterror gives
 * us virConnectPtr, virDomainPtr, virNetworkPtr pointers.  If we
 * follow standard practice and wrap these up in blocks with
 * finalizers then we'll end up double-freeing (in particular, calling
 * virConnectClose at the wrong time).  So for virterror, we have
 * "special" wrapper functions (Val_connect_no_finalize, etc.).
 *
 * Update 2008/01: Storage pools and volumes work the same way as
 * domains and networks.  And jobs.
 */

/* Unwrap a custom block. */
#define Connect_val(rv) (*((virConnectPtr *)Data_custom_val(rv)))
#define Dom_val(rv) (*((virDomainPtr *)Data_custom_val(rv)))
#define Net_val(rv) (*((virNetworkPtr *)Data_custom_val(rv)))
#ifdef HAVE_VIRSTORAGEPOOLPTR
#define Pol_val(rv) (*((virStoragePoolPtr *)Data_custom_val(rv)))
#endif
#ifdef HAVE_VIRSTORAGEVOLPTR
#define Vol_val(rv) (*((virStorageVolPtr *)Data_custom_val(rv)))
#endif
#ifdef HAVE_VIRJOBPTR
#define Jb_val(rv) (*((virJobPtr *)Data_custom_val(rv)))
#endif

/* Wrap up a pointer to something in a custom block. */
static value Val_connect (virConnectPtr conn);
static value Val_dom (virDomainPtr dom);
static value Val_net (virNetworkPtr net);
#ifdef HAVE_VIRSTORAGEPOOLPTR
static value Val_pol (virStoragePoolPtr pool);
#endif
#ifdef HAVE_VIRSTORAGEVOLPTR
static value Val_vol (virStorageVolPtr vol);
#endif
#ifdef HAVE_VIRJOBPTR
static value Val_jb (virJobPtr jb);
#endif

/* ONLY for use by virterror wrappers. */
static value Val_connect_no_finalize (virConnectPtr conn);
static value Val_dom_no_finalize (virDomainPtr dom);
static value Val_net_no_finalize (virNetworkPtr net);

/* Domains and networks are stored as pairs (dom/net, conn), so have
 * some convenience functions for unwrapping and wrapping them.
 */
#define Domain_val(rv) (Dom_val(Field((rv),0)))
#define Network_val(rv) (Net_val(Field((rv),0)))
#ifdef HAVE_VIRSTORAGEPOOLPTR
#define Pool_val(rv) (Pol_val(Field((rv),0)))
#endif
#ifdef HAVE_VIRSTORAGEVOLPTR
#define Volume_val(rv) (Vol_val(Field((rv),0)))
#endif
#ifdef HAVE_VIRJOBPTR
#define Job_val(rv) (Jb_val(Field((rv),0)))
#endif
#define Connect_domv(rv) (Connect_val(Field((rv),1)))
#define Connect_netv(rv) (Connect_val(Field((rv),1)))
#ifdef HAVE_VIRSTORAGEPOOLPTR
#define Connect_polv(rv) (Connect_val(Field((rv),1)))
#endif
#ifdef HAVE_VIRSTORAGEVOLPTR
#define Connect_volv(rv) (Connect_val(Field((rv),1)))
#endif
#ifdef HAVE_VIRJOBPTR
#define Connect_jobv(rv) (Connect_val(Field((rv),1)))
#endif

static value Val_domain (virDomainPtr dom, value connv);
static value Val_network (virNetworkPtr net, value connv);
#ifdef HAVE_VIRSTORAGEPOOLPTR
static value Val_pool (virStoragePoolPtr pol, value connv);
#endif
#ifdef HAVE_VIRSTORAGEVOLPTR
static value Val_volume (virStorageVolPtr vol, value connv);
#endif
#ifdef HAVE_VIRJOBPTR
static value Val_job (virJobPtr jb, value connv);
#endif

/* ONLY for use by virterror wrappers. */
static value Val_domain_no_finalize (virDomainPtr dom, value connv);
static value Val_network_no_finalize (virNetworkPtr net, value connv);
