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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libvirt/libvirt.h>
#include <libvirt/virterror.h>

#include <caml/config.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

static char *Optstring_val (value strv);
typedef value (*Val_ptr_t) (void *);
static value Val_opt (void *ptr, Val_ptr_t Val_ptr);
/*static value option_default (value option, value deflt);*/
static value _raise_virterror (virConnectPtr conn, const char *fn);
static value Val_virterror (virErrorPtr err);

#define CHECK_ERROR(cond, conn, fn) \
  do { if (cond) _raise_virterror (conn, fn); } while (0)

#define NOT_SUPPORTED(fn)			\
  caml_invalid_argument (fn " not supported")

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
  do { if (!sym) NOT_SUPPORTED(#sym); } while (0)
#else
#define WEAK_SYMBOL_CHECK(sym)
#endif /* HAVE_WEAK_SYMBOLS */

#ifdef HAVE_WEAK_SYMBOLS
#ifdef HAVE_VIRCONNECTGETHOSTNAME
extern char *virConnectGetHostname (virConnectPtr conn)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRCONNECTGETURI
extern char *virConnectGetURI (virConnectPtr conn)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRDOMAINBLOCKSTATS
extern int virDomainBlockStats (virDomainPtr dom,
				const char *path,
				virDomainBlockStatsPtr stats,
				size_t size)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRDOMAINGETSCHEDULERPARAMETERS
extern int virDomainGetSchedulerParameters (virDomainPtr domain,
					    virSchedParameterPtr params,
					    int *nparams)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRDOMAINGETSCHEDULERTYPE
extern char *virDomainGetSchedulerType(virDomainPtr domain,
				       int *nparams)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRDOMAININTERFACESTATS
extern int virDomainInterfaceStats (virDomainPtr dom,
				    const char *path,
				    virDomainInterfaceStatsPtr stats,
				    size_t size)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRDOMAINMIGRATE
extern virDomainPtr virDomainMigrate (virDomainPtr domain, virConnectPtr dconn,
				      unsigned long flags, const char *dname,
				      const char *uri, unsigned long bandwidth)
  __attribute__((weak));
#endif
#ifdef HAVE_VIRDOMAINSETSCHEDULERPARAMETERS
extern int virDomainSetSchedulerParameters (virDomainPtr domain,
					    virSchedParameterPtr params,
					    int nparams)
  __attribute__((weak));
#endif
#endif /* HAVE_WEAK_SYMBOLS */

/*----------------------------------------------------------------------*/

CAMLprim value
ocaml_libvirt_get_version (value driverv, value unit)
{
  CAMLparam2 (driverv, unit);
  CAMLlocal1 (rv);
  const char *driver = Optstring_val (driverv);
  unsigned long libVer, typeVer = 0, *typeVer_ptr;
  int r;

  typeVer_ptr = driver ? &typeVer : NULL;
  r = virGetVersion (&libVer, driver, typeVer_ptr);
  CHECK_ERROR (r == -1, NULL, "virGetVersion");

  rv = caml_alloc_tuple (2);
  Store_field (rv, 0, Val_int (libVer));
  Store_field (rv, 1, Val_int (typeVer));
  CAMLreturn (rv);
}

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
 */

/* Unwrap a custom block. */
#define Connect_val(rv) (*((virConnectPtr *)Data_custom_val(rv)))
#define Dom_val(rv) (*((virDomainPtr *)Data_custom_val(rv)))
#define Net_val(rv) (*((virNetworkPtr *)Data_custom_val(rv)))

/* Wrap up a pointer to something in a custom block. */
static value Val_connect (virConnectPtr conn);
static value Val_dom (virDomainPtr dom);
static value Val_net (virNetworkPtr net);

/* ONLY for use by virterror wrappers. */
static value Val_connect_no_finalize (virConnectPtr conn);
static value Val_dom_no_finalize (virDomainPtr dom);
static value Val_net_no_finalize (virNetworkPtr net);

/* Domains and networks are stored as pairs (dom/net, conn), so have
 * some convenience functions for unwrapping and wrapping them.
 */
#define Domain_val(rv) (Dom_val(Field((rv),0)))
#define Network_val(rv) (Net_val(Field((rv),0)))
#define Connect_domv(rv) (Connect_val(Field((rv),1)))
#define Connect_netv(rv) (Connect_val(Field((rv),1)))

static value Val_domain (virDomainPtr dom, value connv);
static value Val_network (virNetworkPtr net, value connv);

/* ONLY for use by virterror wrappers. */
static value Val_domain_no_finalize (virDomainPtr dom, value connv);
static value Val_network_no_finalize (virNetworkPtr net, value connv);

/*----------------------------------------------------------------------*/

/* Connection object. */

CAMLprim value
ocaml_libvirt_connect_open (value namev, value unit)
{
  CAMLparam2 (namev, unit);
  CAMLlocal1 (rv);
  const char *name = Optstring_val (namev);
  virConnectPtr conn;

  conn = virConnectOpen (name);
  CHECK_ERROR (!conn, NULL, "virConnectOpen");

  rv = Val_connect (conn);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_open_readonly (value namev, value unit)
{
  CAMLparam2 (namev, unit);
  CAMLlocal1 (rv);
  const char *name = Optstring_val (namev);
  virConnectPtr conn;

  conn = virConnectOpenReadOnly (name);
  CHECK_ERROR (!conn, NULL, "virConnectOpen");

  rv = Val_connect (conn);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_close (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  int r;

  r = virConnectClose (conn);
  CHECK_ERROR (r == -1, conn, "virConnectClose");

  /* So that we don't double-free in the finalizer: */
  Connect_val (connv) = NULL;

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_connect_get_type (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  const char *r;

  r = virConnectGetType (conn);
  CHECK_ERROR (!r, conn, "virConnectGetType");

  rv = caml_copy_string (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_get_version (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  unsigned long hvVer;
  int r;

  r = virConnectGetVersion (conn, &hvVer);
  CHECK_ERROR (r == -1, conn, "virConnectGetVersion");

  CAMLreturn (Val_int (hvVer));
}

CAMLprim value
ocaml_libvirt_connect_get_hostname (value connv)
{
#ifdef HAVE_VIRCONNECTGETHOSTNAME
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *r;

  WEAK_SYMBOL_CHECK (virConnectGetHostname);
  r = virConnectGetHostname (conn);
  CHECK_ERROR (!r, conn, "virConnectGetHostname");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
#else
  NOT_SUPPORTED ("virConnectGetHostname");
#endif
}

CAMLprim value
ocaml_libvirt_connect_get_uri (value connv)
{
#ifdef HAVE_VIRCONNECTGETURI
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *r;

  WEAK_SYMBOL_CHECK (virConnectGetURI);
  r = virConnectGetURI (conn);
  CHECK_ERROR (!r, conn, "virConnectGetURI");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
#else
  NOT_SUPPORTED ("virConnectGetURI");
#endif
}

CAMLprim value
ocaml_libvirt_connect_get_max_vcpus (value connv, value typev)
{
  CAMLparam2 (connv, typev);
  virConnectPtr conn = Connect_val (connv);
  const char *type = Optstring_val (typev);
  int r;

  r = virConnectGetMaxVcpus (conn, type);
  CHECK_ERROR (r == -1, conn, "virConnectGetMaxVcpus");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_connect_list_domains (value connv, value iv)
{
  CAMLparam2 (connv, iv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  int ids[i], r;

  r = virConnectListDomains (conn, ids, i);
  CHECK_ERROR (r == -1, conn, "virConnectListDomains");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i)
    Store_field (rv, i, Val_int (ids[i]));

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_num_of_domains (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  int r;

  r = virConnectNumOfDomains (conn);
  CHECK_ERROR (r == -1, conn, "virConnectNumOfDomains");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_connect_get_capabilities (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *r;

  r = virConnectGetCapabilities (conn);
  CHECK_ERROR (!r, conn, "virConnectGetCapabilities");

  rv = caml_copy_string (r);
  free (r);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_num_of_defined_domains (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  int r;

  r = virConnectNumOfDefinedDomains (conn);
  CHECK_ERROR (r == -1, conn, "virConnectNumOfDefinedDomains");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_connect_list_defined_domains (value connv, value iv)
{
  CAMLparam2 (connv, iv);
  CAMLlocal2 (rv, strv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  char *names[i];
  int r;

  r = virConnectListDefinedDomains (conn, names, i);
  CHECK_ERROR (r == -1, conn, "virConnectListDefinedDomains");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i) {
    strv = caml_copy_string (names[i]);
    Store_field (rv, i, strv);
    free (names[i]);
  }

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_num_of_networks (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  int r;

  r = virConnectNumOfNetworks (conn);
  CHECK_ERROR (r == -1, conn, "virConnectNumOfNetworks");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_connect_list_networks (value connv, value iv)
{
  CAMLparam2 (connv, iv);
  CAMLlocal2 (rv, strv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  char *names[i];
  int r;

  r = virConnectListNetworks (conn, names, i);
  CHECK_ERROR (r == -1, conn, "virConnectListNetworks");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i) {
    strv = caml_copy_string (names[i]);
    Store_field (rv, i, strv);
    free (names[i]);
  }

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_num_of_defined_networks (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  int r;

  r = virConnectNumOfDefinedNetworks (conn);
  CHECK_ERROR (r == -1, conn, "virConnectNumOfDefinedNetworks");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_connect_list_defined_networks (value connv, value iv)
{
  CAMLparam2 (connv, iv);
  CAMLlocal2 (rv, strv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  char *names[i];
  int r;

  r = virConnectListDefinedNetworks (conn, names, i);
  CHECK_ERROR (r == -1, conn, "virConnectListDefinedNetworks");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i) {
    strv = caml_copy_string (names[i]);
    Store_field (rv, i, strv);
    free (names[i]);
  }

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_connect_get_node_info (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);
  virConnectPtr conn = Connect_val (connv);
  virNodeInfo info;
  int r;

  r = virNodeGetInfo (conn, &info);
  CHECK_ERROR (r == -1, conn, "virNodeGetInfo");

  rv = caml_alloc (8, 0);
  v = caml_copy_string (info.model); Store_field (rv, 0, v);
  v = caml_copy_int64 (info.memory); Store_field (rv, 1, v);
  Store_field (rv, 2, Val_int (info.cpus));
  Store_field (rv, 3, Val_int (info.mhz));
  Store_field (rv, 4, Val_int (info.nodes));
  Store_field (rv, 5, Val_int (info.sockets));
  Store_field (rv, 6, Val_int (info.cores));
  Store_field (rv, 7, Val_int (info.threads));

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_create_linux (value connv, value xmlv)
{
  CAMLparam2 (connv, xmlv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *xml = String_val (xmlv);
  virDomainPtr r;

  r = virDomainCreateLinux (conn, xml, 0);
  CHECK_ERROR (!r, conn, "virDomainCreateLinux");

  rv = Val_domain (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_lookup_by_id (value connv, value iv)
{
  CAMLparam2 (connv, iv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  virDomainPtr r;

  r = virDomainLookupByID (conn, i);
  CHECK_ERROR (!r, conn, "virDomainLookupByID");

  rv = Val_domain (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_lookup_by_uuid (value connv, value uuidv)
{
  CAMLparam2 (connv, uuidv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *uuid = String_val (uuidv);
  virDomainPtr r;

  r = virDomainLookupByUUID (conn, (unsigned char *) uuid);
  CHECK_ERROR (!r, conn, "virDomainLookupByUUID");

  rv = Val_domain (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_lookup_by_uuid_string (value connv, value uuidv)
{
  CAMLparam2 (connv, uuidv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *uuid = String_val (uuidv);
  virDomainPtr r;

  r = virDomainLookupByUUIDString (conn, uuid);
  CHECK_ERROR (!r, conn, "virDomainLookupByUUIDString");

  rv = Val_domain (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_lookup_by_name (value connv, value namev)
{
  CAMLparam2 (connv, namev);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *name = String_val (namev);
  virDomainPtr r;

  r = virDomainLookupByName (conn, name);
  CHECK_ERROR (!r, conn, "virDomainLookupByName");

  rv = Val_domain (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_destroy (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainDestroy (dom);
  CHECK_ERROR (r == -1, conn, "virDomainDestroy");

  /* So that we don't double-free in the finalizer: */
  Domain_val (domv) = NULL;

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_free (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainFree (dom);
  CHECK_ERROR (r == -1, conn, "virDomainFree");

  /* So that we don't double-free in the finalizer: */
  Domain_val (domv) = NULL;

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_suspend (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainSuspend (dom);
  CHECK_ERROR (r == -1, conn, "virDomainSuspend");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_resume (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainResume (dom);
  CHECK_ERROR (r == -1, conn, "virDomainResume");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_save (value domv, value pathv)
{
  CAMLparam2 (domv, pathv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *path = String_val (pathv);
  int r;

  r = virDomainSave (dom, path);
  CHECK_ERROR (r == -1, conn, "virDomainSave");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_restore (value connv, value pathv)
{
  CAMLparam2 (connv, pathv);
  virConnectPtr conn = Connect_val (connv);
  char *path = String_val (pathv);
  int r;

  r = virDomainRestore (conn, path);
  CHECK_ERROR (r == -1, conn, "virDomainRestore");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_core_dump (value domv, value pathv)
{
  CAMLparam2 (domv, pathv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *path = String_val (pathv);
  int r;

  r = virDomainCoreDump (dom, path, 0);
  CHECK_ERROR (r == -1, conn, "virDomainCoreDump");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_shutdown (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainShutdown (dom);
  CHECK_ERROR (r == -1, conn, "virDomainShutdown");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_reboot (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainReboot (dom, 0);
  CHECK_ERROR (r == -1, conn, "virDomainReboot");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_get_name (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  const char *r;

  r = virDomainGetName (dom);
  CHECK_ERROR (!r, conn, "virDomainGetName");

  rv = caml_copy_string (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_uuid (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned char uuid[VIR_UUID_BUFLEN];
  int r;

  r = virDomainGetUUID (dom, uuid);
  CHECK_ERROR (r == -1, conn, "virDomainGetUUID");

  rv = caml_copy_string ((char *) uuid);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_uuid_string (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char uuid[VIR_UUID_STRING_BUFLEN];
  int r;

  r = virDomainGetUUIDString (dom, uuid);
  CHECK_ERROR (r == -1, conn, "virDomainGetUUIDString");

  rv = caml_copy_string (uuid);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_id (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned int r;

  r = virDomainGetID (dom);
  /* There's a bug in libvirt which means that if you try to get
   * the ID of a defined-but-not-running domain, it returns -1,
   * and there's no way to distinguish that from an error.
   */
  CHECK_ERROR (r == (unsigned int) -1, conn, "virDomainGetID");

  CAMLreturn (Val_int ((int) r));
}

CAMLprim value
ocaml_libvirt_domain_get_os_type (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *r;

  r = virDomainGetOSType (dom);
  CHECK_ERROR (!r, conn, "virDomainGetOSType");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_max_memory (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned long r;

  r = virDomainGetMaxMemory (dom);
  CHECK_ERROR (r == 0 /* [sic] */, conn, "virDomainGetMaxMemory");

  rv = caml_copy_int64 (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_set_max_memory (value domv, value memv)
{
  CAMLparam2 (domv, memv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned long mem = Int64_val (memv);
  int r;

  r = virDomainSetMaxMemory (dom, mem);
  CHECK_ERROR (r == -1, conn, "virDomainSetMaxMemory");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_set_memory (value domv, value memv)
{
  CAMLparam2 (domv, memv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  unsigned long mem = Int64_val (memv);
  int r;

  r = virDomainSetMemory (dom, mem);
  CHECK_ERROR (r == -1, conn, "virDomainSetMemory");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_get_info (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal2 (rv, v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  virDomainInfo info;
  int r;

  r = virDomainGetInfo (dom, &info);
  CHECK_ERROR (r == -1, conn, "virDomainGetInfo");

  rv = caml_alloc (5, 0);
  Store_field (rv, 0, Val_int (info.state)); // These flags are compatible.
  v = caml_copy_int64 (info.maxMem); Store_field (rv, 1, v);
  v = caml_copy_int64 (info.memory); Store_field (rv, 2, v);
  Store_field (rv, 3, Val_int (info.nrVirtCpu));
  v = caml_copy_int64 (info.cpuTime); Store_field (rv, 4, v);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_xml_desc (value domv)
{
  CAMLparam1 (domv);
  CAMLlocal1 (rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *r;

  r = virDomainGetXMLDesc (dom, 0);
  CHECK_ERROR (!r, conn, "virDomainGetXMLDesc");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_scheduler_type (value domv)
{
#ifdef HAVE_VIRDOMAINGETSCHEDULERTYPE
  CAMLparam1 (domv);
  CAMLlocal2 (rv, strv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *r;
  int nparams;

  WEAK_SYMBOL_CHECK (virDomainGetSchedulerType);
  r = virDomainGetSchedulerType (dom, &nparams);
  CHECK_ERROR (!r, conn, "virDomainGetSchedulerType");

  rv = caml_alloc_tuple (2);
  strv = caml_copy_string (r); Store_field (rv, 0, strv);
  free (r);
  Store_field (rv, 1, nparams);
  CAMLreturn (rv);
#else
  NOT_SUPPORTED ("virDomainGetSchedulerType");
#endif
}

CAMLprim value
ocaml_libvirt_domain_get_scheduler_parameters (value domv, value nparamsv)
{
#ifdef HAVE_VIRDOMAINGETSCHEDULERPARAMETERS
  CAMLparam2 (domv, nparamsv);
  CAMLlocal4 (rv, v, v2, v3);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int nparams = Int_val (nparamsv);
  virSchedParameter params[nparams];
  int r, i;

  WEAK_SYMBOL_CHECK (virDomainGetSchedulerParameters);
  r = virDomainGetSchedulerParameters (dom, params, &nparams);
  CHECK_ERROR (r == -1, conn, "virDomainGetSchedulerParameters");

  rv = caml_alloc (nparams, 0);
  for (i = 0; i < nparams; ++i) {
    v = caml_alloc_tuple (2); Store_field (rv, i, v);
    v2 = caml_copy_string (params[i].field); Store_field (v, 0, v2);
    switch (params[i].type) {
    case VIR_DOMAIN_SCHED_FIELD_INT:
      v2 = caml_alloc (1, 0);
      v3 = caml_copy_int32 (params[i].value.i); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_UINT:
      v2 = caml_alloc (1, 1);
      v3 = caml_copy_int32 (params[i].value.ui); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_LLONG:
      v2 = caml_alloc (1, 2);
      v3 = caml_copy_int64 (params[i].value.l); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_ULLONG:
      v2 = caml_alloc (1, 3);
      v3 = caml_copy_int64 (params[i].value.ul); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_DOUBLE:
      v2 = caml_alloc (1, 4);
      v3 = caml_copy_double (params[i].value.d); Store_field (v2, 0, v3);
      break;
    case VIR_DOMAIN_SCHED_FIELD_BOOLEAN:
      v2 = caml_alloc (1, 5);
      Store_field (v2, 0, Val_int (params[i].value.b));
      break;
    default:
      caml_failwith ((char *)__FUNCTION__);
    }
    Store_field (v, 1, v2);
  }
  CAMLreturn (rv);
#else
  NOT_SUPPORTED ("virDomainGetSchedulerParameters");
#endif
}

CAMLprim value
ocaml_libvirt_domain_set_scheduler_parameters (value domv, value paramsv)
{
#ifdef HAVE_VIRDOMAINSETSCHEDULERPARAMETERS
  CAMLparam2 (domv, paramsv);
  CAMLlocal1 (v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int nparams = Wosize_val (paramsv);
  virSchedParameter params[nparams];
  int r, i;
  char *name;

  for (i = 0; i < nparams; ++i) {
    v = Field (paramsv, i);	/* Points to the two-element tuple. */
    name = String_val (Field (v, 0));
    strncpy (params[i].field, name, VIR_DOMAIN_SCHED_FIELD_LENGTH);
    params[i].field[VIR_DOMAIN_SCHED_FIELD_LENGTH-1] = '\0';
    v = Field (v, 1);		/* Points to the sched_param_value block. */
    switch (Tag_val (v)) {
    case 0:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_INT;
      params[i].value.i = Int32_val (Field (v, 0));
      break;
    case 1:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_UINT;
      params[i].value.ui = Int32_val (Field (v, 0));
      break;
    case 2:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_LLONG;
      params[i].value.l = Int64_val (Field (v, 0));
      break;
    case 3:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_ULLONG;
      params[i].value.ul = Int64_val (Field (v, 0));
      break;
    case 4:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_DOUBLE;
      params[i].value.d = Double_val (Field (v, 0));
      break;
    case 5:
      params[i].type = VIR_DOMAIN_SCHED_FIELD_BOOLEAN;
      params[i].value.b = Int_val (Field (v, 0));
      break;
    default:
      caml_failwith ((char *)__FUNCTION__);
    }
  }

  WEAK_SYMBOL_CHECK (virDomainSetSchedulerParameters);
  r = virDomainSetSchedulerParameters (dom, params, nparams);
  CHECK_ERROR (r == -1, conn, "virDomainSetSchedulerParameters");

  CAMLreturn (Val_unit);
#else
  NOT_SUPPORTED ("virDomainSetSchedulerParameters");
#endif
}

CAMLprim value
ocaml_libvirt_domain_define_xml (value connv, value xmlv)
{
  CAMLparam2 (connv, xmlv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *xml = String_val (xmlv);
  virDomainPtr r;

  r = virDomainDefineXML (conn, xml);
  CHECK_ERROR (!r, conn, "virDomainDefineXML");

  rv = Val_domain (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_undefine (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainUndefine (dom);
  CHECK_ERROR (r == -1, conn, "virDomainUndefine");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_create (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainCreate (dom);
  CHECK_ERROR (r == -1, conn, "virDomainCreate");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_get_autostart (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r, autostart;

  r = virDomainGetAutostart (dom, &autostart);
  CHECK_ERROR (r == -1, conn, "virDomainGetAutostart");

  CAMLreturn (autostart ? Val_true : Val_false);
}

CAMLprim value
ocaml_libvirt_domain_set_autostart (value domv, value autostartv)
{
  CAMLparam2 (domv, autostartv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r, autostart = autostartv == Val_true ? 1 : 0;

  r = virDomainSetAutostart (dom, autostart);
  CHECK_ERROR (r == -1, conn, "virDomainSetAutostart");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_set_vcpus (value domv, value nvcpusv)
{
  CAMLparam2 (domv, nvcpusv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r, nvcpus = Int_val (nvcpusv);

  r = virDomainSetVcpus (dom, nvcpus);
  CHECK_ERROR (r == -1, conn, "virDomainSetVcpus");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_pin_vcpu (value domv, value vcpuv, value cpumapv)
{
  CAMLparam3 (domv, vcpuv, cpumapv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int maplen = caml_string_length (cpumapv);
  unsigned char *cpumap = (unsigned char *) String_val (cpumapv);
  int vcpu = Int_val (vcpuv);
  int r;

  r = virDomainPinVcpu (dom, vcpu, cpumap, maplen);
  CHECK_ERROR (r == -1, conn, "virDomainPinVcpu");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_get_vcpus (value domv, value maxinfov, value maplenv)
{
  CAMLparam3 (domv, maxinfov, maplenv);
  CAMLlocal5 (rv, infov, strv, v, v2);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int maxinfo = Int_val (maxinfov);
  int maplen = Int_val (maplenv);
  virVcpuInfo info[maxinfo];
  unsigned char cpumaps[maxinfo * maplen];
  int r, i;

  memset (info, 0, sizeof (virVcpuInfo) * maxinfo);
  memset (cpumaps, 0, maxinfo * maplen);

  r = virDomainGetVcpus (dom, info, maxinfo, cpumaps, maplen);
  CHECK_ERROR (r == -1, conn, "virDomainPinVcpu");

  /* Copy the virVcpuInfo structures. */
  infov = caml_alloc (maxinfo, 0);
  for (i = 0; i < maxinfo; ++i) {
    v2 = caml_alloc (4, 0); Store_field (infov, i, v2);
    Store_field (v2, 0, Val_int (info[i].number));
    Store_field (v2, 1, Val_int (info[i].state));
    v = caml_copy_int64 (info[i].cpuTime); Store_field (v2, 2, v);
    Store_field (v2, 3, Val_int (info[i].cpu));
  }

  /* Copy the bitmap. */
  strv = caml_alloc_string (maxinfo * maplen);
  memcpy (String_val (strv), cpumaps, maxinfo * maplen);

  /* Allocate the tuple and return it. */
  rv = caml_alloc_tuple (3);
  Store_field (rv, 0, Val_int (r)); /* number of CPUs. */
  Store_field (rv, 1, infov);
  Store_field (rv, 2, strv);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_domain_get_max_vcpus (value domv)
{
  CAMLparam1 (domv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  int r;

  r = virDomainGetMaxVcpus (dom);
  CHECK_ERROR (r == -1, conn, "virDomainGetMaxVcpus");

  CAMLreturn (Val_int (r));
}

CAMLprim value
ocaml_libvirt_domain_attach_device (value domv, value xmlv)
{
  CAMLparam2 (domv, xmlv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *xml = String_val (xmlv);
  int r;

  r = virDomainAttachDevice (dom, xml);
  CHECK_ERROR (r == -1, conn, "virDomainAttachDevice");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_detach_device (value domv, value xmlv)
{
  CAMLparam2 (domv, xmlv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *xml = String_val (xmlv);
  int r;

  r = virDomainDetachDevice (dom, xml);
  CHECK_ERROR (r == -1, conn, "virDomainDetachDevice");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_domain_migrate_native (value domv, value dconnv, value flagsv, value optdnamev, value opturiv, value optbandwidthv, value unitv)
{
#ifdef HAVE_VIRDOMAINMIGRATE
  CAMLparam5 (domv, dconnv, flagsv, optdnamev, opturiv);
  CAMLxparam2 (optbandwidthv, unitv);
  CAMLlocal2 (flagv, rv);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  virConnectPtr dconn = Connect_val (dconnv);
  int flags = 0;
  const char *dname = Optstring_val (optdnamev);
  const char *uri = Optstring_val (opturiv);
  unsigned long bandwidth;
  virDomainPtr r;

  /* Iterate over the list of flags. */
  for (; flagsv != Val_int (0); flagsv = Field (flagsv, 1))
    {
      flagv = Field (flagsv, 0);
      if (flagv == Int_val(0))
	flags |= VIR_MIGRATE_LIVE;
    }

  if (optbandwidthv == Val_int (0)) /* None */
    bandwidth = 0;
  else				/* Some bandwidth */
    bandwidth = Int_val (Field (optbandwidthv, 0));

  WEAK_SYMBOL_CHECK (virDomainMigrate);
  r = virDomainMigrate (dom, dconn, flags, dname, uri, bandwidth);
  CHECK_ERROR (!r, conn, "virDomainMigrate");

  rv = Val_domain (r, dconnv);

  CAMLreturn (rv);

#else /* virDomainMigrate not supported */
  NOT_SUPPORTED ("virDomainMigrate");
#endif
}

CAMLprim value
ocaml_libvirt_domain_migrate_bytecode (value *argv, int argn)
{
  return ocaml_libvirt_domain_migrate_native (argv[0], argv[1], argv[2],
					      argv[3], argv[4], argv[5],
					      argv[6]);
}

CAMLprim value
ocaml_libvirt_domain_block_stats (value domv, value pathv)
{
#if HAVE_VIRDOMAINBLOCKSTATS
  CAMLparam2 (domv, pathv);
  CAMLlocal2 (rv,v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *path = String_val (pathv);
  struct _virDomainBlockStats stats;
  int r;

  WEAK_SYMBOL_CHECK (virDomainBlockStats);
  r = virDomainBlockStats (dom, path, &stats, sizeof stats);
  CHECK_ERROR (r == -1, conn, "virDomainBlockStats");

  rv = caml_alloc (5, 0);
  v = caml_copy_int64 (stats.rd_req); Store_field (rv, 0, v);
  v = caml_copy_int64 (stats.rd_bytes); Store_field (rv, 1, v);
  v = caml_copy_int64 (stats.wr_req); Store_field (rv, 2, v);
  v = caml_copy_int64 (stats.wr_bytes); Store_field (rv, 3, v);
  v = caml_copy_int64 (stats.errs); Store_field (rv, 4, v);

  CAMLreturn (rv);
#else
  NOT_SUPPORTED ("virDomainBlockStats");
#endif
}

CAMLprim value
ocaml_libvirt_domain_interface_stats (value domv, value pathv)
{
#if HAVE_VIRDOMAININTERFACESTATS
  CAMLparam2 (domv, pathv);
  CAMLlocal2 (rv,v);
  virDomainPtr dom = Domain_val (domv);
  virConnectPtr conn = Connect_domv (domv);
  char *path = String_val (pathv);
  struct _virDomainInterfaceStats stats;
  int r;

  WEAK_SYMBOL_CHECK (virDomainInterfaceStats);
  r = virDomainInterfaceStats (dom, path, &stats, sizeof stats);
  CHECK_ERROR (r == -1, conn, "virDomainInterfaceStats");

  rv = caml_alloc (8, 0);
  v = caml_copy_int64 (stats.rx_bytes); Store_field (rv, 0, v);
  v = caml_copy_int64 (stats.rx_packets); Store_field (rv, 1, v);
  v = caml_copy_int64 (stats.rx_errs); Store_field (rv, 2, v);
  v = caml_copy_int64 (stats.rx_drop); Store_field (rv, 3, v);
  v = caml_copy_int64 (stats.tx_bytes); Store_field (rv, 4, v);
  v = caml_copy_int64 (stats.tx_packets); Store_field (rv, 5, v);
  v = caml_copy_int64 (stats.tx_errs); Store_field (rv, 6, v);
  v = caml_copy_int64 (stats.tx_drop); Store_field (rv, 7, v);

  CAMLreturn (rv);
#else
  NOT_SUPPORTED ("virDomainInterfaceStats");
#endif
}

CAMLprim value
ocaml_libvirt_network_lookup_by_name (value connv, value namev)
{
  CAMLparam2 (connv, namev);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *name = String_val (namev);
  virNetworkPtr r;

  r = virNetworkLookupByName (conn, name);
  CHECK_ERROR (!r, conn, "virNetworkLookupByName");

  rv = Val_network (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_lookup_by_uuid (value connv, value uuidv)
{
  CAMLparam2 (connv, uuidv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *uuid = String_val (uuidv);
  virNetworkPtr r;

  r = virNetworkLookupByUUID (conn, (unsigned char *) uuid);
  CHECK_ERROR (!r, conn, "virNetworkLookupByUUID");

  rv = Val_network (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_lookup_by_uuid_string (value connv, value uuidv)
{
  CAMLparam2 (connv, uuidv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *uuid = String_val (uuidv);
  virNetworkPtr r;

  r = virNetworkLookupByUUIDString (conn, uuid);
  CHECK_ERROR (!r, conn, "virNetworkLookupByUUIDString");

  rv = Val_network (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_create_xml (value connv, value xmlv)
{
  CAMLparam2 (connv, xmlv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *xml = String_val (xmlv);
  virNetworkPtr r;

  r = virNetworkCreateXML (conn, xml);
  CHECK_ERROR (!r, conn, "virNetworkCreateXML");

  rv = Val_network (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_define_xml (value connv, value xmlv)
{
  CAMLparam2 (connv, xmlv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  char *xml = String_val (xmlv);
  virNetworkPtr r;

  r = virNetworkDefineXML (conn, xml);
  CHECK_ERROR (!r, conn, "virNetworkDefineXML");

  rv = Val_network (r, connv);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_undefine (value netv)
{
  CAMLparam1 (netv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  int r;

  r = virNetworkUndefine (net);
  CHECK_ERROR (r == -1, conn, "virNetworkUndefine");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_network_create (value netv)
{
  CAMLparam1 (netv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  int r;

  r = virNetworkCreate (net);
  CHECK_ERROR (r == -1, conn, "virNetworkCreate");

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_network_destroy (value netv)
{
  CAMLparam1 (netv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  int r;

  r = virNetworkDestroy (net);
  CHECK_ERROR (r == -1, conn, "virNetworkDestroy");

  /* So that we don't double-free in the finalizer: */
  Network_val (netv) = NULL;

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_network_free (value netv)
{
  CAMLparam1 (netv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  int r;

  r = virNetworkFree (net);
  CHECK_ERROR (r == -1, conn, "virNetworkFree");

  /* So that we don't double-free in the finalizer: */
  Network_val (netv) = NULL;

  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_network_get_name (value netv)
{
  CAMLparam1 (netv);
  CAMLlocal1 (rv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  const char *r;

  r = virNetworkGetName (net);
  CHECK_ERROR (!r, conn, "virNetworkGetName");

  rv = caml_copy_string (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_get_uuid (value netv)
{
  CAMLparam1 (netv);
  CAMLlocal1 (rv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  unsigned char uuid[VIR_UUID_BUFLEN];
  int r;

  r = virNetworkGetUUID (net, uuid);
  CHECK_ERROR (r == -1, conn, "virNetworkGetUUID");

  rv = caml_copy_string ((char *) uuid);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_get_uuid_string (value netv)
{
  CAMLparam1 (netv);
  CAMLlocal1 (rv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  char uuid[VIR_UUID_STRING_BUFLEN];
  int r;

  r = virNetworkGetUUIDString (net, uuid);
  CHECK_ERROR (r == -1, conn, "virNetworkGetUUIDString");

  rv = caml_copy_string (uuid);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_get_xml_desc (value netv)
{
  CAMLparam1 (netv);
  CAMLlocal1 (rv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  char *r;

  r = virNetworkGetXMLDesc (net, 0);
  CHECK_ERROR (!r, conn, "virNetworkGetXMLDesc");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_get_bridge_name (value netv)
{
  CAMLparam1 (netv);
  CAMLlocal1 (rv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  char *r;

  r = virNetworkGetBridgeName (net);
  CHECK_ERROR (!r, conn, "virNetworkGetBridgeName");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_network_get_autostart (value netv)
{
  CAMLparam1 (netv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  int r, autostart;

  r = virNetworkGetAutostart (net, &autostart);
  CHECK_ERROR (r == -1, conn, "virNetworkGetAutostart");

  CAMLreturn (autostart ? Val_true : Val_false);
}

CAMLprim value
ocaml_libvirt_network_set_autostart (value netv, value autostartv)
{
  CAMLparam2 (netv, autostartv);
  virNetworkPtr net = Network_val (netv);
  virConnectPtr conn = Connect_netv (netv);
  int r, autostart = autostartv == Val_true ? 1 : 0;

  r = virNetworkSetAutostart (net, autostart);
  CHECK_ERROR (r == -1, conn, "virNetworkSetAutostart");

  CAMLreturn (Val_unit);
}

/*----------------------------------------------------------------------*/

CAMLprim value
ocaml_libvirt_virterror_get_last_error (value unitv)
{
  CAMLparam1 (unitv);
  CAMLlocal1 (rv);
  virErrorPtr err = virGetLastError ();

  rv = Val_opt (err, (Val_ptr_t) Val_virterror);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_virterror_get_last_conn_error (value connv)
{
  CAMLparam1 (connv);
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);

  rv = Val_opt (conn, (Val_ptr_t) Val_connect);

  CAMLreturn (rv);
}

CAMLprim value
ocaml_libvirt_virterror_reset_last_error (value unitv)
{
  CAMLparam1 (unitv);
  virResetLastError ();
  CAMLreturn (Val_unit);
}

CAMLprim value
ocaml_libvirt_virterror_reset_last_conn_error (value connv)
{
  CAMLparam1 (connv);
  virConnectPtr conn = Connect_val (connv);
  virConnResetLastError (conn);
  CAMLreturn (Val_unit);
}

/*----------------------------------------------------------------------*/

/* Initialise the library. */
CAMLprim value
ocaml_libvirt_init (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (rv);
  int r;

  r = virInitialize ();
  CHECK_ERROR (r == -1, NULL, "virInitialize");

  CAMLreturn (Val_unit);
}

/*----------------------------------------------------------------------*/

static char *
Optstring_val (value strv)
{
  if (strv == Val_int (0))	/* None */
    return NULL;
  else				/* Some string */
    return String_val (Field (strv, 0));
}

static value
Val_opt (void *ptr, Val_ptr_t Val_ptr)
{
  CAMLparam0 ();
  CAMLlocal2 (optv, ptrv);

  if (ptr) {			/* Some ptr */
    optv = caml_alloc (1, 0);
    ptrv = Val_ptr (ptr);
    Store_field (optv, 0, ptrv);
  } else			/* None */
    optv = Val_int (0);

  CAMLreturn (optv);
}

#if 0
static value
option_default (value option, value deflt)
{
  if (option == Val_int (0))    /* "None" */
    return deflt;
  else                          /* "Some 'a" */
    return Field (option, 0);
}
#endif

static value
_raise_virterror (virConnectPtr conn, const char *fn)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  virErrorPtr errp;
  struct _virError err;

  errp = conn ? virConnGetLastError (conn) : virGetLastError ();

  if (!errp) {
    /* Fake a _virError structure. */
    memset (&err, 0, sizeof err);
    err.code = VIR_ERR_INTERNAL_ERROR;
    err.domain = VIR_FROM_NONE;
    err.level = VIR_ERR_ERROR;
    err.message = (char *) fn;
    errp = &err;
  }

  rv = Val_virterror (errp);
  caml_raise_with_arg (*caml_named_value ("ocaml_libvirt_virterror"), rv);

  /*NOTREACHED*/
  CAMLreturn (Val_unit);
}

static value
Val_virterror (virErrorPtr err)
{
  CAMLparam0 ();
  CAMLlocal3 (rv, connv, optv);

  rv = caml_alloc (12, 0);
  Store_field (rv, 0, Val_int (err->code));
  Store_field (rv, 1, Val_int (err->domain));
  Store_field (rv, 2,
	       Val_opt (err->message, (Val_ptr_t) caml_copy_string));
  Store_field (rv, 3, Val_int (err->level));

  /* conn, dom and net fields, all optional */
  if (err->conn) {
    connv = Val_connect_no_finalize (err->conn);
    optv = caml_alloc (1, 0);
    Store_field (optv, 0, connv);
    Store_field (rv, 4, optv);	/* Some conn */

    if (err->dom) {
      optv = caml_alloc (1, 0);
      Store_field (optv, 0, Val_domain_no_finalize (err->dom, connv));
      Store_field (rv, 5, optv); /* Some (dom, conn) */
    }
    else
      Store_field (rv, 5, Val_int (0)); /* None */
    if (err->net) {
      optv = caml_alloc (1, 0);
      Store_field (optv, 0, Val_network_no_finalize (err->net, connv));
      Store_field (rv, 11, optv); /* Some (net, conn) */
    } else
      Store_field (rv, 11, Val_int (0)); /* None */
  } else {
    Store_field (rv, 4, Val_int (0)); /* None */
    Store_field (rv, 5, Val_int (0)); /* None */
    Store_field (rv, 11, Val_int (0)); /* None */
  }

  Store_field (rv, 6,
	       Val_opt (err->str1, (Val_ptr_t) caml_copy_string));
  Store_field (rv, 7,
	       Val_opt (err->str2, (Val_ptr_t) caml_copy_string));
  Store_field (rv, 8,
	       Val_opt (err->str3, (Val_ptr_t) caml_copy_string));
  Store_field (rv, 9, caml_copy_int32 (err->int1));
  Store_field (rv, 10, caml_copy_int32 (err->int2));

  CAMLreturn (rv);
}

static void conn_finalize (value);
static void dom_finalize (value);
static void net_finalize (value);

static struct custom_operations conn_custom_operations = {
  "conn_custom_operations",
  conn_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static struct custom_operations dom_custom_operations = {
  "dom_custom_operations",
  dom_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default

};

static struct custom_operations net_custom_operations = {
  "net_custom_operations",
  net_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value
Val_connect (virConnectPtr conn)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc_custom (&conn_custom_operations,
			  sizeof (virConnectPtr), 0, 1);
  Connect_val (rv) = conn;
  CAMLreturn (rv);
}

/* This wraps up the raw domain handle (Domain.dom). */
static value
Val_dom (virDomainPtr dom)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc_custom (&dom_custom_operations,
			  sizeof (virDomainPtr), 0, 1);
  Dom_val (rv) = dom;
  CAMLreturn (rv);
}

/* This wraps up the raw network handle (Network.net). */
static value
Val_net (virNetworkPtr net)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc_custom (&net_custom_operations,
			  sizeof (virNetworkPtr), 0, 1);
  Net_val (rv) = net;
  CAMLreturn (rv);
}

/* No-finalize versions of Val_connect, Val_dom, Val_net ONLY for use
 * by virterror wrappers.
 */
static value
Val_connect_no_finalize (virConnectPtr conn)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc (1, Abstract_tag);
  Store_field (rv, 0, (value) conn);
  CAMLreturn (rv);
}

static value
Val_dom_no_finalize (virDomainPtr dom)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc (1, Abstract_tag);
  Store_field (rv, 0, (value) dom);
  CAMLreturn (rv);
}

static value
Val_net_no_finalize (virNetworkPtr net)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc (1, Abstract_tag);
  Store_field (rv, 0, (value) net);
  CAMLreturn (rv);
}

/* This wraps up the (dom, conn) pair (Domain.t). */
static value
Val_domain (virDomainPtr dom, value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);

  rv = caml_alloc_tuple (2);
  v = Val_dom (dom);
  Store_field (rv, 0, v);
  Store_field (rv, 1, connv);
  CAMLreturn (rv);
}

/* This wraps up the (net, conn) pair (Network.t). */
static value
Val_network (virNetworkPtr net, value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);

  rv = caml_alloc_tuple (2);
  v = Val_net (net);
  Store_field (rv, 0, v);
  Store_field (rv, 1, connv);
  CAMLreturn (rv);
}

/* No-finalize versions of Val_domain, Val_network ONLY for use by
 * virterror wrappers.
 */
static value
Val_domain_no_finalize (virDomainPtr dom, value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);

  rv = caml_alloc_tuple (2);
  v = Val_dom_no_finalize (dom);
  Store_field (rv, 0, v);
  Store_field (rv, 1, connv);
  CAMLreturn (rv);
}

static value
Val_network_no_finalize (virNetworkPtr net, value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);

  rv = caml_alloc_tuple (2);
  v = Val_net_no_finalize (net);
  Store_field (rv, 0, v);
  Store_field (rv, 1, connv);
  CAMLreturn (rv);
}

static void
conn_finalize (value connv)
{
  virConnectPtr conn = Connect_val (connv);
  if (conn) (void) virConnectClose (conn);
}

static void
dom_finalize (value domv)
{
  virDomainPtr dom = Dom_val (domv);
  if (dom) (void) virDomainFree (dom);
}

static void
net_finalize (value netv)
{
  virNetworkPtr net = Net_val (netv);
  if (net) (void) virNetworkFree (net);
}
