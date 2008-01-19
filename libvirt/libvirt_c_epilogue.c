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

static void
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
  /* Suppresses a compiler warning. */
  (void) caml__frame;
}

/* Raise an error if a function is not supported. */
static void
not_supported (const char *fn)
{
  CAMLparam0 ();
  CAMLlocal1 (fnv);

  fnv = caml_copy_string (fn);
  caml_raise_with_arg (*caml_named_value ("ocaml_libvirt_not_supported"), fnv);

  /*NOTREACHED*/
  /* Suppresses a compiler warning. */
  (void) caml__frame;
}

/* Convert the virErrorNumber, virErrorDomain and virErrorLevel enums
 * into values (longs because they are variants in OCaml).
 *
 * The enum values are part of the libvirt ABI so they cannot change,
 * which means that we can convert these numbers directly into
 * OCaml variants (which use the same ordering) very fast.
 *
 * The tricky part here is when we are linked to a newer version of
 * libvirt than the one we were compiled against.  If the newer libvirt
 * generates an error code which we don't know about then we need
 * to convert it into VIR_*_UNKNOWN (code).
 */

#define MAX_VIR_CODE VIR_ERR_NO_STORAGE_VOL
#define MAX_VIR_DOMAIN VIR_FROM_STORAGE
#define MAX_VIR_LEVEL VIR_ERR_ERROR

static inline value
Val_err_number (virErrorNumber code)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);

  if (0 <= code && code <= MAX_VIR_CODE)
    rv = Val_int (code);
  else {
    rv = caml_alloc (1, 0);	/* VIR_ERR_UNKNOWN (code) */
    Store_field (rv, 0, Val_int (code));
  }

  CAMLreturn (rv);
}

static inline value
Val_err_domain (virErrorDomain code)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);

  if (0 <= code && code <= MAX_VIR_DOMAIN)
    rv = Val_int (code);
  else {
    rv = caml_alloc (1, 0);	/* VIR_FROM_UNKNOWN (code) */
    Store_field (rv, 0, Val_int (code));
  }

  CAMLreturn (rv);
}

static inline value
Val_err_level (virErrorLevel code)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);

  if (0 <= code && code <= MAX_VIR_LEVEL)
    rv = Val_int (code);
  else {
    rv = caml_alloc (1, 0);	/* VIR_ERR_UNKNOWN_LEVEL (code) */
    Store_field (rv, 0, Val_int (code));
  }

  CAMLreturn (rv);
}

/* Convert a virterror to a value. */
static value
Val_virterror (virErrorPtr err)
{
  CAMLparam0 ();
  CAMLlocal3 (rv, connv, optv);

  rv = caml_alloc (12, 0);
  Store_field (rv, 0, Val_err_number (err->code));
  Store_field (rv, 1, Val_err_domain (err->domain));
  Store_field (rv, 2,
	       Val_opt (err->message, (Val_ptr_t) caml_copy_string));
  Store_field (rv, 3, Val_err_level (err->level));

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
#ifdef HAVE_VIRSTORAGEPOOLPTR
static void pol_finalize (value);
#endif
#ifdef HAVE_VIRSTORAGEVOLPTR
static void vol_finalize (value);
#endif

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

#ifdef HAVE_VIRSTORAGEPOOLPTR
static struct custom_operations pol_custom_operations = {
  "pol_custom_operations",
  pol_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};
#endif

#ifdef HAVE_VIRSTORAGEVOLPTR
static struct custom_operations vol_custom_operations = {
  "vol_custom_operations",
  vol_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};
#endif

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

#ifdef HAVE_VIRSTORAGEPOOLPTR
static value
Val_pol (virStoragePoolPtr pol)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc_custom (&pol_custom_operations,
			  sizeof (virStoragePoolPtr), 0, 1);
  Pol_val (rv) = pol;
  CAMLreturn (rv);
}
#endif

#ifdef HAVE_VIRSTORAGEVOLPTR
static value
Val_vol (virStorageVolPtr vol)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);
  rv = caml_alloc_custom (&vol_custom_operations,
			  sizeof (virStorageVolPtr), 0, 1);
  Vol_val (rv) = vol;
  CAMLreturn (rv);
}
#endif

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

#ifdef HAVE_VIRSTORAGEPOOLPTR
/* This wraps up the (pol, conn) pair (Pool.t). */
static value
Val_pool (virStoragePoolPtr pol, value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);

  rv = caml_alloc_tuple (2);
  v = Val_pol (pol);
  Store_field (rv, 0, v);
  Store_field (rv, 1, connv);
  CAMLreturn (rv);
}
#endif

#ifdef HAVE_VIRSTORAGEVOLPTR
/* This wraps up the (vol, conn) pair (Volume.t). */
static value
Val_volume (virStorageVolPtr vol, value connv)
{
  CAMLparam1 (connv);
  CAMLlocal2 (rv, v);

  rv = caml_alloc_tuple (2);
  v = Val_vol (vol);
  Store_field (rv, 0, v);
  Store_field (rv, 1, connv);
  CAMLreturn (rv);
}
#endif

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

#ifdef HAVE_VIRSTORAGEPOOLPTR
static void
pol_finalize (value polv)
{
  virStoragePoolPtr pol = Pol_val (polv);
  if (pol) (void) virStoragePoolFree (pol);
}
#endif

#ifdef HAVE_VIRSTORAGEVOLPTR
static void
vol_finalize (value volv)
{
  virStorageVolPtr vol = Vol_val (volv);
  if (vol) (void) virStorageVolFree (vol);
}
#endif
