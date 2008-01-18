#!/usr/bin/perl -w
#
# OCaml bindings for libvirt.
# (C) Copyright 2007-2008 Richard W.M. Jones, Red Hat Inc.
# http://libvirt.org/
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

# This generates libvirt_c.c (the core of the bindings).  You don't
# need to run this program unless you are extending the bindings
# themselves (eg. because libvirt has been extended).

use strict;

#----------------------------------------------------------------------

# The functions in the libvirt API that we can generate.

my @functions = (
    { name => "virConnectGetHostname", sig => "conn : string", weak => 1 },
    { name => "virConnectGetURI", sig => "conn : string", weak => 1 },
    { name => "virConnectGetType", sig => "conn : static string" },
    { name => "virConnectNumOfDomains", sig => "conn : int" },
    { name => "virConnectListDomains", sig => "conn, int : int array" },
    { name => "virConnectNumOfDefinedDomains", sig => "conn : int" },
    { name => "virConnectListDefinedDomains",
      sig => "conn, int : string array" },
    { name => "virConnectNumOfNetworks", sig => "conn : int" },
    { name => "virConnectListNetworks", sig => "conn, int : string array" },
    { name => "virConnectNumOfDefinedNetworks", sig => "conn : int" },
    { name => "virConnectListDefinedNetworks",
      sig => "conn, int : string array" },
    { name => "virConnectNumOfStoragePools", sig => "conn : int", weak => 1 },
    { name => "virConnectListStoragePools",
      sig => "conn, int : string array", weak => 1 },
    { name => "virConnectNumOfDefinedStoragePools",
      sig => "conn : int", weak => 1 },
    { name => "virConnectListDefinedStoragePools",
      sig => "conn, int : string array", weak => 1 },
    { name => "virConnectGetCapabilities", sig => "conn : string" },

    { name => "virDomainGetName", sig => "dom : static string" },
    { name => "virDomainGetOSType", sig => "dom : string" },
    { name => "virDomainGetXMLDesc", sig => "dom, 0 : string" },
    { name => "virDomainSuspend", sig => "dom : unit" },
    { name => "virDomainResume", sig => "dom : unit" },
    { name => "virDomainShutdown", sig => "dom : unit" },
    { name => "virDomainReboot", sig => "dom, 0 : unit" },
    { name => "virDomainUndefine", sig => "dom : unit" },
    { name => "virDomainCreate", sig => "dom : unit" },

    { name => "virNetworkGetName", sig => "net : static string" },
    { name => "virNetworkGetXMLDesc", sig => "net, 0 : string" },
    { name => "virNetworkGetBridgeName", sig => "net : string" },
    { name => "virNetworkUndefine", sig => "net : unit" },
    { name => "virNetworkCreate", sig => "net : unit" },

    { name => "virStoragePoolGetName",
      sig => "pool : static string", weak => 1 },
    { name => "virStoragePoolGetXMLDesc",
      sig => "pool, 0 : string", weak => 1 },
    { name => "virStoragePoolUndefine",
      sig => "pool : string", weak => 1 },
    { name => "virStoragePoolCreate",
      sig => "pool : string", weak => 1 },
    { name => "virStoragePoolShutdown",
      sig => "pool : string", weak => 1 },
    { name => "virStoragePoolRefresh",
      sig => "pool, 0 : string", weak => 1 },

    { name => "virStorageVolGetXMLDesc",
      sig => "pool, 0 : string", weak => 1 },
    { name => "virStorageVolGetPath",
      sig => "pool : string", weak => 1 },
    { name => "virStorageVolGetKey",
      sig => "pool : static string", weak => 1 },
    { name => "virStorageVolGetName",
      sig => "pool : static string", weak => 1 },

    );

# Functions we haven't implemented anywhere yet.
# We create stubs for these, but they need to either be moved ^^ so they
# are auto-generated or implementations written in libvirt_c_oneoffs.c.

my @unimplemented = (
    "ocaml_libvirt_domain_create_job",
    "ocaml_libvirt_domain_core_dump_job",
    "ocaml_libvirt_domain_restore_job",
    "ocaml_libvirt_domain_save_job",
    "ocaml_libvirt_connect_create_linux_job",
    "ocaml_libvirt_network_create_job",
    "ocaml_libvirt_network_create_xml_job",
    "ocaml_libvirt_storage_pool_set_autostart",
    "ocaml_libvirt_storage_pool_get_autostart",
    "ocaml_libvirt_storage_pool_get_info",
    "ocaml_libvirt_storage_pool_get_uuid_string",
    "ocaml_libvirt_storage_pool_get_uuid",
    "ocaml_libvirt_storage_pool_free",
    "ocaml_libvirt_storage_pool_destroy",
    "ocaml_libvirt_storage_pool_define_xml",
    "ocaml_libvirt_storage_pool_create_xml",
    "ocaml_libvirt_storage_pool_lookup_by_uuid_string",
    "ocaml_libvirt_storage_pool_lookup_by_uuid",
    "ocaml_libvirt_storage_pool_lookup_by_name",
    "ocaml_libvirt_storage_vol_free",
    "ocaml_libvirt_storage_vol_destroy",
    "ocaml_libvirt_storage_vol_create_xml",
    "ocaml_libvirt_storage_vol_get_info",
    "ocaml_libvirt_pool_of_volume",
    "ocaml_libvirt_storage_vol_lookup_by_path",
    "ocaml_libvirt_storage_vol_lookup_by_key",
    "ocaml_libvirt_storage_vol_lookup_by_name",
    "ocaml_libvirt_job_cancel",
    "ocaml_libvirt_job_get_network",
    "ocaml_libvirt_job_get_domain",
    "ocaml_libvirt_job_get_info",
    );

#----------------------------------------------------------------------

# Open the output file.

my $filename = "libvirt_c.c";
open F, ">$filename" or die "$filename: $!";

# Write the prologue.

print F <<'END';
/* WARNING: THIS FILE IS AUTOMATICALLY GENERATED BY 'generator.pl'.
 * Any changes you make to this file may be overwritten.
 */

/* OCaml bindings for libvirt.
 * (C) Copyright 2007-2008 Richard W.M. Jones, Red Hat Inc.
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
#include <caml/signals.h>

#include "libvirt_c_prologue.c"

#include "libvirt_c_oneoffs.c"

END

#----------------------------------------------------------------------

sub camel_case_to_underscores
{
    my $name = shift;

    $name =~ s/([A-Z][a-z]+|XML|URI|OS)/$1,/g;
    my @subs = split (/,/, $name);
    @subs = map { lc($_) } @subs;
    join "_", @subs
}

# Helper functions dealing with signatures.

sub short_name_to_c_type
{
    local $_ = shift;

    if ($_ eq "conn") { "virConnectPtr" }
    elsif ($_ eq "dom") { "virDomainPtr" }
    elsif ($_ eq "net") { "virNetworkPtr" }
    elsif ($_ eq "pool") { "virStoragePoolPtr" }
    elsif ($_ eq "vol") { "virStorageVolPtr" }
    else {
	die "unknown short name $_"
    }
}

sub gen_c_signature
{
    my $sig = shift;
    my $c_name = shift;

    if ($sig =~ /^(\w+) : string$/) {
	my $c_type = short_name_to_c_type ($1);
	"char *$c_name ($c_type $1)"
    } elsif ($sig =~ /^(\w+) : static string$/) {
	my $c_type = short_name_to_c_type ($1);
	"const char *$c_name ($c_type $1)"
    } elsif ($sig =~ /^(\w+) : int$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1)"
    } elsif ($sig eq "conn, int : int array") {
	"int $c_name (virConnectPtr conn, int *ids, int maxids)"
    } elsif ($sig eq "conn, int : string array") {
	"int $c_name (virConnectPtr conn, char **const names, int maxnames)"
    } elsif ($sig =~ /^(\w+), 0 : string$/) {
	my $c_type = short_name_to_c_type ($1);
	"char *$c_name ($c_type $1, int flags)"
    } elsif ($sig =~ /^(\w+), 0 : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1, int flags)"
    } elsif ($sig =~ /^(\w+) : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1 dom)"
    } else {
	die "unknown signature $sig"
    }
}

sub gen_arg_names
{
    my $sig = shift;

    if ($sig =~ /^(\w+) : string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : static string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : int$/) {
	( "$1v" )
    } elsif ($sig eq "conn, int : int array") {
	( "connv", "iv" )
    } elsif ($sig eq "conn, int : string array") {
	( "connv", "iv" )
    } elsif ($sig =~ /^(\w+), 0 : string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+), 0 : unit$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : unit$/) {
	( "$1v" )
    } else {
	die "unknown signature $sig"
    }
}

sub gen_unpack_args
{
    local $_ = shift;

    if ($_ eq "conn") {
	"virConnectPtr conn = Connect_val (connv);"
    } elsif ($_ eq "dom") {
	"virDomainPtr dom = Domain_val (domv);\n".
	"  virConnectPtr conn = Connect_domv (domv);"
    } elsif ($_ eq "net") {
	"virNetworkPtr net = Network_val (netv);\n".
	"  virConnectPtr conn = Connect_netv (netv);"
    } elsif ($_ eq "pool") {
	"virStoragePoolPtr pool = Pool_val (poolv);\n".
	"  virConnectPtr conn = Connect_polv (poolv);"
    } elsif ($_ eq "vol") {
	"virStorageVolPtr vol = Volume_val (volv);\n".
	"  virConnectPtr conn = Connect_volv (volv);"
    } else {
	die "unknown short name $_"
    }
}

sub gen_c_code
{
    my $sig = shift;
    my $c_name = shift;

    if ($sig =~ /^(\w+) : string$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char *r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (!r, conn, \"$c_name\");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : static string$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  const char *r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (!r, conn, \"$c_name\");

  rv = caml_copy_string (r);
  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : int$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_int (r));
"
    } elsif ($sig eq "conn, int : int array") {
	"\
  CAMLlocal1 (rv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  int ids[i], r;

  NONBLOCKING (r = $c_name (conn, ids, i));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i)
    Store_field (rv, i, Val_int (ids[i]));

  CAMLreturn (rv);
"
    } elsif ($sig eq "conn, int : string array") {
	"\
  CAMLlocal2 (rv, strv);
  virConnectPtr conn = Connect_val (connv);
  int i = Int_val (iv);
  char *names[i];
  int r;

  NONBLOCKING (r = $c_name (conn, names, i));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  rv = caml_alloc (r, 0);
  for (i = 0; i < r; ++i) {
    strv = caml_copy_string (names[i]);
    Store_field (rv, i, strv);
    free (names[i]);
  }

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), 0 : string$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char *r;

  NONBLOCKING (r = $c_name ($1, 0));
  CHECK_ERROR (!r, conn, \"$c_name\");

  rv = caml_copy_string (r);
  free (r);
  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : unit$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_unit);
"
    } elsif ($sig =~ /^(\w+), 0 : unit$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r;

  NONBLOCKING (r = $c_name ($1, 0));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_unit);
"
    } else {
	die "unknown signature $sig"
    }
}

# Generate each function.

foreach my $function (@functions) {
    my $c_name = $function->{name};
    my $is_weak = $function->{weak};
    my $sig = $function->{sig};

    my $is_pool_func = $c_name =~ /^virStoragePool/;
    my $is_vol_func = $c_name =~ /^virStorageVol/;

    # Generate an equivalent C-external name for the function, unless
    # one is defined already.
    my $c_external_name;
    if (exists ($function->{c_external_name})) {
	$c_external_name = $function->{c_external_name};
    } elsif ($c_name =~ /^vir/) {
	$c_external_name = substr $c_name, 3;
	$c_external_name = camel_case_to_underscores ($c_external_name);
	$c_external_name = "ocaml_libvirt_" . $c_external_name;
    } else {
	die "cannot convert c_name $c_name to c_external_name"
    }

    # Generate a full function prototype if the function is weak.
    my $have_name = "HAVE_" . uc ($c_name);
    if ($is_weak) {
	my $c_sig = gen_c_signature ($sig, $c_name);
	print F <<END;
#ifdef HAVE_WEAK_SYMBOLS
#ifdef $have_name
extern $c_sig __attribute__((weak));
#endif
#endif

END
    }

    my @arg_names = gen_arg_names ($sig);
    my $nr_arg_names = scalar @arg_names;
    my $arg_names = join ", ", @arg_names;
    my $arg_names_as_values = join (", ", map { "value $_" } @arg_names);

    # Generate the start of the function, arguments.
    print F <<END;
CAMLprim value
$c_external_name ($arg_names_as_values)
{
  CAMLparam$nr_arg_names ($arg_names);
END

    # If weak, check the function exists at compile time or runtime.
    if ($is_weak) {
	print F <<END;
#ifndef $have_name
  /* Symbol $c_name not found at compile time. */
  not_supported ("$c_name");
  /* Suppresses a compiler warning. */
  (void) caml__frame;
#else
  /* Check that the symbol $c_name
   * is in runtime version of libvirt.
   */
  WEAK_SYMBOL_CHECK ($c_name);
END
    }

    # Generate the internals of the function.
    print F (gen_c_code ($sig, $c_name));

    # Finish off weak #ifdef.
    if ($is_weak) {
	print F <<END;
#endif
END
    }

    # Finish off the function.
    print F <<END;
}

END
}

#----------------------------------------------------------------------

# Unimplemented functions.

printf "$0: warning: %d unimplemented functions\n", scalar (@unimplemented);

foreach my $c_external_name (@unimplemented) {
    print F <<END
CAMLprim value
$c_external_name ()
{
  failwith ("$c_external_name is unimplemented");
}

END
}

#----------------------------------------------------------------------

# Write the epilogue.

print F <<'END';
#include "libvirt_c_epilogue.c"

/* EOF */
END

close F;
print "$0: written $filename\n"

