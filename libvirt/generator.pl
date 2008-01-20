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
#
# Please read libvirt/README.

use strict;

#----------------------------------------------------------------------

# The functions in the libvirt API that we can generate.

# The 'sig' (signature) doesn't have a meaning or any internal structure.
# It is interpreted by the generation functions below to indicate what
# "class" the function falls into, and to generate the right class of
# binding.
#
# Any function added since libvirt 0.2.1 must be marked weak.

my @functions = (
    { name => "virConnectClose", sig => "conn : free" },
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

    { name => "virDomainCreateLinux", sig => "conn, string, 0U : dom" },
    { name => "virDomainCreateLinuxJob",
      sig => "conn, string, 0U : job", weak => 1 },
    { name => "virDomainFree", sig => "dom : free" },
    { name => "virDomainDestroy", sig => "dom : free" },
    { name => "virDomainLookupByName", sig => "conn, string : dom" },
    { name => "virDomainLookupByID", sig => "conn, int : dom" },
    { name => "virDomainLookupByUUID", sig => "conn, uuid : dom" },
    { name => "virDomainLookupByUUIDString", sig => "conn, string : dom" },
    { name => "virDomainGetName", sig => "dom : static string" },
    { name => "virDomainGetOSType", sig => "dom : string" },
    { name => "virDomainGetXMLDesc", sig => "dom, 0 : string" },
    { name => "virDomainGetUUID", sig => "dom : uuid" },
    { name => "virDomainGetUUIDString", sig => "dom : uuid string" },
    { name => "virDomainGetMaxVcpus", sig => "dom : int" },
    { name => "virDomainSave", sig => "dom, string : unit" },
    { name => "virDomainSaveJob",
      sig => "dom, string : job from dom", weak => 1 },
    { name => "virDomainRestore", sig => "conn, string : unit" },
    { name => "virDomainRestoreJob",
      sig => "conn, string : job", weak => 1 },
    { name => "virDomainCoreDump", sig => "dom, string, 0 : unit" },
    { name => "virDomainCoreDumpJob",
      sig => "dom, string, 0 : job from dom", weak => 1 },
    { name => "virDomainSuspend", sig => "dom : unit" },
    { name => "virDomainResume", sig => "dom : unit" },
    { name => "virDomainShutdown", sig => "dom : unit" },
    { name => "virDomainReboot", sig => "dom, 0 : unit" },
    { name => "virDomainDefineXML", sig => "conn, string : dom" },
    { name => "virDomainUndefine", sig => "dom : unit" },
    { name => "virDomainCreate", sig => "dom : unit" },
    { name => "virDomainCreateJob",
      sig => "dom, 0U : job from dom", weak => 1 },
    { name => "virDomainAttachDevice", sig => "dom, string : unit" },
    { name => "virDomainDetachDevice", sig => "dom, string : unit" },
    { name => "virDomainGetAutostart", sig => "dom : bool" },
    { name => "virDomainSetAutostart", sig => "dom, bool : unit" },

    { name => "virNetworkFree", sig => "net : free" },
    { name => "virNetworkDestroy", sig => "net : free" },
    { name => "virNetworkLookupByName", sig => "conn, string : net" },
    { name => "virNetworkLookupByUUID", sig => "conn, uuid : net" },
    { name => "virNetworkLookupByUUIDString", sig => "conn, string : net" },
    { name => "virNetworkGetName", sig => "net : static string" },
    { name => "virNetworkGetXMLDesc", sig => "net, 0 : string" },
    { name => "virNetworkGetBridgeName", sig => "net : string" },
    { name => "virNetworkGetUUID", sig => "net : uuid" },
    { name => "virNetworkGetUUIDString", sig => "net : uuid string" },
    { name => "virNetworkUndefine", sig => "net : unit" },
    { name => "virNetworkCreateXML", sig => "conn, string : net" },
    { name => "virNetworkCreateXMLJob",
      sig => "conn, string : job", weak => 1 },
    { name => "virNetworkDefineXML", sig => "conn, string : net" },
    { name => "virNetworkCreate", sig => "net : unit" },
    { name => "virNetworkCreateJob",
      sig => "net : job from net", weak => 1 },
    { name => "virNetworkGetAutostart", sig => "net : bool" },
    { name => "virNetworkSetAutostart", sig => "net, bool : unit" },

    { name => "virStoragePoolFree", sig => "pool : free", weak => 1 },
    { name => "virStoragePoolDestroy", sig => "pool : free", weak => 1 },
    { name => "virStoragePoolLookupByName",
      sig => "conn, string : pool", weak => 1 },
    { name => "virStoragePoolLookupByUUID",
      sig => "conn, uuid : pool", weak => 1 },
    { name => "virStoragePoolLookupByUUIDString",
      sig => "conn, string : pool", weak => 1 },
    { name => "virStoragePoolGetName",
      sig => "pool : static string", weak => 1 },
    { name => "virStoragePoolGetXMLDesc",
      sig => "pool, 0 : string", weak => 1 },
    { name => "virStoragePoolGetUUID",
      sig => "pool : uuid", weak => 1 },
    { name => "virStoragePoolGetUUIDString",
      sig => "pool : uuid string", weak => 1 },
    { name => "virStoragePoolCreateXML",
      sig => "conn, string : pool", weak => 1 },
    { name => "virStoragePoolDefineXML",
      sig => "conn, string : pool", weak => 1 },
    { name => "virStoragePoolUndefine",
      sig => "pool : unit", weak => 1 },
    { name => "virStoragePoolCreate",
      sig => "pool : unit", weak => 1 },
    { name => "virStoragePoolShutdown",
      sig => "pool : unit", weak => 1 },
    { name => "virStoragePoolRefresh",
      sig => "pool, 0U : unit", weak => 1 },
    { name => "virStoragePoolGetAutostart",
      sig => "pool : bool", weak => 1 },
    { name => "virStoragePoolSetAutostart",
      sig => "pool, bool : unit", weak => 1 },

    { name => "virStorageVolFree", sig => "vol : free", weak => 1 },
    { name => "virStorageVolDestroy", sig => "vol : free", weak => 1 },
#    { name => "virStorageVolLookupByName", XXX see libvir-list posting
#      sig => "pool, string : vol", weak => 1 },
    { name => "virStorageVolLookupByKey",
      sig => "conn, string : vol", weak => 1 },
    { name => "virStorageVolLookupByPath",
      sig => "conn, string : vol", weak => 1 },
#    { name => "virStorageVolCreateXML",
#      sig => "pool, string : vol", weak => 1 }, XXX
    { name => "virStorageVolGetXMLDesc",
      sig => "vol, 0 : string", weak => 1 },
    { name => "virStorageVolGetPath",
      sig => "vol : string", weak => 1 },
    { name => "virStorageVolGetKey",
      sig => "vol : static string", weak => 1 },
    { name => "virStorageVolGetName",
      sig => "vol : static string", weak => 1 },
    { name => "virStoragePoolLookupByVolume",
      sig => "vol : pool from vol", weak => 1 },

    { name => "virJobFree",
      sig => "job : free", weak => 1 },
    { name => "virJobCancel",
      sig => "job : unit", weak => 1 },
    { name => "virJobGetNetwork",
      sig => "job : net from job", weak => 1 },
    { name => "virJobGetDomain",
      sig => "job : dom from job", weak => 1 },

    );

# Functions we haven't implemented anywhere yet but which are mentioned
# in 'libvirt.ml'.
#
# We create stubs for these, but eventually they need to either be
# moved ^^^ so they are auto-generated, or implementations of them
# written in 'libvirt_c_oneoffs.c'.

my @unimplemented = (
    "ocaml_libvirt_storage_vol_lookup_by_name", # XXX see above
    "ocaml_libvirt_storage_vol_create_xml",     # XXX see above
    );

#----------------------------------------------------------------------

# Open the output file.

my $filename = "libvirt_c.c";
open F, ">$filename" or die "$filename: $!";

# Write the prologue.

print F <<'END';
/* !!! WARNING WARNING WARNING WARNING WARNING WARNING WARNING !!!
 *
 * THIS FILE IS AUTOMATICALLY GENERATED BY 'generator.pl'.
 *
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

    $name =~ s/([A-Z][a-z]+|XML|URI|OS|UUID)/$1,/g;
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
    elsif ($_ eq "job") { "virJobPtr" }
    else {
	die "unknown short name $_"
    }
}

# Generate a C signature for the original function.  Used when building
# weak bindings.

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
    } elsif ($sig =~ /^(\w+) : uuid$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1, unsigned char *)"
    } elsif ($sig =~ /^(\w+) : uuid string$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1, char *)"
    } elsif ($sig =~ /^(\w+) : bool$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1, int *r)"
    } elsif ($sig =~ /^(\w+), bool : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1, int b)"
    } elsif ($sig eq "conn, int : int array") {
	"int $c_name (virConnectPtr conn, int *ids, int maxids)"
    } elsif ($sig eq "conn, int : string array") {
	"int $c_name (virConnectPtr conn, char **const names, int maxnames)"
    } elsif ($sig =~ /^(\w+), 0(U?) : string$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	"char *$c_name ($c_type $1, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+), 0(U?) : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	"int $c_name ($c_type $1, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+) : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1)"
    } elsif ($sig =~ /^(\w+) : free$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1)"
    } elsif ($sig =~ /^(\w+), string : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	"int $c_name ($c_type $1, const char *str)"
    } elsif ($sig =~ /^(\w+), string, 0(U?) : unit$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	"int $c_name ($c_type $1, const char *str, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+), string : (\w+)$/) {
	my $c_type = short_name_to_c_type ($1);
	my $c_ret_type = short_name_to_c_type ($2);
	"$c_ret_type $c_name ($c_type $1, const char *str)"
    } elsif ($sig =~ /^(\w+), string, 0(U?) : (\w+)$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	my $c_ret_type = short_name_to_c_type ($3);
	"$c_ret_type $c_name ($c_type $1, const char *str, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+), int : (\w+)$/) {
	my $c_type = short_name_to_c_type ($1);
	my $c_ret_type = short_name_to_c_type ($2);
	"$c_ret_type $c_name ($c_type $1, int i)"
    } elsif ($sig =~ /^(\w+), uuid : (\w+)$/) {
	my $c_type = short_name_to_c_type ($1);
	my $c_ret_type = short_name_to_c_type ($2);
	"$c_ret_type $c_name ($c_type $1, const unsigned char *str)"
    } elsif ($sig =~ /^(\w+), 0(U?) : (\w+)$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	my $c_ret_type = short_name_to_c_type ($3);
	"$c_ret_type $c_name ($c_type $1, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+) : (\w+)$/) {
	my $c_type = short_name_to_c_type ($1);
	my $c_ret_type = short_name_to_c_type ($2);
	"$c_ret_type $c_name ($c_type $1)"
    } elsif ($sig =~ /^(\w+), string : (\w+) from \w+$/) {
	my $c_type = short_name_to_c_type ($1);
	my $c_ret_type = short_name_to_c_type ($2);
	"$c_ret_type $c_name ($c_type $1, const char *str)"
    } elsif ($sig =~ /^(\w+), string, 0(U?) : (\w+) from \w+$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	my $c_ret_type = short_name_to_c_type ($3);
	"$c_ret_type $c_name ($c_type $1, const char *str, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+), 0(U?) : (\w+) from \w+$/) {
	my $c_type = short_name_to_c_type ($1);
	my $unsigned = $2 eq "U" ? "unsigned " : "";
	my $c_ret_type = short_name_to_c_type ($3);
	"$c_ret_type $c_name ($c_type $1, $unsigned int flags)"
    } elsif ($sig =~ /^(\w+) : (\w+) from \w+$/) {
	my $c_type = short_name_to_c_type ($1);
	my $c_ret_type = short_name_to_c_type ($2);
	"$c_ret_type $c_name ($c_type $1)"
    } else {
	die "unknown signature $sig"
    }
}

# OCaml argument names.

sub gen_arg_names
{
    my $sig = shift;

    if ($sig =~ /^(\w+) : string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : static string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : int$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : uuid$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : uuid string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : bool$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+), bool : unit$/) {
	( "$1v", "bv" )
    } elsif ($sig eq "conn, int : int array") {
	( "connv", "iv" )
    } elsif ($sig eq "conn, int : string array") {
	( "connv", "iv" )
    } elsif ($sig =~ /^(\w+), 0U? : string$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+), 0U? : unit$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : unit$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : free$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+), string : unit$/) {
	( "$1v", "strv" )
    } elsif ($sig =~ /^(\w+), string, 0U? : unit$/) {
	( "$1v", "strv" )
    } elsif ($sig =~ /^(\w+), string : (\w+)$/) {
	( "$1v", "strv" )
    } elsif ($sig =~ /^(\w+), string, 0U? : (\w+)$/) {
	( "$1v", "strv" )
    } elsif ($sig =~ /^(\w+), int : (\w+)$/) {
	( "$1v", "iv" )
    } elsif ($sig =~ /^(\w+), uuid : (\w+)$/) {
	( "$1v", "uuidv" )
    } elsif ($sig =~ /^(\w+), 0U? : (\w+)$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : (\w+)$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+), string : (\w+) from \w+$/) {
	( "$1v", "strv" )
    } elsif ($sig =~ /^(\w+), string, 0U? : (\w+) from \w+$/) {
	( "$1v", "strv" )
    } elsif ($sig =~ /^(\w+), 0U? : (\w+) from \w+$/) {
	( "$1v" )
    } elsif ($sig =~ /^(\w+) : (\w+) from \w+$/) {
	( "$1v" )
    } else {
	die "unknown signature $sig"
    }
}

# Unpack the first (object) argument.

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
    } elsif ($_ eq "job") {
	"virJobPtr job = Job_val (jobv);\n".
	"  virConnectPtr conn = Connect_jobv (jobv);"
    } else {
	die "unknown short name $_"
    }
}

# Pack the result if it's an object.

sub gen_pack_result
{
    local $_ = shift;

    if ($_ eq "dom") {     "rv = Val_domain (r, connv);" }
    elsif ($_ eq "net") {  "rv = Val_network (r, connv);" }
    elsif ($_ eq "pool") { "rv = Val_pool (r, connv);" }
    elsif ($_ eq "vol") {  "rv = Val_volume (r, connv);" }
    elsif ($_ eq "job") {  "rv = Val_job (r, connv);" }
    else {
	die "unknown short name $_"
    }
}

sub gen_free_arg
{
    local $_ = shift;

    if ($_ eq "conn") {     "Connect_val (connv) = NULL;" }
    elsif ($_ eq "dom") {   "Domain_val (domv) = NULL;" }
    elsif ($_ eq "net") {   "Network_val (netv) = NULL;" }
    elsif ($_ eq "pool") {  "Pool_val (poolv) = NULL;" }
    elsif ($_ eq "vol") {   "Volume_val (volv) = NULL;" }
    elsif ($_ eq "job") {   "Job_val (jobv) = NULL;" }
    else {
	die "unknown short name $_"
    }
}

# Generate the C body for each signature (class of function).

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
    } elsif ($sig =~ /^(\w+) : uuid$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  unsigned char uuid[VIR_UUID_BUFLEN];
  int r;

  NONBLOCKING (r = $c_name ($1, uuid));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  /* UUIDs are byte arrays with a fixed length. */
  rv = caml_alloc_string (VIR_UUID_BUFLEN);
  memcpy (String_val (rv), uuid, VIR_UUID_BUFLEN);
  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : uuid string$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char uuid[VIR_UUID_STRING_BUFLEN];
  int r;

  NONBLOCKING (r = $c_name ($1, uuid));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  rv = caml_copy_string (uuid);
  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : bool$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r, b;

  NONBLOCKING (r = $c_name ($1, &b));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (b ? Val_true : Val_false);
"
    } elsif ($sig =~ /^(\w+), bool : unit$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r, b;

  b = bv == Val_true ? 1 : 0;

  NONBLOCKING (r = $c_name ($1, b));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_unit);
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
    } elsif ($sig =~ /^(\w+), 0U? : string$/) {
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
    } elsif ($sig =~ /^(\w+), 0U? : unit$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r;

  NONBLOCKING (r = $c_name ($1, 0));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_unit);
"
    } elsif ($sig =~ /^(\w+) : unit$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_unit);
"
    } elsif ($sig =~ /^(\w+) : free$/) {
	"\
  " . gen_unpack_args ($1) . "
  int r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  /* So that we don't double-free in the finalizer: */
  " . gen_free_arg ($1) . "

  CAMLreturn (Val_unit);
"
    } elsif ($sig =~ /^(\w+), string : unit$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char *str = String_val (strv);
  int r;

  NONBLOCKING (r = $c_name ($1, str));
  CHECK_ERROR (r == -1, conn, \"$c_name\");

  CAMLreturn (Val_unit);
"
    } elsif ($sig =~ /^(\w+), string, 0U? : unit$/) {
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char *str = String_val (strv);
  int r;

  NONBLOCKING (r = $c_name ($1, str, 0));
  CHECK_ERROR (!r, conn, \"$c_name\");

  CAMLreturn (Val_unit);
"
    } elsif ($sig =~ /^(\w+), string : (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char *str = String_val (strv);
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, str));
  CHECK_ERROR (!r, conn, \"$c_name\");

  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), string, 0U? : (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  char *str = String_val (strv);
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, str, 0));
  CHECK_ERROR (!r, conn, \"$c_name\");

  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), int : (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  int i = Int_val (iv);
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, i));
  CHECK_ERROR (!r, conn, \"$c_name\");

  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), uuid : (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  unsigned char *uuid = (unsigned char *) String_val (uuidv);
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, uuid));
  CHECK_ERROR (!r, conn, \"$c_name\");

  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), 0U? : (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, 0));
  CHECK_ERROR (!r, conn, \"$c_name\");

  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal1 (rv);
  " . gen_unpack_args ($1) . "
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (!r, conn, \"$c_name\");

  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), string : (\w+) from (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal2 (rv, connv);
  " . gen_unpack_args ($1) . "
  char *str = String_val (strv);
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, str));
  CHECK_ERROR (!r, conn, \"$c_name\");

  connv = Field ($3v, 1);
  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), string, 0U? : (\w+) from (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal2 (rv, connv);
  " . gen_unpack_args ($1) . "
  char *str = String_val (strv);
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, str, 0));
  CHECK_ERROR (!r, conn, \"$c_name\");

  connv = Field ($3v, 1);
  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+), 0U? : (\w+) from (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal2 (rv, connv);
  " . gen_unpack_args ($1) . "
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1, 0));
  CHECK_ERROR (!r, conn, \"$c_name\");

  connv = Field ($3v, 1);
  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
"
    } elsif ($sig =~ /^(\w+) : (\w+) from (\w+)$/) {
	my $c_ret_type = short_name_to_c_type ($2);
	"\
  CAMLlocal2 (rv, connv);
  " . gen_unpack_args ($1) . "
  $c_ret_type r;

  NONBLOCKING (r = $c_name ($1));
  CHECK_ERROR (!r, conn, \"$c_name\");

  connv = Field ($3v, 1);
  " . gen_pack_result ($2) . "

  CAMLreturn (rv);
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

    print F <<END;
/* Automatically generated binding for $c_name.
 * In generator.pl this function has signature "$sig".
 */

END

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

if (@unimplemented) {
    print F <<'END';
/* The following functions are unimplemented and always fail.
 * See generator.pl '@unimplemented'
 */

END

    foreach my $c_external_name (@unimplemented) {
	print F <<END;
CAMLprim value
$c_external_name ()
{
  failwith ("$c_external_name is unimplemented");
}

END
    } # end foreach
} # end if @unimplemented

#----------------------------------------------------------------------

# Write the epilogue.

print F <<'END';
#include "libvirt_c_epilogue.c"

/* EOF */
END

close F;
print "$0: written $filename\n"

