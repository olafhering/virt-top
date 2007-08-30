(** OCaml bindings for libvirt.
    (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
    http://libvirt.org/
    $Id: libvirt_version.mli,v 1.2 2007/08/21 14:36:15 rjones Exp $
*)

val package : string
val version : string
(** The name and version of the OCaml libvirt bindings.

    (To get the version of libvirt C library itself
     use {!Libvirt.get_version}). *)
