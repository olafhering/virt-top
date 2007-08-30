(* virt-manager-like graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
   $Id: mlvirtmanager_domain_ops.mli,v 1.1 2007/08/06 10:16:53 rjones Exp $

   Domain operations buttons.
*)

val start_domain : GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit
val pause_domain : GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit
val resume_domain : GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit
val shutdown_domain : GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit
