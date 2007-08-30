(* virt-manager-like graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
   $Id: mlvirtmanager_mainwindow.mli,v 1.1 2007/08/06 10:16:53 rjones Exp $

   Make the main window.
*)

(** This function creates the main window.  You have to pass in
    callback functions to wire everything up.
*)
val make : open_connection:(unit -> unit) ->
  start_domain:(GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit) ->
  pause_domain:(GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit) ->
  resume_domain:(GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit) ->
  shutdown_domain:(GTree.view -> GTree.tree_store -> Mlvirtmanager_connections.columns -> unit -> unit) -> unit
