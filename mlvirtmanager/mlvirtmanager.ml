(* virt-manager-like graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
   $Id: mlvirtmanager.ml,v 1.1 2007/08/06 10:16:53 rjones Exp $
*)

open Printf

let () =
  (* Build the main window and wire up the buttons to the callback functions *)
  Mlvirtmanager_mainwindow.make
    ~open_connection:Mlvirtmanager_connections.open_connection
    ~start_domain:Mlvirtmanager_domain_ops.start_domain
    ~pause_domain:Mlvirtmanager_domain_ops.pause_domain
    ~resume_domain:Mlvirtmanager_domain_ops.resume_domain
    ~shutdown_domain:Mlvirtmanager_domain_ops.shutdown_domain;

  (* Enter the Gtk main loop. *)
  GMain.main ()
