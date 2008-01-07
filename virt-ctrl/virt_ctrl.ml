(* virt-manager-like graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*)

open Printf

let () =
  (* Build the main window and wire up the buttons to the callback functions *)
  Vc_mainwindow.make
    ~open_connection:Vc_connections.open_connection
    ~start_domain:Vc_domain_ops.start_domain
    ~pause_domain:Vc_domain_ops.pause_domain
    ~resume_domain:Vc_domain_ops.resume_domain
    ~shutdown_domain:Vc_domain_ops.shutdown_domain;

  (* Enter the Gtk main loop. *)
  GMain.main ()
