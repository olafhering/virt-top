(* virt-ctrl: A graphical management tool.
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

(* Open connection dialog.
 * This should be a lot more sophisticated. XXX
 *)
let open_connection () =
  let title = "Open connection to hypervisor" in
  let uri =
    GToolbox.input_string ~title ~text:"xen:///" ~ok:"Open" "Connection:" in
  match uri with
  | None -> ()
  | Some uri -> Vc_connections.open_connection uri
