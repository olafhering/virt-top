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

   Make the main window.
*)

(** The connection dialog. *)
val open_connection : GWindow.window -> unit -> unit

(** Quick connect to local Xen. *)
val open_local_xen : unit -> unit

(** Quick connect to local QEMU or KVM. *)
val open_local_qemu : unit -> unit

type name = string
type uri = string
type service = name * uri

(** Hook to find libvirtd network services with zeroconf using some
    external method, eg. D-Bus or Avahi. *)
val find_libvirtd_with_zeroconf : (unit -> service list) ref

(** Hooks for icons. *)
val icon_16x16_devices_computer_png : GdkPixbuf.pixbuf option ref
val icon_24x24_devices_computer_png : GdkPixbuf.pixbuf option ref
val icon_32x32_devices_computer_png : GdkPixbuf.pixbuf option ref
val icon_48x48_devices_computer_png : GdkPixbuf.pixbuf option ref
