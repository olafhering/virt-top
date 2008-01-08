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

   Handle connections and the complicated GtkTreeView which
   displays the connections / domains.
*)

(** Get the list of current connections. *)
val get_conns : unit -> (int * Libvirt.rw Libvirt.Connect.t) list

(** The current/previous state last time repopulate was called.  The
    repopulate function uses this state to determine what has changed
    (eg. domains added, removed) since last time.
*)
type state

type columns = string GTree.column * string GTree.column * string GTree.column * string GTree.column * string GTree.column * int GTree.column

(** This function should be called once per second in order to
    redraw the GtkTreeView.

    Takes the previous state as a parameter and returns the new state.
*)
val repopulate : GTree.view -> GTree.tree_store -> columns -> state -> state

(** Create the GtkTreeView.  Returns the widget itself, the model,
    the list of columns, and the initial state.
*)
val make_treeview :
  ?packing:(GObj.widget -> unit) -> unit ->
    GTree.view * GTree.tree_store * columns * state

(** This callback creates the Connect to hypervisor dialog. *)
val open_connection : unit -> unit
