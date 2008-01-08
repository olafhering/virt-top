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

   Helper functions.
*)

(** Given two lists, xs and ys, return a list of items which have been
    added to ys, items which are the same, and items which have been
    removed from ys.
    Returns a triplet (list of added, list of same, list of removed).
*)
val differences : 'a list -> 'a list -> 'a list * 'a list * 'a list

(** Convert libvirt domain state to a string. *)
val string_of_domain_state : Libvirt.Domain.state -> string

(** Filter top level rows (only) in a GtkTreeStore.  If function f returns
    true then the row remains, but if it returns false then the row is
    removed.
*)
val filter_top_level_rows : GTree.tree_store -> (Gtk.tree_iter -> bool) -> unit

(** Filter rows in a tree_store at a particular level. *)
val filter_rows : GTree.tree_store -> (Gtk.tree_iter -> bool) -> Gtk.tree_iter -> unit

(** Find the first top level row matching predicate and return it. *)
val find_top_level_row : GTree.tree_store -> (Gtk.tree_iter -> bool) -> Gtk.tree_iter

(** Find the first row matching predicate f at a particular level. *)
val find_row : GTree.tree_store -> (Gtk.tree_iter -> bool) -> Gtk.tree_iter -> Gtk.tree_iter

(** Iterate over top level rows (only) in a GtkTreeStore. *)
val iter_top_level_rows : GTree.tree_store -> (Gtk.tree_iter -> unit) -> unit

(** Iterate over rows in a tree_store at a particular level. *)
val iter_rows : GTree.tree_store -> (Gtk.tree_iter -> unit) -> Gtk.tree_iter -> unit
