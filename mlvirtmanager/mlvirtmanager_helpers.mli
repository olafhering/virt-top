(* virt-manager-like graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
   $Id: mlvirtmanager_helpers.mli,v 1.1 2007/08/06 10:16:53 rjones Exp $

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
