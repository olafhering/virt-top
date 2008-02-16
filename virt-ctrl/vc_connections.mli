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

(** Open a new connection to the hypervisor URI given. *)
val open_connection : string -> unit

(** Return the amount of historical data that we hold about a
    domain (in seconds).

    The parameters are connection ID (see {!get_conns}) and domain ID.

    This can return from [0] to [86400] (or 1 day of data).
*)
val get_hist_size : int -> int -> int

(** Return a slice of historical %CPU data about a domain.

    The required parameters are connection ID (see {!get_conns})
    and domain ID.

    The optional [latest] parameter is the latest data we should
    return.  It defaults to [0] meaning to return everything up to now.

    The optional [earliest] parameter is the earliest data we should
    return.  This is a positive number representing number of seconds
    back in time.  It defaults to returning all data.

    The optional [granularity] parameter is the granularity of data
    that we should return, in seconds.  This defaults to [1], meaning
    to return all data (once per second), but you might for example
    set this to [60] to return data for each minute.

    This returns an array of data.  The first element of the array is
    the oldest data.  The last element of the array is the most recent
    data.  The array returned might be shorter than you expect (if
    data is missing or for some other reason) so always check the
    length.

    Entries in the array are clamped to [0..100], except that if an
    entry is [-1] it means "no data".

    This returns a zero-length array if we don't know about the domain.
*)
val get_hist_cpu : ?latest:int -> ?earliest:int -> ?granularity:int ->
  int -> int ->
  int array

(** Return a slice of historical memory data about a domain.

    Parameters as above.

    Entries in the array are 64 bit integers corresponding to the
    amount of memory in KB allocated to the domain (not necessarily
    the amount being used, which we don't know about).
*)
val get_hist_mem : ?latest:int -> ?earliest:int -> ?granularity:int ->
  int -> int ->
  int64 array
