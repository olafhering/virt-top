(* 'top'-like tool for libvirt domains.
   (C) Copyright 2007-2017 Richard W.M. Jones, Red Hat Inc.
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

(** The virt-top screen layout. *)

(* Line numbers. *)
val top_lineno : int
val summary_lineno : int (** this takes 2 lines *)
val message_lineno : int
val header_lineno : int
val domains_lineno : int

(* Easier to use versions of curses functions addstr, mvaddstr, etc. *)
val move : int -> int -> unit
val refresh : unit -> unit
val addch : char -> unit
val addstr : string -> unit
val mvaddstr : int -> int -> string -> unit

(* Print in the "message area". *)
val clear_msg : unit -> unit
val print_msg : string -> unit

(* Show a libvirt domain state (the 'S' column). *)
val show_state : Libvirt.Domain.state -> char
