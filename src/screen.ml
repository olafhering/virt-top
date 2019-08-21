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

(* The virt-top screen layout. *)

open Curses

module D = Libvirt.Domain

(* Line numbers. *)
let top_lineno = 0
let summary_lineno = 1 (* this takes 2 lines *)
let message_lineno = 3
let header_lineno = 4
let domains_lineno = 5

(* Easier to use versions of curses functions addstr, mvaddstr, etc. *)
let move y x = ignore (move y x)
let refresh () = ignore (refresh ())
let addch c = ignore (addch (int_of_char c))
let addstr s = ignore (addstr s)
let mvaddstr y x s = ignore (mvaddstr y x s)

(* Print in the "message area". *)
let clear_msg () = move message_lineno 0; clrtoeol ()
let print_msg str = clear_msg (); mvaddstr message_lineno 0 str

(* Show a libvirt domain state (the 'S' column). *)
let show_state = function
  | D.InfoNoState -> '?'
  | D.InfoRunning -> 'R'
  | D.InfoBlocked -> 'S'
  | D.InfoPaused -> 'P'
  | D.InfoShutdown -> 'D'
  | D.InfoShutoff -> 'O'
  | D.InfoCrashed -> 'X'
  | D.InfoPMSuspended -> 'M'
