(* 'top'-like tool for libvirt domains.
   (C) Copyright 2007-2009 Richard W.M. Jones, Red Hat Inc.
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

   Just contains the main function.
*)

open Curses

open Virt_top_gettext.Gettext
open Virt_top

(* Note: make sure we catch any exceptions and clean up the display.
 *
 * Note (2): make sure all exit paths call the GC so that we can check
 * that all allocated resources are being counted properly (by running
 * the program under --debug ...).
 *)
let error =
  let ((_, _, script_mode, _, _, _, _) as setup) = start_up () in

  try
    main_loop setup;
    if not script_mode then endwin ();
    false
  with
  | Libvirt.Virterror err ->
      if not script_mode then endwin ();
      prerr_endline (Libvirt.Virterror.to_string err);
      true
  | exn ->
      if not script_mode then endwin ();
      prerr_endline (s_ "Error" ^ ": " ^ Printexc.to_string exn);
      true

let () =
  Gc.compact (); (* See note above. *)

  exit (if error then 1 else 0)
