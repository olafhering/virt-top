(* 'top'-like tool for libvirt domains.
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

   This file contains all code which requires CSV support.
*)

open Virt_top_gettext.Gettext

(* Output channel, or None if CSV output not enabled. *)
let chan = ref None ;;

Virt_top.csv_start :=
  fun filename ->
    chan := Some (open_out filename) ;;

Virt_top.csv_write :=
  fun row ->
    match !chan with
    | None -> ()			(* CSV output not enabled. *)
    | Some chan ->
	Csv.save_out chan [row];
	(* Flush the output to the file immediately because we don't
	 * explicitly close this file.
	 *)
	flush chan
