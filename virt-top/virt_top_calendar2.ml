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

   This file contains all code which requires ocaml-calendar >= 2.0
*)

open CalendarLib

open Printf
open ExtString

open Virt_top_gettext.Gettext ;;

Virt_top.parse_date_time :=
fun time ->
  let cal : Calendar.t =
    if String.starts_with time "+" then ( (* +something *)
      let period = String.sub time 1 (String.length time - 1) in
      let period =
	if String.contains period ':' then ( (* +HH:MM:SS *)
	  let t = Printer.TimePrinter.from_string period in
	  let hh = Time.hour t and mm = Time.minute t and ss = Time.second t in
	  Calendar.Period.make 0 0 0 hh mm ss
	) else				(* +seconds *)
	  Calendar.Period.second (int_of_string period) in
      (* Add it as an offset from the current time. *)
      Calendar.add (Calendar.now ()) period
    ) else (
      if String.contains time '-' then	(* YYYY-MM-DD HH:MM:SS *)
	Printer.CalendarPrinter.from_string time
      else (				(* HH:MM:SS *)
	let time = Printer.TimePrinter.from_string time in
	Calendar.create (Date.today ()) time
      )
    ) in

  eprintf "end time: %s\n" (Printer.CalendarPrinter.to_string cal);

  (* Convert to a time_t, adjusting for the current timezone. *)
  fst (Unix.mktime (Calendar.to_unixtm cal))
