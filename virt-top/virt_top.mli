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
*)

(* Hook for virt_top_xml to override (if present). *)
val parse_device_xml :
  (int -> [ `R ] Libvirt.Domain.t -> string list * string list) ref

(* Hooks for virt_top_csv to override (if present). *)
val csv_start : (string -> unit) ref
val csv_write : (string list -> unit) ref

(* Hook for virt_top_calendar to override (if present). *)
val parse_date_time : (string -> float) ref

type setup =
    Libvirt.ro Libvirt.Connect.t	(* connection *)
    * bool * bool * bool		(* batch, script, csv modes *)
    * Libvirt.Connect.node_info		(* node_info *)
    * string				(* hostname *)
    * (int * int * int)			(* libvirt version *)

val start_up : unit -> setup
val main_loop : setup -> unit
