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

(* XXX We should get rid of this type. *)
type setup =
    Libvirt.ro Libvirt.Connect.t	(* connection *)
    * bool * bool * bool * bool		(* batch, script, csv, stream mode *)
    * Libvirt.Connect.node_info		(* node_info *)
    * string				(* hostname *)
    * (int * int * int)			(* libvirt version *)

(* Sort order. *)
type sort_order =
  | DomainID | DomainName | Processor | Memory | Time
  | NetRX | NetTX | BlockRdRq | BlockWrRq

val all_sort_fields : sort_order list
val printable_sort_order : sort_order -> string
val sort_order_of_cli : string -> sort_order
val cli_of_sort_order : sort_order -> string

(* Current major display mode: TaskDisplay is the normal display. *)
type display = TaskDisplay | PCPUDisplay | BlockDisplay | NetDisplay

val display_of_cli : string -> display
val cli_of_display : display -> string

(* Helpers for manipulating block_stats & interface_stats. *)
val sum_block_stats : Libvirt.Domain.block_stats list -> Libvirt.Domain.block_stats
val diff_block_stats : Libvirt.Domain.block_stats -> Libvirt.Domain.block_stats -> Libvirt.Domain.block_stats

val sum_interface_stats : Libvirt.Domain.interface_stats list -> Libvirt.Domain.interface_stats
val diff_interface_stats : Libvirt.Domain.interface_stats -> Libvirt.Domain.interface_stats -> Libvirt.Domain.interface_stats
