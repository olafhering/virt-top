(* 'df' command for virtual domains.

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

   Support for LVM2 PVs.
*)

open Printf
open Virt_df_gettext.Gettext
open Virt_df

let sector_size = 512
let sector_size64 = 512L

let pv_label_offset = sector_size64

let rec probe_pv dev =
  try ignore (read_pv_label dev); true
  with _ -> false

and read_pv_label dev =
  (* Load the second sector. *)
  let bits = dev#read_bitstring pv_label_offset sector_size in

  bitmatch bits with
  | labelone : 8*8 : bitstring;		(* "LABELONE" *)
    padding : 16*8 : bitstring;
    lvm2_ver : 8*8 : bitstring;		(* "LVM2 001" *)
    uuid : 32*8 : bitstring		(* UUID *)
      when Bitmatch.string_of_bitstring labelone = "LABELONE" &&
	Bitmatch.string_of_bitstring lvm2_ver = "LVM2 001" ->
    uuid
  | _ ->
    invalid_arg (sprintf "read_pv_label: %s: not an LVM2 physical volume"
		   dev#name)

let list_lvs devs = []

(* Register with main code. *)
let () =
  lvm_type_register "LVM2" probe_pv list_lvs
