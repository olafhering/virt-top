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

let plugin_name = "LVM2"

let sector_size = 512
let sector_size64 = 512L

let pv_label_offset = sector_size64

(* Probe to see if it's an LVM2 PV.  Look for the "LABELONE" label. *)
let rec probe_pv lvm_plugin_id dev =
  try
    let uuid = read_pv_label dev in
    { lvm_plugin_id = lvm_plugin_id; pv_uuid = uuid }
  with _ -> raise Not_found

and read_pv_label dev =
  (* Load the second sector. *)
  let bits = dev#read_bitstring pv_label_offset sector_size in

  (*Bitmatch.hexdump_bitstring stdout bits;*)

  bitmatch bits with
  | labelone : 8*8 : bitstring;		(* "LABELONE" *)
    padding : 16*8 : bitstring;
    lvm2_ver : 8*8 : bitstring;		(* "LVM2 001" *)
    uuid : 32*8 : bitstring		(* UUID *)
      when Bitmatch.string_of_bitstring labelone = "LABELONE" &&
	Bitmatch.string_of_bitstring lvm2_ver = "LVM2 001" ->
    Bitmatch.string_of_bitstring uuid
  | _ ->
    invalid_arg (sprintf "read_pv_label: %s: not an LVM2 physical volume"
		   dev#name)

(* We are passed a list of devices which we previously identified
 * as PVs belonging to us.  From these produce a list of all LVs
 * (as devices) and return them.  Note that we don't try to detect
 * what is on these LVs - that will be done in the main code.
 *)
let list_lvs devs = []

(* Register with main code. *)
let () =
  lvm_type_register plugin_name probe_pv list_lvs
