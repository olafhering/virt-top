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

(* Probe to see if it's an LVM2 PV. *)
let rec probe_pv lvm_plugin_id dev =
  try
    let uuid, _ = read_pv_label dev in
    if debug then
      eprintf "LVM2 detected PV UUID %s\n%!" uuid;
    { lvm_plugin_id = lvm_plugin_id; pv_uuid = uuid }
  with exn ->
    if debug then prerr_endline (Printexc.to_string exn);
    raise Not_found

and read_pv_label dev =
  (* Load the first 8 sectors.  I found by experimentation that
   * the second sector contains the header ("LABELONE" etc) and
   * the nineth sector contains some additional information about
   * the location of the current metadata.
   *)
  let bits = dev#read_bitstring 0L (9 * sector_size) in

  (*Bitmatch.hexdump_bitstring stdout bits;*)

  bitmatch bits with
  | sector0 : sector_size*8 : bitstring; (* sector 0 *)
    labelone : 8*8 : bitstring;		(* "LABELONE" *)
    padding : 16*8 : bitstring;		(* Seems to contain something. *)
    lvm2_ver : 8*8 : bitstring;		(* "LVM2 001" *)
    uuid : 32*8 : bitstring;		(* UUID *)
    padding2 : (sector_size-64)*8 : bitstring; (* to end of second sector *)
    sector234567 : sector_size*8 * 6 : bitstring; (* sectors 2-6 *)
    padding3 : 0x28*8 : bitstring;      (* start of sector 8 *)
    metadata_offset : 32 : littleendian;(* metadata offset *)
    padding4 : 4*8 : bitstring;
    metadata_length : 32 : littleendian	(* length of metadata (bytes) *)
      when Bitmatch.string_of_bitstring labelone = "LABELONE" &&
	   Bitmatch.string_of_bitstring lvm2_ver = "LVM2 001" ->
    let metadata_offset = metadata_offset +* 0x1000_l in
    let metadata = read_metadata dev metadata_offset metadata_length in
    (*prerr_endline metadata;*)
    let uuid = Bitmatch.string_of_bitstring uuid in

    uuid, metadata

  | _ ->
    invalid_arg
      (sprintf "LVM2: read_pv_label: %s: not an LVM2 physical volume" dev#name)

and read_metadata dev offset32 len32 =
  if debug then
    eprintf "metadata: offset 0x%lx len %ld bytes\n%!" offset32 len32;

  (* Check the offset and length are sensible. *)
  let offset64 =
    if offset32 <= Int32.max_int then Int64.of_int32 offset32
    else invalid_arg "LVM2: read_metadata: metadata offset too large" in
  let len64 =
    if len32 <= 2_147_483_647_l then Int64.of_int32 len32
    else invalid_arg "LVM2: read_metadata: metadata length too large" in

  if offset64 <= 0x1200L || offset64 >= dev#size
    || len64 <= 0L || offset64 +^ len64 >= dev#size then
      invalid_arg "LVM2: read_metadata: bad metadata offset or length";

  (* If it is outside the disk boundaries, this will throw an exception,
   * otherwise it will read and return the metadata string.
   *)
  dev#read offset64 (Int64.to_int len64)

(* We are passed a list of devices which we previously identified
 * as PVs belonging to us.  From these produce a list of all LVs
 * (as devices) and return them.  Note that we don't try to detect
 * what is on these LVs - that will be done in the main code.
 *)
let list_lvs devs =
  (* Read the UUID and metadata (again) from each device. *)
  let uuidmetas = List.map read_pv_label devs in
  []

(* Register with main code. *)
let () =
  lvm_type_register plugin_name probe_pv list_lvs
