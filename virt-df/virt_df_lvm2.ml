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
open ExtList

open Virt_df_gettext.Gettext
open Virt_df

open Virt_df_lvm2_metadata

let plugin_name = "LVM2"

let sector_size = 512
let sector_size64 = 512L

(* Probe to see if it's an LVM2 PV. *)
let rec probe_pv lvm_plugin_id dev =
  try
    let uuid, _ = read_pv_label dev in
    if !debug then
      eprintf "LVM2 detected PV UUID %s\n%!" uuid;
    { lvm_plugin_id = lvm_plugin_id; pv_uuid = uuid }
  with exn ->
    if !debug then prerr_endline (Printexc.to_string exn);
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

    (* Metadata offset is relative to end of PV label. *)
    let metadata_offset = metadata_offset +* 0x1000_l in
    (* Metadata length appears to include the trailing \000 which
     * we don't want.
     *)
    let metadata_length = metadata_length -* 1_l in

    let metadata = read_metadata dev metadata_offset metadata_length in

    let uuid = Bitmatch.string_of_bitstring uuid in

    uuid, metadata

  | _ ->
    invalid_arg
      (sprintf "LVM2: read_pv_label: %s: not an LVM2 physical volume" dev#name)

and read_metadata dev offset32 len32 =
  if !debug then
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
let rec list_lvs devs =
  (* Read the UUID and metadata (again) from each device to end up with
   * an assoc list of PVs, keyed on the UUID.
   *)
  let pvs = List.map (
    fun dev ->
      let uuid, metadata = read_pv_label dev in
      (uuid, (metadata, dev))
  ) devs in

  (* Parse the metadata using the external lexer/parser. *)
  let pvs = List.map (
    fun (uuid, (metadata, dev)) ->
      uuid, (Virt_df_lvm2_lexer.parse_lvm2_metadata_from_string metadata,
	     dev)
  ) pvs in

  (* Print the parsed metadata.
  List.iter (
    fun (uuid, (metadata, dev)) ->
      eprintf "metadata for UUID %s:\n" uuid;
      output_metadata stderr metadata
  ) pvs;
  *)

  (* Scan for volume groups.  The first entry in the metadata
   * appears to be the volume group name.  This gives us a
   * list of VGs and the metadata for each underlying PV.
   *)
  let vgnames =
    List.filter_map (
      function
      | pvuuid, (((vgname, Metadata vgmeta) :: _), dev) ->
	  Some (vgname, (pvuuid, vgmeta))
      | _ -> None
    ) pvs in

  let cmp ((a:string),_) ((b:string),_) = compare a b in
  let vgnames = List.sort ~cmp vgnames in
  let vgs = group_by vgnames in

  (* Note that the metadata is supposed to be duplicated
   * identically across all PVs (for redundancy purposes).
   * In theory we should check this and use the 'seqno'
   * field to find the latest metadata if it doesn't match,
   * but in fact we don't check this.
   *)
  let vgs = List.map (
    fun (vgname, metas) ->
      let pvuuids = List.map fst metas in
      let _, vgmeta = List.hd metas in (* just pick any metadata *)
      vgname, (pvuuids, vgmeta)) vgs in

  (* Print the VGs. *)
  if !debug then
    List.iter (
      fun (vgname, (pvuuids, vgmeta)) ->
	eprintf "VG %s is on PVs: %s\n%!" vgname (String.concat "," pvuuids)
    ) vgs;

  (* Some useful getter functions.  If these can't get a value
   * from the metadata or if the type is wrong they raise Not_found.
   *)
  let rec get_int64 field meta =
    match List.assoc field meta with
    | Int i -> i
    | _ -> raise Not_found
  and get_int field meta min max =
    match List.assoc field meta with
    | Int i when Int64.of_int min <= i && i <= Int64.of_int max ->
	Int64.to_int i
    | _ -> raise Not_found
  and get_string field meta =
    match List.assoc field meta with
    | String s -> s
    | _ -> raise Not_found
  and get_meta field meta =
    match List.assoc field meta with
    | Metadata md -> md
    | _ -> raise Not_found in
  in

  (* Scan for logical volumes.  Each VG contains several LVs.
   * This gives us a list of LVs within each VG (hence extends
   * the vgs variable).
   *)
  let vgs = List.map (
    fun (vgname, (pvuuids, vgmeta)) ->
      let lvs =
	try
	  let extent_size = get_int "extent_size" vgmeta 0 (256*1024) in
	  let lvs = get_meta "logical_volumes" vgmeta in
	  let lvs = List.filter_map (
	    function
	    | lvname, Metadata lvmeta ->
		(try
		   let segment_count = get_int "segment_count" lvmeta 0 1024 in

		   (* Get the segments for this LV. *)
		   let segments = range 1 (segment_count+1) in
		   let segments =
		     List.map
		       (fun i -> get_meta ("segment" ^ string_of_int i) lvmeta)
		       segments in

		   let segments =
		     List.map (
		       fun segmeta ->
			 let start_extent =
			   get_int64 "start_extent" segmeta in
			 let extent_count =
			   get_int64 "extent_count" segmeta in
			 let segtype = get_string "type" segmeta in
			 if segtype <> "striped" then raise Not_found;
			 let stripe_count =
			   get_int "stripe_count" segmeta 0 1024 in
			 (* let stripes =  in *)

			 (start_extent, extent_count, stripe_count)
		     ) segments in

		   Some (lvname, (lvmeta, segments))
		 with
		   (* Something went wrong with segments - omit this LV. *)
		   Not_found -> None)
	    | _ -> None
	  ) lvs in

	  lvs
	with
	  Not_found ->
	    (* Something went wrong - assume no LVs found. *)
	    [] in
      (vgname, (pvuuids, vgmeta, lvs))
  ) vgs in

  (* Print the LVs. *)
  if !debug then
    List.iter (
      fun (vgname, (pvuuids, vgmeta, lvs)) ->
	let lvnames = List.map fst lvs in
	eprintf "VG %s contains LVs: %s\n%!" vgname (String.concat ", " lvnames)
    ) vgs;

  []

(* Register with main code. *)
let () =
  lvm_type_register plugin_name probe_pv list_lvs
