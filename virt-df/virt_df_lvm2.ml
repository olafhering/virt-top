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

(*----------------------------------------------------------------------*)
(* Block device which can do linear maps, same as the kernel dm-linear.c *)
class linear_map_device name extent_size segments =
  (* The segments are passed containing (start_extent, extent_count, ...)
   * but it's easier to deal with (start_extent, end_extent, ...) so
   * rewrite them.
   *)
  let segments = List.map
    (fun (start_extent, extent_count, dev, pvoffset) ->
       (start_extent, start_extent +^ extent_count, dev, pvoffset)
    ) segments in

  (* Calculate the size of the device (in bytes).  Note that because
   * of the random nature of the mapping this doesn't imply that we can
   * satisfy any read request up to the full size.
   *)
  let size_in_extents =
    List.fold_left max 0L
      (List.map (fun (_, end_extent, _, _) -> end_extent) segments) in
  let size = size_in_extents *^ extent_size in
object
  inherit device
  method name = name
  method size = size

  (* Read method checks which segment the request lies inside and
   * maps it to the underlying device.  If there is no mapping then
   * we have to return an error.
   *
   * The request must lie inside a single extent, otherwise this is
   * also an error (XXX - should lift this restriction, however default
   * extent size is 4 MB so we probably won't hit this very often).
   *)
  method read offset len =
    let offset_in_extents = offset /^ extent_size in

    (* Check we don't cross an extent boundary. *)
    if (offset +^ Int64.of_int (len-1)) /^ extent_size <> offset_in_extents
    then invalid_arg "linear_map_device: request crosses extent boundary";

    if offset_in_extents < 0L || offset_in_extents >= size_in_extents then
      invalid_arg "linear_map_device: read outside device";

    let rec loop = function
      | [] ->
	  invalid_arg "linear_map_device: offset not mapped"
      | (start_extent, end_extent, dev, pvoffset) :: rest ->
	  eprintf "pvoffset = %Ld\n" pvoffset;
	  if start_extent <= offset_in_extents &&
	     offset_in_extents < end_extent
	  then dev#read (offset +^ pvoffset *^ extent_size) len
	  else loop rest
    in
    loop segments
end

(*----------------------------------------------------------------------*)
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

(*----------------------------------------------------------------------*)
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

  (* Print the parsed metadata. *)
  if !debug then
    List.iter (
      fun (uuid, (metadata, dev)) ->
	eprintf "metadata for PV UUID %s on %s:\n" uuid dev#name;
	output_metadata stderr metadata
    ) pvs;

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
    | _ -> raise Not_found
  and get_stripes field meta =		(* List of (string,int) pairs. *)
    match List.assoc field meta with
    | List xs ->
	let rec loop = function
	  | [] -> []
	  | String pvname :: Int offset :: xs ->
	      (pvname, offset) :: loop xs
	  | _ -> raise Not_found
	in
	loop xs
    | _ -> raise Not_found
  in

  (* The volume groups refer to the physical volumes using their
   * own naming system ("pv0", "pv1", etc.) instead of PV UUIDs.
   *
   * Each PV also has a start (in sectors) & count (in extents)
   * of the writable area (the bit after the superblock and metadata)
   * which normally starts at sector 384.
   *
   * Create a PV device (simple offset + size) and a map from PV
   * names to these devices.
   *)
  let vgs = List.map (
    fun (vgname, (pvuuids, vgmeta)) ->
      let pvdevs, extent_size =
	try
	  (* NB: extent_size is in sectors here - we convert to bytes. *)
	  let extent_size = get_int "extent_size" vgmeta 0 (1024*1024) in
	  let extent_size = Int64.of_int extent_size *^ sector_size64 in

	  (* Get the physical_volumes section of the metadata. *)
	  let pvdevs = get_meta "physical_volumes" vgmeta in

	  List.filter_map (
	    function
	    | (pvname, Metadata meta) ->
		(* Get the UUID. *)
		let pvuuid = get_string "id" meta in
		let pvuuid = canonical_uuid pvuuid in

		(* Get the underlying physical device. *)
		let _, dev = List.assoc pvuuid pvs in

		(* Construct a PV device. *)
		let pe_start = get_int64 "pe_start" meta in
		let pe_start = pe_start *^ sector_size64 in
		let pe_count = get_int64 "pe_count" meta in
		let pe_count = pe_count *^ extent_size in
		let pvdev = new offset_device pvuuid pe_start pe_count dev in

		Some (pvname, pvdev)
	    | _ ->
		None
	  ) pvdevs, extent_size
	with
	  (* Something went wrong - just return an empty map. *)
	  Not_found -> [], 0L in
      (vgname, (pvuuids, vgmeta, pvdevs, extent_size))
  ) vgs in

  (* Scan for logical volumes.  Each VG contains several LVs.
   * This gives us a list of LVs within each VG (hence extends
   * the vgs variable).
   *)
  let vgs = List.map (
    fun (vgname, (pvuuids, vgmeta, pvdevs, extent_size)) ->
      let lvs =
	try
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

			 (* Can only handle striped segments at the
			  * moment. XXX
			  *)
			 if segtype <> "striped" then raise Not_found;

			 let stripe_count =
			   get_int "stripe_count" segmeta 0 1024 in
			 let stripes = get_stripes "stripes" segmeta in

			 if List.length stripes <> stripe_count then
			   raise Not_found;

			 (* Can only handle linear striped segments at
			  * the moment. XXX
			  *)
			 if stripe_count <> 1 then raise Not_found;
			 let pvname, pvoffset = List.hd stripes in

			 (start_extent, extent_count, pvname, pvoffset)
		     ) segments in

		   Some (lvname, segments)
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
      (vgname, (pvuuids, vgmeta, pvdevs, extent_size, lvs))
  ) vgs in

  (* Print the LVs. *)
  if !debug then (
    List.iter (
      fun (vgname, (pvuuids, vgmeta, pvdevs, extent_size, lvs)) ->
	eprintf "VG %s: (extent_size = %Ld bytes)\n" vgname extent_size;
	List.iter (
	  fun (lvname, segments) ->
	    eprintf "  %s/%s:\n" vgname lvname;
	    List.iter (
	      fun (start_extent, extent_count, pvname, pvoffset) ->
		eprintf "    start %Ld count %Ld at %s:%Ld\n"
		  start_extent extent_count pvname pvoffset
	    ) segments
	) lvs
    ) vgs;
    flush stderr
  );

  (* Finally we can set up devices for the LVs. *)
  let lvs =
    List.map (
      fun (vgname, (pvuuid, vgmeta, pvdevs, extent_size, lvs)) ->
	try
	  List.map (
	    fun (lvname, segments) ->
	      let name = vgname ^ "/" ^ lvname in
	      let segments = List.map (
		fun (start_extent, extent_count, pvname, pvoffset) ->
		  (* Get the PV device. *)
		  let pvdev = List.assoc pvname pvdevs in

		  (* Extents                 mapped to:             *)
		  (start_extent, extent_count,          pvdev, pvoffset)
	      ) segments in

	      (* Create a linear mapping device. *)
	      let lv_dev = new linear_map_device name extent_size segments in

	      { lv_dev = lv_dev }
	  ) lvs
	with
	  Not_found -> []
    ) vgs in
  let lvs = List.concat lvs in

  (* Return the list of LV devices. *)
  lvs

(*----------------------------------------------------------------------*)
(* Register with main code. *)
let () =
  lvm_type_register plugin_name probe_pv list_lvs
