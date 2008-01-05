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
 *)

open Printf
open ExtList

open Unix

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

(* Int64 operators for convenience.
 * For sanity we do all int operations as int64's.
 *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

let uri = ref None
let inodes = ref false
let human = ref false

(* Maximum number of extended partitions possible. *)
let max_extended_partitions = 100

let sector_size = 512L

(* Parse out the device XML to get the names of disks. *)
type domain = {
  dom_name : string;			(* Domain name. *)
  dom_id : int option;			(* Domain ID (if running). *)
  dom_disks : disk list;		(* Domain disks. *)
}
and disk = {
  d_type : string option;		(* The <disk type=...> *)
  d_device : string option;		(* The <disk device=...> *)
  d_source : string option;		(* The <source file=... or dev> *)
  d_target : string option;		(* The <target dev=...> *)
}

type partition = {
  part_status : partition_status;	(* Bootable, etc. *)
  part_type : int;			(* Partition type. *)
  part_lba_start : int64;		(* LBA start sector. *)
  part_len : int64;			(* Length in sectors. *)
}
and partition_status = Bootable | Nonbootable | Malformed | NullEntry

type filesystem_stats = {
  fs_name : string;
  fs_block_size : int64;		(* Block size (bytes). *)
  fs_blocks_total : int64;		(* Total blocks. *)
  fs_blocks_reserved : int64;		(* Blocks reserved for super-user. *)
  fs_blocks_avail : int64;		(* Blocks free (available). *)
  fs_blocks_used : int64;		(* Blocks in use. *)
  fs_inodes_total : int64;		(* Total inodes. *)
  fs_inodes_reserved : int64;		(* Inodes reserved for super-user. *)
  fs_inodes_avail : int64;		(* Inodes free (available). *)
  fs_inodes_used : int64;		(* Inodes in use. *)
}
and swap_stats = {
  swap_name : string;
  swap_block_size : int64;		(* Block size (bytes). *)
  swap_blocks_total : int64;		(* Total blocks. *)
}
and fs_probe_t =			(* Return type of the probe_partition.*)
  | Filesystem of filesystem_stats
  | Swap of swap_stats
  | ProbeFailed of string		(* Probe failed for some reason. *)
  | ProbeIgnore				(* This filesystem should be ignored. *)

(* Register a filesystem type. *)
let filesystems = Hashtbl.create 13
let fs_register part_types probe_fn =
  List.iter
    (fun part_type -> Hashtbl.replace filesystems part_type probe_fn)
    part_types

(* Probe the devices and display.
 * - dom_name is the domain name
 * - target will be something like "hda"
 * - source will be the name of a file or disk partition on the local machine
 *)
let rec probe_device dom_name target source =
  let fd = openfile source [ O_RDONLY ] 0 in
  let size = (LargeFile.fstat fd).LargeFile.st_size in
  let size = size /^ sector_size in	(* Size in sectors. *)

  print_device dom_name target source size;

  let partitions = probe_mbr fd in

  if partitions <> [] then (
    let stats =
      List.mapi (
	fun i part ->
	  if part.part_status = Bootable ||
	    part.part_status = Nonbootable then (
	      let pnum = i+1 in
	      let target = target ^ string_of_int pnum in
	      Some (target,
		    probe_partition target (Some part.part_type)
		      fd part.part_lba_start part.part_len)
	    )
	  else
	    None
      ) partitions in
    let stats = List.filter_map (fun x -> x) stats in
    print_stats stats
  ) else	     (* Not an MBR, assume it's a single partition. *)
    print_stats [target, probe_partition target None fd 0L size];

  close fd

(* Probe the master boot record (if it is one) and read the partitions.
 * Returns [] if this is not an MBR.
 * http://en.wikipedia.org/wiki/Master_boot_record
 *)
and probe_mbr fd =
  lseek fd 510 SEEK_SET;
  let str = String.create 2 in
  if read fd str 0 2 <> 2 || str.[0] != '\x55' || str.[1] != '\xAA' then
    [] (* Not MBR *)
  else (
    (* Read the partition table. *)
    lseek fd 446 SEEK_SET;
    let str = String.create 64 in
    if read fd str 0 64 <> 64 then
      failwith "error reading partition table"
    else (
      (* Extract partitions from the data. *)
      let primaries = List.map (get_partition str) [ 0; 16; 32; 48 ] in
      (* XXX validate partition extents compared to disk. *)
      (* Read extended partition data. *)
      let extendeds = List.map (
	function
	| { part_type = 0x05 } as part ->
	    probe_extended_partition
	      max_extended_partitions fd part part.part_lba_start
	| part -> []
      ) primaries in
      let extendeds = List.concat extendeds in
      primaries @ extendeds
    )
  )

(* Probe an extended partition. *)
and probe_extended_partition max fd epart sect =
  if max > 0 then (
    (* Offset of the first EBR. *)
    let ebr_offs = sect *^ sector_size in
    (* EBR Signature? *)
    LargeFile.lseek fd (ebr_offs +^ 510L) SEEK_SET;
    let str = String.create 2 in
    if read fd str 0 2 <> 2 || str.[0] != '\x55' || str.[1] != '\xAA' then
      [] (* Not EBR *)
    else (
      (* Read the extended partition table entries (just 2 of them). *)
      LargeFile.lseek fd (ebr_offs +^ 446L) SEEK_SET;
      let str = String.create 32 in
      if read fd str 0 32 <> 32 then
	failwith "error reading extended partition"
      else (
	(* Extract partitions from the data. *)
	let part1, part2 =
	  match List.map (get_partition str) [ 0; 16 ] with
	  | [p1;p2] -> p1,p2
	  | _ -> failwith "probe_extended_partition: internal error" in
	(* First partition entry has offset to the start of this partition. *)
	let part1 = { part1 with
			part_lba_start = sect +^ part1.part_lba_start } in
	(* Second partition entry is zeroes if end of list, otherwise points
	 * to the next partition.
	 *)
	if part2.part_status = NullEntry then
	  [part1]
	else
	  part1 :: probe_extended_partition
	             (max-1) fd epart (sect +^ part2.part_lba_start)
      )
    )
  )
  else []

(* Get the partition data from str.[offs] - str.[offs+15] *)
and get_partition str offs =
  let part_type = Char.code str.[offs+4] in
  let part_lba_start = read_int32_le str (offs+8) in
  let part_len = read_int32_le str (offs+12) in

  let part_status =
    if part_type = 0 && part_lba_start = 0L && part_len = 0L then
      NullEntry
    else (
      let part_status = Char.code str.[offs] in
      match part_status with
      | 0x80 -> Bootable | 0 -> Nonbootable | _ -> Malformed
    ) in

  { part_status = part_status;
    part_type = part_type;
    part_lba_start = part_lba_start;
    part_len = part_len }

(* Probe a single partition, which we assume contains either a
 * filesystem or is a PV.
 * - target will be something like "hda" or "hda1"
 * - part_type will be the partition type if known, or None
 * - fd is a file descriptor opened on the device
 * - start & size are where we think the start and size of the
 *   partition is within the file descriptor (in SECTORS)
 *)
and probe_partition target part_type fd start size =
  match part_type with
  | None ->
      ProbeFailed "detection of unpartitioned devices not yet supported"
  | Some 0x05 ->
      ProbeIgnore (* Extended partition - ignore it. *)
  | Some part_type ->
      try
	let probe_fn = Hashtbl.find filesystems part_type in
	probe_fn target part_type fd start size
      with
	Not_found ->
	  ProbeFailed
	    (sprintf "unsupported partition type %02x" part_type)

and print_stats statss =
  List.iter (
    function
    (* Swap partition. *)
    | (target, Swap { swap_name = swap_name;
		      swap_block_size = block_size;
		      swap_blocks_total = blocks_total }) ->
	if not !human then
	  printf "\t%s %Ld %s\n"
	    target (block_size *^ blocks_total /^ 1024L) swap_name
	else
	  printf "\t%s %s %s\n"
	    target (printable_size (block_size *^ blocks_total)) swap_name

    (* Ordinary filesystem. *)
    | (target, Filesystem stats) ->
	printf "\t%s " target;

	if not !inodes then (		(* Block display. *)
	  (* 'df' doesn't count the restricted blocks. *)
	  let blocks_total =
	    stats.fs_blocks_total -^ stats.fs_blocks_reserved in
	  let blocks_avail =
	    stats.fs_blocks_avail -^ stats.fs_blocks_reserved in
	  let blocks_avail =
	    if blocks_avail < 0L then 0L else blocks_avail in

	  if not !human then (		(* Display 1K blocks. *)
	    printf "%Ld %Ld %Ld %s\n"
	      (blocks_total *^ stats.fs_block_size /^ 1024L)
	      (stats.fs_blocks_used *^ stats.fs_block_size /^ 1024L)
	      (blocks_avail *^ stats.fs_block_size /^ 1024L)
	      stats.fs_name
	  ) else (			(* Human-readable blocks. *)
	    printf "%s %s %s %s\n"
	      (printable_size (blocks_total *^ stats.fs_block_size))
	      (printable_size (stats.fs_blocks_used *^ stats.fs_block_size))
	      (printable_size (blocks_avail *^ stats.fs_block_size))
	      stats.fs_name
	  )
	) else (			(* Inodes display. *)
	  printf "%Ld %Ld %Ld %s\n"
	    stats.fs_inodes_total stats.fs_inodes_used stats.fs_inodes_avail
	    stats.fs_name
	)

    (* Unsupported filesystem or other failure. *)
    | (target, ProbeFailed reason) ->
	printf "\t%s %s\n" target reason

    | (_, ProbeIgnore) -> ()
  ) statss

(* Target is something like "hda" and size is the size in sectors. *)
and print_device dom_name target source size =
  printf "%s /dev/%s (%s) %s\n"
    dom_name target (printable_size (size *^ sector_size)) source

and printable_size bytes =
  if bytes < 1024L *^ 1024L then
    sprintf "%Ld bytes" bytes
  else if bytes < 1024L *^ 1024L *^ 1024L then
    sprintf "%.1f MiB" (Int64.to_float (bytes /^ 1024L) /. 1024.)
  else
    sprintf "%.1f GiB" (Int64.to_float (bytes /^ 1024L /^ 1024L) /. 1024.)

and read_int32_le str offs =
  Int64.of_int (Char.code str.[offs]) +^
    256L *^ Int64.of_int (Char.code str.[offs+1]) +^
    65536L *^ Int64.of_int (Char.code str.[offs+2]) +^
    16777216L *^ Int64.of_int (Char.code str.[offs+3])

and read_int16_le str offs =
  Int64.of_int (Char.code str.[offs]) +^
    256L *^ Int64.of_int (Char.code str.[offs+1])

let main () =
  (* Command line argument parsing. *)
  let set_uri = function "" -> uri := None | u -> uri := Some u in

  let argspec = Arg.align [
    "-c", Arg.String set_uri, "uri Connect to URI (default: Xen)";
    "--connect", Arg.String set_uri, "uri Connect to URI (default: Xen)";
    "-h", Arg.Set human, " Print sizes in human-readable format";
    "--human-readable", Arg.Set human, " Print sizes in human-readable format";
    "-i", Arg.Set inodes, " Show inodes instead of blocks";
    "--inodes", Arg.Set inodes, " Show inodes instead of blocks";
  ] in

  let anon_fun str = raise (Arg.Bad (str ^ ": unknown parameter")) in
  let usage_msg = "virt-df : like 'df', shows disk space used in guests

SUMMARY
  virt-df [-options]

OPTIONS" in

  Arg.parse argspec anon_fun usage_msg;

  let xmls =
    (* Connect to the hypervisor. *)
    let conn =
      let name = !uri in
      try C.connect_readonly ?name ()
      with
	Libvirt.Virterror err ->
	  prerr_endline (Libvirt.Virterror.to_string err);
	  (* If non-root and no explicit connection URI, print a warning. *)
	  if geteuid () <> 0 && name = None then (
	    print_endline "NB: If you want to monitor a local Xen hypervisor, you usually need to be root";
	  );
	  exit 1 in

    (* Get the list of active & inactive domains. *)
    let doms =
      let nr_active_doms = C.num_of_domains conn in
      let active_doms = Array.to_list (C.list_domains conn nr_active_doms) in
      let active_doms = List.map (D.lookup_by_id conn) active_doms in
      let nr_inactive_doms = C.num_of_defined_domains conn in
      let inactive_doms =
	Array.to_list (C.list_defined_domains conn nr_inactive_doms) in
      let inactive_doms = List.map (D.lookup_by_name conn) inactive_doms in
      active_doms @ inactive_doms in

    (* Get their XML. *)
    let xmls = List.map D.get_xml_desc doms in

    (* Parse the XML. *)
    let xmls = List.map Xml.parse_string xmls in

    (* Return just the XML documents - everything else will be closed
     * and freed including the connection to the hypervisor.
     *)
    xmls in

  let doms : domain list =
    (* Grr.. Need to use a library which has XPATH support (or cduce). *)
    List.map (
      fun xml ->
	let nodes, domain_attrs =
	  match xml with
	  | Xml.Element ("domain", attrs, children) -> children, attrs
	  | _ -> failwith "get_xml_desc didn't return <domain/>" in

	let domid =
	  try Some (int_of_string (List.assoc "id" domain_attrs))
	  with Not_found -> None in

	let rec loop = function
	  | [] ->
	      failwith "get_xml_desc returned no <name> node in XML"
	  | Xml.Element ("name", _, [Xml.PCData name]) :: _ -> name
	  | Xml.Element ("name", _, _) :: _ ->
	      failwith "get_xml_desc returned strange <name> node"
	  | _ :: rest -> loop rest
	in
	let name = loop nodes in

	let devices =
	  let devices =
	    List.filter_map (
	      function
	      | Xml.Element ("devices", _, devices) -> Some devices
	      | _ -> None
	    ) nodes in
	  List.concat devices in

	let rec target_dev_of = function
	  | [] -> None
	  | Xml.Element ("target", attrs, _) :: rest ->
	      (try Some (List.assoc "dev" attrs)
	       with Not_found -> target_dev_of rest)
	  | _ :: rest -> target_dev_of rest
	in

	let rec source_file_of = function
	  | [] -> None
	  | Xml.Element ("source", attrs, _) :: rest ->
	      (try Some (List.assoc "file" attrs)
	       with Not_found -> source_file_of rest)
	  | _ :: rest -> source_file_of rest
	in

	let rec source_dev_of = function
	  | [] -> None
	  | Xml.Element ("source", attrs, _) :: rest ->
	      (try Some (List.assoc "dev" attrs)
	       with Not_found -> source_dev_of rest)
	  | _ :: rest -> source_dev_of rest
	in

	let disks =
	  List.filter_map (
	    function
	    | Xml.Element ("disk", attrs, children) ->
		let typ =
		  try Some (List.assoc "type" attrs)
		  with Not_found -> None in
		let device =
		  try Some (List.assoc "device" attrs)
		  with Not_found -> None in
		let source =
		  match source_file_of children with
		  | (Some _) as source -> source
		  | None -> source_dev_of children in
		let target = target_dev_of children in

		Some {
		  d_type = typ; d_device = device;
		  d_source = source; d_target = target
		}
	    | _ -> None
	  ) devices in

	{ dom_name = name; dom_id = domid; dom_disks = disks }
    ) xmls in

  (* Probe the devices. *)
  List.iter (
    fun { dom_name = dom_name; dom_disks = dom_disks } ->
      List.iter (
	function
	| { d_source = Some source; d_target = Some target } ->
	    probe_device dom_name target source
	| { d_device = Some "cdrom" } ->
	    () (* Ignore physical CD-ROM devices. *)
	| _ ->
	    printf "(device omitted)\n";
      ) dom_disks
  ) doms
