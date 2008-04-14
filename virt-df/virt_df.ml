(* 'df' command for virtual domains.
   (C) Copyright 2007-2008 Richard W.M. Jones, Red Hat Inc.
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

open Virt_df_gettext.Gettext

module C = Libvirt.Connect
module D = Libvirt.Domain

(* If set to true, then emit lots of debugging information. *)
let debug = true

(* Int32 infix operators for convenience. *)
let ( +* ) = Int32.add
let ( -* ) = Int32.sub
let ( ** ) = Int32.mul
let ( /* ) = Int32.div

(* Int64 infix operators for convenience. *)
let ( +^ ) = Int64.add
let ( -^ ) = Int64.sub
let ( *^ ) = Int64.mul
let ( /^ ) = Int64.div

(* State of command line arguments. *)
let uri = ref None			(* Hypervisor/libvirt URI. *)
let inodes = ref false			(* Display inodes. *)
let human = ref false			(* Display human-readable. *)
let all = ref false			(* Show all/active domains. *)
let test_files = ref []			(* Used for test mode only. *)

(*----------------------------------------------------------------------*)
(* The "domain/device model" that we currently understand looks
 * like this:
 *
 * domains
 *   |
 *   \--- host partitions / disk image files
 *          ||
 *        guest block devices
 *          |
 *          +--> guest partitions (eg. using MBR)
 *          |      |
 *          \-(1)->+--- filesystems (eg. ext3)
 *                 |
 *                 \--- PVs for LVM
 *                        |||
 *                      VGs and LVs
 *
 * (1) Filesystems and PVs may also appear directly on guest
 * block devices.
 *
 * Partition schemes (eg. MBR) and filesystems register themselves
 * with this main module and they are queried first to get an idea
 * of the physical devices, partitions and filesystems potentially
 * available to the guest.
 *
 * Volume management schemes (eg. LVM) register themselves here
 * and are called later with "spare" physical devices and partitions
 * to see if they contain LVM data.  If this results in additional
 * logical volumes then these are checked for filesystems.
 *
 * Swap space is considered to be a dumb filesystem for the purposes
 * of this discussion.
 *)

(* A virtual (or physical!) device, encapsulating any translation
 * that has to be done to access the device.  eg. For partitions
 * there is a simple offset, but for LVM you may need complicated
 * table lookups.
 *
 * We keep the underlying file descriptors open for the duration
 * of the program.  There aren't likely to be many of them, and
 * the program is short-lived, and it's easier than trying to
 * track which device is using what fd.  As a result, there is no
 * need for any close/deallocation function.
 *
 * Note the very rare use of OOP in OCaml!
 *)
class virtual device =
object (self)
  method virtual read : int64 -> int -> string
  method virtual size : int64
  method virtual name : string

  (* Helper method to read a chunk of data into a bitstring. *)
  method read_bitstring offset len =
    let str = self#read offset len in
    (str, 0, len * 8)
end

(* A concrete device which just direct-maps a file or /dev device. *)
class block_device filename =
  let fd = openfile filename [ O_RDONLY ] 0 in
  let size = (LargeFile.fstat fd).LargeFile.st_size in
object (self)
  inherit device
  method read offset len =
    ignore (LargeFile.lseek fd offset SEEK_SET);
    let str = String.make len '\000' in
    read fd str 0 len;
    str
  method size = size
  method name = filename
end

(* A null device.  Any attempt to read generates an error. *)
let null_device : device =
object
  inherit device
  method read _ _ = assert false
  method size = 0L
  method name = "null"
end

(* Domains and candidate guest block devices. *)

type domain = {
  dom_name : string;			(* Domain name. *)
  dom_id : int option;			(* Domain ID (if running). *)
  dom_disks : disk list;		(* Domain disks. *)
}
and disk = {
  (* From the XML ... *)
  d_type : string option;		(* The <disk type=...> *)
  d_device : string;			(* The <disk device=...> (eg "disk") *)
  d_source : string;		        (* The <source file=... or dev> *)
  d_target : string;			(* The <target dev=...> (eg "hda") *)

  (* About the device itself. *)
  d_dev : device;			(* Disk device. *)
  d_content : disk_content;		(* What's on it. *)
}
and disk_content =
  [ `Unknown				(* Not probed or unknown. *)
  | `Partitions of partitions		(* Contains partitions. *)
  | `Filesystem of filesystem		(* Contains a filesystem directly. *)
  | `PhysicalVolume of unit		(* Contains an LVM PV. *)
  ]

(* Partitions. *)

and partitions = {
  parts_name : string;			(* Name of partitioning scheme. *)
  parts : partition list		(* Partitions. *)
}
and partition = {
  part_status : partition_status;	(* Bootable, etc. *)
  part_type : int;			(* Partition filesystem type. *)
  part_dev : device;			(* Partition device. *)
  part_content : partition_content;	(* What's on it. *)
}
and partition_status = Bootable | Nonbootable | Malformed | NullEntry
and partition_content =
  [ `Unknown				(* Not probed or unknown. *)
  | `Filesystem of filesystem		(* Filesystem. *)
  | `PhysicalVolume of unit		(* Contains an LVM PV. *)
  ]

(* Filesystems (also swap devices). *)
and filesystem = {
  fs_name : string;			(* Name of filesystem. *)
  fs_block_size : int64;		(* Block size (bytes). *)
  fs_blocks_total : int64;		(* Total blocks. *)
  fs_is_swap : bool;			(* If swap, following not valid. *)
  fs_blocks_reserved : int64;		(* Blocks reserved for super-user. *)
  fs_blocks_avail : int64;		(* Blocks free (available). *)
  fs_blocks_used : int64;		(* Blocks in use. *)
  fs_inodes_total : int64;		(* Total inodes. *)
  fs_inodes_reserved : int64;		(* Inodes reserved for super-user. *)
  fs_inodes_avail : int64;		(* Inodes free (available). *)
  fs_inodes_used : int64;		(* Inodes in use. *)
}

(* Convert partition, filesystem types to printable strings for debugging. *)
let string_of_partition
    { part_status = status; part_type = typ; part_dev = dev } =
  sprintf "%s: %s partition type %d"
    dev#name
    (match status with
     | Bootable -> "bootable"
     | Nonbootable -> "nonbootable"
     | Malformed -> "malformed"
     | NullEntry -> "empty")
    typ

let string_of_filesystem { fs_name = name; fs_is_swap = swap } =
  if not swap then name
  else name ^ " [swap]"

(* Register a partition scheme. *)
let partition_types = ref []
let partition_type_register (parts_name : string) probe_fn =
  partition_types := (parts_name, probe_fn) :: !partition_types

(* Probe a device for partitions.  Returns [Some parts] or [None]. *)
let probe_for_partitions dev =
  if debug then eprintf "probing for partitions on %s ...\n%!" dev#name;
  let rec loop = function
    | [] -> None
    | (parts_name, probe_fn) :: rest ->
	try Some (probe_fn dev)
	with Not_found -> loop rest
  in
  let r = loop !partition_types in
  if debug then (
    match r with
    | None -> eprintf "no partitions found on %s\n%!" dev#name
    | Some { parts_name = name; parts = parts } ->
	eprintf "found %d %s partitions on %s:\n"
	  (List.length parts) name dev#name;
	List.iter (fun p -> eprintf "\t%s\n%!" (string_of_partition p)) parts
  );
  r

(* Register a filesystem type (or swap). *)
let filesystem_types = ref []
let filesystem_type_register (fs_name : string) probe_fn =
  filesystem_types := (fs_name, probe_fn) :: !filesystem_types

(* Probe a device for filesystems.  Returns [Some fs] or [None]. *)
let probe_for_filesystems dev =
  if debug then eprintf "probing for a filesystem on %s ...\n%!" dev#name;
  let rec loop = function
    | [] -> None
    | (fs_name, probe_fn) :: rest ->
	try Some (probe_fn dev)
	with Not_found -> loop rest
  in
  let r = loop !filesystem_types in
  if debug then (
    match r with
    | None -> eprintf "no filesystem found on %s\n%!" dev#name
    | Some fs ->
	eprintf "found a filesystem on %s:\n" dev#name;
	eprintf "\t%s\n%!" (string_of_filesystem fs)
  );
  r

(* Register a volume management type. *)
(*
let lvm_types = ref []
let lvm_type_register (lvm_name : string) probe_fn =
  lvm_types := (lvm_name, probe_fn) :: !lvm_types
*)

(*----------------------------------------------------------------------*)

let main () =
  (* Command line argument parsing. *)
  let set_uri = function "" -> uri := None | u -> uri := Some u in

  let version () =
    printf "virt-df %s\n" (Libvirt_version.version);

    let major, minor, release =
      let v, _ = Libvirt.get_version () in
      v / 1_000_000, (v / 1_000) mod 1_000, v mod 1_000 in
    printf "libvirt %d.%d.%d\n" major minor release;
    exit 0
  in

  let test_mode filename =
    test_files := filename :: !test_files
  in

  let argspec = Arg.align [
    "-a", Arg.Set all,
      " " ^ s_ "Show all domains (default: only active domains)";
    "--all", Arg.Set all,
      " " ^ s_ "Show all domains (default: only active domains)";
    "-c", Arg.String set_uri,
      "uri " ^ s_ "Connect to URI (default: Xen)";
    "--connect", Arg.String set_uri,
      "uri " ^ s_ "Connect to URI (default: Xen)";
    "-h", Arg.Set human,
      " " ^ s_ "Print sizes in human-readable format";
    "--human-readable", Arg.Set human,
      " " ^ s_ "Print sizes in human-readable format";
    "-i", Arg.Set inodes,
      " " ^ s_ "Show inodes instead of blocks";
    "--inodes", Arg.Set inodes,
      " " ^ s_ "Show inodes instead of blocks";
    "-t", Arg.String test_mode,
      "dev" ^ s_ "(Test mode) Display contents of block device or file";
    "--version", Arg.Unit version,
      " " ^ s_ "Display version and exit";
  ] in

  let anon_fun str =
    raise (Arg.Bad (sprintf (f_ "%s: unknown parameter") str)) in
  let usage_msg = s_ "virt-df : like 'df', shows disk space used in guests

SUMMARY
  virt-df [-options]

OPTIONS" in

  Arg.parse argspec anon_fun usage_msg;

  let doms : domain list =
    if !test_files = [] then (
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
		print_endline (s_ "NB: If you want to monitor a local Xen hypervisor, you usually need to be root");
	      );
	      exit 1 in

	(* Get the list of active & inactive domains. *)
	let doms =
	  let nr_active_doms = C.num_of_domains conn in
	  let active_doms =
	    Array.to_list (C.list_domains conn nr_active_doms) in
	  let active_doms =
	    List.map (D.lookup_by_id conn) active_doms in
	  if not !all then
	    active_doms
	  else (
	    let nr_inactive_doms = C.num_of_defined_domains conn in
	    let inactive_doms =
	      Array.to_list (C.list_defined_domains conn nr_inactive_doms) in
	    let inactive_doms =
	      List.map (D.lookup_by_name conn) inactive_doms in
	    active_doms @ inactive_doms
	  ) in

	(* Get their XML. *)
	let xmls = List.map D.get_xml_desc doms in

	(* Parse the XML. *)
	let xmls = List.map Xml.parse_string xmls in

	(* Return just the XML documents - everything else will be closed
	 * and freed including the connection to the hypervisor.
	 *)
	xmls in

      (* Grr.. Need to use a library which has XPATH support (or cduce). *)
      List.map (
	fun xml ->
	  let nodes, domain_attrs =
	    match xml with
	    | Xml.Element ("domain", attrs, children) -> children, attrs
	    | _ -> failwith (s_ "get_xml_desc didn't return <domain/>") in

	  let domid =
	    try Some (int_of_string (List.assoc "id" domain_attrs))
	    with Not_found -> None in

	  let rec loop = function
	    | [] ->
		failwith (s_ "get_xml_desc returned no <name> node in XML")
	    | Xml.Element ("name", _, [Xml.PCData name]) :: _ -> name
	    | Xml.Element ("name", _, _) :: _ ->
		failwith (s_ "get_xml_desc returned strange <name> node")
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

		  (* We only care about devices where we have
		   * source and target.  Ignore CD-ROM devices.
		   *)
		  (match source, target, device with
		   | _, _, Some "cdrom" -> None (* ignore *)
		   | Some source, Some target, Some device ->
		       (* Try to create a 'device' object for this
			* device.  If it fails, print a warning
			* and ignore the device.
			*)
		       (try
			  let dev = new block_device source in
			  Some {
			    d_type = typ; d_device = device;
			    d_source = source; d_target = target;
			    d_dev = dev; d_content = `Unknown
			  }
			with
			  Unix_error (err, func, param) ->
			    eprintf "%s:%s: %s" func param (error_message err);
			    None
		       )
		   | _ -> None (* ignore anything else *)
		  )

	      | _ -> None
	    ) devices in

	  { dom_name = name; dom_id = domid; dom_disks = disks }
      ) xmls
    ) else (
      (* In test mode (-t option) the user can pass one or more
       * block devices or filenames (containing partitions/filesystems/etc)
       * which we use for testing virt-df itself.  We create fake domains
       * from these.
       *)
      List.map (
	fun filename ->
	  {
	    dom_name = filename; dom_id = None;
	    dom_disks = [
	      {
		d_type = Some "disk"; d_device = "disk";
		d_source = filename; d_target = "hda";
		d_dev = new block_device filename; d_content = `Unknown;
	      }
	    ]
	  }
      ) !test_files
    ) in

  (* HOF to map over disks. *)
  let map_over_disks doms f =
    List.map (
      fun ({ dom_disks = disks } as dom) ->
	let disks = List.map f disks in
	{ dom with dom_disks = disks }
    ) doms
  in

  (* 'doms' is our list of domains and their guest block devices, and
   * we've successfully opened each block device.  Now probe them
   * to find out what they contain.
   *)
  let doms = map_over_disks doms (
    fun ({ d_dev = dev } as disk) ->
      (* See if it is partitioned first. *)
      let parts = probe_for_partitions dev in
      match parts with
      | Some parts ->
	  { disk with d_content = `Partitions parts }
      | None ->
	  (* Not partitioned.  Does it contain a filesystem? *)
	  let fs = probe_for_filesystems dev in
	  match fs with
	  | Some fs ->
	      { disk with d_content = `Filesystem fs }
	  | None ->
	      (* Not partitioned, no filesystem, so it's spare. *)
	      disk
  ) in

  (* Now we have either detected partitions or a filesystem on each
   * physical device (or perhaps neither).  See what is on those
   * partitions.
   *)
  let doms = map_over_disks doms (
    function
    | ({ d_dev = dev; d_content = `Partitions parts } as disk) ->
	let ps = List.map (
	  fun p ->
	    if p.part_status = Bootable || p.part_status = Nonbootable then (
	      let fs = probe_for_filesystems p.part_dev in
	      match fs with
	      | Some fs ->
		  { p with part_content = `Filesystem fs }
	      | None ->
		  p
	    ) else p
	) parts.parts in
	let parts = { parts with parts = ps } in
	{ disk with d_content = `Partitions parts }
    | disk -> disk
  ) in

  (* XXX LVM stuff here. *)



  (* Print the title. *)
  let () =
    let total, used, avail =
      match !inodes, !human with
      | false, false -> s_ "1K-blocks", s_ "Used", s_ "Available"
      | false, true -> s_ "Size", s_ "Used", s_ "Available"
      | true, _ -> s_ "Inodes", s_ "IUse", s_ "IFree" in
    printf "%-20s %10s %10s %10s %s\n%!"
      (s_ "Filesystem") total used avail (s_ "Type") in

  let printable_size bytes =
    if bytes < 1024L *^ 1024L then
      sprintf "%Ld bytes" bytes
    else if bytes < 1024L *^ 1024L *^ 1024L then
      sprintf "%.1f MiB" (Int64.to_float (bytes /^ 1024L) /. 1024.)
    else
      sprintf "%.1f GiB" (Int64.to_float (bytes /^ 1024L /^ 1024L) /. 1024.)
  in

  (* HOF to iterate over filesystems. *)
  let iter_over_filesystems doms f =
    List.iter (
      fun ({ dom_disks = disks } as dom) ->
	List.iter (
	  function
	  | ({ d_content = `Filesystem fs } as disk) ->
	      f dom disk None fs
	  | ({ d_content = `Partitions partitions } as disk) ->
	      List.iteri (
		fun i ->
		  function
		  | ({ part_content = `Filesystem fs } as part) ->
		      f dom disk (Some (part, i)) fs
		  | _ -> ()
	      ) partitions.parts
	  | _ -> ()
	) disks
    ) doms
  in

  (* Print stats for each recognized filesystem. *)
  let print_stats dom disk part fs =
    (* Printable name is like "domain:hda" or "domain:hda1". *)
    let name =
      let dom_name = dom.dom_name in
      let d_target = disk.d_target in
      match part with
      | None ->
	  dom_name ^ ":" ^ d_target
      | Some (_, pnum) ->
	  dom_name ^ ":" ^ d_target ^ string_of_int pnum in
    printf "%-20s " name;

    if fs.fs_is_swap then (
      (* Swap partition. *)
      if not !human then
	printf "%10Ld                       %s\n"
	  (fs.fs_block_size *^ fs.fs_blocks_total /^ 1024L) fs.fs_name
      else
	printf "%10s                       %s\n"
	  (printable_size (fs.fs_block_size *^ fs.fs_blocks_total)) fs.fs_name
    ) else (
      (* Ordinary filesystem. *)
      if not !inodes then (		(* Block display. *)
	(* 'df' doesn't count the restricted blocks. *)
	let blocks_total = fs.fs_blocks_total -^ fs.fs_blocks_reserved in
	let blocks_avail = fs.fs_blocks_avail -^ fs.fs_blocks_reserved in
	let blocks_avail = if blocks_avail < 0L then 0L else blocks_avail in

	if not !human then (		(* Display 1K blocks. *)
	  printf "%10Ld %10Ld %10Ld %s\n"
	    (blocks_total *^ fs.fs_block_size /^ 1024L)
	    (fs.fs_blocks_used *^ fs.fs_block_size /^ 1024L)
	    (blocks_avail *^ fs.fs_block_size /^ 1024L)
	    fs.fs_name
	) else (			(* Human-readable blocks. *)
	  printf "%10s %10s %10s %s\n"
	    (printable_size (blocks_total *^ fs.fs_block_size))
	    (printable_size (fs.fs_blocks_used *^ fs.fs_block_size))
	    (printable_size (blocks_avail *^ fs.fs_block_size))
	    fs.fs_name
	)
      ) else (				(* Inodes display. *)
	printf "%10Ld %10Ld %10Ld %s\n"
	  fs.fs_inodes_total fs.fs_inodes_used fs.fs_inodes_avail
	  fs.fs_name
      )
    )
  in
  iter_over_filesystems doms print_stats

(*
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
      ProbeFailed (s_ "detection of unpartitioned devices not yet supported")
  | Some 0x05 ->
      ProbeIgnore (* Extended partition - ignore it. *)
  | Some part_type ->
      try
	let probe_fn = Hashtbl.find filesystems part_type in
	probe_fn target part_type fd start size
      with
	Not_found ->
	  ProbeFailed
	    (sprintf (f_ "unsupported partition type %02x") part_type)
*)
