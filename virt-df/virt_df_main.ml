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

open Virt_df_gettext.Gettext
open Virt_df

let () =
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

	  { dom_name = name; dom_id = domid;
	    dom_disks = disks; dom_lv_filesystems = [] }
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
	    ];
	    dom_lv_filesystems = []
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
	  let fs = probe_for_filesystem dev in
	  match fs with
	  | Some fs ->
	      { disk with d_content = `Filesystem fs }
	  | None ->
	      (* Not partitioned, no filesystem, is it a PV? *)
	      let pv = probe_for_pv dev in
	      match pv with
	      | Some lvm_name ->
		  { disk with d_content = `PhysicalVolume lvm_name }
	      | None ->
		  disk (* Spare/unknown. *)
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
	      let fs = probe_for_filesystem p.part_dev in
	      match fs with
	      | Some fs ->
		  { p with part_content = `Filesystem fs }
	      | None ->
		  (* Is it a PV? *)
		  let pv = probe_for_pv p.part_dev in
		  match pv with
		  | Some lvm_name ->
		      { p with part_content = `PhysicalVolume lvm_name }
		  | None ->
		      p (* Spare/unknown. *)
	    ) else p
	) parts.parts in
	let parts = { parts with parts = ps } in
	{ disk with d_content = `Partitions parts }
    | disk -> disk
  ) in

  (* LVM filesystem detection
   *
   * For each domain, look for all disks/partitions which have been
   * identified as PVs and pass those back to the respective LVM
   * plugin for LV detection.
   *
   * (Note - a two-stage process because an LV can be spread over
   * several PVs, so we have to detect all PVs belonging to a
   * domain first).
   *)
  (* First: LV detection. *)
  let doms = List.map (
    fun ({ dom_disks = disks } as dom) ->
      (* Find all physical volumes, can be disks or partitions. *)
      let pvs_on_disks = List.filter_map (
	function
	| { d_dev = d_dev;
	    d_content = `PhysicalVolume pv } -> Some (pv, d_dev)
	| _ -> None
      ) disks in
      let pvs_on_partitions = List.map (
	function
	| { d_content = `Partitions { parts = parts } } ->
	    List.filter_map (
	      function
	      | { part_dev = part_dev;
		  part_content = `PhysicalVolume pv } ->
		    Some (pv, part_dev)
	      | _ -> None
	    ) parts
	| _ -> []
      ) disks in
      let lvs = List.concat (pvs_on_disks :: pvs_on_partitions) in
      dom, lvs
  ) doms in

  (* Second: filesystem on LV detection. *)
  let doms = List.map (
    fun (dom, lvs) ->
      (* Group the LVs by plug-in type. *)
      let cmp (a,_) (b,_) = compare a b in
      let lvs = List.sort ~cmp lvs in
      let lvs = group_by lvs in

      let lvs =
	List.map (fun (pv, devs) -> list_lvs pv.lvm_plugin_id devs)
	  lvs in
      let lvs = List.concat lvs in

      (* lvs is a list of potential LV devices.  Now run them through the
       * probes to see if any contain filesystems.
       *)
      let filesystems =
	List.filter_map (
	  fun { lv_dev = dev } -> probe_for_filesystem dev
	) lvs in

      { dom with dom_lv_filesystems = filesystems }
  ) doms in

  (* Now print the results.
   *
   * Print the title.
   *)
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
  let iter_over_filesystems doms
      (f : domain -> ?disk:disk -> ?part:(partition * int) -> filesystem ->
	unit) =
    List.iter (
      fun ({ dom_disks = disks; dom_lv_filesystems = filesystems } as dom) ->
	(* Ordinary filesystems found on disks & partitions. *)
	List.iter (
	  function
	  | ({ d_content = `Filesystem fs } as disk) ->
	      f dom ~disk fs
	  | ({ d_content = `Partitions partitions } as disk) ->
	      List.iteri (
		fun i ->
		  function
		  | ({ part_content = `Filesystem fs } as part) ->
		      f dom ~disk ~part:(part, i) fs
		  | _ -> ()
	      ) partitions.parts
	  | _ -> ()
	) disks;
	(* LV filesystems. *)
	List.iter (fun fs -> f dom fs) filesystems
    ) doms
  in

  (* Print stats for each recognized filesystem. *)
  let print_stats dom ?disk ?part fs =
    (* Printable name is like "domain:hda" or "domain:hda1". *)
    let name =
      let dom_name = dom.dom_name in
      let disk_name =
	match disk with
	| None -> "???" (* XXX keep LV dev around *)
	| Some disk -> disk.d_target
      in
      match part with
      | None ->
	  dom_name ^ ":" ^ disk_name
      | Some (_, pnum) ->
	  dom_name ^ ":" ^ disk_name ^ string_of_int pnum in
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
