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

let debug = true     (* If true emit lots of debugging information. *)

let ( +* ) = Int32.add
let ( -* ) = Int32.sub
let ( ** ) = Int32.mul
let ( /* ) = Int32.div

let ( +^ ) = Int64.add
let ( -^ ) = Int64.sub
let ( *^ ) = Int64.mul
let ( /^ ) = Int64.div

let uri = ref None
let inodes = ref false
let human = ref false
let all = ref false
let test_files = ref []

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

(* The null device.  Any attempt to read generates an error. *)
let null_device : device =
object
  inherit device
  method read _ _ = assert false
  method size = 0L
  method name = "null"
end

type domain = {
  dom_name : string;			(* Domain name. *)
  dom_id : int option;			(* Domain ID (if running). *)
  dom_disks : disk list;		(* Domain disks. *)
  dom_lv_filesystems : filesystem list;	(* Domain LV filesystems. *)
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
  | `PhysicalVolume of string		(* Contains an LVM PV. *)
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
  | `PhysicalVolume of string		(* Contains an LVM PV. *)
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

(* Probe a device for a filesystem.  Returns [Some fs] or [None]. *)
let probe_for_filesystem dev =
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
let lvm_types = ref []
let lvm_type_register (lvm_name : string) probe_fn list_lvs_fn =
  lvm_types := (lvm_name, (probe_fn, list_lvs_fn)) :: !lvm_types

(* Probe a device for a PV.  Returns [Some lvm_name] or [None]. *)
let probe_for_pv dev =
  if debug then eprintf "probing if %s is a PV ...\n%!" dev#name;
  let rec loop = function
    | [] -> None
    | (lvm_name, (probe_fn, _)) :: rest ->
	if probe_fn dev then Some lvm_name else loop rest
  in
  let r = loop !lvm_types in
  if debug then (
    match r with
    | None -> eprintf "no PV found on %s\n%!" dev#name
    | Some lvm_name ->
	eprintf "%s contains a %s PV\n%!" dev#name lvm_name
  );
  r

let list_lvs lvm_name devs =
  let _, list_lvs_fn = List.assoc lvm_name !lvm_types in
  list_lvs_fn devs

(*----------------------------------------------------------------------*)

(* This version by Isaac Trotts. *)
let group_by ?(cmp = Pervasives.compare) ls =
  let ls' =
    List.fold_left
      (fun acc (day1, x1) ->
         match acc with
             [] -> [day1, [x1]]
           | (day2, ls2) :: acctl ->
               if cmp day1 day2 = 0
               then (day1, x1 :: ls2) :: acctl
               else (day1, [x1]) :: acc)
      []
      ls
  in
  let ls' = List.rev ls' in
  List.map (fun (x, xs) -> x, List.rev xs) ls'
