(** 'df' command for virtual domains. *)
(* (C) Copyright 2007-2008 Richard W.M. Jones, Red Hat Inc.
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

(** This module (Virt_df) contains functions and values which are
    used throughout the plug-ins and main code.
*)

val debug : bool
(** If true, emit logs of debugging information to stderr. *)

val ( +* ) : int32 -> int32 -> int32
val ( -* ) : int32 -> int32 -> int32
val ( ** ) : int32 -> int32 -> int32
val ( /* ) : int32 -> int32 -> int32
val ( +^ ) : int64 -> int64 -> int64
val ( -^ ) : int64 -> int64 -> int64
val ( *^ ) : int64 -> int64 -> int64
val ( /^ ) : int64 -> int64 -> int64
(** int32 and int64 infix operators for convenience. *)

val uri : string option ref		(** Hypervisor/libvirt URI. *)
val inodes : bool ref			(** Display inodes. *)
val human : bool ref			(** Display human-readable. *)
val all : bool ref			(** Show all or just active domains. *)
val test_files : string list ref	(** In test mode (-t) list of files. *)
(** State of command line arguments. *)

(**
   {2 Domain/device model}

   The "domain/device model" that we currently understand looks
   like this:

{v
domains
  |
  \--- host partitions / disk image files
         ||
       guest block devices
         |
         +--> guest partitions (eg. using MBR)
         |      |
         \-(1)->+--- filesystems (eg. ext3)
                |
                \--- PVs for LVM
                       |||
                     VGs and LVs
v}
    
   (1) Filesystems and PVs may also appear directly on guest
   block devices.
    
   Partition schemes (eg. MBR) and filesystems register themselves
   with this main module and they are queried first to get an idea
   of the physical devices, partitions and filesystems potentially
   available to the guest.
    
   Volume management schemes (eg. LVM2) register themselves here
   and are called later with "spare" physical devices and partitions
   to see if they contain LVM data.  If this results in additional
   logical volumes then these are checked for filesystems.
    
   Swap space is considered to be a dumb filesystem for the purposes
   of this discussion.
*)

class virtual device :
  object
    method virtual name : string
    method virtual read : int64 -> int -> string
    method read_bitstring : int64 -> int -> string * int * int
    method virtual size : int64
  end
  (**
     A virtual (or physical!) device, encapsulating any translation
     that has to be done to access the device.  eg. For partitions
     there is a simple offset, but for LVM you may need complicated
     table lookups.
    
     We keep the underlying file descriptors open for the duration
     of the program.  There aren't likely to be many of them, and
     the program is short-lived, and it's easier than trying to
     track which device is using what fd.  As a result, there is no
     need for any close/deallocation function.
    
     Note the very rare use of OOP in OCaml!
  *)

class block_device :
  string ->
  object
    method name : string
    method read : int64 -> int -> string
    method read_bitstring : int64 -> int -> string * int * int
    method size : int64
  end
    (** A concrete device which just direct-maps a file or /dev device. *)

val null_device : device
    (** The null device.  Any attempt to read generates an error. *)

type domain = {
  dom_name : string;			(** Domain name. *)
  dom_id : int option;			(** Domain ID (if running). *)
  dom_disks : disk list;		(** Domain disks. *)
  dom_lv_filesystems : filesystem list;	(** Domain LV filesystems. *)
}
and disk = {
  d_type : string option;		(** The <disk type=...> *)
  d_device : string;			(** The <disk device=...> (eg "disk") *)
  d_source : string;			(** The <source file=... or dev> *)
  d_target : string;			(** The <target dev=...> (eg "hda") *)
  d_dev : device;			(** Disk device. *)
  d_content : disk_content;		(** What's on it. *)
}
and disk_content =
    [ `Filesystem of filesystem		(** Contains a direct filesystem. *)
    | `Partitions of partitions		(** Contains partitions. *)
    | `PhysicalVolume of pv		(** Contains an LVM PV. *)
    | `Unknown				(** Not probed or unknown. *)
    ]
and partitions = {
  parts_name : string;			(** Name of partitioning scheme. *)
  parts : partition list;		(** Partitions. *)
}
and partition = {
  part_status : partition_status;	(** Bootable, etc. *)
  part_type : int;			(** Partition filesystem type. *)
  part_dev : device;			(** Partition device. *)
  part_content : partition_content;	(** What's on it. *)
}
and partition_status = Bootable | Nonbootable | Malformed | NullEntry
and partition_content =
    [ `Filesystem of filesystem		(** Filesystem. *)
    | `PhysicalVolume of pv		(** Contains an LVM PV. *)
    | `Unknown				(** Not probed or unknown. *)
    ]
and filesystem = {
  fs_name : string;			(** Name of filesystem. *)
  fs_block_size : int64;		(** Block size (bytes). *)
  fs_blocks_total : int64;		(** Total blocks. *)
  fs_is_swap : bool;			(** If swap, following not valid. *)
  fs_blocks_reserved : int64;		(** Blocks reserved for super-user. *)
  fs_blocks_avail : int64;		(** Blocks free (available). *)
  fs_blocks_used : int64;		(** Blocks in use. *)
  fs_inodes_total : int64;		(** Total inodes. *)
  fs_inodes_reserved : int64;		(** Inodes reserved for super-user. *)
  fs_inodes_avail : int64;		(** Inodes free (available). *)
  fs_inodes_used : int64;		(** Inodes in use. *)
}
and pv = {
  lvm_plugin_id : lvm_plugin_id;        (** The LVM plug-in which detected
					    this. *)
  pv_uuid : string;			(** UUID. *)
}
and lv = {
  lv_dev : device;			(** Logical volume device. *)
}

and lvm_plugin_id

val string_of_partition : partition -> string
val string_of_filesystem : filesystem -> string
(** Convert a partition or filesystem struct to a string (for debugging). *)

(** {2 Plug-in registration functions} *)

val partition_type_register : string -> (device -> partitions) -> unit
(** Register a partition probing plug-in. *)

val probe_for_partitions : device -> partitions option
(** Do a partition probe on a device.  Returns [Some partitions] or [None]. *)

val filesystem_type_register : string -> (device -> filesystem) -> unit
(** Register a filesystem probing plug-in. *)

val probe_for_filesystem : device -> filesystem option
(** Do a filesystem probe on a device.  Returns [Some filesystem] or [None]. *)

val lvm_type_register :
  string -> (lvm_plugin_id -> device -> pv) -> (device list -> lv list) -> unit
(** [lvm_type_register lvm_name probe_fn list_lvs_fn]
    registers a new LVM type.  [probe_fn] is a function which
    should probe a device to find out if it contains a PV.
    [list_lvs_fn] is a function which should take a list of
    devices (PVs) and construct a list of LV devices.
*)

val probe_for_pv : device -> pv option
(** Do a PV probe on a device.  Returns [Some pv] or [None]. *)

val list_lvs : lvm_plugin_id -> device list -> lv list
(** Construct LV devices from a list of PVs. *)

(** {2 Utility functions} *)

val group_by : ?cmp:('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b list) list
(** Group a sorted list of pairs by the first element of the pair. *)

val range : int -> int -> int list
(** [range a b] returns the list of integers [a <= i < b].
    If [a >= b] then the empty list is returned.
*)
