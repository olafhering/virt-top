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

   Support for Master Boot Record partition scheme.
*)

open Printf
open Unix
open ExtList

open Virt_df_gettext.Gettext
open Virt_df

let sector_size = 512
let sector_size64 = 512L

(* Maximum number of extended partitions possible. *)
let max_extended_partitions = 100

(* Device representing a single partition.  It just acts as an offset
 * into the underlying device.
 *
 * Notes:
 * (1) 'start'/'size' are measured in sectors.
 * (2) 'partno' is the partition number, starting at 1
 *     (cf. /dev/hda1 is the first partition).
 * (3) 'dev' is the underlying block device.
 *)
class partition_device partno start size dev =
  let devname = dev#name in
  let name = sprintf "%s%d" devname partno in
  let start = start *^ sector_size64 in
  let size = size *^ sector_size64 in
object (self)
  inherit offset_device name start size dev
end

(** Probe the
    {{:http://en.wikipedia.org/wiki/Master_boot_record}master boot record}
    (if it is one) and read the partitions.

    @raise Not_found if it is not an MBR.
 *)
let rec probe_mbr dev =
  (* Adjust size to sectors. *)
  let size = dev#size /^ sector_size64 in

  (* Read the first sector. *)
  let bits =
    try dev#read_bitstring 0L sector_size
    with exn -> raise Not_found in

  (* Does this match a likely-looking MBR? *)
  bitmatch bits with
  | padding : 3568 : bitstring;		(* padding to byte offset 446 *)
    part0 : 128 : bitstring;		(* partitions *)
    part1 : 128 : bitstring;
    part2 : 128 : bitstring;
    part3 : 128 : bitstring;
    0x55 : 8; 0xAA : 8 ->		(* MBR signature *)

      (* Parse the partition table entries. *)
      let primaries =
	List.mapi (parse_mbr_entry dev) [part0;part1;part2;part3] in

(*
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
*)
      { parts_name = "MBR"; parts = primaries }

  | _ ->
      raise Not_found			(* not an MBR *)

(* Parse a single partition table entry.  See the table here:
 * http://en.wikipedia.org/wiki/Master_boot_record
 *)
and parse_mbr_entry dev i bits =
  bitmatch bits with
  | 0l : 32; 0l : 32; 0l : 32; 0l : 32 ->
    { part_status = NullEntry; part_type = 0;
      part_dev = null_device; part_content = `Unknown }

  | 0 : 8; first_chs : 24;
    part_type : 8; last_chs : 24;
    first_lba : 32 : unsigned, littleendian;
    part_size : 32 : unsigned, littleendian ->
    make_mbr_entry Nonbootable dev (i+1) part_type first_lba part_size

  | 0x80 : 8; first_chs : 24;
    part_type : 8; last_chs : 24;
    first_lba : 32 : unsigned, littleendian;
    part_size : 32 : unsigned, littleendian ->
    make_mbr_entry Bootable dev (i+1) part_type first_lba part_size

  | _ ->
      { part_status = Malformed; part_type = 0;
	part_dev = null_device; part_content = `Unknown }

and make_mbr_entry part_status dev partno part_type first_lba part_size =
  let first_lba = uint64_of_int32 first_lba in
  let part_size = uint64_of_int32 part_size in
  if !debug then
    eprintf "make_mbr_entry: first_lba = %Lx part_size = %Lx\n%!"
      first_lba part_size;
  { part_status = part_status;
    part_type = part_type;
    part_dev = new partition_device partno first_lba part_size dev;
    part_content = `Unknown }

(*
This code worked previously, but now needs some love ...
XXX

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
	failwith (s_ "error reading extended partition")
      else (
	(* Extract partitions from the data. *)
	let part1, part2 =
	  match List.map (get_partition str) [ 0; 16 ] with
	  | [p1;p2] -> p1,p2
	  | _ -> failwith (s_ "probe_extended_partition: internal error") in
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
*)

(* Ugh, fake a UInt32 -> UInt64 conversion without sign extension, until
 * we get working UInt32/UInt64 modules in extlib.
 *)
and uint64_of_int32 u32 =
  let i64 = Int64.of_int32 u32 in
  if u32 >= 0l then i64
  else Int64.add i64 0x1_0000_0000_L

(* Register with main code. *)
let () = partition_type_register "MBR" probe_mbr
