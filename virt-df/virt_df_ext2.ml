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

   Support for EXT2/EXT3 filesystems.
*)

open Unix
open Printf
open Virt_df_gettext.Gettext

(* Int64 operators for convenience. *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

let sector_size = Virt_df.sector_size
let read_int32_le = Virt_df.read_int32_le

let probe_ext2 target part_type fd start size =
  LargeFile.lseek fd ((start+^2L) *^ sector_size) SEEK_SET;
  let str = String.create 128 in
  if read fd str 0 128 <> 128 then
    failwith (s_ "error reading ext2/ext3 magic")
  else (
    if str.[56] != '\x53' || str.[57] != '\xEF' then (
      Virt_df.ProbeFailed (s_ "partition marked EXT2/3 but no valid filesystem")
    ) else (
      (* Refer to <linux/ext2_fs.h> *)
      let s_inodes_count = read_int32_le str 0 in
      let s_blocks_count = read_int32_le str 4 in
      let s_r_blocks_count = read_int32_le str 8 in
      let s_free_blocks_count = read_int32_le str 12 in
      let s_free_inodes_count = read_int32_le str 16 in
      let s_first_data_block = read_int32_le str 20 in
      let s_log_block_size = read_int32_le str 24 in
      (*let s_log_frag_size = read_int32_le str 28 in*)
      let s_blocks_per_group = read_int32_le str 32 in

      (* Work out the block size in bytes. *)
      let s_log_block_size = Int64.to_int s_log_block_size in
      let block_size = 1024L in
      let block_size = Int64.shift_left block_size s_log_block_size in

      (* Number of groups. *)
      let s_groups_count =
	(s_blocks_count -^ s_first_data_block -^ 1L)
	/^ s_blocks_per_group +^ 1L in

(*
      (* Number of group descriptors per block. *)
      let s_inodes_per_block = s_blocksize / 
	let s_desc_per_block = block_size / s_inodes_per_block in
	let db_count =
	  (s_groups_count +^ s_desc_per_block -^ 1L)
	  /^ s_desc_per_block
*)

      (* Calculate the block overhead (used by superblocks, inodes, etc.)
       * See fs/ext2/super.c.
       *)
      let overhead = s_first_data_block in
      let overhead = (* XXX *) overhead in


      Virt_df.Filesystem {
	Virt_df.fs_name = s_ "Linux ext2/3";
	fs_block_size = block_size;
	fs_blocks_total = s_blocks_count -^ overhead;
	fs_blocks_reserved = s_r_blocks_count;
	fs_blocks_avail = s_free_blocks_count;
	fs_blocks_used = s_blocks_count -^ overhead -^ s_free_blocks_count;
	fs_inodes_total = s_inodes_count;
	fs_inodes_reserved = 0L;	(* XXX? *)
	fs_inodes_avail = s_free_inodes_count;
	fs_inodes_used = s_inodes_count (*-^ 0L*) -^ s_free_inodes_count;
      }
    )
  )

(* Register with main code. *)
let () =
  Virt_df.fs_register
    [ 0x83 ]				(* Partition type. *)
    probe_ext2
