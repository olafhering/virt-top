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
open Virt_df

let superblock_offset = 1024L

let probe_ext2 (dev : device) =
  (* Load the superblock. *)
  let bits = dev#read_bitstring superblock_offset 1024 in

  (* The structure is straight from /usr/include/linux/ext3_fs.h *)
  bitmatch bits with
  | s_inodes_count : 32 : littleendian;		(* Inodes count *)
    s_blocks_count : 32 : littleendian;		(* Blocks count *)
    s_r_blocks_count : 32 : littleendian;	(* Reserved blocks count *)
    s_free_blocks_count : 32 : littleendian;	(* Free blocks count *)
    s_free_inodes_count : 32 : littleendian;	(* Free inodes count *)
    s_first_data_block : 32 : littleendian;	(* First Data Block *)
    s_log_block_size : 32 : littleendian;	(* Block size *)
    s_log_frag_size : 32 : littleendian;	(* Fragment size *)
    s_blocks_per_group : 32 : littleendian;	(* # Blocks per group *)
    s_frags_per_group : 32 : littleendian;	(* # Fragments per group *)
    s_inodes_per_group : 32 : littleendian;	(* # Inodes per group *)
    s_mtime : 32 : littleendian;		(* Mount time *)
    s_wtime : 32 : littleendian;		(* Write time *)
    s_mnt_count : 16 : littleendian;		(* Mount count *)
    s_max_mnt_count : 16 : littleendian;	(* Maximal mount count *)
    0xef53 : 16 : littleendian;		        (* Magic signature *)
    s_state : 16 : littleendian;		(* File system state *)
    s_errors : 16 : littleendian;		(* Behaviour when detecting errors *)
    s_minor_rev_level : 16 : littleendian;	(* minor revision level *)
    s_lastcheck : 32 : littleendian;		(* time of last check *)
    s_checkinterval : 32 : littleendian;	(* max. time between checks *)
    s_creator_os : 32 : littleendian;		(* OS *)
    s_rev_level : 32 : littleendian;		(* Revision level *)
    s_def_resuid : 16 : littleendian;		(* Default uid for reserved blocks *)
    s_def_resgid : 16 : littleendian;		(* Default gid for reserved blocks *)
    s_first_ino : 32 : littleendian;		(* First non-reserved inode *)
    s_inode_size : 16 : littleendian;		(* size of inode structure *)
    s_block_group_nr : 16 : littleendian;	(* block group # of this superblock *)
    s_feature_compat : 32 : littleendian;	(* compatible feature set *)
    s_feature_incompat : 32 : littleendian;	(* incompatible feature set *)
    s_feature_ro_compat : 32 : littleendian;	(* readonly-compatible feature set *)
    s_uuid : 128 : bitstring;		        (* 128-bit uuid for volume *)
    s_volume_name : 128 : bitstring;	        (* volume name XXX string *)
    s_last_mounted : 512 : bitstring;	        (* directory where last mounted XXX string *)
    s_algorithm_usage_bitmap : 32 : littleendian; (* For compression *)
    s_prealloc_blocks : 8;	                (* Nr of blocks to try to preallocate*)
    s_prealloc_dir_blocks : 8;	                (* Nr to preallocate for dirs *)
    s_reserved_gdt_blocks : 16 : littleendian;	(* Per group desc for online growth *)
    s_journal_uuid : 128 : bitstring;	        (* uuid of journal superblock *)
    s_journal_inum : 32 : littleendian;		(* inode number of journal file *)
    s_journal_dev : 32 : littleendian;		(* device number of journal file *)
    s_last_orphan : 32 : littleendian;		(* start of list of inodes to delete *)
    s_hash_seed0 : 32 : littleendian;		(* HTREE hash seed *)
    s_hash_seed1 : 32 : littleendian;
    s_hash_seed2 : 32 : littleendian;
    s_hash_seed3 : 32 : littleendian;
    s_def_hash_version : 8;	                (* Default hash version to use *)
    s_reserved_char_pad : 8;
    s_reserved_word_pad : 16 : littleendian;
    s_default_mount_opts : 32 : littleendian;
    s_first_meta_bg : 32 : littleendian;	(* First metablock block group *)
    s_reserved : 6080 : bitstring ->            (* Padding to the end of the block *)

   (* Work out the block size in bytes. *)
   let s_log_block_size = Int32.to_int s_log_block_size in
   let block_size = 1024L in
   let block_size = Int64.shift_left block_size s_log_block_size in

   (* Number of groups. *)
   let s_groups_count =
     Int64.of_int32 (
       (s_blocks_count -* s_first_data_block -* 1l)
       /* s_blocks_per_group +* 1l
     ) in

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
   let overhead = Int64.of_int32 s_first_data_block in
   let overhead = (* XXX *) overhead in

   {
     fs_name = s_ "Linux ext2/3";
     fs_block_size = block_size;
     fs_blocks_total = Int64.of_int32 s_blocks_count -^ overhead;
     fs_is_swap = false;
     fs_blocks_reserved = Int64.of_int32 s_r_blocks_count;
     fs_blocks_avail = Int64.of_int32 s_free_blocks_count;
     fs_blocks_used =
       Int64.of_int32 s_blocks_count -^ overhead
       -^ Int64.of_int32 s_free_blocks_count;
     fs_inodes_total = Int64.of_int32 s_inodes_count;
     fs_inodes_reserved = 0L;	(* XXX? *)
     fs_inodes_avail = Int64.of_int32 s_free_inodes_count;
     fs_inodes_used = Int64.of_int32 s_inodes_count
       (*-^ 0L*)
       -^ Int64.of_int32 s_free_inodes_count;
   }

  | _ ->
      raise Not_found			(* Not an EXT2/3 superblock. *)

(* Register with main code. *)
let () = filesystem_type_register "ext2" probe_ext2
