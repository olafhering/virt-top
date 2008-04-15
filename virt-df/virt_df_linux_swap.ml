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

   Support for Linux swap partitions.
*)

open Virt_df_gettext.Gettext
open Virt_df

let probe_swap dev =
  (* Load the "superblock" (ie. first 0x1000 bytes). *)
  let bits = dev#read_bitstring 0L 0x1000 in

  bitmatch bits with
    (* Actually this isn't just padding. *)
  | padding : 8*0x1000 - 10*8 : bitstring;
    magic : 10*8 : bitstring
      when Bitmatch.string_of_bitstring magic = "SWAPSPACE2" ->
    {
      fs_name = s_ "Linux swap";
      fs_block_size = 4096L;		(* XXX *)
      fs_blocks_total = dev#size /^ 4096L;

      (* The remaining fields are ignored when fs_is_swap is true. *)
      fs_is_swap = true;
      fs_blocks_reserved = 0L;
      fs_blocks_avail = 0L;
      fs_blocks_used = 0L;
      fs_inodes_total = 0L;
      fs_inodes_reserved = 0L;
      fs_inodes_avail = 0L;
      fs_inodes_used = 0L;
    }
  | _ ->
      raise Not_found			(* Not Linux swapspace. *)

(* Register with main code. *)
let () = filesystem_type_register "linux_swap" probe_swap
