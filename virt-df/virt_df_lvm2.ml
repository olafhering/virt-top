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
open Virt_df_gettext.Gettext

(* Int64 operators for convenience. *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

let probe_lvm2 target part_type fd start size =
  Virt_df.ProbeFailed (s_ "LVM2 not supported yet")

(* Register with main code. *)
let () =
  Virt_df.fs_register
    [ 0x8e ]				(* Partition type. *)
    probe_lvm2
