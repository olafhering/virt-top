(* 'df' command for virtual domains.  -*- text -*-
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

(* Part of the parser for LVM2 metadata. *)

type metadata = metastmt list

and metastmt = string * metavalue

and metavalue =
  | Metadata of metadata		(* name { ... } *)
  | String of string			(* name = "..." *)
  | Int of int64
  | Float of float
  | List of metavalue list		(* name = [...] *)

val output_metadata : out_channel -> metadata -> unit
(** This function prints out the metadata on the selected channel.

    The output format isn't particularly close to the input
    format.  This is just for debugging purposes.
*)
