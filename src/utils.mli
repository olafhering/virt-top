(* 'top'-like tool for libvirt domains.
   (C) Copyright 2007-2009 Richard W.M. Jones, Red Hat Inc.
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

   This file contains utility functions.
*)

(* Filename concatenation. *)
val (//) : string -> string -> string

(* failwithf is a printf-like version of failwith. *)
val failwithf : ('a, unit, string, 'b) format4 -> 'a

(* Return the list of integers [a..b] (inclusive). *)
val range : int -> int -> int list

(* Read a configuration file as a list of (lineno, key, value) pairs.
 * If the config file is missing this returns an empty list.
 *)
val read_config_file : string -> (int * string * string) list

(* Pad or truncate a string to a fixed width. *)
val pad : int -> string -> string

(* Int64 operators for convenience. *)
val (+^) : int64 -> int64 -> int64
val (-^) : int64 -> int64 -> int64
val ( *^ ) : int64 -> int64 -> int64
val (/^) : int64 -> int64 -> int64

(* Utility functions for formating numbers as short strings. *)
module Show : sig
  val percent : float -> string
  val int64_option : int64 option -> string
  val int64 : int64 -> string
  val time : int64 -> string
end
