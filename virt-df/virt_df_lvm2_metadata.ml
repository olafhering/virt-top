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

(* Part of the parser for LVM2 metadata. *)

type metadata = metastmt list

and metastmt = string * metavalue

and metavalue =
  | Metadata of metadata		(* name { ... } *)
  | String of string			(* name = "..." *)
  | Int of int64
  | Float of float
  | List of metavalue list		(* name = [...] *)

let rec output_metadata chan md =
  _output_metadata chan "" md

and _output_metadata chan prefix = function
  | [] -> ()
  | (name, value) :: rest ->
      output_string chan prefix;
      output_string chan name;
      output_string chan " = ";
      output_metavalue chan prefix value;
      output_string chan "\n";
      _output_metadata chan prefix rest

and output_metavalue chan prefix = function
  | Metadata md ->
      output_string chan "{\n";
      _output_metadata chan (prefix ^ "  ") md;
      output_string chan prefix;
      output_string chan "}";
  | String str ->
      output_char chan '"';
      output_string chan str;
      output_char chan '"';
  | Int i ->
      output_string chan (Int64.to_string i)
  | Float f ->
      output_string chan (string_of_float f)
  | List [] -> ()
  | List [x] -> output_metavalue chan prefix x
  | List (x :: xs) ->
      output_metavalue chan prefix x;
      output_string chan ", ";
      output_metavalue chan prefix (List xs)
