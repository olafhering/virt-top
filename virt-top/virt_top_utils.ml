(* 'top'-like tool for libvirt domains.
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
*)

let (//) = Filename.concat

(* Input a whole file as a list of lines. *)
let input_all_lines chan =
  let lines = ref [] in
  (try
     while true; do
       lines := input_line chan :: !lines
     done
   with
     End_of_file -> ());
  List.rev !lines

(* Trim whitespace from the beginning and end of strings. *)
let isspace c =
  c = ' '
  (* || c = '\f' *) || c = '\n' || c = '\r' || c = '\t' (* || c = '\v' *)

let triml ?(test = isspace) str =
  let i = ref 0 in
  let n = ref (String.length str) in
  while !n > 0 && test str.[!i]; do
    decr n;
    incr i
  done;
  if !i = 0 then str
  else String.sub str !i !n

let trimr ?(test = isspace) str =
  let n = ref (String.length str) in
  while !n > 0 && test str.[!n-1]; do
    decr n
  done;
  if !n = String.length str then str
  else String.sub str 0 !n

let trim ?(test = isspace) str =
  trimr (triml str)

(* Read a configuration file as a list of (key, value) pairs.
 * If the config file is missing this returns an empty list.
 *)
let blanks_and_comments = Str.regexp "^[ \t]*\\(#.*\\)?$"

let read_config_file filename =
  let lines =
    try
      let chan = open_in filename in
      let lines = input_all_lines chan in
      close_in chan;
      lines
    with
      Sys_error _ -> [] in	     (* Ignore errors opening file. *)

  (* Line numbers. *)
  let lines =
    let i = ref 0 in List.map (fun line -> (incr i; !i), line) lines in

  (* Remove blank lines and comment lines. *)
  let lines =
    List.filter
      (fun (lineno, line) ->
	 not (Str.string_match blanks_and_comments line 0)) lines in

  (* Convert to key, value pairs. *)
  List.map (
    fun (lineno, line) ->
      let key, value = ExtString.String.split line " " in
      lineno, trim key, trim value
  ) lines
