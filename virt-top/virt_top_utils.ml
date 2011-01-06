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

open Printf

open Virt_top_gettext.Gettext

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

let (//) = Filename.concat

(* Int64 operators for convenience. *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

(* failwithf is a printf-like version of failwith. *)
let failwithf fs = ksprintf failwith fs

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

(* Pad a string to the full width with spaces.  If too long, truncate. *)
let pad width str =
  if width <= 0 then ""
  else (
    let n = String.length str in
    if n = width then str
    else if n > width then String.sub str 0 width
    else (* if n < width then *) str ^ String.make (width-n) ' '
  )

module Show = struct
  (* Show a percentage in 4 chars. *)
  let percent percent =
    if percent <= 0. then " 0.0"
    else if percent <= 9.9 then sprintf " %1.1f" percent
    else if percent <= 99.9 then sprintf "%2.1f" percent
    else "100 "

  (* Show an int64 option in 4 chars. *)
  let rec int64_option = function
    | None -> "    "
    | Some n -> int64 n
  (* Show an int64 in 4 chars. *)
  and int64 = function
    | n when n < 0L -> "-!!!"
    | n when n <= 9999L ->
	sprintf "%4Ld" n
    | n when n /^ 1024L <= 999L ->
	sprintf "%3LdK" (n /^ 1024L)
    | n when n /^ 1_048_576L <= 999L ->
	sprintf "%3LdM" (n /^ 1_048_576L)
    | n when n /^ 1_073_741_824L <= 999L ->
	sprintf "%3LdG" (n /^ 1_073_741_824L)
    | _ -> ">!!!"

  (* Format the total time (may be large!) in 9 chars. *)
  let time ns =
    let secs_in_ns = 1_000_000_000L in
    let mins_in_ns = 60_000_000_000L in
    let hours_in_ns = 3_600_000_000_000L in

    let hours = ns /^ hours_in_ns in
    let ns = ns -^ (hours *^ hours_in_ns) in
    let mins = ns /^ mins_in_ns in
    let ns = ns -^ (mins *^ mins_in_ns) in
    let secs = ns /^ secs_in_ns in
    let ns = ns -^ (secs *^ secs_in_ns) in
    let pennies = ns /^ 10_000_000L in

    if hours < 12L then
      sprintf "%3Ld:%02Ld.%02Ld" (hours *^ 60L +^ mins) secs pennies
    else if hours <= 999L then
      sprintf "%3Ld:%02Ld:%02Ld" hours mins secs
    else (
      let days = hours /^ 24L in
      let hours = hours -^ (days *^ 24L) in
      sprintf "%3Ldd%02Ld:%02Ld" days hours mins
    )
end

(* Sum Domain.block_stats structures together.  Missing fields
 * get forced to 0.  Empty list returns all 0.
 *)
let zero_block_stats =
  { D.rd_req = 0L; rd_bytes = 0L; wr_req = 0L; wr_bytes = 0L; errs = 0L }
let add_block_stats bs1 bs2 =
  let add f1 f2 = if f1 >= 0L && f2 >= 0L then f1 +^ f2 else 0L in
  { D.rd_req = add bs1.D.rd_req   bs2.D.rd_req;
    rd_bytes = add bs1.D.rd_bytes bs2.D.rd_bytes;
    wr_req   = add bs1.D.wr_req   bs2.D.wr_req;
    wr_bytes = add bs1.D.wr_bytes bs2.D.wr_bytes;
    errs     = add bs1.D.errs     bs2.D.errs }
let sum_block_stats =
  List.fold_left add_block_stats zero_block_stats

(* Get the difference between two block_stats structures.  Missing data
 * forces the difference to -1.
 *)
let diff_block_stats curr prev =
  let sub f1 f2 = if f1 >= 0L && f2 >= 0L then f1 -^ f2 else -1L in
  { D.rd_req = sub curr.D.rd_req   prev.D.rd_req;
    rd_bytes = sub curr.D.rd_bytes prev.D.rd_bytes;
    wr_req   = sub curr.D.wr_req   prev.D.wr_req;
    wr_bytes = sub curr.D.wr_bytes prev.D.wr_bytes;
    errs     = sub curr.D.errs     prev.D.errs }

(* Sum Domain.interface_stats structures together.  Missing fields
 * get forced to 0.  Empty list returns all 0.
 *)
let zero_interface_stats =
  { D.rx_bytes = 0L; rx_packets = 0L; rx_errs = 0L; rx_drop = 0L;
    tx_bytes = 0L; tx_packets = 0L; tx_errs = 0L; tx_drop = 0L }
let add_interface_stats is1 is2 =
  let add f1 f2 = if f1 >= 0L && f2 >= 0L then f1 +^ f2 else 0L in
  { D.rx_bytes = add is1.D.rx_bytes   is2.D.rx_bytes;
    rx_packets = add is1.D.rx_packets is2.D.rx_packets;
    rx_errs    = add is1.D.rx_errs    is2.D.rx_errs;
    rx_drop    = add is1.D.rx_drop    is2.D.rx_drop;
    tx_bytes   = add is1.D.tx_bytes   is2.D.tx_bytes;
    tx_packets = add is1.D.tx_packets is2.D.tx_packets;
    tx_errs    = add is1.D.tx_errs    is2.D.tx_errs;
    tx_drop    = add is1.D.tx_drop    is2.D.tx_drop }
let sum_interface_stats =
  List.fold_left add_interface_stats zero_interface_stats

(* Get the difference between two interface_stats structures.
 * Missing data forces the difference to -1.
 *)
let diff_interface_stats curr prev =
  let sub f1 f2 = if f1 >= 0L && f2 >= 0L then f1 -^ f2 else -1L in
  { D.rx_bytes = sub curr.D.rx_bytes   prev.D.rx_bytes;
    rx_packets = sub curr.D.rx_packets prev.D.rx_packets;
    rx_errs    = sub curr.D.rx_errs    prev.D.rx_errs;
    rx_drop    = sub curr.D.rx_drop    prev.D.rx_drop;
    tx_bytes   = sub curr.D.tx_bytes   prev.D.tx_bytes;
    tx_packets = sub curr.D.tx_packets prev.D.tx_packets;
    tx_errs    = sub curr.D.tx_errs    prev.D.tx_errs;
    tx_drop    = sub curr.D.tx_drop    prev.D.tx_drop }
