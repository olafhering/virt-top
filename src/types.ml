(* 'top'-like tool for libvirt domains.
   (C) Copyright 2007-2017 Richard W.M. Jones, Red Hat Inc.
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

open Opt_gettext.Gettext
open Utils

module C = Libvirt.Connect
module D = Libvirt.Domain

(* XXX We should get rid of this type. *)
type setup =
    Libvirt.ro C.t              (* connection *)
    * bool * bool * bool * bool (* batch, script, csv, stream mode *)
    * C.node_info		(* node_info *)
    * string                    (* hostname *)
    * (int * int * int)         (* libvirt version *)

(* Sort order. *)
type sort_order =
  | DomainID | DomainName | Processor | Memory | Time
  | NetRX | NetTX | BlockRdRq | BlockWrRq
let all_sort_fields = [
  DomainID; DomainName; Processor; Memory; Time;
  NetRX; NetTX; BlockRdRq; BlockWrRq
]
let printable_sort_order = function
  | Processor -> s_"%CPU"
  | Memory -> s_"%MEM"
  | Time -> s_"TIME (CPU time)"
  | DomainID -> s_"Domain ID"
  | DomainName -> s_"Domain name"
  | NetRX -> s_"Net RX bytes"
  | NetTX -> s_"Net TX bytes"
  | BlockRdRq -> s_"Block read reqs"
  | BlockWrRq -> s_"Block write reqs"
let sort_order_of_cli = function
  | "cpu" | "processor" -> Processor
  | "mem" | "memory" -> Memory
  | "time" -> Time
  | "id" -> DomainID
  | "name" -> DomainName
  | "netrx" -> NetRX | "nettx" -> NetTX
  | "blockrdrq" -> BlockRdRq | "blockwrrq" -> BlockWrRq
  | str ->
      failwithf (f_"%s: sort order should be: %s")
	str "cpu|mem|time|id|name|netrx|nettx|blockrdrq|blockwrrq"
let cli_of_sort_order = function
  | Processor -> "cpu"
  | Memory -> "mem"
  | Time -> "time"
  | DomainID -> "id"
  | DomainName -> "name"
  | NetRX -> "netrx"
  | NetTX -> "nettx"
  | BlockRdRq -> "blockrdrq"
  | BlockWrRq -> "blockwrrq"

(* Current major display mode: TaskDisplay is the normal display. *)
type display = TaskDisplay | PCPUDisplay | BlockDisplay | NetDisplay

let display_of_cli = function
  | "task" -> TaskDisplay
  | "pcpu" -> PCPUDisplay
  | "block" -> BlockDisplay
  | "net" -> NetDisplay
  | str ->
      failwithf (f_"%s: display should be %s") str "task|pcpu|block|net"
let cli_of_display = function
  | TaskDisplay -> "task"
  | PCPUDisplay -> "pcpu"
  | BlockDisplay -> "block"
  | NetDisplay -> "net"

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
