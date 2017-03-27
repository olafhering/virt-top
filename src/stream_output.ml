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

(* [--stream] mode output functions. *)

open Printf
open ExtList

open Utils
open Collect

module C = Libvirt.Connect
module D = Libvirt.Domain

let append_stream (_, _, _, _, _, node_info, hostname, _) (* setup *)
                  block_in_bytes
                  { rd_doms = doms;
                    rd_printable_time = printable_time;
                    rd_nr_pcpus = nr_pcpus; rd_total_cpu = total_cpu;
                    rd_totals = totals } (* state *) =
  (* Header for this iteration *)
  printf "virt-top time  %s Host %s %s %d/%dCPU %dMHz %LdMB \n"
    printable_time hostname node_info.C.model node_info.C.cpus nr_pcpus
    node_info.C.mhz (node_info.C.memory /^ 1024L);
  (* dump domain information one by one *)
   let rd, wr = if block_in_bytes then "RDBY", "WRBY" else "RDRQ", "WRRQ"
   in
     printf "   ID S %s %s RXBY TXBY %%CPU %%MEM   TIME    NAME\n" rd wr;

  (* sort by ID *)
  let doms =
    let compare =
      (function
       | Active {rd_domid = id1 }, Active {rd_domid = id2} ->
           compare id1 id2
       | Active _, Inactive -> -1
       | Inactive, Active _ -> 1
       | Inactive, Inactive -> 0)
    in
    let cmp  (name1, dom1) (name2, dom2) = compare(dom1, dom2) in
    List.sort ~cmp doms in
  (*Print domains *)
  let dump_domain = fun name rd
  -> begin
    let state = Screen.show_state rd.rd_info.D.state in
         let rd_req = if rd.rd_block_rd_info = None then "   0"
                      else Show.int64_option rd.rd_block_rd_info in
         let wr_req = if rd.rd_block_wr_info = None then "   0"
                      else Show.int64_option rd.rd_block_wr_info in
    let rx_bytes = if rd.rd_net_rx_bytes = None then "   0"
    else Show.int64_option rd.rd_net_rx_bytes in
    let tx_bytes = if rd.rd_net_tx_bytes = None then "   0"
    else Show.int64_option rd.rd_net_tx_bytes in
    let percent_cpu = Show.percent rd.rd_percent_cpu in
    let percent_mem = Int64.to_float rd.rd_mem_percent in
    let percent_mem = Show.percent percent_mem in
    let time = Show.time rd.rd_info.D.cpu_time in
    printf "%5d %c %s %s %s %s %s %s %s %s\n"
      rd.rd_domid state rd_req wr_req rx_bytes tx_bytes
      percent_cpu percent_mem time name;
  end
  in
  List.iter (
    function
    | name, Active dom -> dump_domain name dom
    | name, Inactive -> ()
  ) doms;
  flush stdout
