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

(* CSV output functions. *)

open Printf
open ExtList

open Collect

module C = Libvirt.Connect

(* Hook for CSV support (see [opt_csv.ml]). *)
let csv_write : (string list -> unit) ref =
  ref (
    fun _ -> ()
  )

(* Write CSV header row. *)
let write_csv_header (csv_cpu, csv_mem, csv_block, csv_net) block_in_bytes =
  (!csv_write) (
    [ "Hostname"; "Time"; "Arch"; "Physical CPUs";
      "Count"; "Running"; "Blocked"; "Paused"; "Shutdown";
      "Shutoff"; "Crashed"; "Active"; "Inactive";
      "%CPU";
      "Total hardware memory (KB)";
      "Total memory (KB)"; "Total guest memory (KB)";
      "Total CPU time (ns)" ] @
      (* These fields are repeated for each domain: *)
    [ "Domain ID"; "Domain name"; ] @
    (if csv_cpu then [ "CPU (ns)"; "%CPU"; ] else []) @
    (if csv_mem then [ "Mem (bytes)"; "%Mem";] else []) @
    (if csv_block && not block_in_bytes
       then [ "Block RDRQ"; "Block WRRQ"; ] else []) @
    (if csv_block && block_in_bytes
       then [ "Block RDBY"; "Block WRBY"; ] else []) @
    (if csv_net then [ "Net RXBY"; "Net TXBY" ] else [])
  )

(* Write summary data to CSV file. *)
let append_csv (_, _, _, _, _, node_info, hostname, _) (* setup *)
               (csv_cpu, csv_mem, csv_block, csv_net)
               { rd_doms = doms;
                 rd_printable_time = printable_time;
                 rd_nr_pcpus = nr_pcpus; rd_total_cpu = total_cpu;
                 rd_totals = totals } (* state *) =
  (* The totals / summary fields. *)
  let (count, running, blocked, paused, shutdown, shutoff,
       crashed, active, inactive,
       total_cpu_time, total_memory, total_domU_memory) = totals in

  let percent_cpu = 100. *. total_cpu_time /. total_cpu in

  let summary_fields = [
    hostname; printable_time; node_info.C.model; string_of_int nr_pcpus;
    string_of_int count; string_of_int running; string_of_int blocked;
    string_of_int paused; string_of_int shutdown; string_of_int shutoff;
    string_of_int crashed; string_of_int active; string_of_int inactive;
    sprintf "%2.1f" percent_cpu;
    Int64.to_string node_info.C.memory;
    Int64.to_string total_memory; Int64.to_string total_domU_memory;
    Int64.to_string (Int64.of_float total_cpu_time)
  ] in

  (* The domains.
   *
   * Sort them by ID so that the list of relatively stable.  Ignore
   * inactive domains.
   *)
  let doms = List.filter_map (
    function
    | _, Inactive -> None		(* Ignore inactive domains. *)
    | name, Active rd -> Some (name, rd)
  ) doms in
  let cmp (_, { rd_domid = rd_domid1 }) (_, { rd_domid = rd_domid2 }) =
    compare rd_domid1 rd_domid2
  in
  let doms = List.sort ~cmp doms in

  let string_of_int64_option = Option.map_default Int64.to_string "" in

  let domain_fields = List.map (
    fun (domname, rd) ->
      [ string_of_int rd.rd_domid; domname ] @
	(if csv_cpu then [
	   string_of_float rd.rd_cpu_time; string_of_float rd.rd_percent_cpu
	 ] else []) @
        (if csv_mem then [
            Int64.to_string rd.rd_mem_bytes; Int64.to_string rd.rd_mem_percent
         ] else []) @
	(if csv_block then [
	   string_of_int64_option rd.rd_block_rd_info;
	   string_of_int64_option rd.rd_block_wr_info;
	 ] else []) @
	(if csv_net then [
	   string_of_int64_option rd.rd_net_rx_bytes;
	   string_of_int64_option rd.rd_net_tx_bytes;
	 ] else [])
  ) doms in
  let domain_fields = List.flatten domain_fields in

  (!csv_write) (summary_fields @ domain_fields)
