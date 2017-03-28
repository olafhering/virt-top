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

(* Hook for [Opt_xml] to override (if present). *)
val parse_device_xml :
  (int -> [ `R ] Libvirt.Domain.t -> string list * string list) ref

(* Intermediate "domain + stats" structure that we use to collect
 * everything we know about a domain within the collect function.
 *)
type rd_domain = Inactive | Active of rd_active
and rd_active = {
  rd_domid : int;			(* Domain ID. *)
  rd_domuuid : Libvirt.uuid;            (* Domain UUID. *)
  rd_dom : [`R] Libvirt.Domain.t;       (* Domain object. *)
  rd_info : Libvirt.Domain.info;        (* Domain CPU info now. *)
  rd_block_stats : (string * Libvirt.Domain.block_stats) list;
                                        (* Domain block stats now. *)
  rd_interface_stats : (string * Libvirt.Domain.interface_stats) list;
                                        (* Domain net stats now. *)
  rd_prev_info : Libvirt.Domain.info option; (* Domain CPU info previously. *)
  rd_prev_block_stats : (string * Libvirt.Domain.block_stats) list;
                                        (* Domain block stats prev. *)
  rd_prev_interface_stats : (string * Libvirt.Domain.interface_stats) list;
                                        (* Domain interface stats prev. *)
  (* The following are since the last slice, or 0 if cannot be calculated: *)
  rd_cpu_time : float;			(* CPU time used in nanoseconds. *)
  rd_percent_cpu : float;		(* CPU time as percent of total. *)
  rd_mem_bytes : int64;		        (* Memory usage in bytes *)
  rd_mem_percent: int64;		(* Memory usage as percent of total *)
  (* The following are since the last slice, or None if cannot be calc'd: *)
  rd_block_rd_reqs : int64 option;      (* Number of block device read rqs. *)
  rd_block_wr_reqs : int64 option;      (* Number of block device write rqs. *)
  rd_block_rd_bytes : int64 option;     (* Number of bytes block device read *)
  rd_block_wr_bytes : int64 option;     (* Number of bytes block device write *)
  rd_net_rx_bytes : int64 option;	(* Number of bytes received. *)
  rd_net_tx_bytes : int64 option;	(* Number of bytes transmitted. *)
}

type stats = {
  rd_doms : (string * rd_domain) list;  (* List of domains. *)
  rd_time : float;
  rd_printable_time : string;
  rd_nr_pcpus : int;
  rd_total_cpu : float;
  rd_total_cpu_per_pcpu : float;
  rd_totals : (int * int * int * int * int * int * int * int * int * float *
                 int64 * int64);
}

type pcpu_stats = {
  rd_pcpu_doms : (int * string * int *
                  Libvirt.Domain.vcpu_info array * int64 array array *
                  int64 array array * string * int) list;
  rd_pcpu_pcpus : int64 array array array;
  rd_pcpu_pcpus_cpu_time : float array
}

val collect : Types.setup -> stats
(** Collect statistics. *)

val collect_pcpu : stats -> pcpu_stats
(** Used in PCPUDisplay mode only, this returns extra per-PCPU stats. *)

val clear_pcpu_display_data : unit -> unit
(** Clear the cache of pcpu_usages used by PCPUDisplay display_mode
    when we switch back to TaskDisplay mode. *)
