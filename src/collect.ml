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

module C = Libvirt.Connect
module D = Libvirt.Domain

open Printf
open ExtList

open Utils
open Types

(* Hook for XML support (see [opt_xml.ml]). *)
let parse_device_xml : (int -> [>`R] D.t -> string list * string list) ref =
  ref (
    fun _ _ -> [], []
  )

(* Intermediate "domain + stats" structure that we use to collect
 * everything we know about a domain within the collect function.
 *)
type rd_domain = Inactive | Active of rd_active
and rd_active = {
  rd_domid : int;			(* Domain ID. *)
  rd_domuuid : Libvirt.uuid;            (* Domain UUID. *)
  rd_dom : [`R] D.t;			(* Domain object. *)
  rd_info : D.info;			(* Domain CPU info now. *)
  rd_block_stats : (string * D.block_stats) list;
                                        (* Domain block stats now. *)
  rd_interface_stats : (string * D.interface_stats) list;
                                        (* Domain net stats now. *)
  rd_prev_info : D.info option;		(* Domain CPU info previously. *)
  rd_prev_block_stats : (string * D.block_stats) list;
                                        (* Domain block stats prev. *)
  rd_prev_interface_stats : (string * D.interface_stats) list;
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

(* We cache the list of block devices and interfaces for each domain
 * here, so we don't need to reparse the XML each time.
 *)
let devices = Hashtbl.create 13

(* Function to get the list of block devices, network interfaces for
 * a particular domain.  Get it from the devices cache, and if not
 * there then parse the domain XML.
 *)
let get_devices id dom =
  try Hashtbl.find devices id
  with Not_found ->
    let blkdevs, netifs = (!parse_device_xml) id dom in
    Hashtbl.replace devices id (blkdevs, netifs);
    blkdevs, netifs

(* We save the state of domains across redraws here, which allows us
 * to deduce %CPU usage from the running total.
 *)
let last_info = Hashtbl.create 13
let last_time = ref (Unix.gettimeofday ())

(* Save pcpu_usages structures across redraws too (only for pCPU display). *)
let last_pcpu_usages = Hashtbl.create 13

let clear_pcpu_display_data () =
  Hashtbl.clear last_pcpu_usages

(* What to get from virConnectGetAllDomainStats. *)
let what = [
  D.StatsState; D.StatsCpuTotal; D.StatsBalloon; D.StatsVcpu;
  D.StatsInterface; D.StatsBlock
]
(* Which domains to get.  Empty list means return all domains:
 * active, inactive, persistent, transient etc.
 *)
let who = []

let collect (conn, _, _, _, _, node_info, _, _) =
  (* Number of physical CPUs (some may be disabled). *)
  let nr_pcpus = C.maxcpus_of_node_info node_info in

  (* Get the current time. *)
  let time = Unix.gettimeofday () in
  let tm = Unix.localtime time in
  let printable_time =
    sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in

  (* What's the total CPU time elapsed since we were last called? (ns) *)
  let total_cpu_per_pcpu = 1_000_000_000. *. (time -. !last_time) in
  (* Avoid division by zero. *)
  let total_cpu_per_pcpu =
    if total_cpu_per_pcpu <= 0. then 1. else total_cpu_per_pcpu in
  let total_cpu = float node_info.C.cpus *. total_cpu_per_pcpu in

  (* Get the domains.  Match up with their last_info (if any). *)
  let doms =
    let doms = D.get_all_domain_stats conn what who in
    let doms = Array.to_list doms in
    List.map (
      fun { D.dom_uuid = uuid; D.params = params } ->
        let nr_params = Array.length params in
        let get_param name =
          let rec loop i =
            if i = nr_params then None
            else if fst params.(i) = name then Some (snd params.(i))
            else loop (i+1)
          in
          loop 0
        in
        let get_param_int name default =
          match get_param name with
          | None -> None
          | Some (D.TypedFieldInt32 i)
          | Some (D.TypedFieldUInt32 i) -> Some (Int32.to_int i)
          | Some (D.TypedFieldInt64 i)
          | Some (D.TypedFieldUInt64 i) -> Some (Int64.to_int i)
          | _ -> default
        in
        let get_param_int64 name default =
          match get_param name with
          | None -> None
          | Some (D.TypedFieldInt32 i)
          | Some (D.TypedFieldUInt32 i) -> Some (Int64.of_int32 i)
          | Some (D.TypedFieldInt64 i)
          | Some (D.TypedFieldUInt64 i) -> Some i
          | _ -> default
        in

        let dom = D.lookup_by_uuid conn uuid in
        let id = D.get_id dom in
        let name = D.get_name dom in
        let state = get_param_int "state.state" None in

        if state = Some 5 (* VIR_DOMAIN_SHUTOFF *) then
          (name, Inactive)
        else (
          (* Active domain. *)

          (* Synthesize a D.info struct out of the data we have
           * from virConnectGetAllDomainStats.  Doing this is an
           * artifact from the old APIs we used to use to fetch
           * stats, we could simplify here, and also return the
           * RSS memory. XXX
           *)
          let state =
            match state with
            | None | Some 0 -> D.InfoNoState
            | Some 1 -> D.InfoRunning
            | Some 2 -> D.InfoBlocked
            | Some 3 -> D.InfoPaused
            | Some 4 -> D.InfoShutdown
            | Some 5 -> D.InfoShutoff
            | Some 6 -> D.InfoCrashed
            | Some 7 -> D.InfoPaused (* XXX really VIR_DOMAIN_PMSUSPENDED *)
            | _ -> D.InfoNoState in
          let memory =
            match get_param_int64 "balloon.current" None with
            | None -> 0_L
            | Some m -> m in
          let nr_virt_cpu =
            match get_param_int "vcpu.current" None with
            | None -> 1
            | Some v -> v in
          let cpu_time =
            (* NB: libvirt does not return cpu.time for non-root domains. *)
            match get_param_int64 "cpu.time" None with
            | None -> 0_L
            | Some ns -> ns in
          let info = {
            D.state = state;
            max_mem = -1_L; (* not used anywhere in virt-top *)
            memory = memory;
            nr_virt_cpu = nr_virt_cpu;
            cpu_time = cpu_time
          } in

          let nr_block_devs =
            match get_param_int "block.count" None with
            | None -> 0
            | Some i -> i in
          let block_stats =
            List.map (
              fun i ->
              let dev =
                match get_param (sprintf "block.%d.name" i) with
                | None -> sprintf "blk%d" i
                | Some (D.TypedFieldString s) -> s
                | _ -> assert false in
              dev, {
                D.rd_req =
                  (match get_param_int64 (sprintf "block.%d.rd.reqs" i) None
                   with None -> 0_L | Some v -> v);
                rd_bytes =
                  (match get_param_int64 (sprintf "block.%d.rd.bytes" i) None
                   with None -> 0_L | Some v -> v);
                wr_req =
                  (match get_param_int64 (sprintf "block.%d.wr.reqs" i) None
                   with None -> 0_L | Some v -> v);
                wr_bytes =
                  (match get_param_int64 (sprintf "block.%d.wr.bytes" i) None
                   with None -> 0_L | Some v -> v);
                errs = 0_L
              }
            ) (range 0 (nr_block_devs-1)) in

          let nr_interface_devs =
            match get_param_int "net.count" None with
            | None -> 0
            | Some i -> i in
          let interface_stats =
            List.map (
              fun i ->
              let dev =
                match get_param (sprintf "net.%d.name" i) with
                | None -> sprintf "net%d" i
                | Some (D.TypedFieldString s) -> s
                | _ -> assert false in
              dev, {
                D.rx_bytes =
                  (match get_param_int64 (sprintf "net.%d.rx.bytes" i) None
                   with None -> 0_L | Some v -> v);
                rx_packets =
                  (match get_param_int64 (sprintf "net.%d.rx.pkts" i) None
                   with None -> 0_L | Some v -> v);
                rx_errs =
                  (match get_param_int64 (sprintf "net.%d.rx.errs" i) None
                   with None -> 0_L | Some v -> v);
                rx_drop =
                  (match get_param_int64 (sprintf "net.%d.rx.drop" i) None
                   with None -> 0_L | Some v -> v);
                tx_bytes =
                  (match get_param_int64 (sprintf "net.%d.tx.bytes" i) None
                   with None -> 0_L | Some v -> v);
                tx_packets =
                  (match get_param_int64 (sprintf "net.%d.tx.pkts" i) None
                   with None -> 0_L | Some v -> v);
                tx_errs =
                  (match get_param_int64 (sprintf "net.%d.tx.errs" i) None
                   with None -> 0_L | Some v -> v);
                tx_drop =
                  (match get_param_int64 (sprintf "net.%d.tx.drop" i) None
                   with None -> 0_L | Some v -> v);
              }
            ) (range 0 (nr_interface_devs-1)) in

	  let prev_info, prev_block_stats, prev_interface_stats =
	    try
	      let prev_info, prev_block_stats, prev_interface_stats =
		Hashtbl.find last_info uuid in
	      Some prev_info, prev_block_stats, prev_interface_stats
	    with Not_found -> None, [], [] in

	  (name,
           Active {
	     rd_domid = id; rd_domuuid = uuid; rd_dom = dom;
             rd_info = info;
	     rd_block_stats = block_stats;
	     rd_interface_stats = interface_stats;
	     rd_prev_info = prev_info;
	     rd_prev_block_stats = prev_block_stats;
	     rd_prev_interface_stats = prev_interface_stats;
	     rd_cpu_time = 0.; rd_percent_cpu = 0.;
             rd_mem_bytes = 0L; rd_mem_percent = 0L;
	     rd_block_rd_reqs = None; rd_block_wr_reqs = None;
             rd_block_rd_bytes = None; rd_block_wr_bytes = None;
	     rd_net_rx_bytes = None; rd_net_tx_bytes = None;
	   })
        )
    ) doms in

  (* Calculate the CPU time (ns) and %CPU used by each domain. *)
  let doms =
    List.map (
      function
      (* We have previous CPU info from which to calculate it? *)
      | name, Active ({ rd_prev_info = Some prev_info } as rd) ->
	 let cpu_time =
	   Int64.to_float (rd.rd_info.D.cpu_time -^ prev_info.D.cpu_time) in
	 let percent_cpu = 100. *. cpu_time /. total_cpu in
         let mem_usage = rd.rd_info.D.memory in
         let mem_percent =
           100L *^ rd.rd_info.D.memory /^ node_info.C.memory in
	 let rd = { rd with
		    rd_cpu_time = cpu_time;
		    rd_percent_cpu = percent_cpu;
		    rd_mem_bytes = mem_usage;
                    rd_mem_percent = mem_percent} in
	 name, Active rd
      (* For all other domains we can't calculate it, so leave as 0 *)
      | rd -> rd
    ) doms in

  (* Calculate the number of block device read/write requests across
   * all block devices attached to a domain.
   *)
  let doms =
    List.map (
      function
      (* Do we have stats from the previous slice? *)
      | name, Active ({ rd_prev_block_stats = ((_::_) as prev_block_stats) }
		      as rd) ->
	 let block_stats = rd.rd_block_stats in (* stats now *)

	 (* Add all the devices together.  Throw away device names. *)
	 let prev_block_stats =
	   sum_block_stats (List.map snd prev_block_stats) in
	 let block_stats =
	   sum_block_stats (List.map snd block_stats) in

	 (* Calculate increase in read & write requests. *)
	 let read_reqs =
	   block_stats.D.rd_req -^ prev_block_stats.D.rd_req in
	 let write_reqs =
	   block_stats.D.wr_req -^ prev_block_stats.D.wr_req in
         let read_bytes =
           block_stats.D.rd_bytes -^ prev_block_stats.D.rd_bytes in
         let write_bytes =
           block_stats.D.wr_bytes -^ prev_block_stats.D.wr_bytes in

	 let rd = { rd with
		    rd_block_rd_reqs = Some read_reqs;
		    rd_block_wr_reqs = Some write_reqs;
                    rd_block_rd_bytes = Some read_bytes;
                    rd_block_wr_bytes = Some write_bytes;
         } in
	 name, Active rd
      (* For all other domains we can't calculate it, so leave as None. *)
      | rd -> rd
    ) doms in

  (* Calculate the same as above for network interfaces across
   * all network interfaces attached to a domain.
   *)
  let doms =
    List.map (
      function
      (* Do we have stats from the previous slice? *)
      | name, Active ({ rd_prev_interface_stats =
			  ((_::_) as prev_interface_stats) }
		      as rd) ->
	 let interface_stats = rd.rd_interface_stats in (* stats now *)

	 (* Add all the devices together.  Throw away device names. *)
	 let prev_interface_stats =
	   sum_interface_stats (List.map snd prev_interface_stats) in
	 let interface_stats =
	   sum_interface_stats (List.map snd interface_stats) in

	 (* Calculate increase in rx & tx bytes. *)
	 let rx_bytes =
	   interface_stats.D.rx_bytes -^ prev_interface_stats.D.rx_bytes in
	 let tx_bytes =
	   interface_stats.D.tx_bytes -^ prev_interface_stats.D.tx_bytes in

	 let rd = { rd with
		    rd_net_rx_bytes = Some rx_bytes;
		    rd_net_tx_bytes = Some tx_bytes } in
	 name, Active rd
      (* For all other domains we can't calculate it, so leave as None. *)
      | rd -> rd
    ) doms in

  (* Calculate totals. *)
  let totals =
    List.fold_left (
        fun (count, running, blocked, paused, shutdown, shutoff,
	     crashed, active, inactive,
	     total_cpu_time, total_memory, total_domU_memory) ->
	function
	| (name, Active rd) ->
	   let test state orig =
	     if rd.rd_info.D.state = state then orig+1 else orig
	   in
	   let running = test D.InfoRunning running in
	   let blocked = test D.InfoBlocked blocked in
	   let paused = test D.InfoPaused paused in
	   let shutdown = test D.InfoShutdown shutdown in
	   let shutoff = test D.InfoShutoff shutoff in
	   let crashed = test D.InfoCrashed crashed in

	   let total_cpu_time = total_cpu_time +. rd.rd_cpu_time in
	   let total_memory = total_memory +^ rd.rd_info.D.memory in
	   let total_domU_memory =
             total_domU_memory +^
	       if rd.rd_domid > 0 then rd.rd_info.D.memory else 0L in

	   (count+1, running, blocked, paused, shutdown, shutoff,
	    crashed, active+1, inactive,
	    total_cpu_time, total_memory, total_domU_memory)

	| (name, Inactive) -> (* inactive domain *)
	   (count+1, running, blocked, paused, shutdown, shutoff,
	    crashed, active, inactive+1,
	    total_cpu_time, total_memory, total_domU_memory)
    ) (0,0,0,0,0,0,0,0,0, 0.,0L,0L) doms in

  (* Update last_time, last_info. *)
  last_time := time;
  Hashtbl.clear last_info;
  List.iter (
    function
    | (_, Active rd) ->
       let info = rd.rd_info, rd.rd_block_stats, rd.rd_interface_stats in
       Hashtbl.add last_info rd.rd_domuuid info
    | _ -> ()
  ) doms;

  { rd_doms = doms;
    rd_time = time;
    rd_printable_time = printable_time;
    rd_nr_pcpus = nr_pcpus;
    rd_total_cpu = total_cpu;
    rd_total_cpu_per_pcpu = total_cpu_per_pcpu;
    rd_totals = totals }

(* Collect some extra information in PCPUDisplay display_mode. *)
let collect_pcpu { rd_doms = doms; rd_nr_pcpus = nr_pcpus } =
  (* Get the VCPU info and VCPU->PCPU mappings for active domains.
   * Also cull some data we don't care about.
   *)
  let doms =
    List.filter_map (
      function
      | (name, Active rd) ->
	 (try
	     let domid = rd.rd_domid in
	     let maplen = C.cpumaplen nr_pcpus in
	     let cpu_stats = D.get_cpu_stats rd.rd_dom in

             (* Note the terminology is confusing.
              *
              * In libvirt, cpu_time is the total time (hypervisor +
              * vCPU).  vcpu_time is the time only taken by the vCPU,
              * excluding time taken inside the hypervisor.
              *
              * For each pCPU, libvirt may return either "cpu_time"
              * or "vcpu_time" or neither or both.  This function
              * returns an array pair [|cpu_time, vcpu_time|];
              * if either is missing it is returned as 0.
              *)
	     let find_cpu_usages params =
               let rec find_uint64_field name = function
                 | (n, D.TypedFieldUInt64 usage) :: _ when n = name ->
                    usage
                 | _ :: params -> find_uint64_field name params
                 | [] -> 0L
               in
               [| find_uint64_field "cpu_time" params;
                  find_uint64_field "vcpu_time" params |]
             in

	     let pcpu_usages = Array.map find_cpu_usages cpu_stats in
	     let maxinfo = rd.rd_info.D.nr_virt_cpu in
	     let nr_vcpus, vcpu_infos, cpumaps =
	       D.get_vcpus rd.rd_dom maxinfo maplen in

	     (* Got previous pcpu_usages for this domain? *)
	     let prev_pcpu_usages =
	       try Some (Hashtbl.find last_pcpu_usages domid)
	       with Not_found -> None in
	     (* Update last_pcpu_usages. *)
	     Hashtbl.replace last_pcpu_usages domid pcpu_usages;

	     (match prev_pcpu_usages with
	      | Some prev_pcpu_usages
		   when Array.length prev_pcpu_usages = Array.length pcpu_usages ->
		 Some (domid, name, nr_vcpus, vcpu_infos, pcpu_usages,
		       prev_pcpu_usages, cpumaps, maplen)
	      | _ -> None (* ignore missing / unequal length prev_vcpu_infos *)
	     );
	   with
	     Libvirt.Virterror _ -> None (* ignore transient libvirt errors *)
	 )
      | (_, Inactive) -> None (* ignore inactive doms *)
    ) doms in
  let nr_doms = List.length doms in

  (* Rearrange the data into a matrix.  Major axis (down) is
   * pCPUs.  Minor axis (right) is domains.  At each node we store:
   *  cpu_time hypervisor + domain (on this pCPU only, nanosecs),
   *  vcpu_time domain only (on this pCPU only, nanosecs).
   *)
  let make_3d_array dimx dimy dimz e =
    Array.init dimx (fun _ -> Array.make_matrix dimy dimz e)
  in
  let pcpus = make_3d_array nr_pcpus nr_doms 2 0L in

  List.iteri (
    fun di (domid, name, nr_vcpus, vcpu_infos, pcpu_usages,
	    prev_pcpu_usages, cpumaps, maplen) ->
      (* Which pCPUs can this dom run on? *)
      for p = 0 to Array.length pcpu_usages - 1 do
	pcpus.(p).(di).(0) <-
          pcpu_usages.(p).(0) -^ prev_pcpu_usages.(p).(0);
	pcpus.(p).(di).(1) <-
          pcpu_usages.(p).(1) -^ prev_pcpu_usages.(p).(1)
      done
  ) doms;

  (* Sum the total CPU time used by each pCPU, for the %CPU column. *)
  let pcpus_cpu_time =
    Array.map (
      fun row ->
        let cpu_time = ref 0L in
	for di = 0 to Array.length row-1 do
	  let t = row.(di).(0) in
	  cpu_time := !cpu_time +^ t
	done;
	Int64.to_float !cpu_time
    ) pcpus in

  { rd_pcpu_doms = doms;
    rd_pcpu_pcpus = pcpus;
    rd_pcpu_pcpus_cpu_time = pcpus_cpu_time }
