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

open ExtList
open Curses
open Printf

open Opt_gettext.Gettext
open Utils
open Types
open Screen
open Collect

module C = Libvirt.Connect
module D = Libvirt.Domain

(* Keep a historical list of %CPU usages. *)
let historical_cpu = ref []
let historical_cpu_last_time = ref (Unix.gettimeofday ())

(* Redraw the display. *)
let redraw display_mode sort_order
           (_, _, _, _, _, node_info, _, _) (* setup *)
           block_in_bytes
           historical_cpu_delay
           { rd_doms = doms;
             rd_time = time; rd_printable_time = printable_time;
             rd_nr_pcpus = nr_pcpus;
             rd_total_cpu = total_cpu;
             rd_total_cpu_per_pcpu = total_cpu_per_pcpu;
             rd_totals = totals } (* state *)
           pcpu_display =
  clear ();

  (* Get the screen/window size. *)
  let lines, cols = get_size () in

  (* Time. *)
  mvaddstr top_lineno 0 (sprintf "virt-top %s - " printable_time);

  (* Basic node_info. *)
  addstr
    (sprintf "%s %d/%dCPU %dMHz %LdMB "
	     node_info.C.model node_info.C.cpus nr_pcpus node_info.C.mhz
	     (node_info.C.memory /^ 1024L));
  (* Save the cursor position for when we come to draw the
   * historical CPU times (down in this function).
   *)
  let stdscr = stdscr () in
  let historical_cursor = getyx stdscr in

  (match display_mode with

   (*---------- Showing domains ----------*)
   | TaskDisplay ->
      (* Sort domains on current sort_order. *)
      let doms =
	let cmp =
	  match sort_order with
	  | DomainName ->
	     (fun _ -> 0) (* fallthrough to default name compare *)
	  | Processor ->
	     (function
	       | Active rd1, Active rd2 ->
		  compare rd2.rd_percent_cpu rd1.rd_percent_cpu
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | Memory ->
	     (function
	       | Active { rd_info = info1 }, Active { rd_info = info2 } ->
		  compare info2.D.memory info1.D.memory
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | Time ->
	     (function
	       | Active { rd_info = info1 }, Active { rd_info = info2 } ->
		  compare info2.D.cpu_time info1.D.cpu_time
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | DomainID ->
	     (function
	       | Active { rd_domid = id1 }, Active { rd_domid = id2 } ->
		  compare id1 id2
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | NetRX ->
	     (function
	       | Active { rd_net_rx_bytes = r1 }, Active { rd_net_rx_bytes = r2 } ->
		  compare r2 r1
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | NetTX ->
	     (function
	       | Active { rd_net_tx_bytes = r1 }, Active { rd_net_tx_bytes = r2 } ->
		  compare r2 r1
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | BlockRdRq ->
	     (function
	       | Active { rd_block_rd_reqs = r1 }, Active { rd_block_rd_reqs = r2 } ->
		  compare r2 r1
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	  | BlockWrRq ->
	     (function
	       | Active { rd_block_wr_reqs = r1 }, Active { rd_block_wr_reqs = r2 } ->
		  compare r2 r1
	       | Active _, Inactive -> -1
	       | Inactive, Active _ -> 1
	       | Inactive, Inactive -> 0)
	in
	let cmp (name1, dom1) (name2, dom2) =
	  let r = cmp (dom1, dom2) in
	  if r <> 0 then r
	  else compare name1 name2
	in
	List.sort ~cmp doms in

      (* Print domains. *)
      attron A.reverse;
      let header_string =
        if block_in_bytes
        then "   ID S RDBY WRBY RXBY TXBY %CPU %MEM    TIME   NAME"
        else "   ID S RDRQ WRRQ RXBY TXBY %CPU %MEM    TIME   NAME"
      in
      mvaddstr header_lineno 0
	       (pad cols header_string);
      attroff A.reverse;

      let rec loop lineno = function
	| [] -> ()
	| (name, Active rd) :: doms ->
	   if lineno < lines then (
	     let state = show_state rd.rd_info.D.state in
	     let rd_req = Show.int64_option rd.rd_block_rd_info in
	     let wr_req = Show.int64_option rd.rd_block_wr_info in
	     let rx_bytes = Show.int64_option rd.rd_net_rx_bytes in
	     let tx_bytes = Show.int64_option rd.rd_net_tx_bytes in
	     let percent_cpu = Show.percent rd.rd_percent_cpu in
	     let percent_mem = Int64.to_float rd.rd_mem_percent in
	     let percent_mem = Show.percent percent_mem in
	     let time = Show.time rd.rd_info.D.cpu_time in

	     let line =
               sprintf "%5d %c %s %s %s %s %s %s %s %s"
		       rd.rd_domid state rd_req wr_req rx_bytes tx_bytes
		       percent_cpu percent_mem time name in
	     let line = pad cols line in
	     mvaddstr lineno 0 line;
	     loop (lineno+1) doms
	   )
	| (name, Inactive) :: doms -> (* inactive domain *)
	   if lineno < lines then (
	     let line =
	       sprintf
		 "    -                                           (%s)"
		 name in
	     let line = pad cols line in
	     mvaddstr lineno 0 line;
	     loop (lineno+1) doms
	   )
      in
      loop domains_lineno doms

   (*---------- Showing physical CPUs ----------*)
   | PCPUDisplay ->
      let { rd_pcpu_doms = doms;
            rd_pcpu_pcpus = pcpus;
            rd_pcpu_pcpus_cpu_time = pcpus_cpu_time } =
	match pcpu_display with
	| Some p -> p
	| None -> failwith "internal error: no pcpu_display data" in

      (* Display the pCPUs. *)
      let dom_names =
	String.concat "" (
	                List.map (
	                    fun (_, name, _, _, _, _, _, _) ->
		            let len = String.length name in
		            let width = max (len+1) 12 in
		            pad width name
	                  ) doms
	              ) in
      attron A.reverse;
      mvaddstr header_lineno 0 (pad cols ("PHYCPU %CPU " ^ dom_names));
      attroff A.reverse;

      Array.iteri (
	fun p row ->
	  mvaddstr (p+domains_lineno) 0 (sprintf "%4d   " p);
	  let cpu_time = pcpus_cpu_time.(p) in (* ns used on this CPU *)
	  let percent_cpu = 100. *. cpu_time /. total_cpu_per_pcpu in
	  addstr (Show.percent percent_cpu);
	  addch ' ';

	  List.iteri (
	    fun di (domid, name, _, _, _, _, _, _) ->
	      let t = pcpus.(p).(di).(0) in (* hypervisor + domain *)
	      let t_only = pcpus.(p).(di).(1) in (* domain only *)
	      let len = String.length name in
	      let width = max (len+1) 12 in
	      let str_t =
		if t <= 0L then ""
		else (
		  let t = Int64.to_float t in
		  let percent = 100. *. t /. total_cpu_per_pcpu in
		  Show.percent percent
		) in
              let str_t_only =
                if t_only <= 0L then ""
                else (
                  let t_only = Int64.to_float t_only in
                  let percent = 100. *. t_only /. total_cpu_per_pcpu in
                  Show.percent percent
                ) in
              addstr (pad 5 str_t);
              addstr (pad 5 str_t_only);
              addstr (pad (width-10) " ");
	      ()
          ) doms
      ) pcpus;

   (*---------- Showing network interfaces ----------*)
   | NetDisplay ->
      (* Only care about active domains. *)
      let doms =
        List.filter_map (
	    function
	    | (name, Active rd) -> Some (name, rd)
	    | (_, Inactive) -> None
	) doms in

      (* For each domain we have a list of network interfaces seen
       * this slice, and seen in the previous slice, which we now
       * match up to get a list of (domain, interface) for which
       * we have current & previous knowledge.  (And ignore the rest).
       *)
      let devs =
	List.map (
	  fun (name, rd) ->
	    List.filter_map (
	      fun (dev, stats) ->
	        try
		  (* Have prev slice stats for this device? *)
		  let prev_stats =
		    List.assoc dev rd.rd_prev_interface_stats in
		  Some (dev, name, rd, stats, prev_stats)
		with Not_found -> None
	      ) rd.rd_interface_stats
	  ) doms in

      (* Finally we have a list of:
       * device name, domain name, rd_* stuff, curr stats, prev stats.
       *)
      let devs : (string * string * rd_active *
		    D.interface_stats * D.interface_stats) list =
	List.flatten devs in

      (* Difference curr slice & prev slice. *)
      let devs =
        List.map (
	  fun (dev, name, rd, curr, prev) ->
	    dev, name, rd, diff_interface_stats curr prev
	  ) devs in

      (* Sort by current sort order, but map some of the standard
       * sort orders into ones which makes sense here.
       *)
      let devs =
	let cmp =
	  match sort_order with
	  | DomainName ->
	     (fun _ -> 0) (* fallthrough to default name compare *)
	  | DomainID ->
	     (fun (_, { rd_domid = id1 }, _, { rd_domid = id2 }) ->
	      compare id1 id2)
	  | Processor | Memory | Time
          | BlockRdRq | BlockWrRq
	     (* fallthrough to RXBY comparison. *)
	  | NetRX ->
	     (fun ({ D.rx_bytes = b1 }, _, { D.rx_bytes = b2 }, _) ->
	      compare b2 b1)
	  | NetTX ->
	     (fun ({ D.tx_bytes = b1 }, _, { D.tx_bytes = b2 }, _) ->
	      compare b2 b1)
	in
	let cmp (dev1, name1, rd1, stats1) (dev2, name2, rd2, stats2) =
	  let r = cmp (stats1, rd1, stats2, rd2) in
	  if r <> 0 then r
	  else compare (dev1, name1) (dev2, name2)
	in
	List.sort ~cmp devs in

      (* Print the header for network devices. *)
      attron A.reverse;
      mvaddstr header_lineno 0
	       (pad cols "   ID S RXBY TXBY RXPK TXPK DOMAIN       INTERFACE");
      attroff A.reverse;

      (* Print domains and devices. *)
      let rec loop lineno = function
	| [] -> ()
	| (dev, name, rd, stats) :: devs ->
	   if lineno < lines then (
	     let state = show_state rd.rd_info.D.state in
	     let rx_bytes =
	       if stats.D.rx_bytes >= 0L
	       then Show.int64 stats.D.rx_bytes
	       else "    " in
	     let tx_bytes =
	       if stats.D.tx_bytes >= 0L
	       then Show.int64 stats.D.tx_bytes
	       else "    " in
	     let rx_packets =
	       if stats.D.rx_packets >= 0L
	       then Show.int64 stats.D.rx_packets
	       else "    " in
	     let tx_packets =
	       if stats.D.tx_packets >= 0L
	       then Show.int64 stats.D.tx_packets
	       else "    " in

	     let line = sprintf "%5d %c %s %s %s %s %-12s %s"
		                rd.rd_domid state
		                rx_bytes tx_bytes
		                rx_packets tx_packets
		                (pad 12 name) dev in
	     let line = pad cols line in
	     mvaddstr lineno 0 line;
	     loop (lineno+1) devs
	   )
      in
      loop domains_lineno devs

   (*---------- Showing block devices ----------*)
   | BlockDisplay ->
      (* Only care about active domains. *)
      let doms =
        List.filter_map (
	    function
	    | (name, Active rd) -> Some (name, rd)
	    | (_, Inactive) -> None
	) doms in

      (* For each domain we have a list of block devices seen
       * this slice, and seen in the previous slice, which we now
       * match up to get a list of (domain, device) for which
       * we have current & previous knowledge.  (And ignore the rest).
       *)
      let devs =
	List.map (
	  fun (name, rd) ->
	    List.filter_map (
	      fun (dev, stats) ->
	        try
		  (* Have prev slice stats for this device? *)
		  let prev_stats =
		    List.assoc dev rd.rd_prev_block_stats in
		  Some (dev, name, rd, stats, prev_stats)
		with Not_found -> None
	    ) rd.rd_block_stats
	) doms in

      (* Finally we have a list of:
       * device name, domain name, rd_* stuff, curr stats, prev stats.
       *)
      let devs : (string * string * rd_active *
		    D.block_stats * D.block_stats) list =
	List.flatten devs in

      (* Difference curr slice & prev slice. *)
      let devs =
        List.map (
	  fun (dev, name, rd, curr, prev) ->
	    dev, name, rd, diff_block_stats curr prev
        ) devs in

      (* Sort by current sort order, but map some of the standard
       * sort orders into ones which makes sense here.
       *)
      let devs =
	let cmp =
	  match sort_order with
	  | DomainName ->
	     (fun _ -> 0) (* fallthrough to default name compare *)
	  | DomainID ->
	     (fun (_, { rd_domid = id1 }, _, { rd_domid = id2 }) ->
	      compare id1 id2)
	  | Processor | Memory | Time
          | NetRX | NetTX
	     (* fallthrough to RDRQ comparison. *)
	  | BlockRdRq ->
	     (fun ({ D.rd_req = b1 }, _, { D.rd_req = b2 }, _) ->
	      compare b2 b1)
	  | BlockWrRq ->
	     (fun ({ D.wr_req = b1 }, _, { D.wr_req = b2 }, _) ->
	      compare b2 b1)
	in
	let cmp (dev1, name1, rd1, stats1) (dev2, name2, rd2, stats2) =
	  let r = cmp (stats1, rd1, stats2, rd2) in
	  if r <> 0 then r
	  else compare (dev1, name1) (dev2, name2)
	in
	List.sort ~cmp devs in

      (* Print the header for block devices. *)
      attron A.reverse;
      mvaddstr header_lineno 0
	       (pad cols "   ID S RDBY WRBY RDRQ WRRQ DOMAIN       DEVICE");
      attroff A.reverse;

      (* Print domains and devices. *)
      let rec loop lineno = function
	| [] -> ()
	| (dev, name, rd, stats) :: devs ->
	   if lineno < lines then (
	     let state = show_state rd.rd_info.D.state in
	     let rd_bytes =
	       if stats.D.rd_bytes >= 0L
	       then Show.int64 stats.D.rd_bytes
	       else "    " in
	     let wr_bytes =
	       if stats.D.wr_bytes >= 0L
	       then Show.int64 stats.D.wr_bytes
	       else "    " in
	     let rd_req =
	       if stats.D.rd_req >= 0L
	       then Show.int64 stats.D.rd_req
	       else "    " in
	     let wr_req =
	       if stats.D.wr_req >= 0L
	       then Show.int64 stats.D.wr_req
	       else "    " in

	     let line = sprintf "%5d %c %s %s %s %s %-12s %s"
		                rd.rd_domid state
		                rd_bytes wr_bytes
		                rd_req wr_req
		                (pad 12 name) dev in
	     let line = pad cols line in
	     mvaddstr lineno 0 line;
	     loop (lineno+1) devs
	   )
      in
      loop domains_lineno devs
  ); (* end of display_mode conditional section *)

  let (count, running, blocked, paused, shutdown, shutoff,
       crashed, active, inactive,
       total_cpu_time, total_memory, total_domU_memory) = totals in

  mvaddstr summary_lineno 0
           (sprintf
	      (f_"%d domains, %d active, %d running, %d sleeping, %d paused, %d inactive D:%d O:%d X:%d")
	      count active running blocked paused inactive shutdown shutoff crashed);

  (* Total %CPU used, and memory summary. *)
  let percent_cpu = 100. *. total_cpu_time /. total_cpu in
  mvaddstr (summary_lineno+1) 0
           (sprintf
	      (f_"CPU: %2.1f%%  Mem: %Ld MB (%Ld MB by guests)")
	      percent_cpu (total_memory /^ 1024L) (total_domU_memory /^ 1024L));

  (* Time to grab another historical %CPU for the list? *)
  if time >= !historical_cpu_last_time +. float historical_cpu_delay
  then (
    historical_cpu := percent_cpu :: List.take 10 !historical_cpu;
    historical_cpu_last_time := time
  );

  (* Display historical CPU time. *)
  let () =
    let y, x = historical_cursor in
    let maxwidth = cols - x in
    let line =
      String.concat " "
	            (List.map (sprintf "%2.1f%%") !historical_cpu) in
    let line = pad maxwidth line in
    mvaddstr y x line;
    () in

  move message_lineno 0; (* Park cursor in message area, as with top. *)
  refresh ()             (* Refresh the display. *)
